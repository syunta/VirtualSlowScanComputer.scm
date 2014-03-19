(load "./lib.scm")

(define (virtual-SSC program)
  ;constant
  (define mem-length 8)
  (define pc-length  5)
  (define op-length  3)
  (define adr-length 5)

  ;data constructor
  (define (init-RAM)
    (let ((RAM (make-vector 32)))
      (vector-fill! RAM (int->uint 0 mem-length)) RAM))

  (define (init-CPU)
    (list (cons 'ACC (int->uint 0 mem-length))
          (cons 'ISR (int->uint 0 mem-length))
          (cons 'PC  (int->uint 0 pc-length))
          (cons 'OVF (int->uint 0 1))))

  ;  program code
  ;
  ; '((opcode-1 address-1)
  ;   (opcode-2 address-2)
  ;    ...              
  ;   (opcode-n address-n)
  ;   (data-1)
  ;    ...
  ;   (data-n))

  (define oplist
    (hash-table 'equal?
                (cons 'jump   (int->uint 0 op-length))
                (cons 'add    (int->uint 1 op-length))
                (cons 'sub    (int->uint 2 op-length))
                (cons 'load-m (int->uint 3 op-length))
                (cons 'store  (int->uint 4 op-length))
                (cons 'sll    (int->uint 5 op-length))
                (cons 'srl    (int->uint 6 op-length))
                (cons 'svc    (int->uint 7 op-length))))

  (define funclist
    (hash-table 'equal?
                (cons '(#f #f #f) jump   )
                (cons '(#f #f #t) add    )
                (cons '(#f #t #f) sub    )
                (cons '(#f #t #t) load-m )
                (cons '(#t #f #f) store  )
                (cons '(#t #f #t) sll    )
                (cons '(#t #t #f) srl    )
                (cons '(#t #t #t) svc    )))

  ;data selector
  (define (acc CPU)
    (cdr (assoc 'ACC CPU)))

  (define (isr CPU)
    (cdr (assoc 'ISR CPU)))

  (define (pc CPU)
    (cdr (assoc 'PC CPU)))

  (define (ovf CPU)
    (cdr (assoc 'OVF CPU)))

  (define (ram-length RAM)
    (vector-length RAM))

  (define (load-ram RAM n)
    (vector-ref RAM (uint->int n)))

  (define (read-line program)
    (car program))

  (define (next-line program)
    (cdr program))

  (define (opcode pg-or-bin)
    (define (op-pg pg)
      (car pg))
    (define (op-bin bin n op)
      (if (= n 0)
        (reverse op)
        (op-bin (cdr bin) (- n 1) (cons (car bin) op))))
    (if (boolean? (car pg-or-bin))
      (op-bin pg-or-bin op-length nil)
      (op-pg pg-or-bin)))

  (define (adr pg-or-bin)
    (define (adr-pg pg)
      (cadr pg))
    (define (adr-bin bin n)
      (if (= n op-length)
        bin
        (adr-bin (cdr bin) (+ n 1))))
    (if (boolean? (car pg-or-bin))
      (adr-bin pg-or-bin 0)
      (adr-pg pg-or-bin)))

  (define (data pg)
    (car pg))

  (define (op->uint op)
    (hash-table-get oplist op))

  (define (op+adr op adr)
    (append op adr))

  (define (op->fn op)
    (hash-table-get funclist op))

  ;data predicate
  (define (instruction? pg)
    (not (null? (cdr pg))))

  (define (ov? ovf)
    (car ovf))

  ;data setter
  (define (save-ram RAM n bin)
    (vector-set! RAM n bin))

  (define (save-acc CPU bin)
    (set-cdr! (assoc 'ACC CPU) bin))

  (define (save-isr CPU bin)
    (set-cdr! (assoc 'ISR CPU) bin))

  (define (save-pc CPU bin)
    (set-cdr! (assoc 'PC CPU) bin))

  (define (save-ovf CPU bin)
    (set-cdr! (assoc 'OVF CPU) (list bin)))

  ;processor engine
  (define (ready pg RAM)
    (let loop ((pg pg) (RAM RAM) (n 0))
      (cond ((null? pg) RAM)
            ((= n (ram-length RAM)) (error 1))
            (else
              (save-ram RAM n (assemble (read-line pg)))
              (loop (next-line pg) RAM (+ n 1))))))

  (define (assemble pg)
    (if (instruction? pg)
      (op+adr (op->uint (opcode pg))
              (int->uint (adr pg) adr-length))
      (int->uint (data pg) mem-length)))

  (define (run CPU RAM)
    (fetch CPU RAM))

  (define (fetch CPU RAM)
    (save-isr CPU (load-ram RAM (pc CPU)))
    (next-pc CPU)
    (decode CPU RAM))

  (define (next-pc CPU)
    (save-pc CPU (uint-inc (pc CPU))))

  (define (decode CPU RAM)
    (let ((op (opcode (isr CPU))))
      (exec (op->fn op) CPU RAM)))

  (define (jump CPU RAM)
    (if (not (ov? (ovf CPU)))
      (save-pc CPU (adr (isr CPU)))))

  (define (add CPU RAM)
    (receive
      (result ovf) (uint-add (acc CPU) (load-ram RAM (adr (isr CPU))))
      (save-acc CPU result)
      (save-ovf CPU ovf)))

  (define (sub CPU RAM)
    (receive
      (result ovf) (uint-sub (acc CPU) (load-ram RAM (adr (isr CPU))))
      (save-acc CPU result)
      (save-ovf CPU ovf)))

  (define (load-m CPU RAM)
    (save-acc CPU (load-ram RAM (adr (isr CPU)))))

  (define (store CPU RAM)
    (save-ram RAM (uint->int (adr (isr CPU))) (acc CPU)))

  (define (sll CPU RAM)
    (let loop ((n (uint->int (adr (isr CPU)))))
      (cond ((= n 0)) ;loop end
            ((ov? (ovf CPU))) ;loop end
            (else (receive
                    (result ovf) (uint-sll (acc CPU))
                    (save-acc CPU result)
                    (save-ovf CPU ovf)
                    (loop (- n 1)))))))

  (define (srl CPU RAM)
    (let loop ((n (uint->int (adr (isr CPU)))))
      (cond ((= n 0)) ;loop end
            ((ov? (ovf CPU))) ;loop end
            (else (receive
                    (result ovf) (uint-srl (acc CPU))
                    (save-acc CPU result)
                    (save-ovf CPU ovf)
                    (loop (- n 1)))))))

  (define (svc CPU RAM)
    (cond ((= 0 (uint->int (adr (isr CPU))))
           (halt CPU RAM))
          ((= 1 (uint->int (adr (isr CPU))))
           (print "Input a natural number")
           (save-acc CPU (int->uint (read) mem-length)))
          ((= 2 (uint->int (adr (isr CPU))))
           (print "Computed result is ...")
           (print (uint->int (acc CPU))))
          (else (error 2))))

  (define (exec fn CPU RAM)
    (fn CPU RAM)
    (run CPU RAM))

  ;run program
  (let ((RAM (ready program (init-RAM))))
    (run (init-CPU) RAM)))
