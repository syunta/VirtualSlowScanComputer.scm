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


  ;run program
  (let ((RAM (ready program (init-RAM))))
    (run (init-CPU) RAM)))
