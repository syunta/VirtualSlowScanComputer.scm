(load "./lib.scm")

(define (virtual-SSC program)
  ;constant
  (define mem-length 8)
  (define pc-length  5)
  (define op-length  3)
  (define adr-length 5)

  ;run program
  (let ((RAM (ready program (init-RAM))))
    (run (init-CPU) RAM)))
