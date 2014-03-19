(load "./lib.scm")

(define (virtual-SSC program)
  ;run program
  (let ((RAM (ready program (init-RAM))))
    (run (init-CPU) RAM)))
