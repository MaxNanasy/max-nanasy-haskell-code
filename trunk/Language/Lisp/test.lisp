(define 'list (lambda xs xs))
(define 'if (macro (lambda (i t e) (if-function i (lambda () t) (lambda () e)))))
(define 'map (lambda (f xs)
               (if xs
                   (cons (f (car xs)) (map f (cdr xs)))
                 ())))
(map (lambda (x) (define x x)) (list 't 'nil))
(map (lambda (x) (cons x x)) (list t nil))
END
