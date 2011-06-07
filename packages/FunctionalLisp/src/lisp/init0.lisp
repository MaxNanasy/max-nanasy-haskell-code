(define dynamic-let (macro (lambda (bindings form)
                             `(with-dynamic-bindings
                               ',(map car bindings)
                               (list ,@(map cadr bindings))
                               (lambda () ,form)))))

(define write-string (lambda (string stream)
                       (map (lambda (x) (write-char x stream)) (un-new-type string))))
(define write-line (lambda (string stream)
                     (sequence (write-string string stream)
                               (write-char #\
 stream))))

(define print (lambda (x stream) (sequence (write-char #\
 stream) (write x stream) (write-char #\  stream) x)))

(write-line "REPL" *standard-output*)
(write-line "----" *standard-output*)

(define repl (lambda ()
               (sequence
                (print (eval (read %*standard-input*)) %*standard-output*)
                (write-char #\
 %*standard-output*)
                (repl))))
(repl)
