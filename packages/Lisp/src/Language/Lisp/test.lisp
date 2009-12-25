(define-symbol 'list (lambda xs xs))

(define-symbol 'define (macro (lambda (name value)
                                (list
                                 'define-symbol
                                 (list 'quote name)
                                 value))))

(define if (macro (lambda (i t e)
                    (list
                     'if-function i
                     (list 'lambda () t)
                     (list 'lambda () e)))))

(define sequence (lambda xs (last xs)))

(define last (lambda (xs)
               (if (cdr xs)
                   (last (cdr xs))
                 (car xs))))
(define cadr (lambda (x) (car (cdr x))))

(define map (lambda (f xs)
              (if xs
                  (cons (f (car xs)) (map f (cdr xs)))
                ())))

(define let (macro (lambda (bindings body)
                     (cons
                      (list
                       'lambda
                       (map car bindings)
                       body)
                      (map cadr bindings)))))

(define lookup (lambda (x alist)
                 (if alist
                     (let ((entry (car alist)))
                       (if (eq x (car entry))
                           (cdr entry)
                         (lookup x (cdr alist))))
                   ())))

(define print (lambda (x stream) (sequence (write-char #\
 stream) (write x stream) (write-char #\  stream) x)))

(define make-char-reader (lambda (char form) (cons char (lambda (stream) (list form (read stream))))))
(define comma-reader (lambda (stream)
                       (let ((c (peek-char stream)))
                         (list (if (eq c #\@)
                                   (sequence (read-char stream) 'unquote-splice)
                                 'unquote) (read stream)))))
(define eof-reader (lambda (stream) ((dynamic *read-eof*))))
(define double-quote-reader (lambda (stream) (list-to-string (read-delimited-string #\" stream))))
(define read-delimited-string (lambda (char stream)
                                (let ((c (read-char stream)))
                                  (if (eq c char)
                                      ()
                                    (cons c (read-delimited-string char stream))))))

(define *reader-dispatch-table* (list (make-char-reader #\' 'quote             )
                                      (make-char-reader #\` 'quasiquote        )
                                      (cons             #\, comma-reader       )
                                      (make-char-reader #\% 'dynamic           )
                                      (cons             ()  eof-reader         )
                                      (cons             #\" double-quote-reader)))

(define token-char? (lambda (c)
                      (or (lookup char *reader-dispatch-table*) 

(define read (lambda (stream)
               (let ((char (peek-char stream)))
                 (let ((dispatch-function (lookup char *reader-dispatch-table*)))
                   (if dispatch-function
                       (sequence (read-char stream) (dispatch-function stream))
                     (intern (list-to-string (read-token stream))))))))

(define read-token (lambda (stream)
                     (if (token-char? (peek-char stream))
                         ()
                       (cons (read-char stream) (read-token stream)))))

(print (read-token *standard-input*) *standard-output*)

(define load-stream (lambda (stream) (sequence (eval (read stream)) (load-stream stream))))

(define load-file (lambda (name)
                    (call/cc (lambda (k)
                               (with-dynamic-bindings (quote (*read-eof*)) (list (lambda ()
                                                           (k ())))
                                            (load-stream (open-file name)))))))

(load-file (list-to-string (list #\l #\o #\a #\d #\a #\b #\l #\e)))

(define append (lambda (xs ys)
                 (if xs
                     (cons (car xs) (append (cdr xs) ys))
                   ys)))

(define quasiquote-form (lambda (form)
                            (if (eq (type-of form) 'cons)
                                (let ((head (car form)))
                                  (if (eq head 'unquote)
                                      (list 'list (cadr form))
                                    (if (eq head 'unquote-splice)
                                        (cadr form)
                                      (list 'list (list 'append (quasiquote-form head) (list 'car (quasiquote-form (cdr form))))))))
                              (list 'quote (list form)))))
(define quasiquote (macro (lambda (form) (list 'car (quasiquote-form form)))))

(define dynamic (macro (lambda (form) `(dynamic-value ',form))))

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

(write-line "REPL" *standard-output*)
(write-line "----" *standard-output*)

(define repl (lambda ()
               (sequence
                (print (eval (read %*standard-input*)) %*standard-output*)
                (write-char #\
 %*standard-output*)
                (repl))))
(repl)

END

(quit)
