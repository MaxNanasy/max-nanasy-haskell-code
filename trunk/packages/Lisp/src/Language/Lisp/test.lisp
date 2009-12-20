(define-symbol (quote list) (lambda xs xs))

(define-symbol (quote define) (macro (lambda (name value)
                                       (list
                                        (quote define-symbol)
                                        (list (quote quote) name)
                                        value))))

(define if (macro (lambda (i t e) (list (quote if-function) i (list (quote lambda) () t) (list (quote lambda) () e)))))

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
                       (quote lambda)
                       (map car bindings)
                       body)
                      (map (lambda (x) (car (cdr x))) bindings)))))

(define lookup (lambda (x alist)
                 (if alist
                     (let ((entry (car alist)))
                       (if (eq x (car entry))
                           (cdr entry)
                         (lookup x (cdr alist))))
                   ())))

(define print (lambda (x stream) (sequence (write-char #\
 stream) (write x stream) (write-char #\  stream) x)))

(define old-read read)
(define make-char-reader (lambda (char form) (cons char (lambda (stream) (list form (read stream))))))
(define comma-reader (lambda (stream)
                       (let ((c (peek-char stream)))
                         (list (if (eq c #\@)
                                   (sequence (read-char stream) (quote unquote-splice))
                                 (quote unquote)) (read stream)))))
(define eof-reader (lambda (stream) ((dynamic *read-eof*))))

(define *reader-dispatch-table* (list (make-char-reader #\' (quote quote         ))
                                      (make-char-reader #\` (quote quasiquote    ))
                                      (cons             #\, comma-reader          )
                                      (make-char-reader #\% (quote dynamic       ))
                                      (cons             ()  eof-reader           )))
(set read (lambda (stream)
            (let ((char (peek-char stream)))
              (let ((dispatch-function (lookup char *reader-dispatch-table*)))
                (if dispatch-function
                    (sequence (read-char stream) (dispatch-function stream))
                  (old-read stream))))))

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

(define load-stream (lambda (stream) (sequence (eval (read stream)) (load-stream stream))))

(define load-file (lambda (name)
                    (call/cc (lambda (k)
                               (dynamic-let ((*read-eof* (lambda ()
                                                           (k ()))))
                                            (load-stream (open-file name)))))))

(load-file 'loadable)

(define repl (lambda ()
               (sequence
                (print (eval (read %*standard-input*)) %*standard-output*)
                (write-char #\
 %*standard-output*)
                (repl))))
(repl)

END

(quit)

(quasiquote-form '((lambda ,(map car bindings)
                     ,body) ,@(map cadr bindings)))
(let ((bindings '((x a) (y b))) (body '(z x y))) `((lambda ,(map car bindings)
                                                     ,body) ,@(map cadr bindings)))

(define let- (macro (lambda (bindings body)
                      `((lambda ,(map car bindings)
                          ,body) ,@(map cadr bindings)))))

(let- ((x '1)) x)

((macro-function let) '((x ())) '(lambda () (toggle x)))

(define not (lambda (x) (if x () 'true)))
(define toggle (macro (lambda (x) `(set ,x (not ,x)))))
(define inverter (let ((x ())) (lambda () (toggle x))))

(inverter)
(inverter)
(list (inverter) (inverter))

'(define make-Ai-stream (lambda () (make-input-stream  (lambda ( )             #\A ))))
'(define make-Ao-stream (lambda () (make-output-stream (lambda (x) (write-char #\A)))))
'(define skip-whitespace)
'(define read-delimited-list (lambda (delimiter)
                               (sequence
                                (skip-whitespace)
                                (let ((next (read)))))))
