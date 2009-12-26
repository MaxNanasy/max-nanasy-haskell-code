(define-symbol (quote list) (lambda xs xs))

(define-symbol (quote define) (macro (lambda (name value)
                                (list
                                 (quote define-symbol)
                                 (list (quote quote) name)
                                 value))))

(define if (macro (lambda (i t e)
                    (list
                     (quote if-function) i
                     (list (quote lambda) () t)
                     (list (quote lambda) () e)))))

(define not (lambda (x) (if x () (quote true))))

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
                      (map cadr bindings)))))

(define lookup (lambda (x alist)
                 (if alist
                     (let ((entry (car alist)))
                       (if (eq x (car entry))
                           (cdr entry)
                         (lookup x (cdr alist))))
                   ())))

(define make-char-reader (lambda (char form) (cons char (lambda (stream) (list form (read stream))))))
(define comma-reader (lambda (stream)
                       (let ((c (peek-char stream)))
                         (list (if (eq c #\@)
                                   (sequence (read-char stream) (quote unquote-splice))
                                 (quote unquote)) (read stream)))))
(define eof-reader (lambda (stream) ((dynamic *read-eof*))))
(define double-quote-reader (lambda (stream) (list-to-string (read-delimited-string #\" stream))))
(define read-delimited-string (lambda (char stream)
                                (let ((c (read-char stream)))
                                  (if (eq c char)
                                      ()
                                    (cons c (read-delimited-string char stream))))))
(define read-delimited-list (lambda (char stream)
                              (sequence
                               (skip-whitespace stream)
                               (let ((c (peek-char stream)))
                                 (if (eq c char)
                                     (sequence (read-char stream) ())
                                   (cons (read stream) (read-delimited-list char stream)))))))
(define open-parenthesis-reader (lambda (stream) (read-delimited-list #\) stream)))
(define close-parenthesis-reader (lambda (stream) ()))

(define skip-whitespace (lambda (stream)
                          (let ((c (peek-char stream)))
                            (if (whitespace? c)
                                (sequence (read-char stream) (skip-whitespace stream))
                              ()))))
(define whitespace? (lambda (c)
                      (member? c (quote (#\  #\
)))))

(define token-char? (lambda (c)
                      (if (whitespace? c)
                          ()
                        (not (lookup c *reader-dispatch-table*)))))

(define member? (lambda (x ys)
                  (if ys
                      (if (eq x (car ys))
                          (quote true)
                        (member? x (cdr ys)))
                    ())))

(define dynamic (macro (lambda (form) (list (quote dynamic-value) (list (quote quote) form)))))

(define append (lambda (xs ys)
                 (if xs
                     (cons (car xs) (append (cdr xs) ys))
                   ys)))
(define quasiquote-form (lambda (form)
                            (if (eq (type-of form) (quote cons))
                                (let ((head (car form)))
                                  (if (eq head (quote unquote))
                                      (list (quote list) (cadr form))
                                    (if (eq head (quote unquote-splice))
                                        (cadr form)
                                      (list (quote list) (list (quote append) (quasiquote-form head) (list (quote car) (quasiquote-form (cdr form))))))))
                              (list (quote quote) (list form)))))
(define quasiquote (macro (lambda (form) (list (quote car) (quasiquote-form form)))))

(define *reader-dispatch-table* (list (make-char-reader #\' (quote quote)           )
                                      (make-char-reader #\` (quote quasiquote)      )
                                      (cons             #\, comma-reader            )
                                      (make-char-reader #\% (quote dynamic)         )
                                      (cons             ()  eof-reader              )
                                      (cons             #\" double-quote-reader     )
                                      (cons             #\( open-parenthesis-reader )
                                      (cons             #\) close-parenthesis-reader)))

(define read (lambda (stream)
               (let ((char (peek-char stream)))
                 (let ((dispatch-function (lookup char *reader-dispatch-table*)))
                   (if dispatch-function
                       (sequence (read-char stream) (dispatch-function stream))
                     (if (whitespace? char)
                         (sequence (read-char stream) (read stream))
                       (intern (list-to-string (read-token stream)))))))))

(define read-token (lambda (stream)
                     (if (token-char? (peek-char stream))
                         (cons (read-char stream) (read-token stream))
                       ())))

(define load-stream (lambda (stream) (sequence (eval (read stream)) (load-stream stream))))

(define load-file (lambda (name)
                    (call/cc (lambda (k)
                               (with-dynamic-bindings (quote (*read-eof*)) (list (lambda ()
                                                                                   (k ())))
                                                      (lambda ()
                                                        (load-stream (open-file name))))))))

(load-file (list-to-string (list #\l #\o #\a #\d #\a #\b #\l #\e)))
