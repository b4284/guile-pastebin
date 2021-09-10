(use-modules (pastebin httpserver)
             (web server))

(define (run-pastebin args)
  (let ((data-dir (cadr args)))
    (if (not (file-exists? data-dir))
        (mkdir data-dir))
    (run-server (make-pastebin-handler data-dir) 'http '(#:addr 0))))
