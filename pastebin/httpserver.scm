(define-module (pastebin httpserver))

(use-modules (web request)
             (web response)
             (web uri)
             (sxml simple)
             (pastebin data)
             (rnrs bytevectors)
             (ice-9 textual-ports)
             (ice-9 binary-ports)
             (ice-9 regex)
             (ice-9 match)
             (srfi srfi-1))

(export make-pastebin-handler)

(define (read-parts reqbody boundary)
  (define b2 (string-append "(\r\n)?--" boundary))
  (let A ((start 0) (parts '()))
    (let ((sm (string-match b2 reqbody start)))
      (if sm
          ;; +2 => CRLF
          (A (+ 2 (match:end sm 0))
             (cons (substring reqbody start (match:start sm 0)) parts))
          (cdr (reverse parts))))))

(define (get-new-pin str pin)
  (let ((crlfi (string-contains str "\r\n" pin)))
    (if crlfi crlfi (string-length str))))

(define (parse-part partstr)
  (let A ((headers '()) (pin 0))
    (let* ((newpin (get-new-pin partstr pin))
           (line (substring partstr pin newpin)))
      (if (string-null? line)
          (cons (reverse headers) (substring partstr (+ 2 newpin)))
          (A (cons line headers) (+ 2 newpin))))))

(define (get-content-dispo-name-from-headers headers)
  (let ((fl (find (lambda (line) (string-prefix-ci? "content-disposition: " line)) headers)))
    (if fl
        (let ((sm (string-match "name=(.*)" fl)))
          (if sm (string-trim-both (match:substring sm 1) #\") ""))
        "")))

(define (read-multipart-form-data reqbody boundary)
  (define parts (read-parts reqbody boundary))
  (map
   (lambda (part)
     (let ((pp (parse-part part)))
       (cons (get-content-dispo-name-from-headers (car pp)) (cdr pp))))
   parts))

(define (templatize title body)
  `(html (head
          (title ,title)
          (meta (@ (name "viewport") (content "width=device-width, initial-scale=1"))))
         (body ,@body)))

(define (post-handler request request-body pb-data-path)
  (if (eq? (request-method request) 'POST)
      (let* ((headers (request-headers request))
             (content-type-all (assq-ref headers 'content-type))
             (content-type (if content-type-all
                               (car content-type-all)
                               #f))
             (boundary (if (eq? content-type 'multipart/form-data)
                           (assq-ref (cdr content-type-all) 'boundary)
                           #f))
             (reqbody-string (utf8->string request-body))
             (form-data (if boundary
                            (read-multipart-form-data reqbody-string boundary)
                            #f))
             (new-pb-data (if form-data
                              (call-with-dir-as-pb-data
                               pb-data-path
                               (lambda (pb-data)
                                 (pb-data-new-entry pb-data
                                                    (assoc-ref form-data "text"))))
                              #f)))

        ;; determine what to respond
        (if (and new-pb-data
                 (assoc-ref form-data "showUrl"))

            ;; show url after paste
            (values (build-response
                     #:code 200
                     #:headers '((content-type . (text/plain))))
                    (lambda (port)
                      (let* ((hostp (assq-ref headers 'host)))
                        (put-string
                         port
                         (uri->string
                          (build-uri 'http
                                     #:host (car hostp)
                                     #:port (cdr hostp)
                                     #:path (format #f "/raw/~a\r\n"
                                                    (pb-entry-id new-pb-data))))))))

            ;; respond with 303 See Other
            (values (build-response
                     #:code 303
                     #:headers `((location . ,(build-uri-reference #:path "/"))))
                    (lambda (port) 1))))

      ;; INVALID request: access /post without HTTP POST
      (values (build-response #:code 400)
              (lambda (port) 1))))

(define (make-pastebin-handler data-path)
  (lambda (request request-body)
    (match (split-and-decode-uri-path (uri-path (request-uri request)))

      ;; URI: /post -- create paste
      (("post" . _)
       (post-handler request request-body data-path))

      ;; URI: /raw/<id> -- return raw content of the paste
      (("raw" pb-id)
       (values (build-response
                #:code 200
                #:headers '((content-type . (text/plain))))

               (lambda (port)
                 (call-with-input-file
                     ;; the file name
                     (call-with-dir-as-pb-data
                      data-path
                      (lambda (p) (pb-get-file-path p pb-id)))

                   ;; the input port
                   (lambda (inport)
                     (let A ((inport' inport))
                       (let ((bv (get-bytevector-n inport' 4096)))
                         (if (not (eof-object? bv))
                             (begin
                               (put-bytevector port bv)
                               (A inport'))))))))))

      ;; URI: * -- everything else -- show the top 5 paste list
      (_
       (values (build-response
                #:code 200
                #:headers '((content-type . (text/html))))

               (lambda (port)
                 (let* ((top5 (call-with-dir-as-pb-data
                               data-path
                               (lambda (pb-data) (pb-data-get-top pb-data 5))))
                        (sxml (templatize
                               "pastebin"
                               `((form (@ (method "post") (enctype "multipart/form-data")
                                          (action "/post"))
                                       (textarea (@ (name "text")) "")
                                       (input (@ (type "checkbox") (name "showUrl")
                                                 (id "showUrl") (value "1")))
                                       (label (@ (for "showUrl")) "Show raw URL after paste")
                                       (input (@ (type "submit"))))
                                 (table (@ (border 1)) (tr (th "id") (th "text") (th ""))
                                        ,(map (lambda (entry)
                                                `(tr (td ,(pb-entry-id entry))
                                                     (td ,(pb-entry-text entry))
                                                     (td
                                                      (a (@ (href
                                                             ,(format #f "/raw/~a"
                                                                      (pb-entry-id entry))))
                                                         "raw"))))
                                              top5))))))
                   (display "<!DOCTYPE html>\n" port)
                   (sxml->xml sxml port))))))))
