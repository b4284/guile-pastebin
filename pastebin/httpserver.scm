(define-module (pastebin httpserver))

(use-modules (web request)
             (web response)
             (sxml simple)
             (pastebin data)
             (rnrs bytevectors)
             (ice-9 textual-ports)
             (ice-9 regex)
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

(define (make-pastebin-handler data-path)
  (lambda (request request-body)
    (if (eq? (request-method request) 'POST)
        (let* ((headers (request-headers request))
               (content-type-all (assq-ref headers 'content-type))
               (content-type (if content-type-all (car content-type-all) #f))
               (boundary (if (eq? content-type 'multipart/form-data)
                             (assq-ref (cdr content-type-all) 'boundary) #f))
               (reqbody-string (utf8->string request-body)))
          (if boundary
              (let ((form-data (read-multipart-form-data reqbody-string boundary)))
                (call-with-dir-as-pb-data
                 data-path
                 (lambda (pb-data)
                   (pb-data-new-entry pb-data
                                      (assoc-ref form-data "text"))))))))

    (values (build-response
             #:code 200
             #:headers `((content-type . (text/html))))

            (lambda (port)
              (let* ((top5 (call-with-dir-as-pb-data
                            data-path
                            (lambda (pb-data) (pb-data-get-top pb-data 5))))
                     (sxml (templatize
                            "pastebin"
                            `((form (@ (method "post") (enctype "multipart/form-data"))
                                    (textarea (@ (name "text")) "") (input (@ (type "submit"))))
                              (table (@ (border 1)) (tr (th "id") (th "text"))
                                     ,(map (lambda (entry) `(tr (td ,(pb-entry-id entry))
                                                                (td ,(pb-entry-text entry))))
                                           top5))))))
                (display "<!DOCTYPE html>\n" port)
                (sxml->xml sxml port))))))
