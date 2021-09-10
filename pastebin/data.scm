(define-module (pastebin data))

(use-modules (srfi srfi-9)
             (srfi srfi-1)
             (ice-9 ftw)
             (ice-9 textual-ports))

(export <pb-data>
        <pb-entry>
        pb-entry-id
        pb-entry-text
        pb-data-open
        pb-data-close
        call-with-dir-as-pb-data
        pb-data-get-top
        pb-data-new-entry)

(define-record-type <pb-data>
  (make-pb-data dir)
  pb-data?
  (dir pb-data-dir set-pb-data-dir!))

(define-record-type <pb-entry>
  (make-pb-entry id text)
  pb-entry?
  (id pb-entry-id set-pb-entry-id!)
  (text pb-entry-text set-pb-entry-text!))

;; input: dir: string
;; output: <pb-data>
(define (pb-data-open dir)
  (make-pb-data dir))

(define (pb-data-close pb-data) #t)

(define (call-with-dir-as-pb-data dir p)
  (let ((pb-data (pb-data-open dir)))
    (let ((R (p pb-data)))
      (pb-data-close pb-data)
      R)))

(define (get-file-path pb-data filename)
  (string-append (pb-data-dir pb-data) "/" filename))

;; input: <pb-data>, integer
;; output: list of <pb-entry>
(define (pb-data-get-top pb-data n)
  (map
   (lambda (filename)
     (make-pb-entry
      filename
      (call-with-input-file (get-file-path pb-data filename)
        (lambda (port)
          (get-string-all port)))))
   (let ((file-ls (list-files pb-data)))
     (reverse (take-right file-ls (min n (length file-ls)))))))

;; input: <pb-data>, text
;; output: <pb-entry>
(define (pb-data-new-entry pb-data text)
  (let ((next-filename (get-next-filename pb-data)))
    (call-with-output-file (get-file-path pb-data next-filename)
      (lambda (port)
        (put-string port text)))
    (make-pb-entry next-filename text)))

(define (list-files pb-data)
  (scandir (pb-data-dir pb-data) (lambda (filename) (= (string-length filename) 5))))

(define (get-next-filename pb-data)
  (let ((entries (list-files pb-data)))
    (if (null? entries)
        "00000"
        (get-next-5digit (last entries)))))

(define (get-next-5digit str)
  (let A ((add #t)
          (char-int-ls (reverse (map char->integer (string->list str))))
          (result '()))
    (if (null? char-int-ls)
        (list->string (map integer->char result))
        (if add
            (let* ((hd (car char-int-ls)) (tl (cdr char-int-ls)))
              (if (= hd #x7A)
                  (A #t tl (cons #x30 result))
                  (A #f tl (cons (case hd
                                   ((#x39) #x41)
                                   ((#x5A) #x61)
                                   (else (1+ hd))) result))))
            (A #f '() (append (reverse char-int-ls) result))))))
