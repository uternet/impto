#!/usr/bin/env racket
#lang racket/base

(require racket/path)
(require racket/file)
(require racket/string)

(define err-log "impto-error.log")
(define impto-log "impto.log")

(define (error-log fmt)
  (display fmt)
  (display-to-file fmt err-log #:mode 'text #:exists 'append))

(define (add-log fmt)
  (display fmt)
  (display-to-file fmt impto-log #:mode 'text #:exists 'append))


(define (get-u16 in)
  (let ((h (read-byte in)))
    (+ (* h 256) (read-byte in))))


(define (date-from-exif re path)
  (define (find-exif in)
    (let ((marker (get-u16 in)))
      (let ((size (get-u16 in)))
        (if (not (<= #xffe0 marker #xffef))
            #f
            (if (= marker #xffe1)
                (let ((tiff (read-bytes (- size 8) in)))
                  (let ((m (regexp-match re tiff)))
                    (if m
                        (map bytes->string/latin-1 m) ;; 这里必要吗？
                        #f)))
                (begin
                  (read-bytes (- size 2) in)
                  (find-exif in)))))))
  
  (call-with-input-file path
    (lambda (in)
      (let ((header (get-u16 in)))
        (if (not (= header #xffd8))
            #f
            (find-exif in))))))

(define (date-from-file-name re path)
  (regexp-match re (file-name-from-path path)))

;; parsing date from Exif or file name, and convert it to path
;;   "2017:02:15" -> "2017/02/15"
;;   "20170215    -> "2017/02/15"
(define (get-date path)
  (let ((re (pregexp "(19[89]\\d|20[012]\\d)\\D?(0[1-9]|1[0-2])\\D?(0[1-9]|[12]\\d|3[01])")))
    (let ((ret (or (date-from-exif re path)
                   (date-from-file-name re path))))
      (if ret
          (string-join (cdr ret) "/")
          #f))))
  

(define (import-img img dest)
  (let ((date-string (get-date img)))
    (if date-string
        (let* ((sub-dir date-string)
               (dir-tree (string-append dest "/" sub-dir))
               (new-file (string-append dir-tree "/"
                                        (path->string (file-name-from-path img)))))
          (if (file-exists? new-file)
              (add-log (format "[Warning]~s already exists\n" new-file))
              (begin
                (make-directory* dir-tree)
                (copy-file img new-file)
                (add-log (format "~a -> ~a ok\n" img (path-only new-file))))))
        (error-log (format "[Failed]unknow date: ~a\n" img)))))

(define (jpeg? path)
  (let ((ext-name (path-get-extension path)))
    (member ext-name '(#".jpg" #".jpeg" #".JPG" #".JPEG"))))

(define (worker path type dest)
  (when (and (eq? type 'file) (jpeg? path))
    (import-img path dest))
  dest)

(define (go src dest)
  (fold-files worker dest src))


(define (usage)
  (printf "\n This program imports digital photos from 'source dir' to
 'dest dir' and stores them in a directory tree organized in the form
 'yyyy/mm/dd', the date-time information is read from the photo's buile-in
 Exif or file name.

 This program will recursively copy each JPEG file under the 'source dir'
 and all subdirectories. The 'source dir' parameter is optional, in which
 case program will look for JPEG images in the 'current working directory'\n

 If the attempt to get date info from Exif or file name fails, the
 file will be ignored.
 All failed log can be found at 'impto-error.log' in current directory. and
 All successful logs can be found at 'impto.log' too.

 Only files with '.jpg' '.jpeg' '.JPG' '.JPEG' extension will be copied.

  Usage:  ~a [source dir] <dest dir>\n\n" (file-name-from-path (find-system-path 'run-file))))

(let ((paths (vector->list (current-command-line-arguments))))
  (if (null? paths)
      (usage)
      (let ((src (if (= (length paths) 1)
                     (current-directory)
                     (car paths)))
            (dest (if (= (length paths) 1)
                      (car paths)
                      (cadr paths))))
        (cond ((not (directory-exists? src))
               (printf "directory ~a does not exists\n" src))
              ((not (directory-exists? dest))
               (printf "directory ~a does not exists\n" dest))
              (else
               (when (file-exists? err-log)
                 (delete-file err-log))
               (when (file-exists? impto-log)
                 (delete-file impto-log))
               (go src dest))))))
