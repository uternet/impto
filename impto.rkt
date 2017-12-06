#!/usr/bin/env racket
#lang racket/base

(require racket/path)
(require racket/file)
(require racket/list)
(require racket/string)
(require racket/cmdline)

(define SRC_DIR "./")
(define PHOTO_DIR (getenv "PHOTO_DIR"))
;(define SRC_DIR (string-append (getenv "PWD") "/"))
(define replace #f)   ; 强制替换开关

(define ash arithmetic-shift)
(define Exif-ifd-addr #x8769)
(define DateTimeOriginal #x9003)
(define DateTimeDigitized #x9004)

(define err-log "impto-error.log")
(define impto-log "impto.log")

(define (add-log log fmt)
  (display fmt)
  (display-to-file fmt log #:mode 'text #:exists 'append))

(define (get-u16 p)
  (let ([h8 (read-byte p)])
    (let ([l8 (read-byte p)])
      (+ (ash h8 8) l8))))

;; read-int : bytevector int int bool bool -> int
;; 从字节数组中的指定位置，读取指定长度的字节，转换成数字返回
(define (read-int bv offset len signed endian)
  ;(write (subbytes bv offset (+ offset len))) (newline)
  (integer-bytes->integer bv signed endian offset (+ offset len)))

;; get-exif : string -> bytevector
;; 从文件中分离出 tiff 数据结构，其中包含了全部的 exif 数据
;; 之所以要分离出单独的 tiff 结构体，是因为 exif 内部的所有偏移量
;; 都是基于 tiff 头部计算的
(define (get-exif fname)
  ;; find the 0xFFE1 marker
  (define (read-marker p)
    (let ([marker (get-u16 p)])
      (let ([size (get-u16 p)])
        (if (not (<= #xFFE0 marker #xFFEF))
            #f
;            (begin
;              (printf "Exif 0xFFE1 not foune: ~a\n" fname)
;              #f)
            (if (= marker #xFFE1)
                (begin
                  (read-bytes 6 p) ; drop "Exif\0\0"
                  (with-handlers ((exn:fail?
                                   (lambda (e)
                                     (printf "bad Exif: ~a\n" fname)
                                     #f)))
                    ;; Main entry, return the tiff datastruct
                    (read-bytes (- size 8) p)))
                (begin
                  (read-bytes (- size 2) p) ; ignore other APP marker
                  (read-marker p)))))))
  (call-with-input-file fname
    (lambda (p)
      (let ([SOI (get-u16 p)])
        (if (not (= SOI #xFFD8))
            (printf "[ERROR] not a JPEG file: ~a\n" fname)
            (read-marker p))))))

(define (size-of type)
  (list-ref '(#f 1 1 2 4 8 1 1 2 4 8 4 8) type))

(define (bv->string bv)
  (bytes->string/latin-1 (subbytes bv 0 (- (bytes-length bv) 1))))

(define (bv->int bv endian)
  (integer-bytes->integer bv #f endian))

;; search-ifd : bytes int bool bool -> bytes
;; 从指定的IFD中寻找指定的 tag, 并返回其值
;; 其中，bv 为要搜索的字节数组；ifd 为起始地址
(define (search-ifd bv ifd tag endian)
  (let ([number-of-entries (read-int bv ifd 2 #f endian)])
    (let ([result #f])
      (for ([i (range number-of-entries)])
        (let ([entry (+ ifd 2 (* i 12))])
          (let ([t (read-int bv entry 2 #f endian)])
            (when (= t tag)
              (let* ([type (read-int bv (+ entry 2) 2 #f endian)]
                     [count (read-int bv (+ entry 4) 4 #f endian)])
                (let ([real-data (if (> (* count (size-of type)) 4)
                                     (read-int bv (+ entry 8) 4 #f endian)
                                     (+ entry 8))])
                  (set! result (subbytes bv real-data (+ real-data (* count (size-of type)))))))))))
      result)))

;; search-date-in-exif : bytes -> string|#f
;; 先从 ifd0 中找到 exif-ifd 入口，再从 exif-ifd 中找到 DateTime
(define (search-date-in-exif bv)
  (let ([endian (if (= #x4d4d (read-int bv 0 2 #f #f))
                    #t
                    #f)])
    (let ([ifd0 (read-int bv 4 4 #f endian)])
      (let ([exif-ifd (bv->int (search-ifd bv ifd0 Exif-ifd-addr endian) endian)])
        (let ([slice (or (search-ifd bv exif-ifd DateTimeOriginal endian)
                         (search-ifd bv exif-ifd DateTimeDigitized endian))])
          (if slice
              (bv->string slice) ; return a string like "2000:01:01 10:32:11"
              #f))))))           ; or #f

;; 从 Exif 读取拍摄日期
(define (date-from-exif fname)
  (let ([tiff (get-exif fname)])
    (if (and tiff (> (bytes-length tiff) 8))
        ; 某些情况下会得到空的 tiff，而不是 #f 从而引发错误，所以加上一个长度判断
        (search-date-in-exif tiff)
        #f)))

;; 从文件名解析出日期
(define (date-from-file-name re path)
  (regexp-match re (file-name-from-path path)))

;; parsing date from Exif or file name, and convert it to path
;;   "2017:02:15" -> "2017/02/15"
;;   "20170215    -> "2017/02/15"
(define (parse-date path)
  (let ([date-string (or (date-from-exif path)
                         (file-name-from-path path))])
    (let ([e (pregexp "(19[89]\\d|20[012]\\d)\\D?(0[1-9]|1[0-2])\\D?(0[1-9]|[12]\\d|3[01])")])
      (let ([match-ret (regexp-match e date-string)])
        (if match-ret
            (string-join (cdr match-ret) "/")
            #f)))))

  
(define (copy-img img dest)
  (let ((date-string (parse-date img))
        (old-file (path->string img)))
    (if date-string
        (let* ((sub-dir date-string)
               (dir-tree (string-append dest "/" sub-dir))
               (new-file (string-append dir-tree "/"
                                        (path->string (file-name-from-path img)))))
          (if (file-exists? new-file)
              (if replace
                  (begin
                    (make-directory* dir-tree)
                    (copy-file img new-file #t)
                    (add-log impto-log (format "[Warning]~a -> ~a |replaced\n" old-file dir-tree)))
                  (add-log impto-log (format "[Warning]~a -> ~a |skiped\n" old-file dir-tree)))
              (begin
                (make-directory* dir-tree)
                (copy-file img new-file)
                (add-log impto-log (format "~a -> ~a |success\n" old-file dir-tree)))))
        (add-log err-log (format "[Error]~a |Failed to get date\n" old-file)))))

(define (jpeg? path)
  (let ((ext-name (path-get-extension path)))
    (member ext-name '(#".jpg" #".jpeg" #".JPG" #".JPEG"))))

(define (worker path type dest)
  (when (and (eq? type 'file) (jpeg? path))
    (copy-img path dest))
  dest)


(define (start src target)
  (when (file-exists? err-log)
    (delete-file err-log))
  (when (file-exists? impto-log)
    (delete-file impto-log))
  (fold-files worker target src))

(command-line
 #:once-each
 [("-s" "--source") src "Source folder, default current work directory" (set! SRC_DIR src)]
 [("-d" "--dest") dest "Destination folder" (set! PHOTO_DIR dest)]
 [("-r" "--replace") "Replace the file if exists!" (set! replace #t)]
 #:args ()
 (if PHOTO_DIR
     (start SRC_DIR PHOTO_DIR)
     (printf "\n  You need to specify the destination folder for photos\n
via '-d' option or the environment variable 'PHOTO_DIR'\n\nnothing to do!\n")))


;; dump tiff data to a file
;(let ((paths (vector->list (current-command-line-arguments))))
;  (let ((fname (car paths)))
;    (call-with-output-file (string-append fname ".tif")
;      (lambda (out)
;        (let ((tiff (get-exif fname)))
;          (write-bytes tiff out))))))


