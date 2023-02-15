#lang racket/base

(require racket/file
         2htdp/image
         racket/list
         racket/string)

;;; Card Background

(define card-width 406)
(define card-height 584)

(define card-color (make-color 248 248 236))

(define (blank-card)
  (rectangle card-width card-height 'solid card-color))

(define border-width 4)

(define (card-border)
  (rectangle border-width card-height 'solid 'black))

(define row-width 4096)

(define (row-border)
  (rectangle row-width border-width 'solid 'black))

;;; Text Rendering

(define default-size 24)
(define text-color (make-color 0 0 0))

(define (write-text text text-size italics bold underline)
  (text/font text text-size text-color "Roboto Slab" 'default italics bold underline))

(define ctrl-char "$")

(define (detect-ctrl text-list)
  (let* ([text (car text-list)]
         [ctrl (string=? ctrl-char (substring text 0 1))]
         [italics (if (and ctrl (string-contains? text "i"))
                      'italics
                      'normal)]
         [bold (if (and ctrl (string-contains? text "b"))
                   'bold
                   'normal)]
         [underline (and ctrl (string-contains? text "u"))]
         [size (if ctrl
                   (string->number (substring text 1 3))
                   default-size)]
         [clean-list (if ctrl
                         (cdr text-list)
                         text-list)])
    (values clean-list size italics bold underline)))

(define (ct-down ct)
  (lambda (option1 option2)
    (set! ct (- ct 1))
    (if (< 0 ct)
        option1
        option2)))

(define (fill-text text-list)
  (let-values ([(clean-list size italics bold underline) (detect-ctrl text-list)])
    (let ([ct-down-size (ct-down 4)])
      (foldl (lambda (cur acc)             
               (above/align 'left
                            acc
                            (write-text cur (ct-down-size 36 size) italics bold underline)))
             empty-image
             clean-list))))

;; Text Formatting

(define text-limit 26)

(define (format-texts texts)
  (map (lambda (text) (break-long-text text text-limit))
       texts))

(define (break-long-text text limit)
  "Insert newline into text every limit characters"
  (let [(len (string-length text))]
    (if (> len limit)
        (string-append (newline-at-last-whitespace (substring text 0 limit))
                       (break-long-text (substring text limit len)
                                        limit))
        text)))

(define (newline-at-last-whitespace subst)
  (let* [(chars (string->list subst))
         (last-space (last (indexes-of chars #\space)))]
    (let-values ([(front back) (split-at chars (+ last-space 1))])
      (string-append (list->string front)
                     "\n"
                     (list->string back)))))

;;; Card Rendering

(define (fill-card texts)
  (overlay/align 'left 'top
                 (fill-text (format-texts texts))
                 (blank-card)))

;;; Cardsheet Rendering

(define (fill-row cards)
  (foldl (lambda (card acc)
           (beside acc
                   (fill-card card)
                   (card-border)))
         empty-image
         cards))

(define row-length 10)

(define (split-rows cards acc)
  (if (> row-length (length cards))
      (reverse (cons cards acc))
      (let-values ([(front back) (split-at cards row-length)])
        (split-rows back (cons front acc)))))

(define (fill-sheet cards)
  (let [(rows (split-rows cards '()))]
    (foldl (lambda (row acc)
             (above/align 'left
                          acc
                          (fill-row row)
                          (row-border)))
           empty-image
           rows)))

;;; Input

(define card-divider "--")

(define (split-on-divider input acc)
  (if (null? input)
      (reverse acc)
      (let [(next-divider (index-of input card-divider))]
        (let-values ([(front back) (split-at input next-divider)])
          (split-on-divider (cdr back) (cons front acc))))))

(define (load-and-divide-cards input-filename)
  (split-on-divider (file->lines input-filename) '()))

;;; Primary Call

(define (save-sheet input-filename)
  (save-image (fill-sheet (load-and-divide-cards input-filename))
              (string-append input-filename ".png")))
