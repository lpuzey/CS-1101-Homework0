;; Linda Puzey
;; lpuzey
;; Homework 0

;;
;; Problem 1
;;
;; a

;(string-append (substring "CS110X" 0 5)(if (> 5 (+ 6 3)) "1" "2"))
;(string-append "CS110" (if (> 5 (+ 6 3)) "1" "2"))
;(string-append "CS110" (if (> 5 9) "1" "2"))
;(string-append "CS110" (if #false "1" "2"))
;(string-append "CS110" "2")
; "CS1102"

;b

;(define (triple n)(* n 3))
;
;(if (and (< (triple 3)(+ 5 2))true)(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(if (and (< (* 3 3)(+ 5 2))true)(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(if (and (< 9(+ 5 2))true)(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(if (and (< 9 7)true)(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(if (and #false true)(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(if #false(triple (+ 6(- 19 4)))(* 7 (triple 5)))
;(* 7 (triple 5))
;(* 7 (* 5 3))
;(* 7 15)
;105

;;
;; Problem 2
;;


(require 2htdp/image)

(define QUICKSILVER .)

(define ANT-MAN .)


(define (four-square pic)(above(beside pic(flip-horizontal pic))(beside(rotate 180 pic)(flip-vertical pic))))

(four-square QUICKSILVER)
(four-square ANT-MAN)
;Image -> Image
;Takes an image and rotates it four times

;;
;;Problem 3
;;

;a

(define TAX 1.0625)
(define DELIVERY 80)

(check-expect (cost-after-taxes 130)138.125)

(define (cost-after-taxes cost)(* TAX cost))
;number -> number
;put in the cost and you will get the cost with tax

;b

(check-expect (cost-with-delivery 130)218.125)

(define (cost-with-delivery cost)(+ DELIVERY(* TAX cost)))
;number -> number
;put in cost and you will get the cost with tax plus the delivery fee

;c

(check-expect (cost-with-tax-delivery 130)223.125)

(define (cost-with-tax-delivery cost)(* TAX (+ DELIVERY cost)))
;number -> number
;put in cost and you will get the cost plus the delivery fee with tax






