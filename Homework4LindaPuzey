;;
;;Linda Puzey
;;lpuzey
;;9/26/17
;;Homework 4
;;

;;
;;Problem 1
;;

;;a Contact is a (make-contact String String String)
;;interp: represents a customer's contact information
;;name: is the name of the customer
;;address: is the of the customer
;;email: is the customer's email

(define-struct contact(name address email))

(define NATHAN(make-contact "Nathan Drake" "2011 Shambala st" "ohcrap@naughtydog.com"))
(define DELSIN(make-contact "Delsin Rowe" "2014 Seattle dr" "deadbrother@suckerpunch.com"))


;;a Charge is a (make-charge String String Number)
;;interp: represents the charge information for a cardholder
;;name: is the name of the business
;;where: is where the credit card was used
;;amount: is the amount that was charged

(define-struct charge(name where amount))

(define NATHAN-CHAR(make-charge "Nathan Drake" "Libertalia" 2020))
(define DELSIN-CHAR(make-charge "Delsin Rowe" "Washington" 2000))

;;a ListOfCharge is one of
;;empty
;;(cons Charge empty)

(cons NATHAN-CHAR(cons DELSIN-CHAR empty))
empty

;;
;;Problem 2
;;

; ;;contact-fcn: Contact ->
; (define (contact-fcn a-contact)
;   (...(contact-name a-contact)
;       (contact-address a-contact)
;       (contact-email a-contact))]))
; 
; ;;charge-fcn: Charge ->
; (define (charge-fcn a-charge)
;   (...(charge-name a-charge)
;       (charge-where a-charge)
;       (charge-amount a-charge))]))
; 
; ;;fcn-loc: ListOfCharge ->
; (define(fcn-loc loc)(cond
;                       [(empty? loc)empty]
;                       [(cons? loc)(...(fcn-loc (first loc)
;                                                (fcn-loc(rest loc))))]))
; 


;;
;;Problem 3
;;

;; a BST is one of
;;   false
;;   CustNode

(define-struct customer (card-number contact limit charges left right))

;; a CustNode is a (make-customer Natural Contact Number ListOfCharge BST BST)
;;interp: represents a customer
;;card-number is a credit card's number
;;Contact is a (make-contact String String String) and represents a customer's contact information
;;Limit is the amount of money charged before it is considered an outstanding charge
;;Charges is is a ListOfCharge and charges is a (make-charge String String Number)
;;left is the left node
;;right is the right node

;;INVARIANT
;; card number of the left node is less than the card number of the parent (top) node
;; card number of the right node is greater than the card number of the parent (top) node


;;
;;Problem 4
;;

;;EMPTY: Binary Search Tree with nothing in it

(define EMPTY false)

;;DINO-CHARGE-TREE: Bianary Search Tree of all the Dino Charge Members
;;list is sorted by card-number

(define DINO-CHARGE-TREE
  (make-customer 50 (make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com") 200 (cons(make-charge "Tyler" "Amber Beach" 150)empty)
                 (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com") 100 (cons(make-charge "Riley" "Amber Beach" 150)empty)
                                (make-customer 15 (make-contact "Koda" "Amber Beach" "blue@gmail.com") 50 (cons(make-charge "Koda" "Amber Beach" 150)empty) false false)false)
                 (make-customer 75 (make-contact "Shelby Watkins" "Amber Beach" "pink@gmail.com") 300 (cons(make-charge "Shelby" "Amber Beach" 150)empty) false
                                (make-customer 85 (make-contact "Chase Randall" "Amber Beach" "black@gmail.com") 350 (cons(make-charge "Chase" "Amber Beach" 150)empty) false false))))


;;
;;Problem 5
;;

; cus-fcn ->
; 
; (define (cus-fcn acus)(cond
;                           [(false? acus)(...)]
;                            [(customer? acus)
;                                 (...(customer-card-number acus)
;                                 (customer-contact acus))
;                                 (customer-limit acus)
;                                 (customer-charges acus))
;                                 (cus-fcn (customer-left acus))
;                                 (cus-fcn (customer-right acus)))]))


;;
;;Problem 6
;;

;;these are the test cases for the function count-big-limits 

(check-expect(count-big-limits DINO-CHARGE-TREE 199)3)
(check-expect(count-big-limits DINO-CHARGE-TREE 0)5)
(check-expect(count-big-limits false 0)0)                              

;;count-big-limits: BST Natural -> Natural
;;consumes a binary search tree and a credit limit amount, and counts the number of customers in the tree who have a credit limit higher than the given amount


(define(count-big-limits bst limit)(cond
                                     [(boolean? bst) 0]
                                     [(customer? bst)(if (> (customer-limit bst) limit)
                                                         (+ 1 (count-big-limits (customer-left bst) limit)
                                                            (count-big-limits (customer-right bst) limit))
                                                         (+ 0 (count-big-limits (customer-left bst) limit)
                                                            (count-big-limits (customer-right bst) limit)))]))

;;
;;Problem 7
;;

;;these are the test cases for the helper function total-charge

(check-expect (total-charge empty)0)
(check-expect (total-charge (cons NATHAN-CHAR(cons DELSIN-CHAR empty)))4020)

;;total-charge: ListOfCharge -> Natural
 
(define(total-charge loc) (cond
                            [(empty? loc)0]
                            [(cons? loc)(+ (charge-amount (first loc))
                                           (total-charge(rest loc)))]))

;;these are the test cases for the function any-over-limit?

(check-expect(any-over-limit? false)false)
(check-expect(any-over-limit? DINO-CHARGE-TREE)true)
(check-expect(any-over-limit? (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com") 100 (cons(make-charge "Riley" "Amber Beach" 50)empty) false false))false)

;;any-over-limit?: BST -> Boolean
;;consumes a binary search tree and produces true if any customer in the tree has
;;an outstanding balance greater than their credit limit
;;A customer's balance is calculated by summing the amounts
;;of all purchases that they've charged


(define (any-over-limit? bst)(cond
                               [(false? bst)false]
                               [(customer? bst)(if (> (total-charge (customer-charges bst))(customer-limit bst))
                                                   true
                                                   (or (any-over-limit? (customer-left bst))
                                                       (any-over-limit? (customer-right bst))))]))
                                           
                                              
                                                                       
;;
;;Problem 8
;;

;;these are the test cases for the funtion increase-limit

(check-expect (increase-limit false 0 20)false)
(check-expect (increase-limit DINO-CHARGE-TREE 50 100)(make-customer 50(make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com") 300(list (make-charge "Tyler" "Amber Beach" 150))
                                                                     (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com")100(list (make-charge "Riley" "Amber Beach" 150))
                                                                                    (make-customer 15 (make-contact "Koda" "Amber Beach" "blue@gmail.com") 50 (list (make-charge "Koda" "Amber Beach" 150)) false false) false)
                                                                     (make-customer 75 (make-contact "Shelby Watkins" "Amber Beach" "pink@gmail.com") 300
                                                                                    (list (make-charge "Shelby" "Amber Beach" 150))false
                                                                                    (make-customer 85 (make-contact "Chase Randall" "Amber Beach" "black@gmail.com") 350 (list (make-charge "Chase" "Amber Beach" 150)) false false))))
(check-expect(increase-limit
              (make-customer 50(make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com") 300(list (make-charge "Tyler" "Amber Beach" 150))
                             (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com")100(list (make-charge "Riley" "Amber Beach" 150)) false false)false) 25 20)
              (make-customer 50 (make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com") 300 (list (make-charge "Tyler" "Amber Beach" 150))
                             (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com")120(list (make-charge "Riley" "Amber Beach" 150)) false false)false))

;;increase-limit: BST Natural Natural -> BST
;;consumes a binary search tree, a credit card number, and an amount of money.
;;The function returns a tree the same as the original,
;;except that the credit limit of the customer with the given credit card number has been increased by the given amount

(define (increase-limit bst card-number money)(cond
                                                [(false? bst)false]
                                                [(customer? bst)(if (= card-number (customer-card-number bst))
                                                                    (make-customer (customer-card-number bst)
                                                                                   (customer-contact bst)
                                                                                   (+ (customer-limit bst) money)
                                                                                   (customer-charges bst)
                                                                                   (customer-left bst)
                                                                                   (customer-right bst))
                                                                    (make-customer (customer-card-number bst)
                                                                                   (customer-contact bst)
                                                                                   (customer-limit bst)
                                                                                   (customer-charges bst)
                                                                                   (increase-limit (customer-left bst)card-number money)
                                                                                   (increase-limit (customer-right bst) card-number money)))]))

;;
;;Problem 9
;;

;;these are the test cases for the funtion add-customer

(check-expect(add-customer false 66 (make-contact "Kendall Morgan" "Amber Beach" "purple@gamil.com"))
(make-customer 66 (make-contact "Kendall Morgan" "Amber Beach" "purple@gamil.com") 25000 empty false false))

(check-expect(add-customer DINO-CHARGE-TREE 90 (make-contact "Sir Ivan" "Zandar" "gold@gmail.com"))
(make-customer 50(make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com")200(list (make-charge "Tyler" "Amber Beach" 150))
 (make-customer 75(make-contact "Shelby Watkins" "Amber Beach" "pink@gmail.com") 300(list (make-charge "Shelby" "Amber Beach" 150))
  (make-customer 85 (make-contact "Chase Randall" "Amber Beach" "black@gmail.com")350(list (make-charge "Chase" "Amber Beach" 150))
   (make-customer 90 (make-contact "Sir Ivan" "Zandar" "gold@gmail.com") 25000 empty false false)false) false)
 (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com")100(list (make-charge "Riley" "Amber Beach" 150))
  (make-customer 15 (make-contact "Koda" "Amber Beach" "blue@gmail.com") 50 (list (make-charge "Koda" "Amber Beach" 150)) false false)false)))

(check-expect(add-customer DINO-CHARGE-TREE 32 (make-contact "Prince Philip" "Zandar" "graphite@gmail.com"))
(make-customer 50(make-contact "Tyler Navaro" "Amber Beach" "red@gmail.com")200(list (make-charge "Tyler" "Amber Beach" 150))
 (make-customer 75 (make-contact "Shelby Watkins" "Amber Beach" "pink@gmail.com") 300 (list (make-charge "Shelby" "Amber Beach" 150))false
  (make-customer 85 (make-contact "Chase Randall" "Amber Beach" "black@gmail.com") 350 (list (make-charge "Chase" "Amber Beach" 150)) false false))
 (make-customer 25 (make-contact "Riley Griffin" "Amber Beach" "green@gmail.com") 100 (list (make-charge "Riley" "Amber Beach" 150))
  (make-customer 32 (make-contact "Prince Philip" "Zandar" "graphite@gmail.com") 25000 empty false false)
  (make-customer 15 (make-contact "Koda" "Amber Beach" "blue@gmail.com") 50 (list (make-charge "Koda" "Amber Beach" 150)) false false))))

;;add-customer: BST Natuaral Contact -> BST
;;consumes a binary search tree, a credit card number, and a customer's contact information.
;;The function returns a tree with a new customer added.
;;The new customer has the given credit card number
;;and contact information, a credit limit of $25,000, and an empty list of charges.

(define (add-customer bst card-number contact)(cond
                                                [(false? bst)(make-customer card-number
                                                                             contact
                                                                             25000
                                                                             empty
                                                                             false
                                                                             false)]
                                                [(customer? bst)(if (> card-number (customer-card-number bst))
                                                                    (make-customer (customer-card-number bst)
                                                                                   (customer-contact bst)
                                                                                   (customer-limit bst)
                                                                                   (customer-charges bst)
                                                                                   (add-customer(customer-right bst) card-number contact)
                                                                                   (customer-left bst))
                                                                                    
                                                                    (make-customer (customer-card-number bst)
                                                                                   (customer-contact bst)
                                                                                   (customer-limit bst)
                                                                                   (customer-charges bst)
                                                                                   (customer-right bst)
                                                                                   (add-customer(customer-left bst) card-number contact)))]))




