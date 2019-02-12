;;
;;Linda Puzey
;;lpuzey
;;10/3/17
;;Homework 5
;;

;;
;;Problem 1
;;


;;a River is a (make-river String Number Number ListOfTributaries)
;;interp: represents a river
;;name: is the name of the river
;;ph: is the ph level of the water
;;DO: is the dissolved oxygen levels (in mg/L)
;;tributaries: is a list of the tributaries that the river feeds into

(define-struct river(name ph DO tributaries))

;;Tributaries is one of
;;empty
;;(cons River ListOfRiver)


;;
;;Problem 2
;;


(define TORI (make-river "Tori" 11 38
                         (cons(make-river "Madison" 14 32 (cons(make-river "Vida" 21 32 empty)empty))
                               (cons (make-river "Kevin" 18 22 empty)empty))))

;;
;;Problem 3
;;

; ;;river-fcn: River ->
; (define (river-fcn a-river)
;   (...(river-name a-river)
;       (river-ph a-river)
;       (river-BO a-river))
;   (river-tributaries a-river))
; 
; ;;fcn-lot: ListOfTributaries ->
; (define(fcn-lot lot)(cond
;                       [(empty? lot)empty]
;                       [(cons? lot)(...(river-fcn (first lot)
;                                                (fcn-lot(rest lot))))]))


;;
;;Problem 4
;;

;;these are the test cases for the function more-acidic-than

(check-expect (more-acidic-than TORI 40)4)
(check-expect (more-acidic-than TORI 15)2)
(check-expect (more-acidic-than TORI 12)1)
(check-expect (more-acidic-than TORI 0)0)
              
;;more-acidic-than: RiverSystem Number -> Number
;;consumes a river system and a pH level and produces the number of rivers
;;in the system that have a pH level lower than the given level
;;ph level < num
;;num> ph level


(define (more-acidic-than a-river num)(if(> num (river-ph a-river))
                                            (+ 1(more-acidic-list(river-tributaries a-river)num))
                                            (+ 0(more-acidic-list(river-tributaries a-river)num))))

;;these are the test cases for the helper function more-acidic-than-list

(check-expect(more-acidic-list empty 5)0)
(check-expect(more-acidic-list (cons(make-river "Madison" 14 32 (cons(make-river "Vida" 21 32 empty)empty))
                               (cons (make-river "Kevin" 18 22 empty)empty))20)2)

;;more-acidic-list: ListOfTributaries Number -> Number
;;consumes a ListOfTributaries and a Number and produces the number of rivers in the system

(define (more-acidic-list lot num)(cond
                                [(empty? lot)0]
                                [(cons? lot)(+ 0(more-acidic-than (first lot)num)
                                            (more-acidic-list (rest lot)num))]))

;;
;;Problem 5
;;

;;these are the test cases for the function rivers-at-risk

(check-expect (river-at-risk TORI)(list "Tori" "Madison" "Vida" "Kevin"))
(check-expect (river-at-risk(make-river "Dusin" 7 5 empty))(list "Dusin"))
(check-expect (river-at-risk(make-river "Shane" 6 5 empty))(list "Shane"))


;;rivers-at-risk: RiverSystem -> ListOfString
;;consumes a river system and produces a ListOfString
;;The function produces a list of the names of all rivers in the river system that have a pH level below 6.5,
;;or a pH level above 8.5, or a DO level less than 6 mg/L.

(define (river-at-risk a-river)(cond
                                 [(< (river-ph a-river)6.5)(cons(river-name a-river)(river-at-list(river-tributaries a-river)))]
                                 [(> (river-ph a-river)8.5)(cons(river-name a-river)(river-at-list(river-tributaries a-river)))]
                                 [(< (river-DO a-river)6 ) (cons(river-name a-river)(river-at-list(river-tributaries a-river)))]))
                                          
;;these are the test cases for the helper function river-at-list

(check-expect(river-at-list empty)empty)
(check-expect (river-at-list (cons(make-river "Madison" 14 32 (cons(make-river "Vida" 21 32 empty)empty))
                               (cons (make-river "Kevin" 18 22 empty)empty)))(list "Madison" "Vida" "Kevin"))

;;river-at-list: ListOfTributaries -> ListOfString
;;consumes a ListOfTributaries and produces a ListOfString

(define (river-at-list lot)(cond
                                [(empty? lot)empty]
                                [(cons? lot)(append(river-at-risk (first lot))
                                            (river-at-list (rest lot)))]))


;;
;;Problem 6
;;

;;These are the test cases for the function lower-all-ph

(check-expect (lower-all-ph TORI)(make-river "Tori" 10.9 38 (list (make-river "Madison" 13.9 32 (list (make-river "Vida" 20.9 32 empty))) (make-river "Kevin" 17.9 22 empty))))
(check-expect(lower-all-ph (make-river "Kevin" 18 22 empty))(make-river "Kevin" 17.9 22 empty))

;;lower-all-ph: RiverSystem -> RiverSystem
;;consumes a river system and produces a river system
;;The river system that is produced is the same as the original,
;;except that the pH of all the rivers in the system has been lowered by 0.1


(define(lower-all-ph a-river)(make-river(river-name a-river)
                                        (- (river-ph a-river) 0.1)
                                        (river-DO a-river)
                                        (lower-all-list(river-tributaries a-river))))

;;these are the test cases for the helper function lower-all-list

(check-expect (lower-all-list empty)empty)
(check-expect (lower-all-list (cons(make-river "Madison" 14 32 (cons(make-river "Vida" 21 32 empty)empty))
                               (cons (make-river "Kevin" 18 22 empty)empty)))(cons(make-river "Madison" 13.9 32 (cons(make-river "Vida" 20.9 32 empty)empty))
                               (cons (make-river "Kevin" 17.9 22 empty)empty)))

;;lower-all-list: ListOfTributaries -> ListOfTributaries
;;consumes a ListOfTributaries and produces one the same as the original
;;except that the ph of all the rivers in the system has been lowered by 0.1

(define (lower-all-list lot)(cond
                                [(empty? lot)empty]
                                [(cons? lot)(cons(lower-all-ph(first lot))
                                            (lower-all-list (rest lot)))]))

;;
;;Problem 7
;;

;;these are the test cases for the function find-subsystem

(check-expect (find-subsystem "Kevin" TORI)(make-river "Kevin" 18 22 empty))
(check-expect (find-subsystem "Tori" TORI)(make-river "Tori" 11 38 (list (make-river "Madison" 14 32 (list (make-river "Vida" 21 32 empty))) (make-river "Kevin" 18 22 empty))))
(check-expect (find-subsystem "Peter" TORI)false)

;;find-subsystem: String RiverSystem -> RiverSystem or false
;;consumes the name of a river and a river system and produces either a river system or false
;;The function returns the portion of the original river system that has the named river as its root
;;If there is no river in the system with the given name, the function returns false
;;river(name ph DO tributaries)

(define (find-subsystem riv-name a-river)(cond
                                       [(river? a-river)(if (string=? riv-name (river-name a-river))
                                                           a-river
                                                           (find-sublist riv-name(river-tributaries a-river)))]))

;;these are the test cases for the helper function find-sublist

(check-expect (find-sublist "Linda" empty)false)
(check-expect (find-sublist "Vida" (cons(make-river "Madison" 14 32 empty)empty))false)                             
(check-expect (find-sublist "Madison" (cons(make-river "Madison" 14 32 empty)empty))(make-river "Madison" 14 32 empty))

;;find-sublist: String ListOfTributaries -> RiverSystem or false
;;consumes the name of a river and a ListOfTributaries and produces either a river system or false

(define (find-sublist riv-name lot)(cond
                                [(empty? lot)false]
                                [(cons? lot)(if(river? (find-subsystem riv-name (first lot)))
                                            (find-subsystem riv-name(first lot))
                                             (find-sublist riv-name (rest lot)))]))

;;
;;Problem 8
;;

;;these are the test cases for the function count-acidic-tributaries-of

(check-expect (count-acidic-tributaries-of TORI "Madison")0)
(check-expect (count-acidic-tributaries-of TORI "Kevin")0)
(check-expect (count-acidic-tributaries-of TORI "Mike")0)

;;count-acidic-tributaries-of: RiverSystem String -> Natural
;;consumes a river system and the name of a river and
;;produces a count of the number of tributaries of the named river which have a pH < 6.5

(define (count-acidic-tributaries-of a-river riv-name)(cond
                       [(false? (find-subsystem riv-name a-river))0]
                       [(river? (find-subsystem riv-name a-river))
                       (more-acidic-list(river-tributaries(find-subsystem riv-name a-river))6.5)]))

                       
