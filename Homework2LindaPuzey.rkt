;;
;;Linda Puzey
;;lpuzey
;;Homework 2
;;9/12/17
;;

;;Problem 1

;;a Library Item is
;;   Book
;;   Periodical
;;   Cd

;;(a Book is a (make-book String String Natural Boolean String Natural Natural)
;;interp: represents a book from the library where
;;title is the title of the book
;;author is the author/writer of the book
;;isbn is the book's isbn number
;;type? produces true if the book is hardcover and false if it is not
;;publisher is the company that published the book
;;year-pub is the year that the book was published
;;copies are the number of copies that the library owns

(define-struct book (title author isbn type? publisher year-pub copies))

;;these are the check-expects for the type? function

(check-expect (type? NARNIA)false)
(check-expect (type? (make-book "Harry Potter" "J.K. Rowling" 12345 "hardcover" "scholastics" 1997 1))true)

;;type? : Book -> Boolean
;;consumes a Book and produces true if it is hardcover and false if it is not

(define (type? book)(if (string=? "hardcover" (book-type? book))true false))

;;(a Periodical is a (make-periodical String String Natural)
;;interp: represents a periodical/newspaper/magazine from the library where
;;name is the name of the magazine
;;volume is the volume number
;;issue is the issue number

(define-struct periodical (name volume issue))


;;(a Cd is a (make-cd String String String Natural Natural Natural Natural)
;;interp: represents a cd from the library where
;;title is the title of the cd
;;author is the author or artist of the cd
;;narrator is the narrator of an audio book
;;num-disc is the number of discs included with the album
;;year-pub is the year the disc was published
;;play-time is the length of all the audio on the disc
;;copies is the number of copies the library owns

(define-struct cd (title author narrator num-disc year-pub play-time copies))


;;
;;Problem 2
;;

(define NARNIA (make-book "Narnia: The Lion the Witch and the Wardrobe" "C.S. Lewis" 0064404994 "paperback" "HarperCollins" 2008 10)) 

(define GAMEINFORMER (make-periodical "Gameinformer" 2017 291))

(define OWLCITY (make-cd "Ocean Eyes" "Adam Young" "Adam Young" 1 2009 50 10))

;;
;;Problem 3
;;

;;a Library Item is
;;   Book
;;   Periodical     
;;   Cd

; ;;library-item-fcn: Library-Item ->
; 
; (define (library-item-fcn lib-item)(cond
;                                          [(book? lib-item)(...(book-title  lib-item)
;                                                                   (book-author  lib-item)
;                                                                   (book-type?  lib-item)
;                                                                   (book-publisher  lib-item)
;                                                                   (book-year-pub  lib-item)
;                                                                   (book-copies lib-item))]
;                                          [(periodical? lib-item)(...(periodical-name lib-item)
;                                                                     (periodical-volume lib-item)
;                                                                     (periodical-issue lib-item))]
;                                          [(cd? lib-item)(...(cd-title lib-item)
;                                                             (cd-author lib-item)
;                                                             (cd-narrator lib-item)
;                                                             (cd-num-disc lib-item)
;                                                             (cd-year-pub lib-item)
;                                                             (cd-play-time lib-item)
;                                                             (cd-copies lib-item))]))

                                                                    

;;
;;Problem 4
;;

;;these are the test cases for the function contributor?
(check-expect (contributor? NARNIA "C.S. Lewis")true)
(check-expect (contributor? NARNIA "Love")false)
(check-expect (contributor? OWLCITY "Adam Young")true)
(check-expect (contributor? GAMEINFORMER "Gameinformer")false)

;; contributer: Library-Item String -> boolean
;;consumes a library item and a name and produces a boolean

(define (contributor? lib-item name)(cond
                                       [(and(book? lib-item)(string=? name (book-author lib-item)))true]
                                         [(and(cd? lib-item)(string=? name (cd-author lib-item)))true]
                                         [else false]))


;;
;;Problem 5
;;   

;;these are the test cases for the function add-copy
(check-expect (add-copy GAMEINFORMER)(make-periodical "Gameinformer" 2017 291))
(check-expect (add-copy NARNIA)(make-book "Narnia: The Lion the Witch and the Wardrobe" "C.S. Lewis" 0064404994 "paperback" "HarperCollins" 2008 11))
(check-expect (add-copy OWLCITY)(make-cd "Ocean Eyes" "Adam Young" "Adam Young" 1 2009 50 11))

;;add-copy: Libray-Item -> Libray-Item
;;consumes a library item and produces the item that's the same as the origianl, except that the number of copies of the item has been increased by 1
;;if the item is a periodical then it is unchanged

(define (add-copy lib-item)(cond
                            [(periodical? lib-item)(make-periodical(periodical-name lib-item)(periodical-volume lib-item)(periodical-issue lib-item))]
                            [(book? lib-item)(make-book(book-title lib-item)(book-author lib-item)(book-isbn lib-item)(book-type? lib-item)(book-publisher lib-item)(book-year-pub lib-item)(+ 1(book-copies lib-item)))]
                            [(cd? lib-item)(make-cd(cd-title lib-item)(cd-author lib-item)(cd-narrator lib-item)(cd-num-disc lib-item)(cd-year-pub lib-item)(cd-play-time lib-item)(+ 1(cd-copies lib-item)))]))


;;
;;Problem 6 
;;

;; a ListOfString is one of
;;  empty
;;  (cons String ListOfString)
;; interp:  ListOfString represents a list of strings

;;These are the test cases for the function character-count

(check-expect(character-count empty)0)
(check-expect(character-count (cons "dog" empty))3)
(check-expect(character-count (cons "dog" (cons "cat" empty)))6)

;;character-count: ListOfString -> Natural
;;consumes a ListOfStrings and produces the total number of characters in the list

(define (character-count alos)(cond
                                [(empty? alos)0]
                                [(cons? alos) (+(string-length (first alos))(character-count(rest alos)))]))

;;
;;Problem 7
;;

;;these are the test cases for the function numeric-strings

(check-expect (numeric-strings (cons "123" empty))(cons "123" empty))
(check-expect (numeric-strings (cons "lincoln" empty))empty)
(check-expect (numeric-strings (cons "lincoln"(cons "123" empty)))(cons "123" empty))

;;numeric-strings: ListOfString -> ListOfString
;;consumes a ListOfString and produces a list that contains only those strings from the original list that consist entirely of numeric characters

(define(numeric-strings alos)(cond
                               [(empty? alos)empty]
                               [(cons? alos)(if (string-numeric? (first alos))
                                                (cons(first alos)(numeric-strings (rest alos)))
                                                (numeric-strings (rest alos)))]))

;;
;;Problem 8
;;

;;these are the test cases for the helper function turn-to-characters

(check-expect (turn-to-character (cons "cat" empty))(cons #\c (cons #\a (cons #\t '()))))
(check-expect (turn-to-character empty)empty)

;;turn-to-character: ListOfString -> ListOfCharacters
;;consumes a list of strings and produces a list of characters from the strings in the list

(define (turn-to-character alos)
  (cond
   [(empty? alos)empty]
   [(cons? alos)(append (string->list (first alos))(turn-to-character (rest alos)))]))


;;these are the test cases for the helper function find-X

(check-expect (find-x empty)0)
(check-expect (find-x(cons #\x empty))1)

;;find-X: ListOfCharacters -> Natural
;;consumes a ListOfCharacters and produces the number of X's (upper or lowercase) in the list

(define (find-x alos)(cond
                       [(empty? alos)0]
                       [(cons? alos)(if(or (char=? #\x (first alos))(char=? #\X (first alos)))
                                       (+ 1(find-x(rest alos)))
                                       (+ 0 (find-x (rest alos))))]))
                                 

;;these are the test cases for the function count-X

(check-expect(count-X (cons "alienx" (cons "hexx" (cons "Maximoff" (cons "Andor" empty)))))4)
(check-expect(count-X empty)0)

;;count-X ListOfString -> Natural
;;consumes a ListOfString and counts the total number of X's (upper and lower case) that occur in all strings in the list

(define (count-X alos)(cond
                       [(empty? alos)0]
                       [(cons? alos)(find-x(turn-to-character alos))])) 
                                            
