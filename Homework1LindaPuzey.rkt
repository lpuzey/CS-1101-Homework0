;;
;;Linda Puzey
;;lpuzey
;;Homework 1
;;September 6, 2017
;;

;;
;;Problem 1
;;

;;a Date is (make-date Natural Natural Natural)
;;interp: a Date has
;;year is a year
;;month of the year (1-12)
;;day is the day of the month


(define-struct date(year month day))

(define HALLOWEEN(make-date 2017 10 31))
(define CHRISTMAS(make-date 2017 12 25))
(define EASTER(make-date 2018 4 1))

(define-struct film(title genre runtime opendate receipt))

;;a Film is a (make-film String String Natural Date Natural)
;;interp: a Film has
;;title is the title of the Film
;;genre is the genre of the Film
;;runtime is how long the Film is (in minutes)
;;opendate is the date the Film came out it theaters
;;receipt is how much money the Film made at the boxoffice (in dollars)

(define ANT-MAN (make-film "ant-man" "action" 118 (make-date 2015 6 29) 519300000)) 
(define POWER-RANGERS (make-film "power-rangers" "action" 124 (make-date 2017 3 24) 142100000)) 
(define KUNG-FU-PANDA (make-film "kung-fu-panda" "comedy" 92 (make-date 2008 6 6) 215400000)) 
(define LINDA-ADVENTURE (make-film "the-amazing-adventures-of-linda" "comedy" 30 (make-date 2018 4 1) 142100000))

;;
;;Problem 2 
;;

;;signatures
;;make-film: (String String String Natural Date Natural)-> Film
;;film-title: Film -> String
;;film-genere: Film -> String
;;film-runtime: Film -> Natural
;;film-opendate: Film -> (Natural Natural Natural)
;;film-receipt: Film -> Natural
;;film?: Anything -> Boolean


;;
;;Problem 3
;;

;;these are the test cases for the function short 

(check-expect (short ANT-MAN 100) false)
(check-expect (short POWER-RANGERS 144) true)
(check-expect (short KUNG-FU-PANDA 100) true)

;; short: Film Natural -> Boolean
;;short is a helper function for short-comedy?
;;comsumes a film and a length then will produce a Boolean
;;length is your limit on the runtime

(define(short film length)(if (< (film-runtime film) length)true false))


;;test cases for comedy function

(check-expect (comedy? LINDA-ADVENTURE) true)
(check-expect (comedy? POWER-RANGERS) false)
(check-expect (comedy? KUNG-FU-PANDA) true)

;;comedy?: Film -> Boolean
;;comedy? is a helper funtion for short-comedy
;;consumes a Film and will produce a Boolean
;;determines whether the Film's genere is a comedy or not
;;true if comedy false if anything else

(define(comedy? film)(if (string=? "comedy" (film-genre film))true false))



;;test cases for short-comedy? function

(check-expect (short-comedy? ANT-MAN 100) false)
(check-expect (short-comedy? POWER-RANGERS 144) false)
(check-expect (short-comedy? KUNG-FU-PANDA 100) true)

;;short-comedy: Film Natural -> Boolean
;;consumes a Film and a length and produces a Boolean
;;produces true if the Film's genere is a comedy and the runtime is less than the length provided
;; interp: length is your limit on the runtime

(define (short-comedy? film length)(if
                                    (and (comedy? film) (short film length) ) true false))


;;
;;Problem 4
;;

;;These are test cases for the function film-with-more-receipts

(check-expect (film-with-more-receipts ANT-MAN POWER-RANGERS)ANT-MAN)
(check-expect (film-with-more-receipts POWER-RANGERS KUNG-FU-PANDA)KUNG-FU-PANDA)
(check-expect (film-with-more-receipts POWER-RANGERS LINDA-ADVENTURE)POWER-RANGERS)

;;film-with-more-receipts: Film Film -> Film
;;consumes two Films and produces the Film with more boxoffice receipts
;;if both receipts are equal, then the function will produce either
;;film-with-more-receipts: Film Film -> Film

(define (film-with-more-receipts film1 film2)(if (>= (film-receipt film1)(film-receipt film2))film1 film2))


;;
;;Problem 5
;;

;;these are the test cases for the function add-time-for-trailers
(check-expect (add-time-for-trailers ANT-MAN 1) (make-film "ant-man" "action" 121 (make-date 2015 6 29) 519300000))
(check-expect (add-time-for-trailers POWER-RANGERS 2)(make-film "power-rangers" "action" 130 (make-date 2017 3 24) 142100000))
(check-expect (add-time-for-trailers KUNG-FU-PANDA 3)(make-film "kung-fu-panda" "comedy" 101 (make-date 2008 6 6) 215400000))

;;add-time-for-trailers: Film Natural-> Film
;;consumes Film and the number of trailers and produces a film,
;;but the runtime is now 3 minutes longer for each trailer

(define (add-time-for-trailers film trailers)
  (make-film (film-title film) (film-genre film)
             (+ (film-runtime film) (* 3 trailers))
             (film-opendate film) (film-receipt film))) 

;;
;;Problem 6
;;

;;these are the test cases for the funtion opens-before?

(check-expect (opens-before? ANT-MAN (make-date 2016 7 30))true)
(check-expect (opens-before? ANT-MAN (make-date 2014 6 29))false)
(check-expect (opens-before? POWER-RANGERS (make-date 2017 4 20))true)
(check-expect (opens-before? POWER-RANGERS (make-date 2017 2 24))false)
(check-expect (opens-before? KUNG-FU-PANDA (make-date 2008 6 8))true)
(check-expect (opens-before? KUNG-FU-PANDA (make-date 2008 6 5))false)
(check-expect (opens-before? KUNG-FU-PANDA (make-date 2008 6 6))false)


;;compare-dates?: Date Date -> Boolean
;;compare-dates? is a helper function for opens-before?
;;consumes a two Dates and tells you if the date1 is before date2
;;true if date1 is before date2
;;false if it is not

(define (compare-dates? date1 date2)
  (cond [(< (date-year date1)(date-year date2))true]
        [(> (date-year date1)(date-year date2))false]
        [(= (date-year date1)(date-year date2))
           (cond
             [(<(date-month date1)(date-month date2))true]
             [(>(date-month date1)(date-month date2))false]
             [(=(date-month date1)(date-month date2))
                (cond
                  [(< (date-day date1)(date-day date2))true]
                   [(> (date-day date1)(date-day date2))false]
                   [(= (date-day date1)(date-day date2))false])])]))

;;opens-before?: Film Date -> Boolean
;;consumes a Film and Date and tells you if the movie comes out before the date you put in
;;true if the movie release date is before the given
;;false if it is not

(define(opens-before? film date)(compare-dates? (film-opendate film)(make-date (date-year date)(date-month date)(date-day date))))
  
