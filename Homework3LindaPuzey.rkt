;;
;;Linda Puzey
;;lpuzey
;;9/19/17
;;Homework 3
;;

;;
;;Problem 1
;;

;;TimeOfDay is one of
;; "daytime"
;; "primetime"
;; "off-hour"


;;an Ad is a (make-ad String String Natural Natural Boolean TimeOfDay Natural)
;;interp: represents an Ad on tv where
;;product: is the name of the product the ad is for 
;;duration: is the duration of the ad (in seconds)
;;cost: is the cost to produce the ad (in thousands of dollars)
;;national?: is whether or not the ad is to be aired nationally (as opposed to locally)(true  represents national and false to represent local) 
;;time-of-day: is the time of day that the ad is to be aired (either daytime,primetime, or off-hour)
;;air-times: is the number of times the ad is to be aired


(define-struct ad (product duration cost national? time-of-day air-times))

(define POWER-RANGERS(make-ad "Power Rangers Movie" 30 5 true "daytime" 25))
(define SWITCH(make-ad "Nintendo Switch" 90 5000 true "primetime" 1))
(define THE-LAST-OF-US(make-ad "The Last of Us Remastered" 120 10 false "off-hour" 2))

;;a ListOfAd is one of
;; empty
;; (cons loa empty)

(cons POWER-RANGERS (cons SWITCH (cons THE-LAST-OF-US empty)))
empty


;;
;;Problem 2
;;


; ;;ad-fcn: Ad ->  
; (define (ad-fcn an-ad)(cond
;                           [(ad? an-ad)(...(ad-product an-ad)
;                                        (ad-duration an-ad)
;                                        (ad-cost an-ad)
;                                        (ad-national? an-ad)
;                                        (ad-time-of-day an-ad)
;                                        (ad-air-times an-ad))]))
; 
; ;;fcn-loa: ListOfAd ->
; (define(fcn-loa loa)(cond
;                        [(empty? loa)empty])
;                        [(cons? loa)(...(ad-fcn (first los)
;                                                (fcn-loa(rest loa)))])



;;
;;Problem 3
;;

;;these are the test cases for the helper function check-time

(check-expect (check-prime-time SWITCH)true)
(check-expect (check-prime-time THE-LAST-OF-US)false)

;; check-time: Ad -> Boolean
;; consumes an Ad and produces true if the ad's time of day is "primetime"

(define (check-prime-time an-ad)(if (string=? "primetime" (ad-time-of-day an-ad))true false))  
                         
;;these are the test cases for the function count-long-prime-time

(check-expect(count-long-prime-time empty 25)0)
(check-expect(count-long-prime-time (cons SWITCH (cons THE-LAST-OF-US empty))10)1)
(check-expect(count-long-prime-time (cons POWER-RANGERS empty)0)0)
  
;;count-long-prime-time: ListOfAd Natural -> Natural
;;consumes a list of ads and an amount of time (in seconds)
;;and produces the number of primetime ads in the list
;;that run longer than the given number of seconds

(define (count-long-prime-time loa time)(cond
                                     [(empty? loa)0]
                                     [(cons? loa)(if(and(check-prime-time(first loa))(< time (ad-duration (first loa))))
                                                     (+ 1 (count-long-prime-time(rest loa) time))
                                                 (+ 0 (count-long-prime-time(rest loa) time)))]))

;;
;;Problem 4
;;

;;these are the test cases for the helper function check-time

(check-expect (check-time SWITCH "primetime")true)
(check-expect (check-time POWER-RANGERS "primetime")false)

;;check-time: Ad TimeOfDay -> Boolean
;;consumes an Ad and a TimeOfDay and produces a Boolean
;;true if the Ad's primetime is the same as the TimeOfDay provided

(define (check-time an-ad time)(if (string=? time (ad-time-of-day an-ad))true false))

;;these are the test cases for the function any-ads-in-timeslot?

(check-expect (any-ads-in-timeslot? empty "off-hour")false) 
(check-expect (any-ads-in-timeslot? (cons THE-LAST-OF-US empty)"primetime")false)
(check-expect (any-ads-in-timeslot? (cons SWITCH (cons POWER-RANGERS empty))"daytime")true)

;;any-ads-in-timeslot?: ListOfAd TimeOfDay -> Boolean
;; consumes a list of ads and a TimeOfDay, and produces a Boolean.
;;The function returns true if the list contains any ads that run in the given time of day.

(define (any-ads-in-timeslot? loa time)(cond
                                         [(empty? loa)false]
                                         [(cons? loa)(if (check-time (first loa)time)
                                                          true
                                                    (any-ads-in-timeslot? (rest loa)time))]))
                                                    

;;
;;Problem 5
;;

;;these are the test cases for the helper function check-national

(check-expect (check-national SWITCH)true)
(check-expect(check-national POWER-RANGERS)true)
(check-expect(check-national THE-LAST-OF-US)false)

;;check-national: Ad -> ListOfAd
;;consumes an Ad and produces a ListOfAd if the Ad is national

(define (check-national an-ad)
  (if (boolean=? true (ad-national? an-ad))true false))

;;these are the test cases for the function national-ads

(check-expect (national-ads empty) empty)
(check-expect (national-ads (cons SWITCH empty))(cons SWITCH empty))
(check-expect (national-ads (cons POWER-RANGERS(cons SWITCH empty)))(cons POWER-RANGERS(cons SWITCH empty)))
(check-expect (national-ads (cons THE-LAST-OF-US empty))empty)
(check-expect (national-ads (cons POWER-RANGERS (cons THE-LAST-OF-US empty)))(cons POWER-RANGERS empty))

;;national-ads: ListOfAd -> ListOfAd 
;;consumes a list of ads and produces a list of all the ads airing nationally
    

(define (national-ads loa)(cond
                           [(empty? loa)empty]
                           [(cons? loa)(if (check-national(first loa))
                                       (cons(first loa)(national-ads (rest loa)))
                                       (national-ads(rest loa)))]))

;;
;;Problem 6
;;

;;a ListOfStrings is one of
;;  empty
;; (cons String ListOfString)

;;these are the test cases for the helper function check-off-hours

(check-expect(check-off-hours SWITCH)false)
(check-expect(check-off-hours POWER-RANGERS)false)
(check-expect(check-off-hours THE-LAST-OF-US)true)
             
;;check-off-hours: Ad -> Boolean
;;consumes an Ad and produces a Boolean
;;true if the Ad's TimeOfDay is equal to "off-hour"
;;false if it is not

(define (check-off-hours an-ad)(if (string=? "off-hour" (ad-time-of-day an-ad))true false))

;;these are the test cases for the function late-night-products

(check-expect (late-night-products empty)empty)
(check-expect (late-night-products (cons SWITCH empty))empty)
(check-expect (late-night-products (cons THE-LAST-OF-US empty))(cons "The Last of Us Remastered" empty))
(check-expect (late-night-products (cons POWER-RANGERS(cons THE-LAST-OF-US empty)))(cons "The Last of Us Remastered" empty))

;;late-night-products: ListOfAd -> ListOfString
;;consumes a list of ads and produces a list of strings
;;The list that is produced contains the names of the products
;;that are advertised during the off-hours

(define(late-night-products loa)(cond
                                  [(empty? loa)empty]
                                  [(cons? loa)(if(check-off-hours(first loa))
                                              (cons(ad-product (first loa))(late-night-products(rest loa)))
                                              (late-night-products(rest loa)))]))

;;
;;Problem 7
;;

;;these are the test cases for the helper function is-national

(check-expect(is-national SWITCH)300000)
(check-expect(is-national POWER-RANGERS)100000)
(check-expect(is-national THE-LAST-OF-US)20000)

;;constants for the helper function is-national

(define is-national-true 100000)
(define is-national-false 5000)

;;is-national: Ad -> Number
;;consumes an Ad and produces a Number depending on whether the Ad is national
;;and takes the Ad duration into account

(define (is-national an-ad)(if(boolean=? true (ad-national? an-ad))(* is-national-true(/ (ad-duration an-ad) 30))(* is-national-false(/ (ad-duration an-ad) 30))))

;;these are the test cases for the helper function is-time

(check-expect(is-time SWITCH)300000)
(check-expect(is-time POWER-RANGERS)80000)
(check-expect(is-time THE-LAST-OF-US)10000)

;;constants for the helper function is-time

(define day-discount 0.8)
(define off-hour-discount 0.5)

;;is-time: Ad -> Number
;;consumes an Ad and produces a Number based on the TimeOfDay of the Ad

(define(is-time an-ad)(cond
                          [(string=? "daytime" (ad-time-of-day an-ad))(* day-discount (is-national an-ad))]
                          [(string=? "primetime" (ad-time-of-day an-ad))(is-national an-ad)]
                          [(string=? "off-hour" (ad-time-of-day an-ad))(* off-hour-discount (is-national an-ad))]))

;;these are the test cases for the function air-cost

(check-expect(air-cost POWER-RANGERS)2000000)
(check-expect(air-cost SWITCH)300000)
(check-expect(air-cost THE-LAST-OF-US)20000)

;;air-cost: Ad -> Number
;;consumes an Ad and produces a cost to air the Ad
;;The cost of a 30-second primetime ad for a national market is $100,000.
;; The cost of a 30-second primetime ad for a local market is $5000.
;; A discount of 20% is applied to the cost if the ad is aired in the daytime,
;;       and a discount of 50% is applied if the ad is aired during off-hours;
;;       the discounts apply to both national and local ads.
;;       Finally, the air-cost is multiplied by the number of times the ad is to be aired.

(define(air-cost an-ad)(* (is-time an-ad)(ad-air-times an-ad)))


;;
;;Problem 8
;;

;;these are the test cases for the function total-ad

(check-expect(total-ad-cost SWITCH)5300000)
(check-expect(total-ad-cost POWER-RANGERS)2005000)
(check-expect(total-ad-cost THE-LAST-OF-US)30000)

;;total-ad-cost: Ad -> Number
;;consumes an Ad and produces the total cost of the ad.
;;The total cost is the sum of the cost of producing the ad and the cost of airing the ad

(define (total-ad-cost an-ad)(+ (*(ad-cost an-ad)1000)(air-cost an-ad)))

;;
;;Problem 9
;;

;;these are the test cases for the function

(check-expect(expensive-ads empty 10)empty)
(check-expect(expensive-ads (cons SWITCH empty) 20000)(cons SWITCH empty))
(check-expect(expensive-ads (cons POWER-RANGERS(cons THE-LAST-OF-US empty)) 50000)(cons POWER-RANGERS empty))

;;expensive-ads: ListOfAd Number -> ListOfAd
;;consumes a list of ads and a Number and
;; produces a list of those ads for which the total ad cost exceeds the given number

(define (expensive-ads loa num)(cond
                                 [(empty? loa)empty]
                                 [(cons? loa)(if(> (total-ad-cost (first loa)) num)
                                             (cons(first loa)(expensive-ads(rest loa) num))
                                              (expensive-ads(rest loa) num))]))
                                             
                                                   
