;;
;;Linda Puzey
;;lpuzey
;;9/20/17
;;Lab 6
;;

;;
;;Problem 1
;;

;;a TimeOfDay is one of
;;   "daytime"
;;   "primetime"
;;   "off-hour"


(define-struct ad (name duration production-cost national? time-of-day repetitions))
;; an Ad is a (make-ad String Natural Number Boolean TimeOfDay Natural)
;; interp:  a television ad, where
;;          name is the name of the product the ad is for
;;          duration is the length of the ad (in seconds)
;;          production-cost is the cost to produce the ad (in thousands of dollars)
;;          national? is true if the ad is to be aired nationally (false if locally)
;;          time-of-day is the time of day that the ad airs
;;          repetitions is the number of times the ad is to be played

;; a ListOfAd is one of
;;   empty
;;   (cons Ad ListOfAd)

(define POWER-RANGERS(make-ad "Power Rangers Movie" 30 5 true "daytime" 25))
(define SWITCH(make-ad "Nintendo Switch" 90 5000 true "primetime" 1))
(define THE-LAST-OF-US(make-ad "The Last of Us Remastered" 120 10 false "off-hour" 2))

;;These are the test cases for the function count-long-prime-time

(check-expect(count-long-prime-time empty 25)0)
(check-expect(count-long-prime-time (cons SWITCH (cons THE-LAST-OF-US empty))10)1)
(check-expect(count-long-prime-time (cons POWER-RANGERS empty)0)0)

;;count-long-prime-time: ListOfAd Natural -> Natural
;;consumes a list of ads and a length of time and
;;produces the number of ads in the list
;;that air in runtime and that run for more than the given number of seconds

(define (count-long-prime-time loa num)
  (local [(define (check-prime-time an-ad)
            (and(string=? "primetime" (ad-time-of-day an-ad))(> (ad-duration an-ad)num)))]
    (length(filter check-prime-time loa))))  

;;
;;Problem 2
;;

;;these are the test cases for the function national-ads

(check-expect (national-ads empty) empty)
(check-expect (national-ads (cons SWITCH empty))(cons SWITCH empty))
(check-expect (national-ads (cons POWER-RANGERS(cons SWITCH empty)))(cons POWER-RANGERS(cons SWITCH empty)))
(check-expect (national-ads (cons THE-LAST-OF-US empty))empty)
(check-expect (national-ads (cons POWER-RANGERS (cons THE-LAST-OF-US empty)))(cons POWER-RANGERS empty))

;;national-ads: ListOfAd -> ListOfAd 
;;consumes a list of ads and produces a list of all the ads airing nationally
    
(define (national-ads loa)
  (local[(define (check-national an-ad)
           (boolean=? true (ad-national? an-ad)))]
    (filter check-national loa)))

;;
;;Problem 3
;;

;;these are the test cases for the function late-night-products

(check-expect (late-night-products empty)empty)
(check-expect (late-night-products (cons SWITCH empty))empty)
(check-expect (late-night-products (cons THE-LAST-OF-US empty))(cons "The Last of Us Remastered" empty))
(check-expect (late-night-products (cons POWER-RANGERS(cons THE-LAST-OF-US empty)))(cons "The Last of Us Remastered" empty))

;;late-night-products: ListOfAd -> ListOfString
;;consumes a list of ads and produces a list of strings
;;The list that is produced contains the names of the products
;;that are advertised during the off-hours

(define(late-night-products loa)
  (local[(define (check-off-hours an-ad)
           (string=? "off-hour" (ad-time-of-day an-ad)))]
    (map ad-name
         (filter check-off-hours loa))))

;;
;;Problem 4
;;

;;these are the test cases for the function adjust-for-inflation

(check-expect (adjust-for-inflation (cons POWER-RANGERS empty))
              (list(make-ad "Power Rangers Movie" 30 5.05 true "daytime" 25)))
(check-expect (adjust-for-inflation empty)empty)
(check-expect (adjust-for-inflation (cons THE-LAST-OF-US (cons POWER-RANGERS (cons SWITCH empty))))
              (list(make-ad "The Last of Us Remastered" 120 10.1 false "off-hour" 2)
                   (make-ad "Power Rangers Movie" 30 5.05 true "daytime" 25)
                   (make-ad "Nintendo Switch" 90 5050 true "primetime" 1)))
(check-expect(adjust-for-inflation (cons 1 empty))(list empty))

;;adjust-for-inflation: ListOfAd -> ListOfAd
;;consumes a list of ads and produces a list where the
;;production cost for each ad in the list has increased by 1%
;(name duration production-cost national? time-of-day repetitions))

(define (adjust-for-inflation loa)
  (local [(define (adjust-production an-ad)
            (if(ad? an-ad)
               (make-ad
                (ad-name an-ad)
                (ad-duration an-ad)
                (* (ad-production-cost an-ad) 1.01)
                (ad-national? an-ad)
                (ad-time-of-day an-ad)
                (ad-repetitions an-ad)) empty))]
    (map adjust-production loa)))

;;
;;Problem 5
;;
  
(define-struct message (sender message flag))
;;a Message is a (make-struct User String Boolean)
;; a Message where,
;; Sender is the sender of the message
;; Message is the message that was sent
;; Flag indicateds whether or not the user has read the message
;; true if they read the message, false if they did not

(define-struct user (username lom))
;;a User is a (make-struct String ListOfMessage
;; a User where,
;; Username is the user's username
;; ListOfMessage is a list of messages sent

;;a ListOfMessages is one of
;;empty
;;(cons Message ListOfMessage)

;;a MailSystem is one of
;; empty
;; (cons User ListOfUser)

;; a mailsys stores the information for a Mail System
(define mailsys empty)


;;
;;Problem 6
;;

;;add-user: String -> Void
;; The effect of the function is to add a new user
;; with the given username to the mail system
;; The new user should have an empty mailbox.

(define (add-user new-name)
  (set! mailsys (cons (make-user new-name empty) mailsys)))

;;
;;Problem 7
;;

;;send-help: String ListOfUser -> ListOfUser
;;send-help is a helper function for send-email
;;consumes a name and a ListOfUser
;;and produces the ListOfUser with the same name

(define (send-help name lou)
  (cond
    [(empty? lou) empty]
    [(cons? lou)(if (string=? name (user-username (first lou)))
                    (first lou)
                    (send-help name (rest lou)))]))
                                           
      

;;send-email:  String String String -> Void
;; The effect of the function is to store a new unread message in the recipient's mailbox
;; Assume the named recipient is a user in the mail system

(define (send-email sender recipient text)
  (set-user-lom! (send-help recipient mailsys) (cons (make-message sender text false)(user-lom (send-help recipient mailsys)))))


;;
;;Problem 8
;;

;;get-unread-messages-and-mark-read: String -> ListOfMessages
;; consumes a username and produces a list of messages
;; The produced list contains the (previously) unread
;; messages in the mailbox of the user with the given name
;; An effect of the function is that all such
;; unread messages in the named user's mailbox have been set to read

(define (get-unread-messages-and-mark-read name)
  (local [(define (read-messages msg) (not (message-flag msg)))]
           (local [(define unread-list (filter read-messages (user-lom (send-help name mailsys))))]
           (local [(define (make-read msg) (set-message-flag! msg true))]
           (begin (map make-read (user-lom (send-help name mailsys)))
           unread-list))))
  )

                                         

;;
;;Problem 9
;;

;;most-messsages:  -> User
;;doesn't cosume anything and produces the user in the
;;mailsystem with the largest number of messages in his/her mailbox
;;If there are no users in the system, the function produces an appropriate error
;;If two or more users have the most messages,
;;the function just needs to return one of them (it doesn't matter which one)

(define (most-messages)
  (cond
  [(empty? mailsys) (error "no users")]
  [(cons? mailsys)
   (local [(define (more-messages lou name num)
             (cond
               [(empty? lou)name]
               [(cons? lou)(if (> (length (user-lom (first lou))) num)
                               (more-messages (rest lou)(first lou)(length (user-lom (first lou))) )
                               (more-messages (rest lou)name num))]))]
     (more-messages mailsys false 0))]))
             
             
               


;;
;;Problem 10
;;test add-user
"show original contents of mailsys"
mailsys
"add-user Sarah"
(add-user "Sarah")
"show that user Sarah has been added and nothing else has changed"
mailsys

"add-user Linda"
(add-user "Linda")
"show that user Linda has been added and nothing else has changed"
mailsys

"add-user Andrew"
(add-user "Andrew")
"show that user Andrew has been added and nothing else has changed"
mailsys

"add-user Tyler"
(add-user "Tyler")
"show that user Tyler has been added and nothing else has changed"
mailsys

;;test send-email
"show original contents of mailsys"
mailsys
"sends an email from Sarah to Andrew saying 'Hi'"
(send-email "Sarah" "Andrew" "Hi")
"show that an email has been added to Andrew's ListOfMessages and nothing else has changed"
mailsys

;;test get-unread-messages-and-mark-read
"show original contents of mailsys"
mailsys
"sets all Andrew's messages to read"
(get-unread-messages-and-mark-read "Andrew")
"show that all Andrew's messages have been set to read and nothing else has changed"
mailsys

;;test most-messages
"shows which user in mailsys has the most messages"
(most-messages)

