;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname adventure) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require "adventure-define-struct.rkt")
(require "macros.rkt")
(require "utilities.rkt")

;;;
;;; OBJECT
;;; Base type for all in-game objects
;;;

(define-struct object
  ;; adjectives: (listof string)
  ;; List of adjectives to be printed in the description of this object
  (adjectives)
  
  #:methods
  ;; noun: object -> string
  ;; Returns the noun to use to describe this object.
  (define (noun o)
    (type-name-string o))

  ;; description-word-list: object -> (listof string)
  ;; The description of the object as a list of individual
  ;; words, e.g. '("a" "red" "door").
  (define (description-word-list o)
    (add-a-or-an (append (object-adjectives o)
                         (list (noun o)))))
  ;; description: object -> string
  ;; Generates a description of the object as a noun phrase, e.g. "a red door".
  (define (description o)
    (words->string (description-word-list o)))
  
  ;; print-description: object -> void
  ;; EFFECT: Prints the description of the object.
  (define (print-description o)
    (begin (printf (description o))
           (newline)
           (void))))

;;;
;;; CONTAINER
;;; Base type for all game objects that can hold things
;;;

(define-struct (container object)
  ;; contents: (listof thing)
  ;; List of things presently in this container
  (contents)
  
  #:methods
  ;; container-accessible-contents: container -> (listof thing)
  ;; Returns the objects from the container that would be accessible to the player.
  ;; By default, this is all the objects.  But if you want to implement locked boxes,
  ;; rooms without light, etc., you can redefine this to withhold the contents under
  ;; whatever conditions you like.
  (define (container-accessible-contents c)
    (container-contents c))
  
  ;; prepare-to-remove!: container thing -> void
  ;; Called by move when preparing to move thing out of
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-remove! container thing)
    (void))
  
  ;; prepare-to-add!: container thing -> void
  ;; Called by move when preparing to move thing into
  ;; this container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-add! container thing)
    (void))
  
  ;; remove!: container thing -> void
  ;; EFFECT: removes the thing from the container
  (define (remove! container thing)
    (set-container-contents! container
                             (remove thing
                                     (container-contents container))))
  
  ;; add!: container thing -> void
  ;; EFFECT: adds the thing to the container.  Does not update the thing's location.
  (define (add! container thing)
    (set-container-contents! container
                             (cons thing
                                   (container-contents container))))

  ;; describe-contents: container -> void
  ;; EFFECT: prints the contents of the container
  (define (describe-contents container)
    (begin (local [(define other-stuff (remove me (container-accessible-contents container)))]
             (if (empty? other-stuff)
                 (printf "There's nothing here.~%")
                 (begin (printf "You see:~%")
                        (for-each print-description other-stuff))))
           (void))))

;; move!: thing container -> void
;; Moves thing from its previous location to container.
;; EFFECT: updates location field of thing and contents
;; fields of both the new and old containers.
(define (move! thing new-container)
  (begin
    (prepare-to-remove! (thing-location thing)
                        thing)
    (prepare-to-add! new-container thing)
    (prepare-to-move! thing new-container)
    (remove! (thing-location thing)
             thing)
    (add! new-container thing)
    (set-thing-location! thing new-container)))

;; destroy!: thing -> void
;; EFFECT: removes thing from the game completely.
(define (destroy! thing)
  ; We just remove it from its current location
  ; without adding it anyplace else.
  (remove! (thing-location thing)
           thing))

;;;
;;; ROOM
;;; Base type for rooms and outdoor areas
;;;

(define-struct (room container)
  ())

;; new-room: string -> room
;; Makes a new room with the specified adjectives
(define (new-room adjectives)
  (make-room (string->words adjectives)
             '()))

;;;
;;; PEACH
;;; Type of room within a room that must be broken in order to access the next room
;;;
(define-struct  (peach room)
  (break-destination))

(define (new-peach adjectives destination)
  (make-peach (string->words adjectives)
              '()
              destination))
      
(define (break)
  (local [(define location (thing-location me))]
    (if (peach? location)
        (begin
          (printf "You have broken out of the peach")
          (move! me (peach-break-destination location))
          (look))
        (printf "You are not in a peach"))))
  
;;;
;;; THING
;;; Base type for all physical objects that can be inside other objects such as rooms
;;;

(define-struct (thing container)
  ;; location: container
  ;; What room or other container this thing is presently located in.
  (location takeable?)
  
  #:methods
  (define (examine thing)
    (print-description thing))

  ;; prepare-to-move!: thing container -> void
  ;; Called by move when preparing to move thing into
  ;; container.  Normally, this does nothing, but
  ;; if you want to prevent the object from being moved,
  ;; you can throw an exception here.
  (define (prepare-to-move! container thing)
    (void)))


;; initialize-thing!: thing -> void
;; EFFECT: adds thing to its initial location
(define (initialize-thing! thing)
  (add! (thing-location thing)
        thing))

;; new-thing: string container -> thing
;; Makes a new thing with the specified adjectives, in the specified location,
;; and initializes it.
(define (new-thing adjectives location takeable?)
  (local [(define thing (make-thing (string->words adjectives)
                                    '() location takeable?))]
    (begin (initialize-thing! thing)
           thing)))

;;;
;;; DOOR
;;; A portal from one room to another
;;; To join two rooms, you need two door objects, one in each room
;;;

(define-struct (door thing)
  ;; destination: container
  ;; The place this door leads to
  (destination)
  
  #:methods
  ;; go: door -> void
  ;; EFFECT: Moves the player to the door's location and (look)s around.
  (define (go door)
    (begin (move! me (door-destination door))
           (look))))

;; join: room string room string
;; EFFECT: makes a pair of doors with the specified adjectives
;; connecting the specified rooms.
(define (join! room1 adjectives1 room2 adjectives2)
  (local [(define r1->r2 (make-door (string->words adjectives1)
                                    '()  room1 #F room2))
          (define r2->r1 (make-door (string->words adjectives2)
                                    '() room2 #F room1))]
    (begin (initialize-thing! r1->r2)
           (initialize-thing! r2->r1)
           (void))))

;;;
;;; PERSON
;;; A character in the game.  The player character is a person.
;;;

(define-struct (person thing)
  (healing-ability banana# sugar# starch# rice# bone# dango#))

;; initialize-person: person -> void
;; EFFECT: do whatever initializations are necessary for persons.
(define (initialize-person! p)
  (initialize-thing! p))

;; new-person: string container -> person
;; Makes a new person object and initializes it.
(define (new-person adjectives location healing-ability banana# sugar# starch# rice# bone# dango#)
  (local [(define person
            (make-person (string->words adjectives)
                         '()
                         location
                         #F
                         healing-ability
                         banana#
                         sugar#
                         starch#
                         rice#
                         bone#
                         dango#))]
    (begin (initialize-person! person)
           person)))

;; BOSS
(define-struct (boss person)
  (damage monkey-level dog-level crane-level befriendable? sentence)

  #:methods
  (define (monkey-level boss)
    (boss-monkey-level boss))
  (define (crane-level boss)
    (boss-crane-level boss))
  (define (dog-level boss)
    (boss-dog-level boss))

  (define (befriend boss)
    (if (boss-befriendable? boss)
        (if (and (have? (the NPC)) (NPC-cooked? (the NPC)))
            (begin (set-thing-takeable?! boss #T)
                   (destroy! (the NPC))
                   (printf "You have befriended the boss. Now you can take it with you."))
            (printf "You don't have a cooked NPC"))
        (printf "You cannot befriend this boss")))

  (define (beat boss)
    (if (and (have? (the monkey))
             (have? (the dog))
             (have? (the crane)))
        (if (and (>= (animal-level (the monkey)) (boss-monkey-level boss))
                 (>= (animal-level (the dog)) (boss-dog-level boss))
                 (>= (animal-level (the crane)) (boss-crane-level boss)))
            (if (and (>= (animal-life (the monkey)) (boss-damage boss))
                     (>= (animal-life (the dog)) (boss-damage boss))
                     (>= (animal-life (the crane)) (boss-damage boss)))
                (begin (set-animal-life! (the monkey) (- (animal-life (the monkey)) (boss-damage boss)))
                       (set-animal-life! (the crane) (- (animal-life (the crane)) (boss-damage boss)))
                       (set-animal-life! (the dog) (- (animal-life (the dog)) (boss-damage boss)))
                       (set-person-healing-ability! me (+ (person-healing-ability me) (person-healing-ability boss)))
                       (set-person-banana#! me (+ (person-banana# me) (person-banana# boss)))
                       (set-person-sugar#! me (+ (person-sugar# me) (person-sugar# boss)))
                       (set-person-starch#! me (+ (person-starch# me) (person-starch# boss)))
                       (set-person-rice#! me (+ (person-rice# me) (person-rice# boss)))
                       (set-person-bone#! me (+ (person-bone# me) (person-bone# boss)))
                       (set-person-dango#! me (+ (person-dango# me) (person-dango# boss)))
                       (destroy! boss)
                       (printf (boss-sentence boss)))
                (printf "Your animals do not have enough life"))
            (printf "You animals do not have enough level"))
        (printf "You need to have all three animals."))))

(define (new-boss adjectives location banana# sugar# starch# rice# bone# dango# damage monkey-level dog-level crane-level befriendable? sentence)
  (local [(define boss
            (make-boss (string->words adjectives)
                       '()
                       location
                       #F
                       0
                       banana#
                       sugar#
                       starch#
                       rice#
                       bone#
                       dango#
                       damage monkey-level dog-level crane-level befriendable? sentence))]
    (begin (initialize-person! boss)
           boss)))

;; This is the global variable that holds the person object representing
;; the player.  This gets reset by (start-game)
(define me empty)

;;; Everything related to NPC
(define-struct (NPC thing)
  (dialog current cooked? talkable?)

  #:methods
  (define (talk NPC)
    (if (NPC-cooked? NPC)
        (printf "The NPC is cooked. You cannot interact with it anymore")
        (begin (set-NPC-current! NPC (NPC-dialog NPC))
               (printf (conversation-print (NPC-current NPC))))))
  (define (reply NPC Boolean)
    (if (NPC-cooked? NPC)
        (printf "The NPC is cooked. You cannot interact with it anymore")
        (if (empty? (NPC-current NPC))
            (printf "Please talk to the NPC first")
            (if Boolean
                (begin (set-NPC-current! NPC (conversation-iftrue (NPC-current NPC)))
                       (printf (conversation-print (NPC-current NPC)))
                       (when (empty? (conversation-iftrue (NPC-current NPC)))
                         (set-NPC-current! NPC empty)))
                (begin (set-NPC-current! NPC (conversation-iffalse (NPC-current NPC)))
                       (printf (conversation-print (NPC-current NPC)))
                       (when (empty? (conversation-iftrue (NPC-current NPC)))
                         (set-NPC-current! NPC empty)))))))
  (define (cook-NPC NPC)
    (if (NPC? NPC)
        (begin (set-NPC-cooked?! NPC #T)
               (set-thing-takeable?! NPC #T)
               (printf "You just cooked the NPC."))
        (printf "You can only use this on NPC"))))

(define (new-NPC adjectives location dialog)
  (local [(define NPC (make-NPC (string->words adjectives)
                                '() location #F dialog empty #F #T))]
    (begin (initialize-thing! NPC)
           NPC)))
(define-struct conversation (print iftrue iffalse))

(define haveanimal
  (make-conversation "Go ahead!" empty empty))
(define nothaveanimal
  (make-conversation "Monkey is behind the formal door, dog the large door, and crane the damp door" empty empty))
(define notknowboss
  (make-conversation "One is behind an ominous door; the other is behind a dreary door." empty empty))
(define knowboss
  (make-conversation "Do you have all the animals needed?" haveanimal nothaveanimal))
(define default-dialog
  (make-conversation "Do you know where the boss is?" knowboss notknowboss))


;;;
;;; 
;;; A thing in the game that doesn't serve any purpose other than to be there.
;;;

(define-struct (prop thing)
  (;; noun-to-print: string
   ;; The user can set the noun to print in the description so it doesn't just say "prop"
   noun-to-print
   ;; examine-text: string
   ;; Text to print if the player examines this object
   examine-text
   )
  
  #:methods
  (define (noun prop)
    (prop-noun-to-print prop))

  (define (examine prop)
    (display-line (prop-examine-text prop))))

;; new-prop: string container -> prop
;; Makes a new prop with the specified description.
(define (new-prop description examine-text location)
  (local [(define words (string->words description))
          (define noun (last words))
          (define adjectives (drop-right words 1))
          (define prop (make-prop adjectives '() location #T noun examine-text))]
    (begin (initialize-thing! prop)
           prop)))

;;bandage
(define-struct (bandage prop) (heal-amount))

;;new-bandage: makes a new bandage
(define (new-bandage adjectives location amount)
  (local [(define bandage (make-bandage (string->words adjectives) '() location #F "bandage" "It's a bandage. Take it to heal your allies." amount))]
    (begin (initialize-thing! bandage)
           bandage)))

;;food
(define-struct (food prop)(amount))

;;banana
(define-struct (banana food) ())
;;new-banana: makes a new banana
(define (new-banana adjectives location amount)
  (local [(define banana (make-banana (string->words adjectives) '() location #F "banana" "It's a banana" amount))]
    (begin (initialize-thing! banana)
           banana)))

;;bone
(define-struct (bone food) ())
;;new-bone: makes a new bone
(define (new-bone adjectives location amount)
  (local [(define bone (make-bone (string->words adjectives) '() location #F "bone" "It's a bone" amount))]
    (begin (initialize-thing! bone)
           bone)))

;;dango
(define-struct (dango food) ())
;;new-dango: makes a new dango
(define (new-dango adjectives location amount)
  (local [(define dango (make-dango (string->words adjectives) '() location #F "dango" "It's a dango" amount))]
    (begin (initialize-thing! dango)
           dango)))

;;sugar
(define-struct (sugar food) ())
;;new-sugar: makes new sugar
(define (new-sugar adjectives location amount)
  (local [(define sugar (make-sugar (string->words adjectives) '() location #F "sugar" "It's sugar" amount))]
    (begin (initialize-thing! sugar)
           sugar)))

;;starch
(define-struct (starch food) ())
;;new-starch: makes new starch
(define (new-starch adjectives location amount)
  (local [(define starch (make-starch (string->words adjectives) '() location #F "starch" "It's starch" amount))]
    (begin (initialize-thing! starch)
           starch)))

;;rice
(define-struct (rice food) ())
;;new-rice: makes new rice
(define (new-rice adjectives location amount)
  (local [(define rice (make-rice (string->words adjectives) '() location #F "rice" "It's a bag of rice" amount))]
    (begin (initialize-thing! rice)
           rice)))



; Fridge
(define-struct (fridge thing)
  (open?)
  #:methods
  (define (open f)
    (set-fridge-open?! f #true) )
  (define (close f)
    (set-fridge-open?! f #false))
  (define (take-contents f)
    (if (fridge-open? f)
        (for-each (lambda (x) (take x)) (container-contents f))
        (error "The fridge is closed"))))

; Initialize Fridge
(define (new-fridge adjectives location)
  (local [(define the-fridge (make-fridge (string->words adjectives)
                                          '() location
                                          false
                                          false))]
    (begin (initialize-thing! the-fridge)
           the-fridge)))

; Cabinet

(define-struct (cabinet fridge)
  (locked? combination)
  #:methods
  (define (open s)
    (if (not (cabinet-locked? s))
        (set-fridge-open?! s #true)
        (printf "the cabinet is locked")))
  (define (unlock s combo)
    (if (= (cabinet-combination s) combo)
        (begin 
          (set-fridge-open?! s #true)
          (set-cabinet-locked?! s #false))
        (error "wrong combo")))
  (define (lock s)
    (begin (set-cabinet-locked?! s #true)
           (set-fridge-open?! s #false))))

; Initialize Cabinet

(define (new-cabinet adjectives combination location)
  (local [(define the-cabinet (make-cabinet (string->words adjectives)
                                            '() location
                                            false
                                            false
                                            true
                                            combination))]
    (begin (initialize-thing! the-cabinet)
           the-cabinet)))

; Note
(define-struct (note thing)
  (front? front back)
  
  #:methods
  (define (flip n)
    (if (note-front? n)
        (set-note-front?! n #false)
        (set-note-front?! n #true)))
  (define (examine n)
    (if (note-front? n)
        (printf (note-front n))
        (printf (note-back n)))))

; Initialize Note

(define (new-note adjectives front? front back location)
  (local [(define the-note (make-note (string->words adjectives)
                                      '() location
                                      false
                                      front?
                                      front
                                      back))]
    (begin (initialize-thing! the-note)
           the-note)))


; Animal
(define-struct (animal thing)
  (life preferred-food level)

  #:methods
  (define (heal animal amount)
    (if (>= (person-healing-ability me) amount)
        (if (have? animal)
            (begin (set-animal-life! animal (+ (animal-life animal) amount))
                   (set-person-healing-ability! me (- (person-healing-ability me) amount)))
            (printf "you don't have the animal"))
        (printf "you don't have enough healing ability")))
    
  (define (examine-life animal)
    (animal-life animal))
  (define (examine-level animal)
    (animal-level animal))
  (define (examine-preferred-food animal)
    (printf (animal-preferred-food animal))))

; Initialize Animal

(define (new-animal adjectives life level preferred-food location)
  (local [(define the-animal (make-animal (string->words adjectives)
                                          '() location
                                          false
                                          life                                    
                                          preferred-food
                                          level
                                          ))]
    (begin (initialize-thing! the-animal)
           the-animal)))

; Dog
(define-struct (dog animal)
  ())

; Initialize Dog  
(define (new-dog adjectives life level preferred-food location)
  (local [(define the-dog (make-dog (string->words adjectives)
                                    '() location
                                    false
                                    life                                    
                                    preferred-food
                                    level
                                    ))]
    (begin (initialize-thing! the-dog)
           the-dog)))

; Crane
(define-struct (crane animal) ())

; Initialize Crane  
(define (new-crane adjectives life level preferred-food location)
  (local [(define the-crane (make-crane (string->words adjectives)
                                        '() location
                                        false
                                        life                                    
                                        preferred-food
                                        level
                                        ))]
    (begin (initialize-thing! the-crane)
           the-crane)))

; Monkey
(define-struct (monkey animal) ())

; Initialize Monkey

(define (new-monkey adjectives life level preferred-food location)
  (local [(define the-monkey (make-monkey (string->words adjectives)
                                          '() location
                                          false
                                          life                                    
                                          preferred-food
                                          level
                                          ))]
    (begin (initialize-thing! the-monkey)
           the-monkey)))

;;;
;;; USER COMMANDS
;;;

(define (look)
  (begin (printf "You are in ~A.~%"
                 (description (here)))
         (describe-contents (here))
         (void)))

(define-user-command (look) "Prints what you can see in the room")

(define (inventory)
  (if (empty? (my-inventory))
      (printf "You don't have anything.~%")
      (begin (printf "You have:~%")
             (for-each print-description (my-inventory)))))

(define-user-command (inventory)
  "Prints the things you are carrying with you.")

(define-user-command (examine thing)
  "Takes a closer look at the thing")

(define (take thing)
  (if (thing-takeable? thing)
      (move! thing me)
      (if (banana? thing)
          (begin (set-person-banana#! me (+ (person-banana# me) (food-amount thing)))
                 (remove! (thing-location thing) thing)
                 (printf "You took the banana."))
          (if (bone? thing)
              (begin (set-person-bone#! me (+ (person-bone# me) (food-amount thing)))
                     (remove! (thing-location thing) thing)
                     (printf "You took the bone."))
              (if (dango? thing)
                  (begin (set-person-dango#! me (+ (person-dango# me) (food-amount thing)))
                         (remove! (thing-location thing) thing)
                         (printf "You took the dango."))
                  (if (sugar? thing)
                      (begin (set-person-sugar#! me (+ (person-sugar# me) (food-amount thing)))
                             (remove! (thing-location thing) thing)
                             (printf "You took the sugar."))
                      (if (starch? thing)
                          (begin (set-person-starch#! me (+ (person-starch# me) (food-amount thing)))
                                 (remove! (thing-location thing) thing)
                                 (printf "You took the starch."))
                          (if (rice? thing)
                              (begin (set-person-rice#! me (+ (person-rice# me) (food-amount thing)))
                                     (remove! (thing-location thing) thing)
                                     (printf "You took the rice."))
                              (if (bandage? thing)
                                  (begin (set-person-healing-ability! me (+ (person-healing-ability me) (bandage-heal-amount thing)))
                                         (remove! (thing-location thing) thing)
                                         (printf "You took the bandage."))
                                  (error "You cannot take this object"))))))))))

(define-user-command (take thing)
  "Take things with you")

(define (drop thing)
  (move! thing (here)))

(define-user-command (drop thing)
  "Removes thing from your inventory and places it in the room")

(define (put thing container)
  (move! thing container))

(define-user-command (put thing container)
  "Moves the thing from its current location and puts it in the container.")

(define (help)
  (for-each (λ (command-info)
              (begin (display (first command-info))
                     (newline)
                     (display (second command-info))
                     (newline)
                     (newline)))
            (all-user-commands)))

(define-user-command (help)
  "Displays this help information")

(define-user-command (go door)
  "Go through the door to its destination")

(define (check condition)
  (if condition
      (display-line "Check succeeded")
      (error "Check failed!!!")))

(define-user-command (check condition)
  "Throws an exception if condition is false.")

(define-user-command (break)
  "Break from the starting peach")

(define-user-command (talk NPC)
  "Start a conversation with the NPC")

(define-user-command (reply NPC Boolean)
  "Reply to the NPC")

(define-user-command (cook-NPC NPC)
  "Cook the NPC")

(define-user-command (heal animal amount)
  "Heal one of your animals")

(define-user-command (examine-life animal)
  "Check the life of your animal")
(define-user-command (examine-level animal)
  "Check the level of your animal")
(define-user-command (examine-preferred-food animal)
  "Check the preferred food of your animal")

(define-user-command (feed animal)
  "Feed the animal with its preferred food to make it follow you")

(define-user-command (train animal)
  "Level up your animal with its preferred food")

(define-user-command (cook-dango)
  "Cook a dango some rice, starch, and sugar")

(define-user-command (beat boss)
  "Beat the boss")

(define-user-command (monkey-level boss)
  "Check the required level for monkey to beat the boss")
(define-user-command (crane-level boss)
  "Check the required level for crane to beat the boss")
(define-user-command (dog-level boss)
  "Check the required level for dog to beat the boss")

(define-user-command (befriend boss)
  "Befriend a boss")

(define-user-command (open fridge/cabinet)
  "Open the fridge/cabinet")

(define-user-command (close fridge)
  "Close the fridge")

(define-user-command (take-contents fridge/cabinet)
  "Take all the contents out of the fridge/cabinet")

(define-user-command (unlock cabinet)
  "Unlock the cabinet")

(define-user-command (lock cabinet)
  "Lock the cabinet")

(define-user-command (flip note)
  "Flip the note")

;;;
;;; ADD YOUR COMMANDS HERE!
;;;


;; feed: feed an animal it's preferred food (SR)
(define (feed animal)
  (begin (when (string=? (animal-preferred-food animal) "banana")
           (if (>= (person-banana# me) 1)
               (begin (set-person-banana#! me (- (person-banana# me) 1))
                      (set-thing-takeable?! animal #T)
                      (printf "Now you can take the animal with you"))
               (printf "You don't have enough bananas")))
         (when (string=? (animal-preferred-food animal) "dango")
           (if (>= (person-dango# me) 1)
               (begin (set-person-dango#! me (- (person-dango# me) 1))
                      (set-thing-takeable?! animal #T)
                      (printf "Now you can take the animal with you"))
               (printf "You don't have enough dango")))
         (when (string=? (animal-preferred-food animal) "bone")
           (if (>= (person-bone# me) 1)
               (begin (set-person-bone#! me (- (person-bone# me) 1))
                      (set-thing-takeable?! animal #T)
                      (printf "Now you can take the animal with you"))
               (printf "You don't have enough bones")))))

;; train: feed an animal it's preferred food and increase its level
(define (train animal amount)
  (begin (when (string=? (animal-preferred-food animal) "banana")
           (if (>= (person-banana# me) amount)
               (begin (set-person-banana#! me (- (person-banana# me) amount))
                      (set-animal-level! animal (+ (animal-level animal) amount))
                      (printf "Your monkey leveled up!"))
               (printf "You don't have enough bananas")))
         (when (string=? (animal-preferred-food animal) "dango")
           (if (>= (person-dango# me) amount)
               (begin (set-person-dango#! me (- (person-dango# me) amount))
                      (set-animal-level! animal (+ (animal-level animal) amount))
                      (printf "Your crane leveled up!"))
               (printf "You don't have enough dango")))
         (when (string=? (animal-preferred-food animal) "bone")
           (if (>= (person-bone# me) amount)
               (begin (set-person-bone#! me (- (person-bone# me) amount))
                      (set-animal-level! animal (+ (animal-level animal) amount))
                      (printf "Your dog leveled up!"))
               (printf "You don't have enough bones")))))


;; cook: if you have all of the ingredients, cook dango
(define (cook-dango)
  (if (and (> (person-sugar# me) 0)
           (> (person-starch# me) 0)
           (> (person-rice# me) 0))
      (begin (set-person-dango#! me (+ (person-dango# me) 1))
             (set-person-sugar#! me (- (person-sugar# me) 1))
             (set-person-starch#! me (- (person-starch# me) 1))
             (set-person-rice#! me (- (person-rice# me) 1))
             (printf "You cooked 1 dango!"))
      (printf "You don't have all of the ingredients.")))

;;;
;;; THE GAME WORLD - FILL ME IN
;;;

;; start-game: -> void
;; Recreate the player object and all the rooms and things.
(define (start-game)
  ;; Fill this in with the rooms you want
  (local [(define starting-room (new-room "simple"))
          (define starting-peach (new-peach "small" starting-room))
          (define storage-room (new-room "storage"))
          (define fridge-room (new-room "chilly"))
          (define cabinet-room (new-room "clean"))
          (define humid-room (new-room "humid")) 
          (define large-room (new-room "large"))
          (define boss-room-1 (new-room "ominous"))
          (define boss-room-2 (new-room "dreary"))

          (define fridge-1 (new-fridge "" fridge-room))
          (define fridge-2 (new-fridge "" storage-room))
          (define cabinet-1 (new-cabinet "wooden" 76543 cabinet-room))]
         
    (begin (set! me (new-person "" starting-peach 0 0 0 0 0 0 0))
           ;; Add join commands to connect your rooms with doors
           (join! starting-room "storage"
                  storage-room "starting")
           (join! starting-room "cold"
                  fridge-room "starting")
           (join! starting-room "formal"
                  cabinet-room "starting")
           
           (join! fridge-room "damp"
                  humid-room "cold")
           (join! storage-room "large"
                  large-room "storage")
           (join! cabinet-room "ominous"
                  boss-room-1 "formal")
           
           (join! humid-room "dreary"
                  boss-room-2 "damp")
           (join! large-room "dreary"
                  boss-room-2 "large")
           (join! boss-room-1 "dreary"
                  boss-room-2 "ominous")

           ;;Starting Room
           (new-banana "" starting-room 2)

           ;;Storage Room
           (new-banana "" storage-room 3)
           (new-starch "" storage-room 2)
           (new-rice "" storage-room 3)
           (new-bone "" fridge-2 2)

           ;;Fridge Room
           (new-bone "" fridge-1 3)
           (new-bandage "" fridge-room 100)
           (new-banana "" fridge-room 3)
           (new-NPC "" fridge-room default-dialog)

           ;;Cabinet Room
           (new-sugar "" cabinet-1 1)
           (new-note "paper" true "the password is 76543" "cook the NPC to befriend the boss" cabinet-room)
           (new-monkey "stinky"  100 1 "banana" cabinet-room)

           ;;Humid Room
           (new-crane "tall" 100 1 "dango" humid-room)

           ;;Large Room
           (new-bandage "" large-room 50)
           (new-banana "" large-room 1)
           (new-starch "" large-room 4)
           (new-rice "" large-room 3)
           (new-dog "tough" 100 1 "bone" large-room)

           ;;Boss Room 1
           (new-boss "big fat" boss-room-1 0 2 2 2 2 2 40 3 1 1 #F "You've beaten the big fat boss")

           ;;Boss Room 2
           (new-boss "final" boss-room-2 10 10 10 10 10 10 100 5 3 3 #T "You've beaten the final boss")
           
           (check-containers!))))

;;;
;;; PUT YOUR WALKTHROUGHS HERE
;;;

(define (take-test)
  (begin (start-game)
         (break)
         (take (the banana))
         (check (= (person-banana# me) 2))))

(define (fridge-cabinet-note-test)
  (begin (start-game)
         (break)
         (go (the storage door))
         (look)
         (open (the fridge))
         (take-contents (the fridge))
         (check (= (person-bone# me) 2))
         (close (the fridge))
         (check (not (fridge-open? (the fridge))))
         (go (the starting door))
         (go (the cold door))
         (open (the fridge))
         (take-contents (the fridge))
         (close (the fridge))
         (go (the starting door))
         (go (the formal door))
         (examine (the note))
         (flip (the note))
         (examine (the note))
         (flip (the note))
         (examine (the note))
         (unlock (the cabinet) 76543)
         (open (the cabinet))
         (take-contents (the cabinet))
         (lock (the cabinet))
         (check (= (person-sugar# me) 10))
         (check (= (person-bone# me) 5))))

(define (NPC-test)
  (begin (start-game)
         (break)
         (go (the cold door))
         (talk (the NPC))
         (reply (the NPC) #T)
         (reply (the NPC) #T)
         (cook-NPC (the NPC))
         (talk (the NPC))
         (take (the NPC))
         (check (have? (the NPC)))))

(define (dog-check)
  (begin (start-game)
         (break)
         (take (the banana))
         (go (the storage door))
         (take (the banana))
         (take (the starch))
         (take (the rice))
         (open (the fridge))
         (take-contents (the fridge))
         (go (the large door))
         (take (the bandage))
         (take (the banana))
         (take (the starch))
         (take (the rice))
         (feed (the dog))
         (take (the dog))
         (check (have? (the dog)))
         (heal (the dog) 20)
         (train (the dog) 1)
         (check (= (person-banana# me) 6))
         (check (= (person-starch# me) 6))
         (check (= (person-rice# me) 6))
         (check (= (person-bone# me) 0))
         (check (= (person-healing-ability me) 30))
         (check (= (examine-level (the dog)) 2))
         (check (= (examine-life (the dog)) 120))
         (examine-preferred-food (the dog))
         ))

(define (befriend-boss)
  (begin (start-game)
         (break)
         (go (the cold door))
         (cook-NPC (the NPC))
         (take (the NPC))
         (go (the damp door))
         (go (the dreary door))
         (befriend (the boss))
         (take (the boss))
         (check (have? (the boss)))))

(define (win)
  (begin (start-game)
         (break)
         (take (the banana))
         (go (the storage door))
         (take (the banana))
         (take (the starch))
         (take (the rice))
         (open (the fridge))
         (take-contents (the fridge))
         (go (the large door))
         (take (the bandage))
         (take (the banana))
         (take (the starch))
         (take (the rice))
         (feed (the dog))
         (take (the dog))
         (go (the storage door))
         (go (the starting door))
         (go (the formal door))
         (examine (the note))
         (flip (the note))
         (examine (the note))
         (unlock (the cabinet) 76543)
         (open (the cabinet))
         (take-contents (the cabinet))
         (lock (the cabinet))
         (cook-dango)
         (feed (the monkey))
         (take (the monkey))
         (go (the starting door))
         (go (the cold door))
         (open (the fridge))
         (take-contents (the fridge))
         (take (the bandage))
         (take (the banana))
         (talk (the NPC))
         (reply (the NPC) #T)
         (reply (the NPC) #T)
         (cook-NPC (the NPC))
         (talk (the NPC))
         (take (the NPC))
         (go (the damp door))
         (feed (the crane))
         (take (the crane))
         (train (the monkey) 6)
         (train (the dog) 3)
         (heal (the monkey) 45)
         (heal (the dog) 45)
         (heal (the crane) 45)
         (go (the cold door))
         (go (the starting door))
         (go (the formal door))
         (go (the ominous door))
         (beat (the boss))
         (cook-dango)
         (cook-dango)
         (train (the crane) 3)
         (go (the dreary door))
         (beat (the boss))))


;;;
;;; UTILITIES
;;;

;; here: -> container
;; The current room the player is in
(define (here)
  (thing-location me))

;; stuff-here: -> (listof thing)
;; All the stuff in the room the player is in
(define (stuff-here)
  (container-accessible-contents (here)))

;; stuff-here-except-me: -> (listof thing)
;; All the stuff in the room the player is in except the player.
(define (stuff-here-except-me)
  (remove me (stuff-here)))

;; my-inventory: -> (listof thing)
;; List of things in the player's pockets.
(define (my-inventory)
  (container-accessible-contents me))

;; accessible-objects -> (listof thing)
;; All the objects that should be searched by find and the.
(define (accessible-objects)
  (append (stuff-here-except-me)
          (my-inventory)))

;; have?: thing -> boolean
;; True if the thing is in the player's pocket.
(define (have? thing)
  (eq? (thing-location thing)
       me))

;; have-a?: predicate -> boolean
;; True if the player as something satisfying predicate in their pocket.
(define (have-a? predicate)
  (ormap predicate
         (container-accessible-contents me)))

;; find-the: (listof string) -> object
;; Returns the object from (accessible-objects)
;; whose name contains the specified words.
(define (find-the words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (accessible-objects)))

;; find-within: container (listof string) -> object
;; Like find-the, but searches the contents of the container
;; whose name contains the specified words.
(define (find-within container words)
  (find (λ (o)
          (andmap (λ (name) (is-a? o name))
                  words))
        (container-accessible-contents container)))

;; find: (object->boolean) (listof thing) -> object
;; Search list for an object matching predicate.
(define (find predicate? list)
  (local [(define matches
            (filter predicate? list))]
    (case (length matches)
      [(0) (error "There's nothing like that here")]
      [(1) (first matches)]
      [else (error "Which one?")])))

;; everything: -> (listof container)
;; Returns all the objects reachable from the player in the game
;; world.  So if you create an object that's in a room the player
;; has no door to, it won't appear in this list.
(define (everything)
  (local [(define all-containers '())
          ; Add container, and then recursively add its contents
          ; and location and/or destination, as appropriate.
          (define (walk container)
            ; Ignore the container if its already in our list
            (unless (member container all-containers)
              (begin (set! all-containers
                           (cons container all-containers))
                     ; Add its contents
                     (for-each walk (container-contents container))
                     ; If it's a door, include its destination
                     (when (door? container)
                       (walk (door-destination container)))
                     ; If  it's a thing, include its location.
                     (when (thing? container)
                       (walk (thing-location container))))))]
    ; Start the recursion with the player
    (begin (walk me)
           all-containers)))

;; print-everything: -> void
;; Prints all the objects in the game.
(define (print-everything)
  (begin (display-line "All objects in the game:")
         (for-each print-description (everything))))

;; every: (container -> boolean) -> (listof container)
;; A list of all the objects from (everything) that satisfy
;; the predicate.
(define (every predicate?)
  (filter predicate? (everything)))

;; print-every: (container -> boolean) -> void
;; Prints all the objects satisfying predicate.
(define (print-every predicate?)
  (for-each print-description (every predicate?)))

;; check-containers: -> void
;; Throw an exception if there is an thing whose location and
;; container disagree with one another.
(define (check-containers!)
  (for-each (λ (container)
              (for-each (λ (thing)
                          (unless (eq? (thing-location thing)
                                       container)
                            (error (description container)
                                   " has "
                                   (description thing)
                                   " in its contents list but "
                                   (description thing)
                                   " has a different location.")))
                        (container-contents container)))
            (everything)))

;; is-a?: object word -> boolean
;; True if word appears in the description of the object
;; or is the name of one of its types
(define (is-a? obj word)
  (let* ((str (if (symbol? word)
                  (symbol->string word)
                  word))
         (probe (name->type-predicate str)))
    (if (eq? probe #f)
        (member str (description-word-list obj))
        (probe obj))))

;; display-line: object -> void
;; EFFECT: prints object using display, and then starts a new line.
(define (display-line what)
  (begin (display what)
         (newline)
         (void)))

;; words->string: (listof string) -> string
;; Converts a list of one-word strings into a single string,
;; e.g. '("a" "red" "door") -> "a red door"
(define (words->string word-list)
  (string-append (first word-list)
                 (apply string-append
                        (map (λ (word)
                               (string-append " " word))
                             (rest word-list)))))

;; string->words: string -> (listof string)
;; Converts a string containing words to a list of the individual
;; words.  Inverse of words->string.
(define (string->words string)
  (string-split string))

;; add-a-or-an: (listof string) -> (listof string)
;; Prefixes a list of words with "a" or "an", depending
;; on whether the first word in the list begins with a
;; vowel.
(define (add-a-or-an word-list)
  (local [(define first-word (first word-list))
          (define first-char (substring first-word 0 1))
          (define starts-with-vowel? (string-contains? first-char "aeiou"))]
    (cons (if starts-with-vowel?
              "an"
              "a")
          word-list)))

;;
;; The following calls are filling in blanks in the other files.
;; This is needed because this file is in a different langauge than
;; the others.
;;
(set-find-the! find-the)
(set-find-within! find-within)
(set-restart-game! (λ () (start-game)))
(define (game-print object)
  (cond [(void? object)
         (void)]
        [(object? object)
         (print-description object)]
        [else (write object)]))

(current-print game-print)
   
;;;
;;; Start it up
;;;

(start-game)
(look)

