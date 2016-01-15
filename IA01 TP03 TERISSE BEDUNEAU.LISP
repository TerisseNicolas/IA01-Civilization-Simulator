; ===========================================================================================================
; Global variables ==========================================================================================
; ===========================================================================================================

; Functions needed to initiamize global variables============================================================
(defun create-premise-from-model (modelName) "Create the premise from a model"
  (let ((return-value))
    (setq return-value (append return-value (list 'population (car (symbol-value modelName)))))
    (dolist (r (cddr (symbol-value modelName)))
      (setq return-value (append return-value (list (car r) (cadddr r))))
    )
    return-value
  )
) 

; RESSOURCE==================================================================================================
(defparameter population nil)
(defparameter gold nil)
(defparameter silver nil)
(defparameter wood nil)
(defparameter food nil)
(defparameter power nil)

#|
(defparameter goldMineModel '(60 1 (gold 25 0 0) (silver 0 0 0) (wood 0 0 0) (power 0 24000 0) (food 0 0 0)))
(defparameter silverMineModel '(30 1 (gold 0 0 0) (silver 188 0 0) (wood 0 0 0) (power 0 24000 0) (food 0 0 0)))
(defparameter sawmillModel '(40 1 (gold 0 0 0) (silver 0 0 0) (wood 30618 0 0) (power 0 24000 0) (food 0 0 0)))
(defparameter farmModel '(50 1 (gold 0 0 0) (silver 0 0 0)(wood 0 0 0)(power 0 24000 0)(food 35100 0 0)))
(defparameter powerPlantModel '(100 1 (gold 0 0 0) (silver 0 0 0) (wood 0 0 20) (power 117405000 24000 0) (food 0 0 0)))
|#

(defparameter goldMineModel '(60 1 (gold 25 0 50) (silver 0 2 0) (wood 1 1 15) (power 0 0 23) (food 0 2 0)))
(defparameter silverMineModel '(50 1 (gold 0 1 0) (silver 63 0 1) (wood 0 2 51) (power 0 13 142) (food 0 7 0)))
(defparameter sawmillModel '(40 1 (gold 1 0 0) (silver 1 0 20) (wood 30618 0 0) (power 0 10 0) (food 0 0 0)))
(defparameter farmModel '(75 1 (gold 0 2 5) (silver 4 0 5)(wood 0 0 42)(power 0 14 75)(food 7624 0 0)))
(defparameter powerPlantModel '(200 1 (gold 2 0 50) (silver 0 5 42) (wood 0 0 43) (power 1920 12 0) (food 0 15 0)))


(defparameter *FB* '(population gold silver wood food power))
(defparameter *RB* `((R1 ,(create-premise-from-model 'goldmineModel) (create-building goldmine))
               (R2 ,(create-premise-from-model 'silvermineModel) (create-building silvermine))
               (R3 ,(create-premise-from-model 'sawmillModel) (create-building sawmill))
               (R4 ,(create-premise-from-model 'farmModel) (create-building farm))
               (R5 ,(create-premise-from-model 'powerPlantModel) (create-building powerPlant))))
(defparameter *general-FB* '((natality 0.01238)
                       (mortality 0.00916)
                       ;(feed-coef 0.527)
                       (feed-coef 0.5)
                       (moral 1)
                       (productivity 1)
                       (innovation 0)))
(defparameter *general-RB* '((GR1 (apply-natality))
                       (GR2 (apply-mortality))
                       (GR3 (harvest-ressource-production))
                       (GR4 (ressource-consumption))
                       (GR5 (feed-population))))

; GAME=======================================================================================================
(defparameter ITERATIONS-BEFORE-APPLYING-GENERAL-RULES 150)
(defparameter LOOPS-BEFORE-NEW-INTERACTION-WITH-USER 10)
(defparameter TOTAL-LIMIT 8000)
(defparameter TOTAL-INFERENCES 0)
(defparameter VERBOSE t)
(defparameter INTERACTIONS 'Y)
(defparameter INFERENCES-LEFT ITERATIONS-BEFORE-APPLYING-GENERAL-RULES)
(defparameter STRATEGY 'BALANCED)
(defparameter CRITIC-STRATEGY 'BALANCED)
(defparameter RULES-HISTORIC nil)

; ===========================================================================================================
; FB ========================================================================================================
; ===========================================================================================================

; POPULATION=================================================================================================
(defun get-population-number () "Return the total population number"
  (cadr population)
)
(defun get-inactive-population () "Return the inactive amount of population"
  (car population)
)
(defun get-active-population () "Return the active amount of population"
  (- (cadr population) (car population))
)
(defun increase-population (amount) "Increase the size of the population"
  (setq population (list (+ (car population) amount) (+ (cadr population) amount)))
)
(defun decrease-population (amount) "Decrease the size of the population. Kill inactive people first"
  (if (<= amount (get-inactive-population))
      (setq population (list (- (car population) amount) (- (cadr population) amount)))
      ; else : gerer le cas ou il faut tuer des gens actifs--------------------------------
  )
)
(defun inactivate-population (amount) "Inactivate someone. Return nil if failed"
  (if (>= (get-active-population) amount)
      (setq population (list (+ (car population) amount) (cadr population)))
      nil
  )
)
(defun activate-population (amount) "Activate someone. Return nil if failed"
  (if (>= (get-inactive-population) amount)
      (setq population (list (- (car population) amount) (cadr population)))
      nil
  )
)

; RESSOURCES===============================================================================================
(defun get-ressource-stock (ressource) "Return the stock of a ressource"
  (car (symbol-value ressource))
)
(defun get-ressource-production (ressource) "Return the production of a ressource"
  (cadr (symbol-value ressource))
)
(defun increase-ressource-stock (ressource n) "Increase the ressource stock by n units"
  (set ressource (list (+ (car (symbol-value ressource)) n) (cadr (symbol-value ressource))))
)
(defun decrease-ressource-stock (ressource n) "Remove n units from the ressource stock. Return nil if failed"
  (if (> (- (get-ressource-stock ressource) n) 0)
      (set ressource (list (- (car (symbol-value ressource)) n) (cadr (symbol-value ressource))))
      (set ressource (list 0 (cadr (symbol-value ressource))))
  )
)
(defun increase-ressource-production (ressource n) "Increase by n units the production of a ressource"
  (set ressource (list (car (symbol-value ressource)) (+ (cadr (symbol-value ressource)) n)))
)
(defun decrease-ressource-production (ressource n) "Decrease by n units the production of a ressource"
  (set ressource (list (car (symbol-value ressource)) (- (cadr (symbol-value ressource)) n)))
)
(defun get-ressource-min-stock ()"Return the name of the ressouce having the littlest stock"
  (let ((minor 'gold))
    (dolist (r '(silver wood food power))
      (if (< (get-ressource-stock r) (get-ressource-stock minor))
          (setq minor r)
      )
    )
    minor
  )
)
(defun get-ressource-max-stock ()"Return the name of the ressouce having the biggest stock"
  (let ((max 'gold))
    (dolist (r '(silver wood food power))
      (if (> (get-ressource-stock r) (get-ressource-stock max))
          (setq max r)
      )
    )
    max
  )
)
(defun get-ressource-by-increasing-stock () "Return the list of ressource sorted by minor stock to biggest stock"
  (sort '(gold silver food wood power) (lambda (x y) (< (car (symbol-value x)) (car (symbol-value y)))))
)
(defun get-ressource-by-decreasing-stock () "Return the list of ressource sorted by biggest to minor stock"
  (reverse (get-ressource-by-increasing-stock))
)      

; BUILDINGS==================================================================================================
(defun create-building (typeName) "Creation of a building"
  (let ((b (gentemp (string typeName))))
    (cond ((equal typeName 'goldMine)(setf (symbol-value b) goldMineModel))
          ((equal typename 'silverMine)(setf (symbol-value b) silverMineModel))
          ((equal typeName 'sawmill)(setf (symbol-value b) sawmillModel))
          ((equal typeName 'farm)(setf (symbol-value b) farmModel))
          ((equal typeName 'powerPlant) (setf (symbol-value b) powerPlantModel))
    )
    (activate-population (car (symbol-value b)))
    (dolist (r (cddr (symbol-value b)))
       (decrease-ressource-stock (car r) (cadddr r))
       (increase-ressource-production (car r) (cadr r))
    )
    (push b *FB*)
  )
)
(defun inactivate-building (name) "Inactivate a building and free the population"
  (if (equal (cadr (symbol-value name)) 1)
      (progn (inactivate-population (car (symbol-value name)))
             (setf (cadr (symbol-value name)) 0)
             (dolist (r (cddr (symbol-value name)))
               (decrease-ressource-production (car r) (cadr r))
             )
      )
  )
)
(defun activate-building (name) "Activate a building and hire people"
  (if (equal (cadr (symbol-value name)) 0)
      (progn (activate-population (car (symbol-value name)))
             (setf (cadr (symbol-value name)) 1)
             (dolist (r (cddr (symbol-value name)))
               (increase-ressource-production (car r) (cadr r))
             )
      )
  )
)
(defun conditions-from-model (building) "Return a list of conditions from the building model"
  (let ((return-value)(modelName (read-from-string (concatenate 'string (string building) "Model"))))
    (setq return-value (append return-value (list (car (symbol-value modelName)))))
    (dolist (r (cddr (symbol-value modelName)))
      (setq return-value (append return-value (list (list (car r) (cadddr r)))))
    )
    return-value
  )  
)
(defun buildable? (name) "Return t if the building can be build, nil otherwise"
  (let ((conditions (conditions-from-model name))(return-value t))
    (if (< (get-inactive-population)(car conditions))
        (setq return-value nil)
        (dolist (r (cdr conditions))
           (if (< (get-ressource-stock (car r)) (cadr r))
               (setq return-value nil)
           )
        )
    )
    return-value
  )
)

(defun print-beginning (base number) "Print the beginning of the fact base"
 (if (> number 0)
     (progn (format t "~s " (car base))
            (print-beginning (cdr base) (- number 1))
     )
 )
)
; ===========================================================================================================
; RB ========================================================================================================
; ===========================================================================================================

(defun premise (rule) "Get the premises for a rule"
  (let ((return-value))
    (dolist (r *RB*)
      (if (and (not return-value) (equal (car r) rule))
          (setq return-value (cadr r))
      )
    )
    return-value
  )
)
(defun conclusion (rule) "Get the conclusion for a rule"
  (let ((return-value))
    (dolist (r *RB*)
      (if (and (not return-value) (equal (car r) rule))
          (setq return-value (caddr r))
      )
    )
    return-value
  )
)
(defun usable-rule? (rule) "Return t if the rule is usable, nil otherwise"
  (let ((premises (premise rule))(return-value t))
    (loop while premises do
          (if (< (get-ressource-stock (car premises)) (cadr premises))
              (progn (setq return-value nil)
                     (return)
              )
              (setq premises (cddr premises))
          )
    )
    return-value
  )
)
(defun usable-premises? (premises) "Test if the premises are verified. Return t then, nil otherwise"
  (let ((return-value t))
    (loop while premises do
          (if (< (get-ressource-stock (car premises)) (cadr premises))
              (progn (setq return-value nil)
                     (return)
              )
              (setq premises (cddr premises))
          )
    )
    return-value
  )
)
(defun usable-rules () "Return the list of all usable rules in *RB*"
  (let ((return-value))
    (dolist (r *RB*)
      (if (usable-premises? (cadr r))
          (push (car r) return-value)
      )
    )
    return-value
  )
)
(defun apply-rule (name) "Apply a rule of *RB*"
  (let ((conclusion (conclusion name)))
    (apply (car conclusion) (cdr conclusion))
    t
  )
)
(defun unapply-rule (name) "Reverse the application of te rule"
  name
  ; do nothing for now
)

; rules functions ==========================================================================================

; Heuristics ===============================================================================================
(defun main-heuristic-on-rules (rules) "Apply an heuristic to sort the rules, the first must be used"
  (let ((return-value)(ressource-to-produce strategy))
     (if (equal ressource-to-produce 'BALANCED)
         (setq ressource-to-produce (get-ressource-min-stock))
     )
     (dolist (r '(gold silver wood power food))
       (if (> (anticipate-ressource-consumption r) (anticipate-ressource-stock r))
           (progn (setq CRITIC-STRATEGY r)
                  (setq ressource-to-produce CRITIC-STRATEGY)
           )
       )
     )
     (if VERBOSE (format t "Selectionnated ressource to produce: ~s~%" ressource-to-produce))
     (setq return-value (sort rules (lambda (x y) (< (get-ressource-production-in-model (cadr (conclusion x)) ressource-to-produce)
                                                     (get-ressource-production-in-model (cadr (conclusion y)) ressource-to-produce)))))
     (reverse return-value)
  )
)
(defun get-ressource-production-in-model (building ressource) "Get a specific ressource production from the building model"
  (cadr (assoc ressource(cddr (symbol-value (read-from-string (concatenate 'string (string building) "Model"))))))
)
(defun anticipate-ressource-stock (ressource) "Get the amount of a ressource that will be produced"
  (+ (car (symbol-value ressource)) (cadr (symbol-value ressource)))
)
(defun anticipate-ressource-consumption (ressource) "Get the amount of a ressource that will be consume"
  (let ((return-value 0))
    (dolist (f *FB*)
      (if (and (not (equal f 'population))(not (equal f 'gold))(not (equal f 'silver))
               (not (equal f 'wood)) (not (equal f 'food))(not (equal f 'power)))
          (dolist (r (cddr (symbol-value f)))
            (if (equal (car r) ressource)
                (setq return-value (+ return-value (caddr r)))
            )
          )
      )
    )
    (if (equal ressource 'FOOD)
        (setq return-value (+ return-value (get-population-number)))
    )
    return-value
  )
)

; ===========================================================================================================
; general-FB ================================================================================================
; ===========================================================================================================

(defun get-general-fact (name) "Get the value of a general fact"
  (cadr (assoc name *general-FB*))
)

; ===========================================================================================================
; general-RB ================================================================================================
; ===========================================================================================================

(defun get-general-rule (name) "Get the list of the function and its arguments for a general-rule"
  (cadr (assoc name *general-RB*))
)
(defun apply-all-general-rules () "Apply all general-rules"
  (dolist (gr *general-RB*)
    (apply (caadr gr) (cdadr gr))
  )
)

; general-rules functions ===================================================================================
(defun apply-natality (&optional arg) "Apply the natality rate to the population number"
  arg
  (increase-population (floor (* (get-population-number) (get-general-fact 'natality))))
)
(defun apply-mortality (&optional arg) "Apply the mortality rate to the population number"
  arg
  (decrease-population (floor (* (get-population-number) (get-general-fact 'mortality))))
)
(defun harvest-ressource-production (&optional arg) "Harvest all ressources producted"
  arg
  (dolist (r '(gold silver wood food power))
     (increase-ressource-stock r (get-ressource-production r))
  )
)
(defun ressource-consumption (&optional arg) "Consume ressource by buildings"
  arg
  (dolist (f *FB*)
    (if (and (not (equal f 'population))(not (equal f 'gold))(not (equal f 'silver))
             (not (equal f 'wood)) (not (equal f 'food))(not (equal f power)))
        (dolist (r (cddr (symbol-value f)))
          (decrease-ressource-stock (car r) (caddr r))
        )
    )
  )
)
(defun feed-population (&optional arg) "Feed the population with food"
  arg
  (decrease-ressource-stock 'FOOD (floor (* (get-population-number) (get-general-fact 'feed-coef))))
)
(defun test-args (arg1 arg2)
  (format t "~s ~s~%" arg1 arg2)
)

; ===========================================================================================================
; GUI =======================================================================================================
; ===========================================================================================================
(defun welcome-banner() "Print the openning banner"
  (print "==================================================================================================")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                             WELCOME TO THE CIVILISATION SIMULATOR                              =")
  (print "=                                                                                                =")
  (print "=                                            version 1                                           =")
  (print "=                                                                                                =")
  (print "=                             by BEDUNNEAU Guillaume TERISSE Nicolas                             =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "==================================================================================================")
  (values)
)
(defun main-interaction-with-user () "First interaction with the user to init global variables"
  (format t "~%~%~%")
  (format t "The aim of this civilization simulator is to get the optimized strategy for the viability of buildings, ressources and population. This expert system, starting with an initial state based on an expertise over different fields as demography, civil engineering .. will optimize the development of that civilisation. The initial state is composed by ressource stocks, an amount of population, possible constructed buildings. The purpose is to never stop simulating and see how the selected strategy is viable.~%~%")
  (format t "You are now going to choose how to simulate the civilization~%")
  (format t "Do you want to:~%")
  (format t "   -start a new simulation (S)~%")
  (format t "   -continue the previous one (C) !! not working yet !!~%")
  (let ((continueNew (read)) configuration)
    (if (equal continueNew 'S)
        (progn (format t "Do you want to configure setting or let the magic happens~%")
               (format t "   -configure (C)~%")
               (format t "   -magic (M)~%")
               (setq configuration (read))
               (if (equal 'M configuration)
                   (magic-configuration)
                   (if (equal 'C configuration)
                       (manual-configuration)
                       (progn (format t "Option error, try again ..~%")
                              (main-interaction-with-user)
                       )
                   )
               )
          )
          (if (equal continueNew 'C)
              (progn (format t "Loading the previous game ...~%")
                     (format t "Loading previous databases ...~%")
                     (load-previous-game)
              )
              (progn (format t "Option error, try again ...~%")
                     (main-interaction-with-user)
              )
          )
    )                              
  )
  (values)
)
(defun minor-interaction-with-user () "Interaction if the user want to change his strategy"
  (format t "~%Do you want to change the strategy? (y/n)")
  (let ((choice (read)))
    (if (equal choice 'Y)
        (progn (format t "Select a new strategy:~%")
               (format t "  BALANCED (0)~%")
               (format t "  GOLD (1)~%")
               (format t "  SILVER (2)~%")
               (format t "  FOOd (3)~%")
               (format t "  WOOD (4)~%")
               (format t "  POWER (5)~%")
               (setq choice (read))
               (if (equal choice 0)
                   (setq STRATEGY 'BALANCED)
                   (if (equal choice 1)
                       (setq STRATEGY 'GOLD)
                       (if (equal choice 2)
                           (setq STRATEGY 'SILVER)
                           (if (equal choice 3)
                               (setq STRATEGY 'FOOD)      
                               (if (equal choice 4)
                                   (setq STRATEGY 'WOOD)
                                   (if (equal choice 5)
                                       (setq STRATEGY 'POWER)
                                       (progn (format t "Option error, try again ...")
                                              (minor-interaction-with-user)))))))))
        (if (not (equal choice 'N))
            (progn (format t "Option error, try again ...")
                   (minor-interaction-with-user)
            )
        )
    )
  )
  (values)
)
(defun print-population () "Print the current state of the population"
  (format t "~%Current state of the population:~%")
  (format t "Total population: ~d~%" (get-population-number))
  (format t "Active population: ~d ~%" (get-active-population))
  (format t "Inactive population: ~d~%" (get-inactive-population))
  (values)
)
(defun print-ressource (name) "Print the actual state of a ressource"
  (format t "Ressource : ~s~%" name)
  (format t "  stock: ~d~%  production: ~d~%" (car (symbol-value name)) (cadr (symbol-value name)))
)
(defun print-ressources () "Print the actual state of all ressources"
  (format t "~% Actual state of ressources :~%")
  (format t "Inferences: ~d~%" TOTAL-INFERENCES)
  (format t "Main strategy: ~s~%Critical strategy: ~s~%"  STRATEGY CRITIC-STRATEGY)
  (print-population)
  (print-ressource 'gold)
  (print-ressource 'silver)
  (print-ressource 'wood)
  (print-ressource 'food)
  (print-ressource 'power)
  (values)
)
; ===========================================================================================================
; inference engine ==========================================================================================
; ===========================================================================================================
(defun game ()
  (welcome-banner)
  (main-interaction-with-user)
  (let ((keep-playing 1))
    (loop while (equal keep-playing 1) do
               (dotimes (x LOOPS-BEFORE-NEW-INTERACTION-WITH-USER t)
                   (if (and (test-validity-of-current-state) (< TOTAL-INFERENCES TOTAL-LIMIT))
                          (progn (print-ressources)
                                 (setq INFERENCES-LEFT (* ITERATIONS-BEFORE-APPLYING-GENERAL-RULES (floor (log (floor (get-population-number)) 10))))
                                 (engine)
                                 (apply-all-general-rules)
                                 (if (equal INTERACTIONS 'Y)
                                     (minor-interaction-with-user)
                                 )
                          )
                          (progn (state-not-valid)
                                 (setq keep-playing 0)
                                 (return)
                          )
                   )
                      
               )
    )
  )
  (quit-the-game)
)
(defun engine () "Inferences engine"
  (if (> INFERENCES-LEFT 0)
      (progn (setq INFERENCES-LEFT (- INFERENCES-LEFT 1))
             (setq TOTAL-INFERENCES (+ TOTAL-INFERENCES 1))
             (let ((usable-rules (usable-rules)))
                  (setq usable-rules (main-heuristic-on-rules usable-rules))
                  (dolist (r usable-rules)
                    (if (test-validity-of-current-state)
                        (progn (push r RULES-HISTORIC)
                               (if VERBOSE (format t "Applying rule ~s~%" r))
                               (apply-rule r)
                               (engine)
                        )
                    )
                  )
             )
      )
  )
)
(defun engine2 () "Inferences engine"
  (if (> INFERENCES-LEFT 0)
      (progn (setq INFERENCES-LEFT (- INFERENCES-LEFT 1))
             (setq TOTAL-INFERENCES (+ TOTAL-INFERENCES 1))
             (let ((usable-rules (usable-rules)))
                  (setq usable-rules (main-heuristic-on-rules usable-rules))
                  (dolist (r usable-rules)
                    (if (test-validity-of-current-state)
                        (progn (push r RULES-HISTORIC)
                               (if VERBOSE (format t "Applying rule ~s~%" r))
                               (apply-rule r)
                        )
                    )
                  )
                  (if (test-validity-of-current-state)
                      (engine2)
                  )
             )
      )
  )
)
(defun magic-configuration () "Configure a new game with a balanced configuration"
#|
  (defparameter population '(46105000 66553766))
  (defparameter gold '(2430000 0))
  (defparameter silver '(18889200 0))
  (defparameter wood '(306180000 0))
  (defparameter food '(3510000000 0))
  (defparameter power '(11740500000000 0))
  (defparameter INTERACTIONS 'N)
  (defparameter STRATEGY 'BALANCED)
  (defparameter TOTAL-INFERENCES 0)
  (defparameter CRITIC-STRATEGY 'BALANCED)
  (defparameter RULES-HISTORIC nil)

  (dotimes (x 1000 t)
    (create-building 'goldMine)
    (create-building 'silverMine)
    (create-building 'farm)
    (create-building 'sawMill)
    (create-building 'powerPlant)
  )

|#
  (defparameter population '(999 2000))
  (defparameter gold '(100000 0))
  (defparameter silver '(10000 0))
  (defparameter wood '(90000 0))
  (defparameter food '(70000 0))
  (defparameter power '(700000 0))
  (defparameter INTERACTIONS 'N)
  (defparameter STRATEGY 'BALANCED)
  (defparameter TOTAL-INFERENCES 0)
  (defparameter CRITIC-STRATEGY 'BALANCED)
  (defparameter RULES-HISTORIC nil)

  (dotimes (x 2 t)
    (create-building 'goldMine)
    (create-building 'silverMine)
    (create-building 'farm)
    (create-building 'sawMill)
    (create-building 'powerPlant)
  )
)
(defun manual-configuration () "Ask the user to configure the options"
  (format t "~%~%You are going to manually configure the game~%")
  (format t "Select a strategy:~%")
  (format t "  BALANCED (0)~%")
  (format t "  GOLD (1)~%")
  (format t "  SILVER (2)~%")
  (format t "  FOOd (3)~%")
  (format t "  WOOD (4)~%")
  (format t "  POWER (5)~%")
  (let ((choice (read)) choice2)
       (if (equal choice 0)
           (setq STRATEGY 'BALANCED)
           (if (equal choice 1)
               (setq STRATEGY 'GOLD)
               (if (equal choice 2)
                   (setq STRATEGY 'SILVER)
                   (if (equal choice 3)
                       (setq STRATEGY 'FOOD)      
                       (if (equal choice 4)
                           (setq STRATEGY 'WOOD)
                           (if (equal choice 5)
                               (setq STRATEGY 'POWER)
                               (progn (format t "Option error, try again ...~%")
                                      (manual-configuration))))))))
       (format t "You are now going to initialize the population and ressources~%")
       (loop
          (format t "~%Enter the total population: ")
          (setq choice (read))
          (when (> choice 0) (return))
       )
       (loop
          (format t "~%Enter the inactive population (between 0 and ~d): " choice)
          (setq choice2 (read))
          (when (and (>= choice2 0)(<= choice2 choice)) (return))
       )
       (setq population (list choice2 choice))
       (dolist (r '(GOLD SILVER WOOD FOOD POWER))
         (format t "~%Initializing ~s~%" r)
         (loop
            (format t "Enter the stock: ")
            (setq choice (read))
            (when (> choice 0) (return))
         )
         (setf (symbol-value r) (list choice 0))
       )
       (format t "You are now going to build the first buildings~%")
       (dolist (r '(goldMine silverMine sawMill farm powerPlant))
         (format t "~%Building ~s~%" r)
         (loop
            (format t "How many do you want to build? ")
            (setq choice (read))
            (when (> choice 0) (return))
         )
         (dotimes (x choice t)
           (if (buildable? r)
               (progn (create-building r)
                      (format t "~s build~%" r)
               )
               (progn (format t "You cannot build this building.. ~%")
                      (return)
               )
           )
         )
       )
       (format t "~%And finally, do you want to interact with the engine during the process (e.g. change strategy)(Y/N)?")
       (loop
          (setq choice2 (read))
          (when (or (equal choice2 'N)(equal choice2 'Y)) (return))
          (format t "~%Incorrect option: Y or N .. ~%")
       )
       (setq INTERACTIONS choice2)
       (defparameter TOTAL-INFERENCES 0)
       (defparameter CRITIC-STRATEGY 'BALANCED)
       (defparameter RULES-HISTORIC nil)

  )
  (values)
)
(defun load-previous-game () "Load the previous simulation"
  ;load data from file
  ;init vars
)
(defun test-validity-of-current-state () "Test the validity of the state of the game"
  (let ((return-value t))
    (if (equal (get-population-number) 0)
        (setq return-value nil)
        (dolist (r '(gold silver food wood power))
          (if (equal (get-ressource-stock r) 0)
              (setq return-value nil)
          )
        )
    )
    return-value
  )
)
(defun state-not-valid () "Show why the state of the civilization is not valid anymore"
  (format t "~%~%The current state of the civilization is not valid anymore~%")
  (print-ressources)
  (values)
)
(defun quit-the-game () "Final actions before quitting"
  ;save bases
  (format t "~%Historic of applyed ryles:~%")
  (print-beginning RULES-HISTORIC 200)
  (format t "~%Fact base:~%")
  (print-beginning *FB* 200)
  (print "==================================================================================================")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                          GOOD BYE !!!                                          =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                     CIVILISATION SIMULATOR                                     =")
  (print "=                                                                                                =")
  (print "=                                            version 1                                           =")
  (print "=                                                                                                =")
  (print "=                             by BEDUNNEAU Guillaume TERISSE Nicolas                             =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "=                                                                                                =")
  (print "==================================================================================================")
  (values)
)