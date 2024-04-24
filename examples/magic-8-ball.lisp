
(defun rand-nth (lst)
  (nth (random (length lst)) lst))

(defun demo:rotate (l) (append (cdr l) (cons (car l) ())))

(defun spin-and-show (state)
  (pixels (car state))
  (demo:rotate state))

(defun flash (n color d)
  (dotimes (x n)
    (pixels)
    (delay d)
    (pixels color)
    (delay d)))

(defun is-touched () (< 500 (apply 'max (touchpads))))

(defvar *bright-color* 32)

(defvar *dim-color* 10)

(defun is-green (color)
  (if (= *bright-color* (car (cdr (to-rgb color)))) t nil))

(defun is-red (color)
  (if (= *bright-color* (car (to-rgb color))) t nil))

(defun get-sage ()
  (let ((sage-noun  (list "Ghost of John McCarthy" "Recursive Sage" "CONS CDR Seer" "Lambda Oracle" "Symbolic Prophet"))
        (sage-adv   (list "recursively" "iteratively"))
        (sage-verb  (list "contemplates" "muses" "ponders" "pontificates" "reflects")))
    (format nil "The ~a ~a ~a" (rand-nth sage-noun) (rand-nth sage-adv) (rand-nth sage-verb)) ))

(defun get-sage-advice (color)
  (let ((yes-text   (list "It is certain" "It is decidedly so" "Without a doubt" "Yes definitely" "You may rely on it" "As I see it, yes" "Most likely" "Outlook good" "Yes" "Signs point to yes"))
        (maybe-text (list "Reply hazy, try again"  "Ask again later"  "Better not tell you now"  "Cannot predict now"))
        (no-text    (list "Don't count on it"  "My reply is no"  "My sources say no"  "Outlook not so good"  "Very doubtful")) )
    (cond ((is-green color) (rand-nth yes-text))
          ((is-red color)   (rand-nth no-text))
          (t                (rand-nth maybe-text)) )))

(defun magic-8-ball ()
  (format t "Magic LISP Ball!  Press and hold a touchpad to begin.~%~%")
  (let ((state 'waiting)
        (wait-colors (list (list (to-color 0 0 *dim-color*) (to-color 0 0 0)           (to-color 0 0 *dim-color*) (to-color 0 0 0))
                           (list (to-color 0 0 0)           (to-color 0 0 *dim-color*) (to-color 0 0 0)           (to-color 0 0 *dim-color*))))
        (roll-colors (list (to-color *bright-color* 0 0)
                           (to-color 0 *bright-color* 0)
                           (to-color 1 1 0)
                           (to-color 0 *bright-color* 0))) )
    (loop
       (cond ((eq state 'waiting) (progn
                                    (setq wait-colors (spin-and-show wait-colors))
                                    (delay 500)
                                    (setq state (if (is-touched)
                                                    (progn
                                                      (format t "~a " (get-sage))
                                                      'rolling)
                                                    'waiting)) ))
             ((eq state 'rolling) (progn
                                    (setq roll-colors (spin-and-show roll-colors))
                                    (format t ".")                                
                                    (delay 23)
                                    (setq state (if (is-touched) 'rolling 'stopped)) ))
             ((eq state 'stopped) (progn
                                    (format t " ~a~%" (get-sage-advice (car roll-colors))) 
                                    (flash 3 (car roll-colors) 200)
                                    (delay 2000)
                                    (setq state 'waiting))) ))))
