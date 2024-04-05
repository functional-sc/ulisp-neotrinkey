;;;; Emacs Examples for uLisp

;;; How to Hack with Emacs

;;
;; Remember to include the config/dot-emacs.el file within your
;; .emacs configuration file
;;
;; Personalize it by selecting the correct port.
;;
;; Compliments will with paredit-mode!
;;
;;
;; Start by running "M-x ulisp-setup-workspace"
;;
;; Click or "C-x o" to switch to the other pane and "~" to break
;; Click or "C-x o" to switch back
;; 
;; "C-x e" is used to evaluate the last expression
;; 
;; Place the cursor at the end of the expression and hit "C-x e"
;; 
;;   (+ 1 2 3)  
;;            ^  <-- in this spot!
;;


(+ 1 2 3)   ; try it yourself here!


;;
;; Putting the cursor right after 'list' and "C-x e" will only evaluate
;; the list and not the 'defvar'.  
;; 
;; Try it
;; 


(defvar my-colors (list
                   (to-color 64 0 0)
                   (to-color 32 0 0)
                   0
                   0))


;;
;; NEO PIXELS
;;

;;
;; The four lights on your device are Neo Pixels.  Each is capable of being
;; individually addressed to display any color by blending simple shades of
;; red, green and blue.
;; 
;; Easily set all the NeoPixels to the same color, provide values for red,
;; green, and blue ranging from 0 (off) to 255 (very bright), like this:
;;

(pixels 64 0  0)  ; all red

(pixels 0  64 0)  ; all green

(pixels 0  0  64) ; all blue

(pixels 64 0  32) ; all purple

(pixels 0  0  0)  ; off

(pixels)          ; also off


;;
;; To address them individually, utilize the `(to-color)` function to
;; generate distinct RGB colors for each of the four Neo Pixels:
;;

(pixels 
  (to-color 64 0 0)   
  (to-color 0 64 0)   
  (to-color 0 0 64)   
  (to-color 64 0 32)) 

;;
;; Or  pass in a list of colors, in this case:
;; bright red, dimmer red, off and off
;;

(defvar my-colors (list 
                   (to-color 64 0 0) 
                   (to-color 32 0 0) 
                   0                 
                   0))               

(pixels my-colors) ; display the list of colors

;;
;; TOUCHPADS
;;

;; no args it returns a list of both cap touch devices

;; CTRL l selects the form
;; CTRL e evaluates it

(touchpads)

;; an argument it returns the value of the respective device
(touchpads 1)

;; an argument it returns the value of the respective device
(touchpads 2)

;; are either being touched?
;; above 500 seems about right
(when (< 500 (apply max (touchpads)))
    (progn
      (format t "touched!")
      t))

;; test loop
(loop (format t "QT1: ~a  QT2:~a~%" (touchpads 1) (touchpads 2)) (delay 100))

;; on error it rerturns 0
(touchpads 89)

(touchpads 1 2 3)

;;
;; DEMO
;;

;; 
;; Easily change the color of the lights and save it to run the next time
;; you plug it in.  Remember `to-color` expects red, green and blue.
;; This simple modification rotates two low-level red lights:
;; 
(defun mydemo ()
  (demo:run-events (list (to-color 4 0 0)
                         (to-color 2 0 0)
                         0 0)))

(save-image 'mydemo)

(mydemo)



;;
;; OMG it's broken!  
;;
;; Check your connection settings within your .emacs file
;;   * serial-term   /dev/tty/ACM0       (this may vary)
;;

(format t "Happy hacking ~a!~%" 'friend)
