;;;; Visual Studio Code Examples for uLisp

;;; How to Hack with VS Code

;;
;; Remember to install the ulisp.code-profile, see the instructions 
;; at http://lisp.nyc/ulisp
;; 
;; The code profile installs two extenstions:
;;   * Common Lisp by Quinpeng Li
;;   * Serial Monitor by Microsoft
;;  
;; CTRL-L historically will select a line in VS Code, it is used to select 
;; the current form.
;; 
;; CTRL-E is used to evaluate what is seleted by CTRL-L, it is up to you
;; to ensure it is correctly formatted.
;; 
;; Select by placing the cursor anywhere in the parens and hitting Ctrl-L
;; 
;;   (+ 1 2 3)  
;;   ^   ^   ^   <-- in any of these spots
;;


(+ 1 2 3)   ; try it yourself here!


;;
;; Putting the cursor withn the 'list' and CTRL-L will only selet the list
;; and not the 'defvar'.  
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
;; First check your connection settings and ensure you have the
;; following selected:
;;   * port          /dev/tty/AACM0       (this may vary)
;;   * baud          9600
;;   * Line ending   CRLF
;;
;; However you may end up evaluating an expression with unmatched or 
;; mismatched parens.  Like this:
;; 
;;   (+ 1 2 3
;; 
;; Fix it by sending closing parens and then you'll have the REPL
;; prompt again:
;; 
;;   )))
;; 
;; it looks like this:
;; 
;;     1676> 
;;     ---- Sent utf8 encoded message: "(+ 1 2 3" ----
;;     (+ 1 2 3
;;     ---- Sent utf8 encoded message: "(+ 4 5 6)" ----
;;     (+ 4 5 6)
;;     ---- Sent utf8 encoded message: ")))" ----
;;     )
;;     21
;;     
;;     1676> 
;;     ---- Sent utf8 encoded message: "(+ 1 2 3)" ----
;;     (+ 1 2 3)
;;     6
;;     
;;     1676> 

;;
;; This is not a very sophicated evaluation procedure, it sends all code in a
;; single line and it WILL get confused by an in-line comment because the 
;; parens will never close.  You may have to reset the device, beware.
;; 
;; (defvar my-colors (list 
;;                    (to-color 64 0 0) ; never try to 
;;                    (to-color 32 0 0) ; evaluate a form
;;                    0                 ; that has included
;;                    0))               ; comments

(format t "Happy hacking ~a!~%" 'friend)