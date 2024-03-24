
;;
;; This is copied into LispLibrary.h but without comments.  Comments
;; confuse the loader.
;;
;; Use utils/convert-lisp-lib-to-include.sh to do the conversion.
;;
(format t "  Lisp Library options:~%")

;;
;; Optionaly define functions based on features selected.  Looks for a
;; bound AND true variable.
;;
;; Unless explicitly enabled, forceably remove them in case they are captured
;; by save-image.
;;
(if (not (and (boundp 'feature-event-loop)
              feature-event-loop))
    (progn
      (format t "    disabled feature-event-loop~%")
      (mapc makunbound '(wrap-fn-in-time list-to-array run-event-loop)))

    (progn
      (format t "    enabled  feature-event-loop~%")

      ;;
      ;; Copied from the Managed State Event Loop
      ;;  https://github.com/functional-sc/ulisp-managed-state-event-loop
      ;;
      (defun wrap-fn-in-time (number-of-millis fn)
        (let ((next-time 0)) 
          (lambda (current-time state)
            (if (> current-time next-time) 
                (progn
                  (setq next-time (+ number-of-millis current-time)) 
                  (funcall fn state) )
                state))))

      (defun list-to-array (l)
        (let ((a (make-array (length l))))
          (dotimes (i (length l))
            (setf (aref a i) (nth i l)))
          a))

      (defun run-event-loop (time-fn-state-list)
        (let* ((len                (length time-fn-state-list))
               (fn-array           (list-to-array (mapcar (lambda (fn-t) (wrap-fn-in-time (first fn-t) (second fn-t))) time-fn-state-list)))
               (state-offset-array (make-array len)) 
               (state-array        (make-array len)) 
               (name-offset-alist  nil) ) 
    
          (dotimes (i len)
            (setf (aref state-array i) (nth 2 (nth i time-fn-state-list)))
            (let ((this-name (nth 3 (nth i time-fn-state-list))))
              (setf name-offset-alist   (cons (cons this-name i) name-offset-alist))
              (setf (aref state-offset-array i) (if (car (assoc this-name (reverse name-offset-alist)))
                                                    (cdr (assoc this-name (reverse name-offset-alist)))
                                                    i)) ))

          (loop 
             (dotimes (i len) 
               (setf (aref state-array (aref state-offset-array i))
                     (funcall (aref fn-array i) (millis) (aref state-array
                                                               (aref state-offset-array i) ))) ))))
      )) 

;;
;; General purpose functions to make your life easier using 'feature-extra'
;;
(if (not (and (boundp 'feature-extras)
              (not (null feature-extras))))
    (progn
      (format t "    disabled feature-extras~%")
      (mapc makunbound '(to-color to-rgb pixels)) )

    (progn
      (format t "    enabled  feature-extras~%") 
                                                    
      (defun to-color (red green blue)
        (logxor (* #x010000 red)
                (* #x000100 green)
                (* #x000001 blue) ))

      (defun to-rgb (color)
        (list (ash (logand #xFF0000 color)  -16)   
              (ash (logand #x00FF00 color)  -8)  
              (ash (logand #x0000FF color)   0)))

      ;;
      ;; easily turn pixels on with minimal effort, support many options
      ;;
      ;; 0 args - turn them off
      ;;   (pixels)
      ;; 1 arg numeric - make them all the same color
      ;;   (pixels (to-color 64 0 32))
      ;; 1 arg list - set them to each color in the list
      ;;   (pixels (list (to-color 64 0 0) (to-color 32 0 0) 0 0))
      ;; 2 args - even/odd set the pixels to that color
      ;; 3 args - treat the args as rgb and set all the lights to that color
      ;;   (pixels 4 0 0)
      ;; 4 args - set each pixel to it's respective color
      ;;  (pixels 128 64 32 16)
      ;;
      (defun pixels (&optional p0 p1 p2 p3)
        (cond ((null p0) (pixels-clear))
              ((null p1) (if (listp p0) (dotimes (n (length p0)) (pixels-set-pixel-color n (nth n p0)))
                             (pixels p0 p0 p0 p0) ))
              ((null p2) (pixels (list p0 p1 p0 p1)))
              ((null p3) (pixels (to-color p0 p1 p2))) 
              (t         (progn (pixels-set-pixel-color 0 p0)
                                (pixels-set-pixel-color 1 p1)
                                (pixels-set-pixel-color 2 p2)
                                (pixels-set-pixel-color 3 p3) )))
        (pixels-show))

      ;;
      ;; since we just defined pixels, get them started
      ;;
      (pixels-begin)
      
      ))

;;
;; The demo are not general-purpose functions and are scoped using CL's
;; package syntax:
;;
;;   demo:foo
;;
(if (not (and (boundp 'feature-demo)
              (not (null feature-demo))))
    (progn
      (format t "    disabled feature-demo~%")
      (mapc makunbound '(demo:lambda-art demo:rotate demo:rotate-random demo:type-ascii-art demo:make-random-color demo:disp-colors demo:flash-on-touch demo:type-contact demo:run-events)) )

    (progn
      (format t "    enabled  feature-demo~%")
      
      (defvar demo:lambda-art '(""
                                "  ##########"
                                "  #.........#" 
                                "  ####......#"
                                "      #......#"
                                "      #......#"
                                "     #........#"
                                "    #..........#"
                                "   #....##......#"
                                "  #....#  #......#"
                                " #....#    # .....###"
                                "#....#      #.......#"
                                "#####       #########" "" "" ""))

      ;; rotate a list
      (defun demo:rotate (l) (append (cdr l) (cons (car l) ())))

      ;; rotate a list a random number of time, this is good for random colors
      (defun demo:rotate-random (l)
        (if (zerop (random (length l))) l
            (demo:rotate-random (demo:rotate l))))

      ;;
      ;; Output one line of the passed in list-of-lines and rotate the list
      ;; for the next managed state event loop iteration.
      ;;
      (defvar demo:type-ascii-art
        (lambda (list-of-lines)
          (format t "~a~%" (car list-of-lines))
          (demo:rotate list-of-lines)))

      ;;
      ;; If colors are too close togther they result in washed-out white. We
      ;; make an attempt to select for bold colors by forcing wide differences.
      ;;
      ;; The returned color is just random, future releases may slowly
      ;; transition between colors but for now the incoming color is ignored.
      ;;
      ;; This is called through the managed state event loop.
      ;;
      (defvar demo:make-random-color
        (lambda (x)
          (let ((l (demo:rotate-random (list (+ 16 (random 32))
                                             (+ 4 (random 8))
                                             (random 4)))))
            (to-color (nth 0 l) (nth 1 l) (nth 2 l)))))

      ;;
      ;; Display the colors and rotate the list for the next managed state
      ;; event loop iteration.
      ;;
      (defvar demo:disp-colors
        (lambda (lst)
          (pixels (nth 0 lst) (nth 1 lst) (nth 2 lst) (nth 3 lst))
          (demo:rotate lst) ))

      ;;
      ;; Individually light up pixels based on wich button is pressed using
      ;; lower-level functions and not (pixels) so as not to overwrite it.
      ;;
      ;; Using the Managed State Event Loop and passing back out the color
      ;; which is modified in another function.
      ;;
      (defvar demo:flash-on-touch
        (lambda (color)
          (when (< 500 (touchpads 2))
            (pixels-set-pixel-color 0 color)
            (pixels-set-pixel-color 1 color))
          (when (< 500 (touchpads 1))
            (pixels-set-pixel-color 2 color)
            (pixels-set-pixel-color 3 color))
          (pixels-show)
          color))

      ;;
      ;; Called through the managed state event loop, but no state is required
      ;; for this, just timing.
      ;;
      (defvar demo:type-contact
        (lambda (x) (format t "~%Enter ~~ to use LISP      www.lisp.nyc/ulisp~%~%")))

      ;;
      ;; Top-level function of the demo that statrs the managed state event
      ;; loop.  If no color list is passed call self with default colors.
      ;;
      (defun demo:run-events (&optional colors)
         (if (not colors)
             (demo:run-events (list (to-color 4 0 0) 
                                    (to-color 0 4 0) 
                                    (to-color 0 0 4) 
                                    (to-color 2 0 1))))
         (pixels-begin)
         (run-event-loop
          (list
           (list  500    demo:disp-colors        colors                      nil)
           (list  500    demo:type-ascii-art     demo:lambda-art             nil)
           (list 8000    demo:type-contact       nil                         nil)
           (list 2000    demo:make-random-color  nil                         'rnd-color)
           (list   10    demo:flash-on-touch     (demo:make-random-color t)  'rnd-color) ) ))
      
      ))

;;
;; do not run the demo if extra functions are defined, assuming if someone
;; created extra functions they would have run from save-image
;;
(if (and (boundp 'feature-demo)
         feature-demo)
    (if (= 18 (length (globals)))
        (demo:run-events)
        (format t "  To run demo evaluate (demo:run-events)~%")))

;;
;; If feature-demo is unbound, define it as true and the demo will run
;; next time the device is booted.
;;
;; It takes about 6 seconds to get to here after the device is burned in
;; so shine a single blue pixel to let operator know the device is completely
;; programmed.
;;
;; On the NeoTrinkey, the defvars only stick if save-image is given a function
;; I don't know why.
;;
(if (not (boundp 'feature-demo))
    (progn
      (defvar feature-demo t)
      (defvar feature-event-loop t)
      (defvar feature-extras t)
      (pixels-begin)
      (pixels-set-pixel-color 0 64)
      (pixels-show)
      (format t "~%Enabled all options, demo on next boot.~%")
      (delay 250)
      (pixels-set-pixel-color 0 0)
      (pixels-show)
      (save-image (lambda() 'foo)) ))

