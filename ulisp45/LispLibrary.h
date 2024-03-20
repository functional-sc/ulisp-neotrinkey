/*
 * generated from LispLibrary.lisp using convert-lisp-lib-to-include.sh
 */

const char LispLibrary[] PROGMEM = R"lisplibrary(





(format t "  Lisp Library options:~%")

(if (not (and (boundp 'feature-event-loop)
              feature-event-loop))
    (progn
      (format t "    disabled feature-event-loop~%") )

    (progn
      (format t "    enabled  feature-event-loop~%")

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

(if (not (and (boundp 'feature-extras)
              (not (null feature-extras))))
    (progn
      (format t "    disabled feature-extras~%") )

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

      (defun pixels (&optional p0 p1 p2 p3)
        (cond ((null p0) (pixels-clear)) ; 0 args - turn off
              ((null p1) ; 1 args - set all to first
               (if (listp p0) ; list of colors or single color?
                   (dotimes (n (length p0)) (pixels-set-pixel-color n (nth n p0)))
                   (pixels p0 p0 p0 p0) )) 
              (t         (progn (pixels-set-pixel-color 0 p0)
                                (pixels-set-pixel-color 1 p1)
                                (pixels-set-pixel-color 2 p2)
                                (pixels-set-pixel-color 3 p3) )))
        (pixels-show))
      ))

(if (not (and (boundp 'feature-demo)
              (not (null feature-demo))))
    (progn
      (format t "    disabled feature-demo~%") )

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

      (defun demo:rotate (l) (append (cdr l) (cons (car l) ())))

      (defun demo:rotate-random (l)
        (if (zerop (random (length l))) l
            (demo:rotate-random (demo:rotate l))))

      (defvar demo:type-ascii-art
        (lambda (list-of-lines)
          (format t "~a~%" (car list-of-lines))
          (demo:rotate list-of-lines)))

      (defvar demo:make-random-color
        (lambda (x)
          (let ((l (demo:rotate-random (list (+ 16 (random 32))
                                             (+ 4 (random 8))
                                             (random 4)))))
            (to-color (nth 0 l) (nth 1 l) (nth 2 l)))))

      (defvar demo:disp-colors
        (lambda (lst)
          (pixels (nth 0 lst) (nth 1 lst) (nth 2 lst) (nth 3 lst))
          (demo:rotate lst) ))

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

      (defvar demo:type-contact
        (lambda (x) (format t "~%Enter ~~ to use LISP      www.lisp.nyc/ulisp~%~%")))

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

(if (and (boundp 'feature-demo)
         feature-demo)
    (if (= 18 (length (globals)))
        (demo:run-events)
        (format t "  To run demo evaluate (demo:run-events)~%")))

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





)lisplibrary";
