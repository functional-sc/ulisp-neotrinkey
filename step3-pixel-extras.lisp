
(defun delay4 (n)
  (list
   ($mov 'r5 n)
   ($sub 'r5 1)
   ($bne (- *pc* 2))))

(defun *pc* 1000)

(defvar dirset #x08)
(defvar outset #x18)
(defvar outclr #x14)

(defcode neopixel (a b c d)
  ($push '(lr r5 r4 r3 r2 r1 r0))
  ($ldr 'r4 porta)
  ($mov 'r1 1)
  ($lsl 'r3 'r1 5)
  ($str 'r3 '(r4 dirset)) ; make pin an output
  ($mov 'r2 4)
  nextled
  ($pop '(r0)) ; get bytes
  ($mov 'r1 1)
  ($lsl 'r1 23)
  nextbit
  ($tst 'r0 'r1) ; test if bit is 1
  ($bne one)
  zero
  ($cpsid 3)
  ($str 'r3 '(r4 outset))
  (delay4 4)             
  ($str 'r3 '(r4 outclr))
  (delay4 10)
  ($cpsie 3)
  ($b next)
  one
  ($str 'r3 '(r4 outset))
  (delay4 8)
  ($str 'r3 '(r4 outclr))
  (delay4 7)
  next
  ($lsr 'r1 1)
  ($bne nextbit)
  ($sub 'r2 1)
  ($bne nextled)
  ($pop '(r4 r5 pc))
  porta
  ($word #x41004400))

(defun make-color (red green blue)
  (logxor (* #x010000 green)
          (* #x000100 red)
          (* #x000001 blue) ))

(defun color (r g b) (make-color r g b))

(defvar user "there")

(defvar animate-colors 
  (list (make-color 4 0 0) ; red
  	(make-color 0 4 0) ; green
        (make-color 0 0 4) ; blue
        (make-color 2 0 1))) 

(defun animate ()
  (let ((lst animate-colors)) ; purple
    (loop
     (eval (cons 'neopixel lst))
     (setq lst (append (cdr lst) (list (car lst))))
     (delay 1000)
     (format t "Hello ~a!  Enter ~~ to use LISP    ~a~%" user (if (zerop (mod (millis) 7)) "http://functional.sc/ulisp" "")) )))

(defun disp (&optional p0 p1 p2 p3)
  (cond ((null p0) (disp 0 0 0 0)) ;; nothing? turn off
        ((null p1) (disp p0 p0 p0 p0)) ;; set all to first
        ((null p3) (disp (make-color p0 p1 p2))) ;; set all to RGBs
        (t         (neopixel p0 p1 p2 p3)) )) ;; individualy mapped





(defun disp-off () (disp))

;; unload the assembler, about 2K of memory
(mapc makunbound '(regno emit error offset $word lsl-lsr-0 add-sub-1 mov-sub-2-3 add-mov-4 reg-reg bx-blx str-ldr str-ldr-5 add-10 add-sub-11 push-pop b-cond-13 cpside $adc $add $and $asr $b $bcc $bcs $beq $bge $bgt $bhi $bhs $ble $blo $blt $bmi $bne $bpl $bic $bl $blx $bx $cmn $cmp $cpsid $cpsie $eor $ldr $ldrb $ldrh $ldrsb $ldrsh $lsl $lsr $mov $mul $mvn $neg $nop $orr $push $pop $rev $rev16 $revsh $ror $sbc $str $strb $sub $sxtb $sxth $tst $uxtb $uxth))

(save-image 'animate)

;; note: often when pasting, the last "save image" does not come through ok, you may have to type it in my hand


