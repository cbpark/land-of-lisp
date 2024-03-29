(in-package :cl-user)
(defpackage land-of-lisp.chap10
  (:use :cl))
(in-package :land-of-lisp.chap10)

(defparameter *width* 100)

(defparameter *height* 30)

(defparameter *jungle* '(45 10 10 10))

(defparameter *plant-energy* 80)

;;; Growing plants in our world

(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;;; Creating animals

(defstruct animal
  x ; x- and
  y ; y-coordinates
  energy ; how many days of energy remaining
  dir ; direction of moving (from 0 to 7 prop to the gene)
  genes)

(defparameter *animals*
  (list (make-animal :x      (ash *width*  -1)
                     :y      (ash *height* -1)
                     :energy 1000
                     :dir    0
                     :genes  (loop repeat 8 collecting (1+ (random 10))))))

;; Handling animal motion
(defun move (animal)
  (let ((dir (animal-dir animal))
        (x (animal-x animal))
        (y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
                                    (cond ((and (>= dir 2) (< dir 5)) 1)
                                          ((or (= dir 1) (= dir 5)) 0)
                                          (t -1))
                                    *width*)
                                 *width*))
    (setf (animal-y animal) (mod (+ y
                                    (cond ((and (>= dir 0) (< dir 3)) -1)
                                          ((and (>= dir 4) (< dir 7)) 1)
                                          (t 0))
                                    *height*)
                                 *height*))
    (decf (animal-energy animal))))

;; Handling animal turning
(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((xnu (- x (car genes))))
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
                 8)))))

;; Handling animal eating
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

;; Handling animal reproduction
(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu (copy-structure animal))
            (genes     (copy-list (animal-genes animal)))
            (mutation  (random 8)))
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))

;;; Simulating a day in our world

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

;;; Drawing our world

(defun draw-world ()
  (loop for y below *height*
     do (progn
          (fresh-line)
          (princ "|")
          (loop for x below *width*
             do (princ (cond ((some (lambda (animal)
                                      (and (= (animal-x animal) x)
                                           (= (animal-y animal) y)))
                                    *animals*)
                              #\M)
                             ((gethash (cons x y) *plants*) #\*)
                             (t #\space))))
          (princ "|"))))

;;; Creating a user interface

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x (parse-integer str :junk-allowed t)))
               (if x
                   (loop for i below x
                        do (update-world)
                        if (zerop (mod i 1000))
                        do (princ #\.))
                   (update-world))
               (evolution))))))
