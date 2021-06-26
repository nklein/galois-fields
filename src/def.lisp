;;;; src/def.lisp

(in-package #:galois-fields)

(defun poly-to-list (a p d)
  (if (zerop a)
      (list 0)
      (loop :repeat d
         :for x := a :then (values (floor x p))
         :until (zerop x)
         :collecting (mod x p))))

(defun list-to-poly (l p)
  (labels ((incorporate (l)
             (cond
               ((null l) 0)
               (t (destructuring-bind (x &rest l) l
                    (+ (* (incorporate l) p) x))))))
    (incorporate l)))

(defun poly* (a b p)
  (loop :with l := (+ (length a) (length b) -1)
     :for k :from 0 :below l
     :collecting (mod (loop :for j :from 0 :upto k
                         :for i :from (- k j) :downto 0
                         :for aa := (or (nth j a) 0)
                         :for bb := (or (nth i b) 0)
                         :summing (* aa bb))
                      p)))

(defun polymod (a p m)
  (let ((a (reverse a))
        (m (reverse m))
        (lm (length m)))
    (labels ((modp (a)
               (mod a p))
             (polymod (a)
               (cond
                 ((< (length a) lm)
                  (mapcar #'modp (reverse a)))
                 (t (destructuring-bind (an &rest a) a
                      (let ((an*m (mapcar (lambda (x) (modp (* an x))) m)))
                        (polymod (append (loop :for x :in a
                                            :for y :in (rest an*m)
                                            :collecting (modp (- x y)))
                                         (nthcdr (1- lm) a)))))))))
      (polymod a))))

(defun poly (a x p)
  (mod (loop :for aa :in a
          :for i :from 0
          :summing (* aa (expt x i)))
       p))

(defun irreduciblep (a p d)
  (loop :for b :from p :below (expt p (1+ (ceiling d 2)))
     :when (zerop (list-to-poly (polymod a p (poly-to-list b p d))
                                p))
     :do (return-from irreduciblep nil))
  t)

(defun find-irreducible-polynomial (p d)
  (loop :with base := (expt p d)
     :for a :from (1+ base) :below (+ base base)
     :when (irreduciblep (poly-to-list a p (1+ d)) p d)
     :do (return-from find-irreducible-polynomial a)))

(defun galois+ (a b p d)
  (labels ((g+ (a b d)
             (flet ((2+ (a b)
                      (mod (+ a b) p)))
               (let ((a (poly-to-list a p d))
                     (b (poly-to-list b p d)))
                 (list-to-poly (append (mapcar #'2+ a b)
                                       (nthcdr (length a) b)
                                       (nthcdr (length b) a))
                               p)))))
    (g+ a b d)))

(defun galois* (a b p d m)
  (labels ((g* (a b d)
             (cond
               ((= d 1) (mod (* a b) p))
               (t (list-to-poly (polymod (poly* (poly-to-list a p d)
                                                (poly-to-list b p d)
                                                p)
                                         p
                                         m)
                                p)))))
    (g* a b d)))

(defun define-plus-table (p d)
  (let* ((q (expt p d))
         (tbl (make-array (list q q) :element-type `(integer 0 ,(1- q)) :initial-element 0)))
    (loop :for a :below q
       :do (loop :for b :upto a
              :do (let ((v (galois+ a b p d)))
                    (setf (aref tbl a b) v
                          (aref tbl b a) v))))
    tbl))

(defun define-times-table (p d m)
  (unless (or (= d 1) m)
    (error "Unable to find irreducible polynomial~%"))
  (let* ((q (expt p d))
         (tbl (make-array (list q q) :element-type `(integer 0 ,(1- q)) :initial-element 0)))
    (loop :for a :below q
       :do (loop :for b :upto a
              :do (let ((v (galois* a b p d m)))
                    (setf (aref tbl a b) v
                          (aref tbl b a) v))))
    tbl))

(defun primep (p)
  (declare (type (integer 2) p))
  (loop :for f :from 2
     :while (<= (* f f) p)
     :when (zerop (mod p f))
     :do (return-from primep nil))
  t)

(defstruct (galois-field (:constructor %make-galois-field (p d m +-function *-function))
                         (:conc-name :gf))
  p d m +-function *-function)

(defun deffield* (&key p (d 1) (use-tables nil use-tables-p))
  (declare (type (integer 2) p)
           (type (integer 1) d)
           (type boolean use-tables))
  (let* ((q (expt p d))
         (ut (if use-tables-p
                 use-tables
                 (<= 512 q)))
          (m (progn
                (assert (primep p))
                (and (< 1 d)
                     (poly-to-list (find-irreducible-polynomial p d) p (1+ d)))))
          (tbl+ (and ut (define-plus-table p d)))
          (tbl* (and ut (define-times-table p d m)))
          (fn+ (if tbl+
                   (lambda (a b) (aref tbl+ a b))
                   (lambda (a b) (galois+ a b p d))))
          (fn* (if tbl*
                   (lambda (a b) (aref tbl* a b))
                   (lambda (a b) (galois* a b p d m)))))
     (flet ((plus-fn (&rest vs)
              (assert (every (lambda (v) (typep v `(integer 0 ,(1- q)))) vs))
              (cond
                ((null vs) 0)
                (t (reduce fn+
                           (rest vs)
                           :initial-value (first vs)))))
            (times-fn (&rest vs)
              (assert (every (lambda (v) (typep v `(integer 0 ,(1- q)))) vs))
              (cond
                ((null vs) 1)
                (t (reduce fn*
                           (rest vs)
                           :initial-value (first vs))))))
       (%make-galois-field p d m #'plus-fn #'times-fn))))


(defmacro deffield (name &rest args &key p (d 1) use-tables)
  (declare (type symbol name)
           (type (integer 2) p)
           (type (integer 1) d)
           (type boolean use-tables))
  (declare (ignore p d use-tables))
  (let* ((base (string name))
         (plus-fn (intern (concatenate 'string base "+")
                          (symbol-package name)))
         (times-fn (intern (concatenate 'string base "*")
                           (symbol-package name)))
         (ff (gensym "ff")))
    `(let ((,ff (deffield* ,@args)))
       (setf (symbol-function ',plus-fn) (gf+-function ,ff)
             (symbol-function ',times-fn) (gf*-function ,ff))
       ,ff)))
