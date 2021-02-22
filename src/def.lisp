;;;; src/def.lisp

(in-package #:galois-fields)

(defun galois+ (a b p d)
  (labels ((g+ (a b d)
             (cond
               ((= d 1) (mod (+ a b) p))
               (t (error "not implemented yet")))))
    (g+ a b d)))

(defun galois* (a b p d)
  (labels ((g* (a b d)
             (cond
               ((= d 1) (mod (* a b) p))
               (t (error "not implemented yet")))))
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

(defun define-times-table (p d)
  (let* ((q (expt p d))
         (tbl (make-array (list q q) :element-type `(integer 0 ,(1- q)) :initial-element 0)))
    (loop :for a :below q
       :do (loop :for b :upto a
              :do (let ((v (galois* a b p d)))
                    (setf (aref tbl a b) v
                          (aref tbl b a) v))))
    tbl))

(defmacro deffield (name &key p (d 1))
  (declare (type symbol name)
           (type (integer 2) p)
           (type (integer 1) d))
  (let* ((base (string name))
         (plus-fn (intern (concatenate 'string base "+")
                          (symbol-package name)))
         (times-fn (intern (concatenate 'string base "*")
                           (symbol-package name)))
         (q (gensym "Q-"))
         (tbl+ (gensym "+"))
         (tbl* (gensym "*")))
    `(let* ((,q (expt ,p ,d))
            (,tbl+ (define-plus-table ,p ,d))
            (,tbl* (define-times-table ,p ,d)))
       (defun ,plus-fn (&rest vs)
         (assert (every (lambda (v) (typep v `(integer 0 ,(1- ,q)))) vs))
         (cond
           ((null vs) 0)
           (t (reduce (lambda (a b)
                        (aref ,tbl+ a b))
                      (rest vs)
                      :initial-value (first vs)))))
       (defun ,times-fn (&rest vs)
         (assert (every (lambda (v) (typep v `(integer 0 ,(1- ,q)))) vs))
         (cond
           ((null vs) 1)
           (t (reduce (lambda (a b)
                        (aref ,tbl* a b))
                      (rest vs)
                      :initial-value (first vs))))))))
