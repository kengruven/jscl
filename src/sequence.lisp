;;; sequence.lisp

;; JSCL is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; JSCL is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

(defun not-seq-error (thing)
  (error "`~S' is not of type SEQUENCE" thing))

(defmacro do-sequence ((elt seq &optional (index (gensym "i") index-p)) &body body)
  (let ((nseq (gensym "seq")))
    (unless (symbolp elt)
      (error "`~S' must be a symbol." elt))
    `(let ((,nseq ,seq))
       (if (listp ,nseq)
           ,(if index-p
                `(let ((,index -1))
                   (dolist (,elt ,nseq)
                     (incf ,index)
                     ,@body))
                `(dolist (,elt ,nseq)
                   ,@body))
           (dotimes (,index (length ,nseq))
             (let ((,elt (aref ,nseq ,index)))
               ,@body))))))

(defmacro do-sequences ((elts sequences &optional (index (gensym "i") index-p)) &body body)
  (let ((loop (gensym))
        (nsequences (gensym)))
    (unless (symbolp elts)
      (error "`~S' must be a symbol." elts))

    `(let* ((,nsequences ,sequences)
            (,index 0)
            (walkers (apply #'vector ,nsequences))
            (shortest-vector (let ((lengths (mapcar #'length (remove-if-not #'vectorp ,nsequences))))
                               (if lengths (apply #'min lengths) nil))))
       ;; check types
       (dolist (seq sequences)
         (unless (or (vectorp seq) (listp seq))
           (not-seq-error seq)))
       (block ,loop
               ;; build a list of elements, one from each sequence (vector or list), and call body
              (loop
               (let ((,elts (with-collect
                             (dotimes (i (length walkers))
                               (let ((walker (aref walkers i)))
                                 (collect (if (vectorp walker)
                                              (aref walker ,index)
                                            (car walker))))))))
                 ,@body)

               ;; for each list, take the CDR, and stop if any is NIL now
               (dotimes (i (length walkers))
                 (let ((walker (aref walkers i)))
                   (when (listp walker)
                     (when (null (cdr walker))
                       (return-from ,loop))
                     (aset walkers i (cdr walker)))))

               ;; for vectors, keep an index, and stop if it's beyond the shortest one
               (incf ,index)
               (when (and shortest-vector (>= ,index shortest-vector))
                 (return-from ,loop)))))))

(defun find (item seq &key key (test #'eql testp) (test-not #'eql test-not-p))
  (do-sequence (x seq)
    (when (satisfies-test-p item x :key key :test test :testp testp
                            :test-not test-not :test-not-p test-not-p)
      (return x))))

(defun find-if (predicate sequence &key key)
  (if key
      (do-sequence (x sequence)
        (when (funcall predicate (funcall key x))
          (return x)))
      (do-sequence (x sequence)
        (when (funcall predicate x)
          (return x)))))

(defun position (elt sequence
                 &key key (test #'eql testp)
                   (test-not #'eql test-not-p)
                   (start 0) end)
  ;; TODO: Implement START and END efficiently for all the sequence
  ;; functions.
  (let ((end (or end (length sequence))))
    (do-sequence (x sequence index)
      (when (and (<= start index)
                 (< index end)
                 (satisfies-test-p elt x
                                   :key key :test test :testp testp
                                   :test-not test-not :test-not-p test-not-p))
        (return index)))))

;; TODO: need to support &key from-end
(defun position-if (predicate sequence
                 &key key (start 0) end)
  ;; TODO: Implement START and END efficiently for all the sequence
  ;; functions.
  (let ((end (or end (length sequence))))
    (do-sequence (x sequence index)
      (when (and (<= start index)
                 (< index end)
                 (funcall predicate (if key (funcall key x) x)))
        (return index)))))

(defun position-if-not (predicate sequence
                 &key key (start 0) end)
  (position-if (complement predicate) sequence :key key :start start :end end))

(defun remove (x seq &key key (test #'eql testp) (test-not #'eql test-not-p))
  (cond
    ((null seq)
     nil)
    ((listp seq)
     (let* ((head (cons nil nil))
            (tail head))
       (do-sequence (elt seq)
         (unless (satisfies-test-p x elt :key key :test test :testp testp
                                   :test-not test-not :test-not-p test-not-p)
           (let ((new (list elt)))
             (rplacd tail new)
             (setq tail new))))
       (cdr head)))
    (t
     (let (vector)
       (do-sequence (elt seq index)
         (if (satisfies-test-p x elt :key key :test test :testp testp
                               :test-not test-not :test-not-p test-not-p)
             ;; Copy the beginning of the vector only when we find an element
             ;; that does not match.
             (unless vector
               (setq vector (make-array 0))
               (dotimes (i index)
                 (vector-push-extend (aref seq i) vector)))
             (when vector
               (vector-push-extend elt vector))))
       (or vector seq)))))


(defun every (predicate &rest sequences)
  (do-sequences (elts sequences)
    (unless (apply predicate elts)
      (return-from every nil)))
  t)

(defun some (function &rest sequences)
  (do-sequences (elts sequences)
    (when (apply function elts)
      (return-from some t)))
  nil)

(defun remove-if (func seq)
  (cond
    ((listp  seq) (list-remove-if   func seq nil))
    ((arrayp seq) (vector-remove-if func seq nil))
    (t (not-seq-error seq))))

(defun remove-if-not (func seq)
  (cond
    ((listp  seq) (list-remove-if   func seq t))
    ((arrayp seq) (vector-remove-if func seq t))
    (t (not-seq-error seq))))

(defun list-remove-if (func list negate)
  (if (endp list)
    ()
    (let ((test (funcall func (car list))))
      (if (if negate (not test) test)
        (list-remove-if func (cdr list) negate)
        (cons (car list) (list-remove-if func (cdr list) negate))))))

(defun vector-remove-if (func vector negate)
  (let ((out-vector (make-array 0)))
    (do-sequence (element vector i)
      (let ((test (funcall func element)))
        (when (if negate test (not test))
          (vector-push-extend element out-vector))))
    out-vector))

(defun subseq (seq a &optional b)
  (cond
    ((listp seq)
     (if b
       (let ((diff (- b a)))
         (cond
           ((zerop  diff) ())
           ((minusp diff)
            (error "Start index must be smaller than end index"))
           (t
            (let* ((drop-a (copy-list (nthcdr a seq)))
                   (pointer drop-a))
              (dotimes (_ (1- diff))
                (setq pointer (cdr pointer))
                (when (null pointer)
                  (error "Ending index larger than length of list")))
              (rplacd pointer ())
              drop-a))))
       (copy-list (nthcdr a seq))))
    ((vectorp seq)
     (let* ((b (or b (length seq)))
            (size (- b a))
            (new (make-array size :element-type (array-element-type seq))))
       (do ((i 0 (1+ i))
            (j a (1+ j)))
           ((= j b) new)
         (aset new i (aref seq j)))))
    (t (not-seq-error seq))))

(defun copy-seq (sequence)
  (subseq sequence 0))
