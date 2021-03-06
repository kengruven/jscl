;;; boot.lisp --- First forms to be cross compiled

;; Copyright (C) 2012, 2013 David Vazquez
;; Copyright (C) 2012 Raimon Grau

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

;;; This code is executed when JSCL compiles this file itself. The
;;; compiler provides compilation of some special forms, as well as
;;; funcalls and macroexpansion, but no functions. So, we define the
;;; Lisp world from scratch. This code has to define enough language
;;; to the compiler to be able to run.

(eval-when-compile
  (let ((defmacro-macroexpander
         '#'(lambda (form)
              (destructuring-bind (name args &body body)
                  form
                (let ((whole (gensym)))
                  `(eval-when-compile
                     (%compile-defmacro ',name
                                        '#'(lambda (,whole)
                                             (destructuring-bind ,args ,whole
                                               ,@body)))))))))
    (%compile-defmacro 'defmacro defmacro-macroexpander)))

(defmacro declaim (&rest decls)
  `(eval-when-compile
     ,@(mapcar (lambda (decl) `(!proclaim ',decl)) decls)))

(defmacro defconstant (name value &optional docstring)
  `(progn
     (declaim (special ,name))
     (declaim (constant ,name))
     (setq ,name ,value)
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defconstant t 't)
(defconstant nil 'nil)
(%js-vset "nil" nil)

(defmacro lambda (args &body body)
  `(function (lambda ,args ,@body)))

(defmacro when (condition &body body)
  `(if ,condition (progn ,@body) nil))

(defmacro unless (condition &body body)
  `(if ,condition nil (progn ,@body)))

(defmacro defvar (name &optional (value nil value-p) docstring)
  `(progn
     (declaim (special ,name))
     ,@(when value-p `((unless (boundp ',name) (setq ,name ,value))))
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defmacro defparameter (name value &optional docstring)
  `(progn
     (setq ,name ,value)
     ,@(when (stringp docstring) `((oset ,docstring ',name "vardoc")))
     ',name))

(defmacro defun (name args &rest body)
  `(progn
     (fset ',name #'(named-lambda ,name ,args ,@body))
     ',name))

(defmacro return (&optional value)
  `(return-from nil ,value))

(defmacro while (condition &body body)
  `(block nil (%while ,condition ,@body)))

(defvar *gensym-counter* 0)
(defun gensym (&optional (prefix "G"))
  (setq *gensym-counter* (+ *gensym-counter* 1))
  (make-symbol (concat prefix (integer-to-string *gensym-counter*))))

(defun boundp (x)
  (boundp x))

(defun fboundp (x)
  (fboundp x))

(defun eq (x y) (eq x y))
(defun eql (x y) (eq x y))

(defun not (x) (if x nil t))

;; Basic macros
(defmacro incf (place &optional (delta 1))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
                (,(car newval) (+ ,getter ,d))
                ,@(cdr newval))
         ,setter))))

(defmacro decf (place &optional (delta 1))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (- ,getter ,d))
              ,@(cdr newval))
         ,setter))))

(defmacro push (x place)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,x)
              ,@(mapcar #'list dummies vals)
              (,(car newval) (cons ,g ,getter))
              ,@(cdr newval))
         ,setter))))

(defmacro pushnew (x place &rest keys &key key test test-not)
  (declare (ignore key test test-not))
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place)
    (let ((g (gensym))
          (v (gensym)))
      `(let* ((,g ,x)
              ,@(mapcar #'list dummies vals)
              ,@(cdr newval)
              (,v ,getter))
         (if (member ,g ,v ,@keys)
             ,v
             (let ((,(car newval) (cons ,g ,getter)))
               ,setter))))))

(defmacro dolist ((var list &optional result) &body body)
  (let ((g!list (gensym)))
    (unless (symbolp var) (error "`~S' is not a symbol." var))
    `(block nil
       (let ((,g!list ,list)
             (,var nil))
         (%while ,g!list
                 (setq ,var (car ,g!list))
                 (tagbody ,@body)
                 (setq ,g!list (cdr ,g!list)))
         ,result))))

(defmacro dotimes ((var count &optional result) &body body)
  (let ((g!count (gensym)))
    (unless (symbolp var) (error "`~S' is not a symbol." var))
    `(block nil
       (let ((,var 0)
             (,g!count ,count))
         (%while (< ,var ,g!count)
                 (tagbody ,@body)
                 (incf ,var))
         ,result))))

(defmacro cond (&rest clausules)
  (unless (null clausules)
    (destructuring-bind (condition &body body)
        (first clausules)
      (cond
        ((eq condition t)
         `(progn ,@body))
        ((null body)
         (let ((test-symbol (gensym)))
           `(let ((,test-symbol ,condition))
              (if ,test-symbol
                  ,test-symbol
                  (cond ,@(rest clausules))))))
        (t
         `(if ,condition
              (progn ,@body)
              (cond ,@(rest clausules))))))))

(defmacro case (form &rest clausules)
  (let ((!form (gensym)))
    `(let ((,!form ,form))
       (cond
         ,@(mapcar (lambda (clausule)
                     (destructuring-bind (keys &body body)
                         clausule
                       (if (or (eq keys 't) (eq keys 'otherwise))
                           `(t nil ,@body)
                           (let ((keys (if (listp keys) keys (list keys))))
                             `((or ,@(mapcar (lambda (key) `(eql ,!form ',key)) keys))
                               nil ,@body)))))
                   clausules)))))

(defmacro ecase (form &rest clausules)
  (let ((g!form (gensym)))
    `(let ((,g!form ,form))
       (case ,g!form
         ,@(append
            clausules
            `((t
               (error "ECASE expression failed for the object `~S'." ,g!form))))))))

(defmacro and (&rest forms)
  (cond
    ((null forms)
     t)
    ((null (cdr forms))
     (car forms))
    (t
     `(if ,(car forms)
          (and ,@(cdr forms))
          nil))))

(defmacro or (&rest forms)
  (cond
    ((null forms)
     nil)
    ((null (cdr forms))
     (car forms))
    (t
     (let ((g (gensym)))
       `(let ((,g ,(car forms)))
          (if ,g ,g (or ,@(cdr forms))))))))

(defmacro prog1 (form &body body)
  (let ((value (gensym)))
    `(let ((,value ,form))
       ,@body
       ,value)))

(defmacro prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

(defmacro prog (inits &rest body )
  (multiple-value-bind (forms decls docstring) (parse-body body)
    `(block nil
       (let ,inits
         ,@decls
         (tagbody ,@forms)))))


;;; Go on growing the Lisp language in Ecmalisp, with more high level
;;; utilities as well as correct versions of other constructions.

(defun append-two (list1 list2)
  (if (null list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(defun append (&rest lists)
  (!reduce #'append-two lists nil))

(defun revappend (list1 list2)
  (while list1
    (push (car list1) list2)
    (setq list1 (cdr list1)))
  list2)

(defun reverse (list)
  (revappend list '()))

(defmacro psetq (&rest pairs)
  (let (;; For each pair, we store here a list of the form
        ;; (VARIABLE GENSYM VALUE).
        (assignments '()))
    (while t
      (cond
        ((null pairs) (return))
        ((null (cdr pairs))
         (error "Odd paris in PSETQ"))
        (t
         (let ((variable (car pairs))
               (value (cadr pairs)))
           (push `(,variable ,(gensym) ,value)  assignments)
           (setq pairs (cddr pairs))))))
    (setq assignments (reverse assignments))
    ;;
    `(let ,(mapcar #'cdr assignments)
       (setq ,@(!reduce #'append (mapcar #'butlast assignments) nil)))))

(defmacro do (varlist endlist &body body)
  `(block nil
     (let ,(mapcar (lambda (x) (if (symbolp x)
                                   (list x nil)
                                 (list (first x) (second x)))) varlist)
       (while t
         (when ,(car endlist)
           (return (progn ,@(cdr endlist))))
         (tagbody ,@body)
         (psetq
          ,@(apply #'append
                   (mapcar (lambda (v)
                             (and (listp v)
                                  (consp (cddr v))
                                  (list (first v) (third v))))
                           varlist)))))))

(defmacro do* (varlist endlist &body body)
  `(block nil
     (let* ,(mapcar (lambda (x1) (if (symbolp x1)
                                     (list x1 nil)
                                   (list (first x1) (second x1)))) varlist)
       (while t
         (when ,(car endlist)
           (return (progn ,@(cdr endlist))))
         (tagbody ,@body)
         (setq
          ,@(apply #'append
                   (mapcar (lambda (v)
                             (and (listp v)
                                  (consp (cddr v))
                                  (list (first v) (third v))))
                           varlist)))))))

(defun list-length (list)
  (let ((l 0))
    (while (not (null list))
      (incf l)
      (setq list (cdr list)))
    l))

(defun length (seq)
  (cond
    ((stringp seq)
     (string-length seq))
    ((arrayp seq)
     (oget seq "length"))
    ((listp seq)
     (list-length seq))))

(defmacro with-collect (&body body)
  (let ((head (gensym))
        (tail (gensym)))
    `(let* ((,head (cons 'sentinel nil))
            (,tail ,head))
       (flet ((collect (x)
                (rplacd ,tail (cons x nil))
                (setq ,tail (cdr ,tail))
                x))
         ,@body)
       (cdr ,head))))


(defmacro loop (&body body)
  `(while t ,@body))

(defun identity (x) x)

(defun complement (x)
  (lambda (&rest args)
    (not (apply x args))))

(defun constantly (x)
  (lambda (&rest args)
    x))

(defun code-char (x)
  (code-char x))

(defun char-code (x)
  (char-code x))

(defun char= (x y)
  (eql x y))

(defun char< (x y)
  (< (char-code x) (char-code y)))

(defun atom (x)
  (not (consp x)))

(defun alpha-char-p (x)
  (or (<= (char-code #\a) (char-code x) (char-code #\z))
      (<= (char-code #\A) (char-code x) (char-code #\Z))))

(defun digit-char-p (x)
  (if (and (<= (char-code #\0) (char-code x) (char-code #\9)))
      (- (char-code x) (char-code #\0))
      nil))

(defun digit-char (weight)
  (and (<= 0 weight 9)
       (char "0123456789" weight)))

(defun equal (x y)
  (cond
    ((eql x y) t)
    ((consp x)
     (and (consp y)
          (equal (car x) (car y))
          (equal (cdr x) (cdr y))))
    ((stringp x)
     (and (stringp y) (string= x y)))
    (t nil)))

(defun fdefinition (x)
  (cond
    ((functionp x)
     x)
    ((symbolp x)
     (symbol-function x))
    (t
     (error "Invalid function `~S'." x))))

(defun disassemble (function)
  (write-line (lambda-code (fdefinition function)))
  nil)

(defun documentation (x type)
  "Return the documentation of X. TYPE must be the symbol VARIABLE or FUNCTION."
  (ecase type
    (function
     (let ((func (fdefinition x)))
       (oget func "docstring")))
    (variable
     (unless (symbolp x)
       (error "The type of documentation `~S' is not a symbol." type))
     (oget x "vardoc"))))

(defmacro multiple-value-bind (variables value-from &body body)
  `(multiple-value-call (lambda (&optional ,@variables &rest ,(gensym))
                          ,@body)
     ,value-from))

(defmacro multiple-value-list (value-from)
  `(multiple-value-call #'list ,value-from))


;;; Generalized references (SETF)

(defvar *setf-expanders* nil)

(defun get-setf-expansion (place)
  (if (symbolp place)
      (let ((value (gensym)))
        (values nil
                nil
                `(,value)
                `(setq ,place ,value)
                place))
      (let ((place (!macroexpand-1 place)))
        (let* ((access-fn (car place))
               (expander (cdr (assoc access-fn *setf-expanders*))))
          (when (null expander)
            (error "Unknown generalized reference."))
          (apply expander (cdr place))))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (unless (symbolp access-fn)
    (error "ACCESS-FN `~S' must be a symbol." access-fn))
  `(progn (push (cons ',access-fn (lambda ,lambda-list ,@body))
                *setf-expanders*)
          ',access-fn))

(defmacro setf (&rest pairs)
  (cond
    ((null pairs)
     nil)
    ((null (cdr pairs))
     (error "Odd number of arguments to setf."))
    ((null (cddr pairs))
     (let ((place (!macroexpand-1 (first pairs)))
           (value (second pairs)))
       (multiple-value-bind (vars vals store-vars writer-form reader-form)
           (get-setf-expansion place)
         ;; TODO: Optimize the expansion a little bit to avoid let*
         ;; or multiple-value-bind when unnecesary.
         `(let* ,(mapcar #'list vars vals)
            (multiple-value-bind ,store-vars
                ,value
              ,writer-form
              ,reader-form)))))
    (t
     `(progn
        ,@(do ((pairs pairs (cddr pairs))
               (result '() (cons `(setf ,(car pairs) ,(cadr pairs)) result)))
              ((null pairs)
               (reverse result)))))))

;; Incorrect typecase, but used in NCONC.
(defmacro typecase (x &rest clausules)
  (let ((value (gensym)))
    `(let ((,value ,x))
       (cond
         ,@(mapcar (lambda (c)
                     (if (eq (car c) t)
                         `(t ,@(rest c))
                         `((,(ecase (car c)
                                    (integer 'integerp)
                                    (cons 'consp)
                                    (symbol 'symbolp)
                                    (function 'functionp)
                                    (float 'floatp)
                                    (array 'arrayp)
                                    (string 'stringp)
                                    (atom 'atom)
                                    (null 'null))
                             ,value)
                           ,@(or (rest c)
                                 (list nil)))))
                   clausules)))))

(defmacro etypecase (x &rest clausules)
  (let ((g!x (gensym)))
    `(let ((,g!x ,x))
       (typecase ,g!x
         ,@clausules
         (t (error "~X fell through etypecase expression." ,g!x))))))

(defun notevery (predicate &rest sequences)
  (not (apply #'every predicate sequences)))

(defun notany (predicate &rest sequences)
  (not (apply #'some predicate sequences)))

(defconstant internal-time-units-per-second 1000) 

(defun get-internal-real-time ()
  (get-internal-real-time))

(defun get-unix-time ()
  (truncate (/ (get-internal-real-time) 1000)))

(defun get-universal-time ()
  (+ (get-unix-time) 2208988800))

(defun values-list (list)
  (values-array (list-to-vector list)))

(defun values (&rest args)
  (values-list args))

(defun error (fmt &rest args)
  (%throw (apply #'format nil fmt args)))
