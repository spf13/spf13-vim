;;; dash.el --- A modern list library for Emacs

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Version: 1.0.3
;; Keywords: lists

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A modern list api for Emacs.
;;
;; See documentation on https://github.com/magnars/dash.el#functions

;;; Code:

(defmacro !cons (car cdr)
  "Destructive: Sets CDR to the cons of CAR and CDR."
  `(setq ,cdr (cons ,car ,cdr)))

(defmacro !cdr (list)
  "Destructive: Sets LIST to the cdr of LIST."
  `(setq ,list (cdr ,list)))

(defmacro --each (list &rest body)
  "Anaphoric form of `-each'."
  (let ((l (make-symbol "list")))
    `(let ((,l ,list)
           (it-index 0))
       (while ,l
         (let ((it (car ,l)))
           ,@body)
         (setq it-index (1+ it-index))
         (!cdr ,l)))))

(put '--each 'lisp-indent-function 1)

(defun -each (list fn)
  "Calls FN with every item in LIST. Returns nil, used for side-effects only."
  (--each list (funcall fn it)))

(defmacro --each-while (list pred &rest body)
  "Anaphoric form of `-each-while'."
  (let ((l (make-symbol "list"))
        (c (make-symbol "continue")))
    `(let ((,l ,list)
           (,c t))
       (while (and ,l ,c)
         (let ((it (car ,l)))
           (if (not ,pred) (setq ,c nil) ,@body))
         (!cdr ,l)))))

(put '--each-while 'lisp-indent-function 2)

(defun -each-while (list pred fn)
  "Calls FN with every item in LIST while (PRED item) is non-nil.
Returns nil, used for side-effects only."
  (--each-while list (funcall pred it) (funcall fn it)))

(defmacro --dotimes (num &rest body)
  "Repeatedly executes BODY (presumably for side-effects) with `it` bound to integers from 0 through n-1."
  `(let ((it 0))
     (while (< it ,num)
       ,@body
       (setq it (1+ it)))))

(put '--dotimes 'lisp-indent-function 1)

(defun -dotimes (num fn)
  "Repeatedly calls FN (presumably for side-effects) passing in integers from 0 through n-1."
  (--dotimes num (funcall fn it)))

(defun -map (fn list)
  "Returns a new list consisting of the result of applying FN to the items in LIST."
  (mapcar fn list))

(defmacro --map (form list)
  "Anaphoric form of `-map'."
  `(mapcar (lambda (it) ,form) ,list))

(defmacro --reduce-from (form initial-value list)
  "Anaphoric form of `-reduce-from'."
  `(let ((acc ,initial-value))
     (--each ,list (setq acc ,form))
     acc))

(defun -reduce-from (fn initial-value list)
  "Returns the result of applying FN to INITIAL-VALUE and the
first item in LIST, then applying FN to that result and the 2nd
item, etc. If LIST contains no items, returns INITIAL-VALUE and
FN is not called.

In the anaphoric form `--reduce-from', the accumulated value is
exposed as `acc`."
  (--reduce-from (funcall fn acc it) initial-value list))

(defmacro --reduce (form list)
  "Anaphoric form of `-reduce'."
  (let ((lv (make-symbol "list-value")))
    `(let ((,lv ,list))
       (if ,lv
           (--reduce-from ,form (car ,lv) (cdr ,lv))
         (let (acc it) ,form)))))

(defun -reduce (fn list)
  "Returns the result of applying FN to the first 2 items in LIST,
then applying FN to that result and the 3rd item, etc. If LIST
contains no items, FN must accept no arguments as well, and
reduce returns the result of calling FN with no arguments. If
LIST has only 1 item, it is returned and FN is not called.

In the anaphoric form `--reduce', the accumulated value is
exposed as `acc`."
  (if list
      (-reduce-from fn (car list) (cdr list))
    (funcall fn)))

(defmacro --filter (form list)
  "Anaphoric form of `-filter'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (when ,form (!cons it ,r)))
       (nreverse ,r))))

(defun -filter (pred list)
  "Returns a new list of the items in LIST for which PRED returns a non-nil value.

Alias: `-select'"
  (--filter (funcall pred it) list))

(defalias '-select '-filter)
(defalias '--select '--filter)

(defmacro --remove (form list)
  "Anaphoric form of `-remove'."
  `(--filter (not ,form) ,list))

(defun -remove (pred list)
  "Returns a new list of the items in LIST for which PRED returns nil.

Alias: `-reject'"
  (--remove (funcall pred it) list))

(defalias '-reject '-remove)
(defalias '--reject '--remove)

(defmacro --keep (form list)
  "Anaphoric form of `-keep'."
  (let ((r (make-symbol "result"))
        (m (make-symbol "mapped")))
    `(let (,r)
       (--each ,list (let ((,m ,form)) (when ,m (!cons ,m ,r))))
       (nreverse ,r))))

(defun -keep (fn list)
  "Returns a new list of the non-nil results of applying FN to the items in LIST."
  (--keep (funcall fn it) list))

(defmacro --map-when (pred rep list)
  "Anaphoric form of `-map-when'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list (!cons (if ,pred ,rep it) ,r))
       (nreverse ,r))))

(defmacro --map-indexed (form list)
  "Anaphoric form of `-map-indexed'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each ,list
         (!cons ,form ,r))
       (nreverse ,r))))

(defun -map-indexed (fn list)
  "Returns a new list consisting of the result of (FN index item) for each item in LIST.

In the anaphoric form `--map-indexed', the index is exposed as `it-index`."
  (--map-indexed (funcall fn it-index it) list))

(defun -map-when (pred rep list)
  "Returns a new list where the elements in LIST that does not match the PRED function
are unchanged, and where the elements in LIST that do match the PRED function are mapped
through the REP function."
  (--map-when (funcall pred it) (funcall rep it) list))

(defalias '--replace-where '--map-when)
(defalias '-replace-where '-map-when)

(defun -flatten (l)
  "Takes a nested list L and returns its contents as a single, flat list."
  (if (listp l)
      (-mapcat '-flatten l)
    (list l)))

(defun -concat (&rest lists)
  "Returns a new list with the concatenation of the elements in the supplied LISTS."
  (apply 'append lists))

(defmacro --mapcat (form list)
  "Anaphoric form of `-mapcat'."
  `(apply 'append (--map ,form ,list)))

(defun -mapcat (fn list)
  "Returns the result of applying concat to the result of applying map to FN and LIST.
Thus function FN should return a collection."
  (--mapcat (funcall fn it) list))

(defmacro --first (form list)
  "Anaphoric form of `-first'."
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each-while ,list (not ,n)
         (when ,form (setq ,n it)))
       ,n)))

(defun -first (pred list)
  "Returns the first x in LIST where (PRED x) is non-nil, else nil.

To get the first item in the list no questions asked, use `car'."
  (--first (funcall pred it) list))

(defun ---truthy? (val)
  (not (null val)))

(defmacro --any? (form list)
  "Anaphoric form of `-any?'."
  `(---truthy? (--first ,form ,list)))

(defun -any? (pred list)
  "Returns t if (PRED x) is non-nil for any x in LIST, else nil.

Alias: `-some?'"
  (--any? (funcall pred it) list))

(defalias '-some? '-any?)
(defalias '--some? '--any?)

(defalias '-any-p '-any?)
(defalias '--any-p '--any?)
(defalias '-some-p '-any?)
(defalias '--some-p '--any?)

(defmacro --all? (form list)
  "Anaphoric form of `-all?'."
  (let ((a (make-symbol "all")))
    `(let ((,a t))
       (--each-while ,list ,a (setq ,a ,form))
       (---truthy? ,a))))

(defun -all? (pred list)
  "Returns t if (PRED x) is non-nil for all x in LIST, else nil.

Alias: `-every?'"
  (--all? (funcall pred it) list))

(defalias '-every? '-all?)
(defalias '--every? '--all?)

(defalias '-all-p '-all?)
(defalias '--all-p '--all?)
(defalias '-every-p '-all?)
(defalias '--every-p '--all?)

(defmacro --none? (form list)
  "Anaphoric form of `-none?'."
  `(--all? (not ,form) ,list))

(defun -none? (pred list)
  "Returns t if (PRED x) is nil for all x in LIST, else nil."
  (--none? (funcall pred it) list))

(defalias '-none-p '-none?)
(defalias '--none-p '--none?)

(defmacro --only-some? (form list)
  "Anaphoric form of `-only-some?'."
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each-while ,list (not (and ,y ,n))
         (if ,form (setq ,y t) (setq ,n t)))
       (---truthy? (and ,y ,n)))))

(defun -only-some? (pred list)
  "Returns `t` if there is a mix of items in LIST that matches and does not match PRED.
Returns `nil` both if all items match the predicate, and if none of the items match the predicate."
  (--only-some? (funcall pred it) list))

(defalias '-only-some-p '-only-some?)
(defalias '--only-some-p '--only-some?)

(defun -take (n list)
  "Returns a new list of the first N items in LIST, or all items if there are fewer than N."
  (let (result)
    (--dotimes n
      (when list
        (!cons (car list) result)
        (!cdr list)))
    (nreverse result)))

(defun -drop (n list)
  "Returns the tail of LIST without the first N items."
  (--dotimes n (!cdr list))
  list)

(defmacro --take-while (form list)
  "Anaphoric form of `-take-while'."
  (let ((r (make-symbol "result")))
    `(let (,r)
       (--each-while ,list ,form (!cons it ,r))
       (nreverse ,r))))

(defun -take-while (pred list)
  "Returns a new list of successive items from LIST while (PRED item) returns a non-nil value."
  (--take-while (funcall pred it) list))

(defmacro --drop-while (form list)
  "Anaphoric form of `-drop-while'."
  (let ((l (make-symbol "list")))
    `(let ((,l ,list))
       (while (and ,l (let ((it (car ,l))) ,form))
         (!cdr ,l))
       ,l)))

(defun -drop-while (pred list)
  "Returns the tail of LIST starting from the first item for which (PRED item) returns nil."
  (--drop-while (funcall pred it) list))

(defun -split-at (n list)
  "Returns a list of ((-take N LIST) (-drop N LIST))"
  (list (-take n list)
        (-drop n list)))

(defmacro --split-with (form list)
  "Anaphoric form of `-split-with'."
  `(list (--take-while ,form ,list)
         (--drop-while ,form ,list)))

(defun -split-with (pred list)
  "Returns a list of ((-take-while PRED LIST) (-drop-while PRED LIST))"
  (--split-with (funcall pred it) list))

(defmacro --separate (form list)
  "Anaphoric form of `-separate'."
  (let ((y (make-symbol "yes"))
        (n (make-symbol "no")))
    `(let (,y ,n)
       (--each ,list (if ,form (!cons it ,y) (!cons it ,n)))
       (list (nreverse ,y) (nreverse ,n)))))

(defun -separate (pred list)
  "Returns a list of ((-filter PRED LIST) (-remove PRED LIST))."
  (--separate (funcall pred it) list))

(defun -partition (n list)
  "Returns a new list with the items in LIST grouped into N-sized sublists.
If there are not enough items to make the last group N-sized,
those items are discarded."
  (let ((result nil)
        (sublist nil)
        (len 0))
    (while list
      (!cons (car list) sublist)
      (setq len (1+ len))
      (when (= len n)
        (!cons (nreverse sublist) result)
        (setq sublist nil)
        (setq len 0))
      (!cdr list))
    (nreverse result)))

(defun -partition-all (n list)
  "Returns a new list with the items in LIST grouped into N-sized sublists.
The last group may contain less than N items."
  (let (result)
    (while list
      (!cons (-take n list) result)
      (setq list (-drop n list)))
    (nreverse result)))

(defmacro --partition-by (form list)
  "Anaphoric form of `-partition-by'."
  (let ((r (make-symbol "result"))
        (s (make-symbol "sublist"))
        (v (make-symbol "value"))
        (n (make-symbol "new-value"))
        (l (make-symbol "list")))
    `(let ((,l ,list))
       (when ,l
         (let* ((,r nil)
                (it (car ,l))
                (,s (list it))
                (,v ,form)
                (,l (cdr ,l)))
           (while ,l
             (let* ((it (car ,l))
                    (,n ,form))
               (unless (equal ,v ,n)
                 (!cons (nreverse ,s) ,r)
                 (setq ,s nil)
                 (setq ,v ,n))
               (!cons it ,s)
               (!cdr ,l)))
           (!cons (nreverse ,s) ,r)
           (nreverse ,r))))))

(defun -partition-by (fn list)
  "Applies FN to each value in LIST, splitting it each time FN returns a new value."
  (--partition-by (funcall fn it) list))

(defmacro --group-by (form list)
  "Anaphoric form of `-group-by'."
  (let ((l (make-symbol "list"))
        (v (make-symbol "value"))
        (k (make-symbol "key"))
        (r (make-symbol "result")))
    `(let ((,l ,list)
           ,r)
       ;; Convert `list' to an alist and store it in `r'.
       (while ,l
         (let* ((,v (car ,l))
                (it ,v)
                (,k ,form)
                (kv (assoc ,k ,r)))
           (if kv
               (setcdr kv (cons ,v (cdr kv)))
             (push (list ,k ,v) ,r))
           (setq ,l (cdr ,l))))
       ;; Reverse lists in each group.
       (let ((rest ,r))
         (while rest
           (let ((kv (car rest)))
             (setcdr kv (nreverse (cdr kv))))
           (setq rest (cdr rest))))
       ;; Reverse order of keys.
       (nreverse ,r))))

(defun -group-by (fn list)
  "Separate LIST into an alist whose keys are FN applied to the
elements of LIST.  Keys are compared by `equal'."
  (--group-by (funcall fn it) list))

(defun -interpose (sep list)
  "Returns a new list of all elements in LIST separated by SEP."
  (let (result)
    (when list
      (!cons (car list) result)
      (!cdr list))
    (while list
      (setq result (cons (car list) (cons sep result)))
      (!cdr list))
    (nreverse result)))

(defun -interleave (&rest lists)
  "Returns a new list of the first item in each list, then the second etc."
  (let (result)
    (while (-none? 'null lists)
      (--each lists (!cons (car it) result))
      (setq lists (-map 'cdr lists)))
    (nreverse result)))

(defun -partial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with ARGS first and
then additional args."
  (apply 'apply-partially fn args))

(defun -rpartial (fn &rest args)
  "Takes a function FN and fewer than the normal arguments to FN,
and returns a fn that takes a variable number of additional ARGS.
When called, the returned function calls FN with the additional
args first and then ARGS.

Requires Emacs 24 or higher."
  `(closure (t) (&rest args)
            (apply ',fn (append args ',args))))

(defun -applify (fn)
  "Changes an n-arity function FN to a 1-arity function that
expects a list with n items as arguments"
  (apply-partially 'apply fn))

(defmacro -> (x &optional form &rest more)
  "Threads the expr through the forms. Inserts X as the second
item in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
second item in second form, etc."
  (cond
   ((null form) x)
   ((null more) (if (listp form)
                    `(,(car form) ,x ,@(cdr form))
                  (list form x)))
   (:else `(-> (-> ,x ,form) ,@more))))

(defmacro ->> (x form &rest more)
  "Threads the expr through the forms. Inserts X as the last item
in the first form, making a list of it if it is not a list
already. If there are more forms, inserts the first form as the
last item in second form, etc."
  (if (null more)
      (if (listp form)
          `(,(car form) ,@(cdr form) ,x)
        (list form x))
    `(->> (->> ,x ,form) ,@more)))

(defmacro --> (x form &rest more)
  "Threads the expr through the forms. Inserts X at the position
signified by the token `it' in the first form. If there are more
forms, inserts the first form at the position signified by `it'
in in second form, etc."
  (if (null more)
      (if (listp form)
          (--map-when (eq it 'it) x form)
        (list form x))
    `(--> (--> ,x ,form) ,@more)))

(put '-> 'lisp-indent-function 1)
(put '->> 'lisp-indent-function 1)
(put '--> 'lisp-indent-function 1)

(defun -distinct (list)
  "Return a new list with all duplicates removed.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil.

Alias: `-uniq'"
  (let (result)
    (--each list (unless (-contains? result it) (!cons it result)))
    (nreverse result)))

(defun -union (list list2)
  "Return a new list containing the elements of LIST1 and elements of LIST2 that are not in LIST1.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (let (result)
    (--each list (!cons it result))
    (--each list2 (unless (-contains? result it) (!cons it result)))
    (nreverse result)))

(defalias '-uniq '-distinct)

(defun -intersection (list list2)
  "Return a new list containing only the elements that are members of both LIST and LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (-contains? list2 it) list))

(defun -difference (list list2)
  "Return a new list with only the members of LIST that are not in LIST2.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (--filter (not (-contains? list2 it)) list))

(defvar -compare-fn nil
  "Tests for equality use this function or `equal' if this is nil.
It should only be set using dynamic scope with a let, like:
(let ((-compare-fn =)) (-union numbers1 numbers2 numbers3)")

(defun -contains? (list element)
  "Return whether LIST contains ELEMENT.
The test for equality is done with `equal',
or with `-compare-fn' if that's non-nil."
  (not
   (null
    (cond
     ((null -compare-fn)    (member element list))
     ((eq -compare-fn 'eq)  (memq element list))
     ((eq -compare-fn 'eql) (memql element list))
     (t
      (let ((lst list))
        (while (and lst
                    (not (funcall -compare-fn element (car lst))))
          (setq lst (cdr lst)))
        lst))))))

(defalias '-contains-p '-contains?)

(eval-after-load "lisp-mode"
  '(progn
     (let ((new-keywords '(
                           "--each"
                           "-each"
                           "--each-while"
                           "-each-while"
                           "--dotimes"
                           "-dotimes"
                           "-map"
                           "--map"
                           "--reduce-from"
                           "-reduce-from"
                           "--reduce"
                           "-reduce"
                           "--filter"
                           "-filter"
                           "-select"
                           "--select"
                           "--remove"
                           "-remove"
                           "-reject"
                           "--reject"
                           "--keep"
                           "-keep"
                           "-flatten"
                           "-concat"
                           "--mapcat"
                           "-mapcat"
                           "--first"
                           "-first"
                           "--any?"
                           "-any?"
                           "-some?"
                           "--some?"
                           "-any-p"
                           "--any-p"
                           "-some-p"
                           "--some-p"
                           "--all?"
                           "-all?"
                           "-every?"
                           "--every?"
                           "-all-p"
                           "--all-p"
                           "-every-p"
                           "--every-p"
                           "--none?"
                           "-none?"
                           "-none-p"
                           "--none-p"
                           "-only-some?"
                           "--only-some?"
                           "-only-some-p"
                           "--only-some-p"
                           "-take"
                           "-drop"
                           "--take-while"
                           "-take-while"
                           "--drop-while"
                           "-drop-while"
                           "-split-at"
                           "--split-with"
                           "-split-with"
                           "-partition"
                           "-partition-all"
                           "-interpose"
                           "-interleave"
                           "--map-when"
                           "-map-when"
                           "--replace-where"
                           "-replace-where"
                           "-partial"
                           "-rpartial"
                           "->"
                           "->>"
                           "-->"
                           "-distinct"
                           "-intersection"
                           "-difference"
                           "-contains?"
                           "-contains-p"
                           ))
           (special-variables '(
                                "it"
                                "it-index"
                                "acc"
                                )))
       (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\<" (regexp-opt special-variables 'paren) "\\>")
                                                   1 font-lock-variable-name-face)) 'append)
       (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                   1 font-lock-keyword-face)) 'append))
     (--each (buffer-list)
       (with-current-buffer it
         (when (and (eq major-mode 'emacs-lisp-mode)
                    (boundp 'font-lock-mode)
                    font-lock-mode)
           (font-lock-refresh-defaults))))))

(provide 'dash)
;;; dash.el ends here
