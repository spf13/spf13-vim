(defun yas/objc-docset-query (query)
  
  )
(defvar yas/objc-method-names (make-vector 1023 0))
(defvar yas/objc-class-names (make-vector 1023 0))

(defvar yas/objc-languages (list))
(defvar yas/objc-defkinds (list))


(defun yas/objc-extract-super-list ()
  (interactive)
  (setq yas/objc-method-names (make-vector 1023 0)
        yas/objc-class-names (make-vector 1023 0)
        yas/objc-languages (list)
        yas/objc-defkinds (list))
  (with-temp-buffer
    (shell-command
     "/Developer/usr/bin/docsetutil dump -skip-text /Developer/Documentation/DocSets/com.apple.adc.documentation.AppleSnowLeopard.CoreReference.docset/"
     (current-buffer))
    (goto-char (point-min))
    (search-forward-regexp "API index contains .* tokens")
    (while (search-forward-regexp "^\\([^/]*\\)/\\([^/]*\\)/\\([^/]*\\)/\\([^/]*\\)$" nil 'noerror)
      (intern (match-string 3) yas/objc-class-names)
      (intern (match-string 4) yas/objc-method-names)
      (add-to-list 'yas/objc-languages (match-string 1))
      (add-to-list 'yas/objc-defkinds (match-string 2)))))

;; (put (intern-soft (setq chosen (completing-read "Method: " yas/objc-method-names)) yas/objc-method-names)
;;      'someshit
;;      'someday)

;; (completing-read "Class: " yas/objc-class-names)

;; (get (intern-soft (setq chosen (completing-read "hey: " yas/objc-method-names)) yas/objc-method-names)
;;      'someshit)

(defun yas/objc-current-method-signature ()
  (let ((orig-point (point))
        (start-point nil)
        sig
        orig-ppss
        ppss)
    (save-excursion
      (condition-case nil
          (while (not (eq (point) (point-min))) (backward-sexp))
        (error nil))
      (when (eq (preceding-char) ?\[)
        (setq orig-ppss (syntax-ppss))
        (forward-sexp)
        (skip-chars-forward " \t\n")
        (setq ppss (syntax-ppss))
        (while (and (>= (car ppss) (car orig-ppss))
                    (search-forward-regexp "[[:alpha:]]+:" nil 'noerror))
          (setq ppss (syntax-ppss))
          (when (eq (car ppss) (car orig-ppss))
            (setq sig
                  (concat (or sig "") (match-string-no-properties 0)))))
          sig))))

(defun yas/objc-current-method-signature ()
  (let ((orig-point (point))
        (start-point nil)
        sig
        orig-ppss
        ppss)
    (save-excursion
      (condition-case nil
          (while (not (eq (point) (point-max))) (backward-sexp))
        (error ))
      (when (eq (preceding-char) ?\[)
        (setq orig-ppss (syntax-ppss))
        (forward-sexp)
        (skip-chars-forward " \t\n")
        (setq ppss (syntax-ppss))
        (condition-case nil
            (while (and (>= (car ppss) (car orig-ppss))
                        (search-forward-regexp "[[:alpha:]]+:" orig-point 'noerror))
              (setq ppss (syntax-ppss))
              (when (eq (car ppss) (car orig-ppss))
                (setq sig
                      (concat (or sig "") (match-string-no-properties 0))))
              (forward-sexp))
          (error nil))
        (save-excursion
          (backward-word)
          (concat sig (buffer-substring-no-properties (point) orig-point)))
        sig))))
