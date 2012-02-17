;; .yas-setup.el for rails-mode
(defvar yas/rails-root-cache nil)

(defun yas/rails-online-doc ()
  (interactive)
  (browse-url (format "http://apidock.com/rails/search/quick?query=%s" (read-from-minibuffer "Word: " (thing-at-point 'word)))))

(if (require 'rhtml-mode nil t)
    (add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . yas/rails-erb-mode)))


(define-derived-mode yas/rails-erb-mode
  nxml-mode "eRB"
  "Embedded Ruby Mode, very thin layer over `nxml-mode'."
  (add-to-list (make-local-variable 'yas/extra-modes) 'html-mode)
  (rng-set-vacuous-schema)
  (message "hey erb mode"))

(defvar yas/rails-erb-font-lock-keywords
  '(("\\(<%=\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-string-face)
     (3 font-lock-function-name-face))
    ("\\(<%\\)\\(.*+\\)\\(%>\\)"
     (1 font-lock-variable-name-face)
     (2 font-lock-string-face)
     (3 font-lock-variable-name-face)))
  "(Crummy) font lock highlighting for ERB constructs.."
  )
(font-lock-add-keywords 'yas/rails-erb-mode yas/rails-erb-font-lock-keywords)

;; stolen from rinari-mode's rinari-root
(defun yas/rails-root (&optional dir)
  (or dir (setq dir default-directory))
  (or (and (featurep 'rinari) (rinari-root dir))
      yas/rails-root-cache
      (if (file-exists-p (expand-file-name
                          "environment.rb" (expand-file-name "config" dir)))
          (set (make-local-variable 'yas/rails-root-cache) dir)
        (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
          ;; regexp to match windows roots, tramp roots, or regular posix roots
          (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
            (yas/rails-root new-dir))))))

(defun yas/rails-find-alternate-file ()
  (if (featurep 'rinari)
      (cond ((yas/rails-view-p)
             (rinari-find-model))
            ((yas/rails-model-p)
             (rinari-find-controller))
            ((yas/rails-controller-p)
             (rinari-find-view))
            (t
             (message "oops, have to improve `yas/rails-find-alternate-file'")))
      (yas/unimplemented)))
  
;; stolen from rinari-mode's rinari-extract-partial
(defun yas/rails-extract-partial (begin end partial-name)
  (interactive "r\nsName your partial: ")
  (let* ((path (buffer-file-name)) ending)
    (if (string-match "view" path)
	(let ((ending (and (string-match ".+?\\(\\.[^/]*\\)$" path)
			   (match-string 1 path)))
	      (partial-name
	       (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
	  (kill-region begin end)
	  (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
	      (let ((default-directory (expand-file-name (match-string 1 partial-name)
							 (expand-file-name ".."))))
		(find-file (concat "_" (match-string 2 partial-name) ending)))
	    (find-file (concat "_" partial-name ending)))
	  (yank) (pop-to-buffer nil)
	  (insert (concat "<%= render :partial => '" partial-name "' %>\n")))
      (message "not in a view"))))
;;;
;;; The TextMate "intelligent" migration snippet
;;
(defvar yas/rails-intelligent-migration-snippet-bits
      '((:rename_column . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}$0")
                           (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_column_continue . ((:up   . "rename_column :${1:table_name}, :${2:column_name}, :${3:new_column_name}\nmncc$0")
                                    (:down . "rename_column :$1, :$3, :$2" )))

        (:rename_table . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}$0")
                          (:down . "rename_table :$2, :$1" )))

        (:rename_table_continue . ((:up   . "rename_table :${1:old_table_name}, :${2:new_table_name}\nmntc$0")
                                   (:down . "rename_table :$2, :$1" )))

        (:add_remove_column . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}$0")
                               (:down . "remove_column :$1, :$2" )))
        
        (:add_remove_column_continue . ((:up   . "add_column :${1:table_name}, :${2:column_name}, :${3:string}\nmarcc$0")
                                        (:down . "remove_column :$1, :$2" )))
        
        (:remove_add_column . ((:up   . "remove_column :${1:table_name}, :${2:column_name}$0")
                               (:down . "add_column :$1, :$2, :$3{string}" )))

        (:create_drop_table . ((:up   . "create_table :${1:table_name}, :force . true do |t|\nt.$0\nt.timestamps\nend")
                               (:down . "drop_table :$1" )))

        (:change_change_table . ((:up   . "change_table :${1:table_name} do |t|\nt.$0\nend")
                                 (:down . "change_table :$1 do |t|\nend" )))

        (:add_remove_index . ((:up   . "add_index :${1:table_name}, :${2:column_name}$0")
                              (:down . "remove_index :$1, :$2" )))

        (:add_remove_unique_index . ((:up   . "add_index :${1:table_name}, ${2:[:${3:column_name}${4:, :${5:column_name}}]}, :unique . true$0")
                                     (:down . "remove_index :$1, :column . $2" )))

        (:add_remove_named_index . ((:up   . "add_index :${1:table_name}, [:${2:column_name}${3:, :${4:column_name}}], :name . \"${5:index_name}\"${6:, :unique . true}$0")
                                    (:down . "remove_index :$1, :name . :$5" )))))


(defun yas/rails-intelligent-migration-snippet (type)
  (let* ((start  (point))
         (end (save-excursion
                (search-forward-regexp "^\s*def\sself\.down" nil 'noerror)))
         (up (aget (aget yas/rails-intelligent-migration-snippet-bits type) :up))
         (down (aget (aget yas/rails-intelligent-migration-snippet-bits type) :down))
         (snippet
          (and up down start end (concat up
                                         (buffer-substring-no-properties start end)
                                         "\n" down))))
    (when snippet
      (delete-region start end)
      (yas/expand-snippet snippet))))

(yas/define-condition-cache
  yas/rails-intelligent-migration-snippet-condition-p
  "Non-nil if an \"intelligent\" migration snippet should be expanded"
  (and (yas/rails-migration-p)
       (not (yas/rails-in-create-table-p))
       (not (yas/rails-in-change-table-p))
       (yas/rails-in-ruby-block-like "self\.up")))

(defun yas/rails-in-ruby-block-like (regexp)
  (save-excursion
    (ruby-accurate-end-of-block)
    (ruby-backward-sexp)
    (search-forward-regexp regexp (line-end-position) t)))

;;; conditions
(yas/define-condition-cache
 yas/rails-in-create-table-p
 "Non-nil if point is inside a 'create_table' method call."
 (yas/rails-in-ruby-block-like "create_table"))

(yas/define-condition-cache
 yas/rails-in-change-table-p
 "Non-nil if point is inside a 'change_table' method call."
 (yas/rails-in-ruby-block-like "change_table"))

(yas/define-condition-cache
 yas/rails-model-p
 "Non-nil if the current buffer is a rails model."
 (and (yas/rails-root)
      (string-match "app/models/$" default-directory)))

(yas/define-condition-cache
 yas/rails-view-p
 "Non-nil if the current buffer is a rails view."
 (and (yas/rails-root)
      (string-match "app/views/" default-directory)))

(yas/define-condition-cache
 yas/rails-helper-p
 "Non-nil if the current buffer is a rails helper."
 (and (yas/rails-root)
      (string-match "app/helpers/" default-directory)))

(yas/define-condition-cache
 yas/rails-controller-p
"Non-nil if the current buffer is a rails controller." 
 (and (yas/rails-root)
      (string-match "app/controllers/$" default-directory)))

(yas/define-condition-cache
 yas/rails-migration-p
 "Non-nil if the current buffer is a rails migration."
 (and (yas/rails-root)
      (string-match "db/migrate/" default-directory)))

(defun yas/rails-activate-maybe ()
  (when (and yas/minor-mode
             (yas/rails-root))
    (add-to-list (make-local-variable 'yas/extra-modes) 'rails-mode)))

(defadvice cd (after yas/rails-on-cd-activate activate)
  "Add `rails-mode' to `yas/extra-modes' so that rails snippets
are recognized. Stolen from `rinari-mode' more or`' less."
  (setq yas/rails-root-cache nil)
  (yas/rails-activate-maybe))

(add-hook 'yas/minor-mode-hook 'yas/rails-activate-maybe)
;; Substitutions for: content
;; 
;; # as in Macros/Remove 3A Add Column.yasnippet
;; 809BCA42-5C49-4B08-B3C4-BB773036C086                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Named Index.yasnippet
;; A7F692C1-778A-48B8-945E-573568BA0403                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Unit Test.yasnippet
;; BDBB15A4-2824-4BEC-93A5-7475F9C46A39                                                       =yyas> (if (featurep 'rinari) (rinari-find-test) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; 09BB96F2-75FD-48A7-8314-B5B56B09B477                                                       =yyas> (ffap)
;; 
;; # as in Commands/Test Uncommitted.yasnippet
;; 212C3047-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Redo Last Migration.yasnippet
;; CFDA9F62-D071-4E0F-AD10-66AE0729FFCF                                                       =yyas> (yas/rails-compile "rake")
;; 
;; # as in Commands/Documentation for Word.yasnippet
;; 32F30207-D827-46D9-889A-451C35269D52                                                       =yyas> (yas/rails-online-doc)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; 0BCF0EE2-35EE-4959-A771-E74D55271D5A                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; 275C0B86-F735-49B6-8A22-218A8F4CC2E0                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Change Table.yasnippet
;; 20FC02C5-32A3-4F20-B163-FF75C9FDFABF                                                       =yyas> (yas/rails-intelligent-migration-snippet :change_change_table)
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; 985F56D4-82ED-4C45-8250-2ECCFC71957E                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Import.yasnippet
;; 6DEF923E-2347-46EC-AFBE-183D08E63DC1                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures (Test DB).yasnippet
;; F758BFD1-00CA-4742-BE71-032580080F5C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; D696FA2C-785A-4B73-A2F6-F750904DD7C2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Remove 3A Add Timestamps.yasnippet
;; E885A3E8-8020-4AC3-A25E-510B26F114B2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns (marcc).yasnippet
;; 27A6C58A-896B-4956-BA81-D671A2EF9C7D                                                       =yyas> (yas/rails-intelligent-migration-snippet :add_remove_column_continue)
;; 
;; # as in Macros/Add 3A Remove Column.yasnippet
;; 18C76913-061C-4D65-866D-67AA3724AFEF                                                       =yyas> (yas/rails-intelligent-migration-snippet :add_remove_column)
;; 
;; # as in Commands/Go To View.yasnippet
;; EE862691-A624-4797-90CF-EDD39EFB2D8E                                                       =yyas> (if (featurep 'rinari) (rinari-find-view) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Test Plugins.yasnippet
;; 0D966168-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column.yasnippet
;; 42DE1441-D1B7-4998-BAF9-16B1EC7E210C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; 1DD8A214-1C97-45BA-ADEE-8F888DDE8570                                                       =yyas> (call-interactively 'yas/rails-extract-partial)
;; 
;; # as in Commands/Go To Functional Test.yasnippet
;; DFE393BE-0764-49FE-B464-6350A50921E6                                                       =yyas> (if (featurep 'rinari) (rinari-find-test) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Test Recent.yasnippet
;; 190401C2-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test All.yasnippet
;; DC549A45-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Column.yasnippet
;; AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Create 3A Drop Table.yasnippet
;; 25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Unique Index.yasnippet
;; 33057A79-677B-4DFB-99D4-1492778BDDC6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Timestamps.yasnippet
;; 221969A1-A5EA-4A8E-8817-C74EBED63901                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Helper.yasnippet
;; 51C9C27A-D931-49F9-B6D8-C0E7ABEC992D                                                       =yyas> (if (featurep 'rinari) (rinari-find-helper) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/DB Schema Dump.yasnippet
;; 310C901C-EF32-4E88-938A-804ABBF8C428                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Functionals.yasnippet
;; F4EA552D-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Migrate to Previous Version.yasnippet
;; 9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Model.yasnippet
;; C7151BF3-7068-4344-9B09-86F3BF4A9C63                                                       =yyas> (if (featurep 'rinari) (rinari-find-model) (yas/unimplemented 'rinari))
;; 
;; # as in Macros/Drop 3A Create Table.yasnippet
;; A2135370-67A1-488D-B43C-B4F221127C2F                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column Default.yasnippet
;; A219EBB8-004A-4012-B5B2-232C9A5C94F8                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Index.yasnippet
;; 95F83E1D-5B03-424F-8BEC-8AF66C8939BC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures.yasnippet
;; 5EEA0C71-B34B-4408-953B-F47AAD343CCC                                                       =yyas> (yas/unknown)
;; 

;; 
;; # as in Commands/Clone Development DB to Test DB.yasnippet
;; 6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns.yasnippet
;; F03162DE-9DB6-417B-9DD7-52D9F11EA736                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Stylesheet.yasnippet
;; B207BBD4-D6AA-41E9-9530-27210F2D7B66                                                       =yyas> (if (featurep 'rinari) (rinari-find-stylesheet) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Go To Javascript.yasnippet
;; B078346F-61D8-4E75-9427-80720FBC67F7                                                       =yyas> (if (featurep 'rinari) (rinari-find-javascript) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Rake Migrate to Version.yasnippet
;; 07C696F8-79F5-4E0B-9EE9-03B693A54ABB                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> (yas/rails-find-alternate-file) 
;; 
;; # as in Commands/View demo help.yasnippet
;; 964436B8-E578-11DC-8177-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go to Fixture.yasnippet
;; 638D94A4-BDFC-4FE9-8909-9934F3FD2899                                                       =yyas> (if (featurep 'rinari) (rinari-find-fixture) (yas/unimplemented 'rinari))
;; 
;; # as in Macros/Rename Table.yasnippet
;; FD8CC811-2AD3-480F-B975-DF959DC96C67                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns (mncc).yasnippet
;; 04A86178-71B1-430A-A06D-DFF7C9A338B5                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate.yasnippet
;; 4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Integration.yasnippet
;; 04A30A4D-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; 1970AE74-3949-40B3-B263-727AA3FF167A                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns.yasnippet
;; 7BC860E6-7561-4E6E-983B-507D7A6F6228                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Install Bundled Plugin.yasnippet
;; 46ECE243-0448-4A64-A223-27CC21E7704D                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> (if (featurep 'rinari) (rinari-find-file-in-project) (yas/unimplemented 'rinari))
;; 
;; # as in Commands/Test Units.yasnippet
;; 2C60CBA1-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/returning do 7Cvariable7C E280A6 end.yasnippet
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1: |)/}                     =yyas> ${2:$(and (yas/text) " |")}
;; ${2/(^(?<var>\s*[a-z_][a-zA-Z0-9_]*\s*)(,\g<var>)*,?\s*$)|.*/(?1:|)/}                      =yyas> ${2:$(and (yas/text) "|")}
;; 
;; # as in Snippets/form_for label.yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1: :\u$0)/g}                                                        =yyas> ${1:$(capitalize (replace-regexp-in-string "_" " " yas/text))}
;;
;; # as in Snippets/has_one (ho).yasnippet
;; ${1/[[:alpha:]]+|(_)/(?1::\u$0)/g}                                                         =yyas> ${1:$(replace-regexp-in-string "_" "" (capitalize yas/text))}
;;
;; # as in Snippets/Create sweeper class.yasnippet
;; ${1/./\l$0/}                                                                               =yyas> ${1:$(and (yas/text) (concat (downcase (substring yas/text 0 1)) (substring yas/text 1)))} 
;;
;; # as in Snippets/image_submit_tag.yasnippet
;; ${1/^(\w+)(\.\w*)?$/$1/}                                                                   =yyas> ${1:$(file-name-sans-extension yas-text)}
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; $TM_SELECTED_TEXT                                                                          =yyas> `yas/selected-text`
;;
;; # as in Snippets/find_in_batches.yasnippet
;; ${TM_CURRENT_WORD/(\w+)\./\L$1/g}                                                          =yyas> `(downcase (replace-regexp-in-string "\\..*$"  "" (current-word)))` 
;; 

;; Substitutions for: condition

;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.unit_test, source.js, source.css, source.yaml, meta.rails.controller, meta.rails.functional_test, text.haml =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.helper, meta.rails.functional_test, source.js, source.css, source.yaml, meta.rails.model, meta.rails.unit_test, text.haml      =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test, text.haml                                      =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.unit_test, meta.rails.functional_test                                                                               =yyas> t
;; text.html.ruby, source.ruby.rails.embedded.html, meta.rails.controller, meta.rails.helper, text.haml                                                                                       =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, meta.rails.functional_test, source.yaml                                                                                        =yyas> t
;; meta.rails.controller, meta.rails.mailer, source.js, source.css                                                                                                                            =yyas> t
;; meta.rails.controller, meta.rails.helper, meta.rails.model, source.yaml, meta.rails.unit_test                                                                                              =yyas> t
;; meta.rails.migration - meta.rails.migration.create_table - meta.rails.migration.change_table                                                                                               =yyas> (yas/rails-intelligent-migration-snippet-condition-p)
;; meta.rails.migration.create_table, meta.rails.migration.change_table                                                                                                                       =yyas> (or (yas/rails-in-create-table-p) (yas/rails-in-change-table-p))
;; meta.rails.controller, meta.rails.mailer, source.js, source.css                                                                                                                            =yyas> (yas/unknown)
;; meta.rails.migration.create_table                                                                                                                                                          =yyas> (yas/rails-create-table-p)
;; meta.rails.functional_test                                                                                                                                                                 =yyas> (yas/rails-functional-test-p)
;; text.html.ruby, text.haml                                                                                                                                                                  =yyas> (yas/rails-view-p)
;; meta.rails.controller                                                                                                                                                                      =yyas> (yas/rails-controller-p)
;; meta.rails.routes                                                                                                                                                                          =yyas> (yas/rails-routes-p)
;; text.html.ruby                                                                                                                                                                             =yyas> (yas/unknown)
;;
;;
;; AC385ABF-96CD-4FCB-80AD-BF37D6EE79D2  =yyas> (and (member major-mode '(nxml-mode html-mode rhtml-mode)) (yas/rails-view-p))


;; Substitutions for: binding
;; 
;; # as in Snippets/rails session.yasnippet
;; ^j                                                                                         =yyas> C-c M-j
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; ~$                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To View.yasnippet
;; ~$@                                                                                     =yyas> [M-S-s-down]
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; ^M                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; ~@                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; ^@S                                                                                        =yyas> C-c M-s
;; 
;; # as in Snippets/rails params.yasnippet
;; ^p                                                                                         =yyas> C-c M-p
;; 
;; # as in Commands/Go To File.yasnippet
;; 0CCC8443-40F3-4BAB-9440-D737562B5F45                                                       =yyas> [M-s-up]
;; # as in Commands/Go To Alternate File.yasnippet
;; 9453F0B3-B946-445F-BDB0-B01DE70732FC                                                       =yyas> [M-s-down]
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Units.yasnippet
;; ^\                                                                                         =yyas> C-c M-\
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; ^|                                                                                         =yyas> C-c M-|
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; @H                                                                                         =yyas> s-h
;; 
;; # as in Commands/Make Selection in to Partial.yasnippet
;; ^H                                                                                         =yyas> C-c M-m
;; 
;; # as in Commands/View demo help.yasnippet
;; ^h                                                                                         =yyas> C-c M-h
;; 
;;
;; 
;; --**--
;; Automatically generated code, do not edit this part
;; 
;; Translated menu
;; 
(yas/define-menu 'rails-mode
                 '((yas/submenu "Go To"
                                (;; Alternate File
                                 (yas/item "0CCC8443-40F3-4BAB-9440-D737562B5F45")
                                 ;; File on Current Line
                                 (yas/item "09BB96F2-75FD-48A7-8314-B5B56B09B477")
                                 (yas/separator)
                                 ;; Go to Model
                                 (yas/item "C7151BF3-7068-4344-9B09-86F3BF4A9C63")
                                 ;; Go to Controller
                                 (yas/item "9453F0B3-B946-445F-BDB0-B01DE70732FC")
                                 ;; Go to View
                                 (yas/item "EE862691-A624-4797-90CF-EDD39EFB2D8E")
                                 ;; Go to Functional Test
                                 (yas/item "DFE393BE-0764-49FE-B464-6350A50921E6")
                                 ;; Go to Helper
                                 (yas/item "51C9C27A-D931-49F9-B6D8-C0E7ABEC992D")
                                 ;; Go to Javascript
                                 (yas/item "B078346F-61D8-4E75-9427-80720FBC67F7")
                                 ;; Go to Stylesheet
                                 (yas/item "B207BBD4-D6AA-41E9-9530-27210F2D7B66")
                                 ;; Go to Unit Test
                                 (yas/item "BDBB15A4-2824-4BEC-93A5-7475F9C46A39")
                                 ;; Go to Fixture
                                 (yas/item "638D94A4-BDFC-4FE9-8909-9934F3FD2899")))
                   (yas/submenu "Run Tests"
                                (;; Ignoring Test All
                                 (yas/ignore-item "DC549A45-D9B0-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Functionals
                                 (yas/ignore-item "F4EA552D-D9B0-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Integration
                                 (yas/ignore-item "04A30A4D-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Plugins
                                 (yas/ignore-item "0D966168-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Recent
                                 (yas/ignore-item "190401C2-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Uncommitted
                                 (yas/ignore-item "212C3047-D9B1-11DC-94E9-00112475D960")
                                 ;; Ignoring Test Units
                                 (yas/ignore-item "2C60CBA1-D9B1-11DC-94E9-00112475D960")))
                   
                   ;; Ignoring Call Generate Script
                   (yas/ignore-item "4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE")
                   
                   (yas/submenu "Database"
                                (;; Ignoring Migrate to Current
                                 (yas/ignore-item "985F56D4-82ED-4C45-8250-2ECCFC71957E")
                                 ;; Ignoring Migrate to Version ...
                                 (yas/ignore-item "07C696F8-79F5-4E0B-9EE9-03B693A54ABB")
                                 ;; Ignoring Migrate to Previous Version
                                 (yas/ignore-item "9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29")
                                 ;; Redo Last Migration
                                 (yas/item "CFDA9F62-D071-4E0F-AD10-66AE0729FFCF")
                                 (yas/separator)
                                 ;; Ignoring Load Fixtures (Development DB)
                                 (yas/ignore-item "5EEA0C71-B34B-4408-953B-F47AAD343CCC")
                                 ;; Ignoring Load Fixtures (Test DB)
                                 (yas/ignore-item "F758BFD1-00CA-4742-BE71-032580080F5C")
                                 
                                 ;; Ignoring Load schema.rb to DB
                                 (yas/ignore-item "6DEF923E-2347-46EC-AFBE-183D08E63DC1")
                                 ;; Ignoring Dump DB to schema.rb
                                 (yas/ignore-item "310C901C-EF32-4E88-938A-804ABBF8C428")
                                 ;; Ignoring Clone Development DB to Test DB
                                 (yas/ignore-item "6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1")))
                   
                   ;; params[…]
                   (yas/item "AC8EDA3E-875B-11D9-897C-000393CBCE2E")
                   ;; session[…]
                   (yas/item "7B02ABF8-8763-11D9-897C-000393CBCE2E")
                   (yas/submenu "Logger"
                                (;; logger.debug
                                 (yas/item "D975E5C1-42C2-40F1-8960-0DA533B18113")
                                 ;; logger.info
                                 (yas/item "36E2A3EE-E9CC-4B7F-A4CF-AFAF970B8699")
                                 ;; logger.warn
                                 (yas/item "38D5CA05-E219-4399-A244-609AF40B1D0B")
                                 ;; logger.error
                                 (yas/item "7053B86A-9B81-4154-AB3C-61B8035C5D33")
                                 ;; logger.fatal
                                 (yas/item "35E95C81-22F7-4C40-8297-ED21086DDA81")
                                 ;; Rails.logger.debug
                                 (yas/item "7B15B396-1F41-4529-9253-32761E94448C")))
                   (yas/separator)
                   (yas/submenu "Models"
                                (;; Ignoring Show DB Schema for Current Class
                                 (yas/ignore-item "1970AE74-3949-40B3-B263-727AA3FF167A")
                                 
                                 ;; alias_attribute
                                 (yas/item "5694BA8B-64EC-4B1B-A00D-6366D28500C5")
                                 (yas/separator)
                                 (yas/submenu "Callbacks"
                                              (;; before_validation
                                               (yas/item "A1776279-5396-4FE9-9218-8BF2C88C5271")
                                               ;; before_validation_on_create
                                               (yas/item "E2CE2E3B-8A61-4866-9AF5-A12F44CF7233")
                                               ;; before_validation_on_update
                                               (yas/item "86CFB156-E72B-440F-9C7D-08A3375C3ADB")
                                               ;; after_validation
                                               (yas/item "44FBD811-70A9-462B-AC56-F975ADAD62AF")
                                               ;; after_validation_on_create
                                               (yas/item "BA0DE6C7-EAD3-42C9-8ABB-2B9A5F2FE225")
                                               ;; after_validation_on_update
                                               (yas/item "BCB25D36-2D3F-41E9-B2CF-37D6E883E8D1")
                                               ;; before_save
                                               (yas/item "523BE8A6-0845-493D-A9B6-532F73D21950")
                                               ;; after_save
                                               (yas/item "4D1787E3-1583-4CF3-8D99-CC45D7C35EED")
                                               ;; before_create
                                               (yas/item "D64D8863-DCB6-4397-B5B0-073E0AE04167")
                                               ;; after_create
                                               (yas/item "279D1981-B055-4693-B9AF-5B571A62A6AE")
                                               ;; before_destroy
                                               (yas/item "3F4B502B-5F68-4687-88E9-6EF3BDF9677D")
                                               ;; after_update
                                               (yas/item "0C9EA1A1-66C5-4E1C-9C30-E1FFE8EC6EAE")
                                               ;; before_update
                                               (yas/item "1C20EEBE-B4BA-48C8-9B33-7B5BB00D958C")
                                               ;; after_destroy
                                               (yas/item "A2F3E8C1-4216-4890-8491-2F8C7534ED03")))
                                 (yas/submenu "Associations"
                                              (;; belongs_to
                                               (yas/item "B8F08BD7-6160-482C-8A3D-CBC6BD2079A4")
                                               ;; has_and_belongs_to_many
                                               (yas/item "2AC3AC1F-743B-4A33-863C-C37885073806")
                                               ;; has_one
                                               (yas/item "BD2E4045-54E6-450E-B31B-5E1865CFFBC9")
                                               ;; has_many
                                               (yas/item "F396B7BD-8255-48B1-904A-06E7D7CC2741")
                                               ;; has_many :dependent => :destroy
                                               (yas/item "3E3AF538-171B-4108-AB92-827AD7E24C77")
                                               ;; has_many (through)
                                               (yas/item "9D58B6C9-BA52-48B3-B639-D5CB894AF810")
                                               (yas/separator)
                                               ;; accepts_nested_attributes_for
                                               (yas/item "D414D70D-BD2D-4C15-BDA6-1AAEABF7791F")))
                                 (yas/submenu "Scopes"
                                              (;; named_scope
                                               (yas/item "1CB65A0D-4FEC-4438-9B4F-8B0BD13FB875")
                                               ;; named_scope lambda
                                               (yas/item "4E286CB4-069E-474C-A970-95216FE7DE95")
                                               ;; default_scope
                                               (yas/item "83B80B60-6143-4465-B064-0DA25DDDCAA7")))
                                 (yas/submenu "Finders"
                                              (;; find(id)
                                               (yas/item "59CD3A41-8164-4FB4-B462-D7ACE86BCDBF")
                                               ;; find(:all)
                                               (yas/item "A017AB39-A875-40DC-8ACF-7E3551057CA0")
                                               ;; find(:first)
                                               (yas/item "FE430ECD-5D40-4D95-A73B-F064C73992DE")
                                               ;; find(:last)
                                               (yas/item "8B515110-41D3-11DD-AE16-0800200C9A66")
                                               ;; find_in_batches
                                               (yas/item "B660FC85-F69A-43BC-A72A-748CBEA0AA9A")
                                               (yas/separator)
                                               ;; scoped_by
                                               (yas/item "7CC002AE-83BA-4294-B87D-DE9790839D97")))
                                 (yas/submenu "Validations"
                                              (;; validates_acceptance_of
                                               (yas/item "89198999-7E6D-4D97-A20E-45263E1CA993")
                                               ;; validates_acceptance_of if
                                               (yas/item "A2477223-AD5A-4723-8052-943CE9BA634D")
                                               ;; validates_associated
                                               (yas/item "47944705-F605-4ED4-B4C0-9E823EE25138")
                                               ;; validates_associated if
                                               (yas/item "85E9264C-5414-4FA0-AC07-F305A798ED46")
                                               ;; validates_confirmation_of
                                               (yas/item "B5893618-D07C-48F1-8867-736D0AAFF0E7")
                                               ;; validates_confirmation_of if
                                               (yas/item "1354726C-DA64-4CA6-A099-26626A865D8D")
                                               ;; validates_exclusion_of
                                               (yas/item "4CC98A56-B60B-4A89-80E0-400C5314A050")
                                               ;; validates_exclusion_of if
                                               (yas/item "869AB0B7-12DD-440A-905A-BFB1E0E16E1C")
                                               ;; validates_inclusion_of
                                               (yas/item "4611F02E-E9BF-11DC-8518-00112475D960")
                                               ;; validates_inclusion_of if
                                               (yas/item "47FF50AF-E9BF-11DC-8518-00112475D960")
                                               ;; validates_format_of
                                               (yas/item "EB47FBA1-AFB3-42F9-94A4-552D3175C17A")
                                               ;; validates_format_of if
                                               (yas/item "14BF0586-F2E8-4AB3-BB4B-E49099384403")
                                               ;; validates_length_of
                                               (yas/item "5CE8838A-BF2C-497E-B87A-E90C3BC482E0")
                                               ;; validates_length_of if
                                               (yas/item "EC511A43-D3B7-11DC-BA49-00112475D960")
                                               ;; validates_numericality_of
                                               (yas/item "B21BA16D-7C04-4912-8488-425CDCC332A8")
                                               ;; validates_numericality_of if
                                               (yas/item "CF506019-E964-4172-A3DA-475AE3B65558")
                                               ;; validates_presence_of
                                               (yas/item "5DAC28A7-33C8-4DA7-9E85-56618D6BEC9F")
                                               ;; validates_presence_of if
                                               (yas/item "F5CBBE16-F5CC-4EDA-8BC6-30281BD7D854")
                                               ;; validates_uniqueness_of
                                               (yas/item "F8316545-9AE4-4C7F-87ED-A2C00E6637FA")
                                               ;; validates_uniqueness_of if
                                               (yas/item "43680344-0818-42BF-95B4-58CD2D76545B")))))
                   (yas/submenu "Controllers"
                                (;; Create controller class
                                 (yas/item "4B3F798E-E3B6-48C8-8C2F-CB8631011638")
                                 ;; Create resources controller class
                                 (yas/item "F90BFB23-5706-484B-8108-B376A988C0A0")
                                 ;; layout
                                 (yas/item "CCF4C4A1-28EF-499D-AD81-4A4FD2FEF5B6")
                                 ;; before_filter
                                 (yas/item "B782A467-2C4D-48EB-AF39-518AFED4C056")
                                 ;; flash[…]
                                 (yas/item "D864896E-8763-11D9-897C-000393CBCE2E")
                                 (yas/submenu "respond_to"
                                              (;; respond_to (html)
                                               (yas/item "3BDD0D52-443E-4F5F-AE09-ABCC2ABE9A42")
                                               ;; respond_to
                                               (yas/item "B41D3164-EA53-4DDC-850E-27B82B24061F")
                                               ;; wants.format
                                               (yas/item "3F26FDB4-ACF9-4856-9312-6A4D78DC8564")))
                                 (yas/submenu "redirect_to"
                                              (;; redirect_to (path)
                                               (yas/item "A909C4C3-8EFE-4E39-9D96-BA8F0ABE6085")
                                               ;; redirect_to (path plural)
                                               (yas/item "AFE06B67-CE98-42A6-93D1-8EC8E3B9F83C")
                                               ;; redirect_to (nested path)
                                               (yas/item "9D7228B3-A6ED-4598-B096-032B3600864F")
                                               ;; redirect_to (nested path plural)
                                               (yas/item "EF527A27-D1D4-4FD8-BD23-71397881C29A")
                                               ;; redirect_to (action)
                                               (yas/item "F2F3167C-73B9-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (action, id)
                                               (yas/item "2233B484-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller)
                                               (yas/item "053490FE-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller, action)
                                               (yas/item "0C137FBF-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to (controller, action, id)
                                               (yas/item "18D3C1C3-73BA-11D9-B752-000D932CD5BA")
                                               ;; redirect_to :back
                                               (yas/item "BC194AE7-FA60-4E65-9503-6920D1666A29")))
                                 (yas/submenu "render"
                                              (;; render (action)
                                               (yas/item "7B03D38B-7580-41AC-BC2B-3766AB074A43")
                                               ;; render (action, layout)
                                               (yas/item "053F1D6A-B413-43FF-B697-E3120FD0489F")
                                               ;; render (file)
                                               (yas/item "7D43B0EA-2C3C-499B-9346-A8E48CBF29CD")
                                               ;; render (file, use_full_path)
                                               (yas/item "2A8FBE48-E196-4019-AE76-BF3ED4B54F47")
                                               ;; render (inline)
                                               (yas/item "64E93A71-6E62-48D9-9694-123080AE6723")
                                               ;; render (inline, locals)
                                               (yas/item "1E5DE984-510C-4992-8AD5-C5FA6D7F2A88")
                                               ;; render (inline, type)
                                               (yas/item "A8AF8B90-94E8-42E1-8057-DDBA57809F6A")
                                               ;; render (layout)
                                               (yas/item "3F83272F-62D5-4BCB-BAA3-806083078829")
                                               ;; render (nothing)
                                               (yas/item "AC8A995F-0034-433C-905D-E5C1F29D6EFF")
                                               ;; render (nothing, status)
                                               (yas/item "724A68C1-A727-46FF-AF59-288E26B09629")
                                               ;; render (partial)
                                               (yas/item "498168A5-5AF8-4F59-8A2D-B517FAB98CDB")
                                               ;; render (partial, collection)
                                               (yas/item "046FB1B6-9C65-4702-91EC-4AA9878CD949")
                                               ;; render (partial, locals)
                                               (yas/item "6F41AFFD-B3A7-42D0-8A84-D6086C118D92")
                                               ;; render (partial, object)
                                               (yas/item "BFAAC8DA-A043-4684-967B-B3E5DAE08C62")
                                               ;; render (partial, status)
                                               (yas/item "CBB06A4E-3A82-45F3-91AA-259F02314B9D")
                                               ;; render (text)
                                               (yas/item "67C5082F-5011-434A-8EAA-6B8D3600935F")
                                               ;; render (text, layout)
                                               (yas/item "A3B09AFE-40B5-4623-8B85-E9F369ECE22D")
                                               ;; render (text, layout => true)
                                               (yas/item "97C0992D-715F-4322-A3E0-DD4D2B7E2FC2")
                                               ;; render (text, status)
                                               (yas/item "4F636977-F7A6-4BF5-B09B-7F087683C3B9")
                                               ;; render (update)
                                               (yas/item "ECB10C0B-E8B7-4606-ABF5-4A2A26E5AB1A")))
                                 (yas/submenu "REST methods"
                                              (;; def create - resource
                                               (yas/item "54F61419-001F-4B71-83AC-8DC633694AF0")))
                                 (yas/separator)
                                 ;; verify — render
                                 (yas/item "9ECBF20C-003E-41D9-A881-4BAC0656F9DC")
                                 ;; verify — redirect
                                 (yas/item "7BBD3F57-57A5-4CD0-8E79-B931021FC110")))
                   (yas/submenu "View Templates"
                                (;; Create Partial From Selection
                                 (yas/item "1DD8A214-1C97-45BA-ADEE-8F888DDE8570")
                                 (yas/separator)
                                 ;; form_for
                                 (yas/item "7D99041D-C3B7-4940-AE64-6B1758CDB47C")
                                 ;; form_for with errors
                                 (yas/item "15BDD7B6-5C15-4684-93C7-A05E3D2221AC")
                                 (yas/submenu "form_for f. drop-down list"
                                              (;; f.label (ffl)
                                               (yas/item "402C251E-595B-4A58-8EB9-41989040F280")
                                               ;; f.text_field (fftf)
                                               (yas/item "CC1BCD1C-2479-4335-B511-17B880316A75")
                                               ;; f.text_area (ffta)
                                               (yas/item "06498926-F84D-466C-8736-B8A0AC586A94")
                                               ;; f.check_box (ffcb)
                                               (yas/item "F579F9E7-E072-4BCC-BFF9-C8C5BAE7FFA5")
                                               ;; f.radio_button (ffrb)
                                               (yas/item "A95358D2-C68A-4894-8C36-062C9F45848A")
                                               ;; f.password_field (ffpf)
                                               (yas/item "42289456-C8D1-498C-AE30-5206544B349F")
                                               ;; f.hidden_field (ffhf)
                                               (yas/item "5DBA8F72-DD6C-4CBF-83FD-76301E159BA9")
                                               ;; f.file_field (ffff)
                                               (yas/item "79BC2303-3D9D-4E21-AF85-73B388B7B56D")
                                               ;; f.submit (ffs)
                                               (yas/item "C315EC5D-A7F3-49CB-9795-21B78BB42FF4")
                                               ;; f.fields_for (nff)
                                               (yas/item "BBE5B6F2-A8F2-4714-9186-4FCD21A5B432")))
                                 (yas/submenu "form_for helpers"
                                              (;; form_for label
                                               (yas/item "B31822D9-2048-4D16-B2AF-00E0B4E5C368")
                                               ;; form_for text_field
                                               (yas/item "F46EE8EE-239C-46D7-980B-3F861B7D9111")
                                               ;; form_for text_area
                                               (yas/item "4C898FA8-D09C-4B28-BE42-14BB4EA4E2B1")
                                               ;; form_for check_box
                                               (yas/item "F0DB6886-4FFE-45BA-907F-44326AD8142D")
                                               ;; form_for radio_button
                                               (yas/item "D4282CE1-4171-4B13-9220-3F2718BC2505")
                                               ;; form_for password_field
                                               (yas/item "3379FB35-C664-4255-96C6-6E4B91F12759")
                                               ;; form_for hidden_field
                                               (yas/item "99FEFD9B-5A07-46E3-950D-5C474E42B695")
                                               ;; form_for file_field
                                               (yas/item "C8BA285D-E12E-4AB8-A941-514C963E8226")
                                               ;; form_for submit
                                               (yas/item "3000E569-4E19-4566-B08E-A3FFFAAC9075")
                                               ;; form_for fields_for
                                               (yas/item "16645C58-C7C8-4E72-923F-3B44932F946D")))
                                 ;; fields_for
                                 (yas/item "7C7FC66A-D566-40D3-B9DA-FCEA4EFF98C6")
                                 (yas/separator)
                                 ;; form_tag
                                 (yas/item "F0F6DACA-6A0B-11D9-BDC2-000D932CD5BA")
                                 ;; submit_tag
                                 (yas/item "D0E29200-E910-11DC-A399-00112475D960")
                                 ;; image_tag
                                 (yas/item "4EA6FBD1-CDEA-4DF6-9F70-A1EDA35AC3D7")
                                 ;; image_submit_tag
                                 (yas/item "9FB9848E-EA5A-11DC-9DE5-00112475D960")
                                 (yas/submenu "link_to"
                                              (;; link_to (path)
                                               (yas/item "326B57A7-B4A9-447B-A3D2-0EA74158E1E1")
                                               ;; link_to (path plural)
                                               (yas/item "6BA737F0-63D1-4D82-9381-4331E18B12C5")
                                               ;; link_to (nested path)
                                               (yas/item "750DEEF9-18A0-40FC-8E54-574CE5EE5565")
                                               ;; link_to (nested path plural)
                                               (yas/item "866AAD87-E458-4F2D-9E7C-3CE73EFC047B")
                                               ;; link_to (action)
                                               (yas/item "9E2B42FE-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (action, id)
                                               (yas/item "B4F952F4-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller)
                                               (yas/item "74590E16-7BCB-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller, action)
                                               (yas/item "C11C0BF5-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to (controller, action, id)
                                               (yas/item "D21BE958-7BC8-11D9-906A-000D932CD5BA")
                                               ;; link_to model
                                               (yas/item "E5E08AA0-4EDD-4583-BF07-5D6C49E98410")))
                                 ;; end (ERB)
                                 (yas/item "AC385ABF-96CD-4FCB-80AD-BF37D6EE79D2")
                                 (yas/separator)
                                 ;; map(&:sym_proc)
                                 (yas/item "EC605540-C431-4FD0-AD91-D913118DACA7")
                                 (yas/separator)
                                 ;; content_for
                                 (yas/item "9038B99B-4810-4C0B-B547-F72F2AD5CAFC")
                                 ;; for loop in rhtml
                                 (yas/item "F7744F07-306C-4951-AB5A-3D69BA5516B7")))
                   (yas/submenu "Layouts"
                                (;; javascript_include_tag
                                 (yas/item "FEF49C86-9386-405E-A191-684D1C963E3A")
                                 ;; stylesheet_link_tag
                                 (yas/item "980C7667-9D60-49FF-AF74-A7B19B379F45")))
                   (yas/submenu "RJS"
                                (;; page.replace (id, partial)
                                 (yas/item "273E5E76-8D13-4476-9C38-8AF87432CB96")
                                 ;; page.hide (*ids)
                                 (yas/item "390A447F-0FA3-4F01-A10C-4F35675E0A43")
                                 ;; page.replace_html (id, partial)
                                 (yas/item "8B914165-9C66-4FA3-9AD6-1DA41B25F8F1")
                                 ;; page.insert_html (position, id, partial)
                                 (yas/item "62BEA590-F4EF-4001-B661-764EDFB92811")
                                 ;; page.visual_effect (effect, id)
                                 (yas/item "CFDC27A3-58CF-4198-8F93-36360978F0D0")
                                 ;; page.show (*ids)
                                 (yas/item "5ACBF49D-B5A5-495C-89D8-18AA740D9D02")
                                 ;; page.toggle (*ids)
                                 (yas/item "028DA0A4-B310-4BEF-8643-2A22993C21C7")))
                   (yas/submenu "Migrations"
                                (;; Ignoring Quick Migration
                                 (yas/ignore-item "D696FA2C-785A-4B73-A2F6-F750904DD7C2")
                                 
                                 (yas/submenu "Columns"
                                              (;; Add / Remove Column
                                               (yas/item "18C76913-061C-4D65-866D-67AA3724AFEF")
                                               ;; Ignoring Add / Remove Several Columns
                                               (yas/ignore-item "7BC860E6-7561-4E6E-983B-507D7A6F6228")
                                               ;; Add / Remove Several Columns (marcc)
                                               (yas/item "27A6C58A-896B-4956-BA81-D671A2EF9C7D")
                                               ;; Ignoring Add / Remove Timestamps
                                               (yas/ignore-item "221969A1-A5EA-4A8E-8817-C74EBED63901")
                                               ;; Ignoring Change Column
                                               (yas/ignore-item "42DE1441-D1B7-4998-BAF9-16B1EC7E210C")
                                               ;; Ignoring Change Column Default
                                               (yas/ignore-item "A219EBB8-004A-4012-B5B2-232C9A5C94F8")
                                               ;; Ignoring Rename / Rename Column
                                               (yas/ignore-item "AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3")
                                               ;; Ignoring Rename / Rename Several Columns
                                               (yas/ignore-item "F03162DE-9DB6-417B-9DD7-52D9F11EA736")
                                               ;; Ignoring Rename / Rename Several Columns (mncc)
                                               (yas/ignore-item "04A86178-71B1-430A-A06D-DFF7C9A338B5")
                                               ;; Remove / Add Column
                                               (yas/item "16A705EB-10DC-42B5-9FF2-377E206421DC")
                                               ;; Ignoring Remove / Add Timestamps
                                               (yas/ignore-item "E885A3E8-8020-4AC3-A25E-510B26F114B2")))
                                 (yas/submenu "Tables"
                                              (;; Ignoring Create / Drop Table
                                               (yas/ignore-item "25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2")
                                               ;; Ignoring Rename / Rename Table
                                               (yas/ignore-item "FD8CC811-2AD3-480F-B975-DF959DC96C67")
                                               ;; Drop / Create Table
                                               (yas/item "20375601-B13F-4314-B8E4-362706566636")
                                               ;; Change / Change Table
                                               (yas/item "20FC02C5-32A3-4F20-B163-FF75C9FDFABF")
                                               (yas/separator)
                                               (yas/submenu "Create columns t. drop-down list"
                                                            (;; t.string (tcs)
                                                             (yas/item "B757F7E5-E4BD-11DC-A11A-00112475D960")
                                                             ;; t.text (tct)
                                                             (yas/item "FFE7B820-E4BD-11DC-A11A-00112475D960")
                                                             ;; t.integer (tci)
                                                             (yas/item "0E63B7D5-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.float (tcf)
                                                             (yas/item "1BDC463A-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.decimal (tcd)
                                                             (yas/item "26C09807-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.datetime (tcdt)
                                                             (yas/item "3458B140-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.timestamp (tcts)
                                                             (yas/item "49643690-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.time (tcti)
                                                             (yas/item "537BDD48-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.date (tcda)
                                                             (yas/item "61CF5B32-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.boolean (tcb)
                                                             (yas/item "6BE6F315-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.binary (tcbi)
                                                             (yas/item "7CE57C6C-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.timestamps (tctss)
                                                             (yas/item "950B0BF2-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.lock_version (tcl)
                                                             (yas/item "A677FFD4-E4BE-11DC-A11A-00112475D960")
                                                             ;; t.references (tcr)
                                                             (yas/item "B6D9225C-E4BE-11DC-A11A-00112475D960")))
                                               (yas/submenu "Create columns helpers"
                                                            (;; Table column string
                                                             (yas/item "377BF814-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column text
                                                             (yas/item "6A9D4C30-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column integer
                                                             (yas/item "729D559E-D52D-11DC-BD8E-00112475D960")
                                                             ;; Table column float
                                                             (yas/item "8AF989C4-D52E-11DC-BD8E-00112475D960")
                                                             ;; Table column decimal
                                                             (yas/item "93A16768-D52E-11DC-BD8E-00112475D960")
                                                             ;; Table column datetime
                                                             (yas/item "D6CBCA96-D52F-11DC-BD8E-00112475D960")
                                                             ;; Table column timestamp
                                                             (yas/item "4600CE20-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column time
                                                             (yas/item "4F5DDD37-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column date
                                                             (yas/item "56276686-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column boolean
                                                             (yas/item "967093B4-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column binary
                                                             (yas/item "5E9B8B0E-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column timestamps
                                                             (yas/item "E0C8FDC4-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column lock_version
                                                             (yas/item "FC2523C1-D532-11DC-BD8E-00112475D960")
                                                             ;; Table column(s) references
                                                             (yas/item "EDA6568B-D533-11DC-BD8E-00112475D960")))
                                               ;; Create Column in Table
                                               (yas/item "7592CA99-75D7-48B6-9133-00B9F148FF43")
                                               ;; Create Several Columns in Table
                                               (yas/item "67FD2F8F-5F25-45F2-A451-2F39977A9EDE")
                                               (yas/submenu "Change columns t. drop-down list"
                                                            (;; t.change (tch)
                                                             (yas/item "19761681-17E6-4DF1-8C08-73C337D29481")
                                                             ;; t.rename (tre)
                                                             (yas/item "9D4E30E2-4A61-4941-B9F3-BEE97552747A")))
                                               (yas/submenu "Change columns helpers"
                                                            (;; Table column(s) change
                                                             (yas/item "57A9D5BC-DD0F-422B-B857-53F30B5D763A")
                                                             ;; Table column(s) rename
                                                             (yas/item "DF30226E-1111-448A-B669-7CA34EE83909")))))
                                 (yas/submenu "Indexes"
                                              (;; Ignoring Add / Remove Index
                                               (yas/ignore-item "95F83E1D-5B03-424F-8BEC-8AF66C8939BC")
                                               ;; Ignoring Add / Remove Named Index
                                               (yas/ignore-item "A7F692C1-778A-48B8-945E-573568BA0403")
                                               ;; Ignoring Add / Remove Unique Index
                                               (yas/ignore-item "33057A79-677B-4DFB-99D4-1492778BDDC6")))))
                   (yas/submenu "Environment"
                                (;; config.gem
                                 (yas/item "47A6800A-DC7E-4F72-AA0D-CEE2488E1618")))
                   (yas/submenu "Routes"
                                (;; map.named_route
                                 (yas/item "91C543BF-7BD8-4E3A-B493-AE572C5472A0")
                                 ;; map.resources
                                 (yas/item "0FF86C46-0E01-4D03-8232-72CA5BD55706")
                                 ;; map.resource
                                 (yas/item "2183A9A9-17ED-4A4F-ABB6-668EDDD3A6E4")
                                 ;; map.with_options
                                 (yas/item "BD4B90F7-2187-4E75-BFFB-77BE67CB8DAE")
                                 ;; map.catch_all
                                 (yas/item "F3606586-F905-4A91-92CA-82319239221D")))
                   (yas/submenu "ActiveSupport"
                                (;; cattr_accessor
                                 (yas/item "F57522B2-9F5F-4DF9-AE46-9478AF019C63")
                                 ;; mattr_accessor
                                 (yas/item "B25B7560-FACB-4A9E-A226-B71C796BD1F3")
                                 ;; returning do |variable| … end
                                 (yas/item "D2783155-23F3-4B90-A317-5BD139471193")))
                   (yas/separator)
                   (yas/submenu "Fixtures"
                                (;; <%= Fixtures.identify(:symbol) %>
                                 (yas/item "9671EB7A-89D6-4C23-914F-88CBEE0D177A")
                                 (yas/separator)
                                 ;; Ignoring Autocomplete Foreign Key Fixture Reference
                                 (yas/ignore-item "0BCF0EE2-35EE-4959-A771-E74D55271D5A")
                                 ;; Ignoring Autocomplete Foreign Key Fixture Reference (habtm)
                                 (yas/ignore-item "275C0B86-F735-49B6-8A22-218A8F4CC2E0")))
                   ;; test do..end
                   (yas/item "6ECA11FE-E8C1-4EC0-93F3-B4472752E60D")
                   (yas/submenu "Unit Tests"
                                (;; assert_difference
                                 (yas/item "30BEA6FB-301C-4460-93EC-FA3404688962")
                                 ;; assert_no_difference
                                 (yas/item "5C6F4462-70E6-40B4-B3F2-F371656E7784")
                                 (yas/separator)))
                   (yas/submenu "Functional Tests"
                                (;; Create functional test class
                                 (yas/item "F60D0630-CBF5-4283-9D20-FA46C787A88D")
                                 ;; def test_should_get_action
                                 (yas/item "1C491A76-751F-44EF-8DFB-0A585C7EEFF6")
                                 ;; def test_should_post_action
                                 (yas/item "8B9CD068-4338-4039-AA06-D839A6C7A9FF")
                                 (yas/separator)
                                 ;; assert_response
                                 (yas/item "2BD82DCB-1F19-4C8F-BC70-C0BBB06A2138")
                                 ;; assert_redirected_to
                                 (yas/item "CD60F800-850D-47CF-BE32-3DE665DD5C68")
                                 ;; assert_redirected_to (path)
                                 (yas/item "D33EDCE7-F8AF-48D4-AA7A-852BBF03E31D")
                                 ;; assert_redirected_to (path plural)
                                 (yas/item "0249637E-0720-46DA-A8FD-E176A2CC458B")
                                 ;; assert_redirected_to (nested path)
                                 (yas/item "97021C0D-EB65-4046-B688-01F09B3B1615")
                                 ;; assert_redirected_to (nested path plural)
                                 (yas/item "4C92C020-7337-4D6E-91EE-7ABF2BFC7F41")
                                 (yas/separator)
                                 ;; assert_select
                                 (yas/item "DBE14FE8-B415-4DBC-A316-F8DA63FE9FD7")
                                 (yas/separator)
                                 ;; assert_rjs
                                 (yas/item "E0F281EC-5311-41F8-ADD9-2E2D059DA651")
                                 (yas/separator)
                                 ;; assert(var = assigns(:var))
                                 (yas/item "FE9C4B4E-860D-49F0-AAF7-5582B98F5F54")))
                   (yas/submenu "Ajax Tests"
                                (;; xhr post
                                 (yas/item "62C3838B-0790-4FC2-8425-F273A57F5D33")
                                 ;; xhr get
                                 (yas/item "78FCF992-D01B-404F-BC54-5EE7B91F999A")
                                 ;; xhr delete
                                 (yas/item "F1BE0C3D-7203-43E9-BEFB-D1A99CDD31C1")
                                 ;; xhr put
                                 (yas/item "C12C98A5-74E5-4E70-9ADB-8783455D6539")))
                   (yas/separator)
                   ;; Ignoring View demo help
                   (yas/ignore-item "964436B8-E578-11DC-8177-00112475D960")
                   ;; Documentation for Word
                   (yas/item "32F30207-D827-46D9-889A-451C35269D52")
                   ;; find_each
                   (yas/item "B105C480-FB21-4511-9AD0-D5B4FED3BA21")
                   )
                    '("A2135370-67A1-488D-B43C-B4F221127C2F"
                       "809BCA42-5C49-4B08-B3C4-BB773036C086"
                       "275C0B86-F735-49B6-8A22-218A8F4CC2E0"
                       "0BCF0EE2-35EE-4959-A771-E74D55271D5A"
                       "6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1"
                       "D696FA2C-785A-4B73-A2F6-F750904DD7C2"
                       "AECD46CF-9031-4059-B386-262DBABD97B1"
                       "F758BFD1-00CA-4742-BE71-032580080F5C"
                       "5EEA0C71-B34B-4408-953B-F47AAD343CCC"
                       "9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29"
                       "07C696F8-79F5-4E0B-9EE9-03B693A54ABB"
                       "1970AE74-3949-40B3-B263-727AA3FF167A"
                       "DC549A45-D9B0-11DC-94E9-00112475D960"
                       "F4EA552D-D9B0-11DC-94E9-00112475D960"
                       "04A30A4D-D9B1-11DC-94E9-00112475D960"
                       "0D966168-D9B1-11DC-94E9-00112475D960"
                       "190401C2-D9B1-11DC-94E9-00112475D960"
                       "212C3047-D9B1-11DC-94E9-00112475D960"
                       "2C60CBA1-D9B1-11DC-94E9-00112475D960"
                       "964436B8-E578-11DC-8177-00112475D960"
                       "310C901C-EF32-4E88-938A-804ABBF8C428"
                       "6DEF923E-2347-46EC-AFBE-183D08E63DC1"
                       "4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE"
                       "985F56D4-82ED-4C45-8250-2ECCFC71957E"
                       "95F83E1D-5B03-424F-8BEC-8AF66C8939BC"
                       "A7F692C1-778A-48B8-945E-573568BA0403"
                       "7BC860E6-7561-4E6E-983B-507D7A6F6228"
                       "221969A1-A5EA-4A8E-8817-C74EBED63901"
                       "33057A79-677B-4DFB-99D4-1492778BDDC6"
                       "A219EBB8-004A-4012-B5B2-232C9A5C94F8"
                       "42DE1441-D1B7-4998-BAF9-16B1EC7E210C"
                       "25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2"
                       "A2135370-67A1-488D-B43C-B4F221127C2F"
                       "809BCA42-5C49-4B08-B3C4-BB773036C086"
                       "E885A3E8-8020-4AC3-A25E-510B26F114B2"
                       "04A86178-71B1-430A-A06D-DFF7C9A338B5"
                       "F03162DE-9DB6-417B-9DD7-52D9F11EA736"
                       "AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3"
                       "FD8CC811-2AD3-480F-B975-DF959DC96C67"))

;; Unknown substitutions
;; 
;; Substitutions for: content
;; 
;; # as in Macros/Remove 3A Add Column.yasnippet
;; 809BCA42-5C49-4B08-B3C4-BB773036C086                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Named Index.yasnippet
;; A7F692C1-778A-48B8-945E-573568BA0403                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Uncommitted.yasnippet
;; 212C3047-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; 0BCF0EE2-35EE-4959-A771-E74D55271D5A                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; 275C0B86-F735-49B6-8A22-218A8F4CC2E0                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate.yasnippet
;; 985F56D4-82ED-4C45-8250-2ECCFC71957E                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Import.yasnippet
;; 6DEF923E-2347-46EC-AFBE-183D08E63DC1                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures (Test DB).yasnippet
;; F758BFD1-00CA-4742-BE71-032580080F5C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; D696FA2C-785A-4B73-A2F6-F750904DD7C2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Remove 3A Add Timestamps.yasnippet
;; E885A3E8-8020-4AC3-A25E-510B26F114B2                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Plugins.yasnippet
;; 0D966168-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column.yasnippet
;; 42DE1441-D1B7-4998-BAF9-16B1EC7E210C                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Recent.yasnippet
;; 190401C2-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test All.yasnippet
;; DC549A45-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Jump to Method Definition.yasnippet
;; AECD46CF-9031-4059-B386-262DBABD97B1                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Column.yasnippet
;; AC50762C-DE40-4EB9-9A22-2F6AF2EA4EA3                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Create 3A Drop Table.yasnippet
;; 25F8F5D8-2BD1-45D8-8B2A-9F2EA4F73AA2                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Unique Index.yasnippet
;; 33057A79-677B-4DFB-99D4-1492778BDDC6                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Timestamps.yasnippet
;; 221969A1-A5EA-4A8E-8817-C74EBED63901                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/DB Schema Dump.yasnippet
;; 310C901C-EF32-4E88-938A-804ABBF8C428                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Functionals.yasnippet
;; F4EA552D-D9B0-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Migrate to Previous Version.yasnippet
;; 9A1AE6BA-8350-4AB7-B5BD-969A7E64CF29                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Drop 3A Create Table.yasnippet
;; A2135370-67A1-488D-B43C-B4F221127C2F                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Change Column Default.yasnippet
;; A219EBB8-004A-4012-B5B2-232C9A5C94F8                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Index.yasnippet
;; 95F83E1D-5B03-424F-8BEC-8AF66C8939BC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Load Fixtures.yasnippet
;; 5EEA0C71-B34B-4408-953B-F47AAD343CCC                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Clone Development DB to Test DB.yasnippet
;; 6F2AB859-46E3-4FF5-A9A7-E9A813AB5DE1                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns.yasnippet
;; F03162DE-9DB6-417B-9DD7-52D9F11EA736                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Rake Migrate to Version.yasnippet
;; 07C696F8-79F5-4E0B-9EE9-03B693A54ABB                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/find_in_batches.yasnippet
;; `(downcase (replace-regexp-in-string "\..*$"  "" (current-word)))`                         =yyas> (yas/unknown)
;; 
;; # as in Commands/View demo help.yasnippet
;; 964436B8-E578-11DC-8177-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename Table.yasnippet
;; FD8CC811-2AD3-480F-B975-DF959DC96C67                                                       =yyas> (yas/unknown)
;; 
;; # as in Macros/Rename 3A Rename Several Columns (mncc).yasnippet
;; 04A86178-71B1-430A-A06D-DFF7C9A338B5                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate.yasnippet
;; 4904EDC7-5ED3-4132-AAB2-C2AD87C97EFE                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Integration.yasnippet
;; 04A30A4D-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Show DB Schema.yasnippet
;; 1970AE74-3949-40B3-B263-727AA3FF167A                                                       =yyas> (yas/unknown)
;; 
;; # as in Snippets/respond_to (html).yasnippet
;; `yas/selected-text`                                                                        =yyas> (yas/unknown)
;; 
;; # as in Macros/Add 3A Remove Several Columns.yasnippet
;; 7BC860E6-7561-4E6E-983B-507D7A6F6228                                                       =yyas> (yas/unknown)
;; 
;; # as in Commands/Test Units.yasnippet
;; 2C60CBA1-D9B1-11DC-94E9-00112475D960                                                       =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: condition
;; 
;; # as in Snippets/verify - redirect (verify).yasnippet
;;                                                                                            =yyas> (yas/unknown)
;; 
;; # as in Snippets/t_rename (tre).yasnippet
;; meta.rails.migration.change_table                                                          =yyas> (yas/unknown)
;; 
;; # as in Snippets/for loop erb.yasnippet
;; text.html.ruby                                                                             =yyas> (yas/unknown)
;; 
;; 

;; Substitutions for: binding
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference (habtm).yasnippet
;; ~$                                                                                        =yyas> (yas/unknown)
;; 
;; # as in Commands/Generate Quick Migration.yasnippet
;; ^M                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Go To File on This Line.yasnippet
;; ~@                                                                                      =yyas> (yas/unknown)
;; 
;; # as in Commands/Autocomplete Foreign Key Fixture Reference.yasnippet
;; ~                                                                                         =yyas> (yas/unknown)
;; 
;; # as in Commands/Jump to Method Definition.yasnippet
;; ^f                                                                                         =yyas> (yas/unknown)
;; 
;; 

;; .yas-setup.el for rails-mode ends here
