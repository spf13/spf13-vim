;;; mediawiki.el --- mediawiki frontend

;; Copyright (C) 2008, 2009, 2010, 2011 Mark A. Hershberger

;; Original Authors: Jerry <unidevel@yahoo.com.cn>,
;;      Chong Yidong <cyd at stupidchicken com> for wikipedia.el,
;;      Uwe Brauer <oub at mat.ucm.es> for wikimedia.el
;; Author: Mark A. Hershberger <mah@everybody.org>
;; Version: 2.2.3
;; Created: Sep 17 2004
;; Keywords: mediawiki wikipedia network wiki
;; URL: http://launchpad.net/mediawiki-el
;; Last Modified: <2011-11-28 22:55:57 mah>

(defconst mediawiki-version "2.2.3"
  "Current version of mediawiki.el")

;; This file is NOT (yet) part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This version of mediawiki.el represents a merging of
;; wikipedia-mode.el (maintained by Uwe Brauer <oub at mat.ucm.es>)
;; from http://www.emacswiki.org/emacs/wikipedia-mode.el for its
;; font-lock code, menu, draft mode, replying and convenience
;; functions to produce mediawiki.el 2.0.

;;; Installation

;; If you use ELPA (http://tromey.com/elpa), you can install via the
;; M-x package-list-packages interface. This is preferrable as you
;; will have access to updates automatically.

;; Otherwise, just make sure this file is in your load-path (usually
;; ~/.emacs.d is included) and put (require 'mediawiki.el) in your
;; ~/.emacs or ~/.emacs.d/init.el file.

;;; Howto:
;;  M-x customize-group RET mediawiki RET
;;  *dink* *dink*
;;  M-x mediawiki-site RET Wikipedia RET
;;
;; Open a wiki file:    M-x mediawiki-open
;; Save a wiki buffer:  C-x C-s
;; Save a wiki buffer with a different name:  C-x C-w

;;; TODO:
;;  * Optionally use org-mode formatting for editing and translate
;;    that to mw
;;  * Move url-* methods to url-http
;;  * Use the MW API to support searching, etc.
;;  * Clean up and thoroughly test imported wikimedia.el code
;;  * Improve language support.  Currently there is a toggle for
;;    English or German.  This should probably just be replaced with
;;    customizable words given MediaWiki's wide language support.

;;; History

;; From the News section of wikipedia.el comes this bit, kept here for
;; reference later.
;;     (4) "Draft", "send" and "reply" (for discussion pages)
;;         abilities `based' on ideas of John Wigleys remember.el: see
;;         the functions wikipedia-draft-*
;;         RATIONALE: This comes handy in 2 situations
;;            1. You are editing articles which various authors (this I
;;               think is the usual case), you then want not to submit
;;               your edit immediately but want to copy it somewhere and
;;               to continue later. You can use the following functions
;;               for doing that:
;;               wikipedia-draft-buffer \C-c\C-b
;;               wikipedia-draft-region \C-c\C-r
;;               then the buffer/region will be appended to the
;;               wikipedia-draft-data-file (default is
;;               "~/Wiki/discussions/draft.wiki", which you can visit via
;;               wikipedia-draft-view-draft) and it will be
;;               surrounded by the ^L marks in order to set a page.
;;               moreover on top on that a section header == will be
;;               inserted, which consists of the Word Draft, a subject
;;               you are asked for and a date stamp.
;;
;;               Another possibility consists in using the function
;;               wikipedia-draft, bound to \C-c \C-m then a new buffer
;;               will opened already in wikipedia mode. You edit and then
;;               either can send the content of the buffer to the
;;               wikipedia-draft-data-file in the same manner as
;;               described above using the function
;;               wikipedia-draft-buffer (bound to \C-c\C-k)
;;
;;               BACK: In order to copy/send the content of temporary
;;               buffer or of a page in the wikipedia-draft-data-file
;;               back in to your wikipedia file, use the function
;;               wikipedia-send-draft-to-mozex bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text!
;;
;;
;;            2. You want to reply  in a discussion page to a specific
;;               contribution, you can use either the function
;;
;;               \\[wikipedia-reply-at-point-simple] bound to [(meta shift r)]
;;               which inserts a newline, a hline, and the signature of
;;               the author. Or can use
;;               \\[wikipedia-draft-reply] bound  [(meta r)]
;;               which does the same as wikipedia-reply-at-point-simple
;;               but in a temporary draft buffer.
;;
;;               BACK: In order to copy/send the content of that buffer
;;               back in to your wikipedia file, use the function
;;               \\[wikipedia-send-draft-to-mozex] bound to "\C-c\C-c". You
;;               will be asked to which buffer to copy your text! If
;;               you want a copy to be send to your draft file, use
;;               the variable  wikipedia-draft-send-archive
;;

;;; Code:

(require 'url-http)
(require 'mml)
(require 'mm-url)
(require 'ring)
(eval-when-compile (progn
                     (require 'cl)
                     ;; Below copied from url-http to avoid compilation warnings
                     (defvar url-http-extra-headers)
                     (defvar url-http-target-url)
                     (defvar url-http-proxy)
                     (defvar url-http-connection-opened)
                     ;; This should only be used in xemacs, anyway
                     (setq byte-compile-not-obsolete-funcs (list 'assoc-ignore-case))))

;; As of 2010-06-22, these functions are in Emacs
(unless (fboundp 'url-bit-for-url)
  (defun url-bit-for-url (method lookfor url)
    (when (fboundp 'auth-source-user-or-password)
      (let* ((urlobj (url-generic-parse-url url))
             (bit (funcall method urlobj))
             (methods (list 'url-recreate-url
                            'url-host)))
        (if (and (not bit) (> (length methods) 0))
            (auth-source-user-or-password
             lookfor (funcall (pop methods) urlobj) (url-type urlobj))
          bit)))))

(unless (fboundp 'url-url-for-url)
  (defun url-user-for-url (url)
    "Attempt to use .authinfo to find a user for this URL."
    (url-bit-for-url 'url-user "login" url)))

(unless (fboundp 'url-password-for-url)
  (defun url-password-for-url (url)
    "Attempt to use .authinfo to find a password for this URL."
    (url-bit-for-url 'url-password "password" url)))

(when (fboundp 'url-http-create-request)
  (if (string= "GET / HTTP/1.0\nMIME-Version: 1.0\nConnection: close\nHost: example.com\nAccept: */*\nUser-Agent: URL/Emacs\nContent-length: 4\n\ntest"
	       (let ((url-http-target-url (url-generic-parse-url "http://example.com/"))
		     (url-http-data "test") (url-http-version "1.0")
		     url-http-method url-http-attempt-keepalives url-extensions-header
		     url-http-extra-headers url-http-proxy url-mime-charset-string)
		 (url-http-create-request)))
      (defun url-http-create-request (&optional ref-url)
	"Create an HTTP request for `url-http-target-url', referred to by REF-URL."
	(declare (special proxy-info
			  url-http-method url-http-data
			  url-http-extra-headers))
	(let* ((extra-headers)
	       (request nil)
	       (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
	       (using-proxy url-http-proxy)
	       (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
						    url-http-extra-headers))
				   (not using-proxy))
			       nil
			     (let ((url-basic-auth-storage
				    'url-http-proxy-basic-auth-storage))
			       (url-get-authentication url-http-target-url nil 'any nil))))
	       (real-fname (concat (url-filename url-http-target-url)
				   (url-recreate-url-attributes url-http-target-url)))
	       (host (url-host url-http-target-url))
	       (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
			 nil
		       (url-get-authentication (or
						(and (boundp 'proxy-info)
						     proxy-info)
						url-http-target-url) nil 'any nil))))
	  (if (equal "" real-fname)
	      (setq real-fname "/"))
	  (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
	  (if auth
	      (setq auth (concat "Authorization: " auth "\r\n")))
	  (if proxy-auth
	      (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))

	  ;; Protection against stupid values in the referer
	  (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
						 (string= ref-url "")))
	      (setq ref-url nil))

	  ;; We do not want to expose the referer if the user is paranoid.
	  (if (or (memq url-privacy-level '(low high paranoid))
		  (and (listp url-privacy-level)
		       (memq 'lastloc url-privacy-level)))
	      (setq ref-url nil))

	  ;; url-http-extra-headers contains an assoc-list of
	  ;; header/value pairs that we need to put into the request.
	  (setq extra-headers (mapconcat
			       (lambda (x)
				 (concat (car x) ": " (cdr x)))
			       url-http-extra-headers "\r\n"))
	  (if (not (equal extra-headers ""))
	      (setq extra-headers (concat extra-headers "\r\n")))

	  ;; This was done with a call to `format'.  Concatting parts has
	  ;; the advantage of keeping the parts of each header together and
	  ;; allows us to elide null lines directly, at the cost of making
	  ;; the layout less clear.
	  (setq request
		;; We used to concat directly, but if one of the strings happens
		;; to being multibyte (even if it only contains pure ASCII) then
		;; every string gets converted with `string-MAKE-multibyte' which
		;; turns the 127-255 codes into things like latin-1 accented chars
		;; (it would work right if it used `string-TO-multibyte' instead).
		;; So to avoid the problem we force every string to be unibyte.
		(mapconcat
		 ;; FIXME: Instead of `string-AS-unibyte' we'd want
		 ;; `string-to-unibyte', so as to properly signal an error if one
		 ;; of the strings contains a multibyte char.
		 'string-as-unibyte
		 (delq nil
		       (list
			;; The request
			(or url-http-method "GET") " "
			(if using-proxy (url-recreate-url url-http-target-url) real-fname)
			" HTTP/" url-http-version "\r\n"
			;; Version of MIME we speak
			"MIME-Version: 1.0\r\n"
			;; (maybe) Try to keep the connection open
			"Connection: " (if (or using-proxy
					       (not url-http-attempt-keepalives))
					   "close" "keep-alive") "\r\n"
					   ;; HTTP extensions we support
			(if url-extensions-header
			    (format
			     "Extension: %s\r\n" url-extensions-header))
			;; Who we want to talk to
			(if (/= (url-port url-http-target-url)
				(url-scheme-get-property
				 (url-type url-http-target-url) 'default-port))
			    (format
			     "Host: %s:%d\r\n" host (url-port url-http-target-url))
			  (format "Host: %s\r\n" host))
			;; Who its from
			(if url-personal-mail-address
			    (concat
			     "From: " url-personal-mail-address "\r\n"))
			;; Encodings we understand
			(if url-mime-encoding-string
			    (concat
			     "Accept-encoding: " url-mime-encoding-string "\r\n"))
			(if url-mime-charset-string
			    (concat
			     "Accept-charset: " url-mime-charset-string "\r\n"))
			;; Languages we understand
			(if url-mime-language-string
			    (concat
			     "Accept-language: " url-mime-language-string "\r\n"))
			;; Types we understand
			"Accept: " (or url-mime-accept-string "*/*") "\r\n"
			;; User agent
			(url-http-user-agent-string)
			;; Proxy Authorization
			proxy-auth
			;; Authorization
			auth
			;; Cookies
			(url-cookie-generate-header-lines host real-fname
							  (equal "https" (url-type url-http-target-url)))
			;; If-modified-since
			(if (and (not no-cache)
				 (member url-http-method '("GET" nil)))
			    (let ((tm (url-is-cached url-http-target-url)))
			      (if tm
				  (concat "If-modified-since: "
					  (url-get-normalized-date tm) "\r\n"))))
			;; Whence we came
			(if ref-url (concat
				     "Referer: " ref-url "\r\n"))
			extra-headers
			;; Length of data
			(if url-http-data
			    (concat
			     "Content-length: " (number-to-string
						 (length url-http-data))
			     "\r\n"))
			;; End request
			"\r\n"
			;; Any data
			url-http-data "\r\n"))
		 ""))
	  (url-http-debug "Request is: \n%s" request)
	  request))))

(unless (fboundp 'mm-url-encode-multipart-form-data)
  (defun mm-url-encode-multipart-form-data (pairs &optional boundary)
    "Return PAIRS encoded in multipart/form-data."
    ;; RFC1867

    ;; Get a good boundary
    (unless boundary
      (setq boundary (mml-compute-boundary '())))

    (concat

     ;; Start with the boundary
     "--" boundary "\r\n"

     ;; Create name value pairs
     (mapconcat
      'identity
      ;; Delete any returned items that are empty
      (delq nil
            (mapcar (lambda (data)

                      (cond
                       ((consp (car data))
                        (let ((fieldname (cadar data))
                              (filename  (caadar data))
                              (mimetype  (car (caadar data)))
                              (content   (caar (caadar data))))

                          (concat
                           ;; Encode the name
                           "Content-Disposition: form-data; name=\"" fieldname "\"\r\n"
                           "Content-Type: " mimetype "\r\n"
                           "Content-Transfer-Encoding: binary\r\n\r\n"
                           content
                           "\r\n")))

                       ((stringp (car data))
                                ;; For each pair

                        (concat
                         ;; Encode the name
                         "Content-Disposition: form-data; name=\""
                         (car data) "\"\r\n"
                         "Content-Type: text/plain; charset=utf-8\r\n"
                         "Content-Transfer-Encoding: binary\r\n\r\n"

                         (cond ((stringp (cdr data))
                                (cdr data))
                               ((integerp (cdr data))
                                (int-to-string (cdr data))))

                         "\r\n"))
                       (t (error "I don't handle this."))))
                    pairs))
      ;; use the boundary as a separator
      (concat "--" boundary "\r\n"))

     ;; put a boundary at the end.
     "--" boundary "--\r\n")))

;; Not defined in my Xemacs
(unless (fboundp 'assoc-string)
  (defun assoc-string (key list &optional case-fold)
    (if case-fold
        (assoc-ignore-case key list)
      (assoc key list))))

(defun url-compat-retrieve (url post-process bufname callback cbargs)
  (cond ((boundp 'url-be-asynchronous) ; Sniff w3 lib capability
	 (if callback
	     (setq url-be-asynchronous t)
	   (setq url-be-asynchronous nil))
	 (url-retrieve url t)
	 (when (not url-be-asynchronous)
	   (let ((result (funcall post-process bufname)))
	     result)))
	(t (if callback
	       (url-retrieve url post-process
			     (list bufname callback cbargs))
	     (with-current-buffer (url-retrieve-synchronously url)
	       (funcall post-process bufname))))))

(defvar url-http-get-post-process 'url-http-response-post-process)
(defun url-http-get (url &optional headers bufname callback cbargs)
  "Convenience method to use method 'GET' to retrieve URL"
  (let* ((url-request-extra-headers (if headers headers
                                      (if url-request-extra-headers
                                          url-request-extra-headers
                                        (cons nil nil))))
         (url-request-method "GET"))

    (when (url-basic-auth url)
      (add-to-list 'url-request-extra-headers
                   (cons "Authorization" (url-basic-auth url))))
    (url-compat-retrieve url url-http-get-post-process
			 bufname callback cbargs)))

(defvar url-http-post-post-process 'url-http-response-post-process)
(defun url-http-post (url parameters &optional multipart headers bufname
                          callback cbargs)
  "Convenience method to use method 'POST' to retrieve URL"

  (let* ((url-request-extra-headers
          (if headers headers
            (when url-request-extra-headers url-request-extra-headers)))
         (boundary (int-to-string (random)))
         (cs 'utf-8)
         (content-type
          (if multipart
              (concat "multipart/form-data, boundary=" boundary)
            (format "application/x-www-form-urlencoded; charset=%s" cs)))
         (url-request-method "POST")
         (url-request-coding-system cs)
         (url-request-data
          (if multipart
              (mm-url-encode-multipart-form-data
               parameters boundary)
            (mm-url-encode-www-form-urlencoded (delq nil parameters)))))
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (val (cdr pair)))
         (if (assoc key url-request-extra-headers)
             (setcdr (assoc key url-request-extra-headers) val)
           (add-to-list 'url-request-extra-headers
                        (cons key val)))))
     (list
      (cons "Connection" "close")
      (cons "Content-Type" content-type)))

    (message "Posting to: %s" url)
    (url-compat-retrieve url url-http-post-post-process
			 bufname callback cbargs)))

(defun url-http-response-post-process (status &optional bufname
                                              callback cbargs)
  "Post process on HTTP response."
  (declare (special url-http-end-of-headers))
  (let ((kill-this-buffer (current-buffer)))
    (when (and (integerp status) (not (< status 300)))
      (kill-buffer kill-this-buffer)
      (error "Oops! Invalid status: %d" status))

    (when (or (not (boundp 'url-http-end-of-headers))
              (not url-http-end-of-headers))
      (kill-buffer kill-this-buffer)
      (error "Oops! Don't see end of headers!"))

    ;; FIXME: need to limit redirects
    (if (and (listp status)
             (eq (car status) :redirect))
        (progn
          (message (concat "Redirecting to " (cadr status)))
          (url-http-get (cadr status) nil bufname callback cbargs))

      (goto-char url-http-end-of-headers)
      (forward-line)

      (let ((str (decode-coding-string
                  (buffer-substring-no-properties (point) (point-max))
                  'utf-8)))
        (kill-buffer (current-buffer))
        (when bufname
          (set-buffer bufname)
          (insert str)
          (goto-char (point-min))
          (set-buffer-modified-p nil))
        (when callback
          (apply callback (list str cbargs)))
        (when (not (or callback bufname))
          str)))))

(defgroup mediawiki nil
  "A mode for editting pages on MediaWiki sites."
  :tag "MediaWiki"
  :group 'applications)

(defcustom mediawiki-site-default "Wikipedia"
  "The default mediawiki site to point to.  Set here for the
default and use `mediawiki-site' to set it per-session
later."
  :type 'string
  :tag "MediaWiki Site Default"
  :group 'mediawiki)

(defcustom mediawiki-site-alist '(("Wikipedia"
                                   "http://en.wikipedia.org/w/"
                                   "username"
                                   "password"
				   "Main Page"))
  "A list of MediaWiki websites."
  :group 'mediawiki
  :type '(alist :tag "Site Name"
                :key-type (string :tag "Site Name")
                :value-type (list :tag "Parameters"
                                  (string :tag "URL")
                                  (string :tag "Username")
                                  (string :tag "Password")
                                  (string :tag "First Page"
                                          :description "First page to open when `mediawiki-site' is called for this site"))))

(defcustom mediawiki-pop-buffer-hook '()
  "List of functions to execute after popping to a buffer.  Can
be used to to open the whole buffer."
  :options '(delete-other-windows)
  :type 'hook
  :group 'mediawiki)

(defvar mediawiki-page-history '()
  "Assoc list of visited pages on this MW site.")

(defvar mediawiki-enumerate-with-terminate-paragraph nil
"*Before insert enumerate/itemize do \\[mediawiki-terminate-paragraph].")

(defvar mediawiki-english-or-german t
  "*Variable in order to set the english (t) or german (nil) environment.")

(defvar mediawiki-user-simplify-signature t
  "*Simple varible in order to threat complicated signatures of users, which uses
fonts and other makeup.")

(defgroup mediawiki-draft nil
  "A mode to mediawiki-draft information."
  :group 'mediawiki)

;;; User Variables:

(defcustom mediawiki-draft-mode-hook nil
  "*Functions run upon entering mediawiki-draft-mode."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-register ?R
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-filter-functions nil
  "*Functions run to filter mediawiki-draft data.
All functions are run in the mediawiki-draft buffer."
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-handler-functions '(mediawiki-draft-append-to-file)
  "*Functions run to process mediawiki-draft data.
Each function is called with the current buffer narrowed to what the
user wants mediawiki-drafted.
If any function returns non-nil, the data is assumed to have been
recorded somewhere by that function. "
  :type 'hook
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-data-file "~/Wiki/discussions/draft.wiki"
  "*The file in which to store the wikipedia drafts."
  :type 'file
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-reply-register ?M
  "The register in which the window configuration is stored."
  :type 'character
  :group 'mediawiki-draft)

(defcustom mediawiki-draft-page ?S		;Version:1.37
  "The register in which the a page of the wiki draft file is stored."
  :type 'character
  :group 'mediawiki-draft)


(defcustom mediawiki-draft-leader-text "== "
  "*The text used to begin each mediawiki-draft item."
  :type 'string
  :group 'mediawiki-draft)

(defvar mediawiki-reply-with-hline nil
"*Whether to use a hline as a header seperator in the reply.")

(defvar mediawiki-reply-with-quote nil
  "*Whether to use a quotation tempalate or not.")

(defvar mediawiki-imenu-generic-expression
  (list '(nil "^==+ *\\(.*[^\n=]\\)==+" 1))
  "Imenu expression for `mediawiki-mode'.  See `imenu-generic-expression'.")

(defvar mediawiki-login-success "pt-logout"
  "A string that should be present on login success on all
mediawiki sites.")

(defvar mediawiki-permission-denied
  "[^;]The action you have requested is limited"
  "A string that will indicate permission has been denied, Note
that it should not match the mediawiki.el file itself since it
is sometimes put on MediaWiki sites.")

(defvar mediawiki-view-source
  "ca-viewsource"
  "A string that will indicate you can only view source on this
page.")

(defvar mediawiki-site nil
  "The current mediawiki site from `mediawiki-site-alist'.  If
not set, defaults to `mediawiki-site-default'.")

(defvar mediawiki-argument-pattern "?title=%s&action=%s"
  "Format of the string to append to URLs.  Two string arguments
are expected: first is a title and then an action.")

(defvar mediawiki-URI-pattern
  "http://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?/"
  "Pattern matching a URI like this:
	http://mediawiki.sf.net/index.php
Password not support yet")

(defvar mediawiki-page-uri nil
  "The URI of the page corresponding to the current buffer, thus defining
the base URI of the wiki engine as well as group and page name.")

(defvar mediawiki-page-title nil
  "The title of the page corresponding to the current buffer")

(defvar mediawiki-edittoken nil
  "The edit token for this page.")
(defvar mediawiki-starttimestamp nil
  "The starttimestamp for this page.")
(defvar mediawiki-basetimestamp nil
  "The base timestamp for this page.")

(defvar mediawiki-page-ring nil
  "Ring that holds names of buffers we navigate through.")

(defvar mediawiki-page-ring-index 0)

(defvar font-mediawiki-sedate-face 'font-mediawiki-sedate-face
  "Face to use for mediawiki  minor keywords.")
(defvar font-mediawiki-italic-face 'font-mediawiki-italic-face
  "Face to use for mediawiki italics.")
(defvar font-mediawiki-bold-face 'font-mediawiki-bold-face
  "Face to use for mediawiki bolds.")
(defvar font-mediawiki-math-face 'font-mediawiki-math-face
  "Face to use for mediawiki math environments.")
(defvar font-mediawiki-string-face 'font-mediawiki-string-face
  "Face to use for strings.")
(defvar font-mediawiki-verbatim-face 'font-mediawiki-verbatim-face
  "Face to use for text in verbatim macros or environments.")

(defface font-mediawiki-bold-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in bold."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-italic-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "DarkOliveGreen" ,@font))
      (((class color) (background dark))
       (:foreground "OliveDrab" ,@font))
      (t (,@font))))
  "Face used to highlight text to be typeset in italic."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-math-face
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-sedate-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark))  (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "DimGray"))
    (((class color) (background dark))  (:foreground "LightGray"))
   ;;;(t (:underline t))
    )
  "Face used to highlight sedate stuff."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-string-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit italic))
		    ((assq :slant custom-face-attributes) '(:slant italic))
		    (t '(:italic t)))))
    `((((type tty) (class color))
       (:foreground "green"))
      (((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "RosyBrown"))
      (((class color) (background dark))
       (:foreground "LightSalmon"))
      (t (,@font))))
  "Face used to highlight strings."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-warning-face
  (let ((font (cond ((assq :inherit custom-face-attributes) '(:inherit bold))
		    ((assq :weight custom-face-attributes) '(:weight bold))
		    (t '(:bold t)))))
    `((((class grayscale)(background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale)(background dark))
       (:foreground "LightGray" ,@font))
      (((class color)(background light))
       (:foreground "red" ,@font))
      (((class color)(background dark))
       (:foreground "red" ,@font))
      (t (,@font))))
  "Face for important keywords."
  :group 'font-mediawiki-highlighting-faces)

(defface font-mediawiki-verbatim-face
  (let ((font (if (and (assq :inherit custom-face-attributes)
		       (if (fboundp 'find-face)
			   (find-face 'fixed-pitch)
			 (facep 'fixed-pitch)))
		  '(:inherit fixed-pitch)
		'(:family "courier"))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown" ,@font))
      (((class color) (background dark))
       (:foreground "burlywood" ,@font))
      (t (,@font))))
  "Face used to highlight TeX verbatim environments."
  :group 'font-mediawiki-highlighting-faces)

(defvar mediawiki-simple-tags
  '("b" "big" "blockquote" "br" "caption" "code" "center" "cite" "del"
    "dfn" "dl" "em" "i" "ins" "kbd" "math" "nowiki" "ol" "pre" "samp"
    "small" "strike" "strong" "sub" "sup" "tt" "u" "ul" "var")
  "Tags that do not accept arguments.")

(defvar mediawiki-complex-tags
  '("a" "div" "font" "table" "td" "th" "tr")
  "Tags that accept arguments.")

(defvar mediawiki-url-protocols
  '("ftp" "gopher" "http" "https" "mailto" "news")
  "Valid protocols for URLs in Wikipedia articles.")

(defvar mediawiki-draft-buffer "*MW-Draft*"
  "The name of the wikipedia-draft (temporary) data entry buffer.")

(defvar mediawiki-edit-form-vars nil)

(defvar mediawiki-font-lock-keywords
  (list

   ;; Apostrophe-style text markup
   (cons "''''\\([^']\\|[^']'\\)*?\\(''''\\|\n\n\\)"
         'font-lock-builtin-face)
   (cons "'''\\([^']\\|[^']'\\)*?\\('''\\|\n\n\\)"
                                        ;'font-lock-builtin-face)
         'font-mediawiki-bold-face)
   (cons "''\\([^']\\|[^']'\\)*?\\(''\\|\n\n\\)"
         'font-mediawiki-italic-face)

   ;; Headers and dividers
   (list "^\\(==+\\)\\(.*\\)\\(\\1\\)"
         '(1 font-lock-builtin-face)
                                        ;'(2 mediawiki-header-face)
         '(2 font-mediawiki-sedate-face)
         '(3 font-lock-builtin-face))
   (cons "^-----*" 'font-lock-builtin-face)

   ;; Bare URLs and ISBNs
   (cons (concat "\\(^\\| \\)" (regexp-opt mediawiki-url-protocols t)
                 "://[-A-Za-z0-9._\/~%+&#?!=()@]+")
         'font-lock-variable-name-face)
   (cons "\\(^\\| \\)ISBN [-0-9A-Z]+" 'font-lock-variable-name-face)

   ;; Colon indentation, lists, definitions, and tables
   (cons "^\\(:+\\|[*#]+\\||[}-]?\\|{|\\)" 'font-lock-builtin-face)
   (list "^\\(;\\)\\([^:\n]*\\)\\(:?\\)"
         '(1 font-lock-builtin-face)
         '(2 font-lock-keyword-face)
         '(3 font-lock-builtin-face))

   ;; Tags and comments
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-simple-tags t) "\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-builtin-face t t))
   (list (concat "\\(</?\\)"
                 (regexp-opt mediawiki-complex-tags t)
                 "\\(\\(?: \\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*\\)?\\)\\(>\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-function-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))
   (cons (concat "<!-- \\([^->]\\|>\\|-\\([^-]\\|-[^>]\\)\\)*-->")
         '(0 font-lock-comment-face t t))

   ;; External Links
   (list (concat "\\(\\[\\)\\(\\(?:"
                 (regexp-opt mediawiki-url-protocols)
                 "\\)://[-A-Za-z0-9._\/~%-+&#?!=()@]+\\)\\(\\(?: [^]\n]*\\)?\\)\\(\\]\\)")
         '(1 font-lock-builtin-face t t)
         '(2 font-lock-variable-name-face t t)
         '(3 font-lock-keyword-face t t)
         '(4 font-lock-builtin-face t t))

   ;; Wiki links
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-keyword-face t t)
     (5 font-lock-builtin-face t t))

   ;; Semantic relations
   '("\\(\\[\\[\\)\\([^]\n|]*\\)\\(::\\)\\([^]\n|]*\\)\\(|?\\)\\([^]\n]*\\)\\(\\]\\]\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t)
     (4 font-lock-constant-face t t)
     (5 font-lock-builtin-face t t)
     (6 font-lock-keyword-face t t)
     (7 font-lock-builtin-face t t))

   ;; Wiki variables
   '("\\({{\\)\\(.+?\\)\\(}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Semantic variables
   '("\\({{{\\)\\(.+?\\)\\(}}}\\)"
     (1 font-lock-builtin-face t t)
     (2 font-lock-variable-name-face t t)
     (3 font-lock-builtin-face t t))

   ;; Character entity references
   (cons "&#?[a-zA-Z0-9]+;" '(0 font-lock-type-face t t))

   ;; Preformatted text
   (cons "^ .*$" '(0 font-lock-constant-face t t))

   ;; Math environment (uniform highlight only, no TeX markup)
   (list "<math>\\(\\(\n?.\\)*\\)</math>"
         '(1 font-lock-keyword-face t t))))

(defvar mediawiki-draft-send-archive t
  "*Archive the reply.")

(defvar mediawiki-draft-mode-map ())

(defun mediawiki-translate-pagename (name)
  "Given NAME, returns the typical name that MediaWiki would use.
Right now, this only means replacing \"_\" with \" \"."
  (if (not name)
      "Main Page"
    (mapconcat 'identity (split-string name "_" t) " ")))

(defun mediawiki-make-api-url (&optional sitename)
  (format (concat (mediawiki-site-url (or sitename mediawiki-site))
                  "api.php")))

(defun mediawiki-api-call (sitename action args)
  (let* ((raw (url-http-post (mediawiki-make-api-url sitename)
                        (delq nil
                              (append args (list (cons "format" "xml")
                                                 (cons "action" action))))
                        (string= action "upload")))
         (result (assoc 'api
                            (with-temp-buffer
                              (insert raw)
                              (xml-parse-region (point-min) (point-max))))))
    (unless result
      (error "There was an error parsing the result of the API call"))

    (when (assq 'error (cddr result))
      (let* ((err (cadr (assq 'error (cddr result))))
             (err-code (cdr (assq 'code err)))
             (err-info (cdr (assq 'info err))))
        (error "The server encountered an error: (%s) %s" err-code err-info)))


    (if (cddr result)
        (let ((action-res (assq (intern action) (cddr result))))
          (unless action-res
            (error "Didn't see action name in the result list"))

          action-res)
      t)))

(defun mediawiki-make-url (title action &optional sitename)
  (format (concat (mediawiki-site-url (or sitename mediawiki-site))
                  (if action
                      mediawiki-argument-pattern
                    "?title=%s"))
	  (mm-url-form-encode-xwfu
           (mediawiki-translate-pagename title))
	  action))

(defun mediawiki-open (name)
  "Open a wiki page specified by NAME from the mediawiki engine"
  (interactive
   (let ((hist (cdr (assoc-string mediawiki-site mediawiki-page-history))))
     (list (read-string "Wiki Page: " nil 'hist))))
  (when (or (not (stringp name))
            (string-equal "" name))
    (error "Need to specify a name"))
  (mediawiki-edit mediawiki-site name))

(defun mediawiki-reload ()
  (interactive)
  (let ((title mediawiki-page-title))
    (if title
	(mediawiki-open title)
      (error "Error: %s is not a mediawiki document" (buffer-name)))))

(defun mediawiki-add-page-history (site title)
  (let ((hist (cdr (assoc-string site mediawiki-page-history))))
    (unless hist
      (add-to-list 'mediawiki-page-history (cons site "")))
    (setcdr (assoc-string site mediawiki-page-history) (append (list title) hist))))

(defun mediawiki-edit (site title)
  "Edit wiki file with the name of title"
  (when (not (ring-p mediawiki-page-ring))
    (setq mediawiki-page-ring (make-ring 30)))

  (let ((pagetitle (mediawiki-translate-pagename title)))

    (mediawiki-add-page-history site title)
    (with-current-buffer (get-buffer-create
                          (concat site ": " pagetitle))
      (unless (mediawiki-logged-in-p site)
        (mediawiki-do-login site)
        (setq mediawiki-site site))
      (ring-insert mediawiki-page-ring (current-buffer))
      (delete-region (point-min) (point-max))
      (mediawiki-mode)
      (set-buffer-file-coding-system 'utf-8)
      (insert (or (mediawiki-get site pagetitle) ""))

      (set-buffer-modified-p nil)
      (setq buffer-undo-list t)
      (buffer-enable-undo)
      (mediawiki-pop-to-buffer (current-buffer))
      (setq mediawiki-page-title pagetitle)
      (goto-char (point-min))
      (current-buffer))))

(defun mediawiki-get-edit-form-vars (str bufname)
  "Extract the form variables from a page.  This should only be
called from a buffer in mediawiki-mode as the variables it sets
there will be local to that buffer."

  (let ((args (mediawiki-get-form-vars str "id" "editform")))
    (if args
	(with-current-buffer bufname
	  (setq mediawiki-edit-form-vars args))
      (cond
       ((string-match mediawiki-permission-denied str)
	(message "Permission Denied"))
       ((string-match mediawiki-view-source str)
	(message "Editing of this page is disabled, here is the source"))))))

(defun mediawiki-get-form-vars (str attr val)
  ;; Find the form
  (when (string-match
         (concat "<form [^>]*" attr "=[\"']" val "['\"][^>]*>")
         str)

    (let* ((start-form (match-end 0))
           (end-form (when (string-match "</form>" str start-form)
                       (match-beginning 0)))
           (form (substring str start-form end-form))
           (start (string-match
                   "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>"
                   form))
           (vars '(nil)))

      ;; Continue until we can't find any more input elements
      (while start

        ;; First, capture the place where we'll start next.  Have
        ;; to do this here since match-end doesn't seem to let you
        ;; specify the string you were matching against, unlike
        ;; match-string
        (setq start (match-end 0))

        ;; Capture the string that defines this element
        (let ((el (match-string 1 form))
              ;; get the element name
              (el-name (match-string 2 form)))

          ;; figure out if this is a submit button and skip it if it is.
          (when (not (string-match "type=[\"']submit['\"]" el))
            (add-to-list 'vars
                         (if (string-match "value=[\"']\\([^\"']*\\)['\"]" el)
                             (cons el-name (match-string 1 el))
                           (cons el-name nil)))))

        (setq start
              (string-match
               "<input \\([^>]*name=[\"']\\([^\"']+\\)['\"][^>]*\\)>"
               form start)))
      vars)))

(defun mediawiki-logged-in-p (&optional site)
  "Returns t if we are we have cookies for the site."
  (let ((urlobj (url-generic-parse-url
                 (mediawiki-site-url (or site mediawiki-site)))))
    (url-cookie-retrieve
     (url-host urlobj)
     (url-filename urlobj)
     (equal "https" (url-type urlobj)))))

(defun mediawiki-pop-to-buffer (bufname)
  "Pop to buffer and then execute a hook."
  (pop-to-buffer bufname)
  (run-hooks 'mediawiki-pop-buffer-hook))

(defun mediawiki-api-param (v)
  "Concat a list into a bar-separated string, turn an integer
into a string, or just return the string"
  (cond
   ((integerp v) (int-to-string v))
   ((stringp v) v)
   ((listp v) (mapconcat 'identity v "|"))
   (t (error "Don't know what to do with %s" v))))

(defun mediawiki-api-query-revisions (site titles props &optional limit)
  "Get a list of revisions and properties for a given page."
  (cddr (mediawiki-api-call site "query"
                      (list (cons "prop" (mediawiki-api-param (list "info" "revisions")))
                            (cons "intoken" (mediawiki-api-param "edit"))
                            (cons "titles" (mediawiki-api-param titles))
                            (when limit
                              (cons "rvlimit" (mediawiki-api-param limit)))
                            (cons "rvprop" (mediawiki-api-param props))))))

(defun mediawiki-page-get-title (page)
  "Given a page from a pagelist structure, extract the title."
  (cdr (assq 'title (cadr page))))

(defun mediawiki-page-get-revision (page rev &optional bit)
  "Extract a revision from the pagelist structure."
  (let ((rev (cdr (nth rev (cddr (assq 'revisions (cddr page)))))))
    (cond
     ((eq bit 'content)
      (cadr rev))
     ((assoc bit (car rev))
      (cdr (assoc bit (car rev))))
     (t rev))))

(defun mediawiki-pagelist-find-page (pagelist title)
  "Extract a page TITLE from a PAGELIST returned by mediawiki"
  (let ((pl (cddr (assq 'pages pagelist)))
        page current)
    (while (and (not page)
                (setq current (pop pl)))
      ;; This fails when underbars are here instead of spaces,
      ;; so we make sure that it has the mediawiki pagename
      (when (string= (mediawiki-page-get-title current)
                     (mediawiki-translate-pagename title))
        (setq page current)))
    page))

(defun mediawiki-api-query-title (site title)
  "Retrieve the current content of the page."
  (let* ((pagelist (mediawiki-api-query-revisions
                    site title
                    (list "ids" "timestamp" "flags" "comment" "user" "content"))))
    (mediawiki-pagelist-find-page pagelist title)))

(defun mediawiki-get (site title)
  (let ((page (mediawiki-api-query-title site title)))
    (mediawiki-save-metadata site page)
    (mediawiki-page-get-revision page 0 'content)))

(defun mediawiki-page-get-metadata (page item)
  (cdr (assoc item (cadr page))))

(defun mediawiki-save-metadata (site page)
  (setq mediawiki-site site)
  (setq mediawiki-page-title
        (mediawiki-page-get-metadata page 'title))
  (setq mediawiki-edittoken
        (mediawiki-page-get-metadata page 'edittoken))
  (setq mediawiki-basetimestamp
        (mediawiki-page-get-revision page 0 'timestamp))
  (setq mediawiki-starttimestamp
        (mediawiki-page-get-metadata page 'starttimestamp)))

(defun mediawiki-save (&optional summary)
  (interactive "sSummary: ")
  (if mediawiki-page-title
      (mediawiki-save-page
       mediawiki-site
       mediawiki-page-title
       summary
       (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document" (buffer-name))))

(defun mediawiki-prompt-for-page ()
    (let* ((prompt (concat "Page"
                         (when mediawiki-page-title
                           (format " (default %s)" mediawiki-page-title))
                         ": "))
         (answer (completing-read prompt '())))
    (if (string= "" answer)
        mediawiki-page-title
      answer)))

(defun mediawiki-prompt-for-summary ()
    (completing-read  "Summary: " '()))

(defun mediawiki-save-on (&optional site name summary)
  (interactive)
  (when (not site)
    (setq site (mediawiki-prompt-for-site)))
  (when (not name)
    (setq name (mediawiki-translate-pagename (mediawiki-prompt-for-page))))
  (when (not summary)
    (setq summary (mediawiki-prompt-for-summary)))

  (setq mediawiki-site (mediawiki-do-login site))
  (mediawiki-get mediawiki-site name)
  (mediawiki-save-as name summary))

(defun mediawiki-save-as (&optional name summary)
  (interactive "sSave As: \nsSummary: ")
  (if name
      (mediawiki-save-page
       mediawiki-site
       name
       summary
       (buffer-substring-no-properties (point-min) (point-max)))
    (error "Error: %s is not a mediawiki document" (buffer-name))))

(defun mediawiki-save-and-bury (&optional summary)
  (interactive "sSummary: ")
  (mediawiki-save summary)
  (bury-buffer))

(defun mediawiki-site-extract (sitename index)
  (let ((bit (nth index (assoc sitename mediawiki-site-alist))))
    (cond
     ((eq nil sitename)
      (error "Sitename isn't set"))
     ((eq nil bit)
      (error "Couldn't find a site named: %s" sitename))
     ((string-match "[^ \t\n]" bit) bit)
     (nil))))

(defun mediawiki-site-url (sitename)
  "Get the url for a given site."
  (mediawiki-site-extract sitename 1))

(defun mediawiki-site-username (sitename)
  "Get the username for a given site."
  (or (mediawiki-site-extract sitename 2)
      (url-user-for-url (mediawiki-site-url sitename))))

(defun mediawiki-site-password (sitename)
  "Get the password for a given site."
  (or (mediawiki-site-extract sitename 3)
      (url-password-for-url (mediawiki-site-url sitename))))

(defun mediawiki-site-first-page (sitename)
  "Get the password for a given site."
  (mediawiki-site-extract sitename 4))

(defun mediawiki-do-login (&optional sitename username password)
  "Use USERNAME and PASSWORD to log into the MediaWiki site and
get a cookie."
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (setq mediawiki-site nil)             ; This wil be set once we are
                                        ; logged in

  ;; Possibly save info once we have it, eh?
  (lexical-let* ((user (or (mediawiki-site-username sitename)
                           username
                           (read-string "Username: ")))
                 (pass (or (mediawiki-site-password sitename)
                           password
                           (read-passwd "Password: ")))
                 (sitename sitename)
                 (args (list (cons "lgname" user)
                             (cons "lgpassword" pass)))
                 (result (cadr (mediawiki-api-call sitename "login" args))))
    (when (string= (cdr (assq 'result result)) "NeedToken")
      (setq result
            (cadr (mediawiki-api-call
                   sitename "login"
                   (append
                    args (list (cons "lgtoken"
                                     (cdr (assq 'token result)))))))))
    (when (string= "Success" (cdr (assoc 'result result)))
      sitename)))

(defun mediawiki-do-logout (&optional sitename)
  (interactive)
  (when (not sitename)
    (setq sitename (mediawiki-prompt-for-site)))

  (mediawiki-api-call sitename "logout" nil)
  (setq mediawiki-site nil))

(defun mediawiki-save-page (site title summary content)
  "Save the current page to a MediaWiki wiki."
  ;; FIXME error checking, conflicts!
  (mediawiki-api-call site "edit" (list (cons "title"
                                              (mediawiki-translate-pagename title))
                                        (cons "text" content)
                                        (cons "summary" summary)
                                        (cons "token" mediawiki-edittoken)
                                        (cons "basetimestamp"
                                              (or mediawiki-basetimestamp ""))
                                        (cons "starttimestamp"
                                              (or mediawiki-starttimestamp ""))))
  (set-buffer-modified-p nil))

;; (cdr (assoc 'edittoken (cadr (caddr (caddr (mediawiki-api-call "mw-svn" "query"
;;                                                                (list '("prop" . "info")
;;                                                                      '("intoken" . "edit")
;;                                                                      '("titles" . (concat "File:" filename)))))))))
;
;(mediawiki-api-call "mw-svn" "upload" (list '("filename" . "info.exe") '("file" . "edit") '("token" . token)))

(defun mediawiki-browse (&optional buf)
  "Open the buffer BUF in a browser. If BUF is not given,
the current buffer is used."
  (interactive)
  (if mediawiki-page-title
      (browse-url (mediawiki-make-url mediawiki-page-title "view"))))

(defun mediawiki-prompt-for-site ()
  (let* ((prompt (concat "Sitename"
                         (when mediawiki-site
                           (format " (default %s)" mediawiki-site))
                         ": "))
         (answer (completing-read prompt mediawiki-site-alist nil t)))
    (if (string= "" answer)
        mediawiki-site
      answer)))

(defun mediawiki-site (&optional site)
  "Set up mediawiki.el for a site.  Without an argument, use
`mediawiki-site-default'.  Interactively, prompt for a site."
  (interactive)
  (when (not site)
    (setq site (mediawiki-prompt-for-site)))
  (when (or (eq nil mediawiki-site)
            (not (string-equal site mediawiki-site)))
    (setq mediawiki-site (mediawiki-do-login site)))
  (mediawiki-edit site (mediawiki-site-first-page site)))

(defun mediawiki-open-page-at-point ()
  "Open a new buffer with the page at point."
  (interactive)
  (mediawiki-open (mediawiki-page-at-point)))

(defun mediawiki-page-at-point ()
  "Return the page name under point.  Typically, this means
anything enclosed in [[PAGE]]."
  (let ((pos (point))
        (eol (point-at-eol))
        (bol (point-at-bol)))
    (save-excursion
      (let* ((start  (when (search-backward "[[" bol t)
                       (+ (point) 2)))
             (end    (when (search-forward "]]" eol t)
                       (- (point) 2)))
             (middle (progn
                       (goto-char start)
                       (when (search-forward  "|" end t)
                         (1- (point)))))
             (pagename (when (and
                              (not (eq nil start))
                              (not (eq nil end))
                              (<= pos end)
                              (>= pos start))
                         (buffer-substring-no-properties
                          start (or middle end)))))
        (if (string= "/"
                     (substring pagename 0 1))
            (concat mediawiki-page-title pagename)
          pagename)))))

(defun mediawiki-next-header ()
  "Move point to the end of the next section header."
  (interactive)
  (let ((oldpoint (point)))
    (end-of-line)
    (if (re-search-forward "\\(^==+\\).*\\1" (point-max) t)
        (beginning-of-line)
      (goto-char oldpoint)
      (message "No section headers after point."))))

(defun mediawiki-prev-header ()
  "Move point to the start of the previous section header."
  (interactive)
  (unless (re-search-backward "\\(^==+\\).*\\1" (point-min) t)
    (message "No section headers before point.")))

(defun mediawiki-terminate-paragraph ()	;Version:1.58
  "In a list, start a new list item. In a paragraph, start a new
paragraph; if the current paragraph is colon indented, the new
paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\|[#*]+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline) (if (not indent-chars) (newline)
		(insert indent-chars))))

(defun mediawiki-terminate-paragraph-and-indent ()
  "In a list, start a new list item. In a paragraph, start a new
paragraph but *,# will be ignored; if the current paragraph is colon
; indented, the new paragraph will be indented in the same way."
  (interactive)
  (let (indent-chars)
    (save-excursion
      (beginning-of-line)
      (while (cond ((looking-at "^$") nil)
                   ((looking-at "^\\(\\(?: \\|:+\\) *\\)")
                    (setq indent-chars (match-string 1)) nil)
                   ((eq (point) (point-min)) nil)
                   ((progn (forward-line -1) t)))
        t))
    (newline)
    (if (not indent-chars) (newline)
      (insert indent-chars))))


(defun mediawiki-link-fill-nobreak-p ()
  "When filling, don't break the line for preformatted (fixed-width)
text or inside a Wiki link.  See `fill-nobreak-predicate'."
  (save-excursion
    (let ((pos (point)))
      (or (eq (char-after (line-beginning-position)) ? )
          (if (re-search-backward "\\[\\[" (line-beginning-position) t)
              ;; Break if the link is really really long.
              ;; You often get this with captioned images.
              (null (or (> (- pos (point)) fill-column)
                        (re-search-forward "\\]\\]" pos t))))))))

(defun mediawiki-fill-article ()
  "Fill the entire article."
  (interactive)
  (save-excursion
    (fill-region (point-min) (point-max))))

(defun mediawiki-unfill-article ()
  "Undo filling, deleting stand-alone newlines (newlines that do not
end paragraphs, list entries, etc.)"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".\\(\n\\)\\([^# *;:|!\n]\\|----\\)" nil t)
      (replace-match " " nil nil nil 1)))
  (message "Stand-alone newlines deleted"))

(defun mediawiki-draft-reply ()
  "Open a temporary buffer in mediawiki mode for editing an
mediawiki draft, with an arbitrary piece of data. After finishing
the editing |]]:either use \"C-c C-k\" \\[mediawiki-draft-buffer]
to send the data into the mediawiki-draft-data-file, or send the
buffer \"C-c\C-c\", to the current article. Check the variable
mediawiki-draft-send-archive."
  (interactive)
  (mediawiki-reply-at-point-simple)
  (beginning-of-line 1)
  (kill-line nil)
  (save-excursion
	(window-configuration-to-register mediawiki-draft-register)
	(let ((buf (get-buffer-create mediawiki-draft-buffer)))
	  (switch-to-buffer-other-window buf)
	  (mediawiki-mode)
	  (if mediawiki-reply-with-quote
              (progn
		(insert "{{Quotation|")
		(yank)
		(insert "'''Re: ")
		(insert-register mediawiki-draft-reply-register 1)
		(insert "''' |~~~~}}")
		(backward-char 7))
            (when mediawiki-reply-with-hline
              (insert "----")
              (newline 1))
            (yank)
            (end-of-line 1))
	  (message " C-c C-k sends to draft, C-c C-c sends to org buffer."))))

(defun mediawiki-reply-at-point-simple ()
  "Very simple function to reply to posts in the discussion forum. You have to set
the point around the signature, then the functions inserts the following
:'''Re: [[User:foo]]'''."
  (interactive)
  (beginning-of-line 1)
  (if mediawiki-english-or-german
      (progn
        (search-forward "(UTC)")
        (search-backward "[[User:"))
    (search-forward "(CET)")
    (search-backward "[[Benutzer:"))
  (if mediawiki-user-simplify-signature
      (mark-word 2)
    (mark-word 3))
  (copy-to-register mediawiki-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph-and-indent)
  (insert ":'''Re: ")
  (insert-register mediawiki-draft-reply-register 1)
  (if mediawiki-user-simplify-signature
      (insert "|]]''' ")
    (insert "]]''' ")))

(defmacro mediawiki-goto-relative-page (direction)
  `(let ((buff (ring-ref mediawiki-page-ring
                        (setq mediawiki-page-ring-index
                              (,direction mediawiki-page-ring-index 1)))))
     (while (not (buffer-live-p buff))
       (setq buff
             (ring-ref mediawiki-page-ring
                       (setq mediawiki-page-ring-index
                             (,direction mediawiki-page-ring-index 1)))))
     (mediawiki-pop-to-buffer buff)))

(defun mediawiki-goto-previous-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page -))

(defun mediawiki-goto-next-page ()
  "Pop up the previous page being editted."
  (interactive)
  (mediawiki-goto-relative-page +))

(defun mediawiki-goto-relative-link (&optional backward)
  "Move point to a link.  If backward is t, will search backwards."
  (let* ((search (if backward 're-search-backward
                   're-search-forward))
         (limitfunc (if backward 'point-min
                      'point-max))
         (point (funcall search "\\[\\[.+\\]\\]" (funcall limitfunc) t)))
    (when point
      (let ((point (match-beginning 0)))
        (goto-char (+ point 2))))))

(defun mediawiki-goto-next-link ()
  (interactive)
  (mediawiki-goto-relative-link))

(defun mediawiki-goto-prev-link ()
  (interactive)
  (mediawiki-goto-relative-link t))

(defvar wikipedia-enumerate-with-terminate-paragraph nil
"*Before insert enumerate/itemize do \\[wikipedia-terminate-paragraph].")

(defun mediawiki-insert-enumerate ()
"Primitive Function for inserting enumerated items, check the
variable wikipedia-enumerate-with-terminate-paragraph. Note however
that the function \\[wikipedia-terminate-paragraph] does not work very
well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
      (progn
        (mediawiki-terminate-paragraph)
        (insert "#"))
    (newline nil)
    (insert ":#")))

(defun mediawiki-insert-itemize ()
  "Primitive Function for inserting no enumerated items, check
the variable mediawiki-enumerate-with-terminate-paragraph. Note
however that the function \\[mediawiki-terminate-paragraph] does
not work very well will longlines-mode."
  (interactive)
  (if mediawiki-enumerate-with-terminate-paragraph
      (progn
        (mediawiki-terminate-paragraph)
        (insert "*"))
    (newline nil)
    (insert ":*")))

(defun mediawiki-insert (pre post)
  (if (or (and (boundp 'zmacs-region-active-p) zmacs-region-active-p)
          (and (boundp 'transient-mark-mode) transient-mark-mode mark-active))
      (let ((beg (region-beginning))
            (end (region-end)))
        (save-excursion
          (goto-char beg)
          (insert pre)
          (goto-char (+ end (string-width pre)))
          (insert post)))
    (insert (concat pre " " post))
    (backward-char (+ 1 (string-width post)))))

(defun mediawiki-insert-strong-emphasis ()
  "Insert strong emphasis italics via four
apostrophes (e.g. ''''FOO''''.) When mark is active, surrounds
region."
  (interactive)
  (mediawiki-insert "''''" "''''"))

(defun mediawiki-insert-bold ()
  "Insert bold via three apostrophes (e.g. '''FOO'''.)
When mark is active, surrounds region."
  (interactive)
  (mediawiki-insert "'''" "'''"))


(defun mediawiki-insert-italics ()
  "Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "''" "''"))

(defun mediawiki-insert-quotation-with-signature ()
  "Insert bold via TWO apostrophes (e.g. ''FOO''.) When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "{{Quotation|}}" "{{~~~~}}"))

(defun mediawiki-insert-quotation ()
  "Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."
  (interactive)
  (mediawiki-insert "{{Quotation|}}{{" "}}"))

(defun mediawiki-insert-bible-verse-template ()
  "Insert a template for the quotation of bible verses."
  (interactive)
  (insert "({{niv|")
  (let ((name    (read-string "Name: ")))
    (insert (concat name "|"))
    (let ((verse (read-string "Verse: ")))
      (insert (concat verse "|" name " " verse "}})")))))

(defun mediawiki-insert-user ()
  "Inserts, interactively a user name [[User:foo]]"
  (interactive)
  (if mediawiki-english-or-german
      (let ((user (read-string "Name of user: " )))
        (insert (concat "[[User:" user "|" user "]]"))))
  (let ((user (read-string "Name des Benutzers: " )))
    (insert (concat "[[Benutzer:" user "|" user "]]"))))

(defun mediawiki-insert-reply-prefix ()
  "Quotation box of the form {{Quotation}}{{}}. When mark is active,
surrounds region."
  (interactive)
  (beginning-of-line 1)
  (search-forward "[[")
  (backward-char 2)
  (mark-sexp 1)
  (copy-to-register mediawiki-draft-reply-register (region-beginning) (region-end) nil)
  (end-of-line 1)
  (mediawiki-terminate-paragraph)
  (beginning-of-line 1)
  (kill-line nil)
  (insert "----")
  (newline 1)
  (yank)
  (insert ":'''Re: ")
  (insert-register mediawiki-draft-reply-register 1)
  (insert "''' ")
  (end-of-line 1))

(defun mediawiki-insert-header ()
  "Insert subheader  via  == (e.g. == FOO ==.)"
  (interactive)
  (mediawiki-insert "==" "=="))

(defun mediawiki-insert-link ()
  "Insert link via [[ (e.g. [[FOO]].) When mark is active, surround region."
  (interactive)
  (mediawiki-insert "[[" "]]"))

(defun mediawiki-insert-link-www ()
  "Insert link via [[ (e.g. [http://FOO].) When mark is active, surround region."
  (interactive)
  (mediawiki-insert "[http://" "]"))

(defun mediawiki-insert-image ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable
mediawiki-english-or-german. When mark is active, surround region."
  (interactive)
  (mediawiki-insert (if mediawiki-english-or-german
                        "[[Image:"
                      "[[Bild:") "]]"))

(defun mediawiki-insert-audio ()
  "Insert link image  [[ (e.g. [[Image:FOO]].) Check the variable
mediawiki-english-or-german. When mark is active, surround region."
  (interactive)
  (mediawiki-insert (if mediawiki-english-or-german
                        "[[Media:"
                      "[[Bild:") "]]"))

(defun mediawiki-insert-signature ()
  "Insert \"~~~~:\"  "
  (interactive)
  (insert "~~~~: "))

(defun mediawiki-insert-hline ()
  "Insert \"----\"  "
  (interactive)
  (insert "\n----\n"))

(defun mediawiki-unfill-paragraph-or-region ()
  "Unfill region, this function does NOT explicitly search for \"soft newlines\"
as does mediawiki-unfill-region."
  (interactive)
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set-fill-prefix)
  (beginning-of-line 1)

  (if use-hard-newlines
      (progn
        (set (make-local-variable 'use-hard-newlines) nil)
        (set (make-local-variable 'sentence-end-double-space) t))
    (set (make-local-variable 'sentence-end-double-space) nil)
    (set (make-local-variable 'use-hard-newlines) t))
  (let ((fill-column (point-max)))
    (if (fboundp 'fill-paragraph-or-region)
        (fill-paragraph-or-region nil)
      (fill-paragraph nil))))

(defun mediawiki-start-paragraph ()
  (interactive)
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$"))

(defun mediawiki-hardlines ()
"Set use-hard-newlines to NIL."
  (interactive)
  (setq use-hard-newlines nil))

(defun mediawiki-next-long-line ()
  "Move forward to the next long line with column-width greater
than `fill-column'.

TODO: When function reaches end of buffer, save-excursion to
starting point. Generalise to make `previous-long-line'."
  (interactive)
  ;; global-variable: fill-column
  (if (= (forward-line) 0)
	  (let ((line-length
			 (save-excursion
			   (end-of-line)
			   (current-column))))
		(if (<= line-length fill-column)
			(mediawiki-next-long-line)
		  (message "Long line found")))
	;; Stop, end of buffer reached.
 	(error "Long line not found")))

(defun mediawiki-unfill-paragraph-simple ()
  "A very simple function for unfilling a paragraph."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; See http://staff.science.uva.nl/~dominik/Tools/outline-magic.el
(defun mediawiki-outline-magic-keys ()
  (interactive)
  (unless  (featurep 'xemacs)
    (local-set-key [(shift iso-lefttab)] 'outline-cycle)
    (local-set-key [iso-left-tab] 'outline-cycle))
  (local-set-key [(meta left)]  'outline-promote)
  (local-set-key [(meta right)] 'outline-demote)
  (local-set-key [(shift return)] 'newline-and-indent)
  (local-set-key [(control left)]  'mediawiki-simple-outline-promote)
  (local-set-key [(control right)] 'mediawiki-simple-outline-demote)
  (local-set-key [(control up)] 'outline-move-subtree-up)
  (local-set-key [(control down)] 'outline-move-subtree-down))
(add-hook 'mediawiki-mode-hook (lambda () (outline-minor-mode nil)))
(add-hook 'outline-minor-mode-hook 'mediawiki-outline-magic-keys)

(defun mediawiki-enhance-indent ()
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun mediawiki-yank-prefix ()
  (interactive)
  (string-rectangle (region-beginning) (region-end) ":"))

(defun mediawiki-simple-outline-promote ()
  "Function simple deletes \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (search-forward "=")
    (delete-char 1 nil)
    (end-of-line 1)
    (search-backward "=")
    (delete-char 1 nil)))

(defun mediawiki-simple-outline-demote ()
  "Function simple adds \"=\" and the end and the beginning of line,
does not promote the whole tree!"
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (search-forward "=")
    (insert "=")
    (end-of-line 1)
    (search-backward "=")
    (insert "=")))

(defun mediawiki-rename-buffer ()
  "Make sure that the option UNIQUE is used."
  (interactive)
  (rename-buffer (read-string "Name of new buffer (unique): " ) 1))

(defsubst mediawiki-draft-time-to-seconds (time)
  "Convert TIME to a floating point number."
  (+ (* (car time) 65536.0)
     (cadr time)
     (/ (or (car (cdr (cdr time))) 0) 1000000.0)))

(defsubst mediawiki-draft-mail-date (&optional rfc822-p)
  "Return a simple date.  Nothing fancy."
  (if rfc822-p
      (format-time-string "%a, %e %b %Y %T %z" (current-time))
    (format-time-string "%c" (current-time))))

(defun mediawiki-draft-buffer-desc ()
  "Using the first line of the current buffer, create a short description."
  (buffer-substring (point-min)
		    (save-excursion
		      (goto-char (point-min))
		      (end-of-line)
		      (if (> (- (point) (point-min)) 60)
			  (goto-char (+ (point-min) 60)))
		      (point))))

(defun mediawiki-draft-append-to-file ()
  "Add a header together with a subject to the text and add it to the
draft file. It might be better if longlines-mode is off."
  (let ((text (buffer-string)))
    (with-temp-buffer
      (insert (concat "\n\n"  mediawiki-draft-leader-text "Draft: "
                      (read-string "Enter Subject: ") " "
                      (current-time-string) " "
                      mediawiki-draft-leader-text
                      "\n\n\f\n\n" text "\n\f\n"))
      (if (not (bolp))
          (insert "\n\n"))
      (if (find-buffer-visiting mediawiki-draft-data-file)
          (let ((mediawiki-draft-text (buffer-string)))
            (set-buffer (get-file-buffer mediawiki-draft-data-file))
            (save-excursion
              (goto-char (point-max))
              (insert (concat "\n" mediawiki-draft-text "\n"))
              (save-buffer)))
        (append-to-file (point-min) (point-max) mediawiki-draft-data-file)))))

;;;###autoload
(defun mediawiki-draft ()
  "Open a temporary buffer in wikipedia mode for editing an wikipedia
 draft, which an arbitrary piece of data. After finishing the editing
 either use C-c C-k \\[mediawiki-draft-buffer] to send the data into
 the mediawiki-draft-data-file, or send  the buffer using C-x C-s
\\[mediawiki-save]  and insert it later into a wikipedia article."
  (interactive)
  (window-configuration-to-register mediawiki-draft-register)
  (let ((buf (get-buffer-create mediawiki-draft-buffer)))
    (switch-to-buffer-other-window buf)
    (mediawiki-mode)
    (message " C-c C-k sends to draft file, C-c C-c sends to org buffer.")))

;;;###autoload
(defun mediawiki-draft-page ()
  (interactive)
  (mark-page)
  (copy-region-as-kill (region-beginning) (region-end))
  (mediawiki-draft)
  (yank nil))

(defun mediawiki-draft-region (&optional beg end)
  "Mediawiki-Draft the data from BEG to END.
If called from within the mediawiki-draft buffer, BEG and END are ignored,
and the entire buffer will be mediawiki-drafted.  If called from any other
buffer, that region, plus any context information specific to that
region, will be mediawiki-drafted."
  (interactive)
  (let ((b (or beg (min (point) (or (mark) (point-min)))))
	(e (or end (max (point) (or (mark) (point-max))))))
    (save-restriction
      (narrow-to-region b e)
      (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)
    (when (equal mediawiki-draft-buffer (buffer-name))
      (kill-buffer (current-buffer))
      (jump-to-register mediawiki-draft-register)))))

;;;###autoload
(defun mediawiki-draft-buffer ()
  "Mediawiki-draft-buffer sends the contents of the current (temporary)
buffer to the mediawiki-draft-buffer, see the variable
mediawiki-draft-data-file."
  (interactive)
  (mediawiki-draft-region  (point-min) (point-max)))

(defun mediawiki-draft-clipboard ()
  "Mediawiki-Draft the contents of the current clipboard.
Most useful for mediawiki-drafting things from Netscape or other X Windows
application."
  (interactive)
  (with-temp-buffer
    (insert (x-get-clipboard))
    (run-hook-with-args-until-success 'mediawiki-draft-handler-functions)))

(defun mediawiki-draft-view-draft ()
  "Simple shortcut to visit the file, which contains the wikipedia drafts."
  (interactive)
  (find-file mediawiki-draft-data-file))

(defun mediawiki-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "== "  "[a-z,A-z \t]*" " =="))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "== "  "[a-z,A-z \t]*" " "))
  (when (fboundp 'zmacs-activate-region)
    (zmacs-activate-region)))

(defun mediawiki-mark-signature ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward "]]") ;;[[ ]]
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward "[[")
  (when (fboundp 'zmacs-activate-region)
    (zmacs-activate-region)))

(defun mediawiki-draft-copy-page-to-register ()
  "Copy a page via the mediawiki-draft-register."
  (interactive)
  (save-excursion
    (narrow-to-page nil)
    (copy-to-register mediawiki-draft-page (point-min) (point-max) nil)
    (message "draft page copied to wikipedia register mediawiki-draft-page.")
    (widen)))

(defun mediawiki-draft-yank-page-to-register ()
  "Insert a page via the mediawiki-draft-register."
  (interactive)
  (insert-register mediawiki-draft-page nil))

(defun mediawiki-draft-send (target-buffer)
  "Copy the current page from the mediawiki draft file to
TARGET-BUFFER.  Check the variable mediawiki-draft-send-archive.
If it is t, then additionally the text will be archived in the
draft.wiki file. Check longlines-mode, it might be better if it
is set off."
  (interactive "bTarget buffer: ")
  (mediawiki-draft-copy-page-to-register)
  (switch-to-buffer target-buffer)
  (end-of-line 1)
  (newline 1)
  (mediawiki-draft-yank-page-to-register)
  (message "The page has been sent (copied) to the mozex file!")
  (switch-to-buffer "*MW-Draft*")
  (when mediawiki-draft-send-archive
    (let ((text (buffer-string)))
      (with-temp-buffer
	(insert (concat "\n\n" mediawiki-draft-leader-text)
		(insert-register mediawiki-draft-reply-register 1)
		(insert (concat " " (current-time-string) " " 
				mediawiki-draft-leader-text  "\n\n\f\n\n"
				text "\n\f\n"))
		(if (not (bolp))
		    (insert "\n\n"))
		(if (find-buffer-visiting mediawiki-draft-data-file)
		    (let ((mediawiki-draft-text (buffer-string)))
		      (set-buffer (get-file-buffer mediawiki-draft-data-file))
		      (save-excursion
			(goto-char (point-max))
			(insert (concat "\n" mediawiki-draft-text "\n"))
			(save-buffer)))
		  (append-to-file (point-min) (point-max)
				  mediawiki-draft-data-file)))))
    (when (equal mediawiki-draft-buffer (buffer-name))
      (kill-buffer (current-buffer)))
    (switch-to-buffer target-buffer)))

(define-derived-mode mediawiki-draft-mode text-mode "MW-Draft"
  "Major mode for output from \\[mediawiki-draft].
\\<mediawiki-draft-mode-map> This buffer is used to collect data that
you want mediawiki-draft.  Just hit \\[mediawiki-draft-region] when
you're done entering, and it will go ahead and file the data for
latter retrieval, and possible indexing.
\\{mediawiki-draft-mode-map}"
  (kill-all-local-variables)
  (indented-text-mode)
  (define-key mediawiki-draft-mode-map "\C-c\C-k" 'mediawiki-draft-buffer)
  (define-key mediawiki-draft-mode-map "\C-c\C-d" 'mediawiki-draft-buffer))

(define-derived-mode mediawiki-mode text-mode "MW"
  "Major mode for editing articles written in the markup language
used by Mediawiki.

Wikipedia articles are usually unfilled: newline characters are not
used for breaking paragraphs into lines. Unfortunately, Emacs does not
handle word wrapping yet. As a workaround, wikipedia-mode turns on
longlines-mode automatically. In case something goes wrong, the
following commands may come in handy:

\\[mediawiki-fill-article] fills the buffer.
\\[mediawiki-unfill-article] unfills the buffer.

Be warned that function can be dead  slow, better use mediawiki-unfill-paragraph-or-region.
\\[mediawiki-unfill-paragraph-or-region] unfills the paragraph
\\[mediawiki-unfill-paragraph-simple] doehe same but simpler.

The following commands put in markup structures.
\\[mediawiki-insert-strong-emphasis] inserts italics
\\[mediawiki-insert-bold] inserts bold text
\\[mediawiki-insert-italics] italics
\\[mediawiki-insert-header] header
\\[mediawiki-insert-link] inserts a link

The following commands are also defined:
\\[mediawiki-insert-user] inserts user name
\\[mediawiki-insert-signature] inserts ~~~~
\\[mediawiki-insert-enumerate] inserts enumerate type structures
\\[mediawiki-insert-itemize] inserts itemize type structures
\\[mediawiki-insert-hline] inserts a hline

The draft functionality
\\[mediawiki-draft]
\\[mediawiki-draft-region]
\\[mediawiki-draft-view-draft]
\\[mediawiki-draft-page]
\\[mediawiki-draft-buffer]

Replying and sending functionality
\\[mediawiki-reply-at-point-simple]
\\[mediawiki-draft-reply]

The register functionality
\\[mediawiki-copy-page-to-register]
\\[defun mediawiki-insert-page-to-register]

Some simple editing commands.
\\[mediawiki-enhance-indent]
\\[mediawiki-yank-prefix]
\\[mediawiki-unfill-paragraph-or-region]

\\[mediawiki-terminate-paragraph]     starts a new list item or paragraph in a context-aware manner.
\\[mediawiki-next-header]     moves to the next (sub)section header.
\\[mediawiki-prev-header]     moves to the previous (sub)section header."

  (make-local-variable 'change-major-mode-hook)
  (make-local-variable 'mediawiki-edittoken)
  (make-local-variable 'mediawiki-starttimestamp)
  (make-local-variable 'mediawiki-basetimestamp)
  (make-local-variable 'mediawiki-site)
  (make-local-variable 'mediawiki-edit-form-vars)
  (make-local-variable 'mediawiki-page-title)
  (set (make-local-variable 'adaptive-fill-regexp) "[ ]*")
  (set (make-local-variable 'comment-start-skip) "\\(?:<!\\)?-- *")
  (set (make-local-variable 'comment-end-skip) " *--\\([ \n]*>\\)?")
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'paragraph-start)
       "\\*\\| \\|#\\|;\\|:\\||\\|!\\|$")
  (set (make-local-variable 'sentence-end-double-space) nil)
  (set (make-local-variable 'font-lock-multiline) t)
  (set (make-local-variable 'font-lock-defaults)
       '(mediawiki-font-lock-keywords t nil nil nil))
  (set (make-local-variable 'fill-nobreak-predicate)
       'mediawiki-link-fill-nobreak-p)
  (set (make-local-variable 'auto-fill-inhibit-regexp) "^[ *#:|;]")

  ;; Support for outline-minor-mode. No key conflicts, so we'll use
  ;; the normal outline-mode prefix.
  (set (make-local-variable 'outline-regexp) "==+")
  (when (boundp 'outline-minor-mode-prefix)
    (set (make-local-variable 'outline-minor-mode-prefix) "\C-c\C-o"))
; (set (make-local-variable 'outline-regexp) "=+")
; (set (make-local-variable 'outline-regexp) ":")

  ;; Turn on the Imenu automatically.
  (when menu-bar-mode
    (set (make-local-variable 'imenu-generic-expression)
         mediawiki-imenu-generic-expression)
    (imenu-add-to-menubar "Contents"))

  (let ((map (make-sparse-keymap "mediawiki")))
    (define-key mediawiki-mode-map [menu-bar mediawiki]
      (cons "MediaWiki" map))
    (define-key map [unfill-article]
      '("Unfill article" . mediawiki-unfill-article))
    (define-key map [fill-article]
      '("Fill article" . mediawiki-fill-article))
    (define-key map [separator-fill] '("--"))
    (define-key map [next-header]
      '("Next header" . mediawiki-next-header))
    (define-key map [prev-header]
      '("Previous header" . mediawiki-prev-header))
    (define-key map [separator-header] '("--"))
    (define-key map [outline]
      '("Toggle Outline Mode..." . outline-minor-mode))

    (modify-syntax-entry ?< "(>" mediawiki-mode-syntax-table)
    (modify-syntax-entry ?> ")<" mediawiki-mode-syntax-table)

    (define-key mediawiki-mode-map "\M-n" 'mediawiki-next-header)
    (define-key mediawiki-mode-map "\C-c\C-n" 'mediawiki-next-long-line)
    (define-key mediawiki-mode-map "\M-p" 'mediawiki-prev-header)
    (define-key mediawiki-mode-map [(meta down)] 'mediawiki-next-header)
    (define-key mediawiki-mode-map [(meta up)]   'mediawiki-prev-header)
    (define-key mediawiki-mode-map "\C-j" 'mediawiki-terminate-paragraph)

    (define-key mediawiki-mode-map "\C-c\C-q" 'mediawiki-unfill-article)
    (define-key mediawiki-mode-map "\C-c\M-q" 'mediawiki-fill-article)
    (define-key mediawiki-mode-map "\C-c\M-u" 'mediawiki-unfill-paragraph-or-region)
    (define-key mediawiki-mode-map "\C-c\C-u" 'mediawiki-unfill-paragraph-simple)
    (define-key mediawiki-mode-map "\C-c\C-f\C-s" 'mediawiki-insert-strong-emphasis)
    (define-key mediawiki-mode-map "\C-c\C-f\C-b" 'mediawiki-insert-bold)
    (define-key mediawiki-mode-map "\C-c\C-f\C-i" 'mediawiki-insert-italics)
    (define-key mediawiki-mode-map "\C-c\C-f\C-e" 'mediawiki-insert-header)
    (define-key mediawiki-mode-map "\C-c\C-f\C-l" 'mediawiki-insert-link)
    (define-key mediawiki-mode-map "\C-c\C-f\C-u" 'mediawiki-insert-user)
    (define-key mediawiki-mode-map "\C-c\C-f\C-q" 'mediawiki-insert-quotation)
    (define-key mediawiki-mode-map "\C-c\C-f\C-v" 'mediawiki-insert-bible-verse-template)
    (define-key mediawiki-mode-map "\C-c\C-w" 'mediawiki-insert-signature)
    (define-key mediawiki-mode-map "\C-c\C-l" 'mediawiki-insert-hline)
    (define-key mediawiki-mode-map [(meta f7)] 'mediawiki-draft)
    (define-key mediawiki-mode-map [(meta f8)] 'mediawiki-reply-at-point-simple)
    (define-key mediawiki-mode-map [(meta f9)] 'mediawiki-draft-view-draft)
    (define-key mediawiki-mode-map "\C-c\C-r" 'mediawiki-reply-at-point-simple)
    (define-key mediawiki-mode-map "\C-cr" 'mediawiki-draft-region)
    (define-key mediawiki-mode-map [(meta r)] 'mediawiki-draft-reply)
    (define-key mediawiki-mode-map "\C-c\C-m" 'mediawiki-draft)
    (define-key mediawiki-mode-map "\C-c\C-b" 'mediawiki-draft-region)
    (define-key mediawiki-mode-map "\C-c\C-d" 'mediawiki-draft-buffer)
    (define-key mediawiki-mode-map "\C-c\C-k" 'mediawiki-draft-buffer)
    (define-key mediawiki-mode-map "\C-c\C-p" 'mediawiki-draft-copy-page-to-register)
    (define-key mediawiki-mode-map "\C-c\C-c" 'mediawiki-draft-send)
    (define-key mediawiki-mode-map "\C-c\C-s" 'mediawiki-draft-yank-page-to-register)

    (define-key mediawiki-mode-map [(control meta prior)] 'mediawiki-enhance-indent)
    (define-key mediawiki-mode-map [(control meta next)] 'mediawiki-yank-prefix)
    (define-key mediawiki-mode-map [(meta return)] 'mediawiki-insert-enumerate)
    (define-key mediawiki-mode-map [(meta control return)] 'mediawiki-insert-enumerate-nonewline)
    ;; private setting
    (define-key mediawiki-mode-map [(shift return)] 'newline-and-indent)
    (define-key mediawiki-mode-map "\C-\\" 'mediawiki-insert-itemize)
    (define-key mediawiki-mode-map [(control return)] 'mediawiki-insert-itemize)
    (define-key mediawiki-mode-map "\C-ca" 'auto-capitalize-mode)
;    (define-key mediawiki-mode-map "\C-ci" 'set-input-method)
;    (define-key mediawiki-mode-map "\C-ct" 'toggle-input-method)

    (define-key mediawiki-mode-map [(backtab)] 'mediawiki-goto-prev-link)
    (define-key mediawiki-mode-map [(tab)]     'mediawiki-goto-next-link)
    (define-key mediawiki-mode-map "\M-g"      'mediawiki-reload)
    (define-key mediawiki-mode-map "\C-x\C-s"  'mediawiki-save)
    (define-key mediawiki-mode-map "\C-c\C-c"  'mediawiki-save-and-bury)
    (define-key mediawiki-mode-map "\C-x\C-w"  'mediawiki-save-as)
    (define-key mediawiki-mode-map "\C-c\C-o"  'mediawiki-open)
    (define-key mediawiki-mode-map "\M-p"
      'mediawiki-goto-previous-page)
    (define-key mediawiki-mode-map "\M-n"      'mediawiki-goto-next-page)
    (define-key mediawiki-mode-map [(control return)]
      'mediawiki-open-page-at-point)))

;; (defvar mw-pagelist-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (suppress-keymap map)
;;     (define-key map [(return)] 'mw-pl-goto-page-at-point)
;;     (define-key map "n"        'mw-pl-page-down)
;;     (define-key map "C-v"      'mw-pl-page-down)
;;     (define-key map [(next)]  'mw-pl-page-down)
;;     (define-key map "p"        'mw-pl-page-up)
;;     (define-key map "M-v"      'mw-pl-page-up)
;;     (define-key map [(prior)]  'mw-pl-page-up)))

;; (define-derived-mode mw-pagelist-mode special-mode "MW-PageList")

(provide 'mediawiki)

;; Local Variables:
;; time-stamp-pattern: "20/^;; Last Modified: <%%>$"
;; End:

;;; mediawiki.el ends here
