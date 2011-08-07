;; Vi operator as prefix argument."
(defadvice viper-prefix-arg-com (around viper-prefix-arg-com-for-textobj (char value com) activate)
  (let ((cont t)
	cmd-info
	cmd-to-exec-at-end)
    (while (and cont
		(viper-memq-char char
				 (list ?a ?c ?d ?i ?s ?y ?! ?< ?> ?= ?# ?r ?R ?\"
				       viper-buffer-search-char)))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
	    (setq com (cons char com))
	    (setq cont nil))
	;; If com is nil we set com as char, and read more.  Again, if char is
	;; ", we read the name of register and store it in viper-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit the while
	;; loop.
	(cond ((viper-memq-char char '(?! ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((viper= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char)))
	      ((viper= char ?\")
	       (let ((reg (read-char)))
		 (if (viper-valid-register reg)
		     (setq viper-use-register reg)
		   (error "Viper bell"))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (read-char))))))

    (if (atom com)
	;; `com' is a single char, so we construct the command argument
	;; and if `char' is `?', we describe the arg; otherwise
	;; we prepare the command that will be executed at the end.
	(progn
	  (setq cmd-info (cons value com))
	  (while (viper= char ?U)
	    (viper-describe-arg cmd-info)
	    (setq char (read-char)))
	  ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
	  ;; we execute it at the very end
	  (or (viper-movement-command-p char)
	      (viper-digit-command-p char)
	      (viper-regsuffix-command-p char)
	      (viper= char ?!) ; bang command
	      (viper= char ?g) ; the gg command (like G0)
	      (error "Viper bell"))
	  (setq cmd-to-exec-at-end
		(viper-exec-form-in-vi
		 `(key-binding (char-to-string ,char)))))

      ;; as com is non-nil, this means that we have a command to execute
      (if (viper-memq-char (car com) '(?r ?R))
	  ;; execute apropriate region command.
	  (let ((char (car com)) (com (cdr com)))
	    (setq prefix-arg (cons value com))
	    (if (viper= char ?r)
		(viper-region prefix-arg)
	      (viper-Region prefix-arg))
	    ;; reset prefix-arg
	    (setq prefix-arg nil))
	;; otherwise, reset prefix arg and call appropriate command
	(setq value (if (null value) 1 value))
	(setq prefix-arg nil)
	(cond
	 ;; If we change ?C to ?c here, then cc will enter replacement mode
	 ;; rather than deleting lines.  However, it will affect 1 less line
	 ;; than normal.  We decided to not use replacement mode here and
	 ;; follow Vi, since replacement mode on n full lines can be achieved
	 ;; with nC.
	 ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	 ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	 ((equal com '(?d . ?y)) (viper-yank-defun))
	 ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	 ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	 ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	 ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	 ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	 ;; gg  acts as G0
	 ((viper-memq-char (car com) '(?a ?i))
	  (viper-text-obj-command (cons value (cons (read-char) com))))
	 ((equal com '(?s . ?d))
	  (viper-text-obj-surround-delete-command (cons value (cons (read-char) com))))
	 ((equal com '(?s . ?c))
	  (setq com (cons (read-char) com))
	  (setq com (cons (read-char) com))
	  (viper-text-obj-surround-change-command (cons value com)))
	 (t (error "Viper bell")))))
    
    (if cmd-to-exec-at-end
	(progn
	  (setq last-command-event
		(viper-copy-event
		 (if (featurep 'xemacs) (character-to-event char) char)))
	  (condition-case err
	      (funcall cmd-to-exec-at-end cmd-info)
	    (error
	     (error "%s" (error-message-string err))))))
    ))

;; text object command

(defun viper-text-obj-word (val inner)
  (if (viper-looking-at-separator)
      (viper-skip-separators nil)
    (viper-forward-word-kernel 1)
    (viper-backward-word-kernel 1))
  (viper-set-mark-if-necessary)
  (viper-deactivate-mark)
  (if inner
      (viper-loop val (cond ((viper-looking-at-alpha)
			     (viper-skip-alpha-forward "_"))
			    ((viper-looking-at-separator)
			     (viper-skip-all-separators-forward t))
			    ((not (viper-looking-at-alphasep))
			     (viper-skip-nonalphasep-forward))))
    (viper-forward-word-kernel val))
  (exchange-point-and-mark))

(defun viper-text-obj-Word (val inner)
  (if (viper-looking-at-separator)
      (viper-skip-separators nil)
    (viper-skip-nonseparators 'forward)
    (viper-skip-nonseparators 'backward))
  (viper-set-mark-if-necessary)
  (viper-deactivate-mark)
  (if inner
      (viper-loop val (if (viper-looking-at-separator)
			  (viper-skip-separators t)
			(viper-skip-nonseparators 'forward)))
    (viper-loop val
		(when (viper-looking-at-separator)
		  (viper-skip-separators t))
		(viper-skip-nonseparators 'forward)
		(unless (eolp)
		  (viper-skip-separators t))))
  (exchange-point-and-mark))

(defun viper-text-obj-sentence (val inner)
  (forward-sentence 1)
  (backward-sentence 1)
  (viper-set-mark-if-necessary)
  (viper-deactivate-mark)
  (forward-sentence val)
  (unless inner (viper-forward-word-kernel 1))
  (exchange-point-and-mark))

(defun viper-text-obj-paragraph (val inner)
  (forward-paragraph 1)
  (backward-paragraph 1)
  (unless (bobp) (forward-char))
  (viper-set-mark-if-necessary)
  (viper-deactivate-mark)
  (forward-paragraph val)
  (unless inner
    (while (and (looking-at paragraph-separate)
		(zerop (forward-line)))))
  (exchange-point-and-mark))

(defun viper-text-obj-block (val inner)
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?\{ "_")
    (modify-syntax-entry ?\} "_")
    (modify-syntax-entry ?\[ "_")
    (modify-syntax-entry ?\] "_")
    (modify-syntax-entry ?\" "_")	; for ("f-!-oo")
    (viper-text-obj-paren-block val inner "(")))

(defun viper-text-obj-Block (val inner)
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?\( "_")
    (modify-syntax-entry ?\) "_")
    (modify-syntax-entry ?\[ "_")
    (modify-syntax-entry ?\] "_")
    (modify-syntax-entry ?\" "_")	; for {"f-!-oo"}
    (viper-text-obj-paren-block val inner "{")))

(defun viper-text-obj-< (val inner)
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?< "(>")
    (modify-syntax-entry ?> ")<")
    (modify-syntax-entry ?\( "_")
    (modify-syntax-entry ?\) "_")
    (modify-syntax-entry ?\{ "_")
    (modify-syntax-entry ?\} "_")
    (modify-syntax-entry ?\[ "_")
    (modify-syntax-entry ?\] "_")
    (modify-syntax-entry ?\" "_")	; for <"f-!-oo">
    (viper-text-obj-paren-block val inner "<")))

(defun viper-text-obj-\[ (val inner)
  (with-syntax-table (make-syntax-table)
    (modify-syntax-entry ?\( "_")
    (modify-syntax-entry ?\) "_")
    (modify-syntax-entry ?\{ "_")
    (modify-syntax-entry ?\} "_")
    (modify-syntax-entry ?\" "_")	; for ["f-!-oo"]
    (viper-text-obj-paren-block val inner "[")))

(defun viper-text-obj-paren-block (val inner paren)
  (when (looking-at (regexp-quote paren)) (forward-char))
  (up-list val)
  (when inner (backward-char))
  (viper-set-mark-if-necessary)
  (when inner (forward-char))
  (backward-list)
  (when inner (forward-char))
  (viper-deactivate-mark))

(defun viper-text-obj-quoted-str (inner quote-str)
  (let (quoted-str-end)
    (when (and (looking-at quote-str) (looking-back "[^\\]"))
      (with-syntax-table (make-syntax-table)
	(modify-syntax-entry ?\" "_")
	(modify-syntax-entry (string-to-char quote-str) "\"")
	(save-excursion
	  (setq quoted-str-end
		(nth 3 (parse-partial-sexp (point-min) (point)))))))
    (if quoted-str-end
	(forward-char)
      (re-search-forward (concat "[^\\]" quote-str) (point-max) nil))
    (when inner (backward-char))
    (viper-set-mark-if-necessary)
    (viper-deactivate-mark)
    (re-search-backward (concat "\\([^\\]\\|\\`\\)" quote-str)
			(point-min) nil (if inner 1 2))
    (forward-char (+ (if inner 1 0)
		     (if (and (bobp) (looking-at quote-str)) 0 1)))
    (viper-deactivate-mark)))

(defun viper-text-obj-tag (val inner)
  (let ((single-tag (regexp-opt '("area" "base" "basefont" "br" "col" "frame"
				  "hr" "img" "input" "isindex" "link" "meta"
				  "param")))
	(depth val)
	(case-fold-search t))
    (when (looking-at "<") (forward-char))
    (with-syntax-table (make-syntax-table)
      (modify-syntax-entry ?< "(>")
      (modify-syntax-entry ?> ")<")
      (modify-syntax-entry ?\( "_")
      (modify-syntax-entry ?\) "_")
      (modify-syntax-entry ?\{ "_")
      (modify-syntax-entry ?\} "_")
      (modify-syntax-entry ?\[ "_")
      (modify-syntax-entry ?\] "_")
      (let (inside)
	(save-excursion
	  (setq inside
		(/= 0 (car (parse-partial-sexp (point-min) (point))))))
	(when inside
	  (search-backward "<")
	  (if (looking-at "</")
	      (backward-char)
	    (search-forward ">")))))
    (while (and (> depth 0)
		(re-search-backward "<\\(/?\\)\\([^> ]+\\)"))
      (cond ((string= "!--" (match-string 2)))
	    ((and (string= "" (match-string 1))
		  (not (string-match single-tag (match-string 2))))
	     (setq depth (1- depth)))
	    ((string= "/" (match-string 1))
	     (setq depth (1+ depth)))))
    (when inner (search-forward ">"))
    (viper-set-mark-if-necessary)
    (unless inner (search-forward ">"))
    (setq depth 1)
    (while (and (> depth 0)
		(re-search-forward "<\\(/?\\)\\([^> ]+\\)"))
      (cond ((string= "!--" (match-string 2)))
	    ((and (string= "" (match-string 1))
		  (not (string-match single-tag (match-string 2))))
	     (setq depth (1+ depth)))
	    ((string= "/" (match-string 1))
	     (setq depth (1- depth)))))
    (if inner
	(search-backward "</")
      (search-forward ">"))
    (viper-deactivate-mark)
    (exchange-point-and-mark)))

(defun viper-text-obj-subr (val obj inner)
  (cond ((eq obj ?w)
	 (viper-text-obj-word val inner))
	((eq obj ?W)
	 (viper-text-obj-Word val inner))
	((eq obj ?s)
	 (viper-text-obj-sentence val inner))
	((eq obj ?p)
	 (viper-text-obj-paragraph val inner))
	((memq obj '(?b ?\( ?\)))
	 (viper-text-obj-block val inner))
	((memq obj '(?B ?\{ ?\}))
	 (viper-text-obj-Block val inner))
	((memq obj '(?< ?>))
	 (viper-text-obj-< val inner))
	((memq obj '(?\[ ?\]))
	 (viper-text-obj-\[ val inner))
	((memq obj '(?\" ?' ?`))
	 (viper-text-obj-quoted-str inner (char-to-string obj)))
	((eq obj ?t)
	 (viper-text-obj-tag val inner))
	(t (error ""))))

(defun viper-text-obj-command (arg)
  (let* ((val (car arg))
	 (list (cdr arg))
	 (char (car list))
	 (mode (cadr list))
	 (com (cddr list)))
    (viper-text-obj-subr val char (eq mode ?i))
    (viper-move-marker-locally 'viper-com-point (point))
    (exchange-point-and-mark)
    (viper-execute-com 'viper-region nil com)
    (viper-set-destructive-command
     (list 'viper-text-obj-command val list nil nil nil))))

;
;;; text object surround
;

;; arg => (repeat-count target-char "s" "d")
(defun viper-text-obj-surround-delete-command (arg)
  (let* ((val (car arg))
	 (list (cdr arg))
	 (char (car list)))
    (viper-text-obj-subr val char t)
    (viper-move-marker-locally 'viper-com-point (point))
    (delete-backward-char 1)
    (exchange-point-and-mark)
    (delete-char 1)
    (viper-set-destructive-command
     (list 'viper-text-obj-command val list nil nil nil))))
    
;; arg => (repeat-count replace-char target-char "s" "c")
(defun viper-text-obj-surround-change-command (arg)
  (let* ((val (car arg))
	 (list (cdr arg))
	 (post-char (car list))
	 (pre-char (cadr list)))
    (unless (memq post-char '(?b ?( ?) ?B ?{ ?} ?\" ?' ?` ?[ ?] ?< ?>))
      (error ""))
    (viper-text-obj-subr val pre-char t)
    (viper-move-marker-locally 'viper-com-point (point))
    (unless (memq pre-char '(?w ?W ?p ?s))
      (delete-backward-char 1))
    (insert (viper-surround-char post-char t))
    (exchange-point-and-mark)
    (unless (memq pre-char '(?w ?W ?p ?s))
      (delete-char 1))
    (insert (viper-surround-char post-char nil))
    (viper-set-destructive-command
     (list 'viper-text-obj-surround-change-command val list nil nil nil))))
	    
(defun viper-surround-char (char pre)
  (let ((char-list '((?b . ("(" . ")"))
		    (?( . ("(" . ")"))
		    (?) . ("(" . ")"))
		    (?B . ("{" . "}"))
		    (?{ . ("{" . "}"))
		    (?} . ("{" . "}"))
		    (?\" . ("\"" . "\""))
		    (?' . ("'" . "'"))
		    (?` . ("`" . "`"))
		    (?[ . ("[" . "]"))
		    (?] . ("[" . "]")))))
    (if pre
	(cadr (assoc char char-list))
      (cddr (assoc char char-list)))))
    
	      
(provide 'viper-textobj)
