;; -*- lexical-binding: t -*-

;; https://web.archive.org/web/20201206011037/https://github.com/jwiegley/use-package/issues/436
(require 'use-package)

(require 'cl-lib)

(defmacro my-mode-setup (mode-hook &rest body)
  (declare (indent 1))
  (let* ((setup-name (when (consp mode-hook)
                       (cadr mode-hook)))
         (mode-hook (if (consp mode-hook)
                        (car mode-hook)
                      mode-hook))
         (setup-name (or setup-name
                         (intern (concat "my-"
                                         (string-trim-right (symbol-name mode-hook) "-hook")
                                         "-setup")))))
    `(progn
       (defun ,setup-name ()
         ,@body)
       (add-hook ',mode-hook ',setup-name))))

;; ===============================
;; f-keys
;; ===============================

(defun my-ensure-function (symbol)
  ;; In Emacs 30.1, some keymaps aren't defined as a prefix maps.
  ;; That means that they can't be referenced by their symbol names in
  ;; other maps.  Normally that doesn't matter much, but it means that
  ;; in which-key's display they aren't given a nice label.
  (unless (fboundp symbol)
    (fset symbol (symbol-value symbol))))

(my-ensure-function 'narrow-map)

(use-package rect
  :commands (rectangle-mark-mode-map)
  :defer t
  :config
  (my-ensure-function 'rectangle-mark-mode-map))

(defvar my-mode-prefix "C-M-SPC")

(defun my-mode-prefix (key)
  (declare (compiler-macro (lambda (original)
                             (if (stringp key)
                                 (concat my-mode-prefix " " key)
                               original)))
           (pure t))
  (concat my-mode-prefix " " key))

(defvar my-global-prefix "M-SPC")

(defun my-global-prefix (key)
  (declare (compiler-macro (lambda (original)
                             (if (stringp key)
                                 (concat my-global-prefix " " key)
                               original)))
           (pure t))
  (concat my-global-prefix " " key))

(defmacro my-define-keymap (name &rest args)
  (declare (indent defun))
  `(progn
     (defvar-keymap ,name
       ,@args)
     (fset ',name ,name)))

(my-define-keymap my-global-map
  "f" 'find-file
  "s" 'save-buffer
  "w" 'write-file
  "k" 'kill-buffer

  "r" 'rectangle-mark-mode-map

  "1" 'delete-other-windows
  "2" 'split-window-below
  "3" 'split-window-horizontally
  "0" 'delete-window
  "+" 'balance-windows
  "o" 'other-window

  "=" 'global-text-scale-adjust
  "-" 'global-text-scale-adjust

  "n" 'narrow-map

  "(" 'kmacro-start-macro
  ")" 'kmacro-end-macro
  "e" 'kmacro-end-and-call-macro

  "$" 'ispell-word)

(keymap-set global-map my-global-prefix 'my-global-map)
(keymap-set function-key-map "<f1>" my-global-prefix)

(my-define-keymap my-local-map)

(keymap-set global-map my-mode-prefix 'my-local-map)
(keymap-set function-key-map "<f2>" my-mode-prefix)

(defun pinky-saver (original-key new-key &optional keymap)
  (let* ((keymap (or keymap global-map))
         (old-command (keymap-lookup keymap original-key))
         (old-command (or (and (symbolp old-command)
                               (get old-command 'pinky-saver))
                          old-command))
         (new-name (cond
                    ((symbolp old-command)
                     (intern (concat (symbol-name old-command) "-pinky-saver")))

                    (t
                     (gensym "pinky-saver")))))
    (put new-name 'pinky-saver old-command)
    (defalias new-name
      (byte-compile
       (let ((last-execution nil))
         (lambda ()
           (interactive)
           (if (and last-execution (time-less-p (time-since last-execution) 10))
               (call-interactively old-command)
             (message (propertize "Ow!  Your pinky!  Use [%s] instead, or repeat the command" 'face '(error (:height 2.0))) new-key)
             (setf last-execution (float-time))
             nil)))))
    (keymap-set keymap original-key new-name)))

(pinky-saver "C-x C-s" "<f1> s")
(pinky-saver "C-x C-w" "<f1> w")
(pinky-saver "C-x C-f" "<f1> f")
(pinky-saver "C-x C-b" "<f1> b")
(pinky-saver "C-x b" "<f1> b")
(pinky-saver "C-x o" "<f1> o")
(pinky-saver "C-x 1" "<f1> 1")
(pinky-saver "C-x 2" "<f1> 2")
(pinky-saver "C-x 3" "<f1> 3")
(pinky-saver "C-x +" "<f1> +")

(pinky-saver "C-x C-e" "<f2> e") ; Yes this is defined in the global map

;; ===============================
;; repeat
;; ===============================

(use-package repeat
  :config
  (repeat-mode 1)
  :demand t)

;; ===============================
;; completion
;; ===============================

(use-package vertico
  :config (vertico-mode)
  :demand t)

(use-package marginalia
  :config (marginalia-mode)
  :demand t)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(substring orderless basic)))

;; ===============================
;; company
;; ===============================

;; Orderless and company don't play nicely together, sadly.  Well,
;; they do, but the orderless completion style isn't very nice for
;; company, and there doesn't seem to be a better way of changing
;; completion-styles just for company.
;; https://web.archive.org/web/20231118143102/https://www.reddit.com/r/emacs/comments/nichkl/how_to_use_different_completion_styles_in_the/

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :config
  (defun ada-company-capf-around-advice (orig-fun &rest args)
    ;; dynamic scoping saves the day!
    (let ((completion-styles '(basic partial-completion emacs22)))
      (apply orig-fun args)))
  (advice-add 'company-capf :around #'ada-company-capf-around-advice))

;; ===============================
;; consult
;; ===============================

(use-package consult
  :commands (consult-line
             consult-grep
             consult-find
             consult-man
             consult-goto-line
             consult-buffer)
  :init
  (keymap-set my-global-map "l" 'consult-line)
  (keymap-set my-global-map "]" 'consult-grep)
  (keymap-set my-global-map "d" 'consult-find)
  (keymap-set my-global-map "M" 'consult-man)
  (keymap-set my-global-map "b" 'consult-buffer)

  (setf completion-in-region-function
        ;; https://web.archive.org/web/20231120220521/https://github.com/minad/vertico#completion-at-point-and-completion-in-region
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  :config
  ;; Workaround for https://github.com/minad/consult/pull/1217
  (defun my-around-consult-theme (original theme)
    (when (eq theme 'default)
      (setf theme nil))
    (if (not theme)
        (funcall original theme)
      (unless (custom-theme-p theme)
        (load-theme theme t t))
      (if (custom-theme-p theme)
          (funcall original theme)
        (message "%s is not a theme" theme)
        (funcall original nil))))
  (advice-add 'consult-theme :around #'my-around-consult-theme))

;; ===============================
;; rgrep-fast
;; ===============================

(defun rgrep-fast (regexp &optional files dir confirm)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((and grep-find-command (equal current-prefix-arg '(16)))
       (list (read-from-minibuffer "Run: " grep-find-command
                                   nil nil 'grep-find-history)))
      ((not grep-find-template)
       (error "grep.el: No `grep-find-template' available"))
      (t (let* ((regexp (grep-read-regexp))
                (files "*")
                (dir (if (magit-toplevel)
                         (magit-toplevel)
                       (read-directory-name "Base directory: "
                                            nil default-directory t)))
                (confirm (equal current-prefix-arg '(4))))
           (list regexp files dir confirm))))))
  (let ((grep-find-ignored-files grep-find-ignored-files)
        (grep-find-template (if (magit-toplevel dir)
                                "git --no-pager grep -nH <C> -e <R>"
                              grep-find-template)))
    (rgrep regexp files dir confirm)))

(global-set-key (kbd "C-M-]") 'rgrep-fast)

;; ===============================
;; mouse
;; ===============================

(when (window-system)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

;; ===============================
;; misc stuff
;; ===============================

(use-package which-key
  :demand t
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-right-bottom))

(use-package diminish
  :diminish buffer-face-mode
  :diminish auto-revert-mode
  :diminish eldoc-mode)

(set-default 'cursor-type 'box)
(setf column-number-mode t)
(size-indication-mode t)
(menu-bar-mode -1)
(when window-system
  (setf default-frame-alist
        '((width . 80)
          (height . 40))))
(setf inhibit-startup-screen t)
(setf initial-buffer-choice nil)
(set-scroll-bar-mode nil)
(tool-bar-mode 0)
(setf ring-bell-function (lambda ()))
(setf scroll-conservatively 10000)
(pending-delete-mode 1)
(delete-selection-mode 1)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups/"))))

(setq require-final-newline t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq tramp-default-method "ssh")

(setq doc-view-continuous t)
(setf dired-dnd-protocol-alist nil)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(global-unset-key (kbd "<mouse-2>"))
(global-unset-key (kbd "<mouse-3>"))

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

;; Some stuff stolen from Emacs prelude
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap beginning-of-visual-line]
                'prelude-move-beginning-of-line)
(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)
;; Okay back to my code, now

(defun my-kill-region (original beginning end &optional region)
  (if (and region (not mark-active))
      (let ((kill-whole-line t))
        (move-beginning-of-line 1)
        (call-interactively (keymap-lookup nil "C-k")))
    (funcall original beginning end region)))

(advice-add 'kill-region :around 'my-kill-region)

(defun my-kill-ring-save (original beginning end &optional region)
  (if (and region (not mark-active))
      (let (vhl-start-point)
        (save-excursion
          (move-beginning-of-line 1)
          (setf vhl-start-point (point))
          (prog1
              (let ((kill-whole-line t)
                    (buffer-read-only t)
                    (kill-read-only-ok t)
                    (inhibit-message t))
                (call-interactively (keymap-lookup nil "C-k")))
            (message "Copied")
            (vhl/add-range vhl-start-point (point)))))
    (funcall original beginning end region)))

(advice-add 'kill-ring-save :around 'my-kill-ring-save)

(defvar-local my-indent-after-yank nil)

(defun my-indent-after-yank (&rest args)
  (when my-indent-after-yank
    (let ((beginning (point))
          (end (mark t)))
      (when (> beginning end)
        (cl-rotatef beginning end))
      (indent-region beginning end))))

(advice-add 'yank :after 'my-indent-after-yank)
(advice-add 'yank-pop :after 'my-indent-after-yank)

(defvar-local my-allow-unfill-on-visual-line-mode t)

(defun my-fill-advice (original &rest rest)
  (if (and my-allow-unfill-on-visual-line-mode
           visual-line-mode)
      (let ((fill-column (point-max)))
        (apply original rest))
    (apply original rest)))

(advice-add 'fill-paragraph :around 'my-fill-advice)

(use-package define-word
  :commands (define-word-at-point)
  :init
  (keymap-set my-global-map "#" 'define-word-at-point))

(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode 1)
    (setf undo-tree-auto-save-history nil)))

(use-package elec-pair
  :init
  (electric-pair-mode 1))

(use-package windmove
  :demand t
  :config
  (windmove-default-keybindings 'meta))

(use-package volatile-highlights
  :demand t
  :diminish volatile-highlights-mode
  :config
  (progn
    (volatile-highlights-mode t)
    (defadvice pop-tag-mark (after highlight-line)
      (save-excursion
	(let ((line-start (progn (move-beginning-of-line 1) (point)))
	      (line-end (progn (move-end-of-line 1) (1+ (point)))))
	  (vhl/add-range line-start line-end))))
    (ad-activate 'pop-tag-mark)

    (defadvice recenter-top-bottom (after highlight-line)
      (save-excursion
	(let ((line-start (progn (move-beginning-of-line 1) (point)))
	      (line-end (progn (move-end-of-line 1) (1+ (point)))))
	  (vhl/add-range line-start line-end))))
    (ad-activate 'recenter-top-bottom)))

;; ===============================
;; fringe
;; ===============================

(when window-system
  ;; Get a very small right fringe
  (set-fringe-mode '(5 . 5))
  ;; Put useful things in it
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default indicate-empty-lines t)
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq visual-line-fringe-indicators '(nil right-curly-arrow))

  ;; By default, scrolling in fringes and margins is unbound. This is
  ;; silly. Make it do the sane thing and scroll like normal. Be
  ;; careful, though! At least one Emacs mac port rebinds the scroll
  ;; keys after init! So, we go through a layer of indirection and
  ;; lookup the correct function to call every time the scroll event
  ;; happens.
  (dolist (direction '("up" "down" "left" "right"))
    (dolist (side '("left" "right"))
      (dolist (type '("fringe" "margin"))
        (let* ((fringe-str (concat "<" side "-" type ">"))
               (wheel-str (concat "<wheel-" direction ">"))
               (from-kbd (kbd (concat fringe-str " " wheel-str)))
               (to-fn (lambda (event)
                         ; Need to be interactive, for some
                         ; reason. Copied paramater from function we
                         ; are wraping.
                         (interactive (list last-input-event))
                         ; Call the real function, but bake the key to
                         ; lookup into the lambda function
                         (call-interactively (global-key-binding
                                              (kbd wheel-str))
                                             nil [event]))))
          (global-set-key from-kbd to-fn))))))

;; ===============================
;; Flyspell
;; ===============================

(use-package flyspell
  :hook text-mode
  :hook (prog-mode . flyspell-prog-mode)
  :diminish flyspell-mode
  :diminish flyspell-prog-mode
  :config
  (keymap-set flyspell-mouse-map "<mouse-2>" nil)
  (keymap-set flyspell-mouse-map "<mouse-3>" 'flyspell-correct-word))

;; ===============================
;; c
;; ===============================

(use-package cc-mode
  :defer t
  :config
  (setq c-default-style
        (quote
         ((c-mode . "stroustrup")
          (c++-mode . "stroustrup")
          (java-mode . "java")
          (awk-mode . "awk")
          (other . "gnu")))))

;; ===============================
;; lisp
;; ===============================

(use-package slime
  :after lisp-mode
  :defer t
  :config
  (slime-setup '(slime-fancy slime-company slime-trace-dialog slime-xref-browser))

  (defun my-after-slime-show-description (&rest rest)
    ;; Slime defaults to fundamental mode when showing docs.  This is
    ;; a little annoying because slime-company has a nice mode for
    ;; showing the exact same text.  Let's just use their mode!
    (let ((name (slime-buffer-name :description)))
      (with-current-buffer name
        (when (eql major-mode 'fundamental-mode)
          (slime-company-doc-mode)))))
  (advice-add 'slime-show-description :after 'my-after-slime-show-description)

  (my-define-keymap my-lisp-mode-map
    "c" 'slime-compile-defun
    "e" 'slime-eval-last-expression
    "l" 'slime-load-file
    "h" 'slime-documentation-lookup
    "d" 'slime-describe-symbol
    "a" 'slime-apropos
    "r" 'slime-eval-region)

  (keymap-set lisp-mode-map my-mode-prefix 'my-lisp-mode-map))

(use-package paredit
  :diminish paredit-mode
  :hook ((lisp-data-mode . paredit-mode)
         (lisp-mode . paredit-mode))
  :config
  (my-mode-setup paredit-mode
    (setq-local kill-whole-line t)))

(use-package elisp-mode
  :defer t
  :config
  (my-define-keymap my-emacs-lisp-mode-map
    "e" 'eval-last-sexp
    "l" 'load-file)
  (keymap-set emacs-lisp-mode-map my-mode-prefix 'my-emacs-lisp-mode-map))

;; ===============================
;; macOS
;; ===============================

(when (eq system-type 'darwin)
  ;; keyboard shortcuts
  (setq mac-command-modifier 'super)
  (setq mac-option-modifier 'meta))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  ;; We are running a guifull emacs! Fix our path.
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; ===============================
;; Git
;; ===============================

(use-package magit
  :commands (magit-status magit-file-dispatch)
  :init
  (keymap-set my-global-map "g" 'magit-status)
  (keymap-set my-global-map "F" 'magit-file-dispatch)
  :config
  (setf magit-log-arguments '("--graph" "--color" "--decorate" "-n256")))

(use-package git-gutter
  :demand t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode t)
  (custom-theme-set-faces
   'user
   '(git-gutter-fr:added ((t (:inherit (fringe git-gutter:added)))))
   '(git-gutter-fr:deleted ((t (:inherit (fringe git-gutter:deleted)))))
   '(git-gutter-fr:modified ((t (:inherit (fringe git-gutter:modified)))))))

(use-package git-gutter-fringe
  :after git-gutter
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package vc
  :defer t
  :config
  (setf vc-git-resolve-conflicts nil))

;; ===============================
;; Org
;; ===============================

(use-package org
  :mode (("\\.org$" . org-mode))
  :diminish org-num-mode
  :diminish org-indent-mode
  :config
  (defvar-local my-org-header-line-content nil)
  (put 'my-org-header-line-content 'risky-local-variable t)

  (defconst my-org-header-line
    '(:eval
      (progn
        ;; We need to bounce through a symbol to prevent emacs from
        ;; processing % constructs in the resulting string.
        (setf my-org-header-line-content (my-get-org-header-line))
        (list
         (propertize " " 'display '((space :align-to left-margin)))
         'my-org-header-line-content))))

  (defconst my-org-header-line-back-to-top
    (progn
      (let ((keymap (make-sparse-keymap)))
        (define-key keymap (kbd "<header-line> <mouse-1>") 'beginning-of-buffer)
        (concat
         (propertize "Back to top" 'keymap keymap 'face '(shadow org-level-1) 'mouse-face 'underline)))))

  (defun my-get-org-header-line ()
    (cl-block return
      (let ((headings (my-get-org-headings)))
        (concat
         (propertize "⮤ " 'face 'shadow)
         (cond
          (headings
           (mapconcat 'identity
                      headings
                      (propertize " > " 'face 'shadow)))
          ((not (equal (window-start) (point-min)))
           my-org-header-line-back-to-top)
          (t
           (cl-return-from return nil)))))))

  (defun my-org-ensure-title-fontified (string org-level)
    (if (get-text-property 0 'face string)
        string
      (propertize string 'face (if org-cycle-level-faces
		                   (nth (% (1- org-level) org-n-level-faces) org-level-faces)
		                 (nth (1- (min org-level org-n-level-faces)) org-level-faces)))))

  (defun my-get-org-headings ()
    (save-excursion
      ;; Move to the start of the first non-empty line
      (goto-char (window-start))
      (let ((first-non-blank (re-search-forward (rx (not space)) nil t)))
        (when first-non-blank
          (goto-char first-non-blank)
          (forward-line 0)))

      (let* (number-prefix
             titles
             (top (point))
             (current (org-element-lineage
                       (org-element-at-point)
                       '(headline)
                       'with-self)))
        ;; Special case.  If the buffer starts with a headline, then
        ;; we want to skip the current element in the navigation line.
        (when (equal top (org-element-begin current))
          (setf current (org-element-lineage current 'headline)))

        (while current
          (goto-char (org-element-begin current))
          (save-match-data
            (re-search-forward (regexp-quote (org-element-property :title current)) (save-excursion (end-of-line) (point)))
            (let* ((string (match-string 0))
                   (string (my-org-ensure-title-fontified string (org-element-property :level current)))
                   (keymap (make-sparse-keymap))
                   (begin (org-element-begin current)))
              (when (and org-num-mode (null number-prefix))
                (save-excursion
                  (re-search-backward (rx "*"))
                  (forward-char)
                  (cl-block done
                    (dolist (overlay (overlays-at (point)))
                      (when (and (overlay-get overlay 'org-num)
                                 (overlay-get overlay 'after-string))
                        (setf number-prefix (overlay-get overlay 'after-string))
                        (cl-return-from done nil))))))
              (define-key keymap (kbd "<header-line> <mouse-1>")
                          (lambda (event)
                            (interactive "e")
                            (select-window (posn-window (event-start event)))
                            (setf (window-start) begin)
                            (goto-char begin)))
              (setf string (propertize string
                                       'keymap keymap
                                       'mouse-face 'underline))
              (push string titles)))
          (setf current (org-element-lineage current 'headline)))

        (when (and number-prefix titles)
          (setf (car titles)
                (concat (propertize number-prefix 'face '(shadow org-level-1)) (car titles))))
        titles)))

  (setf org-startup-numerated t)
  (setf org-startup-indented t)
  (setf org-indent-indentation-per-level 0)
  (setf org-hide-emphasis-markers t)
  (advice-add 'org-fill-paragraph :around 'my-fill-advice)

  (use-package consult
    :defer t
    :commands (consult-org-heading))

  (my-define-keymap my-org-mode-map
    "h" 'consult-org-heading)
  (keymap-set org-mode-map my-mode-prefix 'my-org-mode-map)

  ;; https://web.archive.org/web/20241226010147/https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  (my-mode-setup org-mode-hook
    (org-bullets-mode 1)
    (setq-local line-spacing 0.25)
    (setq-local header-line-format my-org-header-line)
    (setq-local mode-line-position-line-format "")
    (setq-local mode-line-position-column-format "")
    (setq-local mode-line-position-column-line-format "")
    (face-remap-add-relative 'header-line 'fringe '(:height 0.75) 'default)))

(use-package org-bullets
  :defer t
  :diminish org-bullets-mode)

;; ===============================
;; Looks
;; ===============================

(defvar my-frame-tweaks-applied? nil)

(defun my-frame-tweaks ()
  (setf my-frame-tweaks-applied? t)
  (load-theme 'doom-bluloco-dark t)
  (apply 'custom-theme-set-faces
         'user
         (append
          (when (x-list-fonts "Fira Code")
            '((fixed-pitch ((t (:family "Fira Code" :height 120))))
              (default ((t (:family "Fira Code" :height 120))))))
          (when (x-list-fonts "ETBembo")
            '((variable-pitch ((t (:family "ETBembo" :height 160 :weight thin))))))
          '((fringe ((t (:inherit mode-line-inactive))))))))

(if (and (daemonp) (not my-frame-tweaks-applied?))
    (add-hook 'server-after-make-frame-hook 'my-frame-tweaks)
  (when (display-graphic-p)
    (my-frame-tweaks)))

;; ===============================
;; Olivetti
;; ===============================

(use-package olivetti
  :defer t
  :diminish olivetti-mode
  :config
  (setf olivetti-style 'fancy))

;; ===============================
;; whitespace
;; ===============================

(use-package whitespace
  :defer t
  :config
  (setf whitespace-style
        '(trailing
          empty
          space-after-tab
          space-before-tab
          lines-tail
          indentation
          face)))

;; ===============================
;; journalctl-mode
;; ===============================

(use-package journalctl-mode
  :defer t
  :init
  (keymap-set my-global-map "J J" 'journalctl))
