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

;;; Key remap

(defmacro my-define-keymap (name &rest args)
  (declare (indent defun))
  `(progn
     (defvar-keymap ,name)
     (define-keymap :keymap ,name
       ,@args)
     (fset ',name ,name)))

(my-define-keymap my-rectangle-mark-map
  "t" 'string-rectangle
  "k" 'kill-rectangle)

(my-define-keymap my-narrow-map
  :parent narrow-map)

(my-define-keymap my-project-prefix-map
  :parent project-prefix-map)

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

(my-define-keymap my-global-map
  "r" 'my-rectangle-mark-map
  "n" 'my-narrow-map
  "p" 'my-project-prefix-map)

(keymap-set global-map my-global-prefix 'my-global-map)
(keymap-set function-key-map "<f1>" my-global-prefix)
(keymap-unset global-map "<f1>")

(my-define-keymap my-local-map)

(keymap-set global-map my-mode-prefix 'my-local-map)
(keymap-set function-key-map "<f2>" my-mode-prefix)
(keymap-unset global-map "<f2>")

(defun pinky-saver (original-key &optional original-keymap new-key new-keymap)
  (let* ((original-keymap (or original-keymap global-map))
         (new-keymap (or new-keymap global-map))
         (old-command (keymap-lookup original-keymap original-key))
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
      (let ((last-execution nil))
        (lambda ()
          (interactive)
          (if (and last-execution (time-less-p (time-since last-execution) 10))
              (call-interactively old-command)
            (let ((other-key (where-is-internal old-command nil t nil nil)))
              (message (propertize "Ow!  Your pinky!  Use %s instead, or repeat the command" 'face '(error (:height 2.0)))
                       (if other-key
                           (key-description other-key)
                         (format "%s %s"
                                 (key-description (where-is-internal 'execute-extended-command nil t))
                                 old-command))))
            (setf last-execution (float-time))
            nil))))
    (keymap-set original-keymap original-key new-name)
    (when new-key
      (keymap-set new-keymap new-key old-command))))

(progn ;; Define pinky-savers
  ;; Buffers
  (pinky-saver "C-x C-s" nil "s" my-global-map)
  (pinky-saver "C-x C-w" nil "w" my-global-map)
  (pinky-saver "C-x C-f" nil "f" my-global-map)
  (pinky-saver "C-x b"   nil "b" my-global-map)
  (pinky-saver "C-x k"   nil "k" my-global-map)

  ;; Windows
  (pinky-saver "C-x 1"   nil "1" my-global-map)
  (pinky-saver "C-x 2"   nil "2" my-global-map)
  (pinky-saver "C-x 3"   nil "3" my-global-map)
  (pinky-saver "C-x 0"   nil "0" my-global-map)
  (pinky-saver "C-x +"   nil "+" my-global-map)
  (pinky-saver "C-x o"   nil "o" my-global-map)

  ;; Text-size
  (pinky-saver "C-x C-M-=" nil "="   my-global-map)
  (pinky-saver "C-x C-M--" nil "-"   my-global-map)
  (pinky-saver "C-x C-="   nil "M-=" my-global-map)
  (pinky-saver "C-x C--"   nil "M--" my-global-map)

  ;; Keyboard macros
  (pinky-saver "C-x ("   nil "(" my-global-map)
  (pinky-saver "C-x )"   nil ")" my-global-map)
  (pinky-saver "C-x e"   nil "e" my-global-map)

  ;; Goto line
  (pinky-saver "M-g M-g" nil "j" my-global-map)
  (pinky-saver "M-g g")

  ;; Misc
  (pinky-saver "M-$"     nil "$"   my-global-map)
  (pinky-saver "M-%"     nil "%"   my-global-map)
  (pinky-saver "C-M-%"   nil "M-%" my-global-map))

;;; repeat

(use-package repeat
  :config
  (repeat-mode 1)
  :demand t)

;;; completion

(use-package vertico
  :config (vertico-mode 1)
  :demand t)

(use-package marginalia
  :config (marginalia-mode)
  :demand t)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(substring orderless basic)))

;;; company

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

;;; consult

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
  (keymap-set my-global-map "O" 'consult-outline)
  (keymap-set global-map "<remap> <goto-line>" 'consult-goto-line)
  (keymap-set global-map "<remap> <switch-to-buffer>" 'consult-buffer)

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

;;; rgrep-fast

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

;;; mouse

(when (window-system)
  (setq mouse-wheel-scroll-amount '(1))
  (setq mouse-wheel-progressive-speed nil))

;;; misc stuff

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (setf which-key-show-early-on-C-h t)
  (setf which-key-idle-secondary-delay 0.05)
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

(defun my-buffer-flag ()
  (cond
   ((buffer-modified-p)
    '(:propertize "*" face font-lock-warning-face))
   (buffer-read-only
    ;; Don't forget that strings are checked for format directives!
    "%%")
   (t
    " ")))

(defun my-remote-flag ()
  (if (and default-directory (file-remote-p default-directory))
      "@"
    ""))

(defun my-eol-flag ()
  (pcase (coding-system-eol-type buffer-file-coding-system)
    (0 "")
    (1 '(:propertize "W" face font-lock-keyword-face))
    (2 '(:propertize "!" face font-lock-warning-face))))

(defvar-local my-project-mode-line-cache nil)
(defvar my-project-mode-line-cache-lifespan 60)

(defun my-project-mode-line-format ()
  ;; It seems that project-mode-line-format doesn't cache if the
  ;; buffer doesn't have an associated project.  This is a problem
  ;; because the mode line is refreshed... a lot.  Let's just cache
  ;; the result ourselves.
  (when (or (null my-project-mode-line-cache)
            (< my-project-mode-line-cache-lifespan
               (abs (- (float-time)
                       (cdr my-project-mode-line-cache)))))
    (let* ((formatted (project-mode-line-format))
           (formatted (when (and formatted
                                 (or (not dired-directory)
                                     (not (equal (expand-file-name (project-root (project-current)))
                                                 (expand-file-name default-directory))))
                                 (not (derived-mode-p '(magit-mode))))
                        formatted)))
      (setf my-project-mode-line-cache (cons formatted (float-time)))))
  (car my-project-mode-line-cache))

(defvar-local my-mode-name nil)
(put 'my-mode-name 'risky-local-variable t)
(defvar-local my-mode-name-previous nil)

(defun my-mode-name ()
  ;; Bah.  mode-line-format doesn't support nested :propertize forms.
  ;; Despite what the documentation says, the innermost :propertize
  ;; form is the only one that gets to set text properties.
  ;; Properties set via outer forms are completely lost.  If a
  ;; mode-name wants to, for example, set the help-echo property, that
  ;; means we can't apply a face to it.  Let's hack it.  We'll format
  ;; mode-name and then apply the face afterwards.  Note that we can't
  ;; use the face parameter to format-mode-line, since it has the
  ;; exact same issue as nested :propertize forms.

  ;; Since mode lines are rendered very frequently, we should cache
  ;; the result of propertizing the mode name.
  (if (eql my-mode-name-previous mode-name)
      'my-mode-name
    (setf my-mode-name (propertize (format-mode-line mode-name) 'face 'font-lock-builtin-face))
    (setf my-mode-name-previous mode-name)
    'my-mode-name))

(defun my-minor-mode-line ()
  (cons ""
        (cl-loop
         for entry in minor-mode-alist
         for key = (car entry)
         when (and (symbol-value key) (cadr entry) (not (equal "" (cadr entry))))
         collect `(:propertize
                   ,(cadr entry)
                   help-echo ,(symbol-name key)
                   mouse-face mode-line-highlight
                   keymap ,(let ((k (make-sparse-keymap))
                                 (fn key))
                             (keymap-set k "<mode-line> <mouse-1>"
                                         (lambda ()
                                           (interactive)
                                           (describe-minor-mode-from-symbol fn)))
                             k)))))

(defvar-local my-vc-mode nil)
(put 'my-vc-mode 'risky-local-variable t)
(defun my-vc-mode ()
  (setf my-vc-mode
        (propertize vc-mode
                    'face 'font-lock-keyword-face
                    'mouse-face 'mode-line-highlight
                    'keymap (let ((k (make-sparse-keymap)))
                              (keymap-set k "<mode-line> <mouse-1>" 'magit-status)
                              k)))
  'my-vc-mode)

(setq-default
 mode-line-format
 `("%e" " " (:eval (my-buffer-flag)) (:eval (my-remote-flag)) (:eval (my-eol-flag)) " "
   (:propertize "%b"
                face mode-line-buffer-id
                mouse-face mode-line-highlight
                help-echo "Open dired"
                keymap ,(let ((k (make-sparse-keymap)))
                          (keymap-set k "<mode-line> <mouse-1>" (lambda ()
                                                                  (interactive)
                                                                  (dired default-directory)))
                          k))
   " "
   (line-number-mode
    (column-number-mode (7 "%l:%c ") (3 "%l "))
    (3 (column-number-mode ":%c ")))
   " " (:eval (my-mode-name)) mode-line-process
   "  " mode-line-percent-position
   (size-indication-mode "  %I ")
   (:eval (my-minor-mode-line))
   mode-line-format-right-align
   (project-mode-line (:propertize (:eval (my-project-mode-line-format))
                                   face font-lock-constant-face
                                   mouse-face mode-line-highlight
                                   help-echo "Show project dired"
                                   keymap ,(let ((k (make-sparse-keymap)))
                                             (keymap-set k "<mode-line> <mouse-1>" 'project-dired)
                                             k)))
   (vc-mode (:eval (my-vc-mode)))
   " "))

(setq-default mode-line-right-align-edge 'right-fringe)

(setq-default vc-display-status 'no-backend)
(setf project-mode-line t)

(defun my-move-beginning-of-line (arg)
  (interactive "^p")
  ;; move-beginning-of-line (which we are imitating) says that 1 means
  ;; "this line".
  (let ((line-movement (1- (or arg 1))))
    (cond
     ((not (zerop line-movement))
      (forward-line line-movement)
      (back-to-indentation))

     (line-move-visual
      (let* ((indentation
              (save-excursion
                (back-to-indentation)
                (point)))
             (line-start
              (save-excursion
                (beginning-of-visual-line 1)
                (point))))
        (if (equal (point) indentation)
            (goto-char line-start)
          (goto-char (max indentation line-start)))))

     (t
      (let ((start (point)))
        (back-to-indentation)
        (when (equal (point) start)
          (move-beginning-of-line 1)))))))

(keymap-set global-map
            "<remap> <beginning-of-visual-line>"
            'my-move-beginning-of-line)

(keymap-set global-map
            "<remap> <move-beginning-of-line>"
            'my-move-beginning-of-line)

(keymap-set visual-line-mode-map
            "<remap> <move-beginning-of-line>"
            'my-move-beginning-of-line)

(defun my-quick-kill-line (deletep)
  ;; This is a bit silly.  This function implements the behavior I'd
  ;; like to have when pressing C-w or M-w without an active region.
  ;; In particular, I want it to kill the current line.  However, some
  ;; modes (e.g. paredit) enforce a sort of consistency on the buffer
  ;; and don't want you to delete indiscriminately.  In these modes,
  ;; C-k is overridden to do something fancier.  So, to kill the line,
  ;; we use whatever C-k is bound to.  But, we don't actually want to
  ;; use C-k since it doesn't respect kill-ring-deindent-mode and
  ;; other buffer filters.

  ;; First, we go to the "start" of the line.  Then we perform C-k
  ;; while in read-only mode.  This moves the cursor to the end of the
  ;; region to be killed.  Then, we use kill-region to perform the
  ;; kill.  If, after performing the kill, we're on a line containing
  ;; nothing but whitespace, then kill the rest of the line (which can
  ;; happen in paredit-mode for example).
  (save-excursion
    (let* ((start (progn (if kill-ring-deindent-mode
                             (back-to-indentation)
                           (beginning-of-line 1))
                         (point)))
           (end (let ((kill-whole-line nil)
                      (buffer-read-only t)
                      (kill-read-only-ok t)
                      (inhibit-message t)
                      (kill-transform-function (lambda (_string) nil)))
                  (call-interactively (keymap-lookup nil "C-k"))
                  (point))))
      (if deletep
          (progn
            (kill-region start end)
            (kill-append "\n" nil)
            (when (looking-at "\n")
              (beginning-of-line 1)
              (when (looking-at (rx (* blank) "\n"))
                (let ((kill-whole-line t)
                      (kill-transform-function (lambda (_string) nil)))
                  (kill-line)))))
        (kill-ring-save start end)
        (kill-append "\n" nil)
        (message "Copied")
        (pulse-momentary-highlight-region start end)))))

(defun my-kill-region ()
  (interactive)
  (if (not mark-active)
      (my-quick-kill-line t)
    (call-interactively 'kill-region)))

(defun my-kill-ring-save ()
  (interactive)
  (if (not mark-active)
      (my-quick-kill-line nil)
    (call-interactively 'kill-ring-save)))

(keymap-set global-map "<remap> <kill-region>" 'my-kill-region)
(keymap-set global-map "<remap> <kill-ring-save>" 'my-kill-ring-save)

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

(use-package pulse
  :defer t
  :init
  (defun my-highlight-current-line (&rest _args)
    (when (called-interactively-p 'interactive)
      (pulse-momentary-highlight-one-line)))

  (defun my-highlight-region (&rest _args)
    (when (called-interactively-p 'interactive)
      (let ((beginning (point))
            (end (mark)))
        (when (> beginning end)
          (cl-rotatef beginning end))
        (pulse-momentary-highlight-region beginning end))))

  (advice-add 'recenter-top-bottom :after 'my-highlight-current-line)
  (advice-add 'yank :after 'my-highlight-region)
  (advice-add 'yank-pop :after 'my-highlight-region))

;;; project

(use-package project
  :defer t
  :config
  (defun my-is-nix-path? (path)
    (cl-block done
      (save-match-data
        (unless (string-match (rx (group "/nix/store/" (* "/") (+ (not "/"))) "/") path)
          (cl-return-from done))

        (let* ((match (match-string 1 path))
               (base-name (file-name-nondirectory (directory-file-name match)))
               (name (progn
                       (string-match (rx (+ alnum) "-" (group (+ anychar))) base-name)
                       (concat "nix:" (match-string 1 base-name)))))
          (list 'my-nix-path match name)))))

  (add-to-list 'project-find-functions 'my-is-nix-path?)

  (cl-defmethod project-root ((project (head my-nix-path)))
    (nth 1 project))

  (cl-defmethod project-name ((project (head my-nix-path)))
    (nth 2 project)))

;;; fringe

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

;;; Flyspell

(use-package flyspell
  :hook text-mode
  :hook (prog-mode . flyspell-prog-mode)
  :diminish flyspell-mode
  :diminish flyspell-prog-mode
  :config
  (keymap-set flyspell-mouse-map "<mouse-2>" nil)
  (keymap-set flyspell-mouse-map "<mouse-3>" 'flyspell-correct-word))

;;; c

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

;;; lisp

(use-package slime
  :after lisp-mode
  :defer t
  :config
  (slime-setup '(slime-fancy slime-company slime-trace-dialog slime-xref-browser))

  (defun my-after-slime-show-description (&rest _rest)
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
  (keymap-set lisp-mode-map my-mode-prefix 'my-lisp-mode-map)

  (defun my-slime-flash-region (start end &optional _duration)
    (pulse-momentary-highlight-region start end))
  (advice-add 'slime-flash-region :override 'my-slime-flash-region))

(use-package lisp-mode
  :defer t
  :config
  (my-mode-setup lisp-mode-hook
    (setf my-indent-after-yank t)))

(use-package paredit
  :diminish paredit-mode
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (lisp-mode . paredit-mode))
  :config
  (my-mode-setup paredit-mode
    (setq-local kill-whole-line t)))

(use-package elisp-mode
  :defer t
  :config
  (my-mode-setup emacs-lisp-mode-hook
    (setf my-indent-after-yank t))

  (defun my-compile-defun ()
    (interactive)
    (save-excursion
      (let ((end (progn
                   (end-of-defun)
                   (point)))
            (start (progn
                     (beginning-of-defun)
                     (point))))
        (pulse-momentary-highlight-region start end)))
    (call-interactively 'compile-defun))

  (my-define-keymap my-emacs-lisp-mode-map
    "e" 'eval-last-sexp
    "l" 'load-file
    "c" 'my-compile-defun
    "r" 'eval-region)
  (keymap-set emacs-lisp-mode-map my-mode-prefix 'my-emacs-lisp-mode-map))

;;; macOS

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

;;; Git

(use-package magit
  :commands (magit-status magit-file-dispatch)
  :init
  (my-define-keymap my-magit-map
    "g" 'magit-status
    "f" 'magit-file-dispatch)
  (keymap-set my-global-map "g" 'my-magit-map)
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

;;; Org

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
    (face-remap-add-relative 'header-line 'fringe '(:height 1.25) 'default)
    (face-remap-add-relative 'fixed-pitch '(:height .8))))

(use-package org-bullets
  :defer t
  :diminish org-bullets-mode)

;;; Looks

(defvar my-frame-tweaks-applied? nil)

(defun my-frame-tweaks ()
  (setf my-frame-tweaks-applied? t)
  (load-theme 'doom-bluloco-dark t)
  (apply 'custom-theme-set-faces
         'user
         (append
          (when (x-list-fonts "Fira Code")
            '((fixed-pitch ((t (:family "Fira Code" :inherit default))))
              (default ((t (:family "Fira Code" :height 120))))))
          (when (x-list-fonts "ETBembo")
            '((variable-pitch ((t (:family "ETBembo" :height 1.25 :inherit default))))))
          '((fringe ((t (:inherit mode-line-inactive))))
            (header-line ((t (:inherit (mode-line) :height 1.25))))
            (mode-line ((t (:inherit (fixed-pitch) :height 0.8))))
            (mode-line-active ((t (:inherit (mode-line)))))
            (mode-line-inactive ((t (:inherit (shadow mode-line)))))
            (mode-line-buffer-id ((t :height 1.25)))))))

(if (and (daemonp) (not my-frame-tweaks-applied?))
    (add-hook 'server-after-make-frame-hook 'my-frame-tweaks)
  (when (display-graphic-p)
    (my-frame-tweaks)))

;;; Olivetti

(use-package olivetti
  :defer t
  :diminish olivetti-mode
  :config
  (setf olivetti-style 'fancy))

;;; whitespace

(use-package whitespace
  :defer t
  :hook ((prog-mode . my-maybe-whitespace-mode)
         (org-mode . my-maybe-whitespace-mode))
  :init
  (defun my-maybe-whitespace-mode ()
    (unless buffer-read-only
      (whitespace-mode)))

  (my-define-keymap my-whitespace-map
    "m" 'whitespace-mode
    "c" 'my-whitespace-cleanup-region
    "C-c" 'my-whitespace-cleanup)
  (keymap-set my-global-map "W" 'my-whitespace-map)

  (defun my-whitespace-cleanup-region (beginning end)
    (interactive
     (if mark-active
         (list (point) (mark))
       (save-excursion
         (list (progn
                 (move-beginning-of-line 1)
                 (point))
               (progn
                 (move-end-of-line 1)
                 (point))))))
    (when (> beginning end)
      (cl-rotatef beginning end))
    (whitespace-cleanup-region beginning end)
    (pulse-momentary-highlight-region beginning end)
    (message "Cleaned whitespace in region"))

  (defun my-whitespace-cleanup ()
    (interactive "@")
    (let ((mark-active nil))
      (whitespace-cleanup)
      (pulse-momentary-highlight-region (point-min) (point-max))
      (message "Cleaned whitespace in buffer")))

  :config
  (setf whitespace-style
        '(trailing
          empty
          space-after-tab
          space-before-tab
          indentation
          face)))

;;; journalctl-mode

(use-package journalctl-mode
  :defer t
  :init
  (my-define-keymap my-journalctl-map
    "J" 'journalctl)
  (keymap-set my-global-map "J" 'my-journalctl-map))

;;; Initial buffer

(defvar-local my-welcome-section-shortcuts
    '("a"
      "b"
      "c"
      "d"
      "e"
      "f"))

(defvar-local my-welcome-item-shortcuts
    '("1"
      "2"
      "3"
      "4"
      "5"
      "6"
      "7"
      "8"
      "9"
      "0"))

(defun my-welcome-projects ()
  (let ((projects (sort (project-known-project-roots)
                        :lessp 'string<)))
    (dolist (project projects)
      (let* ((project-lexical project)
             (start (point))
             (keymap (my-welcome-render-item
                      project
                      (lambda ()
                        (interactive)
                        (if (file-exists-p (file-name-concat project-lexical ".git"))
                            (magit-status-setup-buffer project-lexical)
                          (find-file project-lexical))))))
        (keymap-set keymap
                    "k"
                    (lambda ()
                      (interactive)
                      (project-forget-project project-lexical)
                      (save-excursion
                        (goto-char start)
                        (with-silent-modifications
                          (let ((inhibit-read-only t))
                            (move-beginning-of-line 2)
                            (delete-region start (point)))))))))))

(defun my-welcome-buffer-or-call (name function)
  (let ((buffer (get-buffer name)))
    (if buffer
        (switch-to-buffer buffer)
      (funcall function))))

(defun my-welcome-some-shell-entry (buffer-name spawn-function title)
  (let* ((action
          (lambda ()
            (interactive)
            (let ((default-directory "~"))
              (my-welcome-buffer-or-call buffer-name spawn-function))))
         (buffer (get-buffer buffer-name))
         (current-path
          (when buffer
            (buffer-local-value 'default-directory buffer))))
    (when current-path
      (set-text-properties 0 (length current-path) nil current-path))
    (my-welcome-render-item
     title action current-path)))

(defun my-welcome-shell-entry ()
  (my-welcome-some-shell-entry "*shell*" 'shell "Shell"))

(defun my-welcome-eshell-entry ()
  (my-welcome-some-shell-entry "*eshell*" 'eshell "Eshell"))

(defvar my-welcome-sections
  `(("Common"
     my-welcome-shell-entry
     my-welcome-eshell-entry
     ("Slime REPL" . slime)
     ("Home" .
      ,(lambda ()
         (interactive)
         (find-file "~")))
     ("Scratch" .
      ,(lambda ()
         (interactive)
         (switch-to-buffer "*scratch*"))))
    ("Projects" . my-welcome-projects)))

(defvar-local my-welcome-hidden-sections nil)
(defvar-local my-welcome-maximum-width 0)
(defvar-local my-welcome-section-keymap nil)

(defun my-welcome-render-item (item-name action &optional comment)
  (let ((line-keymap (make-sparse-keymap))
        (text-keymap (make-sparse-keymap))
        (line-start (point))
        link-end)

    (set-keymap-parent text-keymap line-keymap)
    (set-keymap-parent line-keymap my-welcome-section-keymap)
    (keymap-set text-keymap "<mouse-1>" action)
    (keymap-set line-keymap "<RET>" action)

    (insert (propertize "  " 'keymap line-keymap))
    (let* ((short-key (pop my-welcome-item-shortcuts)))
      (when short-key
        (insert (propertize short-key
                            'keymap line-keymap
                            'face 'shadow
                            'my-welcome-item-shortcut t
                            'display " "))
        (keymap-set my-welcome-section-keymap short-key
                    (lambda ()
                      (interactive)
                      (goto-char line-start)
                      (funcall action)))))

    (insert (propertize " " 'keymap line-keymap))

    (insert (propertize item-name 'mouse-face 'highlight 'keymap text-keymap 'face font-lock-doc-face))
    (setf link-end (point))
    (setf my-welcome-maximum-width
          (max my-welcome-maximum-width
               (string-pixel-width (buffer-substring line-start link-end))))

    (when comment
      (insert (propertize " " 'keymap line-keymap 'display '(space :align-to my-welcome-maximum-width)))
      (insert (propertize "    " 'keymap line-keymap))
      (insert (propertize comment 'keymap line-keymap 'face font-lock-comment-face)))
    (insert (apply 'propertize "\n" 'keymap line-keymap
                   (unless comment
                     '(rear-sticky nil))))

    line-keymap))

(defvar my-welcome-jump-keymap nil)

(defun my-welcome-render-section (id name contents)
  (let ((start (point))
        entries-start
        entries-end
        (my-welcome-item-shortcuts my-welcome-item-shortcuts)
        (my-welcome-section-keymap (make-sparse-keymap)))

    (let ((section-shortcut (pop my-welcome-section-shortcuts)))
      (if (not section-shortcut)
          (insert "  ")
        (insert (propertize (format "%s" section-shortcut)
                            'face 'shadow
                            'my-welcome-section-shortcut t))
        (insert " ")
        (keymap-set my-welcome-jump-keymap section-shortcut
                    (lambda ()
                      (interactive)
                      (goto-char start)))))
    (insert (propertize name 'face '(:inherit (font-lock-keyword-face) :weight bold)) "\n")
    (setf entries-start (point))

    (cond
     ((functionp contents)
      (funcall contents))

     ((consp contents)
      (dolist (item contents)
        (cond
         ((not (consp item))
          (funcall item))

         ((consp (car item))
          (my-welcome-render-item (caar item) (cdr item) (cdar item)))

         (t
          (my-welcome-render-item (car item) (cdr item))))))
     (t
      (error "Unrecognized section contents")))

    (setf entries-end (point))

    (put-text-property start entries-end 'my-welcome-section id)
    (put-text-property start entries-end 'cursor-sensor-functions
                       (list (lambda (_window _previous change)
                               (let ((inhibit-read-only t))
                                 (with-silent-modifications
                                   (save-excursion
                                     (save-restriction
                                       (widen)
                                       (narrow-to-region entries-start entries-end)
                                       (goto-char (point-min))
                                       (while (text-property-search-forward 'my-welcome-item-shortcut t nil)
                                         (let ((shortcut-start (point))
                                               shortcut-stop)
                                           (text-property-search-forward 'my-welcome-item-shortcut t t)
                                           (setf shortcut-stop (point))
                                           (if (eq change 'entered)
                                               (remove-text-properties shortcut-start shortcut-stop '(display))
                                             (add-text-properties shortcut-start shortcut-stop '(display " "))))))))))))

    (put-text-property entries-start entries-end 'my-welcome-entries id)

    (alter-text-property start entries-end 'keymap
                         (lambda (old)
                           (or old my-welcome-section-keymap)))))

(defun my-welcome-revert (&optional _ignore-auto _noconfirm)
  (with-silent-modifications
    (let ((inhibit-read-only t)
          (my-welcome-section-shortcuts my-welcome-section-shortcuts)
          (my-welcome-jump-keymap (make-sparse-keymap)))
      (widen)
      (delete-region (point-min) (point-max))
      (setq-local my-welcome-maximum-width 0)

      (insert "Its Emacs!\n\n")

      (cl-loop
       with first? = t
       for section in my-welcome-sections
       for id from 0 do
       (unless first?
         (insert "\n"))
       (my-welcome-render-section id (car section) (cdr section))
       (setf first? nil))

      (goto-char (point-min))
      (when (display-graphic-p)
        (setq-local my-welcome-maximum-width (list my-welcome-maximum-width)))
      (text-property-search-forward 'my-welcome-section 0)

      (use-local-map (make-composed-keymap (list my-welcome-mode-map my-welcome-jump-keymap))))))

(defvar-local my-welcome-mode nil)

(define-derived-mode my-welcome-mode special-mode
  (setq-local revert-buffer-function 'my-welcome-revert)
  (read-only-mode 1)
  (cursor-sensor-mode 1)
  (setf my-welcome-mode t)
  (my-welcome-revert))

(defun my-welcome-next-section ()
  (interactive)
  (let ((id (get-text-property (point) 'my-welcome-section))
        target)
    (save-excursion
      (if id
          (text-property-search-forward 'my-welcome-section (1+ id))
        (text-property-search-forward 'my-welcome-section nil t))
      (when (get-text-property (point) 'my-welcome-section)
        (setf target (point))))
    (if target
        (goto-char target)
      (message "No next section"))))

(defun my-welcome-previous-section ()
  (interactive)
  (let ((id (get-text-property (point) 'my-welcome-section))
        target)
    (save-excursion
      (if id
          (progn
            (text-property-search-backward 'my-welcome-section id t)
            (text-property-search-backward 'my-welcome-section (1- id) t))
        (text-property-search-backward 'my-welcome-section nil))
      (when (get-text-property (point) 'my-welcome-section)
        (setf target (point))))
    (if target
        (goto-char target)
      (message "No previous section"))))

(my-define-keymap my-welcome-mode-map
  "n" 'next-line
  "M-n" 'my-welcome-next-section
  "p" 'previous-line
  "M-p" 'my-welcome-previous-section)

(defun my-welcome-noselect ()
  (with-current-buffer (get-buffer-create "*my-welcome*")
    (if my-welcome-mode
        (my-welcome-revert)
      (my-welcome-mode))
    (current-buffer)))

(defun my-welcome ()
  (interactive)
  (with-current-buffer (pop-to-buffer "*my-welcome*")
    (if my-welcome-mode
        (my-welcome-revert)
      (my-welcome-mode))))

(setf initial-buffer-choice 'my-welcome-noselect)
