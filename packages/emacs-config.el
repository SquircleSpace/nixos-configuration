;; -*- lexical-binding: t -*-

;; https://web.archive.org/web/20201206011037/https://github.com/jwiegley/use-package/issues/436
(require 'use-package)

(require 'cl-lib)

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
  :preface
  (defun ada-company-capf-around-advice (orig-fun &rest args)
    ;; dynamic scoping saves the day!
    (let ((completion-styles '(basic partial-completion emacs22)))
      (apply orig-fun args)))
  :hook (after-init . global-company-mode)
  :config
  (advice-add 'company-capf :around #'ada-company-capf-around-advice))

;; ===============================
;; consult
;; ===============================

(use-package consult
  :bind
  (("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-]" . consult-grep)
   ("C-x C-d" . consult-find))
  :preface
  (setf completion-in-region-function
        ;; https://web.archive.org/web/20231120220521/https://github.com/minad/vertico#completion-at-point-and-completion-in-region
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

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

(use-package expand-region
  :bind ("C-=" . 'er/expand-region))

;; Some stuff stolen from Emacs prelude
(defadvice kill-ring-save (before smart-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-end-position)))))

(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

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

(defvar-local squircle-space-allow-unfill-on-visual-line-mode t)

(defun squircle-space-fill-advice (original &rest rest)
  (if (and squircle-space-allow-unfill-on-visual-line-mode
           visual-line-mode)
      (let ((fill-column (point-max)))
        (apply original rest))
    (apply original rest)))

(advice-add 'fill-paragraph :around 'squircle-space-fill-advice)

(use-package define-word
  :bind (("M-#" . define-word-at-point)))

(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode 1)
    (diminish 'undo-tree-mode)
    (setf undo-tree-auto-save-history nil)))

(use-package paredit
  :hook ((lisp-data-mode . paredit-mode)
         (lisp-mode . paredit-mode)))

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
  :hook (prog-mode . flyspell-prog-mode))

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
;; sly
;; ===============================

(use-package sly
  :after lisp-mode
  :defer t)

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
  :bind (("C-c s" . magit-status)
         ("C-c f" . magit-file-popup))
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

(defvar-local squircle-space-org-header-line-content nil)
(put 'squircle-space-org-header-line-content 'risky-local-variable t)

(defconst squircle-space-org-header-line
  '(:eval
    (progn
      ;; We need to bounce through a symbol to prevent emacs from
      ;; processing % constructs in the resulting string.
      (setf squircle-space-org-header-line-content (squircle-space-get-org-header-line))
      (list
       (propertize " " 'display '((space :align-to left-margin)))
       'squircle-space-org-header-line-content))))

(defconst squircle-space-org-header-line-back-to-top
  (progn
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap (kbd "<header-line> <mouse-1>") 'beginning-of-buffer)
      (concat
       (propertize "Back to top" 'keymap keymap 'face '(shadow org-level-1) 'mouse-face 'underline)))))

(defun squircle-space-get-org-header-line ()
  (cl-block return
    (let ((headings (squircle-space-get-org-headings)))
      (concat
       (propertize "⮤ " 'face 'shadow)
       (cond
        (headings
         (mapconcat 'identity
                    headings
                    (propertize " > " 'face 'shadow)))
        ((not (equal (window-start) (point-min)))
         squircle-space-org-header-line-back-to-top)
        (t
         (cl-return-from return nil)))))))

(defun squircle-space-org-ensure-title-fontified (string org-level)
  (if (get-text-property 0 'face string)
      string
    (propertize string 'face (if org-cycle-level-faces
		                 (nth (% (1- org-level) org-n-level-faces) org-level-faces)
		               (nth (1- (min org-level org-n-level-faces)) org-level-faces)))))

(defun squircle-space-get-org-headings ()
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
                 (string (squircle-space-org-ensure-title-fontified string (org-element-property :level current)))
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

(defun squircle-space-set-up-org-mode ()
  (setq-local line-spacing 0.25)
  (setq-local header-line-format squircle-space-org-header-line)
  (face-remap-add-relative 'header-line 'fringe '(:height 0.75) 'default))

(use-package org
  :mode (("\\.org$" . org-mode))
  :diminish org-num-mode
  :config
  (setf org-startup-numerated t)
  (setf org-startup-indented t)
  (setf org-indent-indentation-per-level 0)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (add-hook 'org-mode-hook 'squircle-space-set-up-org-mode)
  (setf org-hide-emphasis-markers t)
  (advice-add 'org-fill-paragraph :around 'squircle-space-fill-advice)
  ;; https://web.archive.org/web/20241226010147/https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(use-package org-bullets
  :defer t
  :diminish org-bullets-mode)

;; ===============================
;; Looks
;; ===============================

(defvar squircle-space-frame-tweaks-applied? nil)

(defun squircle-space-frame-tweaks ()
  (setf squircle-space-frame-tweaks-applied? t)
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

(if (and (daemonp) (not squircle-space-frame-tweaks-applied?))
    (add-hook 'server-after-make-frame-hook 'squircle-space-frame-tweaks)
  (squircle-space-frame-tweaks))

;; ===============================
;; Olivetti
;; ===============================

(use-package olivetti
  :defer t
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
