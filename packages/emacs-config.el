;; -*- lexical-binding: t -*-

;; https://github.com/jwiegley/use-package/issues/436
(require 'use-package)

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
  (completion-styles '(orderless basic)))

;; ===============================
;; consult
;; ===============================

(use-package consult
  :bind (("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-]" . consult-grep)
         ("C-x C-d" . consult-find)))

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

(use-package define-word
  :bind (("M-#" . define-word-at-point)))

(use-package undo-tree
  :demand t
  :config
  (progn
    (global-undo-tree-mode 1)
    (diminish 'undo-tree-mode)
    (setf undo-tree-auto-save-history nil)))

(use-package smartparens
  :demand t
  :config
  (progn
    (setf sp-autoskip-closing-pair t)
    (sp-use-paredit-bindings)
    (smartparens-global-mode 1)
    (show-smartparens-global-mode 1)
    (setf sp-highlight-pair-overlay nil)
    (diminish 'smartparens-mode)))

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
;; slime
;; ===============================

(use-package slime
  :after lisp-mode
  :defer t
  :config
  (setf slime-contribs '(slime-fancy)))

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
  :config
  (global-git-gutter-mode t))

(use-package git-gutter-fringe
  :after git-gutter)

(use-package vc
  :defer t
  :config
  (setf vc-git-resolve-conflicts nil))

;; ===============================
;; Org
;; ===============================

(use-package org
  :mode (("\\.org$" . org-mode))
  :config
  (add-hook 'org-mode-hook 'org-indent-mode))

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
