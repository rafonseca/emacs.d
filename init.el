
(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(cl-pushnew "~/.emacs.d/lisp" load-path)

;;; General UI
(load-theme 'modus-vivendi)


(setq inhibit-splash-screen t)
(setq initial-major-mode 'emacs-lisp-mode)

;; Lispy mode
(use-package lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :bind (("S-SPC" . "(")
	 :map lispy-mode-map
	 ("C->" . lispy-forward-slurp-sexp)
	 ("C-<" . lispy-backward-slurp-sexp)))

(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Minibuffer
(savehist-mode)
(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

;; Completion
(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager


;(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well
(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)


;; Other UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'visual-line-mode)


(menu-bar-mode 0)
(tool-bar-mode 0)
(recentf-mode t)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


;;; Set shell path
(setq shell-command-switch "-ic")
;; -i switch enforces .bashrc source, where PATH is configured
(use-package exec-path-from-shell
  :ensure t)
(when (daemonp)
  (exec-path-from-shell-initialize))
;;; Packages


(use-package magit
  :bind (("<f7>" . 'magit))
  :ensure t)
(use-package forge
  :ensure t)


(use-package yasnippet
  :config (yas-global-mode 1)
  :ensure t)

;;; General

(use-package consult
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline, consult-error
  :bind (("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ("M-s M-s" . consult-ripgrep)
         ("C-s" . consult-line)) ;; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  :ensure t)

(use-package embark
  :after avy
  :bind (("C-c a" . embark-act))
  :ensure t)

(use-package embark-consult
  :ensure t)

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :init
  (fido-mode -1)
  (vertico-mode)
  :ensure t)


;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;;Popup completion-at-point
(use-package corfu
  :ensure t
  :config
  (global-corfu-mode))
 
;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :ensure t
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package eshell
  :ensure t
  :bind (("C-r" . consult-history)))

(use-package vterm
  :ensure t)

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package aggressive-indent
  :ensure t
  :config
  (aggressive-indent-mode))


;; Python
(use-package poetry
  :ensure t)
(use-package blacken
  :ensure t)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
		 ,(eglot-alternatives '("pylsp" "pyls" ("poetry" "run" "pyright-langserver" "--stdio")  ("pyright-langserver" "--stdio") "jedi-language-server")))))



(define-key global-map (kbd "C-q")
	    (defun kill-window-and-buffer ()
	      (interactive)
	      (kill-buffer)
	      (delete-window)))

(define-key global-map (kbd "M-w")
	    (defun mark-or-copy ()
	      (interactive)
	      (if (use-region-p)
		  (kill-ring-save nil nil t)
		(mark-sexp))))
;;; Org


(setq org-todo-keywords '((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)" "FAILED(f)")))

(add-hook
 'org-after-todo-state-change-hook
 (defun clock-in-when-doing ()
   (if (string-equal "DOING" (org-get-todo-state))
       (org-clock-in)))
 )

(setq org-duration-format 'h:mm)
(setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)(sql . t)))

(setq org-confirm-babel-evaluate nil)


;; My packages

(require 'path-complete)

;; Commitizen hack
(defvar cz-scope-history nil)
(defvar cz-msg-history nil)
(defvar cz-types '("feat" "fix" "refactor" "build" "ci" "perf" "docs" "revert" "style" "test" "chore"))

(defun cz-msg (convention scope msg &optional breaking)
  (interactive
   (list (completing-read "Type: " cz-types)
	 (completing-read "Scope: " cz-scope-history nil nil nil 'cz-scope-history)
	 (completing-read "Message: " cz-msg-history nil nil nil 'cz-msg-history)
	 ))
  (insert (format "%s(%s)%s: %s" convention scope (if current-prefix-arg "!" "") msg)))

(keymap-set git-commit-mode-map "C-c C-z" 'cz-msg)




