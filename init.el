
(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(cl-pushnew "~/.emacs.d/lisp" load-path)

;;; General UI
(load-theme 'tsdh-dark)
(setq inhibit-splash-screen t)
(setq initial-major-mode 'emacs-lisp-mode) 


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

(electric-pair-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(recentf-mode t)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))




;;; Packages

(use-package magit
  :bind (("<f7>" . 'magit)))
(use-package forge)

(use-package wgrep)
(use-package devdocs)
(use-package htmlize)
(use-package which-key
  :config
  (which-key-mode))
(use-package yasnippet
  :config (yas-global-mode 1))

;;; General
(use-package avy
  :bind (("M-j" . avy-goto-char)))

(use-package consult
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline, consult-error
  :bind (("C-x b" . consult-buffer) ;; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ("C-s" . consult-line))    ;; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :after avy
  :bind (("C-c a" . embark-act))
  )

(use-package embark-consult)

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :init
  (fido-mode -1)
  (vertico-mode))


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
()

(use-package eshell
  :bind (("C-r" . consult-history)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package aggressive-indent
  :config
  (aggressive-indent-mode))



;; Python
(use-package poetry)
(use-package blacken)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
		 ,(eglot-alternatives '("pylsp" "pyls" ("poetry" "run" "pyright-langserver" "--stdio")  ("pyright-langserver" "--stdio") "jedi-language-server")))))
(define-key python-mode-map (kbd "M-<left>")
	    (defun move-backward-symbol ()
	      (interactive)
	      (let ((stuff
		     (and (region-active-p)
			  (delete-and-extract-region (point) (mark)))))
		(forward-symbol -1)
		(set-mark (point))
		(when stuff
		  (insert stuff))
		(activate-mark))
	      ))

(define-key python-mode-map (kbd "M-<right>")
	    (defun move-forward-symbol ()
	      (interactive)
	      (let ((stuff
		     (and (region-active-p)
			  (delete-and-extract-region (point) (mark)))))
		(forward-symbol 1)
		(set-mark (point))
		(when stuff
		  (insert stuff))
		(activate-mark))
	      ))


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



(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(setq org-confirm-babel-evaluate nil)

;; My packages

(require 'dired-tweaks)
(require 'db-credentials)
(require 'path-complete)
