
(with-eval-after-load 'package
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package blacken)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) .
		 ,(eglot-alternatives '("pylsp" "pyls" ("poetry" "run" "pyright-langserver" "--stdio")  ("pyright-langserver" "--stdio") "jedi-language-server")))))
(use-package magit
  :bind (("<f7>" . 'magit)))
(use-package forge)

(use-package wgrep)
(use-package devdocs)
(use-package htmlize)
(use-package which-key
  :config
  (which-key-mode))

;; If you want to turn off the welcome screen, uncomment this
(setq inhibit-splash-screen t)

(setq initial-major-mode 'emacs-lisp-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))


;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; We won't set these, but they're good to know about
;;
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Use common keystrokes by default
;;(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))



(load-theme 'tsdh-dark)

(use-package poetry)


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
  ;; :init
  ;; ;; Add the option to run embark when using avy
  ;; (defun bedrock/avy-action-embark (pt)
  ;;   (unwind-protect
  ;;       (save-excursion
  ;;         (goto-char pt)
  ;;         (embark-act))
  ;;     (select-window
  ;;      (cdr (ring-ref avy-ring 0))))
  ;;   t))

  ;; ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; ;; candidate you select
  ;; (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

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
 
;; Part of corfu
;; (use-package corfu-popupinfo
;;   :after corfu
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom
;;   (corfu-popupinfo-delay '(0.25 . 0.1))
;;   (corfu-popupinfo-hide nil)
;;   :config
;;   (corfu-popupinfo-mode))

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
(electric-pair-mode)


;; my tweaks
(load-file "/home/rafonseca/.emacs.d/dired-tweaks.el")


(defun kill-window-and-buffer ()
  (interactive)
  (kill-buffer)
  (delete-window))
(define-key global-map (kbd "C-q") 'kill-window-and-buffer)

(defun mark-or-copy ()
  (interactive)
  (if (use-region-p)
      (kill-ring-save nil nil t)
      (mark-sexp)))

(defun my/project-search ()
  (interactive)
  (if (use-region-p)
      (let ((start (region-beginning))
            (end (region-end)))

           (+vertico/project-search nil (buffer-substring start end)))
      (+vertico/project-search)))

;; org
(use-package yasnippet
  :config (yas-global-mode 1))

(defun org-set-date-property ()
  (interactive)
  (org-set-property "date" (org-time-stamp nil)))


(setq org-todo-keywords '((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)" "FAILED(f)")))
(defun clock-in-when-doing ()
  (if (string-equal "DOING" (org-get-todo-state))
      (org-clock-in)))
(add-hook 'org-after-todo-state-change-hook 'clock-in-when-doing)
(menu-bar-mode 0)
(tool-bar-mode 0)
(recentf-mode t)

(use-package google-this
  :config (google-this-mode 1))
(setq browse-url-browser-function 'browse-url-chromium)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)))

(setq org-confirm-babel-evaluate nil)

;; My packages
(cl-pushnew "~/.emacs.d/" load-path)
(require 'db-credentials)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(htmlize poetry fish-mode google-this sqlformat blacken forge aggressive-indent rainbow-delimiters devdocs wgrep which-key vertico tangotango-theme orderless marginalia magit kind-icon embark-consult corfu-terminal company avy))
 '(use-package-always-ensure t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
