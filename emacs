(defvar my-packages '(company ;; autocomplete
                      company-quickhelp ;; methods and variables info in autocomplete
                      neotree ;; file tree by F3
                      evil    ;; vim bindings
                      evil-nerd-commenter ;; commenter 
                      evil-leader  ;; vim leader button
                      evil-search-highlight-persist ;; vim search highlight
                      projectile ;; fuzzy find files
                      gradle-mode ;; build gradle project
                      groovy-mode ;; groovy syntax
                      autopair ;; autopair bracers 
                      smex ;; ido-style M-x complete
                      idomenu  ;; find methods and variables in current buffer (Ctrl-P)
                      ido-vertical-mode ;; ido menu is vertical now
                      powerline ;; powerline mode
                      powerline-evil ;; powerline with vim bindings
                      idea-darkula-theme ;; color theme like intellij idea
                      ))

(require 'cl)
(require 'package)

;;package repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;local libs and themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/lisp/")


(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(if (display-graphic-p)
;;    (progn
      ;; if graphic
      (load-theme 'caroline t))
  ;; else 
;;  (load-theme 'noctilux t))

(require 'smart-tab)
(global-smart-tab-mode 1)

;; company quickhelp
(company-quickhelp-mode 1)

;; nerd commenter
(global-set-key (kbd "C-\\") 'evilnc-comment-or-uncomment-lines)
(evilnc-default-hotkeys)

;; evil mode
(require 'evil)
(evil-mode 1)
(eval-after-load "evil-maps"
  (define-key evil-normal-state-map "\C-n" nil))
(eval-after-load "evil-maps"
  (define-key evil-normal-state-map "\C-p" nil))

;; evil search persist highlight
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; smartline (powerline analog)
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-center-color-theme)
(setq powerline-arrow-shape 'curve)

;; neotree file browser
(require 'neotree)
(global-set-key [f3] 'neotree-toggle)

;;projectile file fuzzy search
(projectile-global-mode)
(setq projectile-completion-system 'ido)
(global-set-key (kbd "C-n") 'projectile-find-file) ;; popup projectile find file 

;;smex (ido-style M-x menu)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex) ;; enabling smex M-x
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x.

;; ido - completion for everything
(require 'ido-vertical-mode)
;; ido pretty menu
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "orange")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)
;; alt-j and alt-k for navigation
(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "M-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-k") 'ido-prev-match))
(add-hook 'ido-setup-hook #'bind-ido-keys)
(global-set-key (kbd "C-p") 'idomenu) ;; popup idomenu
;; turn it on
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; company-mode (autocomplete engine)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode) ;; company always enabled
(setq-default company-minimum-prefix-length 2   ;; minimum prefix character number for auto complete.
              company-idle-delay 0    ;; show tooltip without delay
              company-echo-delay 0    ;; show inline tooltip without delay
              company-show-numbers nil  ;; show numbers in autocomplete popup
              company-selection-wrap-around t ;; loop over candidates
              )
(eval-after-load 'company  '(progn
                              (define-key company-active-map (kbd "TAB") 'company-select-next)
                              (define-key company-active-map [tab] 'company-select-next)))

(add-to-list 'company-backends 'company-shell) ;; shell script completion

;;autopair
(autopair-global-mode)

(define-key global-map (kbd "RET") 'newline-and-indent) ;; autoindent
(setq-default indent-tabs-mode nil) ;; disable tabs

(menu-bar-mode -1) ;; disable menu
(tool-bar-mode -1) ;; disable tool-bar
(show-paren-mode 1) ;; pair bracers
(column-number-mode 1) ;; line numbers
(global-linum-mode 1) 
;; (global-hl-line-mode 1) ;; current line

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

(setq inhibit-startup-message t) ;; disable start message
(setq frame-title-format "emacs") ;; set title format

;;gradle errors highlight
(add-to-list 'compilation-error-regexp-alist
             '("^\[ERROR\] \(.*\):\[\([0-9]+\),\([0-9]+\)\]" 1 2 3))
