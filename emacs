(defvar my-packages '(company ;; autocomplete
                      company-quickhelp ;; methods and variables info in autocomplete
                      neotree ;; file tree by F3
                      evil    ;; vim bindings
                      evil-leader  ;; vim leader button
                      evil-search-highlight-persist ;; vim search highlight
                      evil-surround ;; vim surround functionality
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
                      linum-relative ;; relative line numbers 
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

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "\C-n" nil)
(define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\S-b" 'ido-switch-buffer)

;; evil search persist highlight
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; enable surround (try to push ysiW" to surround whole WORLD with ")
(require 'evil-surround)
(global-evil-surround-mode 1)

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
                              (define-key company-active-map (kbd "TAB") 'company-select-next) ;; choose candidate by tab
                              (define-key company-active-map (kbd "M-j") 'company-select-next) ;; alt-j
                              (define-key company-active-map (kbd "M-k") 'company-select-previous) ;; and alt-k
                              (define-key company-active-map [tab] 'company-select-next)))

(add-to-list 'company-backends 'company-shell) ;; shell script completion

;;autopair
(autopair-global-mode)

;; line numbers
(require 'linum-relative)
(setq linum-format "%4d \u2502 ")
(setq linum-relative-format "%4s \u2502 ")
(linum-relative-global-mode)
;; (global-linum-mode)

(define-key global-map (kbd "RET") 'newline-and-indent) ;; autoindent
(setq-default indent-tabs-mode nil) ;; disable tabs
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; disable scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; disable tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; disable menu-bar
(show-paren-mode 1) ;; pair bracers
(column-number-mode 1) ;; line numbers
(global-hl-line-mode 0) ;; current line
(set-face-background hl-line-face "light gray") ;; current line color

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

(setq inhibit-startup-message t) ;; disable start message
(setq frame-title-format "emacs") ;; set title format
(setq x-select-enable-clipboard nil) ;; enable clipboard
(fset 'evil-visual-update-x-selection 'ignore)

;;gradle errors highlight
(add-to-list 'compilation-error-regexp-alist
             '("^\[ERROR\] \(.*\):\[\([0-9]+\),\([0-9]+\)\]" 1 2 3))

;; email for wanderlust (disabled now)
;; (setq elmo-imap4-default-server "mail.nic.ru"
;;       elmo-imap4-default-user "agraschenkov@wildred.ru"
;;       elmo-imap4-default-authenticate-type 'clear
;;       elmo-imap4-default-port '993
;;       elmo-imap4-default-stream-type 'ssl
;;       elmo-imap4-use-modified-utf7 t)

;; (setq wl-smtp-connection-type 'ssl
;;       wl-smtp-posting-port 465
;;       wl-smtp-authenticate-type "plain"
;;       wl-smtp-posting-user "agraschenkov@wildred.ru"
;;       wl-smtp-posting-server "mail.nic.ru"
;;       wl-local-domain "nic.ru"
;;       wl-message-id-domain "mail.nic.ru")

;; (setq wl-from "Anton Graschenkov <agraschenkov@wildred.ru>"
;;       wl-fcc-force-as-read    t
;;       wl-default-spec "%")
