(defvar my-packages '(company ;; autocomplete
                      company-quickhelp ;; methods and variables info in autocomplete
                      neotree ;; file tree by F3
                      evil    ;; vim bindings
                      evil-leader  ;; vim leader button
                      evil-org     ;; vim bindings for org mode
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
                      yasnippet ;; snippets for emacs
                      ggtags ;; work with tags in large projects
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

;; load idea-like theme in graphics mode
(if (display-graphic-p)
    (progn
      (load-theme 'idea-darkula t)
      (set-default-font "Ubuntu Mono 12") 
    ))

;; company quickhelp
(company-quickhelp-mode 1)

;; enable evil leader button
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "f" 'find-file ;; find file
 "b" 'ido-switch-buffer ;; switch buffers with menu
 "k" 'kill-buffer ;; kill buffer
 "m" 'idomenu ;; goto method
 "c" 'projectile-find-file ;; goto project class (file)
 "<SPC>" 'evil-scroll-down ;; scroll down one screen
)

;; evil mode
(require 'evil)
(evil-mode 1)
(define-key evil-normal-state-map "\C-n" nil)
(define-key evil-normal-state-map "\C-p" nil)
(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
;; insert mode motoin
(define-key evil-insert-state-map "\M-k" 'evil-previous-visual-line)
(define-key evil-insert-state-map "\M-j" 'evil-next-visual-line)
(define-key evil-insert-state-map "\M-l" 'forward-char)
(define-key evil-insert-state-map "\M-h" 'backward-char)

;; evil search persist highlight
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; enable surround (try to push ysiW" to surround whole WORLD with ")
(require 'evil-surround)
(global-evil-surround-mode 1)

;; org-mode settings
(require 'evil-org)
(setq org-todo-keywords ;; TODO workflow
      '((sequence "TODO" "IN_PROGRESS" "|" "DONE")
        (sequence "ASSIGNED" "|" "DONE")
        (sequence "|" "HOLD")
        )) 
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("IN_PROGRESS" . "yellow")
        ("ASSIGNED" . "yellow")
        ("HOLD" . "red")
        ("DONE" . "green")))
(setq org-log-done 'time) ;; time on close todo entry

;; org-mode and redmine integration
(evil-leader/set-key-for-mode 'org-mode
  "i" 'org-insert-heading-after-current ;; insert heading after current
)
(evil-define-key 'normal evil-org-mode-map
  "gt" 'org-tracker-goto ;; open issue in redmine if possible
  "gc" 'org-tracker-create ;; create issue in redmine 
)

(setq redmine-url "http://trackerdev.openhd.ru") ;; redmine url

(defun redmine-open-issue (id) ;; open issue function
  (browse-url (concat redmine-url "/issues/" id)))

(defun redmine-create-issue (subject description) ;; create issue function
  (browse-url (url-encode-url(concat redmine-url "/issues/new?issue[subject]=" subject "&issue[description]=" description))))

(defun org-tracker-goto (&optional pom) ;; open current issue function
  (interactive)
  (org-with-point-at pom
    (redmine-open-issue  (org-entry-get nil "issue"))))

(defun org-tracker-create (&optional pom) ;; create issue function
  (interactive)
  (redmine-create-issue
   (org-get-heading 1 1)
   (org-get-entry)
   ))

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
(setq company-dabbrev-downcase 'nil)
(add-hook 'after-init-hook 'global-company-mode) ;; company always enabled
(setq-default company-minimum-prefix-length 2   ;; minimum prefix character number for auto complete.
              company-idle-delay 2    ;; show tooltip without delay
              company-echo-delay 0    ;; show inline tooltip without delay
              company-show-numbers nil  ;; show numbers in autocomplete popup
              company-selection-wrap-around t ;; loop over candidates
              )
(define-key company-active-map (kbd "TAB") 'company-select-next) ;; choose candidate by tab
(define-key company-active-map [tab] 'company-select-next) ;; choose candidate by tab
(define-key company-active-map (kbd "M-j") 'company-select-next) ;; alt-j
(define-key company-active-map (kbd "M-k") 'company-select-previous) ;; and alt-k
(define-key company-active-map (kbd "M-l") 'company-abort) ;; abort by M-l

(add-to-list 'company-backends 'company-shell) ;; shell script completion
(add-to-list 'company-backends 'company-capf) ;; org completion

(define-key evil-insert-state-map (kbd "C-<SPC>") 'company-complete)
(define-key evil-insert-state-map [tab] 'company-complete)

;;autopair
(autopair-global-mode)

;; yassnippet
(require 'yasnippet)
(yas-global-mode)
(yas-reload-all)
;; Completing point by some yasnippet key
(define-key yas-minor-mode-map (kbd "C-j") 'company-yasnippet)

;; gtags
(require 'ggtags)

;; COMMON SETTINGS

;; line numbers
(require 'linum-relative)
(setq linum-format "%5d \u2502 ") ;; line numbers 
(setq linum-relative-format "%5s \u2502 ") ;; line numbers
(linum-relative-global-mode) ;; relative line numbers
(column-number-mode 1) ;; line numbers
;; (global-linum-mode)

(define-key global-map (kbd "RET") 'newline-and-indent) ;; autoindent
(setq-default indent-tabs-mode nil) ;; disable tabs
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;; disable scroll-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ;; disable tool-bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;; disable menu-bar
(show-paren-mode 1) ;; pair bracers

(global-hl-line-mode 1) ;; current line
(set-face-background 'hl-line "#3e4446") ;; current line color
(set-face-foreground 'highlight nil) ;; syntax highlight in current line
(global-visual-line-mode 1) ;; wrap long lines

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

(setq inhibit-startup-message t) ;; disable start message
(setq frame-title-format "emacs") ;; set title format

;; set global clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(fset 'evil-visual-update-x-selection 'ignore)

;; gradle errors highlight
(add-to-list 'compilation-error-regexp-alist
             '("^\[ERROR\] \(.*\):\[\([0-9]+\),\([0-9]+\)\]" 1 2 3))

;; email client wanderlust (disabled now)
(setq elmo-imap4-default-server "mail.nic.ru"
      elmo-imap4-default-user "agraschenkov@wildred.ru"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t)

(setq wl-smtp-connection-type 'ssl
      wl-smtp-posting-port 465
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "agraschenkov@wildred.ru"
      wl-smtp-posting-server "mail.nic.ru"
      wl-local-domain "nic.ru"
      wl-message-id-domain "mail.nic.ru")

(setq wl-from "Anton Graschenkov <agraschenkov@wildred.ru>"
      wl-fcc-force-as-read    t
      wl-default-spec "%")

