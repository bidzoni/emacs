(defvar my-packages '(company ;; autocomplete
                      company-quickhelp ;; methods and variables info in autocomplete
                      company-web ;; web auto complete
                      company-jedi ;; company for jedi
                      neotree ;; file tree by F3
                      evil    ;; vim bindings
                      evil-leader  ;; vim leader button
                      evil-surround ;; vim surround functionality
                      evil-magit ;; evil git
                      evil-escape ;; excape from everywhere by pressing jk
                      evil-numbers ;; incremetn/decrement numbers in vim style
                      evil-org ;; evil org mode
                      ace-jump-mode ;; easy motion for emacs
                      projectile ;; fuzzy find files
                      gradle-mode ;; build gradle project
                      groovy-mode ;; groovy syntax
                      autopair ;; autopair bracers 
                      smex ;; ido-style M-x complete
                      idomenu  ;; find methods and variables in current buffer (Ctrl-P)
                      ido-vertical-mode ;; ido menu is vertical now
                      flx-ido ;; ido fuzzy mutching
                      powerline ;; powerline mode
                      powerline-evil ;; powerline with vim bindings
                      idea-darkula-theme ;; color theme like intellij idea
                      linum-relative ;; relative line numbers 
                      yasnippet ;; snippets for emacs
                      ggtags ;; work with tags in large projects
                      magit ;; git for emacs
                      flyspell-popup ;; correct word spelling with popup-menu
                      json-mode ;; major mode for json editing
                      expand-region ;; expand region (like in IDEA) 
                      web-mode ;; major mode for web development
                      js3-mode ;; java script mode
                      yaml-mode ;; yml editing mode
                      ag ;; grep on steroids
                      evil-nerd-commenter ;; commenter plugin
                      ))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
    (load-theme 'idea-darkula t)
    (load-theme 'zenburn))

;;
(require 'logcat)

(require 'expand-region)
(global-set-key (kbd "M-w") 'er/expand-region)

;; company quickhelp
(company-quickhelp-mode 1)

;; enable evil leader button
(global-evil-leader-mode)

;; evil mode
(require 'evil)
(require 'evil-magit)
(evil-mode 1)
(add-to-list 'evil-emacs-state-modes 'nav-mode)
;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
;; mapping 
(define-key evil-normal-state-map "\C-u" 'evil-scroll-up)
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "C-c C-x") 'evil-numbers/dec-at-pt)
;; insert mode motoin
(define-key evil-insert-state-map "\M-k" 'evil-previous-visual-line)
(define-key evil-insert-state-map "\M-j" 'evil-next-visual-line)
(define-key evil-insert-state-map "\M-l" 'forward-char)
(define-key evil-insert-state-map "\M-h" 'backward-char)
(define-key evil-insert-state-map "\M-o" 'scroll-other-window-down)
;; rsi
(define-key evil-insert-state-map "\C-w" 'backward-kill-word)
(define-key evil-insert-state-map "\C-a" 'beginning-of-line)
(define-key evil-insert-state-map "\C-f" 'forward-char)
(define-key evil-insert-state-map "\C-e" 'end-of-line)
(define-key evil-insert-state-map "\C-d" 'delete-forward-char)
(define-key evil-insert-state-map "\C-t" 'transpose-chars)
(define-key evil-insert-state-map "\C-u" '(lambda () (interactive) (kill-line 0)))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "f" 'find-file ;; find file
 "b" 'ido-switch-buffer ;; switch buffers with menu
 "k" 'kill-buffer ;; kill buffer
 "m" 'idomenu ;; goto method
 "c" 'projectile-find-file ;; goto project class (file)
 "p" 'projectile-commander;; goto project class (file)
 "r" 'mode-line-other-buffer ;; recent buffer
 "`" 'magit-status ;; recent buffer
 "a" 'evil-numbers/inc-at-pt ;; increase number
 "x" 'evil-numbers/dec-at-pt ;; decrease number
 "SPC f" 'ace-jump-mode ;; easy motion find char
 "SPC j" 'ace-jump-line-mode ;; easy motion find line
 "SPC k" 'ace-jump-line-mode ;; easy motion find line
 "SPC w" 'ace-jump-word-mode ;; easy motion find word
)

;; enable surround (try to push ysiW" to surround whole WORLD with ")
(require 'evil-surround)
(global-evil-surround-mode 1)

(require 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk")

;; org mode settins
(setq org-startup-indented t) ;; indent instead of stars
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(setq org-todo-keywords ;; TODO workflow
      '((sequence "TODO" "IN_PROGRESS" "|" "DONE")
        (sequence "ASSIGNED" "|" "DONE")
        (sequence "|" "HOLD")
        (sequence "|" "FAILED")
        )) 
(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
        ("IN_PROGRESS" . "yellow")
        ("ASSIGNED" . "yellow")
        ("HOLD" . "red")
        ("FAILED" . "red")
        ("DONE" . "green")))

;; archive all done tasks
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
      "/DONE" 'tree))

;; org-mode and redmine integration
(evil-leader/set-key-for-mode 'org-mode
  "O" 'org-insert-heading-respect-content ;; insert new heading
  "o" 'org-insert-heading-after-current ;; insert new heading
  "i" 'org-insert-heading-after-current ;; insert new heading
  "t" 'org-todo ;; toggle todo
  "d" 'org-deadline ;; insert deadline
  "a" 'org-agenda ;; open agenda
  "/" 'org-sparse-tree ;; sparse tree
)
(evil-define-key 'normal org-mode-map
  "gt" 'org-tracker-goto ;; open issue in redmine if possible
  "gc" 'org-tracker-create ;; create issue in redmine 
  "t" 'org-todo ;; toggle todo
  "M-j" 'org-move-item-down
  "M-J" 'org-move-subtree-down
  "M-k" 'org-move-item-up
  "M-K" 'org-move-subtree-up
  "TAB" 'org-cycle ;; cycle visibility
)
(evil-define-key 'insert org-mode-map
  "M-ENTER"  'org-insert-heading-after-current;; insert new heading
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
(setq neo-smart-open t)
(add-hook 'neotree-mode-hook ;; neotree and evil
        (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;;projectile file fuzzy search
(projectile-global-mode)
(setq projectile-completion-system 'ido)
(global-set-key (kbd "<f9>") 'projectile-compile-project)

;;smex (ido-style M-x menu)
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex) ;; enabling smex M-x
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;; old M-x.

;; ido - completion for everything
(require 'flx-ido)
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
  (define-key ido-completion-map (kbd "M-k") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook #'bind-ido-keys)
;; turn it on
(ido-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; company-mode (autocomplete engine)
(require 'company)
(require 'company-web-html)
(require 'company-jedi)
(setq company-dabbrev-downcase 'nil)
(add-hook 'after-init-hook 'global-company-mode) ;; company always enabled
(setq-default company-minimum-prefix-length 3   ;; minimum prefix character number for auto complete.
              company-idle-delay 0    ;; show tooltip without delay
              company-echo-delay 0    ;; show inline tooltip without delay
              company-show-numbers nil  ;; show numbers in autocomplete popup
              company-selection-wrap-around t ;; loop over candidates
              )
(define-key company-active-map (kbd "TAB") 'company-select-next) ;; choose candidate by tab
(define-key company-active-map (kbd "M-j") 'company-select-next) ;; alt-j
(define-key company-active-map (kbd "M-k") 'company-select-previous) ;; and alt-k
(define-key company-active-map (kbd "M-l") 'company-abort) ;; abort by M-l
(define-key company-active-map (kbd "M-l") 'company-abort) ;; abort by M-l

(add-to-list 'company-backends 'company-shell) ;; shell script completion
(add-to-list 'company-backends 'company-capf) ;; org completion

(define-key evil-insert-state-map (kbd "C-6") 'toggle-input-method)
;; (define-key evil-insert-state-map (kbd "C-<SPC>") 'toggle-input-method)
;; (define-key evil-insert-state-map [tab] 'company-complete)

;; jedi for python mode
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

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
(evil-leader/set-key
 "gr" 'ggtags-find-reference ;; find symbol references
 "gd" 'ggtags-find-definition ;; find symbol definition
 "gt" 'ggtags-find-tag-dwim ;; find tag
)

;; tramp settings
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))


;; WEB development settings

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("thymeleaf" . "\\.html\\'"))
)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)
;; evil key bindings
(evil-define-key 'normal web-mode-map
  "gtn" 'web-mode-tag-next
  "gtp" 'web-mode-tag-previous
  "gtb" 'web-mode-tag-beginning
  "gte" 'web-mode-tag-end
  "gtm" 'web-mode-tag-match
  "gts" 'web-mode-tag-select
)
(evil-leader/set-key-for-mode 'web-mode
  "ew" 'web-mode-element-wrap
  "ee" 'web-mode-element-close
  "ec" 'web-mode-element-clone
)

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

;; spelling
(setq ispell-local-dictionary-alist
    '(("russian"
       "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
       "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
       "[-]"  nil ("-d" "ru_RU") nil utf-8)
      ("english"
       "[A-Za-z]" "[^A-Za-z]"
       "[']"  nil ("-d" "en_US") nil iso-8859-1)))

;; aspell -> hunspell
(setq ispell-really-aspell nil
      ispell-really-hunspell t)

;; kill other buffers command
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(evil-define-key 'normal mime-view-mode-map
  (kbd "q") 'mime-preview-quit
)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "420689cc31d01fe04b8e3adef87b8838ff52faa169e69ca4e863143ae9f3a9f9" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#383838")
 '(jdee-jdk (quote ("1.8")))
 '(jdee-jdk-registry (quote (("1.8" . "/opt/java-8-oracle/"))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (notmuch yasnippet yaml-mode writeroom-mode web-mode wanderlust vimrc-mode smex smart-mode-line-powerline-theme seq seoul256-theme restclient relative-line-numbers projectile powerline-evil org-bullets neotree monokai-theme markdown-mode+ log4j-mode linum-relative let-alist jtags json-mode js3-mode jdee idomenu ido-vertical-mode idea-darkula-theme helm groovy-mode gradle-mode ggtags fringe-helper flyspell-popup flyspell-correct-popup flx-ido expand-region evil-surround evil-space evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-magit evil-escape evil-easymotion elscreen ecb company-web company-quickhelp company-jedi colemak-evil caroline-theme badwolf-theme autopair auto-complete android-mode airline-themes ag ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; fly spell cycle through languages
(let ((langs '("english" "russian")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(global-set-key [f2] 'cycle-ispell-languages)
(define-key evil-normal-state-map "]s" 'flyspell-goto-next-error)
(define-key evil-normal-state-map "z=" 'flyspell-popup-correct)
(define-key evil-normal-state-map (kbd "C-;") 'flyspell-popup-correct)

;; session manipulation
(global-set-key [f5] 'desktop-save)
(global-set-key [f6] 'desktop-read)

(load "server")
(unless (server-running-p) (server-start))
