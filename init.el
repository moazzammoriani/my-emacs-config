(setq inhibit-startup-message t)

(scroll-bar-mode -1)           ;; Disable scrollvar visibility
(tool-bar-mode -1)             ;; Disable emacs toolbar
(tooltip-mode -1)              ;; disable tooltips
(menu-bar-mode -1)             ;; disable the menu


(set-face-attribute 'default nil :font "Fira Code" :height 130)


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)      ;; Use <esc> to exit prompts
(setq-default tab-width 4)        ;; set the tab width to 4

;; Initialize package sources


(require 'package)     ;; this loads the contents of packages.el

(setq package-archives '(("melpa" . "https://melpa.org/packages/")               ;; The package-archives variable is supposed to contain a list of kv-pairs for the name of a
                         ("org" . "https://orgmode.org/elpa/")                   ;; package repository and its url
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)       ;; a function from package.el that initializes the package system

(unless package-archive-contents  ;; makes sure that package-archive-contents is updated with all the emacs lisp package archives
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)   ;; If the use-package package manager is not installed then install it
   (package-install 'use-package))

(require 'use-package)   ;; load 'use-package

(setq use-package-always-ensure t)     ;; makes sure that ':ensure' is set to true in all the use-package forms
                                       ;; note: if ':ensure' is set to true in all the use-package forms then the package will be downloaded and installed before they are run

(use-package swiper
  :ensure t)

(use-package counsel
  :bind (("M-x ". counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x t". counsel-load-theme)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-mini-buffer-history))
  

  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)                                                                              	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" default))
 '(ivy-mode t)
 '(package-selected-packages
   '(vi-tilde-fringe visual-fill-column org-bullets org-superstar general evil-smartparens smartparens parinfer-rust-mode highlight-parentheses evil-surround doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline counsel swiper ivy use-package no-littering evil-collection auto-package-update))
 '(which-key-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 


(use-package all-the-icons) ;; needed for doom-modeline

(use-package doom-modeline ;; installs doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 24)))

(column-number-mode)
(setq-default display-line-numbers 'visual          ;; this sets displays the line number to relative AND accounts for folding in things like org mode
	      display-line-numbers-current-absolute t
	      display-line-numbers-width 2
	      display-line-numbers-widen t)

(dolist (mode '(shell-mode-hook     ;; what this does is that is iterates through the list of hooks and adds the lambda expression inside those hooks
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package rainbow-delimiters        ;; color codes a pair of parantheses to know which one belongs to which
  :hook (prog-mode . rainbow-delimiters-mode))

(add-to-list 'load-path "~/.my-emacs/emacs-which-key") ;; had to manually install which-key because there was some trouble finding it in the package
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0)


(use-package ivy-rich    ;; more meaningful and helpful information regarding commands and variables within ivy
  :init
  (ivy-rich-mode 1))

(use-package helpful  ;; improves help in emacs
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))




(use-package doom-themes)
(load-theme 'doom-dark+ t)  ;; the t tells emacs that yes I do indeed want to load an external theme
(set-face-background 'show-paren-match "#4d4b4b") ;; highlight matching parenthesis

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer) ;; globally map the combination of <CTRL>-<ALT>-j to the counsel-switch-buffer command


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-insert-state-cursor 'box)
  :config 
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "u") 'evil-undo)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)) 

(use-package evil-surround       ;; emulate vim surround in evil mode
  :ensure t
  :config
  (global-evil-surround-mode 1))


(use-package smartparens
  :hook (prog-mode . smartparens-mode))    ;; get autocompletion of parentheses and other delimiters


(use-package evil-smartparens
  :after smartparens
  :config (evil-smartparens-mode)) ;; evil integration for smartparens

(add-hook 'prog-mode-hook #'show-paren-mode) ;; attaching show-parens-mode to the prog-mode hook


(use-package general               ;; Installing the general package
  :config
  (general-create-definer mm/leader-keys                           ;; defining a my leader key to be
			  :keymaps '(normal insert visual emacs)    
			  :prefix "SPC"                ;; doesn't work in evil normal mode
                          :global-prefix "C-SPC")      ;; works in evil insert mode as well
  (mm/leader-keys
   "." '(counsel-find-file :which-key "find-files")))

(defun system-crafters/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  ;; Set faces for heading levels

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1))))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . system-crafters/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (system-crafters/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))



(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t
  (visual-fill-column-mode 1)))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(add-hook 'org-mode-hook 'visual-line-mode)

(use-package vi-tilde-fringe                        ;; get vim-like tilde's to denote unused lines
  :config (global-vi-tilde-fringe-mode))

