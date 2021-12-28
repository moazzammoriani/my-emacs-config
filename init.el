;; Initialize package sources


(require 'package)     ;; this loads the contents of packages.el

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")               ;; The package-archives variable is supposed to contain a list of kv-pairs for the name of a
						 ;; package repository and its url
						 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)       ;; a function from package.el that initializes the package system

(unless package-archive-contents  ;; makes sure that package-archive-contents is updated with all the emacs lisp package archives
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms

(unless (package-installed-p 'use-package)   ;; If the use-package package manager is not installed then install it
   (package-install 'use-package))

(require 'use-package)   ;; load 'use-package

(setq use-package-always-ensure t)     ;; makes sure that ':ensure' is set to true in all the use-package forms

(use-package swiper)

(use-package counsel 
:bind (("M-x ". counsel-M-x)
       ("C-x b" . counsel-ibuffer)
       ("C-x t". counsel-load-theme)
       ("C-x C-f" . counsel-find-file)
       :map minibuffer-local-map
       ("C-r" . counsel-mini-buffer-history))
:config
(setq ivy-initial-inputs-alist nil) ) ;; Don't start searches with ^

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

(add-to-list 'load-path "~/.my-emacs/emacs-which-key") ;; had to manually install which-key because there was some trouble finding it in the package
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5)


(use-package ivy-rich    ;; more meaningful and helpful information regarding commands and variables within ivy
  :init
  (ivy-rich-mode 1))

(add-to-list 'load-path "~/.my-emacs/emacs-which-key") ;; had to manually install which-key because there was some trouble finding it in the package
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0)

(use-package helpful  ;; improves help in emacs
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rainbow-delimiters   
  :hook (prog-mode . rainbow-delimiters-mode)
	     (prog-mode . show-paren-mode))

(use-package all-the-icons) ;; needed for doom-modeline

(use-package doom-modeline ;; installs doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))

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

(use-package evil-smartparens
  :after smartparens
  :config (evil-smartparens-mode)) ;; evil integration for smartparens

(use-package vi-tilde-fringe                        ;; get vim-like tilde's to denote unused lines
  :config (global-vi-tilde-fringe-mode))

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

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org
  :hook (org-mode . system-crafters/org-font-setup) 
  :config
  (setq org-ellipsis " ▾")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  (system-crafters/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-roam
	  :ensure t
	  :init (setq org-roam-v2-ack t)
	  :custom
	  (org-roam-directory "~/Documents/RoamHome")
	  :bind (("C-c n l" . org-roam-buffer-toggle)
			 ("C-c n f" . org-roam-node-find)
			 ("C-c n i" . org-roam-node-insert))
	  :config
(org-roam-setup))

(use-package org-roam-ui
  :after org-roam)

(use-package smartparens
  :hook (prog-mode . smartparens-mode))    ;; get autocompletion of parentheses and other delimiters

(use-package general               ;; Installing the general package
  :config
  (general-create-definer mm/leader-keys                           ;; defining a my leader key to be
			  :keymaps '(normal insert visual emacs)    
			  :prefix "SPC"                ;; doesn't work in evil normal mode
                          :global-prefix "C-SPC")      ;; works in evil insert mode as well
  (mm/leader-keys
   "." '(counsel-find-file :which-key "find-files")))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
		(js-mode . lsp-deferred)
		(html-mode . lsp-deferred)
		(css-mode . lsp-deferred)
		(scss-mode . lsp-deferred)
		(c-mode . lsp-deferred)
		(emacs-lisp-mode . lsp-deferred)
		(scheme-mode . lsp-deferred)
	    (haskell-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
(add-hook 'js-mode-hook (lambda () (setq js-indent-level 2))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)


(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
		 ("<tab>" . company-complete-selection))
		(:map lsp-mode-map
		 ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lispy)
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

(use-package auctex
  :hook (latex-mode . auctex))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)           ;; Disable scrollvar visibility
(tool-bar-mode -1)             ;; Disable emacs toolbar
(tooltip-mode -1)              ;; disable tooltips
(menu-bar-mode -1)             ;; disable the menu
(set-fringe-mode 10)
(blink-cursor-mode 0)
  (set-cursor-color "#dedede")

(use-package doom-themes)
(load-theme 'doom-dark+ t)  ;; the t tells emacs that yes I do indeed want to load an external theme
(set-face-background 'show-paren-match "#4d4b4b") ;; highlight matching parenthesis



(set-face-attribute 'default nil :font "Fira Code" :height 127)

(use-package fira-code-mode
    :custom (fira-code-mode-disabled-ligatures '("[]" "x"))  ; ligatures you don't want
	 :hook prog-mode)  

(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
	(mapcar
	 (lambda (s)
	   (setq idx (1+ idx))
	   (let* ((code (+ #Xe100 idx))
		  (width (string-width s))
		  (prefix ())
		  (suffix '(?\s (Br . Br)))
		  (n 1))
	 (while (< n width)
	   (setq prefix (append prefix '(?\s (Br . Bl))))
	   (setq n (1+ n)))
	 (cons s (append prefix suffix (list (decode-char 'ucs code))))))
	 list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
	"{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
	"--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
	"#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
	".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
	"/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
	"|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
	"===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
	">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
	"<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
	"<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
	"<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
	"x" ":" "+" "+" "*"))

(defvar fira-code-mode--old-prettify-alist)

(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
	  (fira-code-mode--enable)
	(fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)

(column-number-mode)
(setq-default display-line-numbers 'visual          ;; this sets displays the line number to relative AND accounts for folding in things like org mode
	   display-line-numbers-current-absolute t
	   display-line-numbers-width 2
	   display-line-numbers-widen t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)      ;; Use <esc> to exit prompts

(add-hook 'text-mode-hook #'show-paren-mode) ;; attaching show-parens-mode to the prog-mode hook

(setq-default tab-width 4)        ;; set the tab width to 4

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer) ;; globally map the combination of <CTRL>-<ALT>-j to the counsel-switch-buffer command

(org-babel-do-load-languages                      ;; load languages for org-babel
  'org-babel-load-language
  '((emacs-lisp . t)
    (python . t)
	(C . t)
	(shell .t)
	(js . t)
	(scheme .t)
	(lisp .t)
	(haskell . t)
	(latex . t) ) )

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

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(dolist (mode '(shell-mode-hook     ;; what this does is that is iterates through the list of hooks and adds the lambda expression inside those hooks
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package python-mode
 :ensure t
 :hook (python-mode . lsp-deferred)
 :custom
 ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3"))

(add-hook 'hack-local-variables-hook
		 (lambda () (when (derived-mode-p 'python-mode) (lsp-deferred))))

(use-package haskell-mode)
