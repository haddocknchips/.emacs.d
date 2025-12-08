;;; based on Emacs from Scratch
;;; Emacs from scratch #2
;;; Last modified 2025-12-06

(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room (what does this mean?)

(menu-bar-mode -1)      ; Disable the menu bar

;; Set up the visible bell; might be annoying on MacOS
;(setq visible-bell t)

;;;; Font sizes: set the font face based on platform
; But trying this:
(defvar efs/default-font-size 180)
(defvar efs/default-variable-font-size 180)

;;;; Need to play around with font sizes

;;;;(on-platform-do
;  ((windows cygwin) (set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130))
;  (osx (set-face-attribute 'default nil :font "Fira Mono" :height 170))
;  (linux (set-face-attribute 'default nil :font "Fira Code Retina" :height 280)))

;(set-face-attribute 'default nil :font "Fira Mono" :height 180)
(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

; trying this:
;(set-face-attribute 'default nil :font "Fira Mono-14")

;; Load theme
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialise package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Adding based on the Melpa website
;; Not sure this is needed
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialise use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Show line numbers (why here? move later)
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line number for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

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

;; Switch buffers; demo only => see general package binding below
;(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package ivy-rich
             :init
             (ivy-rich-mode 1))

(use-package counsel
             :bind (("M-x" . counsel-M-x)
                    ("C-x b" . counsel-ibuffer)
                    ("C-x C-f" . counsel-find-file)
                    :map minibuffer-local-map
                    ("C-r" . 'counsel-minibuffer-history))
             :config
             (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Helpful => very helpful(!) when trying to understand details of commands
(use-package helpful
             :custom
             (counsel-describe-function-function #'helpful-callable)
             (counsel-describe-variable-function #'helpful-variable)
             :bind
             ([remap describe-function] . counsel-describe-function) ; will use counsel instead of helpful version (which is not as good)
             ([remap describe-command] . helpful-command)
             ([remap describe-variable] . counsel-describe-variable)
             ([remap describe-key] . helpful-key))

;; Finally, the icons
;; Need to run M-x all-the-icons-install-fonts the first time the below is loaded on system
(use-package all-the-icons)

(use-package doom-modeline
   :init (doom-modeline-mode 1)
   :custom ((doom-modeline-height 15)))

(use-package doom-themes
;   :init (load-theme 'doom-gruvbox))
;   :init (load-theme 'doom-palenight t)) ; t overrides confirmation quesiton to load theme
   :init (load-theme 'doom-dracula t)) ; t overrides confirmation quesiton to load theme

;; Aid in programming esp Lisp
(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

;; which-key => provides panel pop-up to show what keys/keybindings are available under that prefix; see readme
(use-package which-key
             :init (which-key-mode)
             :diminish which-key-mode
             :config
             (setq which-key-idle-delay 0.3))

;; General for bindings
;; Can also be used for maintaining general key prefix
;(use-package general)
;(general-define-key
;  "C-M-j" 'counsel-switch-buffer)
(use-package general
             :config
             (general-evil-setup t)

             (general-create-definer efs/leader-keys
               :keymaps '(normal insert visual emacs)
               :prefix "SPC"
               :global-prefix "C-SPC")

             (efs/leader-keys
               "t"  '(:ignore t :which-key "toggles")
               "tt" '(counsel-load-theme :which-key "choose theme")))

;; Evil mode and Evil collection
;; Evil collection: look into removing items from list eg, if they don't work well
(use-package evil
             :init
             (setq evil-want-integration t)
             (setq evil-want-keybinding nil)
             (setq evil-want-C-u-scroll t)
             (setq evil-want-C-i-jump nil)
             :config
             (evil-mode 1)
             (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
             (define-key evil-insert-state-map (kbd "jj") 'evil-normal-state) ;; my preferred escape to Normal mode 
             (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

             ;; Use visual line motions even outside of visual-line-mode buffers
             (evil-global-set-key 'motion "j" 'evil-next-visual-line)
             (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

             (evil-set-initial-state 'messages-buffer-mode 'normal)
             (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
             :after evil
             :config
             (evil-collection-init))

;; Hydra => transient keybindings? 
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
          "scale text"
          ("j" text-scale-increase "in")
          ("k" text-scale-decrease "out")
          ("f" nil "finished" :exit t))

(efs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Ep 4, 2012-12-07: Projectile project interaction library eg, for folders with .gitk

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7de64ff2bb2f94d7679a7e9019e23c3bf1a6a04ba54341c36e7cf2d2e56e2bcc"
     "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
     default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
