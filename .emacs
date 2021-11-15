;;Commentary
;;https://blog.sumtypeofway.com/posts/emacs-config.html
;;He brought us through recursion theory and now he is our
;;guide through emacs. Starting from his configuration and then
;;drifting into new territory.

(setq lexical-binding t)
(setq gc-cons-threshold 100000000)
(setq use-package-always-ensure t)

;;[association lists](https://www.gnu.org/software/emacs/manual/html_node/elisp/Association-Lists.html)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-themes nix-mode attrap direnv envrc dante))
 '(safe-local-variable-values
   '((haskell-process-type . ghci)
     (buffer-file-coding-system . utf-8-unix))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))

;;(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq
   ;; No need to see GNU agitprop.
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is.
   initial-scratch-message nil
   ;; Double-spaces after periods is morally wrong.
   sentence-end-double-space nil
   ;; Never ding at me, ever.
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; search should be case-sensitive by default
   case-fold-search nil
   )

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

;;utf-8 is the default for terminal, keyboard and selections.
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

;;Line highlight mode
;;(require 'hl-line)
;;(add-hook 'prog-mode-hook #'hl-line-mode)
;;(add-hook 'text-mode-hook #'hl-line-mode)
;;(set-face-attribute 'hl-line nil :background "gray21")

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)

(require 'recentf)
(add-to-list 'recentf-exclude "\\elpa")

;;"actively hostile key-bindings".;
;;HL not sure about this first one? I kind of like opening files this way and they aren't readonly.
;;(unbind-key "C-x C-f") ;; find-file-read-only
;;(unbind-key "C-x C-d") ;; list-directory
;;(unbind-key "C-z") ;; suspend-frame
;;(unbind-key "M-o") ;; facemenu-mode
;;(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
;;(unbind-key "<C-wheel-down>") ;; text scale adjust
;;(unbind-key "<C-wheel-up>") ;; text scale adjust

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

(defun do-nothing (interactive))
(defalias 'view-emacs-news 'do-nothing)
(defalias 'describe-gnu-project 'do-nothing)

;;undo undo undo!
(use-package undo-tree
;;  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

;like so many this is another southern ontario programmer.
;https://github.com/hlissner/emacs-doom-themes/issues/314
;;disabling warnings on safe variables defined with defvar.
(setq enable-local-variables :all)

;;"opinion::Emacs looks a lot better when it has a modern monospaced font and VSCode-esque icons."
;;HL this one looks cool maybe not great for coding (ignore-errors (set-frame-font "Courier-14"))
(ignore-errors (set-frame-font "Menlo-14"))
(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . 90))
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(use-package doom-themes
  :ensure t
  :config
  :custom (doom-challenger-deep-brighter-comments t)
          (doom-challenger-deep-brighter-modeline t)
  (doom-themes-org-config)
  (load-theme 'doom-moonlight t)
  )

(add-hook 'after-init-hook (lambda () (load-theme 'doom-moonlight t)))


;;(use-package diminish
;;  :config (diminish 'eldoc-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  )

(use-package dimmer
  :custom (dimmer-fraction 0.1)
  :config (dimmer-mode))


;;getting lispy
;;(use-package geiser)
;;(use-package slime)
(use-package paredit)

(show-paren-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package purescript-mode)

;;Helper Functions
(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs"))

(bind-key "C-c e" #'open-init-file)

(defun open-cheet-sheet ()
  "Open a personalized Emacs cheat sheet."
  (interactive)
  (find-file "~/.emacs-cheets.md"))

(bind-key "C-c h" #'open-cheet-sheet)

(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun insert-current-date ()
  "Insert the current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date)")))

;;Some HL bindings
(bind-key "C-c d" #'insert-current-date)



;;(bind-key "C-c r" #'replace-string)
;;dired related hooks for darwin
(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))

(defun my-dired-mode-hook ()
  "Dired hooks."
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer)
  )

(add-hook 'dired-mode-hook #'my-dired-mode-hook)

(setq dired-use-ls-dired nil)

(global-so-long-mode)

;;It’s genuinely shocking that there’s no “duplicate whatever’s marked” command built-in.
;;HL so so true
(use-package duplicate-thing
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-c u" . my-duplicate-thing)
         ("C-c C-u" . my-duplicate-thing)))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

;;ido was going nicely but going to try
;;ivy
;;https://oremacs.com/swiper/
;;https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
(use-package ivy
  :defer 0.1
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)

  :bind (("C-c C-r" . #'ivy-resume)
         ("C-c s"   . #'swiper-thing-at-point)
         ("C-s"     . #'swiper))
 )

(use-package ivy-rich
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer nil)
  (ivy-rich-path-style 'full)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode)
  )

;;https://github.com/abo-abo/avy
;;avy is a GNU Emacs package for jumping to visible text using a char-based decision tree.
;;See also ace-jump-mode and vim-easymotion - avy uses the same idea.
;;;;jump-to-line commands mapped to the home rome

(use-package counsel
  :init
  (counsel-mode 1)

  :bind (("C-c ;" . #'counsel-M-x)
         ("C-c U" . #'counsel-unicode-char)
         ("C-c i" . #'counsel-imenu)
         ("C-x f" . #'counsel-find-file)
         ("C-c y" . #'counsel-yank-pop)
         ("C-c r" . #'counsel-recentf)
         ("C-c v" . #'counsel-switch-buffer-other-window)
         ("C-c H" . #'counsel-projectile-rg)
         ("C-h h" . #'counsel-command-history)
         ("C-x C-f" . #'counsel-find-file)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
 :diminish)

(use-package counsel-projectile
 :bind (("C-c f" . #'counsel-projectile)
        ("C-c F" . #'counsel-projectile-switch-project)))

;;had to manually package-install deadgrep
;;not sure why
(use-package deadgrep
  :bind (("C-c h" . #'deadgrep))
  )

(use-package visual-regexp
  :bind (("C-c r " . #'vr/replace))
  )

;;overwrite ivy default
(defun kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)
  )

(bind-key "C-x k" #'kill-this-buffer)
(bind-key "C-x K" #'kill-buffer)


(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/src" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package avy
  :bind
  (("C-c l" . avy-goto-line) ;;This one is... nicee.
   ("C-c j" . avy-goto-char)) ;;This one is... ornate.
  )

;;and the ivy integration for avy
(use-package ivy-avy)


(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.


(use-package haskell-mode
  :config
  (setq haskell-indentation-left-offset 4)
  (setq haskell-indentation-starter-offset 4)
;;  (setq haskell-where-pre-offset 4)
;;  (setq haskell-where-post-offset 4)
;;  (setq haskell-indentation-ifte-offset 4)
  :bind (:map haskell-mode-map
              ("C-c M"   . haskell-compile)
;;              ("C-c a c" . haskell-cabal-visit-file)
;;              ("C-c a i" . haskell-navigate-imports)
;;              ("C-c a I" . haskell-navigate-imports-return)
              )
  )


(use-package lsp-mode
  :commands (lsp lsp-execute-code-action)
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-modeline-diagnostics-mode)
         (haskell-mode . lsp))
  :bind ("C-c C-c" . #'lsp-modeline-diagnostics-mode)
  :custom
  ;;(lsp-keymap-prefix "C-c l") conflicts with avy
  (lsp-diagnostics-modeline-scope :project)
  (lsp-file-watch-threshold 5000)
  (lsp-response-timeout 2)
  (lsp-enable-file-watchers nil))

;;https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :disabled
  :custom
  (lsp-ui-doc-mode nil)
  (lsp-ui-doc-delay 2.0)
  (lsp-ui-doc-max-height 600)
  (lsp-ui-peek-always-show nil)
  (lsp-ui-doc-position nil)
  (lsp-ui-sideline-show-hover nil)
  :after lsp-mode)

(use-package lsp-ivy
  :after (ivy lsp-mode)
  :bind ("C-c a" . lsp-ivy-workspace-symbol)
 )

;; (use-package company
;;   :diminish
;;   :bind (("C-." . #'company-complete))
;;   :hook (prog-mode . company-mode)
;;   :custom
;;   (company-dabbrev-downcase nil "Don't downcase returned candidates.")
;;   (company-show-numbers t "Numbers are helpful.")
;;   (company-tooltip-limit 20 "The more the merrier.")
;;   (company-tooltip-idle-delay 0.4 "Faster!")
;;   (company-async-timeout 20 "Some requests can take a long time. That's fine.")
;;   :config

;;   ;; Use the numbers 0-9 to select company completion candidates
;;   (let ((map company-active-map))
;;     (mapc (lambda (x) (define-key map (format "%d" x)
;;    `(lambda () (interactive) (company-complete-number ,x))))
;;    (number-sequence 0 9))))
;;(use-package company-lsp
;;  :custom (company-lsp-enable-snippet t)
;;  :after (company lsp-mode))

;;Think ivy has more integration with lsp than ido
;;(require 'ido)
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(setq ido-file-extensions-order '(".hs" ".js" ".css" ".md" ".txt"))
;;(setq ido-ignore-extensions t)
;;(ido-mode t)

;;(setq haskell-indent-spaces 4)















(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . lsp-lens-mode)
         )
  :custom
  (rust-format-on-save t)
  (lsp-rust-server 'rust-analyzer))

(use-package typescript-mode)
(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package toml-mode)

(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'")

(use-package markdown-mode
  :bind (("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

;; HL fun with setting up windows on start.
;; When I was using emacs terminal but this caused
;; other problems.
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)


(defun my-default-window-setup ()
  "Called by `emacs-startup-hook` to set up my initial window configuration."
    (set-frame-position (selected-frame) 1000 0)
    (set-frame-height
    (selected-frame)
    (/ (display-pixel-height) (frame-char-height)))

    (set-frame-width
    (selected-frame)
    (/ (/ (display-pixel-width) 2) (frame-char-width)))

;; From When I wanted a little horizontal window for a terminal emulator:
;;    (split-window-vertically)
;;    (halve-other-window-height)
  )
;;  (find-file "~/txt/todo.org")

(add-hook 'emacs-startup-hook #'my-default-window-setup)

;; (provide 'init)
;; (provide '.emacs)
;;; .emacs ends here
