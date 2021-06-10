;;https://blog.sumtypeofway.com/posts/emacs-config.html
;;He brought us through recursion theory and now he is our
;;guide through emacs. Starting from his configuration and then
;;drifting into new territory.
(setq lexical-binding t)
(setq gc-cons-threshold 100000000)
(setq use-package-always-ensure t)

;;(use-package exec-path-from shell
;; :config (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-themes nix-mode attrap direnv envrc dante))
 '(safe-local-variable-values
   '((haskell-process-type . ghci)
     (dante-repl-command-line "ghci")
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




;; Patrick Thompson is Our Guide Here:: https://blog.sumtypeofway.com/posts/emacs-config.html
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


;;"actively hostile key-bindings".;
;;HL not sure about this first one? I kind of like opening files this way and they aren't readonly.
;;(unbind-key "C-x C-f") ;; find-file-read-only
;;(unbind-key "C-x C-d") ;; list-directory
;;(unbind-key "C-z") ;; suspend-frame
;;(unbind-key "M-o") ;; facemenu-mode
;;(unbind-key "<mouse-2>") ;; pasting with mouse-wheel click
(unbind-key "<C-wheel-down>") ;; text scale adjust

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

(defun do-nothing (interactive))
(defalias 'view-emacs-news 'do-nothing)
(defalias 'describe-gnu-project 'do-nothing)

;;undo undo undo!
(use-package undo-tree
  :diminish
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map))

;like so many this is a nother southern ontario programmer.
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

;;Patrick was... aggressive in his allocation of screenspace to emacs.
(add-to-list 'default-frame-alist '(fullscreen . 90))
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-moonlight))
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    (load-theme chosen-theme)))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package dimmer
  :custom (dimmer-fraction 0.2)
  :config (dimmer-mode))

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


(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%a %b %d %Y)")))

;;Some HL bindings
(bind-key "C-c d" #'insert-current-date)

(bind-key "C-c r" #'replace-string)

;;dired related hooks for darwin
(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))

(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))

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
  :init (global-flycheck-mode))

(defcustom flycheck-display-errors-delay 5 "Delay for flycheck to pop up irritating buffer.")

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (haskell-mode . lsp))
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".hs" ".js" ".css" ".md" ".txt"))
(setq ido-ignore-extensions t)
(ido-mode t)

;;Language Specific Configs
;;Haskell
(use-package dante
  :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  ;;(add-hook 'haskell-mode-hook 'flycheck-mode)
  ;; OR for flymake support:
  ;;(add-hook 'haskell-mode-hook 'flymake-mode)
  ;;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  ;;(add-hook 'haskell-mode-hook 'dante-mode)
  )


(use-package haskell-mode
  :config
  (defcustom haskell-formatter 'stylish
    "The Haskell formatter to use. One of: 'ormolu, 'stylish, nil. Set it per-project in .dir-locals."
    :safe 'symbolp
    )
  )

(use-package rust-mode)

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

(global-set-key (kbd "C-c v") 'halve-other-window-height)


(defun my-default-window-setup ()
  "Called by emacs-startup-hook to set up my initial window configuration."
    (set-frame-position (selected-frame) 1000 0)
    (set-frame-height
    (selected-frame)
    (/ (display-pixel-height) (frame-char-height)))

    (set-frame-width
    (selected-frame)
    (/ (/ (display-pixel-width) 2) (frame-char-width)))

    (split-window-vertically)
    (halve-other-window-height)
  )
;;  (find-file "~/txt/todo.org")

(add-hook 'emacs-startup-hook #'my-default-window-setup)

(provide 'init)
