;;; init.el --- user-init-file                    -*- lexical-binding: t -*-
;;; Early birds
(progn ;     startup
  (defvar before-user-init-time (current-time)
    "Value of `current-time' when Emacs begins loading `user-init-file'.")
  (message "Loading Emacs...done (%.3fs)"
           (float-time (time-subtract before-user-init-time
                                      before-init-time)))
  (setq user-init-file (or load-file-name buffer-file-name))
  (setq user-emacs-directory (file-name-directory user-init-file))
  (message "Loading %s..." user-init-file)
  (setq package-enable-at-startup nil)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (menu-bar-mode 0))


;; setup straight
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package solarized-theme
   :straight t
   :config
     (load-theme 'solarized-light t))

(use-package org-journal
  :straight t)

(use-package epkg
  :straight t
  :init (setq epkg-repository
              (expand-file-name "var/epkgs/" user-emacs-directory)))
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

(use-package server
  :config (or (server-running-p) (server-mode)))

(progn ;     startup
  (message "Loading early birds...done (%.3fs)"
           (float-time (time-subtract (current-time)
                                      before-user-init-time))))

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(use-package dash
  :config (dash-enable-font-lock))

(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package dired
  :defer t
  :config (setq dired-listing-switches "-alh"))

(use-package eldoc
  :config (global-eldoc-mode))

(use-package help
  :defer t
  :config (temp-buffer-resize-mode))

(use-package magit
  :straight t
  :defer t
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append)
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package paren
  :config (show-paren-mode))

(use-package prog-mode
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

(use-package recentf
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package savehist
  :config (savehist-mode))

(use-package saveplace
  :config (save-place-mode))

(use-package simple
  :config (column-number-mode))

(progn ;    `text-mode'
  (add-hook 'text-mode-hook #'indicate-buffer-boundaries-left))

(use-package tramp
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

(use-package ivy
  :straight t
  :init (ivy-mode)
  :bind (("C-s" . swiper)
         ("M-x" . counsel-M-x)))

(use-package markdown-mode
  :straight t)


(use-package company
  :straight t
  :config (setq company-minimum-prefix-length 1))

(use-package company-quickhelp
  :straight t
  :config (company-quickhelp-mode))

(use-package projectile
  :straight t
  :init (projectile-mode)
  :config (progn
            (setq projectile-git-submodule-command "")
            (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
  :bind ("C-x C-b" . projectile-switch-to-buffer))

(progn ;     startup
  (message "Loading %s...done (%.3fs)" user-init-file
           (float-time (time-subtract (current-time)
                                      before-user-init-time)))
  (add-hook 'after-init-hook
            (lambda ()
              (message
               "Loading %s...done (%.3fs) [after-init]" user-init-file
               (float-time (time-subtract (current-time)
                                          before-user-init-time))))
            t))

(use-package counsel-projectile
  :straight t
  :config (progn
            (counsel-mode)
            (counsel-projectile-mode)))

;; golang
(use-package company-go
  :straight t
  :config (setq company-go-show-annotation t))


(use-package go-mode
  :straight t
  :config (progn
            (defun go-mode-hook ()
            (local-set-key (kbd "M-.") 'godef-jump)
              ;; company-go
              (set (make-local-variable 'company-backends)
                   '(company-go))
              (company-mode)
              ;; use goimports before saving
              (setq gofmt-command "goimports")
              (add-hook 'before-save-hook 'gofmt-before-save))
            (add-hook 'go-mode-hook 'go-mode-hook)))

(use-package go-projectile
  :straight t)

(use-package flycheck
  :straight t
  :commands (flycheck-mod global-flycheck-mode))

(use-package undo-tree
  :straight t)


(use-package key-chord
  :straight t)

(use-package hydra
  :straight t
  :config
  (progn
    (defhydra hydra-undo-tree (:hint nil)
      "
  _p_: undo  _n_: redo _s_: save _l_: load   "
      ("p"   undo-tree-undo)
      ("n"   undo-tree-redo)
      ("s"   undo-tree-save-history)
      ("l"   undo-tree-load-history)
      ("u"   undo-tree-visualize "visualize" :color blue)
      ("q"   nil "quit" :color blue))
    
    (global-set-key (kbd "C-x u") 'hydra-undo-tree/undo-tree-undo)
   
    (key-chord-define-global
     "hh"
     (defhydra hydra-error ()
       "goto-error"
       ("h" first-error "first")
       ("j" next-error "next")
       ("k" previous-error "prev")))))


(use-package which-key
  :straight t)

(use-package god-mode
  :straight t)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; load python org config
(require 'org)

(org-babel-load-file
 (expand-file-name "lsp.org"
                   user-emacs-directory))

(org-babel-load-file
 (expand-file-name "python.org"
                   user-emacs-directory))

