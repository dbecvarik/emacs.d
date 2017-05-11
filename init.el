
(setq user-full-name "David Becvarik"
      user-mail-address "ldj@l-d-j.org")

(setenv "PATH" (concat (getenv "PATH") ":/home/user/.local/bin"))

(package-initialize)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-refresh-contents)
)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(use-package solarized-theme
   :config
     (load-theme 'solarized-light t))

(use-package helm
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally
(use-package helm-ag
   :config
   (setq
     helm-ag-insert-at-point 'word))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

(use-package projectile
  :init (projectile-global-mode))

(use-package helm-projectile
  :ensure    helm-projectile
  :init
  (helm-projectile-on)
  :config
    (progn
      (setq projectile-completion-system 'helm)))

(use-package org
  :config
   (progn
     (setq org-src-fontify-natively t)))

(setq org-todo-keywords
  '((sequence "NEXT(n)" "TODO(t)" "WAIT(w)" "SOMEDAY(s)" "PROJECT(p)" "|" "DONE(d)" "CANCELED(c)")))

(add-hook 'org-mode-hook 'turn-on-flyspell)

(org-babel-do-load-languages 'org-babel-load-languages
'( (sh . t)
   (emacs-lisp . t)
))

(require 'org-id)

(require 'ob-sh)


(defun org-babel-async-execute:sh ()
  "Execute the shell src-block at point asynchronously.
:var headers are supported.
:results output is all that is supported for output.

A new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (uuid (org-id-uuid))
         (code (org-element-property :value (org-element-context)))
         (temporary-file-directory ".")
         (tempfile (make-temp-file "sh-"))
         (pbuffer (format "*%s*" uuid))
         (varcmds (org-babel-variable-assignments:sh
                   (nth 2 (org-babel-get-src-block-info))))
         process)

    ;; get rid of old results, and put a place-holder for the new results to
    ;; come.
    (org-babel-remove-result)

    (save-excursion
      (re-search-forward "#\\+END_SRC")
      (insert (format
               "\n\n#+RESULTS: %s\n: %s"
               (or (org-element-property :name (org-element-context))
                   "")
               uuid)))

    ;; open the results buffer to see the results in.
    (switch-to-buffer-other-window pbuffer)

    ;; Create temp file containing the code.
    (with-temp-file tempfile
      ;; if there are :var headers insert them.
      (dolist (cmd varcmds)
        (insert cmd)
        (insert "\n"))
      (insert code))

    ;; run the code
    (setq process (start-process
                   uuid
                   pbuffer
                   "bash"
                   tempfile))

    ;; when the process is done, run this code to put the results in the
    ;; org-mode buffer.
    (set-process-sentinel
     process
     `(lambda (process event)
        (save-window-excursion
          (save-excursion
            (save-restriction
              (with-current-buffer (find-file-noselect ,current-file)
                (goto-char (point-min))
                (re-search-forward ,uuid)
                (beginning-of-line)
                (kill-line)
                (insert
                 (mapconcat
                  (lambda (x)
                    (format ": %s" x))
                  (butlast (split-string
                            (with-current-buffer
                                ,pbuffer
                              (buffer-string))
                            "\n"))
                  "\n"))))))
        ;; delete the results buffer then delete the tempfile.
        ;; finally, delete the process.
        (when (get-buffer ,pbuffer)
          (kill-buffer ,pbuffer)
          (delete-window))
        (delete-file ,tempfile)
        (delete-process process)))))

(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c c") 'org-babel-async-execute:sh)))

(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-popup-type 'side-window))

(use-package perspeen
  :ensure t
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode))

(use-package elpy
  :init (elpy-enable)
  :config
    (setq elpy-rpc-backend "rope"
        elpy-modules '(elpy-module-sane-defaults
                       elpy-module-company
                       elpy-module-eldoc
                       elpy-module-flymake
                       elpy-module-highlight-indentation
                       elpy-module-yasnippet)
        elpy-company-post-completion-function 'elpy-company-post-complete-parens
        ))

(use-package virtualenvwrapper
  :ensure t
  :init
  (progn
    (setq eshell-modify-global-environment t)
    (setq venv-location "~/.virtualenvs")
    (venv-initialize-eshell)))

(defun hs-enable-and-toggle ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding))
(defun hs-enable-and-hideshow-all (&optional arg)
  "Hide all blocks. If prefix argument is given, show all blocks."
  (interactive "P")
  (hs-minor-mode 1)
  (if arg
      (hs-show-all)
      (hs-hide-all)))
(global-set-key (kbd "C-c C-h") 'hs-enable-and-toggle)
(global-set-key (kbd "C-c C-j") 'hs-enable-and-hideshow-all)

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
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
