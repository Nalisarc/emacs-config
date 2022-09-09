;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))



(use-package org
  :demand t
  :ensure t
  
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 )
  
  :config
  (require 'org-protocol)
  (setq indent-tabs-mode nil)
  (setq org-src-preserve-indentation t)
  (require 'ob-org)
  (require 'ob-shell)
  (require 'ob-latex)
  (setq org-agenda-files '("~/agcloud/org/gtd.org"
  			 "~/agcloud/org/tickler.org"
  			 "~/agcloud/org/inbox.org"
  			 ))
  
  (setq org-agenda-custom-commands 
        '(("o" "At the office" tags-todo "@office"
  	 ((org-agenda-overriding-header "Office")
  	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)
  	  ))))
  
  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
  	(when (org-current-is-todo)
  	  (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
  	  (goto-char (point-max))))))
  
  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state)))
  (setq org-capture-templates
        '(
  	("t" "Todo" entry (file+headline "~/agcloud/org/inbox.org" "Inbox")
  	 "* TODO %?\n ")
  	("e" "Entry" entry(file+headline "~/agcloud/org/inbox.org" "Inbox")
  	 "* %?\n ")
  	))
  (setq org-refile-targets '(("~/agcloud/org/gtd.org" :maxlevel . 3)
  			   ("~/agcloud/org/someday.org" :level . 1)
  			   ("~/agcloud/org/tickler.org" :maxlevel . 2)))
  )

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/agcloud/org-roam")
  
  :config
  (setq org-roam-db-location "~/.emacs.d/org-roam.db")
  (require 'org-roam-protocol)
  (org-roam-setup)
  :bind
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n d" . org-roam-dailies-capture-today)
  ("C-c n r" . org-roam-node-random)
  )

(use-package org-noter
  :after (org pdf-tools nov)
  :config
  (setq org-noter-doc-property-in-notes t)
  )

(use-package helm
  :config
  (helm-mode 1)
  (global-unset-key (kbd "C-x c"))
  :bind
  (
   	   ("M-x" . helm-M-x)
	   ("C-x r b" . helm-filtered-bookmarks)
	   ("C-x C-f" . helm-find-files)
	   ("C-x b". helm-mini)
	   ("M-y" . helm-show-kill-ring)
	   ("C-c h o" . helm-occur)
	   :map helm-command-map
	   ("C-c h" . helm-command-prefix)
	   ("<tab>" . helm-execute-persistent-action)
	   ("C-i" . helm-execute-persistent-action)
	   ("C-z" . helm-select-action))

   )

(use-package helm-bibtex
  :ensure t
  :demand t)
  

(use-package zeno-theme)

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))



(defalias 'yes-or-no-p 'y-or-n-p) ;;Ease of use

(menu-bar-mode -1)
(tool-bar-mode -1) 
(toggle-scroll-bar -1) 

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config (setq deft-directory "~/agcloud/org-roam"
                deft-extensions '("org")
		deft-recursive t
		deft-use-filename-as-title t
		
		))

(use-package org-fc
  :straight (org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org")
   :after (org)
   :custom (org-fc-directories "~/agcloud/org-roam")))

(use-package org-download
  :config
  (setq org-download-image-dir "~/agcloud/org-roam/media/images")
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(use-package magit
  :ensure t)

(use-package org-ref
  :straight (org-ref :type git :host github :repo "jkitchin/org-ref")
  :after (org helm-bibtex)
  :init
  (require 'bibtex)
  (require 'hydra)

  (setq reftex-default-bibliography "~/agcloud/org-roam/references.bib")
  (setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

:config
(setq org-ref-bibliography-notes  "~/agcloud/org-roam/index.org"
      org-ref-default-bibliography "~/agcloud/org-roam/references.bib"
      org-ref-pdf-directory "~/agcloud/Books")

(require 'org-ref-helm)

:bind
("C-c r" . org-ref-bibtex-hydra/body)
)

(use-package elpy
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))

  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)

  (defun ha/elpy-goto-definition ()
    (interactive)
    (condition-case err
        (elpy-goto-definition)
      ('error (xref-find-definitions (symbol-name (symbol-at-point))))))

  :bind (:map elpy-mode-map ([remap elpy-goto-definition] .
                             ha/elpy-goto-definition)))
(use-package pyenv-mode
  :ensure t
  :after (exec-path-from-shell)
  :config
    (defun projectile-pyenv-mode-set ()
      "Set pyenv version matching project name."
      (let ((project (projectile-project-name)))
        (if (member project (pyenv-mode-versions))
            (pyenv-mode-set project)
          (pyenv-mode-unset))))

    (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
    (add-hook 'python-mode-hook 'pyenv-mode))

(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :config
  (setq python-indent-offset 4))

(use-package jedi
  :ensure t
  :after (exec-path-from-shell)
  :init
  (add-to-list 'company-backends 'company-jedi)
  :config
  (use-package company-jedi
    :ensure t
    :init
    (add-hook 'python-mode-hook (lambda () (add-to-list 'company-backends 'company-jedi)))
    (setq company-jedi-python-bin "python")))

(use-package anaconda-mode
  :ensure t
  :after (exec-path-from-shell)
  :init (add-hook 'python-mode-hook 'anaconda-mode)
        (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config (use-package company-anaconda
            :ensure t
            :init (add-hook 'python-mode-hook 'anaconda-mode)
            (eval-after-load "company"
              '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))



(use-package ob-ipython
  :after (:all org)
)

(use-package ob-async
  :after org
  :config
  (require 'ob-async)
  )

(use-package yasnippet
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets))

(use-package bbdb
  :after (helm)
  :config
  (use-package helm-bbdb))

(use-package crux)

(use-package company
  :hook
  (after-init . global-company-mode))

(use-package flycheck)
