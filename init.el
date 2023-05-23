;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; ensure correct org
(straight-use-package 'org)

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(when (memq window-system '(mac ns x))
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)))

(use-package use-package-hydra)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))


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

(use-package hydra)

(use-package term
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package company
  :hook
  (after-init . global-company-mode))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

(use-package org
  :ensure t

  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))

  :config
  (require 'org-protocol)
  (setq indent-tabs-mode nil)
  (setq org-src-preserve-indentation t)
  (require 'ob-org)
  (require 'ob-shell)
  (require 'ob-latex)
  (setq org-agenda-custom-commands 
        '(
  	("w" "At work" tags-todo "@work"
  	 ((org-agenda-overriding-header "Work")
  	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)
  	  ))
  	("h" "At home" tags-todo "@home"
  	 	 ((org-agenda-overriding-header "Home")
  	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)
  	  ))
  	))
  
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
  
  )

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (require 'org-roam-protocol)
  (org-roam-setup)
  :bind
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n d" . org-roam-dailies-capture-today)
  ("C-c n r" . org-roam-node-random)
  :custom
  (org-roam-db-location "~/.emacs.d/org-roam.db")
  (org-roam-directory "~/agcloud/org")
  (org-roam-file-exclude-regexp "\\.st[^/]*\\|logseq/.*$")
  (org-roam-completion-everywhere t)
  
  ;; ensure org-roam is creating nodes similarly to Logseq
  ;; bear in mind that it won't be exact mapping due to Logseq's built-in
  ;;    :file/name-format :triple-lowbar
  (setq org-roam-capture-templates '(("d" "default"
				      plain
				      "%?"
				      :target (file+head "pages/${slug}.org" "#+title: ${title}\n")
				      :unnarrowed t)))

  ;; ensure your org-roam daily template follows the journal settings in Logseq
  ;;    :journal/page-title-format "yyyy-MM-dd"
  ;;    :journal/file-name-format "yyyy_MM_dd"
  (setq org-roam-dailies-capture-templates '(("d" "default"
					      entry
					      "* %?"
					      :target (file+head "%<%Y_%m_%d>.org" "#+title: %<%Y-%m-%d>\n")))))

(use-package org-roam-logseq
  :straight (org-roam-logseq :type git
			     :host github
			     :repo "nalisarc/org-roam-logseq")
  )

(use-package org-ref
  :straight (org-ref :type git :host github :repo "jkitchin/org-ref")
  :after (org helm-bibtex hydra)
  :init
  (require 'bibtex)
  

  (setq reftex-default-bibliography "~/agcloud/org/references.bib")
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  :bind
  ("C-c r" . org-ref-bibtex-hydra/body)
  ("C-c ]" . org-ref-insert-link-hydra/body)
  :config
  (setq org-ref-bibliography-notes  "~/agcloud/org/index.org"
	org-ref-default-bibliography "~/agcloud/org/references.bib"
	org-ref-pdf-directory "~/agcloud/Books")

  (require 'org-ref-helm)
  (require 'org-ref-wos)
  (require 'org-ref-scopus)
  (require 'org-ref-isbn)
  (require 'org-ref-pubmed)
  (require 'org-ref-arxiv)
  (require 'org-ref-sci-id)
  (require 'x2bib)
  (setq org-latex-pdf-process
	'("pdflatex -interaction nonstopmode -output-directory %o %f"
	  "bibtex %b"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -interaction nonstopmode -output-directory %o %f"))

  )



(use-package org-download
  :ensure-system-package xclip
  :after org
  :custom
  (setq org-download-image-dir "~/agcloud/org/media/images")
  (setq org-download-heading-lvl nil)
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(use-package org-noter
  :after (org pdf-tools nov)
  :config
  (setq org-noter-doc-property-in-notes t)
  )

(use-package org-fc
   :ensure-system-package gawk
   :after (org)
   :custom (org-fc-directories "~/agcloud/org")
   :config
   (require 'org-fc-hydra)
   )

(use-package python
  :ensure-system-package python3
  
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :config
  (setq python-indent-offset 4))

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

(use-package ob-ipython
  :after (:all org)
  :ensure-system-package
   (jupyter . "pip3 install jupyter")
   )

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

(use-package nov
  :magic ("%EPUB" . nov-mode)
  :config
  (setq nov-text-width 90)
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "OpenDyslexic"
			     :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  )

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package yasnippet
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets))

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)

  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  )

(use-package magit
  :ensure t
  :ensure-system-package git)

(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ("S-RET" . crux-smart-open-line)
  ("C-c c" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-c F" . crux-recentf-find-directory)
  ("C-c u" . crux-view-url)
  ("C-c e" . crux-eval-and-replace)
  ("C-x 4 t" . crux-transpose-windows)
  ("C-c D" . crux-delete-file-and-buffer)
  ("C-c t" . crux-visit-term-buffer)
  ("C-c k" . crux-kill-other-buffers)
  )

(use-package super-save
  :config
  (setq auto-save-default nil)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-remote-files nil)
  :hook
  (find-file super-save-hook-trigger))

(use-package flyspell
  :ensure-system-package aspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra")))

(use-package flycheck)

(use-package bbdb
  :after (helm)
  :config
  (use-package helm-bbdb))

(use-package zeno-theme)

(use-package dictionary
  :ensure-system-package
  ((dictd . dictd)
   (dict . dict)
   )
  :config
  (setq dictionary-server "localhost"))

(menu-bar-mode -1)
(tool-bar-mode -1) 
(toggle-scroll-bar -1)

(defalias 'yes-or-no-p 'y-or-n-p)

;; https://emacsredux.com/blog/2023/03/14/avoid-accidentally-minimizing-emacs/
(global-set-key (kbd "C-z") #'undo)

;; https://emacsredux.com/blog/2022/06/12/auto-create-missing-directories/
(defun er-auto-create-missing-dirs ()
  (let ((target-dir (file-name-directory buffer-file-name)))
    (unless (file-exists-p target-dir)
      (make-directory target-dir t))))

(add-to-list 'find-file-not-found-functions #'er-auto-create-missing-dirs)

(setq ring-bell-function 'ignore)
