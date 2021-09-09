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

(straight-use-package 'use-package)
(straight-use-package 'use-package-hydra)

(setq agcloud-dir "~/agcloud/")

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package helm
  :straight t
  :bind (("C-c h" . helm-command-prefix)
	 ("M-x" . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode 1)
  )

(use-package helm-bibtex
  :straight t
  :config
  (setq bib-dir (concat (file-name-as-directory agcloud-dir) "bibliography/")
	bibtex-completion-bibliography (concat (file-name-as-directory bib-dir) "references.bib")
	bibtex-completion-library-path (concat (file-name-as-directory bib-dir) "bibtex-pdfs")
	bibtex-completion-notes-path (concat (file-name-as-directory bib-dir) "bibtex-notes")
	))

(use-package org
  :straight t
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture))
  :config
  (setq indent-tabs-mode nil
	org-src-preserve-indentation nil)
  (require 'org-protocol)

)
(add-to-list 'org-src-lang-modes '("latex-macros" . latex))

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))
(add-to-list 'org-src-lang-modes '("inline-js" . javascript))
(defvar org-babel-default-header-args:inline-js
  '((:results . "html")
    (:exports . "results")))
(defun org-babel-execute:inline-js (body _params)
  (format "<script type=\"text/javascript\">\n%s\n</script>" body))
(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t
	org-roam-directory (concat
			    (file-name-as-directory agcloud-dir) "org-roam/")
	)


  :bind (("C-c n i" . org-roam-node-insert)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n l" . 'org-roam-buffer-toggle)
	 ("C-c n d" . 'org-roam-capture-today))
  :after (org)

  :config
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (org-roam-setup))

(use-package org-ref
  :straight t
  :config
  (setq bib-dir (concat (file-name-as-directory agcloud-dir) "bibliography/")
	reftex-default-bibliography (concat (file-name-as-directory bib-dir) "references.bib")
	org-ref-bibliography-notes (concat (file-name-as-directory bib-dir) "notes.org")
	org-ref-pdf-directory (concat (file-name-as-directory bib-dir) "bibtex-pdfs/")
	org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")
	bibtex-completion-pdf-open-function 'org-open-file)
  )
(use-package org-noter
  :straight t
  )
(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline "~/agcloud/org/inbox.org" "Inbox")
	 "* TODO %?\n ")
	("e" "Entry" entry(file+headline "~/agcloud/org/inbox.org" "Inbox")
	 "* %?\n ")
	))
(setq org-agenda-files '("~/agcloud/org/inbox.org"
			   "~/agcloud/org/gtd.org"
			   "~/agcloud/org/tickler.org"))

(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
	 ((org-agenda-overriding-header "Office")
	  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))

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
(setq org-refile-targets '(("~/agcloud/org/gtd.org" :maxlevel . 3)
			    ("~/agcloud/org/someday.org" :level . 1)
			    ("~/agcloud/org/tickler.org" :maxlevel . 2)))
(use-package org-fc
  :straight (:type git
		   :repo "https://git.sr.ht/~l3kn/org-fc"
		   :files (:defaults "awk" "demo.org")
		   )
  )
(straight-use-package 'org-roam-bibtex)
(add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)
(use-package org-download
  :straight t
  :hook (dired-mode-hook . org-download-enable)
  )

(use-package ob-ipython
  :straight t
  )
(use-package ob-scad
  :straight (:type git :host github :repo "wose/ob-scad"))
(use-package ob-async
  :straight t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (scad . t)
   (shell . t)
   ))

(menu-bar-mode -1)
(tool-bar-mode -1) 
(toggle-scroll-bar -1) 

;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load-file custom-file)


(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :straight t)

(straight-use-package 'weyland-yutani-theme)
(load-theme `weyland-yutani t)

(use-package pdf-tools
  :straight t
  :config
  (pdf-tools-install))

(use-package magit
  :straight t)

(use-package crux
  :straight t)

(use-package super-save
  :straight t
  :config
  (super-save-mode +1)
  (setq auto-save-default nil
	super-save-exclude '(".gpg")
	super-save-remote-files nil)
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

(use-package flyspell
  :straight t
  :config
  (setq ispell-program-name "aspell"
	ispell-extra-args '("--sug-mode=ultra"))
  )

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode 1)
  )

(use-package company
  :straight t
  :config (global-company-mode 1)
  )

(use-package nov
  :straight t
  :config
  (setq nov-text-width 80)
  :mode "\\.epub\\'")

(use-package elpy
  :straight t
  :init
  (setq elpy-rpc-python-command "python3")
  :config
  (elpy-enable))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package bbdb
  :straight t)

(use-package helm-bbdb
  :straight t
  :after (helm bbdb)
  )
