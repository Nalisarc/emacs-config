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

(setq agcloud "~/agcloud")
(setq org-dir "~/agcloud/org")
(setq notes-dir "~/agcloud/org-roam")
(setq sqlite3-path "")
(setq books-dir "~/agcloud/Books")
(setq bib-dir "~/agcloud/bibliography")
(setq scimax-dir "~/.emacs.d/scimax")

;; (concat (file-name-as-directory dirfile) relfile) 

(add-to-list 'load-path scimax-dir)

(straight-use-package 'use-package)
(straight-use-package 'use-package-hydra)

(straight-use-package 'dashboard)
(require 'dashboard)
(dashboard-setup-startup-hook)
  
(defalias 'yes-or-no-p 'y-or-n-p)
  
(straight-use-package 'helm)
(helm-mode 1)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c h o") 'helm-occur)
  
(straight-use-package 'helm-bibtex)

(setq bibtex-completion-bibliography  (concat (file-name-as-directory bib-dir) "references.bib")
      bibtex-completion-library-path books-dir
      bibtex-completion-notes-path notes-dir)
  
(straight-use-package 'org)
(require 'org-protocol)

(setq indent-tabs-mode nil)
(setq org-src-preserve-indentation t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
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
(straight-use-package 'org-roam)
(require 'org-roam-protocol)


(unless (executable-find "sqlite3")
  (add-to-list 'exec-path sqlite3-path)
  )

(setq org-roam-directory notes-dir) 
(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory)
  )

(with-eval-after-load 'org
  (progn
    (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
    (setq org-roam-db-location
	(concat  (file-name-as-directory org-roam-directory) "org-roam.db"))
    (org-roam-setup)

    (global-set-key (kbd "C-c n i") 'org-roam-node-insert)
    (global-set-key (kbd "C-c n f") 'org-roam-node-find)
    (global-set-key (kbd "C-c n l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c n d") 'org-roam-dailies-capture-today)
    (global-set-key (kbd "C-c n r") 'org-roam-node-random)

    ))
(straight-use-package 'org-ref)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq reftex-default-bibliography '((concat (file-name-as-directory bib-dir) "references.bib")))

(setq org-ref-bibliography-notes  '(concat (file-name-as-directory notes-dir) "index.org")
      org-ref-default-bibliography ' (concat (file-name-as-directory bib-dir) "references.bib") 
      org-ref-pdf-directory books-dir)
(setq bibtex-completion-pdf-open-function 'org-open-file)

(require 'org-ref)
(require 'org-ref-helm)

(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link-hydra/body)
(straight-use-package 'org-noter)
(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline (concat org-dir "inbox.org") "Inbox")
	 "* TODO %?\n ")
	("e" "Entry" entry(file+headline (concat org-dir "inbox.org") "Inbox")
	 "* %?\n ")
	))
(setq org-agenda-files '((concat (file-name-as-directory org-dir) "gtd.org")
			 (concat (file-name-as-directory org-dir) "tickler.org")
			 (concat (file-name-as-directory org-dir) "index.org")))

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
(setq org-refile-targets '(((concat (file-name-as-directory dirfile) "gtd.org") :maxlevel . 3)
			   ((concat (file-name-as-directory dirfile) "someday.org") :level . 1)
			   ((concat (file-name-as-directory dirfile) "tickler.org") :maxlevel . 2)))
(straight-use-package
 '(org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org")
   :custom (org-fc-directories '(notes-dir))))

(require 'org-fc-hydra)
(straight-use-package 'org-roam-bibtex)
(add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)
(straight-use-package 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(straight-use-package 'ob-ipython)
(straight-use-package '(ob-scad :type git :host github :repo "wose/ob-scad"))
(require 'ob-scad)
(straight-use-package 'ob-async)
(require 'ob-async)
;;(setq ob-async-no-async-languages-alist '("ipython"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ipython . t)
   (scad . t)
   (shell . t)
   ))

(require 'ox-md)
(require 'ox-twee2)
  
(menu-bar-mode -1)
(tool-bar-mode -1) 
(toggle-scroll-bar -1) 
  
;; https://stackoverflow.com/questions/14071991/how-to-create-an-empty-file-by-elisp
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load-file custom-file)

  
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(yas-global-mode 1)
  
(straight-use-package 'weyland-yutani-theme)
(load-theme `weyland-yutani t)
  
(straight-use-package 'pdf-tools)
(pdf-tools-install)
  
(straight-use-package 'magit)
  
(straight-use-package 'crux)
  
(straight-use-package 'super-save)
  
(super-save-mode +1)
  
(setq auto-save-default nil)
  
(setq super-save-exclude '(".gpg"))
  
(setq super-save-remote-files nil)
  
(add-to-list 'super-save-hook-triggers 'find-file-hook)
  
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
  
(straight-use-package 'flycheck)
(straight-use-package 'flycheck-rust)
(add-hook 'after-init-hook #'global-flycheck-mode)
  
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)
  
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(setq nov-text-width 90)
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "OpenDyslexic"
                                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

(straight-use-package 'elpy)

(setq elpy-rpc-python-command "python3")

(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--pylab=osx --pdb --nosep --classic"
      python-shell-prompt-regexp ">>> "
      python-shell-prompt-output-regexp ""
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

  
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
  
(straight-use-package 'bbdb)
(straight-use-package 'helm-bbdb)

(straight-use-package 'speedread)
