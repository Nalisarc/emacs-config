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

(setq bibtex-completion-bibliography "~/agcloud/bibliography/references.bib"
      bibtex-completion-library-path "~/agcloud/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/agcloud/bibliography/bibtex-notes")

(straight-use-package 'org)
(require 'org-protocol)

(setq indent-tabs-mode nil)
(setq org-src-preserve-indentation nil)
(straight-use-package 'org-roam)
(require 'org-roam-protocol)

(unless (executable-find "sqlite3")
  (add-to-list 'exec-path "path/to/sqlite3") ; TODO REPLACE WITH VARIABLES
  )

(setq org-roam-directory "~/agcloud/org-roam") ; TODO REPLACE WITH VARIABLE
(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory)
  )


(add-hook 'after-init-hook 'org-roam-mode)

(global-set-key (kbd "C-c n") 'org-roam-mode-map)
(define-key org-roam-mode-map (kbd "C-c n l") 'org-roam)
(define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
(define-key org-roam-mode-map (kbd "C-c n g") 'org-roam-graph)
(define-key org-mode-map (kbd "C-c n i") 'org-roam-insert)
(define-key org-mode-map (kbd "C-c n I") 'org-roam-insert-immediate)
(straight-use-package 'org-ref)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq reftex-default-bibliography '("~/agcloud/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/agcloud/bibliography/notes.org"
      org-ref-default-bibliography '("~/agcloud/bibliography/references.bib")
      org-ref-pdf-directory "~/agcloud/bibliography/bibtex-pdfs/")
t
(setq bibtex-completion-pdf-open-function 'org-open-file)
(straight-use-package 'org-noter)
(setq org-capture-templates
      '(
	("t" "Todo" entry (file+headline "~/agcloud/org/gtd.org" "Tasks")
	 "* TODO %?\n ")
	))
(straight-use-package 'hydra)

(straight-use-package
 '(org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org")
   :custom (org-fc-directories '("~/org/"))))

(require 'org-fc-hydra)
(straight-use-package 'org-roam-bibtex)
(add-hook 'org-roam-mode-hook #'org-roam-bibtex-mode)
(straight-use-package 'org-download)
(add-hook 'dired-mode-hook 'org-download-enable)
(straight-use-package 'org-roam-server)
(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 8080
      org-roam-server-authenticate nil
      org-roam-server-export-inline-images t
      org-roam-server-serve-files nil
      org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
      org-roam-server-network-poll t
      org-roam-server-network-arrows nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20)

(org-roam-server-mode)
(straight-use-package 'ob-ipython)
(straight-use-package '(ob-scad :type git :host github :repo "wose/ob-scad"))
  
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (latex . t)
   (python . t)
   (ipython . t)
   (scad . t)
   ))

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
