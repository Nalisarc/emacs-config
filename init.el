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

(setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/bibtex-notes")

(require 'org-protocol)
(straight-use-package 'org-roam)
(require 'org-roam-protocol)

(unless (executable-find "sqlite3")
  (add-to-list 'exec-path "path/to/sqlite3") ; TODO REPLACE WITH VARIABLES
  )

(setq org-roam-directory "~/org-roam") ; TODO REPLACE WITH VARIABLE
(unless (file-directory-p org-roam-directory)
  (make-directory org-roam-directory)
  )


(add-hook 'after-init-hook 'org-roam-mode)
(straight-use-package 'org-ref)
(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(setq bibtex-completion-pdf-open-function 'org-open-file)
(straight-use-package 'org-noter)
(straight-use-package 'hydra)
(straight-use-package
 '(org-fc
   :type git :repo "https://git.sr.ht/~l3kn/org-fc"
   :files (:defaults "awk" "demo.org")
   :custom (org-fc-directories '("~/org/"))
   :config
   (require 'org-fc-hydra)))
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
