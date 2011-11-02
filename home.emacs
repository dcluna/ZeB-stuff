;; load-path
(add-to-list 'load-path "~/.emacs.d/elisp/")
;; shows matching parenthesis
(show-paren-mode 1)
;; sets ede-mode
(global-ede-mode t)
;; recent-files-mode
(require 'recentf)
(recentf-mode 1)
;; auto-complete-mode
;;(auto-complete-mode 1)
(require 'auto-complete-config)
(ac-config-default)
;; flymake-mode
(load "flymake.elc")
;; flymake-ruby
(require 'flymake)

;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(add-hook 'ruby-mode-hook
          '(lambda ()
	     
	     ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
	     (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
		 (flymake-mode))
	     ))
;; flymake-html
;; (defun flymake-html-init ()
;; 	  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 	                     'flymake-create-temp-inplace))
;; 	         (local-file (file-relative-name
;; 	                      temp-file
;; 	                      (file-name-directory buffer-file-name))))
;; 	    (list "tidy" (list local-file))))
	
;; 	(add-to-list 'flymake-allowed-file-name-masks
;; 	             '("\\.html$\\|\\.ctp" flymake-html-init))
	
;; 	(add-to-list 'flymake-err-line-patterns
;; 	             '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
;; 	               nil 1 2 4))
;; desktop-mode (saves/restores desktop)
;; (load "desktop.elc")
;; (desktop-save-mode 1)
;; disables useless menus
(menu-bar-mode -1)
(tool-bar-mode -1)
;; enables copy to clipboard
(setq x-select-enable-clipboard t)
;; add to ~/.emacs.d/init.el if you aren't already using Marmalade.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;; slime-clojure
(defmacro defslime-start (name mapping)
  `(defun ,name ()
     (interactive)
     (let ((slime-default-lisp ,mapping))
       (slime))))
 
(defslime-start ccl 'ccl)
(defslime-start ccl64 'ccl64)
(defslime-start clojure 'clojure)
(defslime-start sbcl 'sbcl)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
