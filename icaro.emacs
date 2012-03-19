;;;;;;;;;;;;;;;
;; functions ;;
;;;;;;;;;;;;;;;

(defun add-all-load-path (directory)
  "Recursively adds all subdirectories of <directory>"
  (let ((default-directory directory))
    (normal-top-level-add-subdirs-to-load-path)))

;;;;;;;;;;;;;;;;;;
;;  load-path   ;;
;;;;;;;;;;;;;;;;;;


(add-all-load-path "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/.emacs.d/elpa/yaml-mode-0.0.7/")

;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;

(require 'color-theme)
(color-theme-gnome2)
;; ;; zenburn theme
;; (require 'zenburn)
;; ;; setting calendar for theme-changer.el
;; (setq calendar-location-name "São Carlos, SP") ;;got lat/long with google maps
;; (setq calendar-latitude -22.00)
;; (setq calendar-longitude -47.90)
;; ;; changing theme : day/night
;; (require 'theme-changer)
;; (change-theme 'color-theme-bharadwaj-slate 'zenburn)

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;

(ido-mode 1)

(require 'yaml-mode)

;; shows matching parenthesis
(show-paren-mode 1)
;; sets ede-mode
;; (global-ede-mode t)
;; recent-files-mode
(require 'recentf)
(recentf-mode 1)
;; auto-complete-mode
;; (require 'auto-complete-config)
;; (ac-config-default)

;; flymake-mode
(load "flymake.elc")
;; flymake-ruby
(require 'flymake)
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red2") ;; does not look so ugly with bharadwaj slate
(set-face-background 'flymake-warnline "dark slate blue")

;; shameless copypasta
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
;; disables useless menus, toolbars and scrollbars
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
;; enables column numbers
(column-number-mode 1)
;; enables copy to clipboard
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Marmalade Package Manager ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; oh pretty lady Marmalade
;; add to ~/.emacs.d/init.el if you aren't already using Marmalade.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("tromeyELPA" . "http://tromey.com/elpa/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; substituting M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; substituting M-!
(global-set-key "\C-ct" 'shell)

;;;;;;;;;;;;;;;;;;;;;
;; Opening buffers ;;
;;;;;;;;;;;;;;;;;;;;;

(recentf-open-files nil recentf-menu-title)
(split-window-vertically)		    
(switch-to-buffer-other-window "*scratch*") ;;reopen *scratch*
(switch-to-buffer-other-window recentf-menu-title) ;; focus recentf-open-files
;;(balance-windows) ;; makes all windows the same height (keybinding: \C-x+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Move backup files to a certain directory ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))