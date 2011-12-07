;;;;;;;;;;;;;;;;;;
;;  load-path   ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/")
(add-to-list 'load-path "~/bin/emacsthemes/") ;; zenburn and theme-changer are there, and i'm too lazy to move them to ~/.emacs.d/elisp

;;;;;;;;;;;;
;; themes ;;
;;;;;;;;;;;;

(require 'color-theme)
(color-theme-initialize)
;; zenburn theme
(require 'zenburn)
;; setting calendar for theme-changer.el
(setq calendar-location-name "São Carlos, SP") ;;got lat/long with google maps
(setq calendar-latitude -22.00)
(setq calendar-longitude -47.90)
;; changing theme : day/night
(require 'theme-changer)
(change-theme 'color-theme-bharadwaj-slate 'zenburn)

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;

;; shows matching parenthesis
(show-paren-mode 1)
;; sets ede-mode
(global-ede-mode t)
;; recent-files-mode
(require 'recentf)
(recentf-mode 1)
;; auto-complete-mode
(require 'auto-complete-config)
(ac-config-default)

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
(package-initialize)

;;;;;;;;;;;;;;;;;;;
;; slime-clojure ;;
;;;;;;;;;;;;;;;;;;;

;; clojure-jack-in
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