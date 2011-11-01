;for haskell-mode
(load "~/haskell/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq haskell-program-name "C:/Program Files (x86)/Haskell Platform/2011.2.0.1/bin/ghci.exe")
(set-variable 'inferior-lisp-program "C:/gcl.exe")
(autoload 'fi:common-lisp "fi-site-init" "" t)
;for php-mode
(load "~/emacs-23.3/extra/php-mode")
;hides welcome screen on startup
(setq inhibit-startup-message t)
;for ruby-mode, enables pretty colors
(global-font-lock-mode 1)
;recent files mode
(require 'recentf)
(recentf-mode 1)
;; for color-theme
;(add-to-list 'load-path "~/.emacs.23.3/extra/")
;(require 'color-theme)
;(eval-after-load "color-theme"
;  '(progn
;     (color-theme-initialize)
;     (color-theme-hober)))
;; for nyan-mode
(load "~/emacs-23.3/extra/nyan-mode")
;; sets show-paren-mode
(show-paren-mode 1)
;; sets all org files on the dir to the todo list
(setq org-agenda-files (file-expand-wildcards "~/txt/org/*.org"))
;; keybind for org-agenda = C-c a
(global-set-key (kbd "C-c a") 'org-agenda)
;; js-mode
(load "~/emacs-23.3/extra/js2-mode.elc")
;; typing-mode game
(load "~/emacs-23.3/extra/typing.el")
(autoload 'typing-of-emacs "typing" "The Typing Of Emacs, a game." t)
;; tip of the day
(require 'cl)
(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))
(add-hook 'after-init-hook 'totd)
;; auto-complete mode
(add-to-list 'load-path "~/emacs-23.3/extra/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs-23.3/extra/ac-dict")
(ac-config-default)
;; full screen space
(if (fboundp 'scroll-bar-mode) 
    (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) 
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
;; csharp-mode
(load "~/emacs-23.3/extra/csharp-mode-0.8.5.el")