;jedi

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; markdown mode http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Add my personal directory to the path 
(setq load-path (cons "~/.emacs.d/" load-path))

;; Trick to get the filename of the installation directory
(defconst epy-install-dir
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs-for-python"
)

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line


;; use company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; https://github.com/TommyX12/company-tabnine
(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(add-to-list 'load-path epy-install-dir)

(require 'epy-setup)
(require 'epy-python)
(require 'epy-completion)
(require 'epy-editing)
(require 'epy-bindings)

(require 'jhc-backups)
(require 'jhc-navigation)
(require 'jhc-buffers)
(require 'jhc-spellcheck)
(require 'jhc-appearance)
(require 'jhc-orgmode)

(require 'template)
(template-initialize)

;(require 'pdb)

(setq auto-mode-alist
      (append '(
                ("\\.css\\'"                           . css-mode)
                ("\\.\\(htm\\|html\\|xhtml\\)$"        . html-mode)
                ("\\.sql$"                             . sql-mode)
                ("\\.js$"                              . js-mode)
                ("\\.json$"                            . js-mode)
                ("\\.js$"                              . js-mode)
                ("\\.py"                               . python-mode)
                ;; sorted by chapter
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
                ("\\.txt$"                             . org-mode)
                ("\\.dat$"                             . ledger-mode)

                ("\\.log$"                             . text-mode)
                ("\\.tex$"                             . LaTeX-mode)
                ("\\.tpl$"                             . LaTeX-mode)
                ("\\.cgi$"                             . perl-mode)
                ("[mM]akefile"                         . makefile-mode)
                ("\\.bash$"                            . shell-script-mode)
                ("\\.expect$"                          . tcl-mode)

                (".ssh/config\\'"                      . ssh-config-mode)
                ("sshd?_config\\'"                     . ssh-config-mode)
                ) auto-mode-alist))


;; Perl based replace function
(defun perl-replace (start end)
  "Replace a text pattern in a  region using perl expressions"
  (interactive "*r")
  (setq regexp (read-string "Regexp: "))
  (setq to-string (read-string (concat "[" regexp "] Replacement: ")))
  (setq command (concat "perl -e 's/" regexp "/" to-string "/g' -p"))
  (print command)
  (shell-command-on-region start end command nil 1)
  )
(global-set-key "\M-r" 'perl-replace)

;; Fullscreen emacs
(defun switch-full-screen ()
      (interactive)
      (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'switch-full-screen)

; My references database
(setq reftex-default-bibliography   (quote       ("~/refs.bib")))       

;; ;;;;;;;;;;;; TRAMP
(setq tramp-default-method "scp")
(setq tramp-default-user "jhuerta")
(setq tramp-auto-save-directory "/tmp/")
;; "turn off" the effect of `backup-directory-alist' for TRAMP files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(inverse-video nil)
 '(line-number-display-limit nil)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (company jedi-direx jedi)))
 '(select-enable-clipboard t)
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(truncate-partial-width-windows t)
 '(use-file-dialog nil)
 '(visible-bell t)
 '(x-stretch-cursor nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "grey10" :foreground "grey85" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flyspell-duplicate ((t (:underline t :slant italic))))
 '(flyspell-incorrect ((t (:underline t :weight bold))))
 '(font-lock-comment-face ((t (:foreground "indianred"))))
 '(font-lock-keyword-face ((t (:foreground "turquoise3"))))
 '(font-lock-string-face ((t (:foreground "palegreen4"))))
 '(outline-2 ((t (:foreground "steelblue1"))))
 '(rst-level-1-face ((t (:background "darkred"))) t)
 '(rst-level-2-face ((t (:background "grey78"))) t)
 '(rst-level-3-face ((t (:background "darkgreen"))) t)
 '(rst-level-4-face ((t (:background "grey20"))) t))
