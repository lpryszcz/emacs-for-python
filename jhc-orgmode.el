(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-0") 'reftex-citation)
  )

(add-hook 'org-mode-hook 'org-mode-reftex-setup)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-todo-keywords
       '((sequence "TODO(t)" "ONGOING(o)" "|" "DONE(d!)" "CANCELED(c!@)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-todo) 
        ("ONGOING" . org-table)
        ("DONE" . org-done)
        ("CANCELED" . org-archived)))

(add-hook 'org-mode-hook (lambda ()
                           ;; display images
                           (local-set-key "\M-I" 'org-toggle-iimage-in-org)
                           (global-set-key [(S-iso-lefttab)] 'my-hippie-expand)
                           (define-key global-map (kbd "C-TAB") 'org-global-cycle)
                           (define-key global-map (kbd "C-c l") 'org-store-link)
                           (define-key global-map (kbd "C-c a") 'org-agenda)
                           ))

;; To display images inline in Org-mode
(defun org-toggle-iimage-in-org ()
  "display images in your org file"
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
      (set-face-underline-p 'org-link t))
  (iimage-mode))

;; activate single letter commands (for example outline navigation with
;; `f', `b', `n', and `p') at beginning of a headline:
;; - f :: org-forward-same-level
;; - b :: org-backward-same-level
;; - n :: outline-next-visible-heading
;; - p :: outline-previous-visible-heading
(setq org-use-speed-commands t)

(provide 'jhc-orgmode)
