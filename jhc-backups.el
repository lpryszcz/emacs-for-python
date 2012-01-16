;; Enable backups 
(setq vc-make-backup-files t)
(setq make-backup-files t) 

;; I do not want backups of files in the same directory 
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs-backups"))   ; don't litter my fs tree
    delete-old-versions t    ; don't ask me
    kept-new-versions 100
    kept-old-versions 20
    version-control t        ; use versioned backups
)

(provide 'jhc-backups)