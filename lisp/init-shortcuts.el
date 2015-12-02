(defun switch-to-personal-current ()
  (interactive)
  (find-file "~/drive/org/current.org"))
(global-set-key (kbd "C-M-1") 'switch-to-personal-current)

(defun switch-to-personal-before ()
  (interactive)
  (find-file "~/drive/org/before.org"))
(global-set-key (kbd "C-M-2") 'switch-to-personal-before)


(defun switch-to-personal-after ()
  (interactive)
  (find-file "~/drive/org/after.org"))
(global-set-key (kbd "C-M-3") 'switch-to-personal-after)

(defun switch-to-personal-emacs ()
  (interactive)
  (find-file "~/drive/org/emacs.org"))
(global-set-key (kbd "C-M-4") 'switch-to-personal-emacs)

(provide 'init-shortcuts)
