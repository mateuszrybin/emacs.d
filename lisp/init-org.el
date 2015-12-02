;; Use variable width font faces in current buffer
(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "source sans pro"))
  (buffer-face-mode))

;; Set default font faces for Info and ERC modes
(add-hook 'org-mode-hook 'my-buffer-face-mode-variable)
(add-hook 'Info-mode-hook 'my-buffer-face-mode-variable)

;; Change org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; make org-mode better
(setq-default org-indent-mode t)

;; Change theme when in org-mode
(require 'load-theme-buffer-local)
(add-hook 'org-mode
          (lambda nil (load-theme 'org-beautify-theme (current-buffer))))
