;;;;;;;;;;;;;;;;;
;;;; General ;;;;
;;;;;;;;;;;;;;;;;

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)

;; Save my seestion when close
(desktop-save-mode 1)

;; Winner mode
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; wind mode change window with shift arrow keys
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

  ;; Turn on ido-mode
  (require 'ido)
  (ido-mode t)
;; set tab width
(setq-default tab-width 2)

;; Config for helm
(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-M-x-fuzzy-match                  t ; optional fuzzy matching for helm-M-x
      helm-ff-file-name-history-use-recentf t
      helm-buffers-fuzzy-matching           t
      helm-recentf-fuzzy-match              t)

(helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; project control ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'helm-projectile-find-file)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-enable-caching t)


;;;;;;;;;;;;;;;;;;;;;;
;;;; Frame/buffer ;;;;
;;;;;;;;;;;;;;;;;;;;;;

(dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
  (with-current-buffer (get-buffer buf)
    (make-local-variable 'face-remapping-alist)
    (add-to-list 'face-remapping-alist '(default (:background "#1C1F21")))))

;; Kill the current buffer, ask only if modfied
(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "M-k") 'kill-current-buffer)

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Remove modeline and activate header
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

;; set lines format
(setq linum-format " %d ")
(hlinum-activate)
(setq make-backup-files nil)

(require 'adaptive-wrap)
(when (fboundp 'adaptive-wrap-prefix-mode)
  (defun my-activate-adaptive-wrap-prefix-mode ()
    "Toggle `visual-line-mode' and `adaptive-wrap-prefix-mode' simultaneously."
    (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook 'my-activate-adaptive-wrap-prefix-mode))



;;;;;;;;;;;;;;;;;;;;
;;;; Zen Coding ;;;;
;;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "RET") 'newline-and-indent)

;;;;;;;;;;;;;;;;;
;;;; Cursor ;;;;;
;;;;;;;;;;;;;;;;;

;; Change curser type
(set-default 'cursor-type 'hbar)



;;;;;;;;;;;;;;;;;
;;;; Shell ;;;;;;
;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;;;; Git ;;;;
;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;;;; Style ;;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;;; misc ;;;;
;;;;;;;;;;;;;;

;; change to yes and no thing
(fset 'yes-or-no-p 'y-or-n-p)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; @see http://stackoverflow.com/questions/4222183/emacs-how-to-jump-to-function-definition-in-el-file
(global-set-key (kbd "C-h C-f") 'find-function)

;; from RobinH, Time management
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;;a no-op function to bind to if you want to set a keystroke to null
(defun void () "this is a no-op" (interactive))

(defalias 'list-buffers 'ibuffer)

;effective emacs item 7; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; increase and decrease font size in GUI emacs
(when (display-graphic-p)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease))

;; {{ https://github.com/nschum/highlight-symbol.el
(autoload 'highlight-symbol "highlight-symbol" "" t)
(autoload 'highlight-symbol-next "highlight-symbol" "" t)
(autoload 'highlight-symbol-prev "highlight-symbol" "" t)
(autoload 'highlight-symbol-nav-mode "highlight-symbol" "" t)
(autoload 'highlight-symbol-query-replace "highlight-symbol" "" t)
;; }}

(defun toggle-full-window()
  "Toggle the full view of selected window"
  (interactive)
  ;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Splitting-Windows.html
  (if (window-parent)
      (delete-other-windows)
    (winner-undo)
    ))

(setq system-time-locale "C")

(getenv "PATH")
 (setenv "PATH"
(concat
 "/usr/texbin" ":"

(getenv "PATH")))

(provide 'init-misc)
