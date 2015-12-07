;; use setq-default to set it for /all/ modes
(setq-default header-line-format
  (list
    "   "
    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat  (propertize "○ "
                             'face 'font-lock-keyword-face
                             'help-echo "Buffer has been modified"))))


    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))
        "   "
    ;; the current major mode for the buffer.

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    ))

(provide 'init-headerline)
