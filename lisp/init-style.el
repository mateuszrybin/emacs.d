;; Set font
(global-font-lock-mode 1)
(setq my/font-family "Source Code Pro")
(set-frame-font my/font-family)
(set-face-attribute 'default nil :font my/font-family :height 140)
(set-face-font 'default my/font-family)
