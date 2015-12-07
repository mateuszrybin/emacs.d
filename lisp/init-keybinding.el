;; active key-chord
(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global ",."     'undo)
(key-chord-define-global "qw"     'helm-mini)
(key-chord-define-global "QW"     'projectile-persp-switch-project)
(key-chord-define-global "FF"     'helm-projectile-find-file)
(key-chord-define-global "qq"     'project-explorer-toggle)

(key-chord-define-global "UU"      'windmove-left)
(key-chord-define-global "PP"     'windmove-right)
(key-chord-define-global "II"        'windmove-up)
(key-chord-define-global "OO"      'windmove-down)

(key-chord-define-global "as"      'ace-jump-mode)
(key-chord-define-global "sd"      'ace-jump-line-mode)
(key-chord-define-global "AS"      'ace-jump-char-mode)

(key-chord-define-global "QQ"      'ace-window)

;;(key-chord-define-global ""     "<>\C-b")

(provide 'init-keybinding)
