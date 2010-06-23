(require 'color-theme)

(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-katester)

(if (eq system-type 'windows-nt)
  (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
  (set-default-font "Inconsolata-8"))

(require 'highlight-80+)

(provide 'myfont)
