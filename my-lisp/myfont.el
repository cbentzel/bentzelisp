(require 'color-theme)

(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-charcoal-black)

(if (boundp '*site-font*)
  (set-default-font *site-font*)
  (if (eq system-type 'windows-nt)
      (set-default-font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1")
      (set-default-font "Inconsolata-10")))

(require 'highlight-80+)

(provide 'myfont)
