(require 'color-theme)

(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-goldenrod)

(if (eq system-type 'windows-nt)
    (set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
  (set-default-font "-bitstream-bitstream vera sans mono-medium-r-normal--10-0-0-0-m-0-iso8859-9"))

(require 'highlight-80+)

(provide 'myfont)
