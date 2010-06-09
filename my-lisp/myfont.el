(require 'color-theme)
(require 'emacs-type)

(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-goldenrod)

(if-not-win32 (set-default-font "-bitstream-bitstream vera sans mono-medium-r-normal--10-0-0-0-m-0-iso8859-9"))

(require 'highlight-80+)

(provide 'myfont)
