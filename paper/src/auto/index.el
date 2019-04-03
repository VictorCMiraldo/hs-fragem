(TeX-add-style-hook
 "index"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "src/body"
    "article"
    "art10"
    "fontenc"
    "inputenc"
    "ismir"
    "amsmath"
    "cite"
    "url"
    "graphicx"
    "color"
    "pgfplots"
    "cleveref")
   (LaTeX-add-bibliographies
    "references"))
 :latex)

