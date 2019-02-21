(TeX-add-style-hook
 "ISMIR2019template"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "url")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "fontenc"
    "inputenc"
    "ismir"
    "amsmath"
    "cite"
    "url"
    "graphicx"
    "color")
   (LaTeX-add-labels
    "sec:intro"
    "fig:egbach"
    "fig:egscale")
   (LaTeX-add-bibliographies
    "sources"))
 :latex)

