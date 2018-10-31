(TeX-add-style-hook
 "acmart-pacmpl-template"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("acmart" "acmsmall" "review" "anonymous")))
   (TeX-run-style-hooks
    "latex2e"
    "acmart"
    "acmart10"
    "booktabs"
    "subcaption"))
 :latex)

