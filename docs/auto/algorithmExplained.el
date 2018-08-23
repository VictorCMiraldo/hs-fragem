(TeX-add-style-hook
 "algorithmExplained"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "11pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("ulem" "normalem") ("hyperref" "linktocpage" "pdfstartview=FitH" "colorlinks" "linkcolor=blue" "anchorcolor=blue" "citecolor=blue" "filecolor=blue" "menucolor=blue" "urlcolor=blue")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art11"
    "inputenc"
    "fontenc"
    "graphicx"
    "grffile"
    "longtable"
    "wrapfig"
    "rotating"
    "ulem"
    "amsmath"
    "textcomp"
    "amssymb"
    "capt-of"
    "natbib"
    "hyperref")
   (LaTeX-add-labels
    "sec:org0680de9"
    "sec:orgac07f33"
    "sec:org1c370dd"
    "sec:orgd29220c"
    "sec:orge455160"
    "sec:org564682a"
    "sec:orgb7b02d6"
    "sec:org37199d7"
    "sec:org6e0d68e"
    "sec:orgf7a9dae"
    "sec:org8c50b4c"
    "sec:orgaa5d4d1"))
 :latex)

