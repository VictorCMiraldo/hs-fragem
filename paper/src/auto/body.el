(TeX-add-style-hook
 "body"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (LaTeX-add-labels
    "sec:intro"
    "fig:egbach"
    "fig:egscale"
    "fig:bc"))
 :latex)

