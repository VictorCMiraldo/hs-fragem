(TeX-add-style-hook
 "body"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-symbols
    '("changed" 1)
    '("iris" 1)
    '("victor" 1)
    "dimbc")
   (LaTeX-add-labels
    "sec:intro"
    "fig:egbach"
    "fig:egscale"
    "fig:bwv861-start"
    "sec:fractal-geom"
    "fig:egboxcount"
    "fig:egscalingmusic"
    "fig:simpeg"
    "fig:extract"))
 :latex)

