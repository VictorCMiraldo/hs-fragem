(TeX-add-style-hook
 "index"
 (lambda ()
   (setq TeX-command-extra-options
         "-shell-escape")
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("acmart" "acmsmall" "10pt")))
   (TeX-run-style-hooks
    "latex2e"
    "acmart"
    "acmart10"
    "amstext"
    "amssymb"
    "stmaryrd"
    "polytable"
    "booktabs"
    "subcaption"
    "xcolor"
    "cleveref")
   (TeX-add-symbols
    '("iris" ["argument"] 1)
    '("victor" ["argument"] 1)
    '("restorecolumns" ["argument"] 0)
    '("savecolumns" ["argument"] 0)
    '("HV" 1)
    '("HS" 1)
    '("HT" 1)
    '("HSComment" 1)
    '("HSVar" 1)
    '("HSCon" 1)
    '("HSSym" 1)
    '("HSSpecial" 1)
    '("HSString" 1)
    '("HSChar" 1)
    '("HSNumeral" 1)
    '("HSKeyword" 1)
    '("mathcoloraux" 3)
    '("tmp" 1)
    '("TODO" 1)
    '("warnme" 1)
    '("ra" 1)
    '("Todo" 1)
    '("hsindent" 1)
    '("aligncolumn" 2)
    '("Varid" 1)
    '("Conid" 1)
    '("tex" 1)
    '("ReadOnlyOnce" 1)
    "EndFmtInput"
    "texfamily"
    "Sp"
    "anonymous"
    "plus"
    "bind"
    "rbind"
    "sequ"
    "mathindent"
    "onelinecommentchars"
    "commentbeginchars"
    "commentendchars"
    "visiblecomments"
    "invisiblecomments"
    "NB"
    "SkipToFmtEnd"
    "resethooks"
    "SaveRestoreHook"
    "ColumnHook"
    "onelinecomment"
    "commentbegin"
    "commentend"
    "hspre"
    "hspost"
    "mathcolor")
   (LaTeX-add-labels
    "#1"
    "sec:intro"
    "fig:egbach"
    "fig:egscale"
    "fig:bc")
   (LaTeX-add-environments
    "temp"
    "myhs")
   (LaTeX-add-bibliographies
    "references")
   (LaTeX-add-counters
    "commentctr")
   (LaTeX-add-lengths
    "blanklineskip")
   (LaTeX-add-xcolor-definecolors
    "C1"
    "C2"
    "hsblack"
    "hsgold1"
    "hsgold2"
    "hsgold3"
    "hsblue1"
    "hsblue2"
    "hsblue3"
    "hsblue4"
    "hsblue5"
    "hsred2"
    "hsred3"))
 :latex)

