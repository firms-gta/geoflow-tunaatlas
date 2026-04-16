options(tinytex.tlmgr.path = NULL)
options(tinytex.tlmgr_update = FALSE)

tinytex::tlmgr(c(
  "option",
  "repository",
  "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"
))

tinytex::tlmgr_install(c(
  "lmodern",
  "multirow",
  "adjustbox",
  "booktabs",
  "tabularx",
  "longtable",
  "colortbl",
  "caption",
  "collectbox",
  "environ",
  "trimspaces",
  "varwidth",
  "xcolor"
))