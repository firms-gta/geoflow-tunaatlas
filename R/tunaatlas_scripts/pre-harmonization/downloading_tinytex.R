options(tinytex.tlmgr.path = NULL)

tinytex::tinytex_root()
tinytex::is_tinytex()
tinytex::tlmgr("--version")

tinytex::tlmgr(c(
  "option",
  "repository",
  "https://ftp.math.utah.edu/pub/tex/historic/systems/texlive/2025/tlnet-final"
))

tinytex::tlmgr("--verify-repo=none update --self")

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