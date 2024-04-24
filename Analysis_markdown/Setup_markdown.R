## ----include=FALSE---------------------------------------------------------------------------------------------------------

knitr::opts_chunk$set(
	echo = FALSE,
	error = 0,
	fig.align = "center",
	message = FALSE,
	warning = FALSE,
	tab.cap.pre = "Table ",
	tab.cap.sep = ": ", results = 'asis'
)
base::options(knitr.duplicate.label = "allow")



## ----options, include=FALSE------------------------------------------------------------------------------------------------

base::options(scipen=9999)
plotting_type <- "plot" 
if (knitr::is_html_output()){plotting_type <- "view" }
tmap_mode(plotting_type)



## ----message=FALSE, warning=FALSE, include=FALSE---------------------------------------------------------------------------

set_flextable_defaults(
  font.size = 10,
  font.color = "black",
  digits = 2,
  theme_fun = "theme_box"
)


#set the dataset caption styling
knitr::opts_chunk$set(tab.cap.pre = "Table ", tab.cap.sep = ": ")

#set the dataset caption styling
autonum <- officer::run_autonum(seq_id = "tab", bkm = "TC1", bkm_all = TRUE) # number the table, bkm (bookmark) is important as the cross-referencing is done using the bookmark



