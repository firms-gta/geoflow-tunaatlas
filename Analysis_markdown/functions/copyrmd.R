copyrmd <- function(x){
  last_path = function(y){tail(str_split(y,"/")[[1]],n=1)}
  if(!file.exists(paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x)))))
    use_github_file(repo_spec =x,
                    save_as = paste0(gsub(as.character(here::here()),"",as.character(getwd())), paste0("/", last_path(x))),
                    ref = NULL,
                    ignore = FALSE,
                    open = FALSE,
                    host = NULL
    ) }
