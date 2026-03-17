##' Create list of script paths and other info related to a script
##' @param path Absolute path to script
##' @return A list of
##' \itemize{
##' \item path Absolute path "/data/dir/script.R"
##' \item name "script"
##' \item label "dir/script.R"
##' }
##' @export


scriptPaths <- function(path){
    list(path=path,
         name=sub("\\.R(md)* *$","",basename(path)),
         label=sub("^/data/","",path),
         dir.main=dirname(dirname(path))
         )
}
