source("Global.R")

outputDir <- "data" # directory with .fcs files
user_dataset_names <- list() # store filenames for the raws

#return filenames in
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_files_from_dir <- function() {
  filenames <-
    list.files(outputDir, pattern = "*.fcs$") # list of filenames
  user_dataset_names <- c(user_dataset_names, filenames)
  return(user_dataset_names)
}

removeFiles <- function(filename) {
  unlink(filename, recursive = F, force = F)
  #file.remove(filename)
}



getColnames <- function(){
  
}

