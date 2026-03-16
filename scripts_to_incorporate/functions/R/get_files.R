#' Get a List of Files with a Specified Extension from a Directory
#'
#' This function retrieves a list of files with a specified file extension from a user-provided directory. It can filter out non-matching files and sort the files based on numeric order within their names if applicable.
#'
#' @param dir Character string indicating the directory path where the files are located. If not provided, the current working directory is used.
#' @param file_ext Character string representing the desired file extension (e.g., ".asc").
#'
#' @return A character vector containing the full paths to the selected files with the specified extension in the directory.
#'
#' @examples
#' # Retrieve a list of .asc files from the current directory
#' asc_files <- get_files(file_ext = ".asc$")
#'
#' # Retrieve a list of .txt files from a specified directory
#' txt_files <- get_files(dir = "/path/to/directory", file_ext = ".txt")
#'
#' @export
get_files<- function(dir= "", file_ext= ".asc$"){
  
  if(dir== ""){
    dir= getwd()
  }
  
  # get a list of all file in dir:
  all_files<- list.files(dir)
  # remove non-asc files (if present)
  all_files<- all_files[grepl(file_ext, all_files)]
  # remove txt files (of present):
  all_files<- all_files[!grepl(".txt", all_files)]
  
  # sort files by number in string
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  num<- get_num(all_files)
  
  if(!is.na(num[1])){
    all_files<- all_files[order(num, all_files)]
  }
  # convert to directory string for each data file:
  if(length(all_files)>0){
    all_dirs<- NULL
    for(i in 1:length(all_files)){
      all_dirs[i]<- paste(dir, "/", all_files[i], sep = "")
    }
    
    message(paste("Found", toString(length(all_files)), file_ext, "files in the specified directory!", "\n"))
    return(all_dirs)
  }else{
    stop(paste("Found no", file_ext, "files in the specified directory!"))
  }
}# end of get_files()