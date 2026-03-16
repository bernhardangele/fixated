get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
