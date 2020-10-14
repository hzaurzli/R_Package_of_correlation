# Correlation!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

read_count_files <- function(file,start,end){
  file_names <- list.files(file)
  path = file
  for (i in 1:length(file_names)) {
    if (i == 1) {
      names = gsub(".htseq","",file_names[i])
      q = assign(names,read.table(paste0(path,file_names[i]),header = T,sep = '\t'))
    }
    else{
      names = gsub(".htseq","",file_names[i])
      p = assign(names,read.table(paste0(path,file_names[i]),header = T,sep = '\t'))
      q = merge(q,p)
      q = q[-c(1,2,start:end),]
    }
  }
  return(q)
}


count_files_median <- function(file,col_start,col_end){
  vec = vector()
  for (i in 1:nrow(file)) {
    med = median(as.numeric(file[i,col_start:col_end]))
    vec[i] = med
  }
  return(vec)
}

Correlate <- function(data_1,data_2,col_start,col_end){
  corr = vector()
  for (i in col_start:col_end) {
    select = data_1[,i]
    corr[i] = cor(select,data_2,method = "spearman")
  }
  return(corr)
}


diff_cor = function(data,feature_1,feature_2){
  apply(data,2,function(y){
    dat = cor(y,feature_1) - cor(y,feature_2)
    return(dat)
  })
}


