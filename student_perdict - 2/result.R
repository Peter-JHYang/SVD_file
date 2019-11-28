# 2019/11/20
# read method one and two result to bind in xlsx

rm(list=ls())

if(!require("xlsx")){
  install.packages("xlsx")
  library(xlsx)
}

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")

# method one
first_scene <- function(method_one_origin, method_one_svd){
  
  method_one_origin <- modify_col(method_one_origin)
  method_one_svd <- modify_col(method_one_svd)
  
  method_one_origin <- split(method_one_origin, f = method_one_origin$year)
  method_one_svd <- split(method_one_svd, f = method_one_svd$year)
  
  method_one_origin <- split_predict_year(method_one_origin)
  method_one_svd <- split_predict_year(method_one_svd)
  
  class_name <- rownames(method_one_origin[[2]])
  
  origin_ten_id <- colnames(method_one_origin[[2]])
  all_tmp <- list()
  for( i in 1:ncol(method_one_origin[[2]]) ){
    tmp <- matrix("", nrow = length(class_name), ncol = 3)
    tmp[,1] <- class_name
    ans_svd_bind <- cbind(method_one_origin[[2]][,i], method_one_svd[[2]][,i])
    tmp[,2:3] <- ans_svd_bind
    tmp <- tmp[order(-as.numeric(tmp[,3])),]
    colnames(tmp) <- c("course_name",origin_ten_id[i],origin_ten_id[i])
    all_tmp <- cbind(all_tmp, tmp)
  }
  all_tmp <- as.data.frame(all_tmp)
  return(all_tmp)
}

# method two
second_scene <- function(method_two_origin, method_two_svd){
  
  method_two_origin <- modify_col(method_two_origin)
  method_two_svd <- modify_col(method_two_svd)
  
  method_two_origin <- split(method_two_origin, f = method_two_origin$year)
  method_two_origin <- split_predict_year(method_two_origin)
  
  class_name <- rownames(method_two_origin[[2]])
  
  origin_ten_id <- colnames(method_two_origin[[2]])
  all_tmp <- list()
  for( i in 1:ncol(method_two_origin[[2]]) ){
    tmp <- matrix("", nrow = length(class_name), ncol = 3)
    tmp[,1] <- class_name
    ans_svd_bind <- cbind(method_two_origin[[2]][,i], method_two_svd[,i])
    tmp[,2:3] <- ans_svd_bind
    tmp <- tmp[order(-as.numeric(tmp[,3])),]
    colnames(tmp) <- c("course_name",origin_ten_id[i],origin_ten_id[i])
    all_tmp <- cbind(all_tmp, tmp)
  }
  all_tmp <- as.data.frame(all_tmp)
  return(all_tmp)
}

output_xlsx_file <- function(){
  first_file_number <- as.numeric(readline(prompt = paste0("what first file do you want?")))
  last_file_number <- as.numeric(readline(prompt = paste0("what last file do you want?")))
  
  for( p in c(first_file_number:last_file_number) ){
    method_one_origin <- read.csv(paste0("./first_scene/origin_answer/bind_two_ans_",p,".csv"), header = T)
    method_one_svd <- read.csv(paste0("./first_scene/svd_change/svd_two_change_",p,".csv"), header = T)
    first_method_result <- first_scene(method_one_origin, method_one_svd)
    file_name <- paste0("answer_one")
    write.xlsx(first_method_result, paste0("./ans_one/",file_name,".xlsx"), sheetName = paste0("ans_",p),append = T, row.names = F)
    
    method_two_origin <- read.csv(paste0("./first_scene/origin_answer/bind_two_ans_",p,".csv"), header = T)
    method_two_svd <- read.csv(paste0("./second_scene/svd_answer/svd_answer_",p,".csv"), header = T)
    
    second_method_result <- second_scene(method_two_origin, method_two_svd)
    name <- name <- paste0("answer_two")
    write.xlsx(second_method_result, paste0("./ans_two/",name,".xlsx"), sheetName = paste0("ans_",p),append = T, row.names = F)
  }
}
output_xlsx_file()










