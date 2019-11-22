# 2019/11/20
# result precision  

rm(list=ls())

old.opt <- options("warn")
options("warn"=-1)

if(!require("xlsx")){
  install.packages("xlsx")
  library(xlsx)
}

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")

# method one
method_one_origin <- read.csv("./first_scene/origin_answer/bind_two_ans_1.csv", header = T)
method_one_svd <- read.csv("./first_scene/svd_change/svd_two_change_1.csv", header = T)

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
first_method_result <- first_scene(method_one_origin, method_one_svd)
file_name <- paste0("answer_one_",1)
write.xlsx(first_method_result, paste0("./ans_one/",file_name,".xlsx"), sheetName = "ans_1",append = T, row.names = F)



# method two
method_two_origin <- read.csv("./first_scene/origin_answer/bind_two_ans_1.csv", header = T)
method_two_svd <- read.csv("./second_scene/svd_answer/svd_answer_1.csv", header = T)

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
second_method_result <- second_scene(method_two_origin, method_two_svd)
name <- name <- paste0("answer_two_",1)
write.xlsx(second_method_result, paste0("./ans_two/",name,".xlsx"), sheetName = "ans_1",append = T, row.names = F)

options("warn"=old.opt[[1]])










