# 2019/11/22
# read method one and two result to bind in xlsx

rm(list=ls())

if(!require("xlsx")){
  install.packages("xlsx")
  library(xlsx)
}
if(!require("readxl")){
  install.packages("readxl")
  library(readxl)
}

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")

# function
calculate <- function(matrix){
  matrix <- method_one_ans
  total_matrix <- matrix(0, nrow = 11, ncol = 10)
  average_matrix <- matrix(0, nrow = 11, ncol = 1)
  start <- 1
  for( i in 1:10 ){
    end <- start + 2
    catch <- as.matrix(matrix[,start:end])
    finish_position <- which(catch[,2]!=-1)
    finish_class_len <- length(catch[,2][catch[,2]!=-1])
    space <- finish_class_len/10
    class_number <- 0
      
    for( j in 1:nrow(total_matrix) ){
      
      j/finish_position[j]
      class_number <- class_number + space
      get_need <- round(class_number)
      total_matrix[j,i] <- round(get_need / , digits = 3)
      
    }
    start <- end + 1
  }
  total_matrix[is.na(total_matrix)] <- 0
  for( k in 1:nrow(total_matrix)){
    average_matrix[k,1] <- sum(total_matrix[k,])/ncol(total_matrix)
  }
  rownames(average_matrix) <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
  
  return (list(total_matrix, average_matrix))
}


final_result_matrix <- function(){
  first_file_number <- as.numeric(readline(prompt = paste0("what first file do you want?")))
  last_file_number <- as.numeric(readline(prompt = paste0("what last file do you want?")))
  
  for( p in c(first_file_number:last_file_number) ){
    
    method_one_ans <- read_excel("./ans_one/answer_one.xlsx", sheet = p )
    method_two_ans <- read_excel("./ans_two/answer_two.xlsx", sheet = p )
    
    method_one_ans <- modify_xlsx_col(method_one_ans)
    method_two_ans <- modify_xlsx_col(method_two_ans)
    
    method_one_ten <- calculate(method_one_ans)[[1]]
    one_ten_name <- paste0("one_total_",p)
    write.xlsx(method_one_ten, paste0("./precision_recall/method_one/",one_ten_name,".xlsx"), sheetName = "ten_total",append = T, row.names = F)
    
    method_one_ten_average <- calculate(method_one_ans)[[2]]
    
    method_two_ten <- calculate(method_two_ans)[[1]] 
    two_ten_name <- paste0("two_total_",p)
    write.xlsx(method_two_ten, paste0("./precision_recall/method_two/",two_ten_name,".xlsx"), sheetName = "ten_total",append = T, row.names = F)
    
    method_two_ten_average <- calculate(method_two_ans)[[2]]
    
    bind_two_mathod <- cbind(method_one_ten_average, method_two_ten_average)
    colnames(bind_two_mathod) <- c("使用SVD分數","修課相似度與SVD分數計算")
    bind_file_name <- paste0("bind")
    write.xlsx(bind_two_mathod, paste0("./precision_recall/bind_result/",bind_file_name,".xlsx"), sheetName = paste0("bind",p),append = T, row.names = F)
    
  }
}
final_result_matrix()































