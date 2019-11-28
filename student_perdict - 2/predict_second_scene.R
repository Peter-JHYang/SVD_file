# 2019/11/08
# second scene
# student not in svd matrix and using Jaccard to calculate

rm(list = ls())

if(!require("jaccard")){
  install.packages("jaccard")
  library(jaccard)
}

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")

# function
change_value <- function(tmp_matrix){
  tmp_matrix[tmp_matrix!=-1] <- 1
  tmp_matrix[tmp_matrix==-1] <- 0
  return(tmp_matrix)
} 

output_change_matrix <- function(neither_ten, ten_student){
  
  ten_student[[1]] <- change_value(ten_student[[1]])
  neither_ten[[1]] <- change_value(neither_ten[[1]])
  
  tmp_nei_ten <- matrix(0, nrow =ncol(ten_student[[1]]) , ncol = ncol(neither_ten[[1]]))
  colnames(tmp_nei_ten) <- colnames(neither_ten[[1]])
  rownames(tmp_nei_ten) <- colnames(ten_student[[1]])
  for( i in 1:ncol(ten_student[[1]])){
    #count = 0
    for(j in 1:ncol(neither_ten[[1]])){
      jac_value <- jaccard(ten_student[[1]][i], neither_ten[[1]][j])
      if(jac_value > 0.5){
        tmp_nei_ten[i,j] <- jac_value 
        #count <- count + 1
      }else{
        tmp_nei_ten[i,j] <- NA
      }
    }
  }
  return (tmp_nei_ten)
}

need_rm <- function(answer, all_sim){
  #answer <- svd_answer[[2]]
  position <- c()
  for(i in 1:nrow(all_sim)){
    place <- which(colnames(answer) == rownames(all_sim)[i])
    position <- append(position, place)
  }
  return (position)
}

output_class_score <- function(answer, all_sim){
  #answer <- svd_answer[[2]]
  
  recommend <- matrix(0, nrow = nrow(answer), ncol = nrow(all_sim))
  colnames(recommend) <- rownames(all_sim)
  rownames(recommend) <- rownames(answer)
  
  for( i in 1:nrow(all_sim) ){
    
    class_value <- c()
    for( j in 1:nrow(answer) ){
      count <- 0
      value <- 0
      for( k in 1:ncol(answer) ){
        if(!is.na(all_sim[i,k])){
          count <- count + 1
          value <- value + all_sim[i,k] * answer[j,k]
        }
      }
      value <- value / count
      class_value <- append(class_value, value)
    }
    recommend[,i] <- class_value
  }
  return (recommend)
}

method_two_function <- function(){
  first_file_number <- as.numeric(readline(prompt = paste0("what first file do you want?")))
  last_file_number <- as.numeric(readline(prompt = paste0("what last file do you want?")))
  
  for( p in c(first_file_number:last_file_number) ){
    # read file
    ten_student <- read.csv(paste0("./ten_student/ten_student_",p,".csv"),header = T)
    ten_student <- modify_col(ten_student)
    neither_ten <- read.csv(paste0("./neither_ten_origin_class/neither_ten_student_",p,".csv"), header = T)
    neither_ten <- modify_col(neither_ten)
    
    svd_answer <- read.csv(paste0("./first_scene/svd_answer/svd_two_ans_",p,".csv"), header = T)
    svd_answer <- as.matrix(modify_col(svd_answer))
    rownames(svd_answer) <- rownames(ten_student)
    svd_answer <- as.data.frame(svd_answer)
    # svd_answer <- svd_answer[,-c(ncol(svd_answer))]
    svd_answer <- split(svd_answer, f = svd_answer$year)
    svd_answer <- split_predict_year(svd_answer)
    
    ten_student <- split(ten_student, f = ten_student$year)
    ten_student <- split_predict_year(ten_student)
    
    neither_ten <- split(neither_ten, f = neither_ten$year)
    neither_ten <- split_predict_year(neither_ten)
    
    all_sim <- output_change_matrix(neither_ten, ten_student)
    
    position <- need_rm(svd_answer[[2]], all_sim)
    svd_answer[[2]] <- svd_answer[[2]][,-position]
    ncol(svd_answer[[2]]) # View
    nrow(svd_answer[[2]]) # View
    
    recommend <- output_class_score(svd_answer[[2]], all_sim)
    write.csv(recommend, paste0("./second_scene/svd_answer/svd_answer_",p,".csv"))
  }
}
method_two_function()







