# 2019/11/08
# second scene
# student not in svd matrix and using Jaccard to calculate

rm(list = ls())

if(!require("jaccard")){
  install.packages("jaccard")
  library(jaccard)
}

old.opt <- options("warn")
options("warn"=-1)

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")
# read file
ten_student <- read.csv("./ten_student/ten_student_1.csv",header = T)
ten_student <- modify_col(ten_student)
neither_ten <- read.csv("./neither_ten_origin_class/neither_ten_student_1.csv", header = T)
neither_ten <- modify_col(neither_ten)

svd_answer <- read.csv("./first_scene/svd_answer/svd_two_ans_1.csv", header = T)
svd_answer <- as.matrix(modify_col(svd_answer))
rownames(svd_answer) <- rownames(ten_student)
svd_answer <- as.data.frame(svd_answer)
# svd_answer <- svd_answer[,-c(ncol(svd_answer))]
svd_answer <- split(svd_answer, f = svd_answer$year)
svd_answer <- split_predict_year(svd_answer)


change_value <- function(tmp_matrix){
  tmp_matrix[tmp_matrix!=-1] <- 1
  tmp_matrix[tmp_matrix==-1] <- 0
  return(tmp_matrix)
} 

ten_student <- split(ten_student, f = ten_student$yesr)
ten_student <- split_predict_year(ten_student)

neither_ten <- split(neither_ten, f = neither_ten$yesr)
neither_ten <- split_predict_year(neither_ten)

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
all_sim <- output_change_matrix(neither_ten, ten_student)

need_rm <- function(answer, all_sim){
  position <- c()
  for(i in 1:nrow(all_sim)){
    place <- which(colnames(answer) == rownames(all_sim)[i])
    position <- append(position, place)
  }
  return (position)
}

position <- need_rm(svd_answer[[2]], all_sim)
svd_answer[[2]] <- svd_answer[[2]][,-position]
ncol(svd_answer[[2]]) # View
nrow(svd_answer[[2]]) # View

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
recommend <- output_class_score(svd_answer[[2]], all_sim)
write.csv(recommend, "./second_scene/svd_answer/svd_answer_1.csv")

options("warn"=old.opt[[1]])






