# 2019/11/08
# first scene
# student put in svd matrix and just using svd value to recommend

# start predict
rm(list = ls())

old.opt <- options("warn")
options("warn"=-1)

load("./all_function.Rdata")

#setwd("/home/lab/下載/student_perdict")

### function

# bind four matrix to one matrix
bind_one <- function(split_nei_up, split_nei_down, split_ten_up, split_ten_down){
  tmp_nei <- rbind(split_nei_up, split_nei_down)
  tmp_ten <- rbind(split_ten_up, split_ten_down)
  
  result <- cbind(tmp_nei, tmp_ten)
  
  return (result)
}

### choose get the answer matrix or change to -1 value matrix
# input : split ten students, split neither ten students, number(0:origin,1:change to -1)
ans_value_change <- function(split_ten, split_neither, num){
  
  split_nei_up <- split_predict_year(split_neither)[[1]]   # neither ten student matrix, up year
  split_nei_down <- split_predict_year(split_neither)[[2]] # neither ten student matrix, down year
  
  split_ten_up <- split_predict_year(split_ten)[[1]]       # ten student matrix
  ten_up_year <- as.matrix(split_predict_year(split_ten)[[3]])
  colnames(ten_up_year) <- "year"
  split_ten_up <- cbind(split_ten_up, ten_up_year)
  
  if(num == 0){
    split_ten_down <- split_predict_year(split_ten)[[2]]
    ten_down_year <- as.matrix(split_predict_year(split_ten)[[4]])
    colnames(ten_down_year) <- "year"
    split_ten_down <- cbind(split_ten_down, ten_down_year)
  }else{
    split_ten_down <- split_predict_year(split_ten)[[2]]
    split_ten_down[split_ten_down != -1] <- -1
    ten_down_year <- as.matrix(split_predict_year(split_ten)[[4]])
    colnames(ten_down_year) <- "year"
    split_ten_down <- cbind(split_ten_down, ten_down_year)
  }
  return (list(split_nei_up, split_nei_down, split_ten_up, split_ten_down))
}
# output : matrix -> split_nei_up, split_nei_down, split_ten_up, split_ten_down

###

# read file
ten_student <- read.csv("./ten_student/ten_student_1.csv",header = T)
ten_student <- modify_col(ten_student)
ten_name <- colnames(ten_student)
ten_name <- ten_name[-c(length(ten_name))]
neither_ten <- read.csv("./neither_ten_origin_class/neither_ten_student_1.csv", header = T)
neither_ten <- modify_col(neither_ten)
row_name <- rownames(ten_student)

split_ten <- split(ten_student, f = ten_student$yesr) 
split_neither <- split(neither_ten, f = neither_ten$yesr) 
  
answer <- ans_value_change(split_ten, split_neither, 0)  # the answer
change <- ans_value_change(split_ten, split_neither, 1)  # change to -1

# first matrix 
bind_two_ans <- as.matrix(bind_one(answer[[1]], answer[[2]], answer[[3]], answer[[4]]))
ans_colname <- colnames(bind_two_ans)
name_rm_year <- ans_colname[-c(length(ans_colname))]
rownames(bind_two_ans) <- row_name
bind_two_ans <- as.data.frame(bind_two_ans)
ten_position <- which(name_rm_year%in%ten_name)
bind_two_ans <- bind_two_ans[,ten_position:ncol(bind_two_ans)]
write.csv(bind_two_ans,"./first_scene/origin_answer/bind_two_ans_1.csv")

# get_year <- as.matrix(bind_two_ans[,ncol(bind_two_ans)])
# colnames(get_year) <- "year"
# bind_two_ans <- bind_two_ans[,-c(ncol(bind_two_ans))]
# bind_two_ans <- get_normalize(bind_two_ans)
# bind_two_ans <- as.matrix(get_svd_dataframe(bind_two_ans, 0.85))
# rownames(bind_two_ans) <- row_name
# bind_two_ans <- cbind(bind_two_ans, get_year)
# colnames(bind_two_ans) <- ans_colname
# bind_two_ans <- as.data.frame(bind_two_ans)
#write.csv(bind_two_ans,"./first_scene/svd_answer/svd_two_ans_1.csv")
  
# second matrix
bind_two_change <- as.matrix(bind_one(change[[1]], change[[2]], change[[3]], change[[4]]))
change_colname <- colnames(bind_two_change)
rownames(bind_two_change) <- row_name
bind_two_change <- as.data.frame(bind_two_change)
#write.csv(bind_two_change,"./first_scene/origin_change/bind_two_change_1.csv")
get_year <- as.matrix(bind_two_change[,ncol(bind_two_change)])
colnames(get_year) <- "year"
bind_two_change <- bind_two_change[,-c(ncol(bind_two_change))]
bind_two_change <- get_normalize(bind_two_change)
bind_two_change <- as.matrix(get_svd_dataframe(bind_two_change, 0.85))
bind_two_change <- bind_two_change[,ten_position]
change_colname <- change_colname[ten_position]
colnames(bind_two_change) <- change_colname
bind_two_change <- cbind(bind_two_change, get_year)
rownames(bind_two_change) <- row_name
bind_two_change <- as.data.frame(bind_two_change)
write.csv(bind_two_change,"./first_scene/svd_change/svd_two_change_1.csv")


options("warn"=old.opt[[1]])
