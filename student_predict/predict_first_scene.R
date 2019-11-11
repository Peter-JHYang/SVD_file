# 2019/11/08
# first scene
# student put in svd matrix and just using svd value to recommend

# start predict
rm(list = ls())

if(!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
}
if(!require("RMySQL")){
  install.packages("RMySQL")
  library(RMySQL)
}
if(!require("DBI")){
  install.packages("DBI")
  library(DBI)
}

old.opt <- options("warn")
options("warn"=-1)

#setwd("/home/lab/下載/tmp")
# read file
back <- read.csv("./back.csv",header = T)
get_origin <- read.csv("./get_class_history.csv",header=T)

# load DB
# con <- dbConnect(MySQL(), dbname = "r_use", username = "root", password = "86070586")
# dbSendQuery(con, "SET NAMES 'utf8'") #big5
# final_save <- dbGetQuery(con, "select * from final_save")
# dbDisconnect(con)
# need_final <- final_save[,c(1,4,7,14)]
# need_final_split <- split(need_final, f = need_final$ID)
# finish_class <- c()
# for( n in 1:length(need_final_split) ){
#   time_tmp <- split(need_final_split[[n]], f = need_final_split[[n]]$save_finishtime) 
#   remove_old_save <- lapply(time_tmp, function(x){if(x$save_finishtime==time_tmp[[length(time_tmp)]]$save_finishtime)return(x) else return(NULL)})
#   remove_old_save[sapply(remove_old_save, is.null)] <- NULL
#   finish_class <- append(finish_class, remove_old_save)
# }

class_name <- back[,2] # storage the class names

modify_col <- function(data){
  data <- data[,-c(1)]
  # change dataframe column name
  data_colname <- colnames(data)
  for( i in 1:length(data_colname) ){
    if(substr(data_colname[i],1,1) == "X"){
      data_colname[i] <- gsub("X","",data_colname[i])
    }
  }
  colnames(data) <- data_colname
  
  return (data)
}
back <- modify_col(back)
get_origin <- modify_col(get_origin)

# split using the last column, the year
split_back <- split(back, f = back$year )
split_origin <- split(get_origin, f = get_origin$year)

# after split, get two matrix, one is history matrix, and the other is the answer
split_predict_year <- function(need_split, class_name){
  #need_split <- split_back
  if(length(need_split) > 3){
    old_mid <- rbind(need_split[[1]], need_split[[2]], need_split[[3]], need_split[[4]])
    old_mid <- old_mid[,c(-ncol(old_mid))]
    old_mid <- old_mid[,-c(1)]
    should_predict <- need_split[[length(need_split)]][,c(-ncol(need_split[[length(need_split)]]))]
    should_pre_name <- should_predict[,1]
    should_predict <- should_predict[,-c(1)]
  }else{
    old_mid <- rbind(need_split[[1]], need_split[[2]])
    old_mid <- old_mid[,c(-ncol(old_mid))]
    old_mid <- old_mid[,-c(1)]
    should_predict <- need_split[[3]][,c(-ncol(need_split[[3]]))]
    should_pre_name <- should_predict[,1]
    should_predict <- should_predict[,-c(1)]
  }
  return (list(old_mid, should_predict))
}
back_should_predict <- split_predict_year(split_back)[[2]]
origin_should_predict <- split_predict_year(split_origin)[[2]]
start <- length(class_name) - nrow(back_should_predict) + 1
class_name <- as.character(class_name[start:length(class_name)])

# using these two matrix to predict
predict <- function(back_should_predict, n){
  #n <- 10
  col_id <- sample(colnames(back_should_predict), n)
  position <- c()
  for( w in 1:length(col_id) ){
    each_pos <- which(colnames(back_should_predict) == col_id[w])
    position <- append(position, each_pos)
  }
  storage_back <- back_should_predict[,position]
  storage_origin <- origin_should_predict[,position]  
  take_id <- colnames(storage_back)
  
  storage_total <- list()
  for( i in 1:ncol(storage_back)){
    tmp_bind <- cbind(storage_back[,i], storage_origin[,i]) 
    rownames(tmp_bind) <- class_name
    tmp_bind <- tmp_bind[order(-tmp_bind[,1]),]
    colnames(tmp_bind) <- c(paste0(take_id[i],"_svd"),paste0(take_id[i],"_origin"))
    #test <- tmp_bind[tmp_bind[,2] != -1,]
    storage_total[[i]] <- tmp_bind
  }
  return (storage_total)
}

# predict result
result <- predict(back_should_predict, 10)

# precision

# recall


options("warn"=old.opt[[1]])
