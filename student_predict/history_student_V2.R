# 2019/11/08
# create slecet class matrix
# give svd function, can output svd matrix

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
# # each dep
# con <- dbConnect(MySQL(), dbname = "r_use", username = "root", password = "86070586")
# dbSendQuery(con, "SET NAMES 'utf8'") #big5
# build_dep <- matrix(,nrow = 27, ncol = 3)
# colnames(build_dep) <- c("num","dep_name","college")
# num <- c(11,12,13,14,15,21,22,23,25,29,24,26,27,28,31,32,33,35,41,42,43,44,47,45,51,52,57)
# dep_name <- c("應數","物理","化學","心理","生科","化工","土木","機械","醫工","環工","工業","電子","資訊","電機","建築","室設","商設","景觀","企管","國貿","會計","資管","財金","財法","特教","應外","應華")
# college <- c("理學院","理學院","理學院","理學院","理學院","工學院","工學院","工學院","工學院","工學院","電資學院","電資學院","電資學院","電資學院","設計學院","設計學院","設計學院","設計學院","商學院","商學院","商學院","商學院","商學院","法學院","人育學院","人育學院","人育學院")
# for(i in 1:length(num)){
#   build_dep[i,1] <- num[i]
#   build_dep[i,2] <- dep_name[i]
#   build_dep[i,3] <- college[i]
# }
# build_dep <- as.data.frame(build_dep)
# #dbWriteTable(con,"name_dep",build_dep,overwrite=TRUE)
# 
# # connect to db
# con <- dbConnect(MySQL(), dbname = "r_use", username = "root", password = "86070586")
# dbSendQuery(con, "SET NAMES 'utf8'") #big5
# 
# old_all_stdata <- dbGetQuery(con, "select * from finished_class_old")
# new_all_stdata <- dbGetQuery(con, "select * from finished_class")
# dbDisconnect(con)
# all_data <- rbind(old_all_stdata, new_all_stdata)
# all_data <- as.data.frame(all_data)
# split_data <- split(all_data, f = all_data$StudentID)
# save.image("/home/lab/下載/tmp/split_data.Rdata")
##################################################################
rm(list = ls())
#setwd("/home/lab/下載/student_predict")
load("./split_data.Rdata")
load("./all_function.Rdata")

# check all student dep
all_user <- as.data.frame(unique(all_data[,1]))
colnames(all_user) <- "id"
all_user["year"] <- NA
all_user["dep"] <- NA
all_user["dep_name"] <- NA
for( i in 1:nrow(all_user) ){
  all_user[,2][i] <- substr(all_user[,1][i],1,3)
  for( j in 1:nrow(build_dep) ){
    if(as.integer(substr(all_user[,1][i],4,5)) == build_dep[,1][j]){
      all_user[,3][i] <- as.character(build_dep[,1][j])
      all_user[,4][i] <- as.character(build_dep[,2][j])
    }  
  }
}
all_user <- all_user[complete.cases(all_user[,2:3]),]
##################################################################  
# remove not need student
all_stid <- as.character(unique(all_user[,1]))
not_need <- which(!names(split_data)%in% all_stid)
if(length(not_need) == 0){
}else{
  for(i in length(not_need):1){
    split_data[[not_need[i]]] <- NULL 
  }
}
############################################################################
# grade matrix
get_class_history <- main_fun(split_data, 105, 2, 0)
# write.csv(get_class_history,"./get_class_history.csv")
get_class_history <- get_class_history[,c(2:225,1)]
test <- get_class_history
tmp <- as.data.frame(test[,ncol(test)])
colnames(tmp) <- "yesr"
test <- test[,-c(ncol(test))]
start <- ncol(test) - 10 + 1

predict_st_10 <- test[,start:ncol(test)]
predict_st_10 <- cbind(predict_st_10, as.matrix(tmp))
file_name_10 <- paste0("ten_student_",1)
path_ten <- paste0("./ten_student/",file_name_10,".csv")
write.csv(predict_st_10, path_ten)
test <- test[,-c(start:ncol(test))]
test <- cbind(test, as.matrix(tmp))
origin_name <- paste0("neither_ten_student_",1)
path_origin <- paste0("./neither_ten_origin_class/",origin_name,".csv")
write.csv(test, path_origin)


options("warn"=old.opt[[1]])


