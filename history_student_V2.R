rm(list = ls())
library(dplyr)
library(RMySQL)
library(DBI)

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
# save.image("/home/lab/下載/split_data.Rdata")
##################################################################
rm(list = ls())
load("/home/lab/下載/split_data.Rdata") 

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

# bind with the big_matrix, and big_matrix will final binf in final_bind
matrix_bind <- function(id, tmp_frame, big_matrix){
  #id, st_need_year, big_matrix
  #tmp_frame <- st_need_year
  for( i in 1:length(tmp_frame) ){
    score <- tmp_frame[[i]] 
    name <- tmp_frame[[i]][,2]
      
    unexist <- setdiff(name, rownames(big_matrix[[i]]))
    tmp_matrix <- matrix(NA,nrow = length(unexist), ncol = ncol(big_matrix[[i]]))
    colnames(tmp_matrix) <- colnames(big_matrix[[i]])
    rownames(tmp_matrix) <- unexist
    big_matrix[[i]] <- rbind(big_matrix[[i]],tmp_matrix)
      
    new <- matrix(NA,nrow = nrow(big_matrix[[i]]), ncol = 1)
    colnames(new) <- id
      
    if(is.null(tmp_frame[[i]]) || nrow(tmp_frame[[i]])==0){
      big_matrix[[i]] <- cbind(big_matrix[[i]], new)
    }else{
      for( t in 1:nrow(big_matrix[[i]])){
        for( n in 1:nrow(score) ){
          if(score[n,2] == rownames(big_matrix[[i]])[t]){
            #print(t)
            if(score[n,3]=="抵免"){
              new[t,1] <- 60
            }else{
              new[t,1] <- as.integer(score[n,3])
            }
          }
        }
      }
      big_matrix[[i]] <- cbind(big_matrix[[i]], new)
    }
  }
  return (big_matrix)
}

# check student is we need 
check_st_need <- function(st_year, check){
  year <- as.integer(st_year)
  semester_check <- check
  
  compare <- c("")
  
  for( j in 1:length(semester_check) ){
    
    if(j%%2==1){
      compare <- append(compare, paste0(as.character(year),"1"))
    }else{
      compare <- append(compare, paste0(as.character(year),"2"))
      year <- year + 1
    }
    
  }
  compare <- compare[! compare %in% ""]
  compare <- rev(compare)
  ans <- ""
  for( i in length(semester_check):1 ){
    
    if(semester_check[i] != compare[i]){
      ans <- "ng"
      break
    }else{
      ans <- "OK"
    }
  }
  return (ans)
}


# main function
main_fun <- function(split_data, start, end, grade, dep){
  #1->2  end<=107
  #start <- 104; end <- 105 ; grade <- 2; dep <- 0
  if(dep == 0){
    i_need <- as.character(all_user$id)
    get_dep <- lapply(split_data, function(x){if(unique(x$StudentID) %in% i_need)return(x) else return(NULL) })
    get_dep[sapply(get_dep, is.null)] <- NULL
  }else{
    i_need <- as.character(all_user[all_user$dep == dep,]$id)
    get_dep <- lapply(split_data, function(x){if(unique(x$StudentID) %in% i_need)return(x) else return(NULL) })
    get_dep[sapply(get_dep, is.null)] <- NULL
  }
  get_bfor <- lapply(get_dep, function(x){ if(substr(unique(x$StudentID),1,3)<=end){return(x)}else return(NULL) })
  get_bfor[sapply(get_bfor, is.null)] <- NULL

  all_course <- matrix(NA,nrow = 0,ncol = 1)
  colnames(all_course) <- "semester_year"

  big_matrix <- list()
  last <- 2*grade+1
  for(se in 1:last){
    big_matrix[[se]] <- all_course
  }
  
  # compare each student
  for( i in 1:length(get_bfor) ){
    # i = 338
    #i = i + 1
    st_year <- unique(substr(get_bfor[[i]]$StudentID,1,3))
    check <- unique(get_bfor[[i]]$Semester)
    ans <- check_st_need(st_year, check)

    if(ans == "OK"){

      year <- as.integer(st_year)
      need_year <- c()
      compare <- c()

      if(grade == 1){
        compare[1:2] <- year
        compare[3] <- year + 1
      }else{
        compare[1:2] <- year
        year <- year + 1
        compare[3:4] <- year
        compare[5] <- year + 1
      }

      for( j in 1:length(compare) ){

        if(j%%2==1){
          need_year[j] <- paste0(compare[j],"1")
        }else{
          need_year[j] <- paste0(compare[j],"2")
          year <- year + 1
        }
      }

      if(FALSE%in%need_year%in%check){
      }else{
        split_each <- split(get_bfor[[i]],f = get_bfor[[i]]$Semester)
        get_need_year <- lapply(split_each, function(x){if(as.integer(need_year[1])<=unique(x$Semester) && unique(x$Semester)<=as.integer(need_year[length(need_year)])){return (x)}})
        get_need_year[sapply(get_need_year, is.null)] <- NULL
        get_need_year <- lapply(get_need_year, function(x){x[x$must_elect=="選修",]})
        
        st_need_year <- list()
        for( j in 1:length(get_need_year) ){
          for(s in 1:nrow(get_need_year[[j]])){
            if(nrow(get_need_year[[j]]) == 0){
              next
            }
            if(grepl("【英】",get_need_year[[j]]$course_name[s])){
              get_need_year[[j]]$course_name[s] <- gsub("【英】","",get_need_year[[j]]$course_name[s])
            }
            if(grepl("【跨】",get_need_year[[j]]$course_name[s])){
              get_need_year[[j]]$course_name[s] <- gsub("【跨】","",get_need_year[[j]]$course_name[s])
            }
            if(grepl("【PBL】",get_need_year[[j]]$course_name[s])){
              get_need_year[[j]]$course_name[s] <- gsub("【PBL】","",get_need_year[[j]]$course_name[s])
            } 
          }
          get_need_year[[j]] <- get_need_year[[j]][,c(2,3,7)]
          st_need_year[[j]] <- get_need_year[[j]]
        }
        if(!""%in%st_need_year[[length(get_need_year)]]$score){
          id <- unique(get_bfor[[i]]$StudentID)
          
          big_matrix <- matrix_bind(id, st_need_year, big_matrix)
          
          for(asint in 1:last){
            big_matrix[[asint]][,1] <- as.integer(need_year[asint])
          }
        }
      }
    }else{  # ans == ok end
      next
    }
  } # total student end
  if(grade == 2){
    final_bind <- rbind(big_matrix[[1]],big_matrix[[2]],big_matrix[[3]],big_matrix[[4]],big_matrix[[5]])
  }else{
    final_bind <- rbind(big_matrix[[1]],big_matrix[[2]],big_matrix[[3]])
  }
  

  final_bind[is.na(final_bind)] <- -1

  return (final_bind)
}
# main end
#save.image(file = "下載/all_function.RData")
############################################################################
#rm(list = ls())
#load("下載/all_function.RData")

# grade matrix
get_class_history <- main_fun(split_data, 104, 105, 2, 0)
#write.csv(get_class_history,"下載/get_class_history.csv")
#original_data <- as.data.frame(get_class_history)

class_year <- as.data.frame(get_class_history[,1]) # storage year
colnames(class_year) <- "year"
get_class_history <- as.data.frame(get_class_history)
all_data <- split(get_class_history, f = get_class_history$semester_year) # split the singal student

get_class_history <- get_class_history[,-c(1)]  # delete the corse year
get_col <- colnames(get_class_history)
get_row <- rownames(get_class_history)

# normalize
get_normalize <- (apply(get_class_history, 2, function(x)(x-min(x))/(max(x)-min(x))))
#normalize_final <- t(apply(normalize_col, 1, function(x)(x-min(x))/(max(x)-min(x))))
write.csv(get_normalize, "下載/get_normalize.csv")

get_svd_dataframe <- function(data){
  #data <- get_normalize
  run_svd <- svd(data)  # SVD function
  # return_svd <- run_svd$u %*% diag(run_svd$d) %*% t(run_svd$v)
  # return_svd <- apply(return_svd,2,as.integer)  ##########problem
  # summary(run_svd$d)
  # hist(run_svd$d)
  # diag(run_svd$d)
  # sum(run_svd$d)
  # cumsum(run_svd$d)
  check <- cumsum(run_svd$d)/sum(run_svd$d) < 0.85
  run_svd$d[check]
  f <- length(run_svd$d[check])
  return_back <- run_svd$u[,1:f] %*% diag(run_svd$d[1:f]) %*% t(run_svd$v[,1:f])
  return_back <- as.data.frame(return_back)
  
  colnames(return_back) <- get_col
  return_back[,"course_name"] <- get_row
  stop <- ncol(return_back)-1
  return_back <- return_back[,c(ncol(return_back),1:stop)]
  
  return (return_back)
}
back <- get_svd_dataframe(get_normalize)
back <- cbind(back, class_year)
#select <- get_svd_dataframe(select_unselect)
#select <- cbind(select, class_year)
#####################################################################
# delete the mean
# for( t in 2:ncol(back_27) ){
#   back_27[,t] <- back_27[,t] - mean(back_27[,t])
# }


# select_27_new <- back_27[,c(-1)]
# select_27_colname <- as.data.frame(select_27[,1])
# colnames(select_27_colname) <- "course_name"
# change_select <- cbind(select_27_colname,change_select)

# start to predict
# split_back <- split(back, f = back$year )
# old_mid <- rbind(split_back[[1]], split_back[[2]])
# old_mid <- old_mid[,c(-ncol(old_mid))]
# should_predict <- split_back[[3]][,c(-ncol(split_back[[3]]))]

write.csv(back,"/home/lab/下載/back.csv")

options("warn"=old.opt[[1]])


