# put all function in this R file
rm(list = ls())

### bind with the big_matrix, and big_matrix will final binf in final_bind
# input : student id(string), student score(list), big_matrix(list)
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
# output : big_matrix -> bind in this matrix

### check student is we need 
# input : student class year(string), we need year(vector)
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
# output : OK or ng

### main function
# input : split each year(list), should get which year(int), which grade(int), which dep(int)
main_fun <- function(split_data, end, grade, dep){
  #1->2  end<=107
  #end <- 105 ; grade <- 2; dep <- 0
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
        #get_need_year <- lapply(get_need_year, function(x){x[x$must_elect=="選修",]})
        
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
# output : final matrix

### modify read dataframe
# input : final matrix from reading(list)
modify_col <- function(data){
  row_name <- data[,1]
  data <- data[,-c(1)]
  # change dataframe column name
  data_colname <- colnames(data)
  for( i in 1:length(data_colname) ){
    if(substr(data_colname[i],1,1) == "X"){
      data_colname[i] <- gsub("X","",data_colname[i])
    }
  }
  colnames(data) <- data_colname
  data <- as.matrix(data)
  rownames(data) <- row_name
  data <- as.data.frame(data)
  return (data)
}
# output : final_matrix

### normalize
# intput : final matrix(list)
get_normalize <- function(get_class_history){
  normalize <- (apply(get_class_history, 2, function(x)(x-min(x)+0.1)/(max(x)-min(x))))
  return (normalize)
}
# output : final_matrix

### SVD function
# input : normalize matrix(list), threshold
get_svd_dataframe <- function(data, threshold){
  #data <- normalize_result
  #threshold <- 0.85
  run_svd <- svd(data)  # SVD function
  check <- cumsum(run_svd$d)/sum(run_svd$d) < threshold
  run_svd$d[check]
  f <- length(run_svd$d[check])
  return_back <- run_svd$u[,1:f] %*% diag(run_svd$d[1:f]) %*% t(run_svd$v[,1:f])
  return_back <- as.data.frame(return_back)
  return (return_back)
}
# output : svd matrix

### split year 
# input : finish split by year matrix list(list)
split_predict_year <- function(need_split){
  #need_split <- split_ten
  if(length(need_split) > 3){
    old_mid <- rbind(need_split[[1]], need_split[[2]], need_split[[3]], need_split[[4]])
    old_mid_year <- old_mid[,c(ncol(old_mid))]
    should_predict_year <- need_split[[5]][,c(ncol(need_split[[length(need_split)]]))]
    old_mid <- old_mid[,-c(ncol(old_mid))]
    should_predict <- need_split[[5]][,-c(ncol(need_split[[length(need_split)]]))]
  }else{
    old_mid <- rbind(need_split[[1]], need_split[[2]])
    old_mid_year <- old_mid[,c(ncol(old_mid))]
    should_predict_year <- need_split[[3]][,c(ncol(need_split[[length(need_split)]]))]
    old_mid <- old_mid[,c(-ncol(old_mid))]
    should_predict <- need_split[[3]][,c(-ncol(need_split[[3]]))]
  }
  return (list(old_mid, should_predict, old_mid_year, should_predict_year))
}
# output : five or three matrix

### change xlsx matrix column name
# input : xlsx matrix nee dto change column name
modify_xlsx_col <- function(m.file){
  name <- colnames(m.file)
  need_name <- c()
  for( i in 1:length(name) ){
    total <- strsplit(name[i],split = ".",fixed = TRUE)
    need_name <- append(need_name, total[[1]][1])
  }  
  colnames(m.file) <- need_name
  return (m.file)
}
# output : xlsx matrix


save.image("/home/lab/下載/student_perdict/all_function.Rdata")

