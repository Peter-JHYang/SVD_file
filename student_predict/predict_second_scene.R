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

#setwd("/home/lab/下載/tmp")
# read file
back <- read.csv("./back.csv",header = T)
get_origin <- read.csv("./get_class_history.csv",header=T)

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
change_origin <- get_origin

class_catch <- as.data.frame(change_origin[,1])
colnames(class_catch) <- "course_name"
year_catch <- as.data.frame(change_origin[,ncol(change_origin)])
colnames(year_catch) <- "year"
change_origin <- change_origin[,-c(1,226)]
change_origin[change_origin!=-1] <- 1
change_origin[change_origin==-1] <- 0
change_origin <- cbind(class_catch, change_origin, year_catch)

# split using the last column, the year
split_back <- split(back, f = back$year )
split_origin <- split(get_origin, f = get_origin$year)
split_origin_change <- split(change_origin, f = change_origin$year)


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
back_should_predict <- split_predict_year(split_back)[[2]] # back can predict
origin_should_sim <- split_predict_year(split_origin_change)[[1]] # should calculate similarity value
origin_should_predict <- split_predict_year(split_origin)[[2]] # origin can predict
start <- length(class_name) - nrow(back_should_predict) + 1
class_name <- as.character(class_name[start:length(class_name)]) # get predict class name first 
rownames(origin_should_predict) <- class_name

get_predict_id <- function(back_should_predict, origin_should_sim, n){
  #n <- 10
  col_id <- sample(colnames(back_should_predict), n)
  position <- c()
  for( w in 1:length(col_id) ){
    each_pos <- which(colnames(back_should_predict) == col_id[w])
    position <- append(position, each_pos)
  }
  storage_back <- back_should_predict[,position]
  storage_origin <- origin_should_sim[,position] # storage these 10 students
  
  origin_should_sim <- origin_should_sim[,-position] # remove these 10 students
  take_id <- colnames(origin_should_sim)
  
  student_jac_list <- list()
  for( i in 1:ncol(storage_origin) ){
    jaccard_i_need <- matrix(NA, nrow = nrow(storage_origin), ncol = 2)
    for( j in 1:ncol(origin_should_sim) ){
      value <- jaccard(storage_origin[,i], origin_should_sim[,j])
      if( value > 0.5 ){
        jaccard_i_need[j,1] <- take_id[j]
        jaccard_i_need[j,2] <- value
      }
      
    }
    jaccard_i_need <- jaccard_i_need[order(-as.numeric(jaccard_i_need[,2])),]
    jaccard_i_need <- jaccard_i_need[c(1:10),]
    
    student_jac_list[[i]] <- jaccard_i_need
  }
  return (list(col_id, student_jac_list))
}
get_total <- get_predict_id(back_should_predict, origin_should_sim, 10)
predict_id <- get_total[[1]]
sim_st_id <- get_total[[2]]

# calculate all 10 students and each other similar student by jaccard, 
# and use origin matrix(have class vlaue) to recommend
calculate_value <- function(predict_id, sim_st_id){
  position <- which(colnames(origin_should_predict)%in%predict_id)
  storage_tmp <- origin_should_predict[,position]
  up_count <- 0
  down_count <- 0
  total_View <- list()
  for( k in 1:ncol(storage_tmp) ){  # total 10 student
    
    place <- which(predict_id == colnames(storage_tmp)[k]) # student which from 10 st catch out their jaccard similar st
    have_value <- 0
    just_view <- list()
    
    for( p in 1:nrow(sim_st_id[[place]]) ){
      if(!is.na(sim_st_id[[place]][p])){
        have_value <- have_value + 1
        catch_out <- origin_should_predict[which(colnames(origin_should_predict) == sim_st_id[[place]][p])] # get need to calculate st from origin dataframe
        tmp_cbind <- cbind(storage_tmp[k], catch_out)  # cbind one st from 10, and another from jaccard st
        tmp_cbind <- tmp_cbind[order(-tmp_cbind[,2]),] # sort the class
        tmp_cbind <- tmp_cbind[c(which(tmp_cbind[,2]!=-1)),]
        tmp_cbind <- tmp_cbind[c(1:10),] # only get Top-10 class
        up_count <- up_count + table(tmp_cbind[,1]!=-1 & !is.na(tmp_cbind[,1]))["TRUE"]/10
        just_view[[p]] <- tmp_cbind
      }else{
        just_view[[p]] <- NA
      }
    }
    down_count <- down_count + have_value
    total_View[[k]] <- just_view
  } 
  final_value <- up_count/down_count
  return (list(final_value, total_View))
}
predict_value <- as.numeric(calculate_value(predict_id, sim_st_id)[[1]])
predict_value
# 0.4695652
just_view <- calculate_value(predict_id, sim_st_id)[[2]]
View(just_view[[1]]) # 10345114

options("warn"=old.opt[[1]])
# [[1]]
# 10345114 10345230
# 海商法                94       91
# 爵士舞(男.女)一       85       86
# 銀行法                -1       84
# 財政學                -1       81
# 生命倫理學            -1       80
# 民法債篇各論          73       77
# 國際法導論            -1       77
# 刑事訴訟法            87       72
# 行政法                -1       69
# 票據法                -1       60
# 
# [[2]]
# 10345114 10345239
# 銀行法               -1       94
# 海商法               94       91
# 創業計畫與表達       -1       83
# 國際法導論           -1       81
# 羽球(男.女)二        -1       81
# 財政學               -1       80
# 生命科學概論         -1       70
# 法院組織法           90       60
# 刑事訴訟法           87       55
# 民法債篇各論         73       49
# 
# [[3]]
# 10345114 10445113
# 保險法                   95       95
# 環境與幸福生活           -1       94
# 日文(一)                 -1       94
# 商事法總論及公司法       83       91
# 財政學                   -1       91
# 法院組織法               90       90
# 中西哲學家的生命觀       -1       89
# 行政程序法               84       83
# 排球(女)一               -1       82
# 民法債編各論             -1       74
# 
# [[4]]
# 10345114 10445243
# 海商法               94       94
# 關稅法               -1       90
# 排球(女)一           -1       86
# 財政學               -1       77
# 法院組織法           90       65
# 英美侵權行為法       -1       64
# 民法債編各論         -1       60
# 法學英文（一）       -1       60
# 票據法               -1       45
# 刑事訴訟法           87       30
# 
# [[5]]
# 10345114 10545111
# 哲學與生活               -1       97
# 海商法                   94       97
# 保險法                   95       96
# 氣排球                   -1       89
# 行政程序法               84       87
# 法院組織法               90       86
# 會計學(一)               -1       86
# 英美侵權行為法           -1       85
# 刑事訴訟法               87       81
# 商事法總論及公司法       83       81
# 
# [[6]]
# 10345114 10545220
# 生命與品格典範       -1       97
# 海商法               94       96
# 法律服務(一)         -1       90
# 壘球(女)一           -1       79
# 刑事訴訟法           87       78
# 法院組織法           90       78
# 財政學               -1       72
# 英美侵權行為法       -1       64
# 行政程序法           84       61
# 票據法               -1       60
# 
# [[7]]
# 10345114 10545129
# 保險法                   95       94
# 法院組織法               90       80
# 性別平等與現代社會       -1       73
# 聖經中的經濟智慧         -1       72
# 商事法總論及公司法       83       61
# 刑事訴訟法               87       60
# 刑法總則                 -1       60
# 行政法                   -1       46
# 民法債編各論             -1       45
# NA                       NA       NA
# 
# [[8]]
# 10345114 10545211
# 保險法              95       96
# 海商法              94       96
# 排球二(男.女)       -1       88
# 生活化學應用        -1       87
# 宗教哲學            -1       76
# 財政學              -1       73
# 憲法                -1       70
# 刑事訴訟法          87       68
# 民法債編各論        -1       51
# 票據法              -1       48
# 
# [[9]]
# 10345114 10545235
# 海商法               94       97
# 職涯列車講座         -1       90
# 法律服務(一)         -1       89
# 法院組織法           90       88
# 愛情關係管理         -1       87
# 桌球(男.女)一        -1       81
# 財政學               -1       76
# 英美侵權行為法       -1       71
# 民法債編各論         -1       70
# 刑事訴訟法           87       60
# 
# [[10]]
# 10345114 10545251
# 少年事件處理法           -1       90
# 法院組織法               90       86
# 哲學與人生               -1       84
# 網球一                   -1       76
# 生命與品格典範           -1       75
# 財政學                   -1       73
# 英美侵權行為法           -1       71
# 商事法總論及公司法       83       67
# 行政法                   -1       64
# 刑事訴訟法               87       61


