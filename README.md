# SVD_file
建立對應學生修課矩陣，並加入SVD中進行轉換
# 2019/11/02
1. history_student_V2.R為建立矩陣並加入SVD的程式碼
2. back.csv是轉換後矩陣，name_matrix.csv是對應的課程名稱
3. split_data.Rdata是從資料庫中抓取資料進行分析，all_function.Rdata是所有可用的函數
# 2019/11/11
將所有檔案重新上傳進一個資料夾中，還無法以windows rstudio進行測試，因為中文課程問題
※接下來會修改為可於windows測試，或是取得課程IPcode
※執行檔按順序 : history_student_V2.R -> predict_first_scene.R -> predict_second_scene.R
1. history_student_V2.R可產生back.csv及get_class_history.csv檔案給予方法一和方法二進行預測
2. get_class_history.csv為學生修課成績矩陣，裡面包含必修及選修課 
   back.csv為取得學生修課成績矩陣後放入svd function轉換出的矩陣
3. 方法一 : 利用svd轉換後的矩陣中數值進行推薦，門檻值設定為85%
   方法二 : 將學生修課矩陣中修過的課與未修過的課轉為1與0，取出10位欲預測的學生，並進行jaccard運算，找出與每位(取出噁的10位中)相似度高的前10名學生，進行課程推薦。
   

