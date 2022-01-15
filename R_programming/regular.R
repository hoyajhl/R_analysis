##Regular Expression (정규식)

char<- c('apple','Apple','APPLE','banana','grape')
grep('pp',char) # 위치 값 반환 1 2 

char3 <- c('grape1','apple1','apple','orange','Apple','grape0')
grep('ap',char3,value=T)  #return value 
grep('[0-9]',char3,value=T)  #숫자 포함 단어 value return

##stringr package
library(stringr)


# str_detect() 
# 해당 단어가 포함된 문자열에 대해 T/F 반환 
fruits <- c('apple','Apple','banana','pineapple')
str_detect(fruits,"A")  

fruits <- str_c('apple',' ','orange',' ','banana')
str_split(fruits," ")  # 공백을 기준으로 분리

# Repetition
# ?: 0 or 1.
# +: 1 or more.
# *: 0 or more.
x <- "1888 is the longest year in Roman numerals: MDCCCCCLXXXVIII"
str_extract(x, "CC?")
str_extract(x, "CC+")
str_extract(x, 'C[LX]+')
str_extract(x,'.*') ##모두 적용
str_extract(x,'[0-9]*') ##숫자만 적용
str_extract(x,'[:alpha:].*') ##alphabet

gsub("Roman numerals","",x) # delete

### 정규식 실전용 코드
con<-c()
for(k in 1:length(con_url)){
  a<-readLines(con_url[k],encoding="UTF-8")
  b<-a[which(str_detect(a,"id=\"newEndsContents\">"))]:a[which(str_detect(a,"news_end_btn"))] 
  ## which 라인 위치 값 반환// ex) b<-a[2:6] // 위치 해당 text 출력
  b<-paste(b,collapse= " ") ## 하나로 만드는 과정
  b<-gsub("<.*?>","",b) ## html tag 없애는 정규함수 
  
  con[k]<-b ## 저장
  cat("\n",k) ##for문 processing 확인
}
new_data<-cbind(previous_data,con)
