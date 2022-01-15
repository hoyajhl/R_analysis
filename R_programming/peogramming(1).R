## EX1
list_new<-list() ##empty list
for(i in 1:10){
  a<-c(1,2,3)
  list_new[[i]]<-a
}
length(list_new) ## list length ##10

## EX2
address<-c("서울시 강남구 역삼동","서울시 강남구 삼성동",
        "서울시 서초구 서초동","서울시 반포동")
a<-str_split(address," ")  ## save into list using split stringr

## using "for"
length(a)  ##4
address_region<-c() ## vector 동 기준
for(i in 1:length(a)){
  address_region[i]<-a[[i]][3]}
address_region

##  sapply (1) : list의 큰방에 접근해서 함수 적용-> 결과 값은 vector로 반환
sapply(a,length)  ## 3 3 3 2
sapply(a,paste0,collapse=" ")

address<-c("서울시 강남구 역삼동","서울시 강남구 삼성동",
           "서울시 서초구 서초동","서울시 반포동")
search<-function(x){
  x[2]
}
address<-str_split(address," ")
sapply(address,search)

## sapply (2) 
search<-function(x,i){
  x[i]
}
address<-str_split(address," ")
sapply(adress,search,1)
sapply(address,search,2)
sapply(address,search,3)


## sapply(3)

name<- sapply(jsonfile$result$site$list,function(x){x$name})

## lapply는 결과 값을 list로 반환
lapply(address,search,1)

## data frame 형식과 list형식 차이
data<-NULL
for(i in 1:10){
  cat("\n",i) ## for문 작동 유무 check
  a<-c(1,2,3,4)
  data<-rbind(data,a)}
data

list<-list()
for(i in 1:10){
  a<-c(1,2,3)
  list[[i]]<-a}
list

## 시간, date 불러오기
Sys.time()
Sys.Date()

## function 활용 (1)
scale <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

scale(c(1, 2, 3, NA, 5))

## function 활용 (2)
# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}

x <- runif(100)
qnorm(c(0.05 / 2, 1 - 0.05 / 2)) 
#> [1] -1.959964  1.959964
mean_ci(x)
#> [1] 0.4976111 0.6099594
mean_ci(x, conf = 0.99)
#> [1] 0.4799599 0.6276105

## sapply,lapply, function 활용
x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77))
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34))
x1
x2
threshold <- function(x, cutoff = 0.8){
  x[x > cutoff]}

x1 %>% 
  sapply(threshold)

x2 %>%
  lapply(threshold) %>% 
  str()
