## set library -------------------------------------------
library(sp)
library(tidyverse)
library(jsonlite)
setwd("C:/Behavior_Analysis/Bonsai/Tracking")

## make function -----------------------------------------
myfunction_jugde <- function(x, y, Ax, Ay){
    result <- point.in.polygon(x, y, Ax, Ay)
    if (result == 0) {
        return(0)
    }else{
        return(1)
    }
}

## Edit parameters ---------------------------------------

## input file ----------------------------
in_f_name <- "position2018-07-13T00_59_04.csv"

## set polgon A to C ---------------------
pol.x_A <- c(217, 298, 390, 889, 897, 875, 782)
pol.y_A <- c(140, 0,   0,   264, 359, 408, 452)

pol.x_B <- c(889, 1329, 1498, 1569, 997, 926, 897)
pol.y_B <- c(264, 0,    0,    170,  440, 408, 359)

pol.x_C <- c(875, 926, 997, 1013, 806,  782)
pol.y_C <- c(408, 408, 440, 1080, 1080, 452)
# pol.x_C <- c(875, 926, 1013, 806)
# pol.y_C <- c(408, 408, 1080, 1079)

## skip frames --------------------------
skip_start <- 864
skip_end <- 5564

## get rawdata --------------------------------------------
rawdata <- read_csv(in_f_name,
                    col_names = FALSE) 
colnames(rawdata) <- c("x","y")
rawdata$frame <- rownames(rawdata)

rawdata <- rawdata[skip_start:skip_end,]

head(rawdata)
dim(rawdata)

## jugde Arm A ----------------
pol.x <- pol.x_A
pol.y <- pol.y_A

in_list <- c()
for(i in 1:dim(rawdata)[1]){
    zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
    in_list <- c(in_list, zone)
}

result_list_A <- in_list # 結果の格納
result_list_A
## jugde Arm B ---------------
pol.x <- pol.x_B
pol.y <- pol.y_B

in_list <- c()
for(i in 1:dim(rawdata)[1]){
    zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
    in_list <- c(in_list, zone)
}

result_list_B <- in_list　# 結果の格納
result_list_B
## jugde Arm C ----------------
pol.x <- pol.x_C
pol.y <- pol.y_C

in_list <- c()
for(i in 1:dim(rawdata)[1]){
    zone <- myfunction_jugde(rawdata$x[i],rawdata$y[i], pol.x, pol.y)
    in_list <- c(in_list, zone)
}

result_list_C <- in_list　# 結果の格納
unique(result_list_C)
rawdata %>% dplyr::filter(y > 408)

## mutate ---------------------
rawdata %>% 
    mutate(Arm_A = result_list_A) %>% 
    mutate(Arm_B = result_list_B) %>% 
    mutate(Arm_C = result_list_C) %>% 
    dplyr::select(frame, x, y, Arm_A, Arm_B, Arm_C)-> rawdata 

head(rawdata)


## ゾーン識別カラム -----------------------------------
myfunction_zone <- function(a,b,c){
    if (a == 1) {
        return("A")
    }else if(b == 1){
        return("B")
    }else if(c == 1){
        return("C")
    }else {
        return("S")
    }
}

in_list<- c()
for(i in 1:dim(rawdata)[1]){
    zone <- myfunction_zone(rawdata$Arm_A[i], rawdata$Arm_B[i], rawdata$Arm_C[i])
    in_list <- c(in_list, zone)
}
rawdata$zone <- in_list

in_f <- c(rawdata$zone[1])
for(i in 2:length(rawdata$zone)){
    if (rawdata$zone[i] != rawdata$zone[i-1]) {
        in_f <- c(in_f, rawdata$zone[i])
    }
}

Alternation_S <- in_f
Alternation <- in_f[-which(in_f == "S")]

## エンター回数の算出 ----------------------------------

## 関数作成 -----------------------------
myfunction_enter <- function(pre, post) {
    if( post - pre == 1){
        return(1)
    } else if ( post - pre == 0) { 
        return(0)
    } else if (post - pre == -1) {
        return(-1)
    } else {
        return(NA)
    }
}

## Arm A ---------------------------------
col_Arm <- 4 

in_list <- c()
for(i in 2:(dim(rawdata)[1])) {
    enter <- myfunction_enter(rawdata[i-1,col_Arm], rawdata[i,col_Arm])
    in_list <- c(in_list, enter)
}
in_list <- c(as.numeric(rawdata[1,col_Arm]), in_list)

rawdata$Enter_A <- in_list

## Arm B --------------------------------
col_Arm <- 5 

in_list <- c()
for(i in 2:(dim(rawdata)[1])) {
    enter <- myfunction_enter(rawdata[i-1,col_Arm],rawdata[i,col_Arm])
    in_list <- c(in_list, enter)
}
in_list <- c(as.numeric(rawdata[1,col_Arm]), in_list)

rawdata$Enter_B <- in_list

## Arm C  --------------------------------
col_Arm <- 6 

in_list <- c()
for(i in 2:(dim(rawdata)[1])) {
    enter <- myfunction_enter(rawdata[i-1,col_Arm],rawdata[i,col_Arm])
    in_list <- c(in_list, enter)
}
in_list <- c(as.numeric(rawdata[1,col_Arm]), in_list)

rawdata$Enter_C <- in_list


## complete -------------------------------
rawdata$frame <- as.numeric(rawdata$frame)

head(rawdata)
dim(rawdata)

write.table(rawdata, "rawdata.txt", sep = "\t")

## Calc Arm stay time-----------------------------------------------------

rawdata %>% 
    dplyr::filter(Arm_A == 1) %>%
    dim() -> tmp
Enter_A_time <- tmp[1] * (1 / 29.97)
Enter_A_time

rawdata %>% 
    dplyr::filter(Arm_B == 1) %>%
    dim() -> tmp
Enter_B_time <- tmp[1] * (1 / 29.97)
Enter_B_time

rawdata %>% 
    dplyr::filter(Arm_C == 1) %>%
    dim() -> tmp
Enter_C_time <- tmp[1] * (1 / 29.97)
Enter_C_time

## Calc Arm enter number ---------------------------------------------------

rawdata %>% 
    dplyr::filter(Enter_A == 1) %>%
    dim() -> tmp
Enter_A_number <- tmp[1] 
Enter_A_number

rawdata %>% 
    dplyr::filter(Enter_B == 1) %>%
    dim() -> tmp
Enter_B_number <- tmp[1] 
Enter_B_number

rawdata %>% 
    dplyr::filter(Enter_C == 1) %>%
    dim() -> tmp
Enter_C_number <- tmp[1] 
Enter_C_number

## make result.json
x <- list(in_f_name,
       Enter_A_time, 
       Enter_B_time, 
       Enter_C_time,
       Enter_A_number,
       Enter_B_number,
       Enter_C_number,
       Alternation_S,
       Alternation)
x
y <- c("file_name",
       "Enter_A_time",
       "Enter_B_time",
       "Enter_C_time",
       "Enter_A_number",
       "Enter_B_number",
       "Enter_C_number",
       "Alternation_with_senter",
       "Alternation" )

result <- rbind(y,x)
result
colnames(result) <- result[1,]
result <- result[-1, ]
tmp <- toJSON(result, pretty = TRUE, auto_unbox = TRUE)
write(tmp, file = "result.json")
