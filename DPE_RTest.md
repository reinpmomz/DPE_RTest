---
title: "DPE_RTest"
author: "Reinp"
date: "2021-04-22"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
  word_document: default
---

# R Programming

## Set Chunk requirements


```r
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
```


## 5. Import data into r


```r
library(tidyverse)
library(lubridate)
## tidyverse includes readr, ggplot2, dplyr, forcats, tibble, tidyr, purrr, stringr
library(stats)
library(readxl)



## Reading our dataset
setwd('F:/Documents/Reinp/GitHub Respositories/DPE_RTest')

R_test <- read_csv("R_test_dataset.csv")
View(R_test)

R_test <- R_test%>%
  mutate(Q_2 = dmy(Q_2))
```


### Structure of the data




### Missing data


```r
#complete.cases(R_test) ##print logical vector indicating complete rows
                       ##(i.e. rows without NA)

#which(!complete.cases(R_test)) #print incomplete cases (rows with NA)

#R_test[complete.cases(R_test), ] ## Keep only the complete rows

#data_complete <- R_test[complete.cases(R_test), ] ## Store the complete cases subset in a new
                                                   ##data frame

#which(is.na(R_test)) #check for missing values

sum(is.na(R_test))
```

```
## [1] 647
```

```r
sapply(R_test,function(x) sum(is.na(x)))
```

```
##       SbjNum   Start_Time          Q_2  Interviewer         Ward           S2 
##            0            0            0            0            0            0 
##           S3           S4         S5_a Social_Class         A1_A         A1_B 
##            0            0            0            0            0            0 
##           A2           A5       Q_25_S           A8           A9          A10 
##            0            0          316            0            0            0 
##          A12       Q_32_S           B3           B4           B8          B16 
##           18          313            0            0            0            0 
##          B17           C1 
##            0            0
```


## 6. Duplicated Cases


```r
#duplicated(R_test$SbjNum)
sum(duplicated(R_test$SbjNum))
```

```
## [1] 4
```

```r
table(duplicated(R_test$SbjNum))
```

```
## 
## FALSE  TRUE 
##   317     4
```

```r
#mean(duplicated(R_test$SbjNum)) #proportion of repeat cases

#sum(duplicated(R_test$SbjNum))/nrow(R_test)
```


## 7. total number of unique interviews 



```r
#unique(R_test$SbjNum)
#count(unique(R_test$SbjNum))
#table(unique(R_test$SbjNum))

## data with unique SbjNum

#R_testu <- R_test[!duplicated(R_test$SbjNum),]


R_testu <- R_test%>%
  filter(SbjNum == unique(SbjNum))


nrow(R_testu)
```

```
## [1] 317
```


## 8. Assigning value labels to variables

```r
R_testu <- R_testu %>%
  mutate(Interviewer = if_else(Interviewer == 1, "Menya Abdmajid",
                        if_else(Interviewer == 2, "Bbale Denis",
                        if_else(Interviewer == 3, "Muwonge Allan Joshua",
                        if_else(Interviewer == 4, "Wambi Ken Paul",        
                        if_else(Interviewer == 5, "Wabwire Thomas", 
                        if_else(Interviewer == 6, "Muhindo Wilfred",
                        if_else(Interviewer == 7, "Ahumuza Owen",
                        if_else(Interviewer == 8, "Mirembe Mary",
                        if_else(Interviewer == 9, "Arinitwe Mackline",        
                        if_else(Interviewer == 10, "Alum Maria",
                        if_else(Interviewer == 11, "Kasule violet",        
                        if_else(Interviewer == 12, "Aweko Monica", "Nabbumba Pennina"       
                                )))))))))))))%>%
  mutate(S2 = if_else(S2 == 1, "17 and below",
                        if_else(S2 == 2, "18-25",
                        if_else(S2 == 3, "26-30", "31 and above" 
                                ))))%>%
  mutate(Social_Class = if_else(Social_Class == 1, "Social Class D",
                        if_else(Social_Class == 2, "Social Class C2",
                        if_else(Social_Class == 3, "Social Class C1", 
                         if_else(Social_Class == 4, "Social Class B", "Social Class A" 
                                )))))%>%
  mutate(B4 = if_else(B4 == 1, "Yes", "No"))%>%
  mutate(B17 = if_else(B17 == 1, "Extremely ready",
                        if_else(B17 == 2, "Somewhat ready",
                        if_else(B17 == 3, "Neutral", 
                         if_else(B17 == 4, "Not ready", "Definitely not ready" 
                                )))))%>%
  mutate(C1 = if_else(B17 == 1, "I have been employed (for example in an office, restaurant,",
              if_else(B17 == 2, "I am self-employed (for example, on my own farm or family's",
              if_else(B17 == 3, "I have been employed as an unpaid intern", 
              if_else(B17 == 4, "I have been engaged in household activities including agricu",
                      "I have not been employed" 
                                )))))
```


## 9. Number of Interviews conducted by each interviewer


```r
library(knitr)
#DollarSign Syntax

kable(table(R_testu$Interviewer))
```



|Var1                 | Freq|
|:--------------------|----:|
|Ahumuza Owen         |   30|
|Alum Maria           |   30|
|Arinitwe Mackline    |   33|
|Kasule violet        |   33|
|Mirembe Mary         |   35|
|Muhindo Wilfred      |   30|
|Muwonge Allan Joshua |   30|
|Nabbumba Pennina     |   31|
|Wabwire Thomas       |   31|
|Wambi Ken Paul       |   34|


## 10. Clean up the ward variable and then compute the number of interviews per ward 


```r
which(is.na(R_testu$Ward)) #check for missing values in ward
```

```
## integer(0)
```

```r
R_testu$Ward <- gsub("Bukoto Nsimbi ziwome", "Bukoto", R_testu$Ward)
R_testu$Ward <- gsub("Bukoto Nsimbi Ziwome", "Bukoto", R_testu$Ward)
R_testu$Ward <- gsub("Bukoto church", "Bukoto", R_testu$Ward)

R_testu$Ward <- gsub("KABOWA", "Kabowa", R_testu$Ward)
R_testu$Ward <- gsub("kabowa", "Kabowa", R_testu$Ward)
R_testu$Ward <- gsub("Kafumbe mukasa", "Kafumbe Mukasa", R_testu$Ward)
R_testu$Ward <- gsub("kagugube", "Kagugube", R_testu$Ward)
R_testu$Ward <- gsub("kamokya", "Kamwokya", R_testu$Ward)
R_testu$Ward <- gsub("KAMWOKYA", "Kamwokya", R_testu$Ward)
R_testu$Ward <- gsub("kansanga", "Kansanga", R_testu$Ward)
R_testu$Ward <- gsub("kasanga", "Kansanga", R_testu$Ward)
R_testu$Ward <- gsub("Kansanga kiwafu Estates", "Kansanga", R_testu$Ward)
R_testu$Ward <- gsub("kanyanya - komamboga zone A", "Kanyanya zone", R_testu$Ward)
R_testu$Ward <- gsub("kanyanya central zone 1", "Kanyanya zone", R_testu$Ward)
R_testu$Ward <- gsub("Kanyanya central zone B", "Kanyanya zone", R_testu$Ward)
R_testu$Ward <- gsub("kasubi", "Kasubi", R_testu$Ward)
R_testu$Ward <- gsub("KASUBI", "Kasubi", R_testu$Ward)
R_testu$Ward <- gsub("kawaala", "Kawaala", R_testu$Ward)
R_testu$Ward <- gsub("Kinawataka zone 1", "Kinawataka", R_testu$Ward)
R_testu$Ward <- gsub("KISASI DUNGU ZONE", "Dungu Zone", R_testu$Ward)
R_testu$Ward <- gsub("kisenyi", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("KISENYI", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("Kisenyi zone 3", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("kisira zone", "Kisira zone", R_testu$Ward)
R_testu$Ward <- gsub("kisugu", "Kisugu", R_testu$Ward)
R_testu$Ward <- gsub("Kisugu Upper Zone", "Kisugu", R_testu$Ward)
R_testu$Ward <- gsub("kiwatule", "Kiwatule", R_testu$Ward)
R_testu$Ward <- gsub("Kiwatule  kinyarwanda", "Kinyarwanda zone", R_testu$Ward)
R_testu$Ward <- gsub("Kiwatule kinyarwanda", "Kinyarwanda zone", R_testu$Ward)

R_testu$Ward <- gsub("lugala", "Lugala", R_testu$Ward)
R_testu$Ward <- gsub("Luwafu kirundu zone", "kiruddu", R_testu$Ward)

R_testu$Ward <- gsub("Makerere  University", "MAKERERE", R_testu$Ward)
R_testu$Ward <- gsub("Makerere Kagugube", "Kagugube", R_testu$Ward)
R_testu$Ward <- gsub("Makerere Kakugube", "Kagugube", R_testu$Ward)
R_testu$Ward <- gsub("makerere kakugube zone", "Kagugube", R_testu$Ward)
R_testu$Ward <- gsub("MAKERERE KAVULE", "Makerere kavule", R_testu$Ward)
R_testu$Ward <- gsub("Makerere kivulu", "Kivulu", R_testu$Ward)
R_testu$Ward <- gsub("Makerere Kivulu", "Kivulu", R_testu$Ward)
R_testu$Ward <- gsub("Mamerere Kagugube", "Kagugube", R_testu$Ward)
R_testu$Ward <- gsub("Makindyd Luwafu", "Makindye", R_testu$Ward)
R_testu$Ward <- gsub("makindye luwafu", "Makindye", R_testu$Ward)
R_testu$Ward <- gsub("Makindye luwafu", "Makindye", R_testu$Ward)
R_testu$Ward <- gsub("Makindye Luwafu", "Makindye", R_testu$Ward)
R_testu$Ward <- gsub("Makindye luwafu kirundu", "kiruddu", R_testu$Ward)
R_testu$Ward <- gsub("Makindye kirundu", "kiruddu", R_testu$Ward)
R_testu$Ward <- gsub("masanafu", "Masanafu", R_testu$Ward)
R_testu$Ward <- gsub("MASANAFU", "Masanafu", R_testu$Ward)
R_testu$Ward <- gsub("MBUYA", "Mbuya", R_testu$Ward)
R_testu$Ward <- gsub("Mbuya 1", "Mbuya", R_testu$Ward)
R_testu$Ward <- gsub("Mbuya kaggo", "Mbuya", R_testu$Ward)
R_testu$Ward <- gsub("Mbuya Kaggo", "Mbuya", R_testu$Ward)
R_testu$Ward <- gsub("Mbuya zone 1", "Mbuya", R_testu$Ward)
R_testu$Ward <- gsub("Mbuya kinawataka", "Kinawataka", R_testu$Ward)
R_testu$Ward <- gsub("Mulago ward C", "Mulago", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo  Some 4", "Mutungo", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo Biina", "Mutungo", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo zone 4", "Mutungo", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo Zone 4", "Mutungo", R_testu$Ward)
R_testu$Ward <- gsub("Mutungu", "Mutungo", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo  Kampala road", "KAMPALA ROAD", R_testu$Ward)
R_testu$Ward <- gsub("Mutungo Kampala road", "KAMPALA ROAD", R_testu$Ward)

R_testu$Ward <- gsub("NABWERU ZONE 1", "Nabweru", R_testu$Ward)
R_testu$Ward <- gsub("nabweru zone1", "Nabweru", R_testu$Ward)
R_testu$Ward <- gsub("NAGURU BARRACKS", "NAGURU", R_testu$Ward)
R_testu$Ward <- gsub("NAKULABYE", "Nakulabye", R_testu$Ward)
R_testu$Ward <- gsub("NALUKABYE", "Nakulabye", R_testu$Ward)
R_testu$Ward <- gsub("NANKULABYE", "Nakulabye", R_testu$Ward)

R_testu$Ward <- gsub("NAMUNGONA", "Namungoona", R_testu$Ward)
R_testu$Ward <- gsub("Namungona Kisugu", "Namungoona", R_testu$Ward)
R_testu$Ward <- gsub("nsambya", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya  Barracks.", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya barracks", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya Barracks", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya kevina", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya Kevina", "Nsambya", R_testu$Ward)

R_testu$Ward <- gsub("Sebagala Zone", "Sebaggala zone", R_testu$Ward)
R_testu$Ward <- gsub("wandegeya.junju road", "Wandegeya", R_testu$Ward)
R_testu$Ward <- gsub("WANDEGEYA", "Wandegeya", R_testu$Ward)
R_testu$Ward <- gsub("wankulukuku", "Wankulukuku", R_testu$Ward)
R_testu$Ward <- gsub("WANKULUKUKU", "Wankulukuku", R_testu$Ward)
R_testu$Ward <- gsub("WUNKULUKUKU", "Wankulukuku", R_testu$Ward)

kable(table(R_testu$Ward))
```



|Var1                  | Freq|
|:---------------------|----:|
|Boma zone             |    1|
|Buganda road flats    |    2|
|BUGOLOBI              |    1|
|Bukoto                |    3|
|BUSEGA                |   11|
|BWAISE                |    2|
|Dungu Zone            |    7|
|Kabalagala            |    1|
|Kabowa                |   10|
|Kafumbe Mukasa        |    2|
|Kagugube              |   10|
|Kagwo                 |    1|
|KAMPALA ROAD          |    3|
|Kamwokya              |   11|
|Kansanga              |   12|
|Kanyanya zone         |    3|
|Kasirye zone/mpererwe |    1|
|Kasubi                |   10|
|kasule zone           |    1|
|KATAZA                |    1|
|Kawaala               |    6|
|kazo                  |    2|
|Kiganda zone          |    5|
|Kinawataka            |    5|
|Kinyarwanda zone      |    5|
|KIREKA MIWANDA ZONE   |    1|
|kiruddu               |    3|
|Kisenyi               |    9|
|Kisira zone           |    2|
|Kisugu                |   18|
|KITEBI                |    1|
|Kivulu                |    5|
|Kiwatule              |    7|
|Kizza zone            |    1|
|KYEBANDO              |    9|
|Liganda Zone          |    1|
|Lugala                |    3|
|Lukalubo zone         |    1|
|LUSAZE                |    3|
|Luwafu                |    6|
|MAKERERE              |    8|
|Makerere kavule       |    3|
|Makindye              |    5|
|Masanafu              |   16|
|Mbuya                 |    9|
|Mulago                |    2|
|Mutungo               |   16|
|NABULAGALA            |    3|
|Nabweru               |    2|
|NAGURU                |    5|
|Nakasero              |    1|
|Nakawa                |    4|
|Nakulabye             |    9|
|Namungoona            |    8|
|NATEETE               |    4|
|NDEEBA                |    4|
|Nsambya               |   12|
|Ntinda                |    1|
|Sebaggala zone        |    3|
|Tula zone             |    1|
|Wandegeya             |    7|
|Wankulukuku           |    6|
|Wheeling Zone         |    2|

## 11. interviews were conducted in every ward per interviewer 


```r
kable(table(R_testu$Ward, R_testu$Interviewer))
```



|                      | Ahumuza Owen| Alum Maria| Arinitwe Mackline| Kasule violet| Mirembe Mary| Muhindo Wilfred| Muwonge Allan Joshua| Nabbumba Pennina| Wabwire Thomas| Wambi Ken Paul|
|:---------------------|------------:|----------:|-----------------:|-------------:|------------:|---------------:|--------------------:|----------------:|--------------:|--------------:|
|Boma zone             |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|Buganda road flats    |            0|          2|                 0|             0|            0|               0|                    0|                0|              0|              0|
|BUGOLOBI              |            0|          0|                 0|             0|            0|               1|                    0|                0|              0|              0|
|Bukoto                |            0|          0|                 0|             0|            0|               0|                    0|                3|              0|              0|
|BUSEGA                |            0|          0|                 0|            11|            0|               0|                    0|                0|              0|              0|
|BWAISE                |            0|          0|                 0|             0|            2|               0|                    0|                0|              0|              0|
|Dungu Zone            |            0|          6|                 0|             0|            0|               1|                    0|                0|              0|              0|
|Kabalagala            |            0|          0|                 0|             0|            0|               1|                    0|                0|              0|              0|
|Kabowa                |            0|          0|                 0|             5|            5|               0|                    0|                0|              0|              0|
|Kafumbe Mukasa        |            2|          0|                 0|             0|            0|               0|                    0|                0|              0|              0|
|Kagugube              |            2|          2|                 0|             0|            0|               0|                    2|                4|              0|              0|
|Kagwo                 |            1|          0|                 0|             0|            0|               0|                    0|                0|              0|              0|
|KAMPALA ROAD          |            0|          0|                 0|             0|            0|               0|                    2|                0|              1|              0|
|Kamwokya              |            0|          0|                 0|             5|            5|               0|                    0|                1|              0|              0|
|Kansanga              |            2|          0|                 0|             0|            0|               4|                    4|                2|              0|              0|
|Kanyanya zone         |            0|          0|                 0|             0|            0|               0|                    0|                0|              3|              0|
|Kasirye zone/mpererwe |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|Kasubi                |            0|          0|                 9|             0|            0|               0|                    0|                0|              0|              1|
|kasule zone           |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|KATAZA                |            0|          0|                 0|             0|            0|               1|                    0|                0|              0|              0|
|Kawaala               |            0|          0|                 6|             0|            0|               0|                    0|                0|              0|              0|
|kazo                  |            0|          0|                 0|             0|            0|               0|                    0|                0|              2|              0|
|Kiganda zone          |            0|          0|                 0|             0|            0|               0|                    0|                0|              5|              0|
|Kinawataka            |            3|          2|                 0|             0|            0|               0|                    0|                0|              0|              0|
|Kinyarwanda zone      |            1|          0|                 0|             0|            0|               0|                    0|                4|              0|              0|
|KIREKA MIWANDA ZONE   |            0|          0|                 0|             0|            0|               1|                    0|                0|              0|              0|
|kiruddu               |            0|          0|                 0|             0|            0|               0|                    2|                1|              0|              0|
|Kisenyi               |            0|          0|                 4|             0|            0|               0|                    0|                0|              0|              5|
|Kisira zone           |            0|          0|                 0|             0|            0|               0|                    0|                0|              2|              0|
|Kisugu                |            3|          4|                 0|             0|            0|               3|                    4|                4|              0|              0|
|KITEBI                |            0|          0|                 0|             0|            1|               0|                    0|                0|              0|              0|
|Kivulu                |            0|          0|                 0|             0|            0|               4|                    0|                0|              1|              0|
|Kiwatule              |            3|          0|                 0|             0|            0|               0|                    4|                0|              0|              0|
|Kizza zone            |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|KYEBANDO              |            0|          0|                 0|             5|            4|               0|                    0|                0|              0|              0|
|Liganda Zone          |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|Lugala                |            0|          0|                 3|             0|            0|               0|                    0|                0|              0|              0|
|Lukalubo zone         |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|LUSAZE                |            0|          0|                 0|             0|            0|               0|                    0|                0|              0|              3|
|Luwafu                |            4|          0|                 1|             0|            0|               0|                    1|                0|              0|              0|
|MAKERERE              |            0|          0|                 0|             0|            7|               0|                    1|                0|              0|              0|
|Makerere kavule       |            0|          0|                 0|             0|            0|               0|                    0|                0|              3|              0|
|Makindye              |            0|          3|                 0|             0|            0|               0|                    0|                2|              0|              0|
|Masanafu              |            0|          0|                 6|             0|            0|               0|                    0|                0|              0|             10|
|Mbuya                 |            1|          2|                 0|             0|            0|               1|                    5|                0|              0|              0|
|Mulago                |            0|          0|                 0|             0|            0|               0|                    0|                0|              2|              0|
|Mutungo               |            5|          5|                 0|             0|            0|               3|                    3|                0|              0|              0|
|NABULAGALA            |            0|          0|                 0|             0|            0|               0|                    0|                0|              0|              3|
|Nabweru               |            0|          0|                 0|             0|            0|               0|                    0|                0|              2|              0|
|NAGURU                |            0|          0|                 0|             0|            0|               5|                    0|                0|              0|              0|
|Nakasero              |            0|          1|                 0|             0|            0|               0|                    0|                0|              0|              0|
|Nakawa                |            0|          0|                 0|             0|            0|               0|                    0|                4|              0|              0|
|Nakulabye             |            0|          0|                 2|             0|            4|               0|                    0|                0|              0|              3|
|Namungoona            |            0|          0|                 2|             0|            0|               1|                    0|                0|              0|              5|
|NATEETE               |            0|          0|                 0|             4|            0|               0|                    0|                0|              0|              0|
|NDEEBA                |            0|          0|                 0|             0|            0|               0|                    0|                0|              0|              4|
|Nsambya               |            3|          0|                 0|             0|            0|               3|                    0|                6|              0|              0|
|Ntinda                |            0|          0|                 0|             0|            0|               1|                    0|                0|              0|              0|
|Sebaggala zone        |            0|          0|                 0|             0|            0|               0|                    0|                0|              3|              0|
|Tula zone             |            0|          0|                 0|             0|            0|               0|                    0|                0|              1|              0|
|Wandegeya             |            0|          1|                 0|             0|            4|               0|                    2|                0|              0|              0|
|Wankulukuku           |            0|          0|                 0|             3|            3|               0|                    0|                0|              0|              0|
|Wheeling Zone         |            0|          2|                 0|             0|            0|               0|                    0|                0|              0|              0|

```r
kable(table(R_testu$Interviewer, R_testu$Ward))
```



|                     | Boma zone| Buganda road flats| BUGOLOBI| Bukoto| BUSEGA| BWAISE| Dungu Zone| Kabalagala| Kabowa| Kafumbe Mukasa| Kagugube| Kagwo| KAMPALA ROAD| Kamwokya| Kansanga| Kanyanya zone| Kasirye zone/mpererwe| Kasubi| kasule zone| KATAZA| Kawaala| kazo| Kiganda zone| Kinawataka| Kinyarwanda zone| KIREKA MIWANDA ZONE| kiruddu| Kisenyi| Kisira zone| Kisugu| KITEBI| Kivulu| Kiwatule| Kizza zone| KYEBANDO| Liganda Zone| Lugala| Lukalubo zone| LUSAZE| Luwafu| MAKERERE| Makerere kavule| Makindye| Masanafu| Mbuya| Mulago| Mutungo| NABULAGALA| Nabweru| NAGURU| Nakasero| Nakawa| Nakulabye| Namungoona| NATEETE| NDEEBA| Nsambya| Ntinda| Sebaggala zone| Tula zone| Wandegeya| Wankulukuku| Wheeling Zone|
|:--------------------|---------:|------------------:|--------:|------:|------:|------:|----------:|----------:|------:|--------------:|--------:|-----:|------------:|--------:|--------:|-------------:|---------------------:|------:|-----------:|------:|-------:|----:|------------:|----------:|----------------:|-------------------:|-------:|-------:|-----------:|------:|------:|------:|--------:|----------:|--------:|------------:|------:|-------------:|------:|------:|--------:|---------------:|--------:|--------:|-----:|------:|-------:|----------:|-------:|------:|--------:|------:|---------:|----------:|-------:|------:|-------:|------:|--------------:|---------:|---------:|-----------:|-------------:|
|Ahumuza Owen         |         0|                  0|        0|      0|      0|      0|          0|          0|      0|              2|        2|     1|            0|        0|        2|             0|                     0|      0|           0|      0|       0|    0|            0|          3|                1|                   0|       0|       0|           0|      3|      0|      0|        3|          0|        0|            0|      0|             0|      0|      4|        0|               0|        0|        0|     1|      0|       5|          0|       0|      0|        0|      0|         0|          0|       0|      0|       3|      0|              0|         0|         0|           0|             0|
|Alum Maria           |         0|                  2|        0|      0|      0|      0|          6|          0|      0|              0|        2|     0|            0|        0|        0|             0|                     0|      0|           0|      0|       0|    0|            0|          2|                0|                   0|       0|       0|           0|      4|      0|      0|        0|          0|        0|            0|      0|             0|      0|      0|        0|               0|        3|        0|     2|      0|       5|          0|       0|      0|        1|      0|         0|          0|       0|      0|       0|      0|              0|         0|         1|           0|             2|
|Arinitwe Mackline    |         0|                  0|        0|      0|      0|      0|          0|          0|      0|              0|        0|     0|            0|        0|        0|             0|                     0|      9|           0|      0|       6|    0|            0|          0|                0|                   0|       0|       4|           0|      0|      0|      0|        0|          0|        0|            0|      3|             0|      0|      1|        0|               0|        0|        6|     0|      0|       0|          0|       0|      0|        0|      0|         2|          2|       0|      0|       0|      0|              0|         0|         0|           0|             0|
|Kasule violet        |         0|                  0|        0|      0|     11|      0|          0|          0|      5|              0|        0|     0|            0|        5|        0|             0|                     0|      0|           0|      0|       0|    0|            0|          0|                0|                   0|       0|       0|           0|      0|      0|      0|        0|          0|        5|            0|      0|             0|      0|      0|        0|               0|        0|        0|     0|      0|       0|          0|       0|      0|        0|      0|         0|          0|       4|      0|       0|      0|              0|         0|         0|           3|             0|
|Mirembe Mary         |         0|                  0|        0|      0|      0|      2|          0|          0|      5|              0|        0|     0|            0|        5|        0|             0|                     0|      0|           0|      0|       0|    0|            0|          0|                0|                   0|       0|       0|           0|      0|      1|      0|        0|          0|        4|            0|      0|             0|      0|      0|        7|               0|        0|        0|     0|      0|       0|          0|       0|      0|        0|      0|         4|          0|       0|      0|       0|      0|              0|         0|         4|           3|             0|
|Muhindo Wilfred      |         0|                  0|        1|      0|      0|      0|          1|          1|      0|              0|        0|     0|            0|        0|        4|             0|                     0|      0|           0|      1|       0|    0|            0|          0|                0|                   1|       0|       0|           0|      3|      0|      4|        0|          0|        0|            0|      0|             0|      0|      0|        0|               0|        0|        0|     1|      0|       3|          0|       0|      5|        0|      0|         0|          1|       0|      0|       3|      1|              0|         0|         0|           0|             0|
|Muwonge Allan Joshua |         0|                  0|        0|      0|      0|      0|          0|          0|      0|              0|        2|     0|            2|        0|        4|             0|                     0|      0|           0|      0|       0|    0|            0|          0|                0|                   0|       2|       0|           0|      4|      0|      0|        4|          0|        0|            0|      0|             0|      0|      1|        1|               0|        0|        0|     5|      0|       3|          0|       0|      0|        0|      0|         0|          0|       0|      0|       0|      0|              0|         0|         2|           0|             0|
|Nabbumba Pennina     |         0|                  0|        0|      3|      0|      0|          0|          0|      0|              0|        4|     0|            0|        1|        2|             0|                     0|      0|           0|      0|       0|    0|            0|          0|                4|                   0|       1|       0|           0|      4|      0|      0|        0|          0|        0|            0|      0|             0|      0|      0|        0|               0|        2|        0|     0|      0|       0|          0|       0|      0|        0|      4|         0|          0|       0|      0|       6|      0|              0|         0|         0|           0|             0|
|Wabwire Thomas       |         1|                  0|        0|      0|      0|      0|          0|          0|      0|              0|        0|     0|            1|        0|        0|             3|                     1|      0|           1|      0|       0|    2|            5|          0|                0|                   0|       0|       0|           2|      0|      0|      1|        0|          1|        0|            1|      0|             1|      0|      0|        0|               3|        0|        0|     0|      2|       0|          0|       2|      0|        0|      0|         0|          0|       0|      0|       0|      0|              3|         1|         0|           0|             0|
|Wambi Ken Paul       |         0|                  0|        0|      0|      0|      0|          0|          0|      0|              0|        0|     0|            0|        0|        0|             0|                     0|      1|           0|      0|       0|    0|            0|          0|                0|                   0|       0|       5|           0|      0|      0|      0|        0|          0|        0|            0|      0|             0|      3|      0|        0|               0|        0|       10|     0|      0|       0|          3|       0|      0|        0|      0|         3|          5|       0|      4|       0|      0|              0|         0|         0|           0|             0|


## 12.	Would you say older people are in high Social class?


```r
kable(table(R_testu$Social_Class, R_testu$A2))
```



|                | 18| 19| 20| 21| 22| 23| 24| 25| 26| 27| 28| 30|
|:---------------|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|--:|
|Social Class B  |  7| 26| 27| 10|  9|  9| 12|  6|  2|  1|  0|  1|
|Social Class C1 | 11| 34| 24| 13|  5|  8|  6|  4|  2|  2|  1|  0|
|Social Class C2 |  8| 25| 19|  9|  9| 10|  7|  6|  0|  2|  1|  1|

```r
kable(table(R_testu$A2, R_testu$Social_Class))
```



|   | Social Class B| Social Class C1| Social Class C2|
|:--|--------------:|---------------:|---------------:|
|18 |              7|              11|               8|
|19 |             26|              34|              25|
|20 |             27|              24|              19|
|21 |             10|              13|               9|
|22 |              9|               5|               9|
|23 |              9|               8|              10|
|24 |             12|               6|               7|
|25 |              6|               4|               6|
|26 |              2|               2|               0|
|27 |              1|               2|               2|
|28 |              0|               1|               1|
|30 |              1|               0|               1|

```r
kable(table(R_testu$Social_Class, R_testu$S2))
```



|                | 18-25| 26-30|
|:---------------|-----:|-----:|
|Social Class B  |   106|     4|
|Social Class C1 |   105|     5|
|Social Class C2 |    93|     4|

```r
kable(table(R_testu$S2, R_testu$Social_Class))
```



|      | Social Class B| Social Class C1| Social Class C2|
|:-----|--------------:|---------------:|---------------:|
|18-25 |            106|             105|              93|
|26-30 |              4|               5|               4|


## 13.	What is the minimum, maximum, average and median age of the respondents 


```r
library(mosaic)
#favstats(~A2, data=R_testu)
kable(favstats(~A2, data=R_testu)[c("min", "max", "mean","median")])
```



|   | min| max|     mean| median|
|:--|---:|---:|--------:|------:|
|   |  18|  30| 20.93691|     20|

```r
#favstats(~B16, data=R_testu)
kable(favstats(~B16, data=R_testu)[c("min", "max", "mean","median")])
```



|   |   min|   max|     mean| median|
|:--|-----:|-----:|--------:|------:|
|   | 2e+05| 3e+06| 769526.8|  7e+05|

## 14.	Plot A2 against B16. Add title and axes labels 

### ggplot- lineplot


```r
ggplot(R_testu%>%group_by(A2) %>%summarise(B16 = mean(B16)), aes(A2, B16))+
  labs(title="Avearage amount willing to be paid for next degree/diploma", 
       x="Age of Respondents", y="Amount per Semester") +
  theme(axis.title=element_text(face="bold.italic", size="12", color="brown"), 
        axis.text = element_text(size = 8, face="bold"), 
        plot.title=element_text(size=12, face="bold", color="red", hjust = 0.5))+
  geom_point(size=2)+
  geom_line(colour="blue", size=1)+
  #geom_text(aes(label=R_testuplot$A2), nudge_x = 0.2, size=3)+
  scale_x_continuous(breaks = c(16, 18, 20, 22, 24, 26, 28, 30, 32), limits=c(17,31))+
  scale_y_continuous(breaks = c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), 
                     limits=c(0,1200000))
```

![](DPE_RTest_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#If the plot is on the screen ggsave(“path/filename.png”)

#If you have a plot object ggsave(myplot, file=“path/filename.png”)

#Specify size ggsave(myplot, “path/filename.png”, width=6, height=4)

# any plot format (pdf, png, eps, svg, jpg)
```

### ggplot Scatterplot



```r
ggplot(R_testu, aes(A2, B16))+
  labs(title="Amount willing to be paid for next degree/diploma", 
       x="Age of Respondents", y="Amount per Semester") +
  theme(axis.title=element_text(face="bold.italic", size="12", color="brown"), 
        axis.text = element_text(size = 8, face="bold"), 
        plot.title=element_text(size=12, face="bold", color="red", hjust = 0.5))+
  geom_point(size=2)+
  scale_x_continuous(breaks = c(16, 18, 20, 22, 24, 26, 28, 30, 32), limits=c(17,31))+
  scale_y_continuous(breaks = c(0, 250000, 500000, 750000, 1000000, 1250000, 1500000, 
                  1750000, 2000000, 2250000, 2500000, 2750000, 3000000, 3250000),
                  limits=c(0,3250000))
```

![](DPE_RTest_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

## 15.	What is the proportion of girls who are in non-girls only schools 



```r
#Was your school an all secondary girls school?
table(R_testu$B4)
```

```
## 
##  No Yes 
## 282  35
```

```r
prop.table(table(R_testu$B4))
```

```
## 
##        No       Yes 
## 0.8895899 0.1104101
```

## 16. dataset of the respondents who are aged between 18 – 25 years only 


```r
R_testu18_25 <- subset(R_testu, S2 == "18-25")

#write_excel_csv(R_testu18_25, "R_testu18_25.csv")
```


## 17.	Conduct any additional analysis in the data 

### Distribution of B16 Variable


```r
nrow(R_test)
```

```
## [1] 321
```

```r
nrow(R_testu)
```

```
## [1] 317
```

```r
R_testu1 = subset(R_testu, select = -c(Q_25_S,Q_32_S) ) #Delete column by name

R_testu1 <- na.omit(R_testu1) # Getting rid of missing data

nrow(R_testu1)
```

```
## [1] 299
```

```r
which(is.na(R_testu1)) #check for missing values
```

```
## integer(0)
```

```r
sum(is.na(R_testu1))
```

```
## [1] 0
```

```r
library(car)
library(MASS) #So that distributions that must be non-zero can make sense of my data


qqp(R_testu1$B16+1, "norm", main="Q-Q Plot ~ Normal model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```
## [1]  26 176
```

```r
qqp(R_testu1$B16+1, "lnorm", main="Q-Q Plot ~ LogNormal model") #lnorm is lognormal
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```
## [1]  26 176
```

```r
qqp(R_testu1$B16+1, "exp", main="Q-Q Plot ~ Exponential model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

```
## [1]  26 176
```

```r
#qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr function.
#negative binomial and gamma distributions can only handle positive numbers.
#Poisson distribution can only handle positive whole numbers.
#Binomial and Poisson distributions are different from the others because they are
#discrete rather than continuous, which means they quantify distinct,
#countable events or the probability of these events

nbinom <- fitdistr(R_testu$B16+1, "Negative Binomial")
qqp(R_testu$B16+1, "nbinom", size = nbinom$estimate[[1]], mu =
nbinom$estimate[[2]], main="Q-Q Plot ~ Negative Binomial model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-4.png)<!-- -->

```
## [1]  27 184
```

```r
pois <- fitdistr(R_testu1$B16+1, "Poisson")
qqp(R_testu1$B16+1, "pois", lambda=pois$estimate, main="Q-Q Plot ~ Poisson model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-5.png)<!-- -->

```
## [1]  26 176
```

```r
gamma <- fitdistr(R_testu1$B16+1, "gamma",
list(shape = 1, rate = 0.1), lower = 0.4)
qqp(R_testu1$B16+1, "gamma", shape = gamma$estimate[[1]], rate =
gamma$estimate[[2]], main="Q-Q Plot ~ Gamma model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-6.png)<!-- -->

```
## [1]  26 176
```

```r
weibull <- fitdistr(R_testu1$B16+1, "weibull")
qqp(R_testu1$B16+1, "weibull", shape = weibull$estimate[[1]],
scale=weibull$estimate[[2]], main="Q-Q Plot ~ Weibull model")
```

![](DPE_RTest_files/figure-html/unnamed-chunk-16-7.png)<!-- -->

```
## [1]  26 176
```

### Multiple Regression


```r
#convert categorical variables to integer data types for regression purpose

R_testu_reg <- R_testu%>%
  mutate(S3 = if_else(S3 == 1, "None/did not complete",
              if_else(S3 == 2, "Uganda Certificate of Education (UCE) 1-2 passes",
              if_else(S3 == 3, "Uganda Certificate of Education (UCE) 3-5 passes",
              if_else(S3 == 4, "Uganda Certificate of Education (UCE) 5 or more passes",        
              if_else(S3 == 5, "Uganda Advanced Certificate of Education (UACE) – 1 principa", 
              if_else(S3 == 6, "Uganda Advanced Certificate of Education (UACE) 2 principal", 
                    "Uganda Advanced Certificate of Education (UACE) 3 principals")))))))%>%
  mutate(S4 = if_else(S4 == 1, "Yes", "No"))%>%
  mutate(S5_a = if_else(S5_a == 1, "Yes",
                        if_else(S5_a == 2, "No", "I don't know")))%>%
  mutate(A5 = if_else(A5 == 1, "Central",
              if_else(A5 == 2, "Western",
              if_else(A5 == 3, "Eastern",
              if_else(A5 == 4, "Northern", "Other")))))%>%
  mutate(A8 = if_else(A8 == 1, "Single",
              if_else(A8 == 2, "Married",
              if_else(A8 == 3, "Divorced or Separated", "Widowed"))))%>%
  mutate(A9 = if_else(A9 == 1, "I do not have children",
              if_else(A9 == 2, "I have 1 child",
              if_else(A9 == 3, "I have 2 children",
              if_else(A9 == 4, "I have 3 children", "I have more than 3 children")))))%>%
  mutate(A10 = if_else(A10 == 1, "I live alone",
              if_else(A10 == 2, "I live with one other person",
              if_else(A10 == 3, "I live with 2-3 other people",
              if_else(A10 == 4, "I live with 4-6 other people",
                      "I live with 7 or more people")))))%>%
  mutate(A12 = if_else(A12 == 1, "I am the head of the household",
              if_else(A12 == 2, "I am the wife of the head of household",
              if_else(A12 == 3, "I am the daughter of the head of household",
              if_else(A12 == 4, "I am a relative of the head of household",        
              if_else(A12 == 5, "I am a friend of the head of household", 
              "Other"))))))%>%
  mutate(B8 = if_else(B8 == 1, "Diploma",
                        if_else(B8 == 2, "Technical certificate", "Bachelor's degree")))%>%
  mutate(S2 = factor(S2))%>%
  mutate(S3 = factor(S3))%>%
  mutate(S4 = factor(S4))%>%
  mutate(Social_Class = factor(Social_Class))%>%
  mutate(A5 = factor(A5))%>%
  mutate(A8 = factor(A8))%>%
  mutate(A9 = factor(A9))%>%
  mutate(A10 = factor(A10))%>%
  mutate(A12 = factor(A12))%>%
  mutate(B4 = factor(B4))%>%
  mutate(B8 = factor(B8))%>%
  mutate(B17 = factor(B17))%>%
  dplyr:: rename(age_grouped = "S2",
                 secondary_school_certificate = "S3",
                 enrolled_tertiary_school_certificate = "S4",
                 age = "A2",
                 region_of_origin = "A5",
                 marital_status = "A8",
                 No_of_children = "A9",
                 No_people_living_with = "A10",
                 relationship_to_head_of_household = "A12",
                 all_secondary_girls_school = "B4",
                 career_next_level_of_education = "B8",
                 willing_to_pay_next_level_of_education = "B16",
                 career_ready = "B17")%>%
dplyr:: select(7,10,13,14, 16,17, 18, 19, 22, 23, 24, 25)%>%
  drop_na()

select
```

```
## function (obj) 
## UseMethod("select")
## <bytecode: 0x0000000022fb2e88>
## <environment: namespace:MASS>
```

```r
str(R_testu_reg)
```

```
## tibble [299 x 12] (S3: tbl_df/tbl/data.frame)
##  $ secondary_school_certificate          : Factor w/ 5 levels "Uganda Advanced Certificate of Education (UACE) – 1 principa",..: 4 5 4 3 3 3 5 5 5 3 ...
##  $ Social_Class                          : Factor w/ 3 levels "Social Class B",..: 2 3 2 1 1 3 3 1 3 1 ...
##  $ age                                   : num [1:299] 20 21 20 19 19 21 19 19 19 19 ...
##  $ region_of_origin                      : Factor w/ 5 levels "Central","Eastern",..: 5 2 1 1 1 1 1 5 1 5 ...
##  $ marital_status                        : Factor w/ 3 levels "Divorced or Separated",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ No_of_children                        : Factor w/ 4 levels "I do not have children",..: 1 1 1 1 1 2 1 1 1 1 ...
##  $ No_people_living_with                 : Factor w/ 5 levels "I live alone",..: 4 5 4 4 4 4 3 4 4 5 ...
##  $ relationship_to_head_of_household     : Factor w/ 6 levels "I am a friend of the head of household",..: 3 4 2 3 3 3 3 3 3 2 ...
##  $ all_secondary_girls_school            : Factor w/ 2 levels "No","Yes": 1 1 1 1 2 1 1 1 1 2 ...
##  $ career_next_level_of_education        : Factor w/ 3 levels "Bachelor's degree",..: 1 2 3 1 1 1 1 1 1 1 ...
##  $ willing_to_pay_next_level_of_education: num [1:299] 1000000 800000 500000 800000 1500000 800000 900000 700000 1500000 700000 ...
##  $ career_ready                          : Factor w/ 5 levels "Definitely not ready",..: 4 5 5 1 5 5 4 4 5 1 ...
```




```r
mlm1a <- lm(willing_to_pay_next_level_of_education ~ .,
data=R_testu_reg) #multiple regression
summary(mlm1a)
```

```
## 
## Call:
## lm(formula = willing_to_pay_next_level_of_education ~ ., data = R_testu_reg)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -607019 -179528  -54167   88640 1754448 
## 
## Coefficients:
##                                                                                          Estimate
## (Intercept)                                                                               1372883
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    129480
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   -67150
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                 8837
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          24022
## Social_ClassSocial Class C1                                                                -23585
## Social_ClassSocial Class C2                                                               -138268
## age                                                                                        -23926
## region_of_originEastern                                                                    -28760
## region_of_originNorthern                                                                    35294
## region_of_originOther                                                                      -50235
## region_of_originWestern                                                                    -44586
## marital_statusMarried                                                                       54429
## marital_statusSingle                                                                        -6418
## No_of_childrenI have 1 child                                                               -32814
## No_of_childrenI have 2 children                                                             36925
## No_of_childrenI have 3 children                                                            -22678
## No_people_living_withI live with 4-6 other people                                           59738
## No_people_living_withI live with 7 or more people                                          -35528
## No_people_living_withI live with one other person                                          -33855
## relationship_to_head_of_householdI am a relative of the head of household                   31961
## relationship_to_head_of_householdI am the daughter of the head of household                 92937
## relationship_to_head_of_householdI am the head of the household                            240712
## relationship_to_head_of_householdI am the wife of the head of household                     37882
## relationship_to_head_of_householdOther                                                     159011
## all_secondary_girls_schoolYes                                                              -32918
## career_next_level_of_educationDiploma                                                     -268827
## career_next_level_of_educationTechnical certificate                                       -439516
## career_readyExtremely ready                                                                 41547
## career_readyNeutral                                                                        167333
## career_readyNot ready                                                                      -24029
## career_readySomewhat ready                                                                  27608
##                                                                                          Std. Error
## (Intercept)                                                                                  591435
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal      208472
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals     204516
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                 205277
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes           205497
## Social_ClassSocial Class C1                                                                   48346
## Social_ClassSocial Class C2                                                                   50319
## age                                                                                           11631
## region_of_originEastern                                                                       60765
## region_of_originNorthern                                                                      71249
## region_of_originOther                                                                        159366
## region_of_originWestern                                                                       51218
## marital_statusMarried                                                                        415209
## marital_statusSingle                                                                         366537
## No_of_childrenI have 1 child                                                                  99351
## No_of_childrenI have 2 children                                                              162697
## No_of_childrenI have 3 children                                                              273131
## No_people_living_withI live with 4-6 other people                                             57318
## No_people_living_withI live with 7 or more people                                             63029
## No_people_living_withI live with one other person                                             77913
## relationship_to_head_of_householdI am a relative of the head of household                    249257
## relationship_to_head_of_householdI am the daughter of the head of household                  248180
## relationship_to_head_of_householdI am the head of the household                              271122
## relationship_to_head_of_householdI am the wife of the head of household                      327757
## relationship_to_head_of_householdOther                                                       274808
## all_secondary_girls_schoolYes                                                                 66837
## career_next_level_of_educationDiploma                                                         48441
## career_next_level_of_educationTechnical certificate                                          138286
## career_readyExtremely ready                                                                  140579
## career_readyNeutral                                                                           88316
## career_readyNot ready                                                                         53212
## career_readySomewhat ready                                                                    54113
##                                                                                          t value
## (Intercept)                                                                                2.321
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.621
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  -0.328
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.043
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.117
## Social_ClassSocial Class C1                                                               -0.488
## Social_ClassSocial Class C2                                                               -2.748
## age                                                                                       -2.057
## region_of_originEastern                                                                   -0.473
## region_of_originNorthern                                                                   0.495
## region_of_originOther                                                                     -0.315
## region_of_originWestern                                                                   -0.871
## marital_statusMarried                                                                      0.131
## marital_statusSingle                                                                      -0.018
## No_of_childrenI have 1 child                                                              -0.330
## No_of_childrenI have 2 children                                                            0.227
## No_of_childrenI have 3 children                                                           -0.083
## No_people_living_withI live with 4-6 other people                                          1.042
## No_people_living_withI live with 7 or more people                                         -0.564
## No_people_living_withI live with one other person                                         -0.435
## relationship_to_head_of_householdI am a relative of the head of household                  0.128
## relationship_to_head_of_householdI am the daughter of the head of household                0.374
## relationship_to_head_of_householdI am the head of the household                            0.888
## relationship_to_head_of_householdI am the wife of the head of household                    0.116
## relationship_to_head_of_householdOther                                                     0.579
## all_secondary_girls_schoolYes                                                             -0.493
## career_next_level_of_educationDiploma                                                     -5.550
## career_next_level_of_educationTechnical certificate                                       -3.178
## career_readyExtremely ready                                                                0.296
## career_readyNeutral                                                                        1.895
## career_readyNot ready                                                                     -0.452
## career_readySomewhat ready                                                                 0.510
##                                                                                          Pr(>|t|)
## (Intercept)                                                                               0.02103
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   0.53507
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  0.74292
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              0.96569
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        0.90703
## Social_ClassSocial Class C1                                                               0.62606
## Social_ClassSocial Class C2                                                               0.00641
## age                                                                                       0.04064
## region_of_originEastern                                                                   0.63639
## region_of_originNorthern                                                                  0.62076
## region_of_originOther                                                                     0.75284
## region_of_originWestern                                                                   0.38481
## marital_statusMarried                                                                     0.89580
## marital_statusSingle                                                                      0.98604
## No_of_childrenI have 1 child                                                              0.74144
## No_of_childrenI have 2 children                                                           0.82063
## No_of_childrenI have 3 children                                                           0.93389
## No_people_living_withI live with 4-6 other people                                         0.29826
## No_people_living_withI live with 7 or more people                                         0.57345
## No_people_living_withI live with one other person                                         0.66426
## relationship_to_head_of_householdI am a relative of the head of household                 0.89807
## relationship_to_head_of_householdI am the daughter of the head of household               0.70835
## relationship_to_head_of_householdI am the head of the household                           0.37543
## relationship_to_head_of_householdI am the wife of the head of household                   0.90807
## relationship_to_head_of_householdOther                                                    0.56333
## all_secondary_girls_schoolYes                                                             0.62276
## career_next_level_of_educationDiploma                                                     6.9e-08
## career_next_level_of_educationTechnical certificate                                       0.00166
## career_readyExtremely ready                                                               0.76781
## career_readyNeutral                                                                       0.05921
## career_readyNot ready                                                                     0.65195
## career_readySomewhat ready                                                                0.61034
##                                                                                             
## (Intercept)                                                                              *  
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal     
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals    
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          
## Social_ClassSocial Class C1                                                                 
## Social_ClassSocial Class C2                                                              ** 
## age                                                                                      *  
## region_of_originEastern                                                                     
## region_of_originNorthern                                                                    
## region_of_originOther                                                                       
## region_of_originWestern                                                                     
## marital_statusMarried                                                                       
## marital_statusSingle                                                                        
## No_of_childrenI have 1 child                                                                
## No_of_childrenI have 2 children                                                             
## No_of_childrenI have 3 children                                                             
## No_people_living_withI live with 4-6 other people                                           
## No_people_living_withI live with 7 or more people                                           
## No_people_living_withI live with one other person                                           
## relationship_to_head_of_householdI am a relative of the head of household                   
## relationship_to_head_of_householdI am the daughter of the head of household                 
## relationship_to_head_of_householdI am the head of the household                             
## relationship_to_head_of_householdI am the wife of the head of household                     
## relationship_to_head_of_householdOther                                                      
## all_secondary_girls_schoolYes                                                               
## career_next_level_of_educationDiploma                                                    ***
## career_next_level_of_educationTechnical certificate                                      ** 
## career_readyExtremely ready                                                                 
## career_readyNeutral                                                                      .  
## career_readyNot ready                                                                       
## career_readySomewhat ready                                                                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 330400 on 267 degrees of freedom
## Multiple R-squared:  0.2665,	Adjusted R-squared:  0.1814 
## F-statistic:  3.13 on 31 and 267 DF,  p-value: 3.137e-07
```



```r
# stepwise regression

library(MASS)
summary(stepAIC(mlm1a))
```

```
## Start:  AIC=7629.56
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + region_of_origin + marital_status + 
##     No_of_children + No_people_living_with + relationship_to_head_of_household + 
##     all_secondary_girls_school + career_next_level_of_education + 
##     career_ready
## 
##                                     Df  Sum of Sq        RSS    AIC
## - region_of_origin                   4 1.5995e+11 2.9305e+13 7623.2
## - No_of_children                     3 3.0269e+10 2.9176e+13 7623.9
## - relationship_to_head_of_household  5 4.6825e+11 2.9614e+13 7624.3
## - marital_status                     2 8.8816e+09 2.9154e+13 7625.7
## - career_ready                       4 5.5797e+11 2.9703e+13 7627.2
## - all_secondary_girls_school         1 2.6479e+10 2.9172e+13 7627.8
## - No_people_living_with              3 4.6042e+11 2.9606e+13 7628.2
## <none>                                            2.9146e+13 7629.6
## - secondary_school_certificate       4 9.2856e+11 3.0074e+13 7630.9
## - age                                1 4.6195e+11 2.9607e+13 7632.3
## - Social_Class                       2 9.2829e+11 3.0074e+13 7634.9
## - career_next_level_of_education     2 3.6273e+12 3.2773e+13 7660.6
## 
## Step:  AIC=7623.2
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + marital_status + No_of_children + No_people_living_with + 
##     relationship_to_head_of_household + all_secondary_girls_school + 
##     career_next_level_of_education + career_ready
## 
##                                     Df  Sum of Sq        RSS    AIC
## - No_of_children                     3 2.9948e+10 2.9335e+13 7617.5
## - relationship_to_head_of_household  5 4.7214e+11 2.9778e+13 7618.0
## - marital_status                     2 1.4170e+10 2.9320e+13 7619.3
## - career_ready                       4 5.1964e+11 2.9825e+13 7620.5
## - No_people_living_with              3 4.2158e+11 2.9727e+13 7621.5
## - all_secondary_girls_school         1 4.0037e+10 2.9346e+13 7621.6
## <none>                                            2.9305e+13 7623.2
## - secondary_school_certificate       4 1.0139e+12 3.0319e+13 7625.4
## - age                                1 5.1026e+11 2.9816e+13 7626.4
## - Social_Class                       2 8.8399e+11 3.0189e+13 7628.1
## - career_next_level_of_education     2 3.8391e+12 3.3145e+13 7656.0
## 
## Step:  AIC=7617.5
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + marital_status + No_people_living_with + 
##     relationship_to_head_of_household + all_secondary_girls_school + 
##     career_next_level_of_education + career_ready
## 
##                                     Df  Sum of Sq        RSS    AIC
## - relationship_to_head_of_household  5 4.7142e+11 2.9807e+13 7612.3
## - marital_status                     2 5.9436e+09 2.9341e+13 7613.6
## - career_ready                       4 5.1993e+11 2.9855e+13 7614.8
## - No_people_living_with              3 4.2099e+11 2.9756e+13 7615.8
## - all_secondary_girls_school         1 4.9909e+10 2.9385e+13 7616.0
## <none>                                            2.9335e+13 7617.5
## - secondary_school_certificate       4 1.0235e+12 3.0359e+13 7619.8
## - age                                1 5.7284e+11 2.9908e+13 7621.3
## - Social_Class                       2 8.9836e+11 3.0234e+13 7622.5
## - career_next_level_of_education     2 3.8406e+12 3.3176e+13 7650.3
## 
## Step:  AIC=7612.27
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + marital_status + No_people_living_with + 
##     all_secondary_girls_school + career_next_level_of_education + 
##     career_ready
## 
##                                  Df  Sum of Sq        RSS    AIC
## - marital_status                  2 2.0677e+09 2.9809e+13 7608.3
## - career_ready                    4 4.6983e+11 3.0277e+13 7608.9
## - all_secondary_girls_school      1 5.6860e+10 2.9864e+13 7610.8
## - No_people_living_with           3 5.3225e+11 3.0339e+13 7611.6
## <none>                                         2.9807e+13 7612.3
## - secondary_school_certificate    4 9.4307e+11 3.0750e+13 7613.6
## - age                             1 3.9483e+11 3.0202e+13 7614.2
## - Social_Class                    2 9.4326e+11 3.0750e+13 7617.6
## - career_next_level_of_education  2 3.8807e+12 3.3688e+13 7644.9
## 
## Step:  AIC=7608.29
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + No_people_living_with + all_secondary_girls_school + 
##     career_next_level_of_education + career_ready
## 
##                                  Df  Sum of Sq        RSS    AIC
## - career_ready                    4 4.7682e+11 3.0286e+13 7605.0
## - all_secondary_girls_school      1 5.6237e+10 2.9865e+13 7606.9
## - No_people_living_with           3 5.3444e+11 3.0343e+13 7607.6
## <none>                                         2.9809e+13 7608.3
## - secondary_school_certificate    4 9.4736e+11 3.0756e+13 7609.6
## - age                             1 4.2872e+11 3.0238e+13 7610.6
## - Social_Class                    2 9.4877e+11 3.0758e+13 7613.7
## - career_next_level_of_education  2 4.0637e+12 3.3873e+13 7642.5
## 
## Step:  AIC=7605.04
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + No_people_living_with + all_secondary_girls_school + 
##     career_next_level_of_education
## 
##                                  Df  Sum of Sq        RSS    AIC
## - all_secondary_girls_school      1 1.0811e+11 3.0394e+13 7604.1
## - No_people_living_with           3 5.2201e+11 3.0808e+13 7604.1
## <none>                                         3.0286e+13 7605.0
## - secondary_school_certificate    4 8.6633e+11 3.1152e+13 7605.5
## - age                             1 3.8993e+11 3.0676e+13 7606.9
## - Social_Class                    2 8.0000e+11 3.1086e+13 7608.8
## - career_next_level_of_education  2 4.0793e+12 3.4365e+13 7638.8
## 
## Step:  AIC=7604.1
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + No_people_living_with + career_next_level_of_education
## 
##                                  Df  Sum of Sq        RSS    AIC
## - No_people_living_with           3 5.7538e+11 3.0969e+13 7603.7
## <none>                                         3.0394e+13 7604.1
## - secondary_school_certificate    4 8.3517e+11 3.1229e+13 7604.2
## - age                             1 4.0128e+11 3.0795e+13 7606.0
## - Social_Class                    2 8.5809e+11 3.1252e+13 7608.4
## - career_next_level_of_education  2 4.3046e+12 3.4698e+13 7639.7
## 
## Step:  AIC=7603.71
## willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + career_next_level_of_education
## 
##                                  Df  Sum of Sq        RSS    AIC
## <none>                                         3.0969e+13 7603.7
## - secondary_school_certificate    4 8.4756e+11 3.1817e+13 7603.8
## - age                             1 6.0956e+11 3.1579e+13 7607.5
## - Social_Class                    2 8.3174e+11 3.1801e+13 7607.6
## - career_next_level_of_education  2 4.0143e+12 3.4984e+13 7636.2
```

```
## 
## Call:
## lm(formula = willing_to_pay_next_level_of_education ~ secondary_school_certificate + 
##     Social_Class + age + career_next_level_of_education, data = R_testu_reg)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -612738 -188027  -55845   87599 1882314 
## 
## Coefficients:
##                                                                                          Estimate
## (Intercept)                                                                               1316673
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    177813
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals    17463
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                52799
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         114731
## Social_ClassSocial Class C1                                                                -24002
## Social_ClassSocial Class C2                                                               -125964
## age                                                                                        -20933
## career_next_level_of_educationDiploma                                                     -256893
## career_next_level_of_educationTechnical certificate                                       -432579
##                                                                                          Std. Error
## (Intercept)                                                                                  277783
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal      198694
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals     193039
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                 196692
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes           194384
## Social_ClassSocial Class C1                                                                   46310
## Social_ClassSocial Class C2                                                                   47832
## age                                                                                            8777
## career_next_level_of_educationDiploma                                                         44411
## career_next_level_of_educationTechnical certificate                                          120246
##                                                                                          t value
## (Intercept)                                                                                4.740
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.895
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   0.090
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.268
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.590
## Social_ClassSocial Class C1                                                               -0.518
## Social_ClassSocial Class C2                                                               -2.633
## age                                                                                       -2.385
## career_next_level_of_educationDiploma                                                     -5.784
## career_next_level_of_educationTechnical certificate                                       -3.597
##                                                                                          Pr(>|t|)
## (Intercept)                                                                              3.36e-06
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal  0.371581
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals 0.927982
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes             0.788558
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes       0.555498
## Social_ClassSocial Class C1                                                              0.604656
## Social_ClassSocial Class C2                                                              0.008907
## age                                                                                      0.017723
## career_next_level_of_educationDiploma                                                    1.89e-08
## career_next_level_of_educationTechnical certificate                                      0.000378
##                                                                                             
## (Intercept)                                                                              ***
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal     
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals    
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          
## Social_ClassSocial Class C1                                                                 
## Social_ClassSocial Class C2                                                              ** 
## age                                                                                      *  
## career_next_level_of_educationDiploma                                                    ***
## career_next_level_of_educationTechnical certificate                                      ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 327400 on 289 degrees of freedom
## Multiple R-squared:  0.2206,	Adjusted R-squared:  0.1964 
## F-statistic: 9.091 on 9 and 289 DF,  p-value: 4.113e-12
```



```r
R_testu_reg$p1 <- round(predict(mlm1a, R_testu_reg),0)


sigma(mlm1a) #Residual Standard Error (RSE)
```

```
## [1] 330392.6
```

```r
#accuracy of the model.
sigma(mlm1a)/mean(R_testu_reg$willing_to_pay_next_level_of_education) #The error rate
```

```
## [1] 0.4280958
```


### Poisson Regression



```r
mglm1a <-glm(formula = willing_to_pay_next_level_of_education ~ . -p1 , data = R_testu_reg,
             family = poisson)
summary(mglm1a)
```

```
## 
## Call:
## glm(formula = willing_to_pay_next_level_of_education ~ . - p1, 
##     family = poisson, data = R_testu_reg)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -695.73  -219.15   -50.71   103.62  1201.44  
## 
## Coefficients:
##                                                                                            Estimate
## (Intercept)                                                                               1.425e+01
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   1.981e-01
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals -4.943e-02
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              3.387e-02
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        6.187e-02
## Social_ClassSocial Class C1                                                              -2.662e-02
## Social_ClassSocial Class C2                                                              -1.820e-01
## age                                                                                      -3.262e-02
## region_of_originEastern                                                                  -4.588e-02
## region_of_originNorthern                                                                  4.432e-02
## region_of_originOther                                                                    -1.088e-01
## region_of_originWestern                                                                  -5.028e-02
## marital_statusMarried                                                                     1.974e-01
## marital_statusSingle                                                                      2.433e-02
## No_of_childrenI have 1 child                                                             -7.664e-02
## No_of_childrenI have 2 children                                                           2.574e-02
## No_of_childrenI have 3 children                                                          -2.338e-01
## No_people_living_withI live with 4-6 other people                                         7.732e-02
## No_people_living_withI live with 7 or more people                                        -3.625e-02
## No_people_living_withI live with one other person                                        -4.441e-02
## relationship_to_head_of_householdI am a relative of the head of household                 6.727e-02
## relationship_to_head_of_householdI am the daughter of the head of household               1.437e-01
## relationship_to_head_of_householdI am the head of the household                           3.795e-01
## relationship_to_head_of_householdI am the wife of the head of household                  -1.297e-03
## relationship_to_head_of_householdOther                                                    2.325e-01
## all_secondary_girls_schoolYes                                                            -6.728e-02
## career_next_level_of_educationDiploma                                                    -3.433e-01
## career_next_level_of_educationTechnical certificate                                      -6.993e-01
## career_readyExtremely ready                                                               6.922e-02
## career_readyNeutral                                                                       1.987e-01
## career_readyNot ready                                                                    -3.481e-02
## career_readySomewhat ready                                                                3.552e-02
##                                                                                          Std. Error
## (Intercept)                                                                               2.439e-03
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   8.132e-04
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  8.021e-04
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              8.067e-04
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        8.040e-04
## Social_ClassSocial Class C1                                                               1.624e-04
## Social_ClassSocial Class C2                                                               1.755e-04
## age                                                                                       4.195e-05
## region_of_originEastern                                                                   2.169e-04
## region_of_originNorthern                                                                  2.347e-04
## region_of_originOther                                                                     5.926e-04
## region_of_originWestern                                                                   1.742e-04
## marital_statusMarried                                                                     1.854e-03
## marital_statusSingle                                                                      1.715e-03
## No_of_childrenI have 1 child                                                              3.780e-04
## No_of_childrenI have 2 children                                                           6.255e-04
## No_of_childrenI have 3 children                                                           1.231e-03
## No_people_living_withI live with 4-6 other people                                         1.942e-04
## No_people_living_withI live with 7 or more people                                         2.163e-04
## No_people_living_withI live with one other person                                         2.784e-04
## relationship_to_head_of_householdI am a relative of the head of household                 9.322e-04
## relationship_to_head_of_householdI am the daughter of the head of household               9.272e-04
## relationship_to_head_of_householdI am the head of the household                           1.015e-03
## relationship_to_head_of_householdI am the wife of the head of household                   1.251e-03
## relationship_to_head_of_householdOther                                                    1.010e-03
## all_secondary_girls_schoolYes                                                             2.489e-04
## career_next_level_of_educationDiploma                                                     1.685e-04
## career_next_level_of_educationTechnical certificate                                       6.074e-04
## career_readyExtremely ready                                                               4.849e-04
## career_readyNeutral                                                                       2.886e-04
## career_readyNot ready                                                                     1.841e-04
## career_readySomewhat ready                                                                1.843e-04
##                                                                                            z value
## (Intercept)                                                                               5844.358
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    243.571
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   -61.634
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                41.980
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          76.957
## Social_ClassSocial Class C1                                                               -163.954
## Social_ClassSocial Class C2                                                              -1036.795
## age                                                                                       -777.561
## region_of_originEastern                                                                   -211.543
## region_of_originNorthern                                                                   188.881
## region_of_originOther                                                                     -183.553
## region_of_originWestern                                                                   -288.636
## marital_statusMarried                                                                      106.484
## marital_statusSingle                                                                        14.186
## No_of_childrenI have 1 child                                                              -202.730
## No_of_childrenI have 2 children                                                             41.154
## No_of_childrenI have 3 children                                                           -189.932
## No_people_living_withI live with 4-6 other people                                          398.163
## No_people_living_withI live with 7 or more people                                         -167.563
## No_people_living_withI live with one other person                                         -159.496
## relationship_to_head_of_householdI am a relative of the head of household                   72.164
## relationship_to_head_of_householdI am the daughter of the head of household                155.017
## relationship_to_head_of_householdI am the head of the household                            373.720
## relationship_to_head_of_householdI am the wife of the head of household                     -1.037
## relationship_to_head_of_householdOther                                                     230.233
## all_secondary_girls_schoolYes                                                             -270.379
## career_next_level_of_educationDiploma                                                    -2037.871
## career_next_level_of_educationTechnical certificate                                      -1151.287
## career_readyExtremely ready                                                                142.765
## career_readyNeutral                                                                        688.342
## career_readyNot ready                                                                     -189.106
## career_readySomewhat ready                                                                 192.803
##                                                                                          Pr(>|z|)
## (Intercept)                                                                                <2e-16
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    <2e-16
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   <2e-16
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               <2e-16
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         <2e-16
## Social_ClassSocial Class C1                                                                <2e-16
## Social_ClassSocial Class C2                                                                <2e-16
## age                                                                                        <2e-16
## region_of_originEastern                                                                    <2e-16
## region_of_originNorthern                                                                   <2e-16
## region_of_originOther                                                                      <2e-16
## region_of_originWestern                                                                    <2e-16
## marital_statusMarried                                                                      <2e-16
## marital_statusSingle                                                                       <2e-16
## No_of_childrenI have 1 child                                                               <2e-16
## No_of_childrenI have 2 children                                                            <2e-16
## No_of_childrenI have 3 children                                                            <2e-16
## No_people_living_withI live with 4-6 other people                                          <2e-16
## No_people_living_withI live with 7 or more people                                          <2e-16
## No_people_living_withI live with one other person                                          <2e-16
## relationship_to_head_of_householdI am a relative of the head of household                  <2e-16
## relationship_to_head_of_householdI am the daughter of the head of household                <2e-16
## relationship_to_head_of_householdI am the head of the household                            <2e-16
## relationship_to_head_of_householdI am the wife of the head of household                       0.3
## relationship_to_head_of_householdOther                                                     <2e-16
## all_secondary_girls_schoolYes                                                              <2e-16
## career_next_level_of_educationDiploma                                                      <2e-16
## career_next_level_of_educationTechnical certificate                                        <2e-16
## career_readyExtremely ready                                                                <2e-16
## career_readyNeutral                                                                        <2e-16
## career_readyNot ready                                                                      <2e-16
## career_readySomewhat ready                                                                 <2e-16
##                                                                                             
## (Intercept)                                                                              ***
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal  ***
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals ***
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes             ***
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes       ***
## Social_ClassSocial Class C1                                                              ***
## Social_ClassSocial Class C2                                                              ***
## age                                                                                      ***
## region_of_originEastern                                                                  ***
## region_of_originNorthern                                                                 ***
## region_of_originOther                                                                    ***
## region_of_originWestern                                                                  ***
## marital_statusMarried                                                                    ***
## marital_statusSingle                                                                     ***
## No_of_childrenI have 1 child                                                             ***
## No_of_childrenI have 2 children                                                          ***
## No_of_childrenI have 3 children                                                          ***
## No_people_living_withI live with 4-6 other people                                        ***
## No_people_living_withI live with 7 or more people                                        ***
## No_people_living_withI live with one other person                                        ***
## relationship_to_head_of_householdI am a relative of the head of household                ***
## relationship_to_head_of_householdI am the daughter of the head of household              ***
## relationship_to_head_of_householdI am the head of the household                          ***
## relationship_to_head_of_householdI am the wife of the head of household                     
## relationship_to_head_of_householdOther                                                   ***
## all_secondary_girls_schoolYes                                                            ***
## career_next_level_of_educationDiploma                                                    ***
## career_next_level_of_educationTechnical certificate                                      ***
## career_readyExtremely ready                                                              ***
## career_readyNeutral                                                                      ***
## career_readyNot ready                                                                    ***
## career_readySomewhat ready                                                               ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for poisson family taken to be 1)
## 
##     Null deviance: 43354926  on 298  degrees of freedom
## Residual deviance: 29168511  on 267  degrees of freedom
## AIC: 29173152
## 
## Number of Fisher Scoring iterations: 4
```

If the Residual Deviance is greater than the degrees of freedom, then over-dispersion exists. This means that the estimates are correct, but the standard errors (standard deviation) are wrong and unaccounted for by the model.

To have a more correct standard error we can use a quasi-poisson model. It uses the mean regression function and the variance function from the Poisson GLM but leaves the dispersion parameter unrestricted. Thus, dispersion parameter is not assumed to be fixed at 1 but is estimated from the data.

This leads to the same coefficient estimates as the standard Poisson model but inference is adjusted for over-dispersion.


### QuasiPoisson Regression

The model leads to an estimated dispersion parameter  which is clearly larger than 1, confirming that over-dispersion is present in the data.


```r
mglm1b <-glm(formula = willing_to_pay_next_level_of_education ~ . -p1 , data = R_testu_reg,
             family = quasipoisson)
summary(mglm1b)
```

```
## 
## Call:
## glm(formula = willing_to_pay_next_level_of_education ~ . - p1, 
##     family = quasipoisson, data = R_testu_reg)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -695.73  -219.15   -50.71   103.62  1201.44  
## 
## Coefficients:
##                                                                                           Estimate
## (Intercept)                                                                              14.253300
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   0.198076
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals -0.049433
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              0.033865
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        0.061874
## Social_ClassSocial Class C1                                                              -0.026623
## Social_ClassSocial Class C2                                                              -0.181992
## age                                                                                      -0.032617
## region_of_originEastern                                                                  -0.045878
## region_of_originNorthern                                                                  0.044322
## region_of_originOther                                                                    -0.108771
## region_of_originWestern                                                                  -0.050284
## marital_statusMarried                                                                     0.197394
## marital_statusSingle                                                                      0.024330
## No_of_childrenI have 1 child                                                             -0.076639
## No_of_childrenI have 2 children                                                           0.025742
## No_of_childrenI have 3 children                                                          -0.233790
## No_people_living_withI live with 4-6 other people                                         0.077320
## No_people_living_withI live with 7 or more people                                        -0.036247
## No_people_living_withI live with one other person                                        -0.044407
## relationship_to_head_of_householdI am a relative of the head of household                 0.067270
## relationship_to_head_of_householdI am the daughter of the head of household               0.143728
## relationship_to_head_of_householdI am the head of the household                           0.379512
## relationship_to_head_of_householdI am the wife of the head of household                  -0.001297
## relationship_to_head_of_householdOther                                                    0.232489
## all_secondary_girls_schoolYes                                                            -0.067283
## career_next_level_of_educationDiploma                                                    -0.343300
## career_next_level_of_educationTechnical certificate                                      -0.699253
## career_readyExtremely ready                                                               0.069225
## career_readyNeutral                                                                       0.198686
## career_readyNot ready                                                                    -0.034814
## career_readySomewhat ready                                                                0.035524
##                                                                                          Std. Error
## (Intercept)                                                                                0.854244
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.284846
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   0.280935
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.282561
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.281620
## Social_ClassSocial Class C1                                                                0.056878
## Social_ClassSocial Class C2                                                                0.061484
## age                                                                                        0.014693
## region_of_originEastern                                                                    0.075964
## region_of_originNorthern                                                                   0.082193
## region_of_originOther                                                                      0.207566
## region_of_originWestern                                                                    0.061021
## marital_statusMarried                                                                      0.649310
## marital_statusSingle                                                                       0.600769
## No_of_childrenI have 1 child                                                               0.132415
## No_of_childrenI have 2 children                                                            0.219094
## No_of_childrenI have 3 children                                                            0.431154
## No_people_living_withI live with 4-6 other people                                          0.068020
## No_people_living_withI live with 7 or more people                                          0.075769
## No_people_living_withI live with one other person                                          0.097523
## relationship_to_head_of_householdI am a relative of the head of household                  0.326515
## relationship_to_head_of_householdI am the daughter of the head of household                0.324761
## relationship_to_head_of_householdI am the head of the household                            0.355698
## relationship_to_head_of_householdI am the wife of the head of household                    0.438106
## relationship_to_head_of_householdOther                                                     0.353702
## all_secondary_girls_schoolYes                                                              0.087164
## career_next_level_of_educationDiploma                                                      0.059007
## career_next_level_of_educationTechnical certificate                                        0.212742
## career_readyExtremely ready                                                                0.169841
## career_readyNeutral                                                                        0.101103
## career_readyNot ready                                                                      0.064483
## career_readySomewhat ready                                                                 0.064538
##                                                                                          t value
## (Intercept)                                                                               16.685
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.695
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  -0.176
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.120
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.220
## Social_ClassSocial Class C1                                                               -0.468
## Social_ClassSocial Class C2                                                               -2.960
## age                                                                                       -2.220
## region_of_originEastern                                                                   -0.604
## region_of_originNorthern                                                                   0.539
## region_of_originOther                                                                     -0.524
## region_of_originWestern                                                                   -0.824
## marital_statusMarried                                                                      0.304
## marital_statusSingle                                                                       0.040
## No_of_childrenI have 1 child                                                              -0.579
## No_of_childrenI have 2 children                                                            0.117
## No_of_childrenI have 3 children                                                           -0.542
## No_people_living_withI live with 4-6 other people                                          1.137
## No_people_living_withI live with 7 or more people                                         -0.478
## No_people_living_withI live with one other person                                         -0.455
## relationship_to_head_of_householdI am a relative of the head of household                  0.206
## relationship_to_head_of_householdI am the daughter of the head of household                0.443
## relationship_to_head_of_householdI am the head of the household                            1.067
## relationship_to_head_of_householdI am the wife of the head of household                   -0.003
## relationship_to_head_of_householdOther                                                     0.657
## all_secondary_girls_schoolYes                                                             -0.772
## career_next_level_of_educationDiploma                                                     -5.818
## career_next_level_of_educationTechnical certificate                                       -3.287
## career_readyExtremely ready                                                                0.408
## career_readyNeutral                                                                        1.965
## career_readyNot ready                                                                     -0.540
## career_readySomewhat ready                                                                 0.550
##                                                                                          Pr(>|t|)
## (Intercept)                                                                               < 2e-16
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   0.48742
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  0.86046
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              0.90469
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        0.82627
## Social_ClassSocial Class C1                                                               0.64011
## Social_ClassSocial Class C2                                                               0.00335
## age                                                                                       0.02727
## region_of_originEastern                                                                   0.54639
## region_of_originNorthern                                                                  0.59017
## region_of_originOther                                                                     0.60069
## region_of_originWestern                                                                   0.41065
## marital_statusMarried                                                                     0.76136
## marital_statusSingle                                                                      0.96773
## No_of_childrenI have 1 child                                                              0.56322
## No_of_childrenI have 2 children                                                           0.90656
## No_of_childrenI have 3 children                                                           0.58810
## No_people_living_withI live with 4-6 other people                                         0.25667
## No_people_living_withI live with 7 or more people                                         0.63277
## No_people_living_withI live with one other person                                         0.64923
## relationship_to_head_of_householdI am a relative of the head of household                 0.83693
## relationship_to_head_of_householdI am the daughter of the head of household               0.65844
## relationship_to_head_of_householdI am the head of the household                           0.28696
## relationship_to_head_of_householdI am the wife of the head of household                   0.99764
## relationship_to_head_of_householdOther                                                    0.51155
## all_secondary_girls_schoolYes                                                             0.44085
## career_next_level_of_educationDiploma                                                     1.7e-08
## career_next_level_of_educationTechnical certificate                                       0.00115
## career_readyExtremely ready                                                               0.68390
## career_readyNeutral                                                                       0.05043
## career_readyNot ready                                                                     0.58973
## career_readySomewhat ready                                                                0.58248
##                                                                                             
## (Intercept)                                                                              ***
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal     
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals    
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          
## Social_ClassSocial Class C1                                                                 
## Social_ClassSocial Class C2                                                              ** 
## age                                                                                      *  
## region_of_originEastern                                                                     
## region_of_originNorthern                                                                    
## region_of_originOther                                                                       
## region_of_originWestern                                                                     
## marital_statusMarried                                                                       
## marital_statusSingle                                                                        
## No_of_childrenI have 1 child                                                                
## No_of_childrenI have 2 children                                                             
## No_of_childrenI have 3 children                                                             
## No_people_living_withI live with 4-6 other people                                           
## No_people_living_withI live with 7 or more people                                           
## No_people_living_withI live with one other person                                           
## relationship_to_head_of_householdI am a relative of the head of household                   
## relationship_to_head_of_householdI am the daughter of the head of household                 
## relationship_to_head_of_householdI am the head of the household                             
## relationship_to_head_of_householdI am the wife of the head of household                     
## relationship_to_head_of_householdOther                                                      
## all_secondary_girls_schoolYes                                                               
## career_next_level_of_educationDiploma                                                    ***
## career_next_level_of_educationTechnical certificate                                      ** 
## career_readyExtremely ready                                                                 
## career_readyNeutral                                                                      .  
## career_readyNot ready                                                                       
## career_readySomewhat ready                                                                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 122689.2)
## 
##     Null deviance: 43354926  on 298  degrees of freedom
## Residual deviance: 29168511  on 267  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 4
```

### Negative binomial Model

If Theta is not known but to be estimated from the data, the negative binomial model is not a
special case of the general GLM—however, an ML fit can easily be computed re-using GLM
methodology by iterating estimation of Beta given Theta and vice versa. This leads to ML estimates
for both Beta and Theta which can be computed.


```r
mglm1c <-glm.nb(formula = willing_to_pay_next_level_of_education ~ . -p1 , data = R_testu_reg)
summary(mglm1c)
```

```
## 
## Call:
## glm.nb(formula = willing_to_pay_next_level_of_education ~ . - 
##     p1, data = R_testu_reg, init.theta = 8.86067865, link = log)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3923  -0.7952  -0.1910   0.3485   3.4826  
## 
## Coefficients:
##                                                                                           Estimate
## (Intercept)                                                                              13.969744
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal   0.182725
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals -0.032926
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes              0.023730
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes        0.059441
## Social_ClassSocial Class C1                                                              -0.012511
## Social_ClassSocial Class C2                                                              -0.173020
## age                                                                                      -0.024564
## region_of_originEastern                                                                  -0.028438
## region_of_originNorthern                                                                  0.061594
## region_of_originOther                                                                    -0.031822
## region_of_originWestern                                                                  -0.057319
## marital_statusMarried                                                                     0.239986
## marital_statusSingle                                                                      0.085346
## No_of_childrenI have 1 child                                                             -0.096653
## No_of_childrenI have 2 children                                                          -0.036659
## No_of_childrenI have 3 children                                                          -0.267008
## No_people_living_withI live with 4-6 other people                                         0.109697
## No_people_living_withI live with 7 or more people                                         0.000236
## No_people_living_withI live with one other person                                        -0.032857
## relationship_to_head_of_householdI am a relative of the head of household                 0.106074
## relationship_to_head_of_householdI am the daughter of the head of household               0.166048
## relationship_to_head_of_householdI am the head of the household                           0.403008
## relationship_to_head_of_householdI am the wife of the head of household                   0.035872
## relationship_to_head_of_householdOther                                                    0.279652
## all_secondary_girls_schoolYes                                                            -0.067566
## career_next_level_of_educationDiploma                                                    -0.347559
## career_next_level_of_educationTechnical certificate                                      -0.655095
## career_readyExtremely ready                                                               0.086400
## career_readyNeutral                                                                       0.172664
## career_readyNot ready                                                                    -0.025545
## career_readySomewhat ready                                                                0.018214
##                                                                                          Std. Error
## (Intercept)                                                                                0.601377
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.211977
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals   0.207954
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.208728
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.208952
## Social_ClassSocial Class C1                                                                0.049158
## Social_ClassSocial Class C2                                                                0.051165
## age                                                                                        0.011826
## region_of_originEastern                                                                    0.061787
## region_of_originNorthern                                                                   0.072446
## region_of_originOther                                                                      0.162045
## region_of_originWestern                                                                    0.052079
## marital_statusMarried                                                                      0.422189
## marital_statusSingle                                                                       0.372699
## No_of_childrenI have 1 child                                                               0.101021
## No_of_childrenI have 2 children                                                            0.165431
## No_of_childrenI have 3 children                                                            0.277722
## No_people_living_withI live with 4-6 other people                                          0.058282
## No_people_living_withI live with 7 or more people                                          0.064089
## No_people_living_withI live with one other person                                          0.079222
## relationship_to_head_of_householdI am a relative of the head of household                  0.253446
## relationship_to_head_of_householdI am the daughter of the head of household                0.252352
## relationship_to_head_of_householdI am the head of the household                            0.275679
## relationship_to_head_of_householdI am the wife of the head of household                    0.333266
## relationship_to_head_of_householdOther                                                     0.279427
## all_secondary_girls_schoolYes                                                              0.067960
## career_next_level_of_educationDiploma                                                      0.049255
## career_next_level_of_educationTechnical certificate                                        0.140610
## career_readyExtremely ready                                                                0.142942
## career_readyNeutral                                                                        0.089800
## career_readyNot ready                                                                      0.054107
## career_readySomewhat ready                                                                 0.055022
##                                                                                          z value
## (Intercept)                                                                               23.230
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal    0.862
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals  -0.158
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes               0.114
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes         0.284
## Social_ClassSocial Class C1                                                               -0.255
## Social_ClassSocial Class C2                                                               -3.382
## age                                                                                       -2.077
## region_of_originEastern                                                                   -0.460
## region_of_originNorthern                                                                   0.850
## region_of_originOther                                                                     -0.196
## region_of_originWestern                                                                   -1.101
## marital_statusMarried                                                                      0.568
## marital_statusSingle                                                                       0.229
## No_of_childrenI have 1 child                                                              -0.957
## No_of_childrenI have 2 children                                                           -0.222
## No_of_childrenI have 3 children                                                           -0.961
## No_people_living_withI live with 4-6 other people                                          1.882
## No_people_living_withI live with 7 or more people                                          0.004
## No_people_living_withI live with one other person                                         -0.415
## relationship_to_head_of_householdI am a relative of the head of household                  0.419
## relationship_to_head_of_householdI am the daughter of the head of household                0.658
## relationship_to_head_of_householdI am the head of the household                            1.462
## relationship_to_head_of_householdI am the wife of the head of household                    0.108
## relationship_to_head_of_householdOther                                                     1.001
## all_secondary_girls_schoolYes                                                             -0.994
## career_next_level_of_educationDiploma                                                     -7.056
## career_next_level_of_educationTechnical certificate                                       -4.659
## career_readyExtremely ready                                                                0.604
## career_readyNeutral                                                                        1.923
## career_readyNot ready                                                                     -0.472
## career_readySomewhat ready                                                                 0.331
##                                                                                          Pr(>|z|)
## (Intercept)                                                                               < 2e-16
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal  0.388684
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals 0.874193
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes             0.909486
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes       0.776047
## Social_ClassSocial Class C1                                                              0.799102
## Social_ClassSocial Class C2                                                              0.000721
## age                                                                                      0.037794
## region_of_originEastern                                                                  0.645332
## region_of_originNorthern                                                                 0.395215
## region_of_originOther                                                                    0.844316
## region_of_originWestern                                                                  0.271064
## marital_statusMarried                                                                    0.569741
## marital_statusSingle                                                                     0.818873
## No_of_childrenI have 1 child                                                             0.338684
## No_of_childrenI have 2 children                                                          0.824626
## No_of_childrenI have 3 children                                                          0.336342
## No_people_living_withI live with 4-6 other people                                        0.059812
## No_people_living_withI live with 7 or more people                                        0.997062
## No_people_living_withI live with one other person                                        0.678329
## relationship_to_head_of_householdI am a relative of the head of household                0.675562
## relationship_to_head_of_householdI am the daughter of the head of household              0.510537
## relationship_to_head_of_householdI am the head of the household                          0.143776
## relationship_to_head_of_householdI am the wife of the head of household                  0.914284
## relationship_to_head_of_householdOther                                                   0.316920
## all_secondary_girls_schoolYes                                                            0.320123
## career_next_level_of_educationDiploma                                                    1.71e-12
## career_next_level_of_educationTechnical certificate                                      3.18e-06
## career_readyExtremely ready                                                              0.545551
## career_readyNeutral                                                                      0.054509
## career_readyNot ready                                                                    0.636845
## career_readySomewhat ready                                                               0.740618
##                                                                                             
## (Intercept)                                                                              ***
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 2 principal     
## secondary_school_certificateUganda Advanced Certificate of Education (UACE) 3 principals    
## secondary_school_certificateUganda Certificate of Education (UCE) 3-5 passes                
## secondary_school_certificateUganda Certificate of Education (UCE) 5 or more passes          
## Social_ClassSocial Class C1                                                                 
## Social_ClassSocial Class C2                                                              ***
## age                                                                                      *  
## region_of_originEastern                                                                     
## region_of_originNorthern                                                                    
## region_of_originOther                                                                       
## region_of_originWestern                                                                     
## marital_statusMarried                                                                       
## marital_statusSingle                                                                        
## No_of_childrenI have 1 child                                                                
## No_of_childrenI have 2 children                                                             
## No_of_childrenI have 3 children                                                             
## No_people_living_withI live with 4-6 other people                                        .  
## No_people_living_withI live with 7 or more people                                           
## No_people_living_withI live with one other person                                           
## relationship_to_head_of_householdI am a relative of the head of household                   
## relationship_to_head_of_householdI am the daughter of the head of household                 
## relationship_to_head_of_householdI am the head of the household                             
## relationship_to_head_of_householdI am the wife of the head of household                     
## relationship_to_head_of_householdOther                                                      
## all_secondary_girls_schoolYes                                                               
## career_next_level_of_educationDiploma                                                    ***
## career_next_level_of_educationTechnical certificate                                      ***
## career_readyExtremely ready                                                                 
## career_readyNeutral                                                                      .  
## career_readyNot ready                                                                       
## career_readySomewhat ready                                                                  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for Negative Binomial(8.8607) family taken to be 1)
## 
##     Null deviance: 471.15  on 298  degrees of freedom
## Residual deviance: 304.62  on 267  degrees of freedom
## AIC: 8327
## 
## Number of Fisher Scoring iterations: 1
## 
## 
##               Theta:  8.861 
##           Std. Err.:  0.711 
## 
##  2 x log-likelihood:  -8261.043
```



```r
R_testu_reg$p2 <- round(predict(mglm1a, R_testu_reg, type = "response"),0)

R_testu_reg$p3 <- round(predict(mglm1b, R_testu_reg, type = "response"),0)

R_testu_reg$p4 <- round(predict(mglm1c, R_testu_reg, type = "response"),0)
```













