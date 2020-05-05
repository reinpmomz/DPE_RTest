---
title: "DPE_RTest"
author: "Reinp"
date: "2020-05-05"
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
#echo=FALSE indicates that the code will not be shown in the final document 
#(though any results/output would still be displayed).
#include=FALSE to have the chunk evaluated, but neither the code nor its output displayed
# warning=FALSE and message=FALSE suppress any R warnings or messages from being included 
#in the final document
```

## loading Relevant packages and Data Set

```r
library(plyr)
library(tidyverse)
## tidyverse includes readr, ggplot2, dplyr, forcats, tibble, tidyr, purrr, stringr
library(stats)
library(psych)


## Reading our dataset
setwd('E:/Documents/Reinp/GitHub Respositories/DPE_RTest')

R_test <- read_csv("R_test_dataset.csv")
View(R_test)
attach(R_test)
```

## Structure of the data

```r
head(R_test)
```

```
## # A tibble: 6 x 26
##   SbjNum Start_Time Q_2   Interviewer Ward     S2    S3    S4  S5_a Social_Class
##    <dbl> <time>     <chr>       <dbl> <chr> <dbl> <dbl> <dbl> <dbl>        <dbl>
## 1 5.82e7 14:44:25   3-Fe~           5 Kiga~     2     3     1     1            3
## 2 5.83e7 12:19:37   6-Fe~           9 kawa~     2     4     1     1            2
## 3 5.83e7 09:31:38   7-Fe~           9 kasu~     2     3     1     1            3
## 4 5.84e7 14:26:47   9-Fe~           4 KISE~     2     7     2     1            4
## 5 5.84e7 11:30:36   9-Fe~          13 kagu~     2     7     2     1            4
## 6 5.82e7 14:39:05   4-Fe~           9 Masa~     2     7     2     1            2
## # ... with 16 more variables: A1_A <chr>, A1_B <chr>, A2 <dbl>, A5 <dbl>,
## #   Q_25_S <chr>, A8 <dbl>, A9 <dbl>, A10 <dbl>, A12 <dbl>, Q_32_S <chr>,
## #   B3 <chr>, B4 <dbl>, B8 <dbl>, B16 <dbl>, B17 <dbl>, C1 <dbl>
```

```r
tail(R_test)
```

```
## # A tibble: 6 x 26
##   SbjNum Start_Time Q_2   Interviewer Ward     S2    S3    S4  S5_a Social_Class
##    <dbl> <time>     <chr>       <dbl> <chr> <dbl> <dbl> <dbl> <dbl>        <dbl>
## 1 5.83e7 11:32:39   5-Fe~           6 Mutu~     2     3     1     1            3
## 2 5.83e7 13:53:00   7-Fe~           6 Nsam~     2     4     1     1            3
## 3 5.83e7 12:25:17   5-Fe~           4 NABU~     2     4     1     1            2
## 4 5.83e7 11:19:47   5-Fe~           9 kasu~     2     4     1     1            4
## 5 5.84e7 13:13:46   8-Fe~           5 kany~     2     6     2     1            4
## 6 5.85e7 15:39:41   9-Fe~           5 Make~     2     7     2     1            4
## # ... with 16 more variables: A1_A <chr>, A1_B <chr>, A2 <dbl>, A5 <dbl>,
## #   Q_25_S <chr>, A8 <dbl>, A9 <dbl>, A10 <dbl>, A12 <dbl>, Q_32_S <chr>,
## #   B3 <chr>, B4 <dbl>, B8 <dbl>, B16 <dbl>, B17 <dbl>, C1 <dbl>
```

```r
## How many variables and observations are there?
ncol(R_test)
```

```
## [1] 26
```

```r
nrow(R_test)
```

```
## [1] 321
```

```r
##learn more about the dataset
help(R_test)
```

```
## No documentation for 'R_test' in specified packages and libraries:
## you could try '??R_test'
```

```r
??R_test


str(R_test)
```

```
## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame':	321 obs. of  26 variables:
##  $ SbjNum      : num  58201936 58298317 58346228 58432078 58433597 ...
##  $ Start_Time  : 'hms' num  14:44:25 12:19:37 09:31:38 14:26:47 ...
##   ..- attr(*, "units")= chr "secs"
##  $ Q_2         : chr  "3-Feb-18" "6-Feb-18" "7-Feb-18" "9-Feb-18" ...
##  $ Interviewer : num  5 9 9 4 13 9 4 11 9 7 ...
##  $ Ward        : chr  "Kiganda zone" "kawaala" "kasubi" "KISENYI" ...
##  $ S2          : num  2 2 2 2 2 2 2 2 2 2 ...
##  $ S3          : num  3 4 3 7 7 7 4 4 4 7 ...
##  $ S4          : num  1 1 1 2 2 2 1 1 1 2 ...
##  $ S5_a        : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ Social_Class: num  3 2 3 4 4 2 2 4 2 4 ...
##  $ A1_A        : chr  "Nalubega" "Aisha" "Esther" "DESIRE" ...
##  $ A1_B        : chr  "Shifah" "Nalwagga" "Namatovu" "NABUKENYA" ...
##  $ A2          : num  20 21 20 19 19 21 19 19 19 19 ...
##  $ A5          : num  2 3 1 1 1 1 1 2 1 2 ...
##  $ Q_25_S      : chr  NA NA NA NA ...
##  $ A8          : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ A9          : num  1 1 1 1 1 2 1 1 1 1 ...
##  $ A10         : num  5 2 5 5 5 5 4 5 5 2 ...
##  $ A12         : num  3 1 4 3 3 3 3 3 3 4 ...
##  $ Q_32_S      : chr  NA NA NA NA ...
##  $ B3          : chr  "Vision High School" "Buhoobe secondary school" "Highway secondary school" "MARYLAND HIGH SCHOOL ENTEBBE" ...
##  $ B4          : num  2 2 2 2 1 2 2 2 2 1 ...
##  $ B8          : num  3 1 2 3 3 3 3 3 3 3 ...
##  $ B16         : num  1000000 800000 500000 800000 1500000 800000 900000 700000 1500000 700000 ...
##  $ B17         : num  4 2 2 5 2 2 4 4 2 5 ...
##  $ C1          : num  5 5 5 2 1 5 5 5 5 5 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   SbjNum = col_double(),
##   ..   Start_Time = col_time(format = ""),
##   ..   Q_2 = col_character(),
##   ..   Interviewer = col_double(),
##   ..   Ward = col_character(),
##   ..   S2 = col_double(),
##   ..   S3 = col_double(),
##   ..   S4 = col_double(),
##   ..   S5_a = col_double(),
##   ..   Social_Class = col_double(),
##   ..   A1_A = col_character(),
##   ..   A1_B = col_character(),
##   ..   A2 = col_double(),
##   ..   A5 = col_double(),
##   ..   Q_25_S = col_character(),
##   ..   A8 = col_double(),
##   ..   A9 = col_double(),
##   ..   A10 = col_double(),
##   ..   A12 = col_double(),
##   ..   Q_32_S = col_character(),
##   ..   B3 = col_character(),
##   ..   B4 = col_double(),
##   ..   B8 = col_double(),
##   ..   B16 = col_double(),
##   ..   B17 = col_double(),
##   ..   C1 = col_double()
##   .. )
```

```r
class(R_test)
```

```
## [1] "spec_tbl_df" "tbl_df"      "tbl"         "data.frame"
```

```r
typeof(R_test) 
```

```
## [1] "list"
```

```r
length(R_test) #number of variables
```

```
## [1] 26
```

```r
names(R_test) #display variable names
```

```
##  [1] "SbjNum"       "Start_Time"   "Q_2"          "Interviewer"  "Ward"        
##  [6] "S2"           "S3"           "S4"           "S5_a"         "Social_Class"
## [11] "A1_A"         "A1_B"         "A2"           "A5"           "Q_25_S"      
## [16] "A8"           "A9"           "A10"          "A12"          "Q_32_S"      
## [21] "B3"           "B4"           "B8"           "B16"          "B17"         
## [26] "C1"
```

```r
#attributes(R_test) names(R_test), class(R_test), row.names(R_test)
```


## Missing data

```r
complete.cases(R_test) ##print logical vector indicating complete rows
```

```
##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [97] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [109] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [133] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [145] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [157] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [169] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [181] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [193] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [205] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [217] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [229] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [241] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [253] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [265] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [277] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [289] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [301] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [313] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

```r
                       ##(i.e. rows without NA)

which(!complete.cases(R_test)) #print incomplete cases (rows with NA)
```

```
##   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
##  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
##  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
##  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
##  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
##  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
## [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
## [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
## [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
## [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
## [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
## [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
## [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
## [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
## [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
## [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
## [289] 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306
## [307] 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321
```

```r
#R_test[complete.cases(R_test), ] ## Keep only the complete rows

#data_complete <- R_test[complete.cases(R_test), ] ## Store the complete cases subset in a new
                                                   ##data frame

which(is.na(R_test)) #check for missing values
```

```
##   [1] 4495 4496 4497 4498 4499 4500 4501 4502 4503 4504 4505 4507 4509 4510 4511
##  [16] 4512 4513 4514 4515 4516 4517 4518 4519 4520 4521 4522 4523 4524 4525 4526
##  [31] 4527 4528 4529 4530 4531 4532 4533 4534 4535 4536 4537 4538 4539 4540 4541
##  [46] 4542 4543 4544 4545 4546 4547 4548 4549 4550 4551 4552 4553 4554 4555 4556
##  [61] 4557 4558 4559 4560 4561 4562 4563 4565 4566 4567 4568 4569 4570 4571 4572
##  [76] 4573 4574 4575 4576 4577 4578 4579 4580 4581 4582 4583 4584 4585 4586 4587
##  [91] 4588 4589 4590 4591 4592 4593 4595 4596 4597 4598 4599 4600 4601 4602 4603
## [106] 4604 4605 4606 4607 4608 4609 4610 4611 4612 4613 4614 4615 4616 4617 4618
## [121] 4619 4620 4621 4622 4623 4624 4625 4626 4627 4628 4629 4630 4631 4632 4633
## [136] 4634 4635 4636 4637 4638 4639 4640 4641 4642 4643 4644 4645 4646 4647 4648
## [151] 4649 4650 4651 4652 4653 4654 4655 4656 4657 4658 4659 4660 4661 4662 4663
## [166] 4664 4665 4666 4667 4668 4669 4670 4671 4672 4673 4674 4675 4676 4677 4678
## [181] 4680 4681 4682 4683 4684 4685 4686 4687 4688 4689 4690 4691 4692 4693 4694
## [196] 4695 4696 4697 4698 4699 4700 4701 4702 4703 4704 4705 4706 4707 4708 4709
## [211] 4710 4711 4712 4713 4714 4715 4716 4717 4718 4719 4720 4721 4722 4723 4724
## [226] 4725 4726 4727 4728 4729 4730 4731 4732 4733 4734 4735 4736 4737 4738 4739
## [241] 4740 4741 4742 4743 4744 4745 4746 4747 4748 4749 4750 4751 4752 4753 4754
## [256] 4755 4756 4757 4758 4759 4760 4761 4762 4763 4764 4765 4766 4767 4768 4769
## [271] 4770 4771 4772 4773 4774 4775 4776 4777 4778 4779 4780 4781 4782 4783 4784
## [286] 4785 4786 4787 4788 4789 4790 4791 4792 4793 4794 4795 4796 4797 4798 4799
## [301] 4800 4801 4802 4803 4804 4805 4806 4807 4808 4809 4810 4811 4812 4813 4814
## [316] 4815 5793 5840 5861 5872 5922 5926 5945 5954 5970 6000 6014 6019 6022 6024
## [331] 6044 6077 6078 6091 6100 6101 6102 6103 6104 6105 6106 6107 6108 6109 6110
## [346] 6111 6112 6113 6114 6115 6116 6117 6118 6119 6120 6122 6123 6124 6125 6126
## [361] 6127 6128 6129 6130 6131 6132 6133 6134 6135 6136 6137 6138 6139 6140 6141
## [376] 6142 6143 6144 6145 6146 6147 6148 6149 6150 6151 6152 6153 6154 6155 6156
## [391] 6157 6158 6159 6160 6161 6162 6163 6164 6165 6166 6167 6168 6169 6170 6171
## [406] 6172 6173 6174 6175 6176 6177 6178 6179 6180 6181 6182 6183 6184 6185 6187
## [421] 6188 6189 6190 6191 6192 6193 6194 6195 6196 6198 6199 6200 6201 6202 6203
## [436] 6204 6205 6206 6207 6208 6209 6210 6211 6212 6213 6214 6215 6216 6217 6218
## [451] 6219 6220 6221 6222 6223 6224 6225 6226 6227 6228 6229 6230 6231 6232 6233
## [466] 6234 6235 6236 6237 6238 6239 6240 6241 6242 6243 6244 6245 6246 6247 6248
## [481] 6249 6250 6251 6254 6255 6256 6257 6258 6259 6260 6261 6262 6263 6264 6265
## [496] 6266 6267 6268 6269 6270 6271 6272 6273 6274 6275 6276 6277 6278 6279 6280
## [511] 6281 6282 6283 6284 6285 6286 6287 6288 6289 6290 6291 6292 6293 6294 6295
## [526] 6296 6297 6298 6299 6300 6301 6302 6303 6304 6305 6306 6307 6308 6309 6311
## [541] 6312 6313 6314 6315 6316 6317 6318 6319 6320 6321 6322 6324 6326 6327 6328
## [556] 6329 6330 6331 6332 6333 6334 6335 6336 6337 6338 6339 6340 6341 6342 6343
## [571] 6344 6345 6346 6347 6348 6349 6350 6351 6352 6353 6354 6355 6356 6357 6358
## [586] 6359 6360 6361 6362 6363 6364 6365 6366 6367 6368 6369 6370 6371 6372 6373
## [601] 6374 6375 6376 6377 6378 6379 6380 6381 6382 6383 6384 6385 6386 6387 6388
## [616] 6389 6390 6391 6392 6393 6394 6395 6396 6397 6398 6399 6400 6401 6402 6403
## [631] 6404 6405 6406 6407 6408 6409 6410 6411 6412 6413 6414 6415 6416 6417 6418
## [646] 6419 6420
```

```r
sum(is.na(R_test))
```

```
## [1] 647
```

## Duplicated Cases

```r
duplicated(R_test$SbjNum)
```

```
##   [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [73] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [85] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
##  [97] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [109] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [121] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [133] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [145] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [157] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [169] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [181] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [193] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [205] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [217] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [229] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [241] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [253] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [265] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [277] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [289] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [301] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
## [313] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
```

```r
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
mean(duplicated(R_test$SbjNum)) #proportion of repeat cases
```

```
## [1] 0.01246106
```

```r
sum(duplicated(R_test$SbjNum))/nrow(R_test)
```

```
## [1] 0.01246106
```

## unique cases

```r
#unique(R_test$SbjNum)
#count(unique(R_test$SbjNum))
#table(unique(R_test$SbjNum))

## data with unique SbjNum

R_testu <- R_test[!duplicated(R_test$SbjNum),]
View(R_testu)
attach(R_testu)

nrow(R_testu)
```

```
## [1] 317
```

## Assigning value labels to variables

```r
R_testu$Interviewer <- factor(R_testu$Interviewer,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),
labels = c("Menya Abdmajid", "Bbale Denis", "Muwonge Allan Joshua", "Wambi Ken Paul", 
           "Wabwire Thomas", "Muhindo Wilfred", "Ahumuza Owen", "Mirembe Mary",
           "Arinitwe Mackline", "Alum Maria", "Kasule violet", "Aweko Monica", 
           "Nabbumba Pennina"))

R_testu$S2 <- factor(R_testu$S2,
levels = c(1,2,3,4),
labels = c("17 and below", "18-25", "25-30", "31 and above"))

R_testu$Social_Class <- factor(R_testu$Social_Class,
levels = c(1,2,3,4,5),
labels = c("Social Class D", "Social Class C2", "Social Class C1", "Social Class B", 
           "Social Class A"))

R_testu$B4 <- factor(R_testu$B4,
levels = c(1,2),
labels = c("Yes", "No"))

R_testu$B17 <- factor(R_testu$B17,
levels = c(1,2,3,4,5),
labels = c("Extremely ready", "Somewhat ready", "Neutral", "Not ready", "Definitely not ready"))

R_testu$C1 <- factor(R_testu$C1,
levels = c(1,2,3,4,5),
labels = c("I have been employed (for example in an office, restaurant,", 
           "I am self-employed (for example, on my own farm or family's", 
           "I have been employed as an unpaid intern", 
           "I have been engaged in household activities including agricu", 
           "I have not been employed"))
```


## Distribution of variables

```r
#DollarSign Syntax
table(R_testu$Interviewer)
```

```
## 
##       Menya Abdmajid          Bbale Denis Muwonge Allan Joshua 
##                    0                    0                   30 
##       Wambi Ken Paul       Wabwire Thomas      Muhindo Wilfred 
##                   34                   31                   30 
##         Ahumuza Owen         Mirembe Mary    Arinitwe Mackline 
##                   30                   35                   33 
##           Alum Maria        Kasule violet         Aweko Monica 
##                   30                   33                    0 
##     Nabbumba Pennina 
##                   31
```

```r
which(is.na(R_testu$Ward)) #check for missing values in ward
```

```
## integer(0)
```

```r
R_testu$Ward <- gsub("Bukoto Nsimbi ziwome", "Bukoto", R_testu$Ward)
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
R_testu$Ward <- gsub("Kinawataka", "Kinawataka", R_testu$Ward)
R_testu$Ward <- gsub("Kinawataka zone 1", "Kinawataka", R_testu$Ward)
R_testu$Ward <- gsub("KISASI DUNGU ZONE", "Dungu Zone", R_testu$Ward)
R_testu$Ward <- gsub("kisenyi", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("KISENYI", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("Kisenyi zone 3", "Kisenyi", R_testu$Ward)
R_testu$Ward <- gsub("kisira zone", "Kisira zone", R_testu$Ward)
R_testu$Ward <- gsub("kisugu", "Kisugu", R_testu$Ward)
R_testu$Ward <- gsub("Kisugu Upper Zone", "Kisugu", R_testu$Ward)

R_testu$Ward <- gsub("NAKULABYE", "Nakulabye", R_testu$Ward)
R_testu$Ward <- gsub("NALUKABYE", "Nakulabye", R_testu$Ward)
R_testu$Ward <- gsub("NANKULABYE", "Nakulabye", R_testu$Ward)

R_testu$Ward <- gsub("nsambya", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya  Barracks.", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya barracks", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya Barracks", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya kevina", "Nsambya", R_testu$Ward)
R_testu$Ward <- gsub("Nsambya Kevina", "Nsambya", R_testu$Ward)

R_testu$Ward <- gsub("wankulukuku", "Wankulukuku", R_testu$Ward)
R_testu$Ward <- gsub("WANKULUKUKU", "Wankulukuku", R_testu$Ward)
R_testu$Ward <- gsub("WUNKULUKUKU", "Wankulukuku", R_testu$Ward)

table(R_testu$Ward)
```

```
## 
##                   Boma zone          Buganda road flats 
##                           1                           2 
##                    BUGOLOBI                      Bukoto 
##                           1                           2 
##        Bukoto Nsimbi Ziwome                      BUSEGA 
##                           1                          11 
##                      BWAISE                  Dungu Zone 
##                           2                           7 
##                  Kabalagala                      Kabowa 
##                           1                          10 
##              Kafumbe Mukasa                    Kagugube 
##                           2                           6 
##                       Kagwo                KAMPALA ROAD 
##                           1                           1 
##                    Kamwokya                    Kansanga 
##                          11                          12 
##               Kanyanya zone       Kasirye zone/mpererwe 
##                           3                           1 
##                      Kasubi                 kasule zone 
##                          10                           1 
##                      KATAZA                     Kawaala 
##                           1                           6 
##                        kazo                Kiganda zone 
##                           2                           5 
##                  Kinawataka            Kinyarwanda zone 
##                           3                           1 
##         KIREKA MIWANDA ZONE                     kiruddu 
##                           1                           1 
##                     Kisenyi                 Kisira zone 
##                           9                           2 
##                      Kisugu                      KITEBI 
##                          18                           1 
##                      Kivulu                    kiwatule 
##                           1                           3 
##                    Kiwatule       kiwatule  kinyarwanda 
##                           4                           1 
##        kiwatule kinyarwanda                  Kizza zone 
##                           3                           1 
##                    KYEBANDO                Liganda Zone 
##                           9                           1 
##                      lugala                      Lugala 
##                           2                           1 
##               Lukalubo zone                      LUSAZE 
##                           1                           3 
##                      Luwafu         Luwafu kirundu zone 
##                           6                           1 
##                    MAKERERE        Makerere  University 
##                           7                           1 
##           Makerere Kagugube           Makerere Kakugube 
##                           1                           1 
##      makerere kakugube zone             Makerere kavule 
##                           1                           2 
##             MAKERERE KAVULE             Makerere kivulu 
##                           1                           1 
##             Makerere Kivulu             Makindyd Luwafu 
##                           3                           1 
##                    Makindye             makindye luwafu 
##                           1                           1 
##             Makindye luwafu             Makindye Luwafu 
##                           1                           1 
##     Makindye luwafu kirundu           Mamerere Kagugube 
##                           1                           1 
##                    masanafu                    Masanafu 
##                           4                           2 
##                    MASANAFU                       Mbuya 
##                          10                           3 
##                       MBUYA                     Mbuya 1 
##                           1                           2 
##                 Mbuya kaggo                 Mbuya Kaggo 
##                           1                           1 
##            Mbuya kinawataka                Mbuya zone 1 
##                           2                           1 
##                      Mulago               Mulago ward C 
##                           1                           1 
##                     Mutungo       Mutungo  Kampala road 
##                           6                           1 
##             Mutungo  Some 4               Mutungo Biina 
##                           1                           3 
##              Mutungo zone 4              Mutungo Zone 4 
##                           3                           2 
## Mutungo Zone 4 Kampala road                     Mutungu 
##                           1                           1 
##                  NABULAGALA              NABWERU ZONE 1 
##                           3                           1 
##               nabweru zone1                      NAGURU 
##                           1                           4 
##             NAGURU BARRACKS                    Nakasero 
##                           1                           1 
##                      Nakawa                   Nakulabye 
##                           4                           9 
##                   NAMUNGONA            Namungona Kisugu 
##                           5                           1 
##                  Namungoona                     NATEETE 
##                           2                           4 
##                      NDEEBA                     Nsambya 
##                           4                          12 
##                      Ntinda               Sebagala Zone 
##                           1                           1 
##              Sebaggala zone                   Tula zone 
##                           2                           1 
##                   Wandegeya                   WANDEGEYA 
##                           3                           3 
##        wandegeya.junju road                 Wankulukuku 
##                           1                           6 
##               Wheeling Zone 
##                           2
```

