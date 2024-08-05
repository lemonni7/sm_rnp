---
title: "sm_rnp_f_u_length"
output: html_document
date: "2024-08-05"
---


```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(readxl)
```

## Import data

Import all follow-up length dataset and separate to 3 groups


```r
followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length)

followup1_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group == 1)

followup2_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group == 2)

followup3_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group == 3)
```

## Check distribution


```r
skimr::skim(followup_df)
```


Table: Data summary

|                         |            |
|:------------------------|:-----------|
|Name                     |followup_df |
|Number of rows           |73          |
|Number of columns        |2           |
|_______________________  |            |
|Column type frequency:   |            |
|numeric                  |2           |
|________________________ |            |
|Group variables          |None        |


**Variable type: numeric**

|skim_variable    | n_missing| complete_rate|    mean|      sd| p0| p25|  p50|  p75|  p100|hist  |
|:----------------|---------:|-------------:|-------:|-------:|--:|---:|----:|----:|-----:|:-----|
|group            |         0|             1|    3.00|    0.00|  3|   3|    3|    3|     3|▁▁▇▁▁ |
|follow_up_length |         0|             1| 2193.75| 1689.93|  0| 696| 2280| 3207| 10458|▇▇▁▁▁ |

```r
skimr::skim(followup1_df)
```


Table: Data summary

|                         |             |
|:------------------------|:------------|
|Name                     |followup1_df |
|Number of rows           |44           |
|Number of columns        |2            |
|_______________________  |             |
|Column type frequency:   |             |
|numeric                  |2            |
|________________________ |             |
|Group variables          |None         |


**Variable type: numeric**

|skim_variable    | n_missing| complete_rate|    mean|      sd| p0|    p25|  p50|     p75| p100|hist  |
|:----------------|---------:|-------------:|-------:|-------:|--:|------:|----:|-------:|----:|:-----|
|group            |         0|             1|    1.00|    0.00|  1|    1.0|    1|    1.00|    1|▁▁▇▁▁ |
|follow_up_length |         0|             1| 2959.02| 2118.51|  2| 1845.5| 2503| 3542.25| 8558|▃▇▂▁▁ |

```r
skimr::skim(followup2_df)
```


Table: Data summary

|                         |             |
|:------------------------|:------------|
|Name                     |followup2_df |
|Number of rows           |16           |
|Number of columns        |2            |
|_______________________  |             |
|Column type frequency:   |             |
|numeric                  |2            |
|________________________ |             |
|Group variables          |None         |


**Variable type: numeric**

|skim_variable    | n_missing| complete_rate|    mean|      sd| p0|     p25|  p50|     p75| p100|hist  |
|:----------------|---------:|-------------:|-------:|-------:|--:|-------:|----:|-------:|----:|:-----|
|group            |         0|             1|    2.00|    0.00|  2|    2.00|    2|    2.00|    2|▁▁▇▁▁ |
|follow_up_length |         0|             1| 2454.81| 1646.13|  2| 1165.75| 3127| 3710.75| 4709|▅▂▁▇▃ |

```r
skimr::skim(followup3_df)
```


Table: Data summary

|                         |             |
|:------------------------|:------------|
|Name                     |followup3_df |
|Number of rows           |73           |
|Number of columns        |2            |
|_______________________  |             |
|Column type frequency:   |             |
|numeric                  |2            |
|________________________ |             |
|Group variables          |None         |


**Variable type: numeric**

|skim_variable    | n_missing| complete_rate|    mean|      sd| p0| p25|  p50|  p75|  p100|hist  |
|:----------------|---------:|-------------:|-------:|-------:|--:|---:|----:|----:|-----:|:-----|
|group            |         0|             1|    3.00|    0.00|  3|   3|    3|    3|     3|▁▁▇▁▁ |
|follow_up_length |         0|             1| 2193.75| 1689.93|  0| 696| 2280| 3207| 10458|▇▇▁▁▁ |

## Perform Mann Whitney U test

Perform Mann Whitney U test between double positive and Sm-RNP positive group


```r
followup1_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group < 3)

result_dp_smrnp = wilcox.test(follow_up_length~group, followup1_df, exact = FALSE)
```

Perform Mann Whitney U test between double positive and RNP positive group


```r
followup2_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group != 2)

result_dp_rnp = wilcox.test(follow_up_length~group, followup2_df, exact = FALSE)
```

Perform Mann Whitney U test between Sm-RNP and RNP positive group


```r
followup3_df = followup_df = read_excel("./r-sm-rnp.xlsx") %>% 
  janitor::clean_names() %>% 
  select(group, follow_up_length) %>% 
  filter(group != 1)

result_smrnp_rnp = wilcox.test(follow_up_length~group, followup3_df, exact = FALSE)
```

# Follow Up Length Comparison Between Two Groups

Comparison between double positive and Sm-RNP only

```r
result_dp_smrnp
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  follow_up_length by group
## W = 357.5, p-value = 0.9334
## alternative hypothesis: true location shift is not equal to 0
```

Comparison between double positive and RNP only

```r
result_dp_rnp
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  follow_up_length by group
## W = 1891.5, p-value = 0.1088
## alternative hypothesis: true location shift is not equal to 0
```

Comparison between Sm-RNP and RNP only

```r
result_smrnp_rnp
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  follow_up_length by group
## W = 687.5, p-value = 0.2711
## alternative hypothesis: true location shift is not equal to 0
```

