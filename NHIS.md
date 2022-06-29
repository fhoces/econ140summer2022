---
title: "Mastering Metrics Example - NHIS Interviews"
author: "Ralf Becker"
date: "25 August 2018"
output: 
    html_document:
        keep_md: yes
    
---



# Introduction

Here we will replicate some of the results in 


# Acknowledgement

First and foremost we thank Joshua D. Angrist and J?rn-Steffen Pischke for writing Mastering Metrics and indeed for making data and STATA code for some of the analysis that appears in their book available via the [http://masteringmetrics.com/](book's website). 

The second thanks goes to Jeffrey Arnold who replicated the STATA work in R and therefore made the production of this much easier. Go to [https://jrnold.github.io/masteringmetrics/index.html](his github page) to see his original work.


# Import Data

The data have been provided on the [http://masteringmetrics.com/wp-content/uploads/2015/01/Data.zip](book's website as a STATA file). They come with ".dta" extensions. To import such a file into R we can use the `haven` package (remember, this needs installing first) which contains the `read_dta` function.


```r
library(haven)
library(tidyverse)


# set the working directory, I assume that the datafile is saved in here
nhis_data <- read_dta("NHIS2009_clean.dta")
```

When you look at the data (`head(nhis_data)`) they will look, in the first instance, like a big spreadsheet. and in a way they are as the dataset contains 40 variables and more than 80K observations. But really there is a lot more information hidden in the data. In particular there is information about the 40 variables that goes beyond their name. This information is "hidden" in the variables' attributes. For instance:


```r
attributes(nhis_data$uninsured)
```

```
## $label
## [1] "Health Insurance coverage status"
## 
## $format.stata
## [1] "%23.0g"
## 
## $class
## [1] "haven_labelled" "vctrs_vctr"     "double"        
## 
## $labels
##                     NIU             Not covered                 Covered 
##                       0                       1                       2 
##         Unknown-refused Unknown-not ascertained      Unknown-don't know 
##                       7                       8                       9
```

```r
table(nhis_data$uninsured)
```

```
## 
##     1     2 
## 14037 66597
```

This reveals that, while there are 6 possible outcomes, the datafile only contains responses "Not covered" and "Covered" (In the attribute `labels` you can find which numerical response has what meaning). You can also see that under the attribute `label` you can find short descriptions of the variable that could be very helpful. The following code extracts this for all 40 variables and displays it next to the variable name. I needed to google ("R extract an attribute from all variables") to fins out how to do that. Don't worry too much about the code.


```r
library(purrr)
n <- ncol(nhis_data)
labels_list <- map(1:n, function(x) attr(nhis_data[[x]], "label") )
cbind(names(nhis_data),labels_list)
```

```
##                        labels_list                                      
##  [1,] "year"           "(mean) year"                                    
##  [2,] "inc1"           NULL                                             
##  [3,] "inc2"           NULL                                             
##  [4,] "inc3"           NULL                                             
##  [5,] "inc4"           NULL                                             
##  [6,] "inc5"           NULL                                             
##  [7,] "inc6"           NULL                                             
##  [8,] "inc7"           NULL                                             
##  [9,] "inc8"           NULL                                             
## [10,] "serial"         "Sequential Serial Number, Household Record"     
## [11,] "hhweight"       "Household weight, final annual"                 
## [12,] "pernum"         "Person number within family (from reformatting)"
## [13,] "perweight"      "Final basic annual weight"                      
## [14,] "sampweight"     "Sample Person Weight"                           
## [15,] "age"            "Age"                                            
## [16,] "marstat"        "Legal marital status"                           
## [17,] "sex"            "Sex"                                            
## [18,] "famsize"        "Number of persons in family"                    
## [19,] "relate"         "Relationship to householder"                    
## [20,] "racenew"        "Self-reported Race (Post-1997 OMB standards)"   
## [21,] "educ"           "Educational attainment"                         
## [22,] "educrec1"       "Educational attainment recode, nonintervalled"  
## [23,] "empstat"        "Employment status in past 1 to 2 weeks"         
## [24,] "incfam07on"     "Total combined family income (2007+)"           
## [25,] "health"         "Health status"                                  
## [26,] "uninsured"      "Health Insurance coverage status"               
## [27,] "age2"           NULL                                             
## [28,] "fml"            NULL                                             
## [29,] "nwhite"         NULL                                             
## [30,] "hi"             NULL                                             
## [31,] "yedu"           NULL                                             
## [32,] "empl"           NULL                                             
## [33,] "hlth"           NULL                                             
## [34,] "inc"            NULL                                             
## [35,] "incmp"          NULL                                             
## [36,] "brooks"         NULL                                             
## [37,] "marradult"      NULL                                             
## [38,] "marradult_empl" NULL                                             
## [39,] "adltempl"       NULL                                             
## [40,] "hi_hsb1"        NULL
```

Unfortunately there is not a description (`label`) for every variable.

Another variable which is used below is `hs_hsb1`. "hsb" stands for health seeking bahaviour. The NHIS contains a range of variables that describe bahaviours (check out this [https://nhis.ipums.org/nhis-action/variables/group](page) on which you can explore the available variables - click on "PERSON" and the "HEALTH BAHAVIORS"). The huge range of behaviours have been summarised to a variable called `hs_hsb1` which takes the value on if the respondednt displays good bahaviour overall and 0 otherwise. Note that I have detail on this classification hs been achieved.

Also note that `serial` identifies the household. All observations with the same `serial` number come from the same household. The variable `fml` takes the value 1 if an observation is from a female respondent.

The variable `adltempl` indicates how many adults are employed in the household.

Some important data-cleaning happened already (check the empirical notes to Table 1.1 in Mastering Metrics):

* The dataset only contains observations for adults who are married (to confirm check `marradult` and `marrstat`, both variables only have one outcome).

* The education variable has been constructed to represent approximate years of formal education (e.g. the median value of that variable is 16 years which represents a Backelor's degree).



# Some data cleaning

To proceed there is an important piece of information that ought to be taken into account. Surveys like the NHIS apply quite complicated sampling scheme such that the selection of people/families/households aren't representative for the population. Such surveys then also supply weight variables (here `perweight` and `hhweight`). Such weights then need to be applied in order to obtain representative summary statistics. In what follows we will use the `perweight` variable as we are looking at a person level analysis (see the respective [https://nhis.ipums.org/nhis/userNotes_weights.shtml](info) om the NHIS website for a more detailed discussion).

Just to make sense of this, let's look at the `perweight` of some survey responses.


```r
random_sel <- c(10513,18773,26083)

# some randomly selected values for perweight
nhis_data$perweight[random_sel]
```

```
## [1] 6745 2746 2827
```

```r
# summary stats for perweight
summary(nhis_data$perweight)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    1870    2853    3419    4165   31688
```

As it turns out there are some observations with 0 weights. The definition of the weights is such that the sum of the weights ,2.7566296\times 10^{8}, is meant to be equal to the total US population in 2009 (308 Million). The sum is somewhat smaller here as some observations have already been removed from the datafile.

In what follows we will remove the following observations:

* all unmarried respondents
* respondents with a value of 0 for `perweight`

We use piping and save the result in a new data structure, `NHIS_temp`.


```r
NHIS_temp <- nhis_data %>%
  filter(marradult == 1, perweight != 0) 
```

The `hi_hsb1` variable has many missing values. It appears as if typically only one HH member has been asked to answer these questions. Here we assign every household member the same value, namely the average value for `hi_hsb1`. Note, here it is importat to instruct the `mean` function that missing values need to be ignored when calculating the mean (`na.rm = TRUE`)

Talking about missing values, the last line of the following code eliminates observations which have missing health seeking behaviour (`hi_hsb`) or missing health insurance status (`hi`).


```r
NHIS_temp <- NHIS_temp %>%
  group_by(serial) %>%     # group observations by household
  mutate(hi_hsb = mean(hi_hsb1, na.rm = TRUE)) %>%  # create new variable hi_hsb
  filter(!is.na(hi_hsb), !is.na(hi))  # remove obs with NA for either hi or hi_hsb
```

We also want to restrict, for this particular analysis, the sample to households with married husband and wife. To achieve this we will generate an auxilliary (helper) variable, `female` to which we assign the number of females (recall by now we only have married individuals) in a household. Then we keep (`filter`) those observations for which this value takes a value of 1.


```r
NHIS_temp <- NHIS_temp %>%
  group_by(serial) %>%           # group observations by household
  mutate(female = sum(fml)) %>%  # create helper variable
  filter(female == 1) %>%        # only keep those with one female
  select(-female)                # delete helper variable as not needed any longer
```

We are now left with 29734 observations.

In the next step we restrict ourselves to adults with ages between 26 and 59 and households for which at least one adult is in employment (`adltempl >= 1`):


```r
NHIS_temp <- NHIS_temp %>%
  filter(between(age, 26, 59), adltempl >= 1)
```

Lastly, as per the discussion in Mastering Metrics' empirical notes to Table 1.1, we are restricted to using households with husbands AND wives and hence we only use households for which we have more than one observation:


```r
NHIS_temp <- NHIS_temp %>%
  group_by(serial) %>%
  filter(length(serial) > 1) %>%
  ungroup()
```

We are now left with 18790 observations which corresponds to the number of observations reported in Table 1.1 (add up the observations for the four groups reported in Table 1.1).

Let's see what the proportion of males and females are that have health insurance. In the following table we also report the sum of the `perweight` variable in each group. 


```r
NHIS_temp %>%
  group_by(fml) %>%
  # normalize person weights to match number of observations in each
  # group
  mutate(perweight = perweight / sum(perweight) * n()) %>%
  group_by(fml, hi) %>%
  summarise(n_wt = sum(perweight)) %>%
  group_by(fml) %>%
  mutate(prop = n_wt / sum(n_wt))
```

```
## `summarise()` has grouped output by 'fml'. You can override using the `.groups`
## argument.
```

```
## # A tibble: 4 × 4
## # Groups:   fml [2]
##     fml    hi  n_wt  prop
##   <dbl> <dbl> <dbl> <dbl>
## 1     0     0 1281. 0.136
## 2     0     1 8114. 0.864
## 3     1     0 1131. 0.120
## 4     1     1 8264. 0.880
```

Compare sample statistics of mean and women, with and without health insurance.


To start with we calculate the average age of the four groups (males and females with and without health insurance).


```r
NHIS_temp_diff <- NHIS_temp %>%
  group_by(fml, hi) %>%
  summarise(mean = mean(age, na.rm = TRUE)) 
```

```
## `summarise()` has grouped output by 'fml'. You can override using the `.groups`
## argument.
```


```r
knitr::kable(NHIS_temp_diff, digits = 3)
```



| fml| hi|   mean|
|---:|--:|------:|
|   0|  0| 41.270|
|   0|  1| 44.163|
|   1|  0| 39.520|
|   1|  1| 42.151|


When comparing these stats to Table 1.1 in Mastering Metrics you will be able to detect slight differences. For instance in the published table the average age for husbands who have some HI is 43.98, whereas here it is 44.163.

The reason for that is that the above calculations do not take into account the weighting of the variables but treat each observation with the same weight. Let's re-calculate the Table but now we will compute a mean that uses the variable `perweight` as a weighting variable using the `weighted.mean` function.


```r
NHIS_temp_diff <- NHIS_temp %>%
  group_by(fml, hi) %>%
  summarise(wmean = weighted.mean(age,perweight, na.rm = TRUE)) %>%
  spread(hi,wmean)   # this spreads the hi variable into columns
```

```
## `summarise()` has grouped output by 'fml'. You can override using the `.groups`
## argument.
```


```r
knitr::kable(NHIS_temp_diff, digits = 3)
```



| fml|      0|      1|
|---:|------:|------:|
|   0| 41.263| 43.977|
|   1| 39.617| 42.237|

The labelling is a bit unclear, so let's change that by taking the previous table and adding an additional pipe to change the column names.


```r
NHIS_temp_diff <- NHIS_temp_diff %>%
  setNames( c("female", "no insurance", "insurance") ) 
```


```r
knitr::kable(NHIS_temp_diff, digits = 3)
```



| female| no insurance| insurance|
|------:|------------:|---------:|
|      0|       41.263|    43.977|
|      1|       39.617|    42.237|

Now we get the correct mean (i.e. the mean you can find in Mastering Metrics Table 1.1) in the table.

Table 1.1 also calculates the differences between the two groups (insured and not insured) and then performs a hypothesis test to establish whether there are statisticaly significant differences.



```r
library(weights)

f1hi1 <- NHIS_temp %>%    # female and health insurance
          filter(fml == 1, hi == 1) %>%
          select(age,perweight)

f1hi0 <- NHIS_temp %>%    # female and health insurance
          filter(fml == 1, hi == 0) %>%
          select(age,perweight)

f0hi1 <- NHIS_temp %>%    # female and health insurance
          filter(fml == 0, hi == 1) %>%
          select(age,perweight)

f0hi0 <- NHIS_temp %>%    # female and health insurance
          filter(fml == 0, hi == 0) %>%
          select(age,perweight)


wttest <- wtd.t.test(f1hi1$age, f1hi0$age, f1hi1$perweight, f1hi0$perweight,bootse=TRUE)
wttest
```

```
## $test
## [1] "Two Sample Weighted T-Test (Welch)"
## 
## $coefficients
##    t.value         df    p.value 
##   10.53347 2046.23835    0.00000 
## 
## $additional
## Difference     Mean.x     Mean.y   Std. Err 
##  2.6194787 42.2369170 39.6174382  0.2486816
```


The variables we are interested in are defined in the following list:


```r
varlist <- c("hlth", "nwhite", "age", "yedu", "famsize", "empl", "inc")
```






```r
varlist <- c("hlth", "nwhite", "age", "yedu", "famsize", "empl", "inc")
NHIS_temp_diff <- NHIS_temp %>%
  # rlang::set_attrs with NULL removes attributes from columns.
  # this avoids a warning from gather about differing attributes
  map_dfc(~ rlang::set_attrs(.x, NULL)) %>%
  select(fml, hi, one_of(varlist)) %>%
  gather(variable, value, -fml, -hi) %>%
  group_by(fml, hi, variable) %>%
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE)) %>%
  gather(stat, value, -fml, -hi, -variable) %>%
  unite(stat_hi, stat, hi) %>%
  spread(stat_hi, value) %>%
  mutate(diff = mean_1 - mean_0)
```

```
## Warning: `set_attrs()` is deprecated as of rlang 0.3.0
## This warning is displayed once per session.
```

```
## `summarise()` has grouped output by 'fml', 'hi'. You can override using the
## `.groups` argument.
```


```r
knitr::kable(NHIS_temp_diff, digits = 3)
```



| fml|variable |    mean_0|     mean_1|      sd_0|      sd_1|      diff|
|---:|:--------|---------:|----------:|---------:|---------:|---------:|
|   0|age      |    41.270|     44.163|     8.402|     8.609|     2.893|
|   0|empl     |     0.852|      0.922|     0.355|     0.268|     0.070|
|   0|famsize  |     4.057|      3.551|     1.544|     1.318|    -0.506|
|   0|hlth     |     3.699|      3.977|     1.010|     0.934|     0.278|
|   0|inc      | 43636.023| 104002.438| 35689.909| 54815.081| 60366.415|
|   0|nwhite   |     0.188|      0.200|     0.391|     0.400|     0.011|
|   0|yedu     |    11.213|     14.132|     3.472|     2.681|     2.919|
|   1|age      |    39.520|     42.151|     8.261|     8.655|     2.631|
|   1|empl     |     0.541|      0.758|     0.498|     0.429|     0.216|
|   1|famsize  |     4.073|      3.553|     1.541|     1.321|    -0.520|
|   1|hlth     |     3.610|      3.992|     1.021|     0.928|     0.382|
|   1|inc      | 43641.387| 103363.629| 35158.894| 55058.644| 59722.242|
|   1|nwhite   |     0.183|      0.202|     0.387|     0.401|     0.018|
|   1|yedu     |    11.359|     14.273|     3.500|     2.600|     2.913|

