# Applied-Multivariate-Data-Analysis
# 多變量報告
---
title: 'Steering Wheel of Fortune - Porto Seguro EDA'
date: '`r Sys.Date()`'
output:
  html_document:
    number_sections: true
    fig_caption: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: cosmo
    highlight: tango
    code_folding: hide
---

```{r setup, include=FALSE, echo=FALSE}
knitr::opts_chunk$set(echo=TRUE, error=FALSE)
```

# Introduction

This is an extensive Exploratory Data Analysis for the [Porto Seguro’s Safe Driver Prediction](https://www.kaggle.com/c/porto-seguro-safe-driver-prediction) competition within the R environment of the [tidyverse](http://tidyverse.org/) and [ggplot2](http://ggplot2.tidyverse.org/). We will visualise all the different data features, their relation to the *target* variable, explore multi-parameter interactions, and perform feature engineering.

The aim of this challenge is to predict the probability whether a driver will make an insurance claim, with the purpose of providing a fairer insurance cost on the basis of individual driving habits. It is sponsored by [Porto Seguro](https://www.portoseguro.com.br/) - a major car and home insurance company in Brazil

The [data](https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/data) comes in the traditional Kaggle form of one training and test file each: `../input/train.csv` & `../input/test.csv`. Each row corresponds to a specific policy holder and the columns describe their features. The target variable is conveniently named *target* here and it indicates whether this policy holder made an insurance claim in the past. 


# Preparations {.tabset .tabset-fade .tabset-pills}

## Load libraries

We load a range of libraries for general data wrangling and general visualisation together with more specialised tools.

```{r, message = FALSE}
# general visualisation
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('ggthemes') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation

# general data manipulation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('rlang') # data manipulation

# specific visualisation
library('alluvial') # visualisation
#library('ggfortify') # visualisation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('VIM') # NAs
library('plotly') # interactive
library('ggforce') # visualisation

# modelling
library('xgboost') # modelling
library('caret') # modelling
library('MLmetrics') # gini metric
```

## Helper functions

We use the *multiplot* function, courtesy of [R Cookbooks](http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/) to create multi-panel plots. We also make use of a brief helper function to compute binomial confidence intervals.

```{r}
# Define multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

```{r}
# function to extract binomial confidence levels
get_binCI <- function(x,n) as.list(setNames(binom.test(x,n)$conf.int, c("lwr", "upr")))
```

## Load data

We use *data.table's* fread function to speed up reading in the data, even though in this challenge our files are not very large with about 110 MB for *train* and 165 MB for *test*. Here we are taking into account the fact that missing values in the original data sets are indicated by `-1` or `-1.0` and turn those into "proper" NAs.

```{r warning=FALSE, results=FALSE}
train <- as.tibble(fread('../input/train.csv', na.strings=c("-1","-1.0")))
test <- as.tibble(fread('../input/test.csv', na.strings=c("-1","-1.0")))
sample_submit <- as.tibble(fread('../input/sample_submission.csv'))
```


# Overview: File structure and content {.tabset .tabset-fade .tabset-pills}

As a first step let's have an overview of the data sets using the *summary* and *glimpse* tools.

## Training data

```{r}
summary(train)
```

```{r}
glimpse(train)
```

We find:

- There are lots of features here. In total, our *training* data has 59 variables, including *id* and *target*. In some of them we already see a number of NAs.

- The [data description](https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/data) mentions that the names of the features indicate whether they are binary (*bin*) or categorical (*cat*) variables. Everything else is continuous or ordinal.

- We have already [been told](https://www.kaggle.com/c/porto-seguro-safe-driver-prediction/discussion/40222) by [Adriano Moala](https://www.kaggle.com/adrianomoala) that the names of the variables indicate certain properties: *Ind" is related to individual or driver, "reg" is related to region, "car" is related to car itself and "calc" is an calculated feature.' Here we will refer to these properties as groups.

- Note, that there is a *ps\_car\_11* as well as a *ps\_car\_11\_cat*. This is the only occasion where the numbering per group is neither consecutive nor unique. Probably a typo in the script that created the variable names.

- The features are anonymised, because that worked so well in [Mercedes](https://www.kaggle.com/c/mercedes-benz-greener-manufacturing). Just kidding. Mostly ;-)


## Test data:

```{r}
summary(test)
```


```{r}
glimpse(test)
```

We find:


## Missing values


```{r}
sum(is.na(train))
sum(is.na(test))
```

There are of the order of 1 million missing values per data set. This is not trivial.


## Reformating features

We will turn the categorical features into factors and the binary ones into logical values. For the *target* variable we choose a factor format.

```{r}
train <- train %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))
test <- test %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))
```


## Combining data frames

Here combine the *train* and *test* data frames for future homogeneous treatment.

```{r}
combine <- bind_rows(train %>% mutate(dset = "train"), 
                     test %>% mutate(dset = "test",
                                     target = NA))
combine <- combine %>% mutate(dset = factor(dset))
```


# Individual feature visualisations {.tabset .tabset-fade .tabset-pills}

We start our exploration with overview distribution plots for the various features. In order to make this visualisation more comprehensive, we will create layouts for the specific groups of features. For the sake of readability we divide each group into multiple parts.

These plots will be one of the pillars of our analysis. They might not be particularly exciting in themselves, but whenever we find an interesting effect in one of the variables we can come back here and examine their distribution. It's always an advantage to start with a clear view of the parameter space.


## Binary features part 1

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 1", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_ind_06_bin, fill = ps_ind_06_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_07_bin, fill = ps_ind_07_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_08_bin, fill = ps_ind_08_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_ind_09_bin, fill = ps_ind_09_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_ind_10_bin, fill = ps_ind_10_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_ind_11_bin, fill = ps_ind_11_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_ind_12_bin, fill = ps_ind_12_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_ind_13_bin, fill = ps_ind_13_bin)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout=layout)

```

We find that some of the binary features are very unbalanced; with "FALSE" accounting for the vast majority of cases. This is particularly true for the *ps\_ind* sequence from "10" to "13".


## Binary features part 2

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 2", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_ind_16_bin, fill = ps_ind_16_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_17_bin, fill = ps_ind_17_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_18_bin, fill = ps_ind_18_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_calc_15_bin, fill = ps_calc_15_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_calc_16_bin, fill = ps_calc_16_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_calc_17_bin, fill = ps_calc_17_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_calc_18_bin, fill = ps_calc_18_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_calc_19_bin, fill = ps_calc_19_bin)) +
  geom_bar() +
  theme(legend.position = "none")

p9 <- train %>%
  ggplot(aes(ps_calc_20_bin, fill = ps_calc_20_bin)) +
  geom_bar() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,9),2,5,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, layout=layout)

```

We find that in this particular set of binary features we have more of a balance between "TRUE" and "FALSE". For the three features *ps\_ind\_16\_bin*, *ps\_calc\_16\_bin*, and *ps\_calc\_17\_bin* we find that the "TRUE" values are in fact dominating.


## Categorical features part 1

Note the logarithmic y-axes:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 3", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_ind_02_cat, fill = ps_ind_02_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_ind_04_cat, fill = ps_ind_04_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_ind_05_cat, fill = ps_ind_05_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_01_cat, fill = ps_car_01_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_car_02_cat, fill = ps_car_02_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_car_03_cat, fill = ps_car_03_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)
```

We find that some categorical features have only very few levels, down to 2 levels (+ NA) for three of them. In others we have up to 11 levels, some of which are clearly dominating the (logarithmic) plots.


## Categorical features part 2

Note again the logarithmic y-axes:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 4", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_car_04_cat, fill = ps_car_04_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_car_05_cat, fill = ps_car_05_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_car_06_cat, fill = ps_car_06_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_07_cat, fill = ps_car_07_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_car_08_cat, fill = ps_car_08_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_car_09_cat, fill = ps_car_09_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_car_10_cat, fill = ps_car_10_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_car_11_cat, fill = ps_car_11_cat)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

layout <- matrix(c(1,1,2,3,4,4,5,5,6,6,7,7,8,8,8,8),4,4,byrow=TRUE)
multiplot(p1, p2, p4, p3, p5, p6, p7, p8, layout=layout)
```

We find that also here the number of levels is mostly low. Recall that the *car* features are related to the automobile itself. Feature "11" has lots of levels. This is the one number shared by a (supposedly) categorical and an integer feature. Maybe there has been a mixup in naming these features?


## Integer features part 1: "ind" and "car"

The integer features for the "ind" and "car" groups are best visualised in a categorical-style barplot, because their ranges are not very large. We are using log axes for some.

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 5", out.width="100%"}
p1 <- train %>%
  mutate(ps_ind_01 = as.factor(ps_ind_01)) %>%
  ggplot(aes(ps_ind_01, fill = ps_ind_01)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  mutate(ps_ind_03 = as.factor(ps_ind_03)) %>%
  ggplot(aes(ps_ind_03, fill = ps_ind_03)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  mutate(ps_ind_14 = as.factor(ps_ind_14)) %>%
  ggplot(aes(ps_ind_14, fill = ps_ind_14)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p4 <- train %>%
  mutate(ps_ind_15 = as.factor(ps_ind_15)) %>%
  ggplot(aes(ps_ind_15, fill = ps_ind_15)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  mutate(ps_car_11 = as.factor(ps_car_11)) %>%
  ggplot(aes(ps_car_11, fill = ps_car_11)) +
  geom_bar() +
  theme(legend.position = "none")


layout <- matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
```

We find that again there are large differences in frequencies, in particular for *ps\_ind\_14* and *ps\_car\_11* where "0" and "3" are the dominating values, respectively. 


## Integer features part 2: "calc"

Whereas most of the "calc" integer features can still be visualised best using barplots, for three of them a histogram is a better choice:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 6", out.width="100%"}
p1 <- train %>%
  mutate(ps_calc_04 = as.factor(ps_calc_04)) %>%
  ggplot(aes(ps_calc_04, fill = ps_calc_04)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- train %>%
  mutate(ps_calc_05 = as.factor(ps_calc_05)) %>%
  ggplot(aes(ps_calc_05, fill = ps_calc_05)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- train %>%
  mutate(ps_calc_06 = as.factor(ps_calc_06)) %>%
  ggplot(aes(ps_calc_06, fill = ps_calc_06)) +
  geom_bar() +
  theme(legend.position = "none")

p4 <- train %>%
  mutate(ps_calc_07 = as.factor(ps_calc_07)) %>%
  ggplot(aes(ps_calc_07, fill = ps_calc_07)) +
  geom_bar() +
  theme(legend.position = "none")

p5 <- train %>%
  mutate(ps_calc_08 = as.factor(ps_calc_08)) %>%
  ggplot(aes(ps_calc_08, fill = ps_calc_08)) +
  geom_bar() +
  theme(legend.position = "none")

p6 <- train %>%
  mutate(ps_calc_09 = as.factor(ps_calc_09)) %>%
  ggplot(aes(ps_calc_09, fill = ps_calc_09)) +
  geom_bar() +
  theme(legend.position = "none")

p7 <- train %>%
  ggplot(aes(ps_calc_10, fill = ps_calc_10)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_calc_11, fill = ps_calc_11)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

p9 <- train %>%
  mutate(ps_calc_12 = as.factor(ps_calc_12)) %>%
  ggplot(aes(ps_calc_12, fill = ps_calc_12)) +
  geom_bar() +
  theme(legend.position = "none")

p10 <- train %>%
  mutate(ps_calc_13 = as.factor(ps_calc_13)) %>%
  ggplot(aes(ps_calc_13, fill = ps_calc_13)) +
  geom_bar() +
  theme(legend.position = "none")

p11 <- train %>%
  ggplot(aes(ps_calc_14, fill = ps_calc_14)) +
  geom_histogram(fill = "blue", binwidth = 1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11),3,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, layout=layout)
```

We find that the histogram features "10", "11", and "14" have close to normal looking distributions with possibly more pronounced tails towards larger values. The other features are not far from a normal or log-normal distribution either and consequently display significant ranges in frequency.


## Float features part 1: "reg" and "calc"

For the floating point features we choose histograms to get a first impression of their distributions:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 7", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_reg_01, fill = ps_reg_01)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_reg_02, fill = ps_reg_02)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_reg_03, fill = ps_reg_03)) +
  geom_histogram(fill = "dark green", binwidth = 0.1) +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_calc_01, fill = ps_calc_01)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_calc_02, fill = ps_calc_02)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_calc_03, fill = ps_calc_03)) +
  geom_histogram(fill = "blue", binwidth = 0.1) +
  theme(legend.position = "none")



layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)
```

We find that while the (green) "reg" features show distributions that are clearly skewed toward a prominent peak, the (blue) "calc" features appear to be pretty uniformly distributed.


## Float features part 2: "car"

Also the second part of these features will be visualised using histograms:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 8", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_car_12, fill = ps_car_12)) +
  geom_histogram(fill = "red", binwidth = 0.05) +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_car_13, fill = ps_car_13)) +
  geom_histogram(fill = "red", binwidth = 0.1) +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_car_14, fill = ps_car_14)) +
  geom_histogram(fill = "red", binwidth = 0.01) +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_15, fill = ps_car_15)) +
  geom_histogram(fill = "red", binwidth = 0.1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
```

We find that the two features on the left show interesting sub-structure in their distributions, while *ps\_car\_15* appears to take only quite distinct values until after `ps_car_15 == 3`when the gaps decrease notably.


## Target variable

Finally, this is what it is all about: whether a claim has been filed ("1") or not ("0"):

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 9", fig.height=3.5, out.width="100%"}
train %>%
  ggplot(aes(target, fill = target)) +
  geom_bar() +
  theme(legend.position = "none")
```

We find:

- The majority of cases has no filed claim:

```{r}
train %>%
  group_by(target) %>%
  summarise(percentage = n()/nrow(train)*100)
```

With less than 4% of policy holders filing a claim the problem is heavily imbalanced.


## More details on missing values

Before proceeding with our analysis we will briefly examine the different combinations of missing values for the individual features in the training data. In this plot, the frequency of NAs per feature is shown in the bar plot at the top. The main part of the plot shows all the combinations where NAs occur in *the same row* for more than 1 feature. Thus, the more red rectangles a feature has the more features it shares NAs with.

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 9", out.width="100%"}
train %>%
  select(which(colMeans(is.na(.)) > 0)) %>%
  aggr(prop = FALSE, combined = TRUE, numbers = TRUE, bars = FALSE, cex.axis = 0.7)
```

We find:

- The features *ps\_car\_03\_cat* and *ps\_car\_05\_cat* have the largest number of NAs. They also share numerous instances where NAs occur in both of them for the same row.

- There are features that share a lot of NA rows with other features, for instance *ps\_reg\_03*. Others are exclusive, like *ps\_car\_12*, or almost exclusive like *ps\_car\_11* or *ps\_car\_02.cat.

- About 2.5% of values are missing in total in eacho of the *train* and *test* data sets:

```{r}
sum(is.na(train))/(nrow(train)*ncol(train))*100
sum(is.na(test))/(nrow(test)*ncol(test))*100
```


# Claim rates for individual features {.tabset .tabset-fade .tabset-pills}

In order to determine the importance of the individual parameters we will study the distribution of their *claim rates*, i.e. how large a fraction per categorical variable ended up claiming or how the distributions of claimed vs unclaimed compare. This analysis will follow a similar structure as above.

Here we estimate error bars from Binomial 95% confidence limits based on the count statistics of the *claim vs no claim* cases. These will help us to decide whether potential differences are significant or not. The corresponding helper function `get_binCI` is defined in Section 2.2 above.


## Binary features part 1

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 10", out.width="100%"}
p1 <- train %>%
  group_by(ps_ind_06_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_06_bin, frac_claim, fill = ps_ind_06_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p2 <- train %>%
  group_by(ps_ind_07_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_07_bin, frac_claim, fill = ps_ind_07_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p3 <- train %>%
  group_by(ps_ind_08_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_08_bin, frac_claim, fill = ps_ind_08_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p4 <- train %>%
  group_by(ps_ind_09_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_09_bin, frac_claim, fill = ps_ind_09_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p5 <- train %>%
  group_by(ps_ind_10_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_10_bin, frac_claim, fill = ps_ind_10_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p6 <- train %>%
  group_by(ps_ind_11_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_11_bin, frac_claim, fill = ps_ind_11_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p7 <- train %>%
  group_by(ps_ind_12_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_12_bin, frac_claim, fill = ps_ind_12_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p8 <- train %>%
  group_by(ps_ind_13_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_13_bin, frac_claim, fill = ps_ind_13_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6,7,8),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1
```

We find that the first batch of binary features has significant differences in the claim fractions especially for the 4 top panels. There are differences in the bottom panels but they are much less significant.


## Binary features part 2

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 11", out.width="100%"}
p1 <- train %>%
  group_by(ps_ind_16_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_16_bin, frac_claim, fill = ps_ind_16_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p2 <- train %>%
  group_by(ps_ind_17_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_17_bin, frac_claim, fill = ps_ind_17_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p3 <- train %>%
  group_by(ps_ind_18_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_18_bin, frac_claim, fill = ps_ind_18_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p4 <- train %>%
  group_by(ps_calc_15_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_15_bin, frac_claim, fill = ps_calc_15_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p5 <- train %>%
  group_by(ps_calc_16_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_16_bin, frac_claim, fill = ps_calc_16_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p6 <- train %>%
  group_by(ps_calc_17_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_17_bin, frac_claim, fill = ps_calc_17_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p7 <- train %>%
  group_by(ps_calc_18_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_18_bin, frac_claim, fill = ps_calc_18_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p8 <- train %>%
  group_by(ps_calc_19_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_19_bin, frac_claim, fill = ps_calc_19_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

p9 <- train %>%
  group_by(ps_calc_20_bin, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_20_bin, frac_claim, fill = ps_calc_20_bin)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6,7,8,9,9),2,5,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1
```

We find that the 2nd part of the binary features have much smaller, statistically insignificant differences in claim rate on average. The only exceptions are *ps\_ind\_16\_bin* and *ps\_ind\_17\_bin* which are significant.


## Categorical features part 1

Note that the features are reordered by claims fraction; except for NA, which is always at the end:


```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 12", out.width="100%"}
p1 <- train %>%
  group_by(ps_ind_02_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_ind_02_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_02_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_02_cat", y = "Claims [%]")

p2 <- train %>%
  group_by(ps_ind_04_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_ind_04_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_04_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_04_cat", y = "Claims [%]")

p3 <- train %>%
  group_by(ps_ind_05_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_ind_05_cat, -frac_claim, FUN = max), frac_claim, fill = ps_ind_05_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_05_cat", y = "Claims [%]")

p4 <- train %>%
  group_by(ps_car_01_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_01_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_01_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_01_cat", y = "Claims [%]")

p5 <- train %>%
  group_by(ps_car_02_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_02_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_02_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_02_cat", y = "Claims [%]")

p6 <- train %>%
  group_by(ps_car_03_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_03_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_03_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_03_cat", y = "Claims [%]")

layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1
```

We find that interestingly enough there appears to be a correlation with the claims rate and whether a certain feature is existing; as evidenced by the high fractions among the NAs for most features. Besides this strong effect there might by a more suble dependence on *ps\_ind\_05\_cat* especially in "2" vs "0".


## Categorical features part 2


```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 12", out.width="100%"}
p1 <- train %>%
  group_by(ps_car_04_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_04_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_04_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_04_cat", y = "Claims [%]")

p2 <- train %>%
  group_by(ps_car_05_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_05_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_05_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_05_cat", y = "Claims [%]")

p3 <- train %>%
  group_by(ps_car_06_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_06_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_06_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_06_cat", y = "Claims [%]")

p4 <- train %>%
  group_by(ps_car_07_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_07_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_07_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_07_cat", y = "Claims [%]")

p5 <- train %>%
  group_by(ps_car_08_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_08_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_08_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_08_cat", y = "Claims [%]")

p6 <- train %>%
  group_by(ps_car_09_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_09_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_09_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_09_cat", y = "Claims [%]")

p7 <- train %>%
  group_by(ps_car_10_cat, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_10_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_10_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_10_cat", y = "Claims [%]")

p8 <- train %>%
  group_by(ps_car_11_cat, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(reorder(ps_car_11_cat, -frac_claim, FUN = max), frac_claim, fill = ps_car_11_cat)) +
  geom_col() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "gray30") +
  theme(legend.position = "none") +
  labs(x = "ps_car_11_cat", y = "Claims [%]")

layout <- matrix(c(1,1,2,3,4,4,5,5,6,6,7,7,8,8,8,8),4,4,byrow=TRUE)
multiplot(p1, p2, p4, p3, p5, p6, p7, p8, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1
```

We find that the second batch of categorical features also shows a few interesting features, such as *ps\_car\_06\_cat* whereas other (like "10") don't seem to be related to the claims rate at all. We notice again that "11\_cat" has far more levels than any other categorical feature 


## Integer features part 1

For the integer features we will now use a scatter plot with (95% confidence) error bars to emphasise the natural ordering of the values:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 13", out.width="100%"}
p1 <- train %>%
  group_by(ps_ind_01, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_01, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_01", y = "Claims [%]")
  
p2 <- train %>%
  group_by(ps_ind_03, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_03, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_03", y = "Claims [%]")
  
p3 <- train %>%
  group_by(ps_ind_14, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_14, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_14", y = "Claims [%]")
  
p4 <- train %>%
  group_by(ps_ind_15, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_ind_15, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "ps_ind_15", y = "Claims [%]")
  
p5 <- train %>%
  filter(!is.na(ps_car_11)) %>%
  group_by(ps_car_11, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_car_11, frac_claim)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "red") +
  theme(legend.position = "none") +
  labs(x = "ps_car_11", y = "Claims [%]")

layout <- matrix(c(1,1,2,2,3,4,4,5),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, layout=layout)
```

We find:

- These variables show much more significant signal in their *claim rates* in the range of 2-3 percentage points.

- *ps\_ind\_03* and *ps\_car\_11* have notable drops in claim number from their maxima which are both at zero.

- The *claims fraction* for *ps\_ind\_01* has an increasing trend, wherease *ps\_ind\_15* is almost monotonically decreasing.

- Note, that here we removed the NAs from the *ps\_car\_11* feature, because they are only 5 values (with zero claims) and would make the plot much harder to read.


## Integer features part 2

For features with a larger range of values we're switching to overlapping density plots. Here we have removed a few values with low counts and very large error bars for about half of the features:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 14", out.width="100%"}
p1 <- train %>%
  group_by(ps_calc_04, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_04, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_04", y = "Claims [%]")
  
p2 <- train %>%
  filter(ps_calc_05 < 6) %>%
  group_by(ps_calc_05, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_05, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_05", y = "Claims [%]")
  
p3 <- train %>%
  filter(ps_calc_06 > 2) %>%
  group_by(ps_calc_06, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_06, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_06", y = "Claims [%]")
  
p4 <- train %>%
  filter(ps_calc_07 < 8) %>%
  group_by(ps_calc_07, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_07, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_07", y = "Claims [%]")
  
p5 <- train %>%
  filter(ps_calc_08 > 2) %>%
  group_by(ps_calc_08, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_08, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_08", y = "Claims [%]")

p6 <- train %>%
  group_by(ps_calc_09, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_09, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_09", y = "Claims [%]")

p7 <- train %>%
  ggplot(aes(ps_calc_10, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4) +
  theme(legend.position = "none")

p8 <- train %>%
  ggplot(aes(ps_calc_11, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4) +
  theme(legend.position = "none")

p9 <- train %>%
  filter(ps_calc_12 < 9) %>%
  group_by(ps_calc_12, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_12, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_12", y = "Claims [%]")

p10 <- train %>%
  filter(ps_calc_13 < 12) %>%
  group_by(ps_calc_13, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ggplot(aes(ps_calc_13, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "ps_calc_13", y = "Claims [%]")

p11 <- train %>%
  ggplot(aes(ps_calc_14, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.4)

layout <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,11),3,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- There is no statistically significant signal in the scatter plots. All error bars overlap in each plot and the slight wiggles that we see might very well just be random variation.

- The density plots show no difference either. Each plot uses two overlapping colours with alpha blending, as shown in the legend on the bottom right, but you only see the blended result because the overlap is practically perfect.


## Float features part 1

We examine the floating point features using overlapping density plots only:

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 15", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_reg_01, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_reg_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_reg_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_calc_01, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p5 <- train %>%
  ggplot(aes(ps_calc_02, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p6 <- train %>%
  ggplot(aes(ps_calc_03, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05)

layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)
```

We find:

- There are small but notable differences in the *reg* features, which are related to the region of the policy holder. Lower region values appear to show a lower number of claims on average.

- The essentially uniform *ps\_calc\_01* - *ps\_calc\_03* features show almost perfect overlap. There might be some small differences but they could well be random.


## Float features part 2

```{r  split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 16", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_car_12, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_car_13, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_car_14, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_15, fill = target)) +
  geom_density(alpha = 0.5, bw = 0.1) +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
```

We find that the *float car* features also show small deviations towards higher numbers having more claims. While the effects might well be statistically significant their practical impact could be small.


## Summary

In terms of the impact of the *target* on the individual features we clearly see strong differences in *claim rates* within and between the different groups. Some of the *binary* features show significant effects in the range of 1-2 percentage points, whereas others are show no practical impact. Specifically, most "ind" features have a clear influence *claims* whereas the *calc binary* features are neutral. The same appears to be true for the *calc integer* and *floating* point features; **suggesting that the *calc* group in general is not of immediate usefulness for our prediction goal.**

**The strongest impact on *claim rates* is shown by the *categorical* and *integer* features; in particular the "ind" and "car" variables which can vary in the range of 2-3 percentage points.** The effect on the *floating* point features is much more subtle, but might prove useful in getting the best prediction out of this data.


# Multi-feature comparisons

After studying each feature individually we will now start to look at *interactions* between them. The annoymity of the features will make it more difficult to interpret these relations. However, they will still be useful for our prediction goal and for gaining a more detailed understanding of our data.


## Correlation overview

We begin with a correlation matrix plot as a first comprehensive overview of our multi-parameter space. We will then take this overview plot as a starting point to investigate specific multi-feature comparisons in the following. Those examinations will likely result in more questions, which we can also examine (to a certain extend) in the same step.

What we will see here is the correlation coefficients for each combination of two features. In simplest terms: this shows whether two features are connected so that one changes with a predictable trend if you change the other. The closer this coefficient is to zero the weaker is the correlation. Both 1 and -1 are the ideal cases of perfect correlation and anti-correlation.

Here we will only include those features that we found to be useful in the previous step. Thereby, our plot will be easier to read and more informative. Note that for the purpose of this plot we will recode our binary features and categorical as integers. All rows with NAs are excluded.

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 17", fig.height=5.5, out.width="100%"}
train %>%
  select(-starts_with("ps_calc"), -ps_ind_10_bin, -ps_ind_11_bin, -ps_car_10_cat, -id) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(target)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE)
```

In this kind of plot we want to look for the bright, large circles which immediately show the strong correlations (size and shading depends on the absolute values of the coefficients; colour depends on direction). Anything that you would have to squint to see is usually not worth seeing. 

We find:

- Most features appear to be primarily correlated with others in their group. We can see this by studying the upper right region near where the diagonal would be and comparing it to the lower left area of the plot.

- There is no obvious correlation with the *target* feature in the left-most column. This could be caused by the sparsity of the `target == 1` values.


Here we plot only the moderately to highly correlated features by showing their correlation coefficients directly:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 18", out.width="100%"}
train %>%
  select(ps_ind_12_bin, ps_ind_14, ps_ind_16_bin, ps_ind_17_bin, ps_ind_18_bin, ps_reg_02,
         ps_reg_03, ps_car_12, ps_car_13, ps_car_14, ps_car_15, ps_car_02_cat, ps_car_04_cat) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE, method = "number")
```

We find:

- There is a very strong correlation between *ps\_ind\_12\_bin* and *ps\_ind\_14*, which is an ordinal integer feature with 5 levels. Other correlations that exist are weaker but still notable.

- The correlation between the "reg" and "car" features, respectively, shows how continuous variables are related. In particular *ps\_car\_14*, which showed only a small effect in the individual plots, might be interesting here. 

- The anti-correlations are generally not as strong, with *ps\_ind\_16\_bin* and *ps\_ind\_18\_bin* accounting for the strongest coefficient with -0.58.


## Alluvial diagram for key features

For a different kind of visualisation of multi-feature interaction we can use an *alluvial plot*. Made available through the [*alluvial* package](https://cran.rstudio.com/web/packages/alluvial/index.html), those plots are a kind of mix between a flow chart and a bar plot and they show how the target categories relate to various discrete features. Here we pick a subset of the more strongly correlated features we had identified in the previous plot:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 19", fig.height=4, out.width="100%"}
allu_train <- train %>%
  filter(!is.na(ps_car_02_cat)) %>%
  filter(ps_car_04_cat %in% c("0","1","2","8","9")) %>%
  group_by(target, ps_ind_17_bin, ps_ind_18_bin, ps_car_02_cat, ps_car_04_cat) %>%
  count() %>%
  ungroup
  
alluvial(allu_train %>% select(-n),
         freq=allu_train$n, border=NA,
         col=ifelse(allu_train$target == 0, "red", "blue"),
         cex=0.75,
         hide = allu_train$n < 200,
         ordering = list(
           order(allu_train$target==1),
           NULL,
           NULL,
           NULL,
           NULL))
```

In this plot, the vertical sizes of the blocks and the widths of the stripes (called "alluvia") are proportional to the frequency. We decided to *hide* the alluvia with the lowest frequencies using the parameter of the same name. Within a category block you can re-*order* the vertical layering of the alluvia to emphasise certain aspects of your data set. 

We see nicely how the small proportion of positive target values (on the upper left) contributes to the make up of the different features. The dominating binary feature values can clearly be seen to carry an approximately proportional amount of *claims*. Feel free to fork this plot to explore the composition of other features.


## Exploring correlated features

Now we will have a closer look at those features we find to be correlated. Here our visualisation will depend on the feature group and how strong the relationship is.


### Pairwise relationships

We begin with a layout of plots that examine the one-to-one relations for all pairings with an (absolute value) *correlation coefficient above 0.5*:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 20", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_ind_14, fill = ps_ind_12_bin)) +
  geom_bar(position = "fill")

p2 <- train %>%
  ggplot(aes(ps_ind_16_bin, ps_ind_18_bin)) +
  geom_count(color = "orange")

p3 <- train %>%
  ggplot(aes(ps_ind_16_bin, ps_ind_17_bin)) +
  geom_count(color = "orange")

p4 <- train %>%
  ggplot(aes(ps_reg_02, ps_reg_03)) +
  geom_point() +
  geom_smooth(method = 'gam', color = "dark green")
  
p5 <- train %>%
  ggplot(aes(ps_car_12, ps_car_13)) +
  geom_point() +
  geom_smooth(method = 'gam', color = "red")
  
p6 <- train %>%
  ggplot(aes(ps_car_12, ps_car_14)) +
  geom_point() +
  geom_smooth(method = 'gam', color = "red")
  
layout <- matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE)
multiplot(p1, p4, p2, p3, p5, p6, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- The barplot of *ps\_ind\_14* demonstrates the strong correlation with *ps\_ind\_12\_bin* that stems mostly from the practically exclusive association of `ps_ind_14 == 0` with `ps_ind_12_bin == FALSE` and `ps_ind_14 == 1` with `ps_ind_12_bin == TRUE`. And even the *ps\_\ind\_14* values in between show a gradually increasing fraction of `ps_ind_12_bin == TRUE`.

- We study the *binary* feature relations using count plots, where the size of the dots correspond to the number of cases. For *ps\_ind\_16_bin* vs *ps\_ind\_17_bin* and *ps\_ind\_18_bin* we see that the predominant associations are `ps_ind_16_bin == TRUE` with `ps_ind_17_bin == FALSE` and `ps_ind_18_bin == FALSE`, respectively. There are no cases in these two relations were both features are *TRUE*.

- For the floating point features we use a scatter plot together with a simple smoothing model to visualise their relationships. In case of *ps\_reg\_02* we clearly see its distinct values. Here the linear relation is formally present but doesn't look too convincing, considering the largely overlapping ranges of *ps\_reg\_03*.

- In contrast, the two *ps\_car* feature plots show a decent correlation that might even be underestimated by the coefficients due to the presence of obvious outliers.


## Interactive multi-dimensional relations

Let's add a little pinch of interactivity to our EDA recipe. Below we are using the R wrapper for the `plotly` [toolkit](https://plot.ly/) which allows us to create plots we can examine from different angles and zoom stages. Here we use it to visualise the covariation between the three *ps\_car* features (12, 13, 14) and its impact on the (colour-coded) *target* variable. Note, that we're only using a subset of the data to keep the plot at a manageable size:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 21", out.width="100%"}
set.seed(4321)
train %>%
  filter(!is.na(ps_car_12) & !is.na(ps_car_13) & !is.na(ps_car_14)) %>%
  select(ps_car_12, ps_car_13, ps_car_14, target) %>%
  sample_n(5e4) %>%
  plot_ly(x = ~ps_car_12, y = ~ps_car_13, z = ~ps_car_14,
          color = ~target, colors = c('#BF382A', '#0C4B8E'),
          text = ~paste('ps_car_12:', ps_car_12,
                        '<br>ps_car_13:', ps_car_13,
                        '<br>ps_car_14:', ps_car_14,
                        '<br>Target:', target)
          ) %>%
  add_markers() %>%
  layout(title = "'Car' group correlations and target impact",
         scene = list(xaxis = list(title = 'ps_car_12'),
                     yaxis = list(title = 'ps_car_13'),
                     zaxis = list(title = 'ps_car_14')))
```

We find that the correlations can clearly be seen and are leading to a *characteristic plane* in the 3D parameter space. The `target == 1` variables appear to be homogeneously dotted throughout this plane. Have a go a exploring the data through this plot :-)


### Multi-parameter grid

Now we are putting all the correlated pieces together in a comprehensive, facetted overview plot for the main parameters in this section. These parameters don't show an obvious connection in the correlation plot, but it is worth looking for more subtle interactions here.

We are plotting the distribution of *ps\_car\_14* (which is correlated with *ps\_car\_12/13*) for the 5 *ps\_ind\_14* classes (correlated with *ps\_ind\_12\_bin*) using *ridgeline plots* through [ggridges](https://cran.r-project.org/web/packages/ggridges/). Ridgeline plots allow for a quick comparison of overlapping (density) curves.

The we separate those overlapping curves by *ps\_ind\_16\_bin* (correlated with *ps\_ind\_17\_bin* and *ps\_ind\_18\_bin*) horizontally and finally by *target* vertically. This is the result:

```{r split=FALSE, fig.align = 'default', warning = FALSE, message = FALSE, fig.cap ="Fig. 22", out.width="100%"}
train %>%
  ggplot(aes(ps_car_14, reorder(ps_ind_14, - ps_car_14, FUN = median), fill = as.factor(ps_ind_14))) +
  geom_density_ridges() +
  labs(y = "ps_ind_14") +
  theme(legend.position = "none", plot.title = element_text(size=11)) +
  facet_grid(ps_ind_16_bin ~ target) +
  ggtitle("Target (left/right) vs ps_ind_16_bin (top/bottom) for ps_car_14 (x) vs ps_ind_14 (y, col)")
```

We find:

- Reading from left to right and top to bottom the number of populated levels of *ps\_ind\_14* (i.e. number of colours) steadily decreases. That means that `target == 1 & ps_ind_16_bin == TRUE` only contain `ps_ind_14 <= 1`. In general, `target == 1` (i.e. claims) contains fewer occupied levels of *ps\_ind\_14* than `target == 0` (no claims).

- The distributions of *ps\_car\_14* show notable differences for different *ps\_ind\_14* levels troughout the grid. In particular `ps_ind_14 == 0` is associated with significantly larger *ps\_car\_14* values.

- The *ps\_ind\_14* levels from 1-3 are much more similar but also show subtle differences particularly towards larger values of *ps\_car\_14*. We have already seen above that level 4 is only sparsely populated.


### The impact of uncorrelated high-signal features

The previous section has revealed a notable amount of variation in the continuous feature *ps\_car\_14* from interactions with correlated categorical features. This was something that we did not see based on the distribution of *ps\_car\_14* alone. Therefore, it is reasonable to ask whether other features might interact in a similar way. That analysis is the subject of this section.

We start by relating the same *ps\_car\_14* to those integer features that we found to have a strong impact on the *claim rates*. Namely, those are the *ps\_ind* features "01", "03", and "15" as well as *ps\_car\_11*:

```{r split=FALSE, fig.align = 'default', warning = FALSE, message = FALSE, fig.cap ="Fig. 23", out.width="100%"}
p1 <- train %>%
  ggplot(aes(ps_car_14, reorder(ps_car_11, - ps_car_14, FUN = median), fill = as.factor(ps_car_11))) +
  geom_density_ridges() +
  labs(y = "ps_car_11") +
  theme(legend.position = "none")

p2 <- train %>%
  ggplot(aes(ps_car_14, reorder(ps_ind_01, - ps_car_14, FUN = median), fill = as.factor(ps_ind_01))) +
  geom_density_ridges() +
  labs(y = "ps_ind_01") +
  theme(legend.position = "none")

p3 <- train %>%
  ggplot(aes(ps_car_14, reorder(ps_ind_03, - ps_car_14, FUN = median), fill = as.factor(ps_ind_03))) +
  geom_density_ridges() +
  labs(y = "ps_ind_03") +
  theme(legend.position = "none")

p4 <- train %>%
  ggplot(aes(ps_car_14, reorder(ps_ind_15, - ps_car_14, FUN = median), fill = as.factor(ps_ind_15))) +
  geom_density_ridges() +
  labs(y = "ps_ind_15") +
  theme(legend.position = "none")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- Our intuition was useful with regard to *ps\_car\_11* and also *ps\_ind\_01*. The *car* feature leads to strong differences in the *ps\_car\_14* distribution - essentially disecting the distribution into distinct peaks. The signature of the NAs is interesting too. In case of the *ind* feature there is a smaller amount of variation in the size of the multiple peaks.

- For *ps\_ind* "03" and "15", however, the differences are very subtle and practically non-existent; respectively. Despite their strong impact on the *claim rates* the variation features appear to leave the *ps\_car\_14* distribution virtually unchanged.


# Feature engineering

Once we have sufficiently explored the connection within the existing features our next step will be to use these insights to build new features, derived from interactions or other characteristics of the data. As in some of my other kernels, the new features will be defined in the following single code block and then studied in the sub-sections below. This gives us the advantage of having everything in one place instead of dotted accross this section. This code block will grow as we engineer new features.

There is of course one problem here: How to do this engineering from *anonymised features*? Not knowing what our variables represent makes it more difficult to interpret their connections. Fortunately, we still have a couple of tricks up our sleeve. Let's see how useful they are :-)

We work on the combined data frame to make sure that our *train* and *test* data are treated consistently.


```{r}
# number of NAs
nano <- combine %>%
  is.na() %>%
  rowSums() %>%
  as.integer()

# Sum up "ind" binary columns
bin_ind <- combine %>% select(ends_with("bin")) %>%
  select(starts_with("ps_ind")) %>%
  rowSums() %>%
  as.integer()

# Sum up "calc" binary columns
bin_calc <- combine %>% select(ends_with("bin")) %>%
  select(starts_with("ps_calc")) %>%
  rowSums() %>%
  as.integer()

# "ind" binary column differences per row
# (uses "rep" to create a combine-size data frame from the
# single reference row that we then subtract from all rows)
bins <- combine %>%
  select(ends_with("bin")) %>%
  select(starts_with("ps_ind")) %>%
  mutate_all(funs(as.integer))

ref_bin <- bins %>% summarise_all(median, na.rm = TRUE)
ref_bin <- ref_bin[rep(1,nrow(combine)),]

diff_ind <- rowSums(abs(bins - ref_bin))

# "calc" binary column differences per row
bins <- combine %>%
  select(ends_with("bin")) %>%
  select(starts_with("ps_calc")) %>%
  mutate_all(funs(as.integer))

ref_bin <- bins %>% summarise_all(median, na.rm = TRUE)
ref_bin <- ref_bin[rep(1,nrow(combine)),]

diff_calc <- rowSums(abs(bins - ref_bin))


# Apply changes to combine frame
combine <- combine %>%
  mutate(nano = nano,
         bin_ind = bin_ind,
         bin_calc = bin_calc,
         diff_ind = diff_ind,
         diff_calc = diff_calc)

# Split into train vs test
train <- combine %>%
  filter(dset == "train")
test <- combine %>%
  filter(dset == "test")
```


## Number of NAs per ID

We start with the number of NAs for each policy holder ID. Sometimes it's just as important to take into account how much we don't know about a certain user than work with the information we have. Below, we examine the distribution of NAs per ID throughout all features (i.e. per row for all columns) and the corresponding claim rates.

Note the logarithmic y-axis in the histogram. For the claim rates, we make use of a *facet\_zoom*, via the [ggforce package](https://cran.r-project.org/web/packages/ggforce), to examine the observations with few NAs in greater detail. The zoomed area (on the left) is highlighted in the full plot (on the right) in a darker shade of grey:

```{r split=FALSE, fig.align = 'default', warning = FALSE, message = FALSE, fig.cap ="Fig. 24", out.width="100%"}
p1 <- train %>%
  ggplot(aes(nano, fill = as.factor(nano))) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "Number of NAs")

p2 <- train %>%
  group_by(nano, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ungroup() %>%
  ggplot(aes(nano, frac_claim)) +
  geom_point(color = "red") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Number of NAs", y = "Claims [%]") +
  facet_zoom(x = nano < 5, y = frac_claim < 10)

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- The number of NAs is predominantly low with most IDs having <= 2 NA entries (log axis). Interestingly, 2 NAs, not zero, is the most common number. Beyond that, the numbers drop quickly and there are only a few cases with more than 5 NAs. It is interesting to note that no ID has exactly 5 NAs, which might reflect the way our features are related. There is a small second peak at 7 NAs, which might also reveal some underlying structure in the data.

- The *claim rates* show a very interesting pattern with a steady decline from 0 to 4 within the percentage range of 2-5%. For 6 and 7 NAs the rates are very high in comparison (means of 60% and 40%) and again there is a declining trend. For 8 NAs there are not enough cases to make a statistically significant statement.

- The differences between 0-4 and 6-8 NAs suggests that we might have two distinct populations here which reflect differences in the policy holder's characteristics. It will be well worth coming back to these distinctions during the course of our further analysis.


## Sum of binary features

Next, we want to examine what happens if we sum up all the binary feature values of each group for the different IDs. There are two groups that have binary features: "ind" (the individual policy holder characteristics) and "calc" (calculated features). Here we show their distributions and *claim rates*: 

```{r split=FALSE, fig.align = 'default', warning = FALSE, message = FALSE, fig.cap ="Fig. 25", out.width="100%"}
p1 <- train %>%
  ggplot(aes(bin_ind, fill = as.factor(bin_ind))) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "Sum of binary 'ind' features")

p2 <- train %>%
  filter(bin_ind < 6) %>%
  group_by(bin_ind, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ungroup() %>%
  ggplot(aes(bin_ind, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Sum of binary 'ind' features", y = "Claims [%]")

p3 <- train %>%
  ggplot(aes(bin_calc, fill = as.factor(bin_calc))) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none") +
  labs(x = "Sum of binary 'calc' features")

p4 <- train %>%
  filter(bin_ind < 6) %>%
  group_by(bin_calc, target) %>%
  count() %>%
  spread(target, n) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ungroup() %>%
  ggplot(aes(bin_calc, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "Sum of binary 'calc' features", y = "Claims [%]")

layout <- matrix(c(1,2,3,4),2,2,byrow=FALSE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- Adding up the "ind" binary features results in a distribution which peaks at 2 and has a range of 1-6. There is no "0", which means that at least one of the binary flags is always set. At the beginning of this kernel we saw that there are a total of 11 "ps\_ind\_??\_bin" features but we only have a maximum of six set to "TRUE/1" at the same time. This is consistent with "FALSE/0" being the dominating value for most of these features.

- For plotting the sum of binary "ind" features (bottom left) we see that there are only a few instances of 6 binary flags set (top left). This leads to a large statistical error bar that would dominate the plot and make it harder to read. Therefore, we remove `binary sum == 6` from the bottom left plot. For the remaining sums there is a trend of increasing *claim rate* with increasing sum; but only "3" vs "1" or "2" is statistically significant. Still, this could be a useful new feature.

- The "calc" binary features on the other hand display no significant variation in their *claim rates*; thus providing further evidence that the "calc" group might not be very useful for our prediction goal. Here the sum distribution also peaks at 2, but takes a minimum of 0 and also a maximum of 6 for in total 6 "calc" features. Toward larger sums the (log-scaled) distribution has less of a sharp decline than for the "ind" features.


## Difference measure for binary features

After adding up the different binary values, our next feature will be a simple measure of how *different* certain binary observations are. We define a reference row, which corresponds to the *median* value of each binary feature. This definition was changed from the original one following constructive criticism by [David J. Slate](https://www.kaggle.com/dslate) and [Oscar Takeshita](https://www.kaggle.com/pliptor) in the comments below. They were rightfully pointing out that this feature is *not* independent of the reference row. 

We then subtract the binary values of this reference row from the set of binary values for each row. This determines all the bins that are different (i.e. are "0" where the reference row is "1" and vice versa). To properly count the differences we are summing up the absolute values of the resulting data frame, thereby adding a value of "-1" as "1" to our difference measure. Without the absolute values, a "-1" and a "1" would cancel out, but both indicate that there is a difference in the binary features and two differences should add up to "2". This is the main difference between this feature and the binary sum feature above.

We do this for the "ind" and the "calc" binary features separately. Below are the resulting plots for the distribution and the claim rates for each distance measure. Note the square-root scaling in the y-axis of the bar plot:

```{r split=FALSE, fig.align = 'default', warning = FALSE, message = FALSE, fig.cap ="Fig. 26", out.width="100%"}
p1 <- train %>%
  ggplot(aes(as.factor(diff_ind), fill = as.factor(diff_ind))) +
  geom_bar() +
  scale_y_sqrt() +
  labs(fill = "diff_ind", x = "Absolute difference of binary 'ind' features")

p2 <- train %>%
  ggplot(aes(as.factor(diff_calc), fill = as.factor(diff_calc))) +
  geom_bar() +
  scale_y_sqrt() +
  labs(fill = "diff_calc", x = "Absolute difference of binary 'calc' features")

p3 <- train %>%
  filter(diff_ind < 7) %>%
  group_by(diff_ind, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ungroup() %>%
  ggplot(aes(diff_ind, frac_claim)) +
  geom_point(color = "orange") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "orange") +
  theme(legend.position = "none") +
  labs(x = "Absolute difference of binary 'ind' features", y = "Claims [%]")

p4 <- train %>%
  filter(diff_calc < 6) %>%
  group_by(diff_calc, target) %>%
  count() %>%
  spread(target, n, fill = 0) %>%
  mutate(frac_claim = `1`/(`1`+`0`)*100,
         lwr = get_binCI(`1`,(`1`+`0`))[[1]]*100,
         upr = get_binCI(`1`,(`1`+`0`))[[2]]*100
         ) %>%
  ungroup() %>%
  ggplot(aes(diff_calc, frac_claim)) +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7, color = "blue") +
  theme(legend.position = "none") +
  labs(x = "Absolute difference of of binary 'calc' features", y = "Claims [%]")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1
```

We find:

- The differences of "ind" binary values are dominated by values 1 and 3. Interestingly, there are no values of zero, indicating that our median reference row does not actually occur in the data. We're seeing a pattern in the *claim rates* suggesting that the *claim fraction* rises the further away we step from our reference row values. However, only the differences between values 1 & 2 vs 3 & 4 appear to be statistically significant.

- In the "calc" binary features we see that we still haven't managed to find a useful signal. There might be a tentative decrease in *claim rate* between the values of 2 vs 3, but everything is consistent within the uncertainties. Therefore, it appears that there is very little predictive power in the "calc" group.


# Modelling preparation

Before we start building our prediction model we want to run a few sanity checks and get our data in shape for the model fitting. These preparations are the subject of this section.


## Train vs test comparison

One thing we want to make sure is that the train and test data do actually cover similar parts of the parameter space. There is very little use in having a feature with a strong *target* correlation in our training data if those crucial feature levels are absent in the test data.

First, we'll prepare the data and define a new helper function. The following code block does most of the (relatively) heavy lifting in this subsection. We will outline its various steps as we explore the results.

```{r}
bin_col <- combine %>% select(ends_with("bin")) %>%
  colnames() %>% unlist(use.names = FALSE) %>% as.character()
cat_col <- combine %>% select(ends_with("cat")) %>%
  colnames() %>% unlist(use.names = FALSE) %>% as.character()

train_frac_bin <- NULL
for (i in bin_col){
  foo <- combine %>%
    group_by(!!sym(i), dset) %>%
    count() %>%
    spread(dset, n, fill = 0) %>%
    mutate(frac_train = train/(train+test)*100,
           lwr = get_binCI(train,(train+test))[[1]]*100,
           upr = get_binCI(train,(train+test))[[2]]*100
           )
  train_frac_bin <- tibble(name = i,
                           value = c(FALSE, TRUE),
                           tfrac = c(foo$frac_train),
                           lwr = c(foo$lwr),
                           upr = c(foo$upr)) %>%
    bind_rows(train_frac_bin)
}

plot_cat_train_test <- function(col){
  col <- enquo(col)
  combine %>%
    group_by(!!col, dset) %>%
    count() %>%
    spread(dset, n, fill = 0) %>%
    mutate(frac_train = train/(train+test)*100,
           lwr = get_binCI(train,(train+test))[[1]]*100,
           upr = get_binCI(train,(train+test))[[2]]*100,
           col = !!col
           ) %>%
    ggplot(aes(col, frac_train)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.5, size = 0.7) +
    labs(x = as.character(col)[2], y = "")
}
```


We begin with a comparison of the `train` vs `test` data in two different aspects: First the distribution of *id* values and second a closer look at the binary features in which we plot the relative fraction of `train` observations for each level:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 27", out.width="100%"}
p1 <- combine %>%
  ggplot(aes(id, fill = dset)) +
  geom_density(bw = 1, alpha = 0.5) +
  coord_cartesian(ylim = c(7.3e-4,8.3e-4))

p2 <- train_frac_bin %>%
  ggplot(aes(name, tfrac, color = value)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr, color = value), width = 0.5, size = 0.7) +
  labs(x = "Binary 'ind' features", y = "Training set percentage") +
  theme(axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)
p1 <- 1; p2 <- 1
```

We find:

- The *id* values appear to be pretty randomly distributed. There is no obvious *id* range where we find predominantly `train` or `test` data. We're using a density plot here to copmare the relative fluctuations.

- The binary feature levels are remarkably similar in terms of their `train/(train+test)` percentage. We see that on average the `train` data makes up about 40% of each feature and for each *TRUE & FALSE* level, respectively. Levels with larger deviation have only a few objects and therefore large errors. Everything is consistent within the uncertainties.


Next, we will have a look at the levels of the categorical features. Our approach will be the same as for the binary features, in that we compute the `train/(train+test)` percentage for each level and then plot everything on a comprehensive plot. This figure is rather busy and the important result is in the overall picture rather than the details:


```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 28", out.width="100%"}
#str_c("p",seq(1,length(cat_col))," <- plot_cat_train_test(",cat_col)

p1 <- plot_cat_train_test(ps_ind_02_cat) 
p2 <- plot_cat_train_test(ps_ind_04_cat)
p3 <- plot_cat_train_test(ps_ind_05_cat) 
p4 <- plot_cat_train_test(ps_car_01_cat)
p5 <- plot_cat_train_test(ps_car_02_cat)
p6 <- plot_cat_train_test(ps_car_03_cat)
p7 <- plot_cat_train_test(ps_car_04_cat)
p8 <- plot_cat_train_test(ps_car_05_cat)
p9 <- plot_cat_train_test(ps_car_06_cat)
p10 <- plot_cat_train_test(ps_car_07_cat)
p11 <- plot_cat_train_test(ps_car_08_cat)
p12 <- plot_cat_train_test(ps_car_09_cat)
p13 <- plot_cat_train_test(ps_car_10_cat)
p14 <- plot_cat_train_test(ps_car_11_cat)

layout <- matrix(seq(1,14),2,7,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1
p11 <- 1; p12 <- 1; p13 <- 1; p14 <- 1
```

We find:

- Once more, practically all levels appear to be consistent with a 40% `train` percentage, which is even more remarkable than for the binary features.

- There are a few tentative outliers, e.g. in *ps_ind_05_cat*, but for so many levels that's almost expected for the 95% confidence uncertainties that we are plotting.


Finally, we will also sample some of the floating-point features. Here we use overlapping density plots for the `train` vs `test` observations:

```{r split=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 29", out.width="100%"}
p1 <- combine %>%
  ggplot(aes(ps_reg_01, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p2 <- combine %>%
  ggplot(aes(ps_reg_02, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p3 <- combine %>%
  ggplot(aes(ps_reg_03, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p4 <- combine %>%
  ggplot(aes(ps_calc_01, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p5 <- combine %>%
  ggplot(aes(ps_calc_02, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05) +
  theme(legend.position = "none")

p6 <- combine %>%
  ggplot(aes(ps_calc_03, fill = dset)) +
  geom_density(alpha = 0.5, bw = 0.05)

layout <- matrix(c(1,2,3,4,5,6),2,3,byrow=TRUE)
multiplot(p1, p2, p3, p4, p5, p6, layout=layout)
p1 <- 1; p2 <- 1; p3 <- 1; p4 <- 1; p5 <- 1; p6 <- 1; p7 <- 1; p8 <- 1; p9 <- 1; p10 <- 1; p11 <- 1

```

We find that for these features the overlap appears to be practically perfect. We're using two different colours, with an alpha-mixing parameter of 0.5, but all we see is the blended overlap.

**In summary**, we find the `train` vs `test` data sets to be remarkably similar in the distributions of their features. The train/test split appears to be well executed and stratified by feature levels.


## Feature selection, evaluation metric, and validation split

Not all features in our data set will be important. In particular, we have seen that the *calc* features appear to be of limited usefulness. Here we only include meaningful variables. The following code block, adapted from my recent [Taxi competition kernel](https://www.kaggle.com/headsortails/nyc-taxi-eda-update-the-fast-the-curious) is designed to be easily modified to a new challenge:


```{r}
# Feature formatting
combine <- combine %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(levels(target))[target])
```


```{r}
# Specific definitions:
#---------------------------------
# predictor features
ind_cols <- c("ps_ind_01","ps_ind_02_cat","ps_ind_03","ps_ind_04_cat","ps_ind_05_cat","ps_ind_06_bin","ps_ind_07_bin","ps_ind_08_bin","ps_ind_09_bin","ps_ind_10_bin","ps_ind_11_bin","ps_ind_12_bin","ps_ind_13_bin","ps_ind_14","ps_ind_15","ps_ind_16_bin","ps_ind_17_bin","ps_ind_18_bin")
reg_cols <- c("ps_reg_01","ps_reg_02","ps_reg_03")
car_cols <- c("ps_car_01_cat","ps_car_02_cat","ps_car_03_cat","ps_car_04_cat","ps_car_05_cat","ps_car_06_cat","ps_car_07_cat","ps_car_08_cat","ps_car_09_cat","ps_car_10_cat","ps_car_11_cat","ps_car_11","ps_car_12","ps_car_13","ps_car_14","ps_car_15")
calc_cols <- c("ps_calc_01","ps_calc_02","ps_calc_03","ps_calc_04","ps_calc_05","ps_calc_06","ps_calc_07","ps_calc_08","ps_calc_09","ps_calc_10","ps_calc_11","ps_calc_12","ps_calc_13","ps_calc_14","ps_calc_15_bin","ps_calc_16_bin","ps_calc_17_bin","ps_calc_18_bin","ps_calc_19_bin","ps_calc_20_bin")
eng_cols <- c("nano", "bin_ind", "bin_calc", "diff_ind", "diff_calc")

train_cols <- c(ind_cols, reg_cols, car_cols, eng_cols)

# target feature
y_col <- c("target")
# identification feature
id_col <- c("id") 
# auxilliary features
aux_cols <- c("dset")
#---------------------------------

# General extraction
#---------------------------------
# extract test id column
test_id <- combine %>%
  filter(dset == "test") %>%
  select(!!sym(id_col))

# all relevant columns for train/test
cols <- c(train_cols, y_col, aux_cols)
combine <- combine %>%
  select_(.dots = cols)


# split train/test
train <- combine %>%
  filter(dset == "train") %>%
  select_(.dots = str_c("-",c(aux_cols)))
test <- combine %>%
  filter(dset == "test") %>%
  select_(.dots = str_c("-",c(aux_cols, y_col)))
#---------------------------------
```

The model evaluation metric here is the [Normalised Gini Function](https://www.kaggle.com/c/porto-seguro-safe-driver-prediction#evaluation). It has the advantage that only the relative order of the predicted probabilities is measured, not their absolute value. Thereby, your prediction is successful as long as it assigns low probabilities to true "no claims" observations and high probabilities to true "claims". The "normalised" property means that probabilities are measured in the interval [0,1]. The metric then orders your predictions by probability and the more "claims" (or "no claims") are among the high (or low) probabilities the better the result.

For a more extensive and intuitive explanation of the Gini metric I recommend [this great kernel by kilian](https://www.kaggle.com/batzner/gini-coefficient-an-intuitive-explanation). There are numerous kernels already that deal with (efficiently) implementing this metric. We borrow our definition from [this great work by Kevin](https://www.kaggle.com/kevinbonnes/xgboost-starter-0-280/code) (many thanks!):

```{r}
xgb_normalizedgini <- function(preds, dtrain){
  actual <- getinfo(dtrain, "label")
  score <- NormalizedGini(preds,actual)
  return(list(metric = "NormalizedGini", value = score))
}
```

In order to assess how well our model generalises we will perform a cross-validation step and also split our training data into a *train* vs *validation* data set. Thereby, the model performance can be evaluated on a data sample that the algorithm has not seen. We split our data into 80/20 fractions:

```{r}
set.seed(4321)
trainIndex <- createDataPartition(train$target, p = 0.8, list = FALSE, times = 1)

train <- train[trainIndex,]
valid <- train[-trainIndex,]
```

Normally, we might want to include a cleaning step where we remove wildly unlikely data points (created by typos or other mistakes) that doesn't teach us anything about our problem at hand. Here, however the data sets appear to be very well behaved and no cleaning is necessary.


## XGBoost parameters and fitting

We fit our model using *XGBoost* because it's a robust and easy to use tool. In essence, it is a decision-tree gradient boosting algorithm with a high degree of popularity here on Kaggle. The parameters used here are not very sophisticated but should give you an idea how the algorithm works. I recommend to spend some time optimising the parameters and also to try out different tools presented in the other kernels.

A few brief words about *gradient boosting*, as far as I understand it: Boosting is what we call the step-by-step improvement of a weak learner (like a relatively shallow decision tree of *max\_depth* levels) by successively applying it to the results of the previous learning step (for *nrounds* times in total). *Gradient Boosting* focusses on minimising the Loss Function (according to our evaluation metric) by training the algorithm on the gradient of this function. The method of *Gradient Decent* iteratively moves into the direction of the greatest decent (i.e. most negative first derivative) of the loss function. The step sizes can vary from iteration to iteration but has a multiplicative *shrinkage factor eta in (0,1]* associated with it for additional tuning. Smaller values of eta result in a slower decent and require higher *nrounds*.

In order for *XGBoost* to properly ingest our data samples we need to re-format them slightly:

```{r}
#convert to XGB matrix
foo <- train %>% select(-target)
bar <- valid %>% select(-target)

dtrain <- xgb.DMatrix(as.matrix(foo),label = train$target)
dvalid <- xgb.DMatrix(as.matrix(bar),label = valid$target)
dtest <- xgb.DMatrix(as.matrix(test))
```


Now we define the meta-parameters that govern how *XGBoost* operates:

```{r}
xgb_params <- list(colsample_bytree = 0.7, #variables per tree 
                   subsample = 0.7, #data subset per tree 
                   booster = "gbtree",
                   max_depth = 5, #tree levels
                   eta = 0.3, #shrinkage
                   eval_metric = xgb_normalizedgini,
                   objective = "reg:logistic",
                   seed = 4321,
                   nthread = -1
                   )

watchlist <- list(train=dtrain, valid=dvalid)
```

We first run a cross-validation (CV) step to measure our model's performance and to determine how many iteration steps are optimal. To keep this stage short, within the limits of this kernel, I'm only running 50 iterations here (*nrounds* parameter). Ideally, you would want to use more.

We are using a 5-fold CV, which essentially means that we split our data in 5 parts and train on any 4 of them while evaluating on the remaining one. This is done iteratively, so that every fold becomes the validation fold. This is an efficient way to measure the performance of your fit on data it has not seen.

**The parts below here are currently not running in the Kernel because of memory problems. I'm investigating. Sorry for that.**

```{r eval=FALSE}
set.seed(1234)
xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 5, nfold = 5, nrounds=50, maximize = TRUE)
```

The number of the best iteration is now stored in a variable we can access for training our classifier for the optimum number of rounds. Here and above we also set a random seed to ensure reproducability:

```{r eval=FALSE}
set.seed(4321)
gb_dt <- xgb.train(params = xgb_params,
                   data = dtrain,
                   print_every_n = 5,
                   watchlist = watchlist,
                   nrounds = xgb_cv$best_iteration)
```

Be aware that this is not a competitive model. Other (engineered) features can be included, the meta-parameters can be optimised, and a larger amount of training steps can be run. In addition, several kernels have already shown the power of ensembling different individual models into a combined prediction that shows better results. 

What this basic model can do is to serve as an example on how to construct your own prediction and give you a place to start from. Make sure to check out other modelling kernels as well to get a broader overview.


## Feature importance

After training we will check which features are the most important for our model. This can provide the starting point for an iterative process where we identify, step by step, the significant features for a model. Here we will simply visualise these features:

```{r eval=FALSE, fig.align = 'default', warning = FALSE, fig.cap ="Fig. 30", out.width="100%"}
imp_matrix <- as.tibble(xgb.importance(feature_names = colnames(train %>% select(-target)), model = gb_dt))

imp_matrix %>%
  ggplot(aes(reorder(Feature, Gain, FUN = max), Gain, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Features", y = "Importance")
```

We find:

- As pointed out in several other modelling kernels, the features *ps\_car\_13* and *ps\_reg\_03* are the most important ones for our tentative prediction model. Also note the two *ps\_ind* features on 3rd and 4th rank.

- In addition, our engineered features are not doing bad either. *diff\_ind* is ranked the 5th most significant feature, and *nano* still has some impact, too. *diff\_calc*, *bin\_calc*, and *bin\_ind* are towards the end of the list but (not only) in this competition small improvements can have a large impact on the resulting leaderboard score.

- Those results are preliminary, since the model performance can certainly be optimised, but they already show that feature engineering is a promising way of improving your prediction.


## Prediction and submission file

Once we are reasonably happy with our CV score we use the corresponding model to make a prediction for the test data, write the submission file, and submit it to the Kaggle leaderboard. This gives a performance score based on an *independent* data set. For this very reason, it is advisable to aim to keep the number of leaderboard submission low. Otherwise you run the risk to have your model influenced too much by this public leaderboard data which will result in overfitting. It's better to trust your local CV.

Practically, this pre-ulimate code block will apply the *XGBoost* model to the testing data and write a submission file according to the competition specification. You will then find the file `submit.csv` in the "Output" tab of the Kaggle kernel or, if you have run the script locally, in the source directory.

```{r eval=FALSE}
pred <- test_id %>%
  mutate(target = predict(gb_dt, dtest))

pred %>% write_csv('submit.csv')
```

All that's left to do is to double-check this submission file to make sure that we are not wasting a (potentially) valuable submission with a mal-formatted output file. We are comparing its shape and content with the *sample submission file* that we read in at the very beginning, and also plot the distribution of the predicted values compared to the training values for an approximate comparison:

```{r eval=FALSE}
identical(dim(sample_submit),dim(pred))
glimpse(sample_submit)
glimpse(pred)
bind_cols(sample_submit, pred) %>% head(5)
```

Prediction file format successfully verified.

AMany thanks for reading this kernel! I hope that it will benefit your own analysis.

---

Have fun!
