# CodeHunter-CAL

---
title: "2016 Election Analysis"
editor_options:
  chunk_output_type: console
date: "Due June 13, 2018, midnight"
output:
  html_document: default
  pdf_document: default
---


# Instructions and Expectations

- You are allowed and encouraged to work with one partner on this project.  Include your names, perm numbers, and whether you are taking the class for 131 or 231 credit.

- You are welcome and encouraged to write up your report as a research paper (e.g. abstract, introduction, methods, results, conclusion) as long as you address each of the questions below.  Alternatively, you can format the assignment like a long homework by addressing each question in parts.

- There should be no raw R _output_ in the paper body!  All of your results should be formatted in a professional and visually appealing manner. That means, Either as a polished visualization or for tabular data, a nicely formatted table (see the documentation for [kable and kableExtra packages](https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf). If you feel you must include extensive raw R output, this should be included in an appendix, not the main report.  

- All R code should be available from your Rmarkdown file, but does not necssarily need to be shown in the body of the report!  Use the chunk option `echo=FALSE` to exclude code from appearing in your write up.  In addition to your Rmarkdown, you should turn in the write up as either a pdf document or an html file (both are acceptable).

- Many of the questions in this project are intentionally vague.  Make sure you always justify the choices you make (e.g. ''we decided to standardize the variables before classifying because ...'').  Feel free experiment and be creative!

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = F, cache = TRUE)

indent1 = '    '
indent2 = paste(rep(indent1, 2), collapse='')
indent3 = paste(rep(indent1, 3), collapse='')

doeval = TRUE
doecho = FALSE

library(knitr)
library(tidyverse)
library(kableExtra)
library(ggmap)
library(maps)
library(Rtsne)
library(NbClust)
library(tree)
library(maptree)
library(class)
library(reshape2)
library(glmnet)
```

# Background

Predicting voter behavior is complicated for many reasons despite the tremendous effort in collecting, analyzing, and understanding many available datasets. 
For our final project, we will analyze the 2016 presidential election dataset.

The presidential election in 2012 did not come as a surprise to most. Many analysts predicted the outcome of the election correctly including [Nate Silver](https://en.wikipedia.org/wiki/Nate_Silver).  There has been some [speculation about his approach](https://www.theguardian.com/science/grrlscientist/2012/nov/08/nate-sliver-predict-us-election).

Despite largely successful predictions in 2012, the 2016 presidential election was
[more surprising](https://fivethirtyeight.com/features/the-polls-missed-trump-we-asked-pollsters-why/).  

Answer the following questions in one paragraph for each.

#1. What makes predicting voter behavior (and thus election forecasting) a hard problem?

There is always a chance that predictions, no matter how good or strong they may be, can be wrong, even if analysis is done. Also, making predictions with voting behavior may be difficult to do, because voting behavior is determined by the human, and it is hard to determine what decisions humans will make. There are also a numerous amount of factors that go into voting behavior, such as race, age, their location, their political stance, how easily they may be influenced or affecting by the media, family, or even friends. Some voters even go to the polls undecided and conflicted by two major parties, and decide at the last second (in the poll), who they vote for. All of these factors that go into voter behavior, truly make predicting voting behavior and election forecasting a difficult problem to analyze. 


#2. Although Nate Silver predicted that Clinton would win 2016, [he gave Trump higher odds than most](http://fivethirtyeight.com/features/why-fivethirtyeight-gave-trump-a-better-chance-than-almost-anyone-else/). 
#What is unique about Nate Silver's methodology?

We think that something that was unique about Nate Silver's methodology, was that he took uncertainty and risk into account while making models. Silver also took into account the amount of polling errors there were, both nationally and on the state level. He even stated that polling errors were "correlated", and that if one poll misses in one direction, many polls in the same state, or even other states can miss in that direction as well. Additionally, Silver looked at the amount of undecided voters and third party voters, more than other polls did, and took those voters into account due to them being a higher population than past elections. Silver also examined the amount of voters that did not decide who they were voting for until the week before the election, and those voters went strongly for Trump. Out of many other polls, Silver was the one who took uncertainty into account and did not play that as a small role in election forecasting. He also was able to show that midwestern states were highly correlated with their voting outcomes, meaning that if one midwestern state voted for Trump, it was likely that many other midwestern states would vote for Trump as well. The majority of the other polls really downplayed Trump's chances of winning the election, and did not take into account the variables that Silver did. Unlike those polls, Silver was able to correctly predict the outcome of the election.


#3. Discuss why analysts believe predictions were less accurate in 2016.  Can anything be done to make future predictions better? What are some challenges for predicting future elections? How do you think journalists communicate results of election forecasting models to a general audience?

One factor that led to predictions being less accurate in the 2016 elections was that many of the voters were undecided until the week before or until they went to the poll. This undecisiveness within the voters led to a huge polling error. Another factor that could have gone into this, was that many Trump supporters did not want to come out and say that they were voting for Trump, which led to the polling error also being greater than past elections. Future elections should really factor in the undecided voters, the third party voters, and the important role that uncertainty can play within elections and forecasting outcomes. For future elections, predictions should really examine all the factors that go into each party's stances, and how many voters feel strongly about each party. Journalists may comminucate the results of election forecasting models based on the time they are given the data, which may lead to mistakes due to the fact that voters may change their mind within the last week, or even last day of the election.



# Data

```{r data}

election.raw = read.csv("data/election/election.csv") %>% as.tbl
census_meta = read.csv("data/census/metadata.csv", sep = ";") %>% as.tbl
census = read.csv("data/census/census.csv") %>% as.tbl
census$CensusTract = as.factor(census$CensusTract)
```

## Election data

Following is the first few rows of the `election.raw` data:

```{r, echo=FALSE}

kable(election.raw %>% head)
```

The meaning of each column in `election.raw` is clear except `fips`. The acronym is short for [Federal Information Processing Standard](https://en.wikipedia.org/wiki/FIPS_county_code).

In our dataset, `fips` values denote the area (US, state, or county) that each row of data represent: i.e., some rows in `election.raw` are summary rows. These rows have `county` value of `NA`. There are two kinds of summary rows:

* Federal-level summary rows have `fips` value of `US`.
* State-level summary rows have names of each states as `fips` value.

## Census data

Following is the first few rows of the `census` data:

```{r, echo=FALSE}

kable(census %>% head, "html")  %>% kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width=FALSE) %>% scroll_box(width = "100%")
```


### Census data: column metadata


## Data wrangling
4. Remove summary rows from `election.raw` data: i.e.,

```{r}

#seperate election federal for its unique factor of having fips=US 
election_federal <- filter(election.raw, fips == "US")

# state does not have the above unique attributes 
election_state <- filter(election.raw, fips != "US" & is.na(county)) 

# county can be seperate by its unique aspect of having the county defined 
election <- filter(election.raw, !is.na(county))

```
    * Federal-level summary into a `election_federal`.
    
    * State-level summary into a `election_state`.
    
    * Only county-level data is to be in `election`.


5. How many named presidential candidates were there in the 2016 election? Draw a bar chart of all votes received by each candidate


```{r}

candidates <- unique(election_federal$candidate)
pres.candidates <- length(candidates)-1

prCa <- paste("The number of presidential cadidate is", pres.candidates)
print(c(prCa))

groupcands <- election %>% group_by(candidate) 

groupcandsbar <- groupcands %>% ggplot(aes(x=candidate, y=votes))+ geom_bar(stat = "identity") + theme(axis.text.x= element_text(angle=60, hjust=1)) 
groupcandsbar

```

6. Create variables `county_winner` and `state_winner` by taking the candidate with the highest proportion of votes. 
  Hint: to create `county_winner`, start with `election`, group by `fips`, compute `total` votes, and `pct = votes/total`. 
  Then choose the highest row using `top_n` (variable `state_winner` is similar).
  
```{r}  

county_winner <- election %>% group_by(fips) %>%
                 mutate(total = sum(votes)) %>%
                 mutate(pct = votes/total) %>%
                 top_n(1)

state_winner <- election_state %>%
                group_by(fips) %>%
                mutate(total = sum(votes)) %>%
                mutate(pct = votes/total) %>%
                top_n(1)

county_winner
state_winner
```
  
# Visualization

Visualization is crucial for gaining insight and intuition during data mining. We will map our data onto maps.

The R package `ggplot2` can be used to draw maps. Consider the following code.

```{r, message=FALSE}

states = map_data("state")

ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # color legend is unnecessary and takes too long

```

The variable `states` contain information to draw white polygons, and fill-colors are determined by `region`.

7. Draw county-level map by creating `counties = map_data("county")`. Color by county

```{r}

counties = map_data("county")

gg=ggplot(data=counties)
gp=geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white")
gg+gp+coord_fixed(1.3)+guides(fill=FALSE)

```

8. Now color the map by the winning candidate for each state. 
  First, combine `states` variable and `state_winner` we created earlier using `left_join()`. 
  Note that `left_join()` needs to match up values of states to join the tables; however, they are in different formats: e.g. `AZ` vs. `arizona`.
  Before using `left_join()`, create a common column by creating a new column for `states` named
  `fips = state.abb[match(some_column, some_function(state.name))]`. 
  Replace `some_column` and `some_function` to complete creation of this new column. Then `left_join()`.
  Your figure will look similar to state_level [New York Times map](https://www.nytimes.com/elections/results/president).

```{r}

states <- states %>%
mutate(fips = state.abb[match(region, tolower(state.name))]) %>%
left_join(state_winner, by="fips")

ggplot(data = states) +
geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") +
coord_fixed(1.3) +
guides(fill=FALSE) 

```


9. The variable `county` does not have `fips` column. So we will create one by pooling information from `maps::county.fips`.
  Split the `polyname` column to `region` and `subregion`. Use `left_join()` combine `county.fips` into `county`. 
  Also, `left_join()` previously created variable `county_winner`. 
  Your figure will look similar to county-level [New York Times map](https://www.nytimes.com/elections/results/president).
  
```{r}

county.fips <- maps::county.fips
county.fips <- separate(data=county.fips, col=polyname, into=c("region","subregion"), sep=",")
county.fips$fips <- as.factor(county.fips$fips)
county_winner <- county_winner %>% left_join(county.fips, by="fips")

counties <- counties %>% left_join(county_winner, by=c("region","subregion"))
ggplot(data = counties) +
geom_polygon(aes(x = long, y = lat, fill = candidate, group = group), color = "white") +
coord_fixed(1.3) +
guides(fill=FALSE) 

```
  
10. Create a visualization of your choice using `census` data. Many exit polls noted that 
    [demographics played a big role in the election](https://fivethirtyeight.com/features/demographics-not-hacking-explain-the-election-results/).
    Use [this Washington Post article](https://www.washingtonpost.com/graphics/politics/2016-election/exit-polls/) 
    and [this R graph gallery](https://www.r-graph-gallery.com/) for ideas and inspiration.
    
```{r}
library(ggcorrplot)

mycensus <- census[,c("White","Black","Hispanic", "Asian","Income")]
mycensus <- mycensus[complete.cases(mycensus),]
corr <- cor(mycensus, method = "pearson")
corr

melt_res1 <- melt(corr)
head(melt_res1)

corrPlot1 <- ggcorrplot(corr)
corrPlot1

mycensus <- census[,c("White","Black","Hispanic", "Asian","Unemployment")]
mycensus <- mycensus[complete.cases(mycensus),]
corr1 <- cor(mycensus, method = "pearson")

corrPlot2 <- ggcorrplot(corr1)
corrPlot2

mycensus <- census[,c("Citizen","Income","Poverty", "Transit","Unemployment")]
mycensus <- mycensus[complete.cases(mycensus),]
corr2 <- cor(mycensus, method = "pearson")

corrPlot3 <- ggcorrplot(corr2)
corrPlot3

```
    
    
11. The `census` data contains high resolution information (more fine-grained than county-level).  
    In this problem, we aggregate the information into county-level data by 
    computing `TotalPop`-weighted average of each attributes for each county. Create the following variables:
    
    * _Clean census data `census.del`_: 
      start with `census`, filter out any rows with missing values, 
      convert {`Men`, `Employed`, `Citizen`} attributes to a percentages (meta data seems to be inaccurate), 
      compute `Minority` attribute by combining {Hispanic, Black, Native, Asian, Pacific}, remove {`Walk`, `PublicWork`, `Construction`}.
      
      _Many columns seem to be related, and, if a set that adds up to 100%, one column will be deleted._  
      

    * _Sub-county census data, `census.subct`_: 
      start with `census.del` from above, `group_by()` two attributes {`State`, `County`}, 
      use `add_tally()` to compute `CountyTotal`. Also, compute the weight by `TotalPop/CountyTotal`.
    

    * _County census data, `census.ct`_: 
      start with `census.subct`, use `summarize_at()` to compute weighted sum
    

    * _Print few rows of `census.ct`_: 
    
```{r}

census.del <- census[complete.cases(census),]

census.del <- census.del %>% mutate(Men = 100*Men/(Men + Women))
census.del <- census.del %>% mutate(Employed = 100*Employed/TotalPop)
census.del <- census.del %>% mutate(Citizen = 100*Citizen/TotalPop)

census.del <- census.del %>% mutate(Minority = Hispanic+Black+Native+Asian+Pacific)
census.del <- subset(census.del, select = -c(Walk,PublicWork,Construction))

census.subct <- census.del %>% group_by(State, County) %>% add_tally(TotalPop) %>% mutate(CountyTotal = n) 
                                                    
census.subct <- census.subct %>% mutate(County_weight = TotalPop/CountyTotal) %>% dplyr::select(-n, -CountyTotal) 

census.subct <- subset(census.subct, select = -c(Women, Pacific))
census.ct <- census.subct %>% group_by(State, County) %>% summarize_at(names(.)[4:33] , .funs = funs(sum(.*County_weight)))

census.ct <- ungroup(census.ct)

head(census.ct)

```
    

# Dimensionality reduction

12. Run PCA for both county & sub-county level data. Save the first two principle components PC1 and PC2 into a two-column data frame, call it `ct.pc` and `subct.pc`, respectively. Discuss whether you chose to center and scale the features before running PCA and the reasons for your choice.  What are the features with the largest absolute values in the loadings matrix?

```{r}

county.pca <- prcomp(census.ct[,3:32], center = T, scale. = T)
summary(county.pca)

subcounty.pca <- prcomp(census.subct[,4:34], center = T, scale. = T)
summary(subcounty.pca)

ct.pcaloadings <- county.pca$rotation
ct.pc <- ct.pcaloadings[,c(1,2)]
subct.pcaloadings <- subcounty.pca$rotation
subct.pc <- subct.pcaloadings[,c(1,2)]

datFr <- ct.pcaloadings[,1:2]
datFr

ct.pc <- as.data.frame(ct.pc)
subct.pc <- as.data.frame(subct.pc)

ct.pc.abPC1 <- abs(ct.pc[1])
ct.pc.sorted <- sort(ct.pc.abPC1$PC1, decreasing = T)
sortedctpc1 <- ct.pc.abPC1[order(ct.pc.abPC1$PC1, decreasing=T), , drop=F]

ct.pc.abPC2 <- abs(ct.pc[2])
ct.pc.sortedpc2 <- sort(ct.pc.abPC2$PC2, decreasing = T)
sortedctpc2 <- ct.pc.abPC2[order(ct.pc.abPC2$PC2, decreasing=T), , drop=F]

subct.pc.abPC1 <- abs(subct.pc[1])
subct.pc.sorted <- sort(subct.pc.abPC1$PC1, decreasing = T)

sortedsubctpc1 <- subct.pc.abPC1[order(subct.pc.abPC1$PC1, decreasing=T), , drop=F]

subct.pc.abPC2 <- abs(subct.pc[2])
subct.pc.sortedPC2 <- sort(subct.pc.abPC2$PC2, decreasing= T)
sortedsubctpc2 <- subct.pc.abPC2[order(subct.pc.abPC2, decreasing=T), ,drop=F]

head(sortedsubctpc1, 5)
head(sortedctpc1, 5)
head(sortedsubctpc2, 5)
head(sortedctpc2, 5)

```


Yes, I chose to center and scale the features because all variables don't have the same unit. 

The largest absolute values in the loadings matrix is IncomePerCap, poverty, drive, and Transit. 



13. Determine the number of minimum number of PCs needed to capture 90% of the variance for both the county and sub-county analyses. Plot proportion of variance explained (PVE) and cumulative PVE for both county and sub-county analyses. 

```{r}

pr.var.county <- county.pca$sdev^2
pr.var.subcounty <- subcounty.pca$sdev^2

pve.county <- pr.var.county / sum(pr.var.county)
cumulative_pve.county <- cumsum(pve.county)

pve.subcounty <- pr.var.subcounty / sum(pr.var.subcounty)
cumulative_pve.subcounty <- cumsum(pve.subcounty)

plot(pve.county, xlab="Principal Component",ylab="Proportion of Variance Explained ")
plot(cumulative_pve.county, xlab="Principal Component",ylab="Cumulative PVE ")
abline(h = 0.9)

PC.Num.county <- which(cumulative_pve.county>=0.9)[1]
PC.Num.county

plot(pve.subcounty, xlab="Principal Component",ylab="Proportion of Variance Explained ")
plot(cumulative_pve.subcounty, xlab="Principal Component",ylab="Cumulative PVE ")
abline(h = 0.9)

PC.Num.subcounty <- which(cumulative_pve.subcounty>=0.9)[1]
PC.Num.subcounty

```


Minimum number of PCs needed to capture 90% of the variance for county is 15 and for subcounty is 18. 

# Clustering

14. With `census.ct`, perform hierarchical clustering with complete linkage.  Cut the tree to partition the observations into 10 clusters. Re-run the hierarchical clustering algorithm using the first 5 principal components of `ct.pc` as inputs instead of the original features. Compare and contrast the results. 

```{r}

#14)
numeric.census.ct = scale(census.ct[ ,-c(1,2)], center=TRUE, scale=TRUE)

distanceCensus = dist(numeric.census.ct, method = "euclidean") 
census.hcComp = hclust(distanceCensus, method = "complete")
census.hc10 = cutree(census.hcComp, k=10)
 
distcensus.pc5 = dist(county.pca$x[,1:5], method = "euclidean")
census.pc5 = hclust(distcensus.pc5, "complete")
census.pc5.hc10 = cutree(census.pc5, k=10)
 
table(census.hc10)
table(census.pc5.hc10)

plot(census.hcComp)
plot(census.pc5)

```


The hierarchical clustering with the first 5 principal component generates more distributed number of clustering which is 
more benefitial for our purposes. The hierarchical clustering without 5 pc's has 777 2's. Labeling a large numbers of 
observations to a certain cluster is not an accurate clustering. 


For both approaches investigate the cluster that contains San Mateo County. Which approach seemed to put San Mateo County in a more appropriate clusters? Comment on what you observe and discuss possible explanations for these observations.

```{r}

#14)
SanMateo <- which(census.ct$County == "San Mateo")
census.hc10[227]
census.pc5.hc10[227]

census.hc10.cl3 <- which(census.hc10 == "3")
census.pc5.hc10.cl1 <- which(census.pc5.hc10 == "4")

census.ct[census.hc10.cl3,]
census.ct[census.pc5.hc10.cl1,]

scaled.three = scale(census.hc10.cl3, center=TRUE)
scaled.one = scale(census.pc5.hc10.cl1, center=TRUE)

scaled.three.dist = dist(scaled.three)
scaled.one.dist = dist(scaled.one)

#mean of cluster, average dist of counties 
mean(scaled.three.dist)
mean(scaled.one.dist)

sd(scaled.three.dist)
sd(scaled.one.dist)
```


We choose hierarchical clustering with first 5 PC's since the standard deviation between observation is smaller which 
means the cluster is less distributed. In a less distributed cluster there is a better chance to label San Mateo 
county to the right cluster more accurately.


# Classification

In order to train classification models, we need to combine `county_winner` and `census.ct` data.
This seemingly straightforward task is harder than it sounds. 
Following code makes necessary changes to merge them into `election.cl` for classification.

```{r}

tmpwinner = county_winner %>% ungroup %>%
  mutate(state = state.name[match(state, state.abb)]) %>%               ## state abbreviations
  mutate_at(vars(state, county), tolower) %>%                           ## to all lowercase
  mutate(county = gsub(" county| columbia| city| parish", "", county))  ## remove suffixes
tmpcensus = census.ct %>% mutate_at(vars(State, County), tolower)

election.cl = tmpwinner %>%
  left_join(tmpcensus, by = c("state"="State", "county"="County")) %>% 
  na.omit

## save meta information
election.meta <- election.cl %>% dplyr::select(c(county, fips, state, votes, pct, total))

## save predictors and class labels
election.cl = election.cl %>% dplyr::select(-c(county, fips, state, votes, pct, total))

```

Using the following code, partition data into 80% training and 20% testing:
```{r}
set.seed(10) 
n = nrow(election.cl)
in.trn= sample.int(n, 0.8*n) 
trn.cl = election.cl[ in.trn,]
tst.cl = election.cl[-in.trn,]
```

Using the following code, define 10 cross-validation folds:
```{r}
set.seed(20) 
nfold = 10
folds = sample(cut(1:nrow(trn.cl), breaks=nfold, labels=FALSE))
```

Using the following error rate function:
```{r}
calc_error_rate = function(predicted.value, true.value){
  return(mean(true.value!=predicted.value))
}
records = matrix(NA, nrow=3, ncol=2)
colnames(records) = c("train.error","test.error")
rownames(records) = c("tree","knn","lda")
```

## Classification

15. Decision tree: train a decision tree by `cv.tree()`. Prune tree to minimize misclassification error. Be sure to use the `folds` from above for cross-validation. Visualize the trees before and after pruning. Save training and test errors to `records` variable. Interpret and discuss the results of the decision tree analysis. Use this plot to tell a story about voting behavior in the US (remember the [NYT infographic?](https://archive.nytimes.com/www.nytimes.com/imagepages/2008/04/16/us/20080416_OBAMA_GRAPHIC.html))

```{r}

trn.cl <- select(trn.cl, -region)
trn.cl <- select(trn.cl, -subregion)
tst.cl <- select(tst.cl, -region)
tst.cl <- select(tst.cl, -subregion)

set.seed(1)
candidate.tree=tree(candidate~., data=trn.cl)
draw.tree(candidate.tree, nodeinfo=TRUE, cex=.5)
summary(candidate.tree)

set.seed(1)
cv = cv.tree(candidate.tree, FUN=prune.misclass, K=nfold)
best.size.cv = cv$size[max(which(cv$dev==min(cv$dev)))]
best.size.cv

prunedtree <- prune.tree(candidate.tree, best=best.size.cv)
draw.tree(prunedtree, nodeinfo=TRUE, cex=.65)
summary(prunedtree)


pred.prunetraining = predict(prunedtree, trn.cl, type="class")
records[1,1] <-calc_error_rate(pred.prunetraining, trn.cl$candidate)
pred.prunetests = predict(prunedtree, tst.cl, type="class")
records[1,2] <-calc_error_rate(pred.prunetests, tst.cl$candidate)
records

```

The first tree is a model that is overfitting which has a total classified correct percentage of 93.7%. 
We lose a small amount of accuracy or high variance to get a more interpretable model. 

If you don't use much transit, the decision tree takes us to the left, where the next variable is white. 
If you're over 48% white, we then move to the variable, Asian. If you are not Asian, the decision tree then 
says that the outcome would be Donald Trump, but if you do hold asian heritage, you would most likely vote 
for Hillary Clinton. On the other hand, if you were less than 48% white, we would move to the variable of 
Unemployment. people who were not really unemployed much would most likely vote for Donald Trump, while people 
who were unemployed or have been unemployed for awhile would vote for Hillary Clinton. Now, if you did ride 
through Transit a great amount, more than 1.1177 times a week per say, it would bring us to the right of the 
decision tree, where the next variable is Asian. If you were not really Asian, it would then take us to Black. 
and people who were more than 48% Black would be more in favor for Hillary Clinton while people who were less 
than 48% Black tended to swing in favor of Donald Trump. Again, on the other hand, if a person were Asian and 
had a transportation method of Transit, they would most likely be in favor of Hillary Clinton.


16. Run a logistic regression to predict the winning candidate in each county.  Save training and test errors to `records` variable.  What are the significant variables? Are the consistent with what you saw in decision tree analysis? Interpret the meaning of a couple of the significant coefficients.  

```{r}

#16)

glmcandidate = glm(candidate ~ ., data=trn.cl, family=binomial)
summary(glmcandidate)

newtrain=predict(glmcandidate, data=trn.cl, type="response")
predict.train <- trn.cl %>% mutate(predict.train.values=(ifelse(newtrain>.50, "Hillary Clinton", "Donald Trump")))

newtest=predict(glmcandidate, tst.cl, type="response")
predict.test <- tst.cl %>% mutate(predict.test.values=(ifelse(newtest>.50, "Hillary Clinton", "Donald Trump")))

records[2,1] <-calc_error_rate(predict.train$predict.train.values, trn.cl$candidate)
records[2,2] <-calc_error_rate(predict.test$predict.test.values, tst.cl$candidate)
records

```


The significant variables are citizen, professional, service, IncomePerCap, Employed, Unemployed, Asian, and Black. 
Yes, Asian, Black, and Unemployment were consistent with what we saw in the decision tree. 
Race plays an important role in the outcome of the candidate, such as being Black, Asian, or White, all of which have
candidates. Citizen is another variable that is important, because both Donald Trump and Hillary Clinton have very different
views on what should happen if a person is a citizen or not. 


17.  You may notice that you get a warning `glm.fit: fitted probabilities numerically 0 or 1 occurred`.  As we discussed in class, this is an indication that we have perfect separation (some linear combination of variables _perfectly_ predicts the winner).  This is usually a sign that we are overfitting. One way to control overfitting in logistic regression is through regularization.  Use the `cv.glmnet` function from the `glmnet` library to run K-fold cross validation and select the best regularization parameter for the logistic regression with LASSO penalty.  Reminder: set `alpha=0` to run LASSO. 


```{r}

set.seed(1)
x.trn <- model.matrix(candidate ~ . , trn.cl)[,-1] #model matrix for training dat
y <- droplevels(as.factor(trn.cl$candidate))

lasso.cv <- cv.glmnet(x.trn, y , alpha=1, family = "binomial")

bestlam = lasso.cv$lambda.min
bestlam
```

What are the non-zero coefficients in the LASSO regression for the optimal value of $\lambda$? How do they compare to the unpenalized logistic regression? Save training and test errors to the `records` variable.

```{r}

lasso_mod = glmnet(x.trn, y, alpha = 1, family="binomial", lambda  = bestlam)
lasso.coef <- coef(lasso_mod)
which(lasso.coef != 0)

x.tst <- model.matrix(candidate ~ . , tst.cl)[,-1] #model matrix for training dat

prob.train.lasso = predict(lasso_mod, newx =x.trn , type="response")
prob.test.lasso = predict(lasso_mod, newx = x.tst, type="response")

pred.lasso.train <- trn.cl %>% mutate(predict.train.lasso=(ifelse(prob.train.lasso>.50, "Hillary Clinton", "Donald Trump")))
pred.lasso.test <- tst.cl %>% mutate(predict.test.lasso=(ifelse(prob.test.lasso>.50, "Hillary Clinton", "Donald Trump")))

records[3,1] <-calc_error_rate(pred.lasso.train$predict.train.lasso, trn.cl$candidate )
records[3,2] <-calc_error_rate(pred.lasso.test$predict.test.lasso, tst.cl$candidate)
rownames(records) <- c("tree", "Log", "Lasso")
records
```

18.  Compute ROC curves for the decision tree, logistic regression and LASSO logistic regression using predictions on the test data.  Display them on the same plot.  Based on your classification results, discuss the pros and cons of the various methods.  Are different classifiers more appropriate for answering different kinds of problems or questions?


```{r}

library(ROCR)
pred.tree <- predict(prunedtree, tst.cl, type = "vector")
perf.tree <- performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.log <- predict(glmcandidate, tst.cl, type = "response")
perf.log <- performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "tpr", "fpr")

pred.lasso <- predict(lasso_mod, newx =x.tst, s = bestlam, type="response")
perf.lasso <- performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "tpr", "fpr")

tree.lm = plot(perf.tree, col=2, lwd=3, text=T, main="ROC Curve")
log.lm = plot(perf.log, add=TRUE, col=3, lwd=3)
lasso.lm = plot(perf.lasso, add=TRUE, col=4, lwd=3)

legend("right", legend=c("decision tree", "logistic","lasso logistic"), col = c(2,3,4),lty=1:1)
abline(0,1)

auc = performance(prediction(pred.tree[,13], as.numeric(tst.cl$candidate)), "auc")@y.values
auc

auc1 = performance(prediction(pred.log, as.numeric(tst.cl$candidate)), "auc")@y.values
auc1
    
auc2 = performance(prediction(pred.lasso, as.numeric(tst.cl$candidate)), "auc")@y.values
auc2
```


Pros of Logistic Regression: It gives us the probability or weight of importance of each predictor
which is a more accurate way of interpreting the predictors. We can control the false positive rate 
(FPR) and true positive rate to get the desirable outcomes we are looking for. 

Cons of logistic Regression: Although we have considerable number of predictors, We may experience
the Simpson's effect in our prediction of winner of the election. 


Pros of Decision Tree: Generally easy to interpret especially when the subject is political discourse
like presidential election. 

Cons of Decision Tree: Predictive accuracy is poor when decision boundaries cut across predictors, and 
this could be a major issue for media outlets that try to be creditentials and for those companies 
whose the accuracy and precision of their work should be observed by matching to the actual result. 
Typically very high variance. A small change in the data might lead to learning a very different tree


Lasso Regression: Datasets With high dimensional features, regularization is critical to avoid overfitting. 

Decision Tree was more interpretable and easy to run, but it gave us the least Area under curve (AUC), 
meaning that it doesn't have the best results. Logistic Regression and Lasso were not the easiest methods to interpret, 
but both methods were very similar to each other. Both Logistic Regression and Lasso had almost identical training and test errors, 
also noting both errors with these two methods were smaller than the errors for the decision tree method. In the end, Lasso had the 
biggest AUC, making it have the best and most accurate results, also showing that it was a great method that was appropriate for answering 
these questions.



# Taking it further

19. This is an open question. Interpret and discuss any overall insights gained in this analysis and possible explanations. Use any tools at your disposal to make your case: visualize errors on the map, discuss what does/doesn't seems reasonable based on your understanding of these methods, propose possible directions (collecting additional data, domain knowledge, etc).  In addition, propose and tackle _at least_ one more interesting question. Creative and thoughtful analyses will be rewarded! _This part will be worth up to a 20\% of your final project grade!  

#Some possibilities for further exploration are:

  * Data preprocessing: we aggregated sub-county level data before performing classification. Would classification at the sub-county level before determining the winner perform better? What implicit assumptions are we making?
  
Rural area had a huge impact on 2016 election outcome since most people living in those areas voted for Trump. When we classify the whole county as red or blue, 
we may miss benefitial information about the rural areas which were Trump supporters. The demographic of sub-county could perform better in determining the winner. 
We implicitly assume in big counties with Clinton's outcome, we missed the demographic of Trump voters who their votes outnumbered because they are living in rural areas. 



  * Exploring additional classification methods: KNN, LDA, QDA, SVM, random forest, boosting etc. (You may research and use methods beyond those covered in this course). How do these compare to logistic regression and the tree method?
  
```{r}
library(randomForest)

trn.cl$candidate <- factor(trn.cl$candidate)
rf.candidate = randomForest(candidate ~ ., data=trn.cl, importance=TRUE)
rf.candidate
importance(rf.candidate)
varImpPlot(rf.candidate, cex=.7)

#BOOSTING
library(gbm)
set.seed(1)
boostedcandidatemodel <- gbm(ifelse(candidate=="Hillary Clinton",1,0)~.,data=trn.cl, n.trees=1000, shrinkage=.01, distribution="bernoulli")
head(summary(boostedcandidatemodel))

boostedcandidatemodel
summary(boostedcandidatemodel)

#SVM 
library(e1071)

svmfit=svm(ifelse(candidate=="Hillary Clinton",1,0)~., data=trn.cl, kernel="radial", cost=1,scale=TRUE)
#svmfit

pred.svm <- predict(svmfit, newdata=tst.cl)
#pred.svm
tst.cl <- tst.cl %>% mutate(pred.svm=pred.svm)
 confusionmatrix = table(pred = pred.svm, truth =tst.cl$candidate)
 #confusionmatrix

set.seed(1) 
tune.out=tune(svm,candidate~.,data=trn.cl,kernel="radial",
ranges=list(cost=c(0.001, 0.01, 0.1,1,10,100)))
tune.out
tune.out$best.model

svmfitbestcost=svm(ifelse(candidate=="Hillary Clinton",1,0)~., data=trn.cl, kernel="radial", cost=10,scale=TRUE)
#svmfitbestcost

pred.svmbestcost <- predict(svmfitbestcost, newdata=tst.cl)

confusionmatrixbestcost= table(pred=pred.svmbestcost, truth= tst.cl$candidate)
#confusionmatrixbestcost

#KNN
set.seed(333)

YTrain = trn.cl$candidate
XTrain = trn.cl %>% select(-candidate)
# YTest is the true labels for High on the test set, Xtest is the design matrix
YTest = tst.cl$candidate
XTest = tst.cl %>% select(-candidate)

validation.error = NULL
allK = 1:50

set.seed(66)
for (i in allK) {                                               # Loop through different number of neighbors
pred.Yval = knn.cv(train=XTrain, cl=YTrain, k=i)                # Predict on the left-out validation set
validation.error = c(validation.error, mean(pred.Yval!=YTrain)) # Combine all validation errors
                 }
     
validation.error
     
numneighbor = max(allK[validation.error == min(validation.error)]) 
numneighbor

set.seed(67)
pred.YTest = knn(train=XTrain, test=XTest[,-28], cl=YTrain, k=numneighbor)

conf.matrix = table(predicted=pred.YTest, true=ifelse(YTest=="Hillary Clinton",1,0)) 
conf.matrix

sum(diag(conf.matrix)/sum(conf.matrix))

1 - sum(diag(conf.matrix)/sum(conf.matrix))
```



For taking if further, we decided to explore data preprocessing and further classification methods, such as Random Forests, 
boosting, Support Vector Machines, and K Nearest Neighbors.

RANDOM FOREST: Through Random Forest, we can see that important variables are white, transit, asian, black, 
and professional in both mean decrease accuracy and mean decrease Gini. These variables are significant and are very similar 
to the results we got  in both logsitic regression, and the decision tree methods.

BOOSTING: In the Boosting method, we also got that the most important variables are Transit, White, Black, Asian, Minority, 
and Professional, which is the same as the random forest method and previous methods.

SVM: For SVM, we first did a model where cost was equal to 1 while doing a sampling method of 10 fold cross validation. We then found 
the results that the optimal cost was equal to 10. We ran it again, where cost was equal to 10  this time, and found that the best model 
had a cost of 10 and gamma equal to .0333333333

KNN: For KNN, we first made a ytrain, ytest, xtrain, and xtest. We set the possible number of nearest neighbors to be considered to be 1 to 50.
We found the results that the best number of neighbors with the smallest validation error was 31. The test accuracy for running this KNN method 
was equal to 86.17%, and a test error of .1382114, which were both fairly good results. 




  * Bootstrap: Perform bootstrap to generate plots similar to ISLR Figure 4.10/4.11. Discuss the results. 
  * Use linear regression models to predict the `total` vote for each candidate by county.  Compare and contrast these results with the classification models.  Which do you prefer and why?  How might they complement one another?


    
  * Conduct an exploratory analysis of the "purple" counties-- the counties which the models predict Clinton and Trump were roughly equally likely to win.  What is it about these counties that make them hard to predict?
    
  * Instead of using the native attributes (the original features), we can use principal components to create new (and lower dimensional) set of features with which to train a classification model.  This sometimes improves classification performance.  Compare classifiers trained on the original features with those trained on PCA features.  













