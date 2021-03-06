Do older planes suffer more delays?
================
J Sudharsaan
3/6/2022

**Setting up working directory**

``` r
setwd("C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files")
getwd()
```

    ## [1] "C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files"

**Installing & Loading packages in R**

``` r
#install.packages("DBI")
library(DBI)

#install.packages("dplyr")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggthemes")
library(ggthemes)
```

**Creating connection with airlinemain\_r.db (already created)**

``` r
conn <- dbConnect(RSQLite::SQLite(), "airlinemain_r.db")
```

**Query on question 2 of coursework**

**Dealing with potentially erroneous data**

**Assumptions**: “year” column in plane-data.csv is assumed to be the
year the planes were manufactured. The “Year” column in the ontime table
created refers to the year the plane was used to fly. We will be using
the values in the “Year” column in the ontime table created &
subtracting its value by the corresponding value in the “year” column by
matching the tail number of the plane to calculate the age of the plane
& answer the question regarding whether older planes suffer more
delays".

``` r
q2_erroneous1 <- dbSendQuery(conn,
                  "SELECT ontime.Year AS Flight_Year, planes.year AS Manufacture_Year, (ontime.Year - planes.year) AS Plane_Age
                  FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                  WHERE (ontime.Year - planes.year) < 0
                  ORDER BY Plane_Age DESC")
q2_erroneous1 <- dbFetch(q2_erroneous1)
head(q2_erroneous1) # The q2_erroneous1 variable contains a dataframe of 1817 rows. 
```

    ##   Flight_Year Manufacture_Year Plane_Age
    ## 1        2006             2007        -1
    ## 2        2006             2007        -1
    ## 3        2006             2007        -1
    ## 4        2006             2007        -1
    ## 5        2006             2007        -1
    ## 6        2006             2007        -1

``` r
# Finding maximum & minimum values in the year column to set the appropriate limit on the lowest & highest possible age of planes.

q2manufactureyearmax = dbSendQuery(conn,
                                   "SELECT max(year)
                                    FROM planes
                                    WHERE year < 2100")
```

    ## Warning: Closing open result set, pending rows

``` r
q2manufactureyearmax = dbFetch(q2manufactureyearmax)
q2manufactureyearmax # The most recently built planes were manufactured in 2008. Given that our dataset holds values of planes which have been used to fly in 2006 & 2007, the "youngest" plane would be of 0 years old. 
```

    ##   max(year)
    ## 1      2008

``` r
q2manufactureyearmin = dbSendQuery(conn,
                                   "SELECT min(year)
                                    FROM planes
                                    WHERE year > 1900")
```

    ## Warning: Closing open result set, pending rows

``` r
q2manufactureyearmin = dbFetch(q2manufactureyearmin)
q2manufactureyearmin # The oldest possible planes built given our dataset were in 1946. Thus, given that our dataset holds values of planes which have flown in 2006 & 2007, the "oldest" plane would be of 61 years old (2007 - 1946 = 61).
```

    ##   min(year)
    ## 1      1946

``` r
# The query below allows us to identify more erroneous data in the form of missing values in the year column of the planes dataset.
q2_erroneous2 <- dbSendQuery(conn,
                             "SELECT ontime.Year, planes.year, (ontime.Year - planes.year)
                              FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                              WHERE (ontime.Year - planes.year) > 61
                              ORDER BY (ontime.Year - planes.year)")
```

    ## Warning: Closing open result set, pending rows

``` r
q2_erroneous2 <- dbFetch(q2_erroneous2)
head(q2_erroneous2) # There are a total of 1062909 missing values in the year column according to the query above. Take note that in python, the same query of q2_erroneous2 only identifies 366493 missing values. We are unable to find other missing values in python. We take note of these additional 700000+ missing values in R. We are also unable to fill these missing values with appropriate values due to lack of information given the dataset. Thus, we will neglect these values & proceed with our analysis. 
```

    ##   Year year (ontime.Year - planes.year)
    ## 1 2006 None                        2006
    ## 2 2006                             2006
    ## 3 2006                             2006
    ## 4 2006 None                        2006
    ## 5 2006                             2006
    ## 6 2006                             2006

**Reasons the above result is considered erroneous data**

We will consider the observations that are within the object
“q2\_erroneous1” as erroneous data since (based on the assumptions made)
the plane age cannot be of negative value (i.e. The year that the planes
took flight cannot be before the year the planes were manufactured.) We
also consider the observations of variable “q2\_erroneous2” are
erroneous data since it has missing values in the year column of the
plane dataset. Thus, we will be neglecting all these data entries in the
analysis later.

**Querying the dataset to answer question regarding whether older planes
suffer more delays**

``` r
# Querying from airlinemain_r.db database to extract information regarding the year the plane was flown, the year the plane was manufactured, & the age of the plane (given the year of flight & manufacture). 
q2distribution <- dbSendQuery(conn,
                              "SELECT (ontime.Year - planes.year), COUNT(*)
                               FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                               WHERE (ontime.Year - planes.year) > -1 AND (ontime.Year - planes.year) < 62
                               GROUP BY (ontime.Year - planes.year)
                               ORDER BY (ontime.Year - planes.year)")
```

    ## Warning: Closing open result set, pending rows

``` r
q2distribution <- dbFetch(q2distribution)
q2distribution <- as.data.frame(q2distribution)
names(q2distribution) <- c("Age of Plane", "Total flights")
head(q2distribution) # Exact same dataset as that of python with approximately 12 million data entries.
```

    ##   Age of Plane Total flights
    ## 1            0        169302
    ## 2            1        508707
    ## 3            2        651319
    ## 4            3        822905
    ## 5            4        879383
    ## 6            5        986362

**Data visualization (histogram) illustrating the distribution of
flights by age of plane**

``` r
q2distribution_colours <- brewer.pal(n = 9, name = "Blues")

barplot(height = as.vector(q2distribution[,2]), xlab = "Age of Planes", ylab = "Number of Flights", main = "Figure 2.1: Distribution of Number of Flights by Age of Plane", col = q2distribution_colours[5], ylim = c(0,1000000), cex.names = 0.5, cex.axis = 0.7)
```

![](rmarkdown_qn2_github_files/figure-gfm/Plotting%20histogram%20illustrating%20distribution%20of%20flights%20by%20age%20of%20plane-1.png)<!-- -->

**Analysis of histogram above**

The number of observations are significantly greater for plane models
that are within the 2-9 years of age compared to those that are of 10-22
years of age. The number of observations for plane models that are older
than 22 years are significantly lower compared to the number of
observations of plane that have less than 22 years of age.

It is important to note that greater number of observations allow for
more accurate sample statistics as the greater the size of the sample,
the closer the sample measures (E.g. mean, median, etc.) are to the true
value of the population measures. Thus, the average arrival delay of
planes that are older than 22 years of age may not be as accurate as
those below 22 years of age due to the relatively small sample size
compared to the sample size of planes that have less than 22 years of
age.

**Methodology to be used to answer the question**

Given the distribution of the number of flights by the age of planes, we
shall: - Plot a scatterplot on the average arrival delay based on the
age of plane - Plot a scatterplot on the percentage of delayed flights
based on age of plane

There will be a total of **4 scatterplots** in which 2 of the
scatterplots (one of average arrival delay & the other on percentage of
delayed flights) are based on the entire dataset of age of planes from 0
to 51 years of age while 2 of the other scatterplots (one on average
arrival delay & the other on percentage of delayed flights) are based on
part of the dataset (Age of planes from 0 to 22 years of age) that have
relatively large sample size compared to the rest of the dataset which
will be deemed as the representative dataset.

**Querying data to produce scatterplots on average arrival delay based
on age of plane**

``` r
q2 <- dbSendQuery(conn,
                  "SELECT (ontime.Year - planes.year), AVG(ArrDelay), COUNT(*)
                   FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                   WHERE (ontime.Year - planes.year) > -1 AND (ontime.Year - planes.year) < 62
                   GROUP BY (ontime.Year - planes.year)
                   ORDER BY (ontime.Year - planes.year) ASC")
```

    ## Warning: Closing open result set, pending rows

``` r
q2 <- dbFetch(q2)
q2 <- as.data.frame(q2)
names(q2) <- c("Age of Plane", "Average Arrival Delay (mins)", "Total Flights")
head(q2)
```

    ##   Age of Plane Average Arrival Delay (mins) Total Flights
    ## 1            0                     9.255117        169302
    ## 2            1                     9.203893        508707
    ## 3            2                     9.585009        651319
    ## 4            3                     9.367449        822905
    ## 5            4                    10.391609        879383
    ## 6            5                     9.307230        986362

``` r
# "Representative" dataset containing data on average arrival delay of planes of age 0 to 22. 
q2representative <- q2[-c(23:51),]
head(q2representative)
```

    ##   Age of Plane Average Arrival Delay (mins) Total Flights
    ## 1            0                     9.255117        169302
    ## 2            1                     9.203893        508707
    ## 3            2                     9.585009        651319
    ## 4            3                     9.367449        822905
    ## 5            4                    10.391609        879383
    ## 6            5                     9.307230        986362

**Data visualization (scatter plot) illustrating how average arrival
delay of planes vary with their age**

``` r
q2_scatterplot <- ggplot(q2) + geom_point(aes(x = q2[,1], y = q2[,2])) +
                  labs(title = "Figure 2.2.1: Average Arrival Delay against Age of Plane", x = "Age of Plane", y = "Average Arrival Delay in mins") + geom_smooth(aes(x = q2[,1], y = q2[,2]), se = FALSE, method = lm, colour = "black") + theme_economist()

q2_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn2_github_files/figure-gfm/Scatterplots%20illustrating%20how%20avg%20arrival%20delay%20of%20planes%20vary%20with%20their%20age-1.png)<!-- -->

``` r
q2representative_scatterplot <- ggplot(q2representative) + geom_point(aes(x = q2representative[,1], y = q2representative[,2])) +
                  labs(title = "Figure 2.2.2: Average Arrival Delay against Age of Plane (Representative)", x = "Age of Plane", y = "Average Arrival Delay in mins") + geom_smooth(aes(x = q2representative[,1], y = q2representative[,2]), se = FALSE, method = lm, colour = "black") + theme_economist() + theme(plot.title = element_text(size = 13))

q2representative_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn2_github_files/figure-gfm/Scatterplots%20illustrating%20how%20avg%20arrival%20delay%20of%20planes%20vary%20with%20their%20age-2.png)<!-- -->

**Analysis of query results & scatterplots above**

We can observe from the linear regression line in **Figure 2.2.1** that
generally as the age of the plane increases, the average arrival delay
in minutes also increases.

Though we observe that average arrival delay beyond 35 years of age is
substantially lower than the planes that are less than 35 years of age,
it is important to take note from the histogram in **Figure 2.1** above
that the sample size of planes’ age that are of **23 years & above** are
much smaller compared to those lower than 23 years of age. Thus, their
values may not be as representative of the true value of their
corresponding average arrival delay.

We can observe from the linear regression line in **Figure 2.2.2** that
as the age of the plane increases, there is a observable increase in
average arrival delay in minutes. The **regression line** in Figure
2.2.2 based on the representative sample is much more **steeper**
compared to the regression line in Figure 2.2.1 based on the entire
dataset (Age 0 to 51).

Thus, we can conclude in both cases of the entire dataset & the
representative dataset that the average arrival delay increases as the
age of plane increases.

**Querying data to produce scatterplots on percentage of delayed flights
based on age of plane**

``` r
q2delaycount <- dbSendQuery(conn,
                            "SELECT (ontime.Year - planes.year), (AVG(ArrDelay)), COUNT(*), SUM(CASE WHEN ArrDelay > 0 THEN 1 ELSE 0 END), ((CAST(SUM(CASE WHEN ArrDelay > 0 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT))*100)
                            FROM ontime JOIN planes ON ontime.TailNum = planes.tailnum
                            WHERE (ontime.Year - planes.year) > -1 AND (ontime.Year - planes.year) < 62
                            GROUP BY (ontime.Year - planes.year)
                            ORDER BY (ontime.Year - planes.year) ASC")
```

    ## Warning: Closing open result set, pending rows

``` r
q2delaycount <- dbFetch(q2delaycount)
q2delaycount <- as.data.frame(q2delaycount)
names(q2delaycount) <- c("Age of Plane", "Average Arrival Delay (mins)", "Total Flights", "Delayed Flights", "Percentage of Delayed Flights")
head(q2delaycount)
```

    ##   Age of Plane Average Arrival Delay (mins) Total Flights Delayed Flights
    ## 1            0                     9.255117        169302           78122
    ## 2            1                     9.203893        508707          234077
    ## 3            2                     9.585009        651319          300714
    ## 4            3                     9.367449        822905          375452
    ## 5            4                    10.391609        879383          416091
    ## 6            5                     9.307230        986362          451127
    ##   Percentage of Delayed Flights
    ## 1                      46.14358
    ## 2                      46.01411
    ## 3                      46.17000
    ## 4                      45.62519
    ## 5                      47.31624
    ## 6                      45.73645

``` r
# "Representative" dataset containing data on average arrival delay of planes of age 0 to 22. 
q2delaycountrepresentative <- q2delaycount[-c(23:51),]
head(q2delaycountrepresentative)
```

    ##   Age of Plane Average Arrival Delay (mins) Total Flights Delayed Flights
    ## 1            0                     9.255117        169302           78122
    ## 2            1                     9.203893        508707          234077
    ## 3            2                     9.585009        651319          300714
    ## 4            3                     9.367449        822905          375452
    ## 5            4                    10.391609        879383          416091
    ## 6            5                     9.307230        986362          451127
    ##   Percentage of Delayed Flights
    ## 1                      46.14358
    ## 2                      46.01411
    ## 3                      46.17000
    ## 4                      45.62519
    ## 5                      47.31624
    ## 6                      45.73645

**Data visualization (scatter plot) illustrating how percentage of
delayed flights vary with their age**

``` r
q2delaycount_scatterplot <- ggplot(q2delaycount) + geom_point(aes(x = q2delaycount[,1], y = q2delaycount[,5])) +
                  labs(title = "Figure 2.3.1: Percentage of Delayed flights against Age of Plane", x = "Age of Plane", y = "Percentage of Delayed flights") + geom_smooth(aes(x = q2delaycount[,1], y = q2delaycount[,5]), se = FALSE, method = lm, colour = "black") + theme_wsj() + theme(axis.title = element_text(size = 8), plot.title = element_text(size = 11)) + ylim(40, 60)

q2delaycount_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn2_github_files/figure-gfm/Scatterplots%20illustrating%20how%20percentage%20of%20delayed%20flights%20vary%20with%20their%20age-1.png)<!-- -->

``` r
q2delaycountrepresentative_scatterplot <- ggplot(q2delaycountrepresentative) + geom_point(aes(x = q2delaycountrepresentative[,1], y = q2delaycountrepresentative[,5])) +
                  labs(title = "Figure 2.3.2: Percentage of Delayed flights against Age of Plane (Representative)", x = "Age of Plane", y = "Percentage of Delayed flights") + geom_smooth(aes(x = q2delaycountrepresentative[,1], y = q2delaycountrepresentative[,5]), se = FALSE, method = lm, colour = "black") + theme_wsj() + theme(axis.title = element_text(size = 8), plot.title = element_text(size = 9)) + ylim(44, 50)

q2delaycountrepresentative_scatterplot
```

    ## `geom_smooth()` using formula 'y ~ x'

![](rmarkdown_qn2_github_files/figure-gfm/Scatterplots%20illustrating%20how%20percentage%20of%20delayed%20flights%20vary%20with%20their%20age-2.png)<!-- -->

**Analysis of query results & scatterplots above**

**Figure 2.3.1** shows the percentage of delayed flights based on the
age of the plane. Based on the Figure 2.3.1, the regression line shows
an increase in the percentage of delayed flights as the age of the plane
increases.

As previously observed, the percentage of delayed flights for planes
that are older than 22 years may not be accurate due to the smaller
sample size available relative to the sample size of planes that are 22
years old & less old.

We can observe from the linear regression line in **Figure 2.3.2** that
as the age of the plane increases, there is a observable increase in
percentage of delayed flights. The **regression line** in Figure 2.3.2
based on the representative sample is much more **steeper** compared to
the regression line in Figure 2.3.1 based on the entire dataset (Age 0
to 51).

Thus, we can conclude in both cases of the entire dataset & the
representative dataset that the percentage of delayed flight increases
as the age of plane increases.

**Overall Conclusion**

Based on the scatter plot, bar plot, regression lines & data we have
obtained, we make 2 observations; 1. We can observe from the linear
regression line (blue) that the **average arrival delay in mins of
planes increases** as the **age of the plane increases**. 2. We can also
observe from the linear regression line (green) that the **percentage of
delayed flights increases** as the **age of the plane increases**.

The observations made above are consistent for both the entire dataset
(Plane Age 0 to 51) & the “representative” dataset (Plane Age 0 to 22)
which the **regression line** based on the **“representative” dataset**
being **steeper** than the **regression line** based on the **entire
dataset**.

We also note that: - The sample size of planes older than 22 years of
age (based on the histogram in Figure 2.1) are significantly lower than
those that are less than 23 years of age. This indicates that there is
possibility for the sample measures (e.g. mean) for planes older than 22
years of age to not be as representative of the true value of its
population compared to the planes less than 23 years of age. - This may
explain the significantly lower values of average arrival delay &
percentage of delayed flights for some of the planes that are older than
35 years of age.

Given the information obtained, we conclude that **older planes suffer
more delays**.

**Disconnect from database**

``` r
dbDisconnect(conn)
```

    ## Warning in connection_release(conn@ptr): There are 1 result in use. The
    ## connection will be released when they are closed
