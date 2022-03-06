Best time of year, day of week, & time of day to fly to minimise delays
================
J Sudharsaan
3/6/2022

**Setting up working directory**

Setting the working directory within location of necessary Harvard
Dataverse files is performed beforehand using the files pane where we
click the cog icon that says “More” & click “Set as working directory”.
We can also set up working directory using shortcut “Ctrl+Shift+H”

``` r
setwd("C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files")

getwd()
```

    ## [1] "C:/Users/Sudharsaan/OneDrive/SIM/Year 2/ST2195 Programming for Data Science/Coursework/Essential Harvard Dataverse Files"

**Installation of packages**

Installing necessary packages. We will be using DBI package primary to
query the database created. The RColorBrewer package allows us to create
cool looking data visualizations with the queries performed.

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
display.brewer.all(colorblindFriendly = TRUE) # display all colour palettes for future reference when creating data visualizations.
```

![](rmarkdown_qn1_github_files/figure-gfm/Installation%20of%20packages-1.png)<!-- -->

**Deleting database if it exists in preparation for knitting to html**

``` r
if (file.exists("airlinemain_r.db"))
  file.remove("airlinemain_r.db")
```

    ## [1] TRUE

**Creation of database**

The tables created within the airlinemain\_r.db database are the 4
tables shown below. These are the 4 tables from which the queries will
primarily occur from. Other tables that are created in the database
would be subsets of any combinations of the 4 tables below.

1.  ontime table contains data from 2006.csv.bz2 & 2007.csv.bz2 files.
2.  airports table contains data from airports.csv
3.  carriers table contains data from carriers.csv
4.  planes tables contains data from plane-data.csv

``` r
conn <- dbConnect(RSQLite::SQLite(), "airlinemain_r.db")

# Loading in data from the csv files into 2 different variables which will be merged using the append function to form the ontime table.
ontime2006 <- read.csv("2006.csv.bz2", header = TRUE)
ontime2007 <- read.csv("2007.csv.bz2", header = TRUE)

# Using dbWriteTable to create the ontime table with the help of the append argument to merge the 2 variables created above. We are also writing the table to the airlinemain_r.db database.
dbWriteTable(conn, "ontime", ontime2006)
dbWriteTable(conn, "ontime", ontime2007, append = TRUE)

# Loading in data from the airports, carriers, & planes csv files & writing them to the arilinemain_r.db database.
airports <- read.csv("airports.csv", header = TRUE)
carriers <- read.csv("carriers.csv", header = TRUE)
planes <- read.csv("plane-data.csv", header = TRUE)

dbWriteTable(conn, "airports", airports)
dbWriteTable(conn, "carriers", carriers)
dbWriteTable(conn, "planes", planes)

dbListTables(conn)
```

    ## [1] "airports" "carriers" "ontime"   "planes"

**Query on question 1 of coursework**

We will now split the query for question 1 into 3 parts. - Firstly we
will analyse the best month of the year to fly to minimize delays. -
Secondly, we will analyse the best day of the week to fly to minimize
delays. - Thirdly, we will analyse the best time of day to fly to
minimize delays.

**1(a) Best month of the year to fly to minimize delays**

**Tables used**: ontime table

**Variables used**: Month, ArrDelay, Cancelled & Diverted

**Reason for variables used**: ArrDelay is used instead of DepDelay
because in the perspective of passengers, passengers will not be
bothered by departure delay as long as they can arrive in their
destination on time. Thus, ArrDelay is a more appropriate variable to
use in the context of passengers over DepDelay. We also find the
percentage of Cancelled flights, percentage of diverted flights, &
percentage of delayed flights by month as it would not be ideal for
passengers to book flights in a month where arrival delay is low but
percentage of cancelled flights, diverted, or delayed flights are high.
Thus, we will look at both percentage of cancelled, diverted, & delayed
flights, & arrival delay to decide on the best month to fly to minimize
delays.

**Additional information**: We do not restrict arrival delay values to
more than 0 since flights that reach ahead of schedule influence the
average arrival delay in a sensible way. We also restrict the query to
flights that have not been cancelled or diverted as technically they
themselves are forms of delays that passengers experience. We instead
calculate percentage of cancelled & diverted flights by month of year to
be included in our analysis.

``` r
# We query the values of percentage of cancelled, diverted, & delayed flights by month of year to be included in decision of best month to fly to minimize delays. Due to rmarkdown, the code seems to be indented incorrectly.
q1a1 <- dbSendQuery(conn,
                   "SELECT Month, (AVG(Cancelled)*100), (AVG(Diverted)*100), ((CAST(SUM(CASE WHEN ArrDelay > 0 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT))*100)
                   FROM ontime
                   GROUP BY Month
                   ORDER BY Month")
q1a1 <- dbFetch(q1a1)
q1a1 <- as.data.frame(q1a1)
names(q1a1) <- c("Months", "% of Cancelled flights", "% of Diverted flights", "% of Delayed flights")
q1a1
```

    ##    Months % of Cancelled flights % of Diverted flights % of Delayed flights
    ## 1       1               2.125293             0.2136599             43.18741
    ## 2       2               3.351230             0.2343071             47.19538
    ## 3       3               1.965806             0.1870742             45.49399
    ## 4       4               1.478501             0.1851668             43.59504
    ## 5       5               1.125774             0.2169250             43.12280
    ## 6       6               2.226386             0.3264920             50.03800
    ## 7       7               1.909035             0.3172143             48.54119
    ## 8       8               1.722138             0.2863470             46.36583
    ## 9       9               1.388631             0.1855502             40.55289
    ## 10     10               1.508082             0.1798327             45.59229
    ## 11     11               1.329337             0.1636804             41.99393
    ## 12     12               3.247772             0.2385764             50.86254

``` r
q1a2 <- dbSendQuery(conn,
                    "SELECT Month, AVG(ArrDelay) as Avg_ArrDelay
                    FROM ontime
                    WHERE Cancelled = 0 AND Diverted = 0
                    GROUP BY Month
                    ORDER BY Month")
```

    ## Warning: Closing open result set, pending rows

``` r
q1a2 <- dbFetch(q1a2)
q1a2 <- as.data.frame(q1a2)
names(q1a2) <- c("Month", "Average Arrival Delay (mins)")
q1a2
```

    ##    Month Average Arrival Delay (mins)
    ## 1      1                     7.447226
    ## 2      2                    10.498526
    ## 3      3                     9.027283
    ## 4      4                     7.491780
    ## 5      5                     6.964305
    ## 6      6                    14.153010
    ## 7      7                    12.736940
    ## 8      8                    10.662703
    ## 9      9                     6.034441
    ## 10    10                     8.559212
    ## 11    11                     6.002432
    ## 12    12                    13.709167

**Data visualization (barplot) illustrating best month to fly to
minimize delays**

``` r
Months <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

q1a1_colours <- brewer.pal(n = 12, name = "Paired")

# Plotting a bar plot on the Percentage of Cancelled flights by month. 
barplot(height = as.vector(q1a1[,2]), ylim = c(0, 3.5), names.arg = Months, xlab = "Months", ylab = "Percentage of Cancelled flights", main = "Figure 1.1a: Percentage of Cancelled flights by Month", col = q1a1_colours[1], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20month%20to%20fly%20to%20minimize%20delays-1.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Diverted flights by month. 
barplot(height = as.vector(q1a1[,3]), ylim = c(0, 0.35), names.arg = Months, xlab = "Months", ylab = "Percentage of Diverted flights", main = "Figure 1.1b: Percentage of Diverted flights by Month", col = q1a1_colours[3], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20month%20to%20fly%20to%20minimize%20delays-2.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Delayed flights by month. 
barplot(height = as.vector(q1a1[,4]), ylim = c(0, 55), names.arg = Months, xlab = "Months", ylab = "Percentage of Delayed flights", main = "Figure 1.1c: Percentage of Delayed flights by Month", col = q1a1_colours[9], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20month%20to%20fly%20to%20minimize%20delays-3.png)<!-- -->

``` r
q1a2_colours <- brewer.pal(n = 9, name = "Oranges")

# Plotting a bar plot on the average arrival delay of flights by month. 
barplot(height = as.vector(q1a2[,2]), names.arg = Months, xlab = "Months", ylab = "Average Arrival Delay in mins", main = "Figure 1.1d: Average Arrival Delay of flights by Month", col = q1a2_colours[4], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20month%20to%20fly%20to%20minimize%20delays-4.png)<!-- -->

**Analysis of results & plots above**

The **percentage of cancelled flights** is lowest in **May** (1.13%),
followed by November (1.33%) & September (1.39%). The **percentage of
diverted flights** is lowest in **November** (0.16%), followed by
October (0.18%) & September (0.19%). The **percentage of delayed
flights** is lowest in **September** (40.55%), followed by November
(42.00%) & May (43.12%).

The month with the **lowest average arrival delay** is **November**
(6.00 mins), followed by September (6.03 mins) & May (6.96 mins).

Since the percentage of cancelled & diverted flights are extremely low,
we shall not take them into consideration for choosing best month to
fly. The main determinants for the best month to fly would be the
percentage of delayed flights & the average arrival delay in each month.
From the results above, **September** & **November** are the **best
months** to fly as the percentage of delayed flights & the average
arrival delay is lowest in those months. The values with respect to both
Months are very similar & thus we do not have a clear answer regarding
the best month to fly.

**Overall decision on best month to fly**

Based on the analysis above, **September** & **November** as the best
months to fly to minimise delays.

**1(b) Best day of week to fly to minimize delays**

**Tables used**: ontime table

**Variables used**: Day, ArrDelay, Cancelled & Diverted

**Reasons for Variables Used**: ArrDelay is used instead of DepDelay due
to reasons stated earlier in 1(b). We also query information regarding
percentage of Diverted flights, Cancelled flights, & Delayed flights by
day of week because it is not beneficial for customers to choose a day
which has low amount of delays but high probability of flights being
cancelled, diverted or delayed.

**Additional Information**: We do not restrict arrival delay values to
more than 0 since flights that reach ahead of schedule where ArrDelay
&lt; 0 have a positive influence on the average arrival delay values by
day of week. We also restrict flights to those that are not cancelled &
not diverted as it is difficult to incorporate those delays into the
calculation of average arrival delays by day of week. Instead we analyse
them through cancelled flight ratio & diverted flights ratio that will
be included in our analysis to determine the best day of week to travel.

``` r
q1b1 <- dbSendQuery(conn,
                    "SELECT DayOfWeek, (AVG(Cancelled)*100), (AVG(Diverted)*100), ((CAST(SUM(CASE WHEN ArrDelay > 0 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT))*100)
                    FROM ontime
                    GROUP BY DayOfWeek
                    ORDER BY DayOfWeek")
```

    ## Warning: Closing open result set, pending rows

``` r
q1b1 <- dbFetch(q1b1)
q1b1 <- as.data.frame(q1b1)
names(q1b1) <- c("Day of Week", "% of Cancelled flights", "% of Diverted flights", "% of Delayed flights")
q1b1
```

    ##   Day of Week % of Cancelled flights % of Diverted flights % of Delayed flights
    ## 1           1               1.846401             0.2246673             45.83500
    ## 2           2               1.968434             0.2150426             42.33993
    ## 3           3               2.097044             0.2311182             44.63076
    ## 4           4               2.186139             0.2521055             48.90079
    ## 5           5               2.119451             0.2476029             50.43676
    ## 6           6               1.490614             0.1915576             40.57715
    ## 7           7               1.778258             0.2325464             45.42729

``` r
q1b2 <- dbSendQuery(conn,
                    "SELECT DayOfWeek, AVG(ArrDelay) 
                    FROM ontime
                    WHERE Cancelled = 0 AND Diverted = 0
                    GROUP BY DayOfWeek
                    ORDER BY DayOfWeek")
```

    ## Warning: Closing open result set, pending rows

``` r
q1b2 <- dbFetch(q1b2)
q1b2 <- as.data.frame(q1b2)
names(q1b2) <- c("Day of Week", "Average Arrival Delay (mins)")
q1b2
```

    ##   Day of Week Average Arrival Delay (mins)
    ## 1           1                     9.701637
    ## 2           2                     7.248411
    ## 3           3                     8.987954
    ## 4           4                    12.141524
    ## 5           5                    12.701676
    ## 6           6                     5.513355
    ## 7           7                     9.247222

**Data visualization (barplot) illustrating best month to fly to
minimize delays**

``` r
DayOfWeek <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

q1a1_colours <- brewer.pal(n = 12, name = "Paired")

# Plotting a bar plot on the Percentage of Cancelled flights by day of week. 
barplot(height = as.vector(q1b1[,2]), ylim = c(0, 2.5), names.arg = DayOfWeek, xlab = "Day of Week", ylab = "Percentage of Cancelled flights", main = "Figure 1.2a: Percentage of Cancelled flights by Day of Week", col = q1a1_colours[1], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20day%20of%20week%20to%20fly%20to%20minimize%20delays-1.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Diverted flights by day of week. 
barplot(height = as.vector(q1b1[,3]), ylim = c(0, 0.30), names.arg = DayOfWeek, xlab = "Day of Week", ylab = "Percentage of Diverted flights", main = "Figure 1.2b: Percentage of Diverted flights by Day of Week", col = q1a1_colours[3], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20day%20of%20week%20to%20fly%20to%20minimize%20delays-2.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Delayed flights by day of week 
barplot(height = as.vector(q1b1[,4]), ylim = c(0, 55), names.arg = DayOfWeek, xlab = "Day of Week", ylab = "Percentage of Delayed flights", main = "Figure 1.2c: Percentage of Delayed flights by Day of Week", col = q1a1_colours[9], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20day%20of%20week%20to%20fly%20to%20minimize%20delays-3.png)<!-- -->

``` r
q1a2_colours <- brewer.pal(n = 9, name = "Oranges")

# Plotting a bar plot on the average arrival delay of flights by day of week 
barplot(height = as.vector(q1b2[,2]), ylim = c(0, 14), names.arg = DayOfWeek, xlab = "Day of Week", ylab = "Average Arrival Delay in mins", main = "Figure 1.2d: Average Arrival Delay of flights by Day of Week", col = q1a2_colours[4], cex.names = 0.8)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20day%20of%20week%20to%20fly%20to%20minimize%20delays-4.png)<!-- -->

**Analysis of results & plots above**

The **percentage of cancelled flights** is lowest in **Saturday**
(1.49%), followed by Sunday (1.78%) & Monday (1.84%). The **percentage
of diverted flights** is lowest in **Saturday** (0.19%), followed by
Tuesday (0.22%) & Monday (0.22%). The **percentage of delayed flights**
is lowest in **Saturday** (40.58%), followed by Tuesday (42.34%) &
Wednesday (44.63%).

The day of week with the **lowest average arrival delay** is
**Saturday** (5.51 mins), followed by Tuesday (7.25 mins) & Wednesday
(8.99 mins).

Once again, since the percentage of cancelled & diverted flights are
extremely low, we shall not take them into consideration for choosing
best day of week to fly. The main determinants for the best day of week
to fly would be the percentage of delayed flights & the average arrival
delay by day of week. From the results above, **Saturday** is the **best
day of week** to fly as the percentage of delayed flights & the average
arrival delay is lowest in Saturday compared to other days of the week.
Even when taking percentage of cancelled & diverted flights into
consideration, Saturday remains to have the lowest percentage which sets
it as the best day of the week to fly.

**Overall decision on best day of week to fly**

Based on the analysis above, **Saturday** is the best day of the week to
fly to minimise delays.

**1(c) Best time of day to fly to minimize delays**

**Tables used**: ontime table

**Variables used**: Cancelled, Diverted, ArrDelay, CRSDepTime

**Reason for Variables Used**: The main variable used is CRSDepTime as
it refers to the time that the flight is scheduled to depart from the
airport. It is more sensible to use CRSDepTime since it would be the
data available to passengers prior to them taking their flights. We
segment 24 hours in a day into 12 parts of 2 hour length each to analyse
the Cancelled Flights ratio & Diverted Flights ratio. We also segment
the time to analyse the lowest average arrival delay based on the 12
2-hour time intervals. The reason we choose 2-hour intervals is because,
from the code below, the average time flights take to travel from one
place to another is **102.32 mins** which is approximately 1hr 40 mins.
Thus, it is appropriate to use 2-hour time intervals for the analysis.

**Additional Info**: We do not include Diverted & Cancelled flights in
our calculation of lowest average arrival delay based on the 12 2-hour
time intervals as it is difficult & inappropriate to incorporate the
delays caused by diversion or cancellation of flights. Intead we
incorporate the information regarding Diverted & Cancelled flights by
time of day using percentage of cancelled & diverted flights.
Furthermore, we do not include cancelled flights as they do not have an
associated flight travel time. (Even if they are included, since NA
values are present, the values will not be used in the calculation)

``` r
AirTime <- dbSendQuery(conn,
                       "SELECT AVG(AirTime) as Avg_Flight_Travel_Time
                        FROM ontime
                        WHERE Cancelled = 0")
```

    ## Warning: Closing open result set, pending rows

``` r
dbFetch(AirTime)
```

    ##   Avg_Flight_Travel_Time
    ## 1               102.8349

``` r
q1c1 <- dbSendQuery(conn,
                    "SELECT (AVG(Cancelled)*100), (AVG(Diverted)*100), ((CAST(SUM(CASE WHEN ArrDelay > 0 THEN 1 ELSE 0 END) AS FLOAT)/CAST(COUNT(*) AS FLOAT))*100),
                    CASE WHEN CRSDepTime BETWEEN 0000 AND 0159 THEN '0000 to 0159'
                         WHEN CRSDepTime BETWEEN 0200 AND 0359 THEN '0200 to 0359'
                         WHEN CRSDepTime BETWEEN 0400 AND 0559 THEN '0400 to 0559'
                         WHEN CRSDepTime BETWEEN 0600 AND 0759 THEN '0600 to 0759'
                         WHEN CRSDepTime BETWEEN 0800 AND 0959 THEN '0800 to 0959'
                         WHEN CRSDepTime BETWEEN 1000 AND 1159 THEN '1000 to 1159'
                         WHEN CRSDepTime BETWEEN 1200 AND 1359 THEN '1200 to 1359'
                         WHEN CRSDepTime BETWEEN 1400 AND 1559 THEN '1400 to 1559'
                         WHEN CRSDepTime BETWEEN 1600 AND 1759 THEN '1600 to 1759'
                         WHEN CRSDepTime BETWEEN 1800 AND 1959 THEN '1800 to 1959'
                         WHEN CRSDepTime BETWEEN 2000 AND 2159 THEN '2000 to 2159'
                         WHEN CRSDepTime BETWEEN 2200 AND 2359 THEN '2200 to 2359'
                         END AS TimeFrame
                    FROM ontime
                    GROUP BY CASE WHEN CRSDepTime BETWEEN 0000 AND 0159 THEN '0000 to 0159'
                                  WHEN CRSDepTime BETWEEN 0200 AND 0359 THEN '0200 to 0359'
                                  WHEN CRSDepTime BETWEEN 0400 AND 0559 THEN '0400 to 0559'
                                  WHEN CRSDepTime BETWEEN 0600 AND 0759 THEN '0600 to 0759'
                                  WHEN CRSDepTime BETWEEN 0800 AND 0959 THEN '0800 to 0959'
                                  WHEN CRSDepTime BETWEEN 1000 AND 1159 THEN '1000 to 1159'
                                  WHEN CRSDepTime BETWEEN 1200 AND 1359 THEN '1200 to 1359'
                                  WHEN CRSDepTime BETWEEN 1400 AND 1559 THEN '1400 to 1559'
                                  WHEN CRSDepTime BETWEEN 1600 AND 1759 THEN '1600 to 1759'
                                  WHEN CRSDepTime BETWEEN 1800 AND 1959 THEN '1800 to 1959'
                                  WHEN CRSDepTime BETWEEN 2000 AND 2159 THEN '2000 to 2159'
                                  WHEN CRSDepTime BETWEEN 2200 AND 2359 THEN '2200 to 2359'
                                  END
                    ORDER BY TimeFrame")
```

    ## Warning: Closing open result set, pending rows

``` r
q1c1 <- dbFetch(q1c1)
q1c1 <- as.data.frame(q1c1)
q1c1 <- q1c1[,c(4,1,2,3)]
names(q1c1) <- c("Time Interval", "% of Cancelled flights", "% of Diverted flights", "% of Delayed flights")
q1c1
```

    ##    Time Interval % of Cancelled flights % of Diverted flights
    ## 1   0000 to 0159              0.7041483            0.13864059
    ## 2   0200 to 0359              0.9497428            0.07914523
    ## 3   0400 to 0559              2.0217230            0.26461763
    ## 4   0600 to 0759              1.7312363            0.23820463
    ## 5   0800 to 0959              1.6397993            0.18188247
    ## 6   1000 to 1159              1.6673278            0.21562795
    ## 7   1200 to 1359              1.7613022            0.26065761
    ## 8   1400 to 1559              2.0198784            0.27876774
    ## 9   1600 to 1759              2.4429243            0.25203001
    ## 10  1800 to 1959              2.4111652            0.20808957
    ## 11  2000 to 2159              1.9868287            0.17518904
    ## 12  2200 to 2359              1.4115302            0.20197197
    ##    % of Delayed flights
    ## 1              41.44624
    ## 2              40.52236
    ## 3              31.34778
    ## 4              34.29553
    ## 5              39.58410
    ## 6              42.96106
    ## 7              46.07736
    ## 8              49.42597
    ## 9              51.87167
    ## 10             52.54186
    ## 11             52.97307
    ## 12             48.40507

**Analysis of results above**

Based on the result, we can once again neglect diverted flights ratio as
they are very small for all the 12 time intervals. The Cancelled flights
ratio is not insignificant & we observe that the lowest percentage of
cancelled flights ratio are from the “0000 to 0159” & “0200 to 0359”
time intervals which are less than 1% whereas the highest percentage of
cancelled flights ratio are from “1600 to 1759”, “1800 to 1959” & “0400
to”0559" which are all more than 3%.

``` r
q1c2 <- dbSendQuery(conn,
                    "SELECT AVG(ArrDelay),
                      CASE WHEN CRSDepTime BETWEEN 0000 AND 0159 THEN '0000 to 0159'
                           WHEN CRSDepTime BETWEEN 0200 AND 0359 THEN '0200 to 0359'
                           WHEN CRSDepTime BETWEEN 0400 AND 0559 THEN '0400 to 0559'
                           WHEN CRSDepTime BETWEEN 0600 AND 0759 THEN '0600 to 0759'
                           WHEN CRSDepTime BETWEEN 0800 AND 0959 THEN '0800 to 0959'
                           WHEN CRSDepTime BETWEEN 1000 AND 1159 THEN '1000 to 1159'
                           WHEN CRSDepTime BETWEEN 1200 AND 1359 THEN '1200 to 1359'
                           WHEN CRSDepTime BETWEEN 1400 AND 1559 THEN '1400 to 1559'
                           WHEN CRSDepTime BETWEEN 1600 AND 1759 THEN '1600 to 1759'
                           WHEN CRSDepTime BETWEEN 1800 AND 1959 THEN '1800 to 1959'
                           WHEN CRSDepTime BETWEEN 2000 AND 2159 THEN '2000 to 2159'
                           WHEN CRSDepTime BETWEEN 2200 AND 2359 THEN '2200 to 2359'
                           END AS TimeFrame
                    FROM ontime
                    WHERE Cancelled = 0 AND Diverted = 0
                    GROUP BY CASE WHEN CRSDepTime BETWEEN 0000 AND 0159 THEN '0000 to 0159'
                                  WHEN CRSDepTime BETWEEN 0200 AND 0359 THEN '0200 to 0359'
                                  WHEN CRSDepTime BETWEEN 0400 AND 0559 THEN '0400 to 0559'
                                  WHEN CRSDepTime BETWEEN 0600 AND 0759 THEN '0600 to 0759'
                                  WHEN CRSDepTime BETWEEN 0800 AND 0959 THEN '0800 to 0959'
                                  WHEN CRSDepTime BETWEEN 1000 AND 1159 THEN '1000 to 1159'
                                  WHEN CRSDepTime BETWEEN 1200 AND 1359 THEN '1200 to 1359'
                                  WHEN CRSDepTime BETWEEN 1400 AND 1559 THEN '1400 to 1559'
                                  WHEN CRSDepTime BETWEEN 1600 AND 1759 THEN '1600 to 1759'
                                  WHEN CRSDepTime BETWEEN 1800 AND 1959 THEN '1800 to 1959'
                                  WHEN CRSDepTime BETWEEN 2000 AND 2159 THEN '2000 to 2159'
                                  WHEN CRSDepTime BETWEEN 2200 AND 2359 THEN '2200 to 2359'
                                  END
                   ORDER BY TimeFrame")
```

    ## Warning: Closing open result set, pending rows

``` r
q1c2 <- dbFetch(q1c2)
q1c2 <- as.data.frame(q1c2)
q1c2 <- q1c2[,c(2,1)]
names(q1c2) <- c("Time Interval", "Average Arrival Delay (mins)")
q1c2
```

    ##    Time Interval Average Arrival Delay (mins)
    ## 1   0000 to 0159                     3.508904
    ## 2   0200 to 0359                     4.502999
    ## 3   0400 to 0559                     0.738958
    ## 4   0600 to 0759                     1.297589
    ## 5   0800 to 0959                     3.815354
    ## 6   1000 to 1159                     5.955356
    ## 7   1200 to 1359                     9.026592
    ## 8   1400 to 1559                    12.374599
    ## 9   1600 to 1759                    15.496993
    ## 10  1800 to 1959                    16.668321
    ## 11  2000 to 2159                    15.724848
    ## 12  2200 to 2359                     9.281447

**Data visualization (barplot & heatmap) illustrating best time of day
to fly to minimize delays**

``` r
TimeFrame <- c("0000 to 0159", "0200 to 0359", "0400 to 0559", "0600 to 0759", "0800 to 0959", "1000 to 1159", "1200 to 1359", "1400 to 1559", "1600 to 1759", "1800 to 1959", "2000 to 2059", "2200 to 2359")

q1a1_colours <- brewer.pal(n = 12, name = "Paired")

# Plotting a bar plot on the Percentage of Cancelled flights by time interval in a day. 
barplot(height = as.vector(q1c1[,2]), ylim = c(0, 2.5), names.arg = TimeFrame, xlab = "Time Interval", ylab = "Percentage of Cancelled flights", main = "Figure 1.3a: Percentage of Cancelled flights by Time of Day", col = q1a1_colours[1], cex.names = 0.4)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20time%20of%20day%20to%20fly%20to%20minimize%20delays-1.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Diverted flights by time interval in a day. 
barplot(height = as.vector(q1c1[,3]), ylim = c(0, 0.30), names.arg = TimeFrame, xlab = "Time Interval", ylab = "Percentage of Diverted flights", main = "Figure 1.3b: Percentage of Diverted flights by Time of Day", col = q1a1_colours[3], cex.names = 0.4)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20time%20of%20day%20to%20fly%20to%20minimize%20delays-2.png)<!-- -->

``` r
# Plotting a bar plot on the Percentage of Delayed flights by time interval in a day. 
barplot(height = as.vector(q1c1[,4]), ylim = c(0, 55), names.arg = TimeFrame, xlab = "Time Interval", ylab = "Percentage of Delayed flights", main = "Figure 1.3c: Percentage of Delayed flights by Time of Day", col = q1a1_colours[9], cex.names = 0.4)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20time%20of%20day%20to%20fly%20to%20minimize%20delays-3.png)<!-- -->

``` r
q1a2_colours <- brewer.pal(n = 9, name = "Oranges")

# Plotting a bar plot on the average arrival delay of flights by time interval in a day. 
barplot(height = as.vector(q1c2[,2]), ylim = c(0, 20), names.arg = TimeFrame, xlab = "Time Interval", ylab = "Average Arrival Delay in mins", main = "Figure 1.3d: Average Arrival Delay of flights by Time of Day", col = q1a2_colours[4], cex.names = 0.4)
```

![](rmarkdown_qn1_github_files/figure-gfm/Barplots%20illustrating%20best%20time%20of%20day%20to%20fly%20to%20minimize%20delays-4.png)<!-- -->

**Analysis of results above**

The **percentage of cancelled flights** is lowest during the time
interval of **0000hrs to 0159hrs** (0.70%), followed by 0200hrs to
0359hrs (0.95%) & 2200hrs to 2359hrs (1.41%). The **percentage of
diverted flights** is lowest in **0200hrs to 0359hrs** (0.08%), followed
by 0000hrs to 0159hrs (0.14%) & 2000hrs to 2159hrs (0.18%). The
**percentage of delayed flights** is lowest in **0400hrs to 0559hrs**
(31.35%), followed by 0600hrs to 0759hrs (34.30%) & 0800hrs to 0959hrs
(39.58%).

The time of day with the **lowest average arrival delay** is the time
interval of **0400hrs to 0559hrs** (0.74 mins), followed by 0600hrs to
0759hrs (1.30 mins) & 0000hrs to 0159hrs (3.51 mins).

Once again, since the percentage of cancelled & diverted flights are
extremely low, we shall not take them into consideration for choosing
best time of day to fly. The main determinants for the best time of day
to fly would be the percentage of delayed flights & the average arrival
delay by time of day. From the results above, **0400hrs to 0559hrs** is
the **best time of day** to fly as the percentage of delayed flights &
the average arrival delay is lowest during the time interval of 0400hrs
to 0559hrs compared to other time intervals within a day.

**Overall decision on Best Time of Day to Fly to Minimize Delays**

Based on the analysis above, **0400hrs to 0559hrs** is the best time of
day to fly to minimise delays.

**Disconnect from database**

``` r
dbDisconnect(conn)
```

    ## Warning in connection_release(conn@ptr): There are 1 result in use. The
    ## connection will be released when they are closed
