How does the number of people flying between different locations change
over time?
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

**Performing Query on question 3 of coursework**

In this rmarkdown script, we aim to answer the question regarding how
the number of people flying between different locations change over
time. Before attempting to answer the question, it is important to make
a few assumptions given the information available in the dataset.

One such assumption is that assume each plane houses the same number of
passengers when making a trip from one location to another. This is
because we lack information regarding the number of passengers who
travelled in each plane & the passenger capacity depending on the
different plane models. Thus, we will approximate the number of people
flying to the number of flights that travel from one location to
another.

We shall answer the question at 3 different scope of levels:

1.  Top 10 U.S. airports with most inbound flights over time.
2.  Top 10 U.S. cities with most inbound flights over time.
3.  Top 10 U.S. states with most inbound flights over time.

We use the concept of inbound flights which helps address the “between
different locations” part of the question where in the context of our
analyses, planes originate from all around the U.S. to its destination
which could be an airport/city/state.

**3(a) Top 10 U.S. airports with most inbound flights over time**

``` r
q3airports_analysis <- dbSendQuery(conn,
                                    "SELECT airports.airport, airports.iata, COUNT(*)
                                    FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                    WHERE Cancelled = 0 AND Diverted = 0 
                                    GROUP BY airports.airport
                                    ORDER BY COUNT(*) DESC")
q3airports_analysis <- dbFetch(q3airports_analysis)
q3airports_analysis <- as.data.frame(q3airports_analysis)
names(q3airports_analysis) <- c("Airport Name", "Airport Code", "Number of Flights")
head(q3airports_analysis)
```

    ##                        Airport Name Airport Code Number of Flights
    ## 1 William B Hartsfield-Atlanta Intl          ATL            801712
    ## 2      Chicago O'Hare International          ORD            716189
    ## 3   Dallas-Fort Worth International          DFW            580154
    ## 4         Los Angeles International          LAX            464443
    ## 5                       Denver Intl          DEN            463808
    ## 6  Phoenix Sky Harbor International          PHX            419908

``` r
q3airports_full <- dbSendQuery(conn,
                                    "SELECT ontime.Year, ontime.Month, airports.airport, airports.iata, COUNT(*)
                                    FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                    WHERE Cancelled = 0 AND Diverted = 0 AND airports.airport != 'None'
                                    GROUP BY airports.airport, ontime.Year, ontime.Month
                                    ORDER BY airports.airport, ontime.Year, ontime.Month")
```

    ## Warning: Closing open result set, pending rows

``` r
q3airports_full <- dbFetch(q3airports_full)
q3airports_full <- as.data.frame(q3airports_full)
names(q3airports_full) <- c("Year", "Month", "Airport Name", "Airport Code", "Number of Flights")
head(q3airports_full)
```

    ##   Year Month     Airport Name Airport Code Number of Flights
    ## 1 2006     1 Abilene Regional          ABI               232
    ## 2 2006     2 Abilene Regional          ABI               217
    ## 3 2006     3 Abilene Regional          ABI               230
    ## 4 2006     4 Abilene Regional          ABI               230
    ## 5 2006     5 Abilene Regional          ABI               237
    ## 6 2006     6 Abilene Regional          ABI               233

``` r
q3airports_full$Date <- ISOdate(q3airports_full$Year, q3airports_full$Month, 1)
head(q3airports_full)
```

    ##   Year Month     Airport Name Airport Code Number of Flights
    ## 1 2006     1 Abilene Regional          ABI               232
    ## 2 2006     2 Abilene Regional          ABI               217
    ## 3 2006     3 Abilene Regional          ABI               230
    ## 4 2006     4 Abilene Regional          ABI               230
    ## 5 2006     5 Abilene Regional          ABI               237
    ## 6 2006     6 Abilene Regional          ABI               233
    ##                  Date
    ## 1 2006-01-01 12:00:00
    ## 2 2006-02-01 12:00:00
    ## 3 2006-03-01 12:00:00
    ## 4 2006-04-01 12:00:00
    ## 5 2006-05-01 12:00:00
    ## 6 2006-06-01 12:00:00

``` r
q3airports_full <- q3airports_full[,c(6,4,5)]
head(q3airports_full)
```

    ##                  Date Airport Code Number of Flights
    ## 1 2006-01-01 12:00:00          ABI               232
    ## 2 2006-02-01 12:00:00          ABI               217
    ## 3 2006-03-01 12:00:00          ABI               230
    ## 4 2006-04-01 12:00:00          ABI               230
    ## 5 2006-05-01 12:00:00          ABI               237
    ## 6 2006-06-01 12:00:00          ABI               233

``` r
q3airports_top10 <- q3airports_full[(q3airports_full$`Airport Code` == "ATL") | (q3airports_full$`Airport Code` == "ORD") | (q3airports_full$`Airport Code` == "DFW") | (q3airports_full$`Airport Code` == "LAX") | (q3airports_full$`Airport Code` == "DEN") | (q3airports_full$`Airport Code` == "PHX") | (q3airports_full$`Airport Code` == "IAH") | (q3airports_full$`Airport Code` == "LAS") | (q3airports_full$`Airport Code` == "EWR") | (q3airports_full$`Airport Code` == "DTW") ,]

# Resetting index of columns
row.names(q3airports_top10) <- NULL
head(q3airports_top10)
```

    ##                  Date Airport Code Number of Flights
    ## 1 2006-01-01 12:00:00          ORD             29638
    ## 2 2006-02-01 12:00:00          ORD             27284
    ## 3 2006-03-01 12:00:00          ORD             30512
    ## 4 2006-04-01 12:00:00          ORD             29843
    ## 5 2006-05-01 12:00:00          ORD             30650
    ## 6 2006-06-01 12:00:00          ORD             30451

``` r
colours <- brewer.pal(n = 12, name = "Paired")

ggplot(data = q3airports_top10, aes(x = as.Date(q3airports_top10$Date), y = q3airports_top10$`Number of Flights`)) + geom_line(aes(colour=q3airports_top10$`Airport Code`), size = 1) + scale_x_date(date_labels = "%b-%Y") + labs(title = "Figure 3.1: Top 10 U.S. Airports with most inbound flights", x = "Date", y = "Number of Flights") + scale_colour_discrete(name = "Airport Code") + theme_minimal()
```

    ## Warning: Use of `q3airports_top10$`Airport Code`` is discouraged. Use `Airport
    ## Code` instead.

    ## Warning: Use of `q3airports_top10$Date` is discouraged. Use `Date` instead.

    ## Warning: Use of `q3airports_top10$`Number of Flights`` is discouraged. Use
    ## `Number of Flights` instead.

![](rmarkdown_qn3_github_files/figure-gfm/Plotting%20line%20graphs%20on%20Top%2010%20U.S.%20airports%20with%20most%20inbound%20flights-1.png)<!-- -->

**Analysis of plot above**

The plot above helps identify seasonality & trends in the top 10
airports with most inbound flights. The data above include data from
years 2006 & 2007 (i.e. Month of January for example contains data from
January 2007 & January 2007). There are some clear seasonal effects that
can be observed from the line graphs above. - There is a decrease in
number of flights travelling into an airport from January to February,
followed by an increase from February to March in all of the 10 airports
as shown above during both periods of Jan-2006 to Mar-2006 & Jan-2007 to
Mar-2007. - Thus, there clearly is some seasonal effect in the number of
flights during the months of January to March - There is a decrease in
number of flights travelling into an airport from August to September,
followed by an increase from September to October in all of the 10
states as shown above during both periods of Aug-2006 to Oct-2006 &
Aug-2007 to Sep-2007. - Thus, once again there clearly is some seasonal
effect in the number of flights during the months of August to October.

**3(b) Top 10 U.S. cities with most inbound flights over time**

``` r
q3cities_analysis <- dbSendQuery(conn,
                                     "SELECT airports.city, COUNT(*)
                                     FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                     WHERE Cancelled = 0 AND Diverted = 0 
                                     GROUP BY airports.city
                                     ORDER BY COUNT(*) DESC")
```

    ## Warning: Closing open result set, pending rows

``` r
q3cities_analysis <- dbFetch(q3cities_analysis)
q3cities_analysis <- as.data.frame(q3cities_analysis)
names(q3cities_analysis) <- c("City Name", "Number of Flights")
head(q3cities_analysis)
```

    ##           City Name Number of Flights
    ## 1           Chicago            909722
    ## 2           Atlanta            801712
    ## 3 Dallas-Fort Worth            580154
    ## 4           Houston            526446
    ## 5          New York            468062
    ## 6       Los Angeles            464443

``` r
q3cities_full <- dbSendQuery(conn,
                                  "SELECT ontime.Year, ontime.Month, airports.city, COUNT(*)
                                  FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                  WHERE Cancelled = 0 AND Diverted = 0 AND airports.city != 'None'
                                  GROUP BY airports.city, ontime.Year, ontime.Month
                                  ORDER BY airports.city, ontime.Year, ontime.Month")
```

    ## Warning: Closing open result set, pending rows

``` r
q3cities_full <- dbFetch(q3cities_full)
q3cities_full <- as.data.frame(q3cities_full)
names(q3cities_full) <- c("Year", "Month", "City", "Number of Flights")
head(q3cities_full)
```

    ##   Year Month    City Number of Flights
    ## 1 2006     1 Abilene               232
    ## 2 2006     2 Abilene               217
    ## 3 2006     3 Abilene               230
    ## 4 2006     4 Abilene               230
    ## 5 2006     5 Abilene               237
    ## 6 2006     6 Abilene               233

``` r
q3cities_full$Date <- ISOdate(q3cities_full$Year, q3cities_full$Month, 1)
head(q3cities_full)
```

    ##   Year Month    City Number of Flights                Date
    ## 1 2006     1 Abilene               232 2006-01-01 12:00:00
    ## 2 2006     2 Abilene               217 2006-02-01 12:00:00
    ## 3 2006     3 Abilene               230 2006-03-01 12:00:00
    ## 4 2006     4 Abilene               230 2006-04-01 12:00:00
    ## 5 2006     5 Abilene               237 2006-05-01 12:00:00
    ## 6 2006     6 Abilene               233 2006-06-01 12:00:00

``` r
q3cities_full <- q3cities_full[,c(5,3,4)]
head(q3cities_full)
```

    ##                  Date    City Number of Flights
    ## 1 2006-01-01 12:00:00 Abilene               232
    ## 2 2006-02-01 12:00:00 Abilene               217
    ## 3 2006-03-01 12:00:00 Abilene               230
    ## 4 2006-04-01 12:00:00 Abilene               230
    ## 5 2006-05-01 12:00:00 Abilene               237
    ## 6 2006-06-01 12:00:00 Abilene               233

``` r
q3cities_top10 <- q3cities_full[(q3cities_full$`City` == "Chicago") | (q3cities_full$`City` == "Atlanta") | (q3cities_full$`City` == "Dallas-Fort Worth") | (q3cities_full$`City` == "Houston") | (q3cities_full$`City` == "New York") | (q3cities_full$`City` == "Los Angeles") | (q3cities_full$`City` == "Denver") | (q3cities_full$`City` == "Phoenix") | (q3cities_full$`City` == "Las Vegas") | (q3cities_full$`City` == "Newark") ,]

# Resetting index of columns
row.names(q3cities_top10) <- NULL
head(q3cities_top10)
```

    ##                  Date    City Number of Flights
    ## 1 2006-01-01 12:00:00 Atlanta             32526
    ## 2 2006-02-01 12:00:00 Atlanta             29768
    ## 3 2006-03-01 12:00:00 Atlanta             34639
    ## 4 2006-04-01 12:00:00 Atlanta             32710
    ## 5 2006-05-01 12:00:00 Atlanta             33948
    ## 6 2006-06-01 12:00:00 Atlanta             32917

``` r
colours <- brewer.pal(n = 12, name = "Paired")

ggplot(data = q3cities_top10, aes(x = as.Date(q3cities_top10$Date), y = q3cities_top10$`Number of Flights`)) + geom_line(aes(colour=q3cities_top10$City), size = 1) + scale_x_date(date_labels = "%b-%Y") + labs(title = "Figure 3.2: Top 10 U.S. Cities with most inbound flights", x = "Date", y = "Number of Flights") + scale_colour_discrete(name = "City") + theme_minimal()
```

    ## Warning: Use of `q3cities_top10$City` is discouraged. Use `City` instead.

    ## Warning: Use of `q3cities_top10$Date` is discouraged. Use `Date` instead.

    ## Warning: Use of `q3cities_top10$`Number of Flights`` is discouraged. Use `Number
    ## of Flights` instead.

![](rmarkdown_qn3_github_files/figure-gfm/Plotting%20line%20graphs%20on%20Top%2010%20U.S.%20cities%20with%20most%20inbound%20flights-1.png)<!-- -->

**Analysis of plot above**

The plot above helps identify seasonality & trends in the top 10 cities
with most inbound flights. The data above include data from years 2006 &
2007 (i.e. Month of January for example contains data from January 2007
& January 2007). There are some clear seasonal effects that can be
observed from the line graphs above. - There is a decrease in number of
flights travelling into a city from January to February, followed by an
increase from February to March in all of the 10 cities as shown above
during both periods of Jan-2006 to Mar-2006 & Jan-2007 to Mar-2007. -
Thus, there clearly is some seasonal effect in the number of flights
during the months of January to March - There is a decrease in number of
flights travelling into a city from August to September, followed by an
increase from September to October in all of the 10 cities as shown
above during both periods of Aug-2006 to Oct-2006 & Aug-2007 to
Sep-2007. - Thus, once again there clearly is some seasonal effect in
the number of flights during the months of August to October.

**3(c) Top 10 U.S. states with most inbound flights over time**

``` r
q3states_analysis <- dbSendQuery(conn,
                                     "SELECT airports.state, COUNT(*)
                                     FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                     WHERE Cancelled = 0 AND Diverted = 0 
                                     GROUP BY airports.state
                                     ORDER BY COUNT(*) DESC")
```

    ## Warning: Closing open result set, pending rows

``` r
q3states_analysis <- dbFetch(q3states_analysis)
q3states_analysis <- as.data.frame(q3states_analysis)
names(q3states_analysis) <- c("State Name", "Number of Flights")
head(q3states_analysis)
```

    ##   State Name Number of Flights
    ## 1         CA           1725043
    ## 2         TX           1566201
    ## 3         IL            953154
    ## 4         FL            923167
    ## 5         GA            844287
    ## 6         NY            660806

``` r
q3states_full <- dbSendQuery(conn,
                                  "SELECT ontime.Year, ontime.Month, airports.state, COUNT(*)
                                  FROM ontime JOIN airports ON ontime.Dest = airports.iata
                                  WHERE Cancelled = 0 AND Diverted = 0 AND airports.state != 'None'
                                  GROUP BY airports.state, ontime.Year, ontime.Month
                                  ORDER BY airports.state, ontime.Year, ontime.Month")
```

    ## Warning: Closing open result set, pending rows

``` r
q3states_full <- dbFetch(q3states_full)
q3states_full <- as.data.frame(q3states_full)
names(q3states_full) <- c("Year", "Month", "State", "Number of Flights")
head(q3states_full)
```

    ##   Year Month State Number of Flights
    ## 1 2006     1    AK              2868
    ## 2 2006     2    AK              2602
    ## 3 2006     3    AK              3118
    ## 4 2006     4    AK              3033
    ## 5 2006     5    AK              3503
    ## 6 2006     6    AK              4300

``` r
q3states_full$Date <- ISOdate(q3states_full$Year, q3states_full$Month, 1)
head(q3states_full) 
```

    ##   Year Month State Number of Flights                Date
    ## 1 2006     1    AK              2868 2006-01-01 12:00:00
    ## 2 2006     2    AK              2602 2006-02-01 12:00:00
    ## 3 2006     3    AK              3118 2006-03-01 12:00:00
    ## 4 2006     4    AK              3033 2006-04-01 12:00:00
    ## 5 2006     5    AK              3503 2006-05-01 12:00:00
    ## 6 2006     6    AK              4300 2006-06-01 12:00:00

``` r
q3states_full <- q3states_full[,c(5,3,4)]
head(q3states_full) 
```

    ##                  Date State Number of Flights
    ## 1 2006-01-01 12:00:00    AK              2868
    ## 2 2006-02-01 12:00:00    AK              2602
    ## 3 2006-03-01 12:00:00    AK              3118
    ## 4 2006-04-01 12:00:00    AK              3033
    ## 5 2006-05-01 12:00:00    AK              3503
    ## 6 2006-06-01 12:00:00    AK              4300

``` r
q3states_top10 <- q3states_full[(q3states_full$`State` == "CA") | (q3states_full$`State` == "TX") | (q3states_full$`State` == "IL") | (q3states_full$`State` == "FL") | (q3states_full$`State` == "GA") | (q3states_full$`State` == "NY") | (q3states_full$`State` == "CO") | (q3states_full$`State` == "AZ") | (q3states_full$`State` == "VA") | (q3states_full$`State` == "NV") ,]

# Resetting index of columns
row.names(q3states_top10) <- NULL
head(q3states_top10)
```

    ##                  Date State Number of Flights
    ## 1 2006-01-01 12:00:00    AZ             20404
    ## 2 2006-02-01 12:00:00    AZ             18468
    ## 3 2006-03-01 12:00:00    AZ             20940
    ## 4 2006-04-01 12:00:00    AZ             19961
    ## 5 2006-05-01 12:00:00    AZ             20508
    ## 6 2006-06-01 12:00:00    AZ             20001

``` r
colours <- brewer.pal(n = 12, name = "Paired")

ggplot(data = q3states_top10, aes(x = as.Date(q3states_top10$Date), y = q3states_top10$`Number of Flights`)) + geom_line(aes(colour=q3states_top10$State), size = 1) + scale_x_date(date_labels = "%b-%Y") + labs(title = "Figure 3.3: Top 10 U.S. States with most inbound flights", x = "Date", y = "Number of Flights") + scale_colour_discrete(name = "State") + theme_minimal()
```

    ## Warning: Use of `q3states_top10$State` is discouraged. Use `State` instead.

    ## Warning: Use of `q3states_top10$Date` is discouraged. Use `Date` instead.

    ## Warning: Use of `q3states_top10$`Number of Flights`` is discouraged. Use `Number
    ## of Flights` instead.

![](rmarkdown_qn3_github_files/figure-gfm/Plotting%20line%20graphs%20on%20Top%2010%20U.S.%20states%20with%20most%20inbound%20flights-1.png)<!-- -->

**Analysis of plot above**

The plot above helps identify seasonality & trends in the top 10 states
with most inbound flights. The data above include data from years 2006 &
2007 (i.e. Month of January for example contains data from January 2007
& January 2007). There are some clear seasonal effects that can be
observed from the line graphs above. - There is a decrease in number of
flights travelling into a state from January to February, followed by an
increase from February to March in all of the 10 states as shown above
during both periods of Jan-2006 to Mar-2006 & Jan-2007 to Mar-2007. -
Thus, there clearly is some seasonal effect in the number of flights
during the months of January to March - There is a decrease in number of
flights travelling into a state from August to September, followed by an
increase from September to October in all of the 10 states as shown
above during both periods of Aug-2006 to Oct-2006 & Aug-2007 to
Sep-2007. - Thus, once again there clearly is some seasonal effect in
the number of flights during the months of August to October.

**Overall observations from plots above**

The seasonal effects are consistent throughout the top 10 airports,
cities & states. Thus, we can conclude that there are obvious **seasonal
effects** during the periods of **January to March** & **August to
September**. The number of passengers flying between different locations
fluctuate according to the seasonal effects as explained above.

**Disconnect from database**

``` r
dbDisconnect(conn)
```

    ## Warning in connection_release(conn@ptr): There are 1 result in use. The
    ## connection will be released when they are closed
