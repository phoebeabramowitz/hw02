HW-02: Shot Charts
================
Phoebe Abramowitz
3/4/2018

Question 5.1
------------

``` r
library("dplyr")
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
library('ggplot2')

shots_data <- read.csv('../data/shots-data.csv')


#group by player name and count number of shots 
player_shot_count<- count(shots_data, name, sort=TRUE)
psc <- rename(player_shot_count,total=n)
psc
```

    ## # A tibble: 5 x 2
    ##   name           total
    ##   <fct>          <int>
    ## 1 Stephen Curry   1250
    ## 2 Klay Thompson   1220
    ## 3 Kevin Durant     915
    ## 4 Draymond Green   578
    ## 5 Andre Iguodala   371

Question 5.2
------------

``` r
#effective shooting percent by player
efctv_shot_count<- rename(count(filter(shots_data, shot_made_flag=="made shot"), 
                                name), made=n)
comb <- left_join(psc,efctv_shot_count)
```

    ## Joining, by = "name"

``` r
shooting_perc <- arrange(mutate(comb, perc_made=(made/total)),desc(perc_made))



shots_data_2pt <- filter(shots_data, shot_type=="2PT Field Goal")
psc_2pt <- rename(count(shots_data_2pt,name, sort=TRUE),total=n)

efctv_shot_count_2pt<- rename(count(filter(shots_data_2pt, shot_made_flag=="made shot"),
                                    name), made=n)
comb_2pt <- left_join(psc_2pt,efctv_shot_count_2pt)
```

    ## Joining, by = "name"

``` r
shooting_perc_2pt <- arrange(mutate(comb_2pt, perc_made=(made/total)),desc(perc_made))


shots_data_3pt <- filter(shots_data, shot_type=="3PT Field Goal")
psc_3pt <- rename(count(shots_data_3pt,name, sort=TRUE),total=n)

efctv_shot_count_3pt<- rename(count(filter(shots_data_3pt, shot_made_flag=="made shot"),
                                    name), made=n)
comb_3pt <- left_join(psc_3pt,efctv_shot_count_3pt)
```

    ## Joining, by = "name"

``` r
shooting_perc_3pt <- arrange(mutate(comb_3pt, perc_made=(made/total)),desc(perc_made))


shooting_perc_2pt
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Andre Iguodala   210   134     0.638
    ## 2 Kevin Durant     643   390     0.607
    ## 3 Stephen Curry    563   304     0.540
    ## 4 Klay Thompson    640   329     0.514
    ## 5 Draymond Green   346   171     0.494

``` r
shooting_perc_3pt
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Klay Thompson    580   246     0.424
    ## 2 Stephen Curry    687   280     0.408
    ## 3 Kevin Durant     272   105     0.386
    ## 4 Andre Iguodala   161    58     0.360
    ## 5 Draymond Green   232    74     0.319

``` r
shooting_perc
```

    ## # A tibble: 5 x 4
    ##   name           total  made perc_made
    ##   <fct>          <int> <int>     <dbl>
    ## 1 Kevin Durant     915   495     0.541
    ## 2 Andre Iguodala   371   192     0.518
    ## 3 Klay Thompson   1220   575     0.471
    ## 4 Stephen Curry   1250   584     0.467
    ## 5 Draymond Green   578   245     0.424

Question 6
----------

``` r
shots_data_dist <-mutate(shots_data,shot_distance=round(sqrt(x^2+y^2)))

                        
dist_shot_count <- count(shots_data_dist, shot_distance)

dsc <- arrange(rename(dist_shot_count,total=n),shot_distance)
dsc
```

    ## # A tibble: 351 x 2
    ##    shot_distance total
    ##            <dbl> <int>
    ##  1          1.00   246
    ##  2          2.00    12
    ##  3          3.00    16
    ##  4          4.00    14
    ##  5          5.00     8
    ##  6          6.00    13
    ##  7          7.00    39
    ##  8          8.00    31
    ##  9          9.00    28
    ## 10         10.0     18
    ## # ... with 341 more rows

``` r
efctv_shot_dist<-count(filter(shots_data_dist, shot_made_flag=="made shot"),
                                shot_distance)
efctv_shot_dist
```

    ## # A tibble: 292 x 2
    ##    shot_distance     n
    ##            <dbl> <int>
    ##  1          1.00   233
    ##  2          2.00     7
    ##  3          3.00    13
    ##  4          4.00     9
    ##  5          5.00     6
    ##  6          6.00     7
    ##  7          7.00    28
    ##  8          8.00    20
    ##  9          9.00    20
    ## 10         10.0     13
    ## # ... with 282 more rows

``` r
comb_dist <- left_join(dsc,efctv_shot_dist)
```

    ## Joining, by = "shot_distance"

``` r
comb_dist
```

    ## # A tibble: 351 x 3
    ##    shot_distance total     n
    ##            <dbl> <int> <int>
    ##  1          1.00   246   233
    ##  2          2.00    12     7
    ##  3          3.00    16    13
    ##  4          4.00    14     9
    ##  5          5.00     8     6
    ##  6          6.00    13     7
    ##  7          7.00    39    28
    ##  8          8.00    31    20
    ##  9          9.00    28    20
    ## 10         10.0     18    13
    ## # ... with 341 more rows

``` r
shooting_perc_dist <- arrange(mutate(comb_dist, 
                                     made_shot_prop=(n/total)))
spd <- select(shooting_perc_dist,shot_distance,made_shot_prop)
spd
```

    ## # A tibble: 351 x 2
    ##    shot_distance made_shot_prop
    ##            <dbl>          <dbl>
    ##  1          1.00          0.947
    ##  2          2.00          0.583
    ##  3          3.00          0.812
    ##  4          4.00          0.643
    ##  5          5.00          0.750
    ##  6          6.00          0.538
    ##  7          7.00          0.718
    ##  8          8.00          0.645
    ##  9          9.00          0.714
    ## 10         10.0           0.722
    ## # ... with 341 more rows

Question 6.2
------------

``` r
ggplot(data=slice(spd,1:38))+
  geom_point(aes(x=shot_distance,y=made_shot_prop),color="blue")+
  ggtitle("Proportion of shots made by distance, up to 38in from net")
```

![](../imagesunnamed-chunk-4-1.png)

``` r
ggplot(data=spd)+
  geom_point(aes(x=shot_distance,y=made_shot_prop),color="green")+
  ggtitle("Proportion of shots made by distance")
```

    ## Warning: Removed 59 rows containing missing values (geom_point).

![](../imagesunnamed-chunk-4-2.png) \#\#\#Observations:/ Looking at the overall scatter plot, there's not an obvious overall downward trend, so we can't confirm that the shorter/ the distance, the more effective the shots. There is however, a clearly visible downward trend within 38 inches of the net./ It's worth noting that there's much higher rates of shooting very close to the net and around 230 inches(closest to the net/ while in the 3-point zone), so there's many more baskets made from there despite scattered proportions. Very few shots are/ taken more than 300 inches from the net. The 20 inches closest to the net all have a &gt;50% of making the shot. I can't infer/ the distance at which chances of making a shot are basically null, but almost no shots are taken beyond 300 inches/ away. If players are choosing not to take shots further than that, it's probably because they don't beleive they/ can be effective.
\#\# Question 7

``` r
d <- data.frame(x1=c(0,24),y1=c(0,0),x2=c(12,36),y2=c(60,60))

by_minute_player <-count(shots_data,minute,name)
gsw_shot_chart <- ggplot(data=by_minute_player)+
  geom_rect(data=d,mapping=aes(xmin=x1,ymin=y1,xmax=x2,ymax=y2),fill="grey97",color="grey90")+
  geom_path(aes(x=minute,y=n),color="lightblue2")+
  geom_point(aes(x=minute,y=n),color="steelblue2",shape=20)+
  ylim(0,60)+
  facet_wrap(~name)+
  scale_x_continuous()+
  ggtitle("Total Number of Shots(By Minute of Occurence)")+
  theme_minimal()+
  labs(x="minute",y="total number of shots")

gsw_shot_chart
```

![](../imagesunnamed-chunk-5-1.png)
