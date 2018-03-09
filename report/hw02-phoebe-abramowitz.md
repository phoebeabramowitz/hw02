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
filter(shots_data_dist,shot_distance<50)
```

    ##                  team_name game_date season period minutes_remaining
    ## 1    Golden State Warriors  12/15/16   2016      3                 3
    ## 2    Golden State Warriors  10/28/16   2016      3                 9
    ## 3    Golden State Warriors   11/1/16   2016      2                 5
    ## 4    Golden State Warriors   12/1/16   2016      3                 5
    ## 5    Golden State Warriors    4/4/17   2016      3                 2
    ## 6    Golden State Warriors  11/19/16   2016      4                 5
    ## 7    Golden State Warriors  11/21/16   2016      3                 9
    ## 8    Golden State Warriors   3/29/17   2016      2                 1
    ## 9    Golden State Warriors  11/25/16   2016      3                10
    ## 10   Golden State Warriors  12/28/16   2016      3                 3
    ## 11   Golden State Warriors   3/28/17   2016      4                 9
    ## 12   Golden State Warriors  12/11/16   2016      4                 3
    ## 13   Golden State Warriors  12/10/16   2016      3                 4
    ## 14   Golden State Warriors   2/27/17   2016      1                 9
    ## 15   Golden State Warriors    3/6/17   2016      1                 1
    ## 16   Golden State Warriors   3/21/17   2016      1                 8
    ## 17   Golden State Warriors   12/3/16   2016      3                 9
    ## 18   Golden State Warriors   2/27/17   2016      4                 5
    ## 19   Golden State Warriors   12/7/16   2016      1                 3
    ## 20   Golden State Warriors   3/16/17   2016      3                 5
    ## 21   Golden State Warriors    1/6/17   2016      1                 8
    ## 22   Golden State Warriors  10/28/16   2016      3                 3
    ## 23   Golden State Warriors  11/23/16   2016      3                 2
    ## 24   Golden State Warriors    2/2/17   2016      4                 6
    ## 25   Golden State Warriors  12/17/16   2016      1                 3
    ## 26   Golden State Warriors  12/20/16   2016      2                 1
    ## 27   Golden State Warriors  11/25/16   2016      4                 2
    ## 28   Golden State Warriors   3/26/17   2016      3                 8
    ## 29   Golden State Warriors   2/10/17   2016      1                 2
    ## 30   Golden State Warriors   1/28/17   2016      1                11
    ## 31   Golden State Warriors  12/25/16   2016      4                 4
    ## 32   Golden State Warriors  11/19/16   2016      3                 8
    ## 33   Golden State Warriors  11/23/16   2016      2                 2
    ## 34   Golden State Warriors   2/15/17   2016      1                 9
    ## 35   Golden State Warriors   12/7/16   2016      1                 7
    ## 36   Golden State Warriors   12/7/16   2016      2                 4
    ## 37   Golden State Warriors  11/26/16   2016      1                 2
    ## 38   Golden State Warriors   11/9/16   2016      2                 8
    ## 39   Golden State Warriors    1/4/17   2016      1                 5
    ## 40   Golden State Warriors  11/18/16   2016      2                 1
    ## 41   Golden State Warriors  11/18/16   2016      4                 0
    ## 42   Golden State Warriors  11/13/16   2016      3                 1
    ## 43   Golden State Warriors   3/29/17   2016      3                 5
    ## 44   Golden State Warriors    1/2/17   2016      1                 2
    ## 45   Golden State Warriors   2/25/17   2016      4                 2
    ## 46   Golden State Warriors  11/16/16   2016      1                 7
    ## 47   Golden State Warriors    4/4/17   2016      1                10
    ## 48   Golden State Warriors   2/23/17   2016      3                 1
    ## 49   Golden State Warriors   3/29/17   2016      1                 1
    ## 50   Golden State Warriors  12/28/16   2016      4                 0
    ## 51   Golden State Warriors  12/20/16   2016      1                 1
    ## 52   Golden State Warriors  11/26/16   2016      3                 2
    ## 53   Golden State Warriors  11/26/16   2016      4                 5
    ## 54   Golden State Warriors  12/30/16   2016      1                 7
    ## 55   Golden State Warriors  12/20/16   2016      1                 1
    ## 56   Golden State Warriors   3/29/17   2016      2                 0
    ## 57   Golden State Warriors  11/19/16   2016      4                 2
    ## 58   Golden State Warriors  11/18/16   2016      3                 7
    ## 59   Golden State Warriors   2/27/17   2016      3                 0
    ## 60   Golden State Warriors   3/31/17   2016      3                 3
    ## 61   Golden State Warriors  12/11/16   2016      2                 0
    ## 62   Golden State Warriors   3/16/17   2016      3                 0
    ## 63   Golden State Warriors   12/8/16   2016      3                 7
    ## 64   Golden State Warriors   12/5/16   2016      2                 1
    ## 65   Golden State Warriors   1/16/17   2016      1                 4
    ## 66   Golden State Warriors   3/31/17   2016      3                 9
    ## 67   Golden State Warriors   11/3/16   2016      1                 8
    ## 68   Golden State Warriors   2/28/17   2016      2                 2
    ## 69   Golden State Warriors   2/11/17   2016      4                 5
    ## 70   Golden State Warriors    3/2/17   2016      1                 6
    ## 71   Golden State Warriors  11/28/16   2016      4                 3
    ## 72   Golden State Warriors  10/25/16   2016      4                 6
    ## 73   Golden State Warriors    1/6/17   2016      1                10
    ## 74   Golden State Warriors    1/2/17   2016      2                 1
    ## 75   Golden State Warriors    3/5/17   2016      1                 1
    ## 76   Golden State Warriors   11/4/16   2016      1                 9
    ## 77   Golden State Warriors    1/2/17   2016      1                 0
    ## 78   Golden State Warriors    3/5/17   2016      3                 4
    ## 79   Golden State Warriors   11/4/16   2016      1                 8
    ## 80   Golden State Warriors   3/28/17   2016      2                 3
    ## 81   Golden State Warriors  11/16/16   2016      3                 8
    ## 82   Golden State Warriors  10/28/16   2016      4                 1
    ## 83   Golden State Warriors  12/28/16   2016      2                 2
    ## 84   Golden State Warriors   2/28/17   2016      3                 4
    ## 85   Golden State Warriors  12/13/16   2016      1                 1
    ## 86   Golden State Warriors  12/13/16   2016      3                10
    ## 87   Golden State Warriors   11/9/16   2016      3                 1
    ## 88   Golden State Warriors  11/28/16   2016      4                 6
    ## 89   Golden State Warriors  12/23/16   2016      4                 3
    ## 90   Golden State Warriors   1/20/17   2016      3                 4
    ## 91   Golden State Warriors  12/23/16   2016      2                 4
    ## 92   Golden State Warriors   1/18/17   2016      1                 7
    ## 93   Golden State Warriors    3/2/17   2016      1                 0
    ## 94   Golden State Warriors   3/26/17   2016      2                 1
    ## 95   Golden State Warriors   1/18/17   2016      1                 2
    ## 96   Golden State Warriors   2/10/17   2016      3                 4
    ## 97   Golden State Warriors    3/2/17   2016      4                 7
    ## 98   Golden State Warriors    2/1/17   2016      3                 6
    ## 99   Golden State Warriors   2/11/17   2016      3                 6
    ## 100  Golden State Warriors   2/13/17   2016      3                 4
    ## 101  Golden State Warriors  10/30/16   2016      1                 4
    ## 102  Golden State Warriors    2/8/17   2016      2                 1
    ## 103  Golden State Warriors  12/30/16   2016      1                 2
    ## 104  Golden State Warriors   2/28/17   2016      3                 1
    ## 105  Golden State Warriors   2/11/17   2016      3                 2
    ## 106  Golden State Warriors   1/22/17   2016      1                 0
    ## 107  Golden State Warriors  10/25/16   2016      3                 1
    ## 108  Golden State Warriors    2/4/17   2016      1                 2
    ## 109  Golden State Warriors    1/8/17   2016      3                 1
    ## 110  Golden State Warriors    3/2/17   2016      4                 6
    ## 111  Golden State Warriors   3/20/17   2016      1                 1
    ## 112  Golden State Warriors  10/25/16   2016      2                 3
    ## 113  Golden State Warriors  12/11/16   2016      3                10
    ## 114  Golden State Warriors  12/11/16   2016      1                 1
    ## 115  Golden State Warriors   3/18/17   2016      2                 0
    ## 116  Golden State Warriors   2/27/17   2016      3                 8
    ## 117  Golden State Warriors  12/15/16   2016      2                 7
    ## 118  Golden State Warriors  11/13/16   2016      2                 2
    ## 119  Golden State Warriors   2/27/17   2016      4                 3
    ## 120  Golden State Warriors   2/27/17   2016      2                 4
    ## 121  Golden State Warriors    2/4/17   2016      2                 1
    ## 122  Golden State Warriors  11/26/16   2016      3                 1
    ## 123  Golden State Warriors    3/2/17   2016      4                 1
    ## 124  Golden State Warriors    3/2/17   2016      2                 2
    ## 125  Golden State Warriors   1/28/17   2016      3                 9
    ## 126  Golden State Warriors   3/29/17   2016      4                 2
    ## 127  Golden State Warriors   1/25/17   2016      4                 4
    ## 128  Golden State Warriors   1/25/17   2016      1                 5
    ## 129  Golden State Warriors    1/6/17   2016      1                 3
    ## 130  Golden State Warriors   12/7/16   2016      1                 2
    ## 131  Golden State Warriors   1/28/17   2016      2                 4
    ## 132  Golden State Warriors    3/5/17   2016      3                 1
    ## 133  Golden State Warriors  10/25/16   2016      3                 3
    ## 134  Golden State Warriors   2/25/17   2016      4                 4
    ## 135  Golden State Warriors   12/3/16   2016      2                 0
    ## 136  Golden State Warriors   3/18/17   2016      3                 5
    ## 137  Golden State Warriors  12/28/16   2016      4                 5
    ## 138  Golden State Warriors   11/3/16   2016      3                 2
    ## 139  Golden State Warriors   11/3/16   2016      1                 9
    ## 140  Golden State Warriors    1/6/17   2016      1                 5
    ## 141  Golden State Warriors    3/2/17   2016      3                11
    ## 142  Golden State Warriors   1/20/17   2016      1                10
    ## 143  Golden State Warriors   1/25/17   2016      3                 1
    ## 144  Golden State Warriors   1/22/17   2016      1                 4
    ## 145  Golden State Warriors    3/6/17   2016      4                 1
    ## 146  Golden State Warriors   3/16/17   2016      3                 3
    ## 147  Golden State Warriors   1/18/17   2016      4                 2
    ## 148  Golden State Warriors  11/13/16   2016      1                 1
    ## 149  Golden State Warriors    1/2/17   2016      3                 3
    ## 150  Golden State Warriors    1/4/17   2016      3                 8
    ## 151  Golden State Warriors  11/16/16   2016      1                 5
    ## 152  Golden State Warriors   1/28/17   2016      1                 0
    ## 153  Golden State Warriors   3/14/17   2016      3                 6
    ## 154  Golden State Warriors  11/23/16   2016      3                 3
    ## 155  Golden State Warriors   1/18/17   2016      3                 4
    ## 156  Golden State Warriors   3/29/17   2016      3                 0
    ## 157  Golden State Warriors   3/24/17   2016      3                 9
    ## 158  Golden State Warriors   1/16/17   2016      1                11
    ## 159  Golden State Warriors  12/11/16   2016      2                 0
    ## 160  Golden State Warriors   3/20/17   2016      2                 5
    ## 161  Golden State Warriors   12/5/16   2016      1                 1
    ## 162  Golden State Warriors  12/15/16   2016      2                11
    ## 163  Golden State Warriors  11/28/16   2016      2                 0
    ## 164  Golden State Warriors   1/16/17   2016      3                 0
    ## 165  Golden State Warriors   1/16/17   2016      3                 8
    ## 166  Golden State Warriors   3/14/17   2016      3                 6
    ## 167  Golden State Warriors    3/5/17   2016      3                10
    ## 168  Golden State Warriors   2/25/17   2016      1                 5
    ## 169  Golden State Warriors   2/25/17   2016      2                 5
    ## 170  Golden State Warriors   11/3/16   2016      1                 4
    ## 171  Golden State Warriors   1/18/17   2016      3                 2
    ## 172  Golden State Warriors  12/13/16   2016      3                 3
    ## 173  Golden State Warriors   3/18/17   2016      1                 9
    ## 174  Golden State Warriors   2/28/17   2016      4                 0
    ## 175  Golden State Warriors  12/13/16   2016      4                 1
    ## 176  Golden State Warriors   11/3/16   2016      2                 5
    ## 177  Golden State Warriors  11/19/16   2016      3                 3
    ## 178  Golden State Warriors   3/14/17   2016      1                 7
    ## 179  Golden State Warriors  12/28/16   2016      4                 2
    ## 180  Golden State Warriors   1/18/17   2016      2                 2
    ## 181  Golden State Warriors  10/28/16   2016      4                 4
    ## 182  Golden State Warriors   3/28/17   2016      2                 5
    ## 183  Golden State Warriors   3/28/17   2016      3                 8
    ## 184  Golden State Warriors   2/28/17   2016      3                 7
    ## 185  Golden State Warriors   2/28/17   2016      4                 6
    ## 186  Golden State Warriors   11/7/16   2016      2                 2
    ## 187  Golden State Warriors  12/13/16   2016      4                 5
    ## 188  Golden State Warriors   11/4/16   2016      3                11
    ## 189  Golden State Warriors   1/25/17   2016      2                 0
    ## 190  Golden State Warriors  10/28/16   2016      4                 5
    ## 191  Golden State Warriors    3/5/17   2016      3                 3
    ## 192  Golden State Warriors  11/23/16   2016      3                 9
    ## 193  Golden State Warriors   1/18/17   2016      2                 1
    ## 194  Golden State Warriors   2/10/17   2016      4                 6
    ## 195  Golden State Warriors  12/17/16   2016      2                 2
    ## 196  Golden State Warriors  12/10/16   2016      1                 5
    ## 197  Golden State Warriors   1/25/17   2016      4                 2
    ## 198  Golden State Warriors    3/5/17   2016      1                 3
    ## 199  Golden State Warriors  12/30/16   2016      3                 8
    ## 200  Golden State Warriors   3/29/17   2016      2                 2
    ## 201  Golden State Warriors  11/10/16   2016      3                10
    ## 202  Golden State Warriors  11/19/16   2016      2                 4
    ## 203  Golden State Warriors   11/1/16   2016      3                 2
    ## 204  Golden State Warriors    2/1/17   2016      1                 1
    ## 205  Golden State Warriors   11/4/16   2016      4                 5
    ## 206  Golden State Warriors    4/4/17   2016      2                 3
    ## 207  Golden State Warriors    1/8/17   2016      3                 9
    ## 208  Golden State Warriors   3/29/17   2016      3                 2
    ## 209  Golden State Warriors   2/25/17   2016      1                 0
    ## 210  Golden State Warriors   2/28/17   2016      4                 5
    ## 211  Golden State Warriors    3/5/17   2016      4                 6
    ## 212  Golden State Warriors   2/15/17   2016      3                 9
    ## 213  Golden State Warriors  10/25/16   2016      3                 4
    ## 214  Golden State Warriors   2/23/17   2016      1                 4
    ## 215  Golden State Warriors  12/13/16   2016      3                 1
    ## 216  Golden State Warriors  11/10/16   2016      1                 6
    ## 217  Golden State Warriors    3/5/17   2016      2                 4
    ## 218  Golden State Warriors  11/18/16   2016      1                 7
    ## 219  Golden State Warriors   2/15/17   2016      1                 2
    ## 220  Golden State Warriors  12/28/16   2016      4                 5
    ## 221  Golden State Warriors   3/16/17   2016      1                 4
    ## 222  Golden State Warriors   2/27/17   2016      3                10
    ## 223  Golden State Warriors   3/31/17   2016      1                 3
    ## 224  Golden State Warriors   2/25/17   2016      3                 2
    ## 225  Golden State Warriors    3/5/17   2016      1                 6
    ## 226  Golden State Warriors  12/20/16   2016      3                 3
    ## 227  Golden State Warriors   12/1/16   2016      3                 6
    ## 228  Golden State Warriors  10/28/16   2016      1                 9
    ## 229  Golden State Warriors  12/13/16   2016      2                 0
    ## 230  Golden State Warriors   1/18/17   2016      2                 3
    ## 231  Golden State Warriors    2/2/17   2016      2                 1
    ## 232  Golden State Warriors  11/25/16   2016      4                 1
    ## 233  Golden State Warriors   11/4/16   2016      2                 3
    ## 234  Golden State Warriors   11/1/16   2016      3                 1
    ## 235  Golden State Warriors   2/27/17   2016      3                 6
    ## 236  Golden State Warriors  11/10/16   2016      2                 3
    ## 237  Golden State Warriors   11/1/16   2016      3                 8
    ## 238  Golden State Warriors   11/1/16   2016      2                 3
    ## 239  Golden State Warriors   2/27/17   2016      3                 5
    ## 240  Golden State Warriors  10/28/16   2016      3                 8
    ## 241  Golden State Warriors   3/29/17   2016      2                 5
    ## 242  Golden State Warriors   11/1/16   2016      3                 0
    ## 243  Golden State Warriors   1/18/17   2016      3                 0
    ## 244  Golden State Warriors    2/2/17   2016      1                 7
    ## 245  Golden State Warriors  11/25/16   2016      2                 4
    ## 246  Golden State Warriors   3/20/17   2016      1                 8
    ## 247  Golden State Warriors    4/4/17   2016      3                 8
    ## 248  Golden State Warriors  12/20/16   2016      3                 8
    ## 249  Golden State Warriors   12/5/16   2016      2                 4
    ## 250  Golden State Warriors    4/4/17   2016      1                 3
    ## 251  Golden State Warriors   3/16/17   2016      2                10
    ## 252  Golden State Warriors    1/8/17   2016      2                 3
    ## 253  Golden State Warriors   1/18/17   2016      1                 5
    ## 254  Golden State Warriors   3/28/17   2016      1                 3
    ## 255  Golden State Warriors    2/1/17   2016      2                 3
    ## 256  Golden State Warriors    1/6/17   2016      3                 4
    ## 257  Golden State Warriors    1/4/17   2016      2                 0
    ## 258  Golden State Warriors  11/13/16   2016      4                 5
    ## 259  Golden State Warriors  12/10/16   2016      3                 4
    ## 260  Golden State Warriors   2/10/17   2016      3                 7
    ## 261  Golden State Warriors   1/20/17   2016      1                 6
    ## 262  Golden State Warriors  12/30/16   2016      2                 2
    ## 263  Golden State Warriors   1/28/17   2016      1                 6
    ## 264  Golden State Warriors    2/2/17   2016      3                 6
    ## 265  Golden State Warriors    2/2/17   2016      3                 9
    ## 266  Golden State Warriors    2/2/17   2016      3                 2
    ## 267  Golden State Warriors  10/30/16   2016      4                 9
    ## 268  Golden State Warriors   12/7/16   2016      1                 4
    ## 269  Golden State Warriors   12/7/16   2016      3                 7
    ## 270  Golden State Warriors    1/6/17   2016      1                 4
    ## 271  Golden State Warriors   3/21/17   2016      3                 6
    ## 272  Golden State Warriors  11/18/16   2016      4                 4
    ## 273  Golden State Warriors    1/6/17   2016      3                 2
    ## 274  Golden State Warriors  12/22/16   2016      2                 1
    ## 275  Golden State Warriors   1/25/17   2016      2                 3
    ## 276  Golden State Warriors  11/19/16   2016      1                 5
    ## 277  Golden State Warriors  11/25/16   2016      1                 9
    ## 278  Golden State Warriors   12/1/16   2016      3                 5
    ## 279  Golden State Warriors  11/25/16   2016      3                 7
    ## 280  Golden State Warriors   3/14/17   2016      1                 5
    ## 281  Golden State Warriors   12/8/16   2016      4                 1
    ## 282  Golden State Warriors   1/16/17   2016      2                 4
    ## 283  Golden State Warriors  11/16/16   2016      3                 7
    ## 284  Golden State Warriors   2/11/17   2016      1                 3
    ## 285  Golden State Warriors   2/25/17   2016      3                 0
    ## 286  Golden State Warriors  11/16/16   2016      2                 3
    ## 287  Golden State Warriors    1/8/17   2016      3                 3
    ## 288  Golden State Warriors   3/18/17   2016      1                 0
    ## 289  Golden State Warriors   3/18/17   2016      1                 6
    ## 290  Golden State Warriors   3/28/17   2016      1                 2
    ## 291  Golden State Warriors  11/18/16   2016      1                11
    ## 292  Golden State Warriors   1/28/17   2016      3                 1
    ## 293  Golden State Warriors  12/20/16   2016      1                 3
    ## 294  Golden State Warriors    3/2/17   2016      1                 9
    ## 295  Golden State Warriors  11/13/16   2016      1                 9
    ## 296  Golden State Warriors   1/18/17   2016      2                 0
    ## 297  Golden State Warriors   3/26/17   2016      1                 8
    ## 298  Golden State Warriors  11/26/16   2016      1                 0
    ## 299  Golden State Warriors  12/15/16   2016      2                 7
    ## 300  Golden State Warriors   11/7/16   2016      1                10
    ## 301  Golden State Warriors  11/16/16   2016      3                 4
    ## 302  Golden State Warriors   3/24/17   2016      3                 2
    ## 303  Golden State Warriors   11/3/16   2016      2                10
    ## 304  Golden State Warriors  10/25/16   2016      2                 0
    ## 305  Golden State Warriors   11/3/16   2016      2                11
    ## 306  Golden State Warriors    1/8/17   2016      4                 0
    ## 307  Golden State Warriors   3/16/17   2016      3                 5
    ## 308  Golden State Warriors    1/2/17   2016      4                 7
    ## 309  Golden State Warriors   12/7/16   2016      4                 9
    ## 310  Golden State Warriors   2/28/17   2016      2                 0
    ## 311  Golden State Warriors    2/1/17   2016      2                 0
    ## 312  Golden State Warriors  11/25/16   2016      1                 0
    ## 313  Golden State Warriors   2/13/17   2016      1                 0
    ## 314  Golden State Warriors  12/28/16   2016      2                 3
    ## 315  Golden State Warriors   3/28/17   2016      3                 5
    ## 316  Golden State Warriors   12/7/16   2016      3                 3
    ## 317  Golden State Warriors  11/16/16   2016      2                 1
    ## 318  Golden State Warriors  11/18/16   2016      1                 3
    ## 319  Golden State Warriors   2/13/17   2016      1                 1
    ## 320  Golden State Warriors   2/11/17   2016      1                 0
    ## 321  Golden State Warriors  11/25/16   2016      2                 3
    ## 322  Golden State Warriors   2/10/17   2016      3                 1
    ## 323  Golden State Warriors   1/16/17   2016      2                 5
    ## 324  Golden State Warriors   1/16/17   2016      4                 9
    ## 325  Golden State Warriors    1/6/17   2016      2                 2
    ## 326  Golden State Warriors   2/13/17   2016      1                 2
    ## 327  Golden State Warriors    3/2/17   2016      4                 0
    ## 328  Golden State Warriors    3/2/17   2016      1                 2
    ## 329  Golden State Warriors  10/28/16   2016      4                11
    ## 330  Golden State Warriors  12/17/16   2016      2                 0
    ## 331  Golden State Warriors   12/7/16   2016      2                 3
    ## 332  Golden State Warriors  11/28/16   2016      1                 2
    ## 333  Golden State Warriors   3/26/17   2016      1                 0
    ## 334  Golden State Warriors  11/28/16   2016      4                10
    ## 335  Golden State Warriors  12/28/16   2016      4                 8
    ## 336  Golden State Warriors   3/29/17   2016      4                 2
    ## 337  Golden State Warriors  12/23/16   2016      2                 0
    ## 338  Golden State Warriors   3/31/17   2016      3                 4
    ## 339  Golden State Warriors   3/18/17   2016      1                 3
    ## 340  Golden State Warriors   11/9/16   2016      2                 3
    ## 341  Golden State Warriors   11/1/16   2016      1                 3
    ## 342  Golden State Warriors    1/8/17   2016      1                 3
    ## 343  Golden State Warriors   3/24/17   2016      3                 3
    ## 344  Golden State Warriors  10/28/16   2016      3                 0
    ## 345  Golden State Warriors   3/28/17   2016      1                 3
    ## 346  Golden State Warriors   3/31/17   2016      4                 0
    ## 347  Golden State Warriors   3/20/17   2016      2                 2
    ## 348  Golden State Warriors   2/28/17   2016      4                 5
    ## 349  Golden State Warriors   1/28/17   2016      3                 3
    ## 350  Golden State Warriors   2/23/17   2016      3                 5
    ## 351  Golden State Warriors  10/30/16   2016      2                 0
    ## 352  Golden State Warriors   1/28/17   2016      2                 0
    ## 353  Golden State Warriors   1/18/17   2016      4                10
    ## 354  Golden State Warriors  12/28/16   2016      3                 1
    ## 355  Golden State Warriors  11/18/16   2016      2                 3
    ## 356  Golden State Warriors   11/1/16   2016      4                 9
    ## 357  Golden State Warriors   3/18/17   2016      4                 8
    ## 358  Golden State Warriors   3/14/17   2016      2                 7
    ## 359  Golden State Warriors   12/7/16   2016      4                11
    ## 360  Golden State Warriors  11/26/16   2016      3                 3
    ## 361  Golden State Warriors    1/8/17   2016      1                 0
    ## 362  Golden State Warriors   12/8/16   2016      2                 8
    ## 363  Golden State Warriors   11/1/16   2016      3                 5
    ## 364  Golden State Warriors  12/11/16   2016      4                 2
    ## 365  Golden State Warriors  11/21/16   2016      1                 2
    ## 366  Golden State Warriors   2/13/17   2016      3                 3
    ## 367  Golden State Warriors  11/21/16   2016      2                 2
    ## 368  Golden State Warriors   1/20/17   2016      1                 0
    ## 369  Golden State Warriors    1/2/17   2016      2                11
    ## 370  Golden State Warriors   3/29/17   2016      1                 5
    ## 371  Golden State Warriors  11/13/16   2016      3                 1
    ## 372  Golden State Warriors  10/28/16   2016      1                 3
    ## 373  Golden State Warriors   3/20/17   2016      2                 5
    ## 374  Golden State Warriors  11/18/16   2016      4                 8
    ## 375  Golden State Warriors   3/16/17   2016      4                 8
    ## 376  Golden State Warriors   12/7/16   2016      4                 8
    ## 377  Golden State Warriors    2/2/17   2016      3                 1
    ## 378  Golden State Warriors   12/8/16   2016      2                 8
    ## 379  Golden State Warriors  11/19/16   2016      4                 3
    ## 380  Golden State Warriors   2/11/17   2016      1                 4
    ## 381  Golden State Warriors   3/16/17   2016      3                 5
    ## 382  Golden State Warriors  11/18/16   2016      4                 8
    ## 383  Golden State Warriors  10/30/16   2016      4                 9
    ## 384  Golden State Warriors   2/15/17   2016      4                 9
    ## 385  Golden State Warriors    2/4/17   2016      4                 2
    ## 386  Golden State Warriors   12/7/16   2016      2                 9
    ## 387  Golden State Warriors   2/27/17   2016      3                 0
    ## 388  Golden State Warriors    1/6/17   2016      2                 3
    ## 389  Golden State Warriors   3/14/17   2016      2                 0
    ## 390  Golden State Warriors   2/28/17   2016      2                 9
    ## 391  Golden State Warriors   3/16/17   2016      2                 8
    ## 392  Golden State Warriors  11/23/16   2016      4                10
    ## 393  Golden State Warriors   3/24/17   2016      3                 3
    ## 394  Golden State Warriors    2/1/17   2016      1                 5
    ## 395  Golden State Warriors   3/26/17   2016      3                 2
    ## 396  Golden State Warriors   12/1/16   2016      1                 8
    ## 397  Golden State Warriors   1/20/17   2016      2                 1
    ## 398  Golden State Warriors  11/16/16   2016      4                11
    ## 399  Golden State Warriors   3/16/17   2016      3                 1
    ## 400  Golden State Warriors  12/28/16   2016      1                 0
    ## 401  Golden State Warriors   3/16/17   2016      2                 9
    ## 402  Golden State Warriors   3/26/17   2016      4                 1
    ## 403  Golden State Warriors    3/6/17   2016      1                 3
    ## 404  Golden State Warriors   2/11/17   2016      1                 2
    ## 405  Golden State Warriors   2/10/17   2016      2                 0
    ## 406  Golden State Warriors   3/14/17   2016      2                 8
    ## 407  Golden State Warriors   2/10/17   2016      4                 4
    ## 408  Golden State Warriors   3/14/17   2016      3                 7
    ## 409  Golden State Warriors  12/15/16   2016      1                 4
    ## 410  Golden State Warriors   3/18/17   2016      3                 6
    ## 411  Golden State Warriors    4/4/17   2016      1                 1
    ## 412  Golden State Warriors  12/15/16   2016      2                 9
    ## 413  Golden State Warriors    3/2/17   2016      2                 9
    ## 414  Golden State Warriors   3/26/17   2016      2                10
    ## 415  Golden State Warriors   2/23/17   2016      1                 0
    ## 416  Golden State Warriors   1/16/17   2016      2                 9
    ## 417  Golden State Warriors  12/25/16   2016      4                10
    ## 418  Golden State Warriors   1/29/17   2016      1                 3
    ## 419  Golden State Warriors   1/29/17   2016      2                11
    ## 420  Golden State Warriors  11/23/16   2016      2                 2
    ## 421  Golden State Warriors  11/10/16   2016      1                 0
    ## 422  Golden State Warriors  11/18/16   2016      1                 4
    ## 423  Golden State Warriors   11/3/16   2016      1                 4
    ## 424  Golden State Warriors  11/28/16   2016      4                11
    ## 425  Golden State Warriors   2/15/17   2016      2                10
    ## 426  Golden State Warriors    1/4/17   2016      4                 7
    ## 427  Golden State Warriors    3/2/17   2016      1                 2
    ## 428  Golden State Warriors   11/1/16   2016      3                 1
    ## 429  Golden State Warriors   12/5/16   2016      1                 2
    ## 430  Golden State Warriors   3/31/17   2016      4                 7
    ## 431  Golden State Warriors   11/1/16   2016      2                 1
    ## 432  Golden State Warriors   3/24/17   2016      1                 0
    ## 433  Golden State Warriors   3/14/17   2016      3                 4
    ## 434  Golden State Warriors   3/24/17   2016      3                 0
    ## 435  Golden State Warriors   3/16/17   2016      3                 4
    ## 436  Golden State Warriors    2/1/17   2016      4                 5
    ## 437  Golden State Warriors   3/14/17   2016      4                 7
    ## 438  Golden State Warriors  12/20/16   2016      4                 7
    ## 439  Golden State Warriors    4/4/17   2016      3                 3
    ## 440  Golden State Warriors    2/2/17   2016      4                 9
    ## 441  Golden State Warriors    2/2/17   2016      3                 1
    ## 442  Golden State Warriors   3/31/17   2016      3                 4
    ## 443  Golden State Warriors   1/25/17   2016      2                 3
    ## 444  Golden State Warriors  12/20/16   2016      1                 0
    ## 445  Golden State Warriors   1/18/17   2016      4                 7
    ## 446  Golden State Warriors   1/18/17   2016      4                10
    ## 447  Golden State Warriors    3/5/17   2016      3                 7
    ## 448  Golden State Warriors  12/30/16   2016      3                 7
    ## 449  Golden State Warriors   1/16/17   2016      1                 9
    ## 450  Golden State Warriors   12/7/16   2016      2                 0
    ## 451  Golden State Warriors  11/19/16   2016      1                 8
    ## 452  Golden State Warriors   1/22/17   2016      3                 9
    ## 453  Golden State Warriors   11/4/16   2016      4                 6
    ## 454  Golden State Warriors   3/16/17   2016      3                 1
    ## 455  Golden State Warriors    2/4/17   2016      4                 0
    ## 456  Golden State Warriors  12/20/16   2016      3                 3
    ## 457  Golden State Warriors    1/2/17   2016      1                10
    ## 458  Golden State Warriors  12/25/16   2016      4                 1
    ## 459  Golden State Warriors  11/21/16   2016      1                 8
    ## 460  Golden State Warriors   1/20/17   2016      2                 2
    ## 461  Golden State Warriors   2/27/17   2016      1                 5
    ## 462  Golden State Warriors  11/16/16   2016      2                 3
    ## 463  Golden State Warriors   2/27/17   2016      4                 4
    ## 464  Golden State Warriors   11/9/16   2016      4                 8
    ## 465  Golden State Warriors   12/7/16   2016      1                 6
    ## 466  Golden State Warriors   3/28/17   2016      1                 9
    ## 467  Golden State Warriors   3/20/17   2016      3                 9
    ## 468  Golden State Warriors   3/18/17   2016      1                 1
    ## 469  Golden State Warriors  11/19/16   2016      1                 8
    ## 470  Golden State Warriors  10/30/16   2016      1                 1
    ## 471  Golden State Warriors   3/21/17   2016      2                 3
    ## 472  Golden State Warriors  12/25/16   2016      2                 8
    ## 473  Golden State Warriors   1/25/17   2016      3                 6
    ## 474  Golden State Warriors  11/13/16   2016      4                 2
    ## 475  Golden State Warriors  10/25/16   2016      1                 9
    ## 476  Golden State Warriors  12/15/16   2016      3                 9
    ## 477  Golden State Warriors   1/29/17   2016      1                 6
    ## 478  Golden State Warriors    1/2/17   2016      4                 0
    ## 479  Golden State Warriors  12/28/16   2016      3                 7
    ## 480  Golden State Warriors   2/11/17   2016      2                10
    ## 481  Golden State Warriors   11/9/16   2016      1                 2
    ## 482  Golden State Warriors   2/10/17   2016      2                 3
    ## 483  Golden State Warriors  10/25/16   2016      2                 6
    ## 484  Golden State Warriors  11/25/16   2016      1                 4
    ## 485  Golden State Warriors  12/28/16   2016      2                 4
    ## 486  Golden State Warriors   12/7/16   2016      3                11
    ## 487  Golden State Warriors  11/25/16   2016      2                 6
    ## 488  Golden State Warriors   1/18/17   2016      1                 7
    ## 489  Golden State Warriors   12/5/16   2016      3                 3
    ## 490  Golden State Warriors   11/7/16   2016      1                 4
    ## 491  Golden State Warriors   12/1/16   2016      1                 5
    ## 492  Golden State Warriors   3/31/17   2016      2                 3
    ## 493  Golden State Warriors  12/30/16   2016      1                 5
    ## 494  Golden State Warriors    4/4/17   2016      2                 4
    ## 495  Golden State Warriors   2/28/17   2016      2                 4
    ## 496  Golden State Warriors    1/2/17   2016      2                 3
    ## 497  Golden State Warriors   12/1/16   2016      4                 4
    ## 498  Golden State Warriors   2/11/17   2016      4                10
    ## 499  Golden State Warriors   2/10/17   2016      4                10
    ## 500  Golden State Warriors   3/18/17   2016      2                 4
    ## 501  Golden State Warriors    2/8/17   2016      2                 0
    ## 502  Golden State Warriors   3/16/17   2016      2                 8
    ## 503  Golden State Warriors   3/31/17   2016      1                 7
    ## 504  Golden State Warriors   2/27/17   2016      3                11
    ## 505  Golden State Warriors   2/23/17   2016      4                11
    ## 506  Golden State Warriors  11/28/16   2016      1                 5
    ## 507  Golden State Warriors  12/25/16   2016      3                 2
    ## 508  Golden State Warriors   1/18/17   2016      4                 3
    ## 509  Golden State Warriors   1/16/17   2016      4                 6
    ## 510  Golden State Warriors  12/30/16   2016      3                 5
    ## 511  Golden State Warriors   3/29/17   2016      1                11
    ## 512  Golden State Warriors    4/4/17   2016      2                 0
    ## 513  Golden State Warriors   2/28/17   2016      3                10
    ## 514  Golden State Warriors   11/7/16   2016      1                11
    ## 515  Golden State Warriors  11/10/16   2016      3                 0
    ## 516  Golden State Warriors   3/24/17   2016      3                11
    ## 517  Golden State Warriors   12/1/16   2016      1                 6
    ## 518  Golden State Warriors   1/16/17   2016      2                11
    ## 519  Golden State Warriors   3/18/17   2016      4                 5
    ## 520  Golden State Warriors    3/2/17   2016      3                 1
    ## 521  Golden State Warriors   2/11/17   2016      2                 2
    ## 522  Golden State Warriors  11/21/16   2016      3                 9
    ## 523  Golden State Warriors    3/2/17   2016      1                 0
    ## 524  Golden State Warriors   2/11/17   2016      4                 4
    ## 525  Golden State Warriors   2/13/17   2016      2                 8
    ## 526  Golden State Warriors    3/6/17   2016      1                 9
    ## 527  Golden State Warriors  10/25/16   2016      1                 2
    ## 528  Golden State Warriors    1/8/17   2016      4                10
    ## 529  Golden State Warriors   3/28/17   2016      3                10
    ## 530  Golden State Warriors    1/6/17   2016      4                 6
    ## 531  Golden State Warriors   1/29/17   2016      4                 2
    ## 532  Golden State Warriors  11/28/16   2016      4                 6
    ## 533  Golden State Warriors    2/1/17   2016      2                 1
    ## 534  Golden State Warriors   12/8/16   2016      1                 9
    ## 535  Golden State Warriors  11/18/16   2016      4                 1
    ## 536  Golden State Warriors   12/1/16   2016      2                 2
    ## 537  Golden State Warriors  11/16/16   2016      2                 0
    ## 538  Golden State Warriors   3/14/17   2016      4                 3
    ## 539  Golden State Warriors   11/4/16   2016      4                 6
    ## 540  Golden State Warriors   12/8/16   2016      3                 4
    ## 541  Golden State Warriors    3/5/17   2016      2                 1
    ## 542  Golden State Warriors  10/30/16   2016      3                10
    ## 543  Golden State Warriors   3/14/17   2016      2                 4
    ## 544  Golden State Warriors   1/20/17   2016      3                11
    ## 545  Golden State Warriors   3/16/17   2016      3                 7
    ## 546  Golden State Warriors    3/5/17   2016      4                10
    ## 547  Golden State Warriors  12/30/16   2016      2                 4
    ## 548  Golden State Warriors    3/2/17   2016      1                 8
    ## 549  Golden State Warriors  12/23/16   2016      4                 1
    ## 550  Golden State Warriors   12/7/16   2016      3                 7
    ## 551  Golden State Warriors    2/4/17   2016      4                11
    ## 552  Golden State Warriors   3/28/17   2016      2                 2
    ## 553  Golden State Warriors   1/28/17   2016      2                 2
    ## 554  Golden State Warriors  12/25/16   2016      4                 6
    ## 555  Golden State Warriors   1/18/17   2016      3                 9
    ## 556  Golden State Warriors  11/25/16   2016      3                 7
    ## 557  Golden State Warriors   11/4/16   2016      2                 5
    ## 558  Golden State Warriors  12/25/16   2016      3                 6
    ## 559  Golden State Warriors    3/6/17   2016      3                 4
    ## 560  Golden State Warriors  11/28/16   2016      3                 6
    ## 561  Golden State Warriors   3/31/17   2016      3                11
    ## 562  Golden State Warriors   3/29/17   2016      3                 0
    ## 563  Golden State Warriors   2/27/17   2016      2                 5
    ## 564  Golden State Warriors    2/4/17   2016      1                 6
    ## 565  Golden State Warriors    2/4/17   2016      2                11
    ## 566  Golden State Warriors   1/20/17   2016      4                 8
    ## 567  Golden State Warriors   1/18/17   2016      3                 8
    ## 568  Golden State Warriors  12/28/16   2016      3                 2
    ## 569  Golden State Warriors  12/17/16   2016      1                 2
    ## 570  Golden State Warriors  12/10/16   2016      1                 6
    ## 571  Golden State Warriors   12/3/16   2016      1                 3
    ## 572  Golden State Warriors   12/1/16   2016      3                 9
    ## 573  Golden State Warriors   11/7/16   2016      2                 1
    ## 574  Golden State Warriors   11/3/16   2016      1                 9
    ## 575  Golden State Warriors  10/28/16   2016      2                 5
    ## 576  Golden State Warriors  10/28/16   2016      2                 1
    ## 577  Golden State Warriors   3/31/17   2016      2                 2
    ## 578  Golden State Warriors   2/13/17   2016      3                 9
    ## 579  Golden State Warriors   2/23/17   2016      2                11
    ## 580  Golden State Warriors   2/25/17   2016      3                 7
    ## 581  Golden State Warriors  10/28/16   2016      1                 6
    ## 582  Golden State Warriors   11/4/16   2016      4                 7
    ## 583  Golden State Warriors   12/1/16   2016      4                 7
    ## 584  Golden State Warriors   11/9/16   2016      1                 8
    ## 585  Golden State Warriors   2/27/17   2016      3                 1
    ## 586  Golden State Warriors  10/28/16   2016      2                 5
    ## 587  Golden State Warriors    4/4/17   2016      3                 9
    ## 588  Golden State Warriors   2/28/17   2016      4                 3
    ## 589  Golden State Warriors   2/28/17   2016      1                 5
    ## 590  Golden State Warriors  12/11/16   2016      1                 5
    ## 591  Golden State Warriors  11/25/16   2016      1                 8
    ## 592  Golden State Warriors  11/23/16   2016      1                 5
    ## 593  Golden State Warriors  11/23/16   2016      1                 4
    ## 594  Golden State Warriors  10/28/16   2016      3                 9
    ## 595  Golden State Warriors  11/10/16   2016      3                 5
    ## 596  Golden State Warriors  12/13/16   2016      3                 7
    ## 597  Golden State Warriors  12/13/16   2016      3                 8
    ## 598  Golden State Warriors  11/21/16   2016      1                 2
    ## 599  Golden State Warriors    3/5/17   2016      1                10
    ## 600  Golden State Warriors  11/19/16   2016      4                 1
    ## 601  Golden State Warriors    3/6/17   2016      1                 6
    ## 602  Golden State Warriors  11/19/16   2016      4                 0
    ## 603  Golden State Warriors   11/3/16   2016      3                11
    ## 604  Golden State Warriors  12/13/16   2016      3                 9
    ## 605  Golden State Warriors  12/13/16   2016      2                 3
    ## 606  Golden State Warriors  12/13/16   2016      3                 9
    ## 607  Golden State Warriors  11/18/16   2016      1                 7
    ## 608  Golden State Warriors   11/1/16   2016      1                 7
    ## 609  Golden State Warriors   3/24/17   2016      4                 5
    ## 610  Golden State Warriors    1/4/17   2016      2                 6
    ## 611  Golden State Warriors   3/24/17   2016      4                 5
    ## 612  Golden State Warriors   3/16/17   2016      2                10
    ## 613  Golden State Warriors    1/8/17   2016      2                 3
    ## 614  Golden State Warriors  12/15/16   2016      1                 5
    ## 615  Golden State Warriors    1/8/17   2016      2                 0
    ## 616  Golden State Warriors   3/28/17   2016      1                 9
    ## 617  Golden State Warriors    1/8/17   2016      1                 5
    ## 618  Golden State Warriors   1/28/17   2016      1                 8
    ## 619  Golden State Warriors   1/18/17   2016      3                 9
    ## 620  Golden State Warriors   1/18/17   2016      3                10
    ## 621  Golden State Warriors  12/17/16   2016      3                10
    ## 622  Golden State Warriors   3/18/17   2016      2                 1
    ## 623  Golden State Warriors   1/25/17   2016      1                 4
    ## 624  Golden State Warriors  11/16/16   2016      3                 8
    ## 625  Golden State Warriors  12/25/16   2016      2                 6
    ## 626  Golden State Warriors   3/16/17   2016      3                 6
    ## 627  Golden State Warriors   1/25/17   2016      4                 2
    ## 628  Golden State Warriors   1/22/17   2016      3                 5
    ## 629  Golden State Warriors   3/14/17   2016      4                11
    ## 630  Golden State Warriors  11/16/16   2016      1                 1
    ## 631  Golden State Warriors   2/11/17   2016      4                 2
    ## 632  Golden State Warriors   12/8/16   2016      1                 9
    ## 633  Golden State Warriors    2/4/17   2016      2                 1
    ## 634  Golden State Warriors  10/28/16   2016      2                 4
    ## 635  Golden State Warriors   12/3/16   2016      1                 6
    ## 636  Golden State Warriors  12/10/16   2016      3                 6
    ## 637  Golden State Warriors   11/3/16   2016      1                 4
    ## 638  Golden State Warriors  10/25/16   2016      3                11
    ## 639  Golden State Warriors  12/10/16   2016      2                 2
    ## 640  Golden State Warriors   12/7/16   2016      2                 1
    ## 641  Golden State Warriors   11/3/16   2016      1                 5
    ## 642  Golden State Warriors   3/28/17   2016      4                 3
    ## 643  Golden State Warriors  11/13/16   2016      1                 2
    ## 644  Golden State Warriors   2/27/17   2016      3                 1
    ## 645  Golden State Warriors  11/25/16   2016      3                 6
    ## 646  Golden State Warriors  11/10/16   2016      1                 1
    ## 647  Golden State Warriors   11/4/16   2016      3                 9
    ## 648  Golden State Warriors   3/24/17   2016      2                 4
    ## 649  Golden State Warriors   3/24/17   2016      2                 6
    ## 650  Golden State Warriors  12/30/16   2016      1                 8
    ## 651  Golden State Warriors   1/18/17   2016      1                 8
    ## 652  Golden State Warriors   3/14/17   2016      3                11
    ## 653  Golden State Warriors  10/25/16   2016      3                 8
    ## 654  Golden State Warriors  10/30/16   2016      3                 2
    ## 655  Golden State Warriors   2/28/17   2016      3                 5
    ## 656  Golden State Warriors  12/20/16   2016      2                 2
    ## 657  Golden State Warriors   1/25/17   2016      4                 1
    ## 658  Golden State Warriors  11/23/16   2016      2                 0
    ## 659  Golden State Warriors   2/10/17   2016      2                 0
    ## 660  Golden State Warriors   3/26/17   2016      1                 7
    ## 661  Golden State Warriors  12/20/16   2016      3                 5
    ## 662  Golden State Warriors   12/1/16   2016      4                 5
    ## 663  Golden State Warriors  12/20/16   2016      1                 7
    ## 664  Golden State Warriors   11/3/16   2016      1                 6
    ## 665  Golden State Warriors  11/16/16   2016      2                 1
    ## 666  Golden State Warriors  12/28/16   2016      1                11
    ## 667  Golden State Warriors   1/16/17   2016      1                 1
    ## 668  Golden State Warriors   2/27/17   2016      1                 0
    ## 669  Golden State Warriors  11/28/16   2016      2                 3
    ## 670  Golden State Warriors   12/1/16   2016      2                 1
    ## 671  Golden State Warriors  12/28/16   2016      3                 2
    ## 672  Golden State Warriors   11/4/16   2016      3                 3
    ## 673  Golden State Warriors   2/25/17   2016      1                 9
    ## 674  Golden State Warriors   12/1/16   2016      1                 5
    ## 675  Golden State Warriors   3/24/17   2016      1                 0
    ## 676  Golden State Warriors    1/8/17   2016      3                10
    ## 677  Golden State Warriors    1/2/17   2016      3                 1
    ## 678  Golden State Warriors   2/13/17   2016      1                 5
    ## 679  Golden State Warriors    3/6/17   2016      1                 7
    ## 680  Golden State Warriors  10/25/16   2016      2                 5
    ## 681  Golden State Warriors  11/19/16   2016      3                 2
    ## 682  Golden State Warriors   12/5/16   2016      1                10
    ## 683  Golden State Warriors   3/28/17   2016      2                 0
    ## 684  Golden State Warriors   3/28/17   2016      2                 0
    ## 685  Golden State Warriors   1/22/17   2016      1                 6
    ## 686  Golden State Warriors   1/20/17   2016      4                 8
    ## 687  Golden State Warriors   3/14/17   2016      3                 6
    ## 688  Golden State Warriors  12/15/16   2016      3                 9
    ## 689  Golden State Warriors  12/15/16   2016      3                 9
    ## 690  Golden State Warriors  12/15/16   2016      4                 3
    ## 691  Golden State Warriors  12/30/16   2016      3                 1
    ## 692  Golden State Warriors    1/2/17   2016      4                 0
    ## 693  Golden State Warriors  11/18/16   2016      1                 1
    ## 694  Golden State Warriors  11/21/16   2016      3                 8
    ## 695  Golden State Warriors  11/21/16   2016      2                 2
    ## 696  Golden State Warriors   11/3/16   2016      1                 3
    ## 697  Golden State Warriors   11/3/16   2016      1                 3
    ## 698  Golden State Warriors  10/25/16   2016      3                 5
    ## 699  Golden State Warriors   11/7/16   2016      4                 3
    ## 700  Golden State Warriors  10/28/16   2016      2                 5
    ## 701  Golden State Warriors  12/10/16   2016      2                 2
    ## 702  Golden State Warriors   12/1/16   2016      4                 7
    ## 703  Golden State Warriors  12/10/16   2016      3                10
    ## 704  Golden State Warriors   3/26/17   2016      3                 0
    ## 705  Golden State Warriors   12/5/16   2016      3                 9
    ## 706  Golden State Warriors  10/28/16   2016      1                11
    ## 707  Golden State Warriors   3/26/17   2016      4                 1
    ## 708  Golden State Warriors  10/25/16   2016      3                 8
    ## 709  Golden State Warriors  12/23/16   2016      3                 9
    ## 710  Golden State Warriors  11/18/16   2016      3                 8
    ## 711  Golden State Warriors   12/5/16   2016      3                 9
    ## 712  Golden State Warriors   11/4/16   2016      3                 5
    ## 713  Golden State Warriors  10/30/16   2016      4                 1
    ## 714  Golden State Warriors    2/8/17   2016      3                10
    ## 715  Golden State Warriors   2/23/17   2016      3                11
    ## 716  Golden State Warriors  11/13/16   2016      3                 5
    ## 717  Golden State Warriors  11/19/16   2016      1                10
    ## 718  Golden State Warriors   1/20/17   2016      3                 6
    ## 719  Golden State Warriors  11/13/16   2016      1                11
    ## 720  Golden State Warriors    1/2/17   2016      2                 3
    ## 721  Golden State Warriors  11/10/16   2016      1                 9
    ## 722  Golden State Warriors  11/16/16   2016      1                 9
    ## 723  Golden State Warriors   11/3/16   2016      3                11
    ## 724  Golden State Warriors   1/20/17   2016      3                 9
    ## 725  Golden State Warriors   2/11/17   2016      4                 5
    ## 726  Golden State Warriors    1/8/17   2016      4                 3
    ## 727  Golden State Warriors   1/18/17   2016      2                 4
    ## 728  Golden State Warriors   2/13/17   2016      1                 8
    ## 729  Golden State Warriors    2/4/17   2016      2                 3
    ## 730  Golden State Warriors  12/28/16   2016      2                 9
    ## 731  Golden State Warriors   11/4/16   2016      3                 8
    ## 732  Golden State Warriors  12/13/16   2016      3                 5
    ## 733  Golden State Warriors   12/7/16   2016      1                 8
    ## 734  Golden State Warriors    1/4/17   2016      3                10
    ## 735  Golden State Warriors   2/13/17   2016      2                 0
    ## 736  Golden State Warriors  11/28/16   2016      4                 1
    ## 737  Golden State Warriors   2/10/17   2016      4                 5
    ## 738  Golden State Warriors    1/4/17   2016      3                10
    ## 739  Golden State Warriors  11/10/16   2016      3                 7
    ## 740  Golden State Warriors  11/25/16   2016      1                 6
    ## 741  Golden State Warriors  10/25/16   2016      2                 2
    ## 742  Golden State Warriors   12/5/16   2016      1                 6
    ## 743  Golden State Warriors  11/23/16   2016      1                 6
    ## 744  Golden State Warriors   2/15/17   2016      3                 6
    ## 745  Golden State Warriors   11/1/16   2016      1                 9
    ## 746  Golden State Warriors   2/13/17   2016      3                11
    ## 747  Golden State Warriors  11/23/16   2016      2                 9
    ## 748  Golden State Warriors   2/15/17   2016      2                 2
    ## 749  Golden State Warriors   1/25/17   2016      4                 7
    ## 750  Golden State Warriors    1/4/17   2016      2                 1
    ## 751  Golden State Warriors   1/25/17   2016      1                 8
    ## 752  Golden State Warriors   12/5/16   2016      2                 2
    ## 753  Golden State Warriors  12/20/16   2016      4                 9
    ## 754  Golden State Warriors  12/30/16   2016      4                 7
    ## 755  Golden State Warriors    1/8/17   2016      3                 4
    ## 756  Golden State Warriors  11/23/16   2016      2                10
    ## 757  Golden State Warriors   1/20/17   2016      3                10
    ## 758  Golden State Warriors  11/18/16   2016      4                11
    ## 759  Golden State Warriors  11/13/16   2016      4                 1
    ## 760  Golden State Warriors  12/11/16   2016      1                 8
    ## 761  Golden State Warriors   2/27/17   2016      4                 1
    ## 762  Golden State Warriors   2/27/17   2016      1                 4
    ## 763  Golden State Warriors   2/13/17   2016      2                 5
    ## 764  Golden State Warriors    1/2/17   2016      3                 6
    ## 765  Golden State Warriors  12/15/16   2016      3                 3
    ## 766  Golden State Warriors  11/23/16   2016      4                 9
    ## 767  Golden State Warriors    2/4/17   2016      4                 7
    ## 768  Golden State Warriors  10/28/16   2016      1                 0
    ## 769  Golden State Warriors  12/25/16   2016      3                 0
    ## 770  Golden State Warriors  12/25/16   2016      1                 4
    ## 771  Golden State Warriors   11/7/16   2016      3                 2
    ## 772  Golden State Warriors    2/2/17   2016      1                10
    ## 773  Golden State Warriors   11/7/16   2016      4                 0
    ## 774  Golden State Warriors    1/2/17   2016      1                 6
    ## 775  Golden State Warriors  12/13/16   2016      4                 8
    ## 776  Golden State Warriors   12/8/16   2016      4                 6
    ## 777  Golden State Warriors   1/28/17   2016      2                 0
    ## 778  Golden State Warriors   2/27/17   2016      2                 3
    ## 779  Golden State Warriors  10/25/16   2016      2                 2
    ## 780  Golden State Warriors   1/16/17   2016      2                 3
    ## 781  Golden State Warriors   11/4/16   2016      2                 8
    ## 782  Golden State Warriors  12/17/16   2016      1                 7
    ## 783  Golden State Warriors   1/25/17   2016      1                11
    ## 784  Golden State Warriors   11/3/16   2016      3                 0
    ## 785  Golden State Warriors   11/3/16   2016      2                 2
    ## 786  Golden State Warriors    1/6/17   2016      1                11
    ## 787  Golden State Warriors  12/17/16   2016      2                 7
    ## 788  Golden State Warriors    2/8/17   2016      1                 2
    ## 789  Golden State Warriors   11/7/16   2016      2                 3
    ## 790  Golden State Warriors   11/9/16   2016      4                 5
    ## 791  Golden State Warriors  11/21/16   2016      1                 7
    ## 792  Golden State Warriors    1/6/17   2016      2                 8
    ## 793  Golden State Warriors   12/1/16   2016      1                 0
    ## 794  Golden State Warriors   1/29/17   2016      3                 4
    ## 795  Golden State Warriors   12/1/16   2016      2                 5
    ## 796  Golden State Warriors  11/26/16   2016      2                 7
    ## 797  Golden State Warriors  12/22/16   2016      4                 9
    ## 798  Golden State Warriors  12/15/16   2016      3                 9
    ## 799  Golden State Warriors  11/23/16   2016      2                 1
    ## 800  Golden State Warriors   2/27/17   2016      1                 7
    ## 801  Golden State Warriors   2/11/17   2016      1                 3
    ## 802  Golden State Warriors   2/27/17   2016      1                 2
    ## 803  Golden State Warriors  11/23/16   2016      3                 8
    ## 804  Golden State Warriors    2/2/17   2016      3                11
    ## 805  Golden State Warriors   1/16/17   2016      2                 5
    ## 806  Golden State Warriors    2/2/17   2016      3                 5
    ## 807  Golden State Warriors  10/30/16   2016      1                11
    ## 808  Golden State Warriors   2/23/17   2016      3                 8
    ## 809  Golden State Warriors   2/13/17   2016      3                 5
    ## 810  Golden State Warriors   1/18/17   2016      1                11
    ## 811  Golden State Warriors   12/1/16   2016      1                 7
    ## 812  Golden State Warriors  10/30/16   2016      1                 9
    ## 813  Golden State Warriors    2/4/17   2016      3                 3
    ## 814  Golden State Warriors   12/7/16   2016      2                 9
    ## 815  Golden State Warriors   1/25/17   2016      3                 8
    ## 816  Golden State Warriors  11/25/16   2016      4                 3
    ## 817  Golden State Warriors  10/28/16   2016      2                 0
    ## 818  Golden State Warriors   2/11/17   2016      3                 8
    ## 819  Golden State Warriors  10/28/16   2016      1                 1
    ## 820  Golden State Warriors   11/7/16   2016      3                 8
    ## 821  Golden State Warriors    1/2/17   2016      3                 8
    ## 822  Golden State Warriors   12/1/16   2016      2                 9
    ## 823  Golden State Warriors   1/22/17   2016      3                11
    ## 824  Golden State Warriors   11/4/16   2016      4                 9
    ## 825  Golden State Warriors   1/22/17   2016      1                 2
    ## 826  Golden State Warriors  12/23/16   2016      2                 9
    ## 827  Golden State Warriors   2/15/17   2016      2                 4
    ## 828  Golden State Warriors    1/8/17   2016      3                11
    ## 829  Golden State Warriors  12/22/16   2016      2                11
    ## 830  Golden State Warriors    1/4/17   2016      1                 2
    ## 831  Golden State Warriors  11/16/16   2016      2                11
    ## 832  Golden State Warriors   12/5/16   2016      2                 7
    ## 833  Golden State Warriors   1/28/17   2016      2                 4
    ## 834  Golden State Warriors  10/25/16   2016      4                 5
    ## 835  Golden State Warriors  12/11/16   2016      4                 7
    ## 836  Golden State Warriors    1/4/17   2016      2                 2
    ## 837  Golden State Warriors    2/8/17   2016      2                 4
    ## 838  Golden State Warriors  12/11/16   2016      3                 8
    ## 839  Golden State Warriors   11/9/16   2016      2                 3
    ## 840  Golden State Warriors  12/23/16   2016      2                 7
    ## 841  Golden State Warriors   2/27/17   2016      1                11
    ## 842  Golden State Warriors   11/1/16   2016      1                 6
    ## 843  Golden State Warriors  12/17/16   2016      1                10
    ## 844  Golden State Warriors   1/25/17   2016      1                 3
    ## 845  Golden State Warriors   1/20/17   2016      3                11
    ## 846  Golden State Warriors   11/3/16   2016      1                 1
    ## 847  Golden State Warriors  12/15/16   2016      2                 6
    ## 848  Golden State Warriors  12/13/16   2016      3                 4
    ## 849  Golden State Warriors  11/10/16   2016      4                 6
    ## 850  Golden State Warriors   12/1/16   2016      1                11
    ## 851  Golden State Warriors  12/22/16   2016      3                11
    ## 852  Golden State Warriors   11/4/16   2016      3                11
    ## 853  Golden State Warriors   2/23/17   2016      2                 5
    ## 854  Golden State Warriors  12/13/16   2016      1                 0
    ## 855  Golden State Warriors  12/15/16   2016      2                 1
    ## 856  Golden State Warriors   11/7/16   2016      4                 8
    ## 857  Golden State Warriors  10/28/16   2016      2                 2
    ## 858  Golden State Warriors  10/28/16   2016      4                 2
    ## 859  Golden State Warriors    2/2/17   2016      1                11
    ## 860  Golden State Warriors  11/23/16   2016      3                 6
    ## 861  Golden State Warriors  11/26/16   2016      1                10
    ## 862  Golden State Warriors  11/26/16   2016      2                 7
    ## 863  Golden State Warriors  12/23/16   2016      1                 8
    ## 864  Golden State Warriors   12/3/16   2016      2                 9
    ## 865  Golden State Warriors  12/11/16   2016      3                10
    ## 866  Golden State Warriors   1/29/17   2016      4                 7
    ## 867  Golden State Warriors    1/8/17   2016      2                 0
    ## 868  Golden State Warriors   1/29/17   2016      3                 6
    ## 869  Golden State Warriors   1/25/17   2016      4                 2
    ## 870  Golden State Warriors  11/13/16   2016      2                 4
    ## 871  Golden State Warriors    2/1/17   2016      3                 6
    ## 872  Golden State Warriors  12/20/16   2016      3                11
    ## 873  Golden State Warriors    1/2/17   2016      2                 9
    ## 874  Golden State Warriors  12/25/16   2016      4                10
    ## 875  Golden State Warriors  12/13/16   2016      1                 0
    ## 876  Golden State Warriors    2/2/17   2016      2                 0
    ## 877  Golden State Warriors  10/28/16   2016      1                 6
    ## 878  Golden State Warriors  10/28/16   2016      1                 3
    ## 879  Golden State Warriors  10/28/16   2016      4                 0
    ## 880  Golden State Warriors   12/7/16   2016      2                10
    ## 881  Golden State Warriors  11/23/16   2016      2                 0
    ## 882  Golden State Warriors   11/4/16   2016      1                 5
    ## 883  Golden State Warriors  11/10/16   2016      4                 8
    ## 884  Golden State Warriors   2/23/17   2016      2                 5
    ## 885  Golden State Warriors  12/28/16   2016      1                 6
    ## 886  Golden State Warriors    1/8/17   2016      4                 4
    ## 887  Golden State Warriors   12/8/16   2016      2                 9
    ## 888  Golden State Warriors   2/28/17   2016      1                11
    ## 889  Golden State Warriors  12/15/16   2016      1                10
    ## 890  Golden State Warriors  12/20/16   2016      3                11
    ## 891  Golden State Warriors  10/25/16   2016      2                 0
    ## 892  Golden State Warriors  12/11/16   2016      1                 5
    ## 893  Golden State Warriors   12/5/16   2016      2                 1
    ## 894  Golden State Warriors   11/4/16   2016      2                 7
    ## 895  Golden State Warriors  11/13/16   2016      3                 7
    ## 896  Golden State Warriors  12/22/16   2016      2                 8
    ## 897  Golden State Warriors  12/22/16   2016      3                 4
    ## 898  Golden State Warriors   12/1/16   2016      4                 2
    ## 899  Golden State Warriors   12/3/16   2016      3                10
    ## 900  Golden State Warriors   12/1/16   2016      3                11
    ## 901  Golden State Warriors    1/6/17   2016      2                 5
    ## 902  Golden State Warriors    2/1/17   2016      3                 9
    ## 903  Golden State Warriors  12/20/16   2016      3                 9
    ## 904  Golden State Warriors  11/19/16   2016      1                 1
    ## 905  Golden State Warriors  11/19/16   2016      2                11
    ## 906  Golden State Warriors  12/23/16   2016      3                 6
    ## 907  Golden State Warriors   1/16/17   2016      1                 7
    ## 908  Golden State Warriors  12/13/16   2016      1                 9
    ## 909  Golden State Warriors   11/3/16   2016      2                11
    ## 910  Golden State Warriors   11/1/16   2016      3                 5
    ## 911  Golden State Warriors   1/18/17   2016      3                 1
    ## 912  Golden State Warriors   1/28/17   2016      2                 7
    ## 913  Golden State Warriors  11/16/16   2016      3                 8
    ## 914  Golden State Warriors   12/8/16   2016      4                 5
    ## 915  Golden State Warriors  10/28/16   2016      4                 8
    ## 916  Golden State Warriors   1/28/17   2016      1                 3
    ## 917  Golden State Warriors   11/1/16   2016      3                 9
    ## 918  Golden State Warriors  10/25/16   2016      3                10
    ## 919  Golden State Warriors  11/19/16   2016      1                10
    ## 920  Golden State Warriors   12/3/16   2016      3                 7
    ## 921  Golden State Warriors   1/16/17   2016      1                10
    ## 922  Golden State Warriors   1/16/17   2016      2                 1
    ## 923  Golden State Warriors   1/18/17   2016      2                 1
    ## 924  Golden State Warriors   11/3/16   2016      4                 7
    ## 925  Golden State Warriors   2/15/17   2016      4                 8
    ## 926  Golden State Warriors   12/7/16   2016      1                10
    ## 927  Golden State Warriors  11/19/16   2016      3                 5
    ## 928  Golden State Warriors   12/3/16   2016      1                 9
    ## 929  Golden State Warriors   2/11/17   2016      2                 0
    ## 930  Golden State Warriors  12/20/16   2016      1                 7
    ## 931  Golden State Warriors   12/5/16   2016      1                 0
    ## 932  Golden State Warriors  12/25/16   2016      2                 5
    ## 933  Golden State Warriors   12/8/16   2016      4                 2
    ## 934  Golden State Warriors  11/16/16   2016      3                10
    ## 935  Golden State Warriors   2/27/17   2016      3                10
    ## 936  Golden State Warriors    2/8/17   2016      4                 5
    ## 937  Golden State Warriors    2/8/17   2016      3                 6
    ## 938  Golden State Warriors   2/15/17   2016      3                 6
    ## 939  Golden State Warriors   2/23/17   2016      3                 4
    ## 940  Golden State Warriors   2/15/17   2016      1                 6
    ## 941  Golden State Warriors    2/1/17   2016      2                 2
    ## 942  Golden State Warriors  11/13/16   2016      2                10
    ## 943  Golden State Warriors    2/1/17   2016      2                 0
    ## 944  Golden State Warriors  11/28/16   2016      4                 4
    ## 945  Golden State Warriors    2/8/17   2016      1                 5
    ## 946  Golden State Warriors  12/22/16   2016      3                 5
    ## 947  Golden State Warriors   1/29/17   2016      3                10
    ## 948  Golden State Warriors   2/27/17   2016      3                10
    ## 949  Golden State Warriors  10/30/16   2016      2                 0
    ## 950  Golden State Warriors    2/1/17   2016      3                 7
    ## 951  Golden State Warriors  12/28/16   2016      1                 9
    ## 952  Golden State Warriors   1/22/17   2016      1                 3
    ## 953  Golden State Warriors   1/22/17   2016      3                 7
    ## 954  Golden State Warriors  12/15/16   2016      2                 0
    ## 955  Golden State Warriors  12/25/16   2016      1                11
    ## 956  Golden State Warriors  11/26/16   2016      1                 9
    ## 957  Golden State Warriors  12/28/16   2016      2                 1
    ## 958  Golden State Warriors   11/4/16   2016      3                 6
    ## 959  Golden State Warriors    1/8/17   2016      4                 2
    ## 960  Golden State Warriors    1/4/17   2016      4                 2
    ## 961  Golden State Warriors   12/8/16   2016      2                10
    ## 962  Golden State Warriors  12/22/16   2016      4                 7
    ## 963  Golden State Warriors  12/22/16   2016      3                 9
    ## 964  Golden State Warriors  12/22/16   2016      2                 7
    ## 965  Golden State Warriors  12/10/16   2016      1                 1
    ## 966  Golden State Warriors    2/8/17   2016      3                 2
    ## 967  Golden State Warriors  12/22/16   2016      1                 6
    ## 968  Golden State Warriors   11/1/16   2016      4                11
    ## 969  Golden State Warriors   1/22/17   2016      3                 3
    ## 970  Golden State Warriors   11/1/16   2016      4                10
    ## 971  Golden State Warriors   1/29/17   2016      1                 8
    ## 972  Golden State Warriors   1/25/17   2016      4                 8
    ## 973  Golden State Warriors   2/11/17   2016      2                 2
    ## 974  Golden State Warriors    1/6/17   2016      2                11
    ## 975  Golden State Warriors   2/13/17   2016      1                 6
    ## 976  Golden State Warriors   2/11/17   2016      3                 1
    ## 977  Golden State Warriors   1/29/17   2016      2                 5
    ## 978  Golden State Warriors   1/22/17   2016      1                 5
    ## 979  Golden State Warriors   11/7/16   2016      1                 0
    ## 980  Golden State Warriors   1/22/17   2016      2                 1
    ## 981  Golden State Warriors  12/28/16   2016      1                 6
    ## 982  Golden State Warriors  11/21/16   2016      3                 8
    ## 983  Golden State Warriors   1/16/17   2016      2                 4
    ## 984  Golden State Warriors  11/10/16   2016      2                 1
    ## 985  Golden State Warriors   2/11/17   2016      2                 5
    ## 986  Golden State Warriors    2/1/17   2016      1                 1
    ## 987  Golden State Warriors   1/29/17   2016      4                 7
    ## 988  Golden State Warriors   12/5/16   2016      3                 5
    ## 989  Golden State Warriors    1/6/17   2016      1                 4
    ## 990  Golden State Warriors   2/13/17   2016      3                 6
    ## 991  Golden State Warriors   11/1/16   2016      1                 9
    ## 992  Golden State Warriors   2/23/17   2016      4                10
    ## 993  Golden State Warriors   2/25/17   2016      3                 5
    ## 994  Golden State Warriors   3/21/17   2016      4                 8
    ## 995  Golden State Warriors  11/21/16   2016      2                 4
    ## 996  Golden State Warriors  12/30/16   2016      4                 7
    ## 997  Golden State Warriors   11/7/16   2016      4                 7
    ## 998  Golden State Warriors   12/5/16   2016      2                 8
    ## 999  Golden State Warriors  11/25/16   2016      4                 7
    ## 1000 Golden State Warriors  11/13/16   2016      2                 8
    ## 1001 Golden State Warriors   11/7/16   2016      3                10
    ## 1002 Golden State Warriors   3/28/17   2016      1                 6
    ## 1003 Golden State Warriors  12/10/16   2016      2                 5
    ## 1004 Golden State Warriors    2/4/17   2016      3                 8
    ## 1005 Golden State Warriors  12/28/16   2016      1                11
    ## 1006 Golden State Warriors  11/21/16   2016      2                 2
    ## 1007 Golden State Warriors  11/13/16   2016      3                 4
    ## 1008 Golden State Warriors    1/6/17   2016      1                 3
    ## 1009 Golden State Warriors  12/10/16   2016      2                 7
    ## 1010 Golden State Warriors  10/28/16   2016      2                 4
    ## 1011 Golden State Warriors   1/25/17   2016      3                11
    ## 1012 Golden State Warriors   3/16/17   2016      1                 7
    ## 1013 Golden State Warriors   3/20/17   2016      2                 0
    ## 1014 Golden State Warriors   12/5/16   2016      1                 6
    ## 1015 Golden State Warriors   12/5/16   2016      3                 9
    ## 1016 Golden State Warriors  12/15/16   2016      1                 2
    ## 1017 Golden State Warriors   2/10/17   2016      1                 7
    ## 1018 Golden State Warriors   1/20/17   2016      1                 5
    ## 1019 Golden State Warriors   1/16/17   2016      4                 9
    ## 1020 Golden State Warriors    3/2/17   2016      2                10
    ## 1021 Golden State Warriors    3/2/17   2016      2                11
    ## 1022 Golden State Warriors    1/2/17   2016      2                 2
    ## 1023 Golden State Warriors   3/28/17   2016      1                11
    ## 1024 Golden State Warriors   3/18/17   2016      4                 9
    ## 1025 Golden State Warriors    1/8/17   2016      2                 7
    ## 1026 Golden State Warriors  12/22/16   2016      2                 6
    ## 1027 Golden State Warriors   11/7/16   2016      1                11
    ## 1028 Golden State Warriors    1/8/17   2016      3                10
    ## 1029 Golden State Warriors  11/13/16   2016      4                 9
    ## 1030 Golden State Warriors  12/25/16   2016      1                 0
    ## 1031 Golden State Warriors   3/20/17   2016      2                 1
    ## 1032 Golden State Warriors   2/25/17   2016      2                11
    ## 1033 Golden State Warriors   11/1/16   2016      1                 1
    ## 1034 Golden State Warriors  12/11/16   2016      4                11
    ## 1035 Golden State Warriors   3/28/17   2016      1                 9
    ## 1036 Golden State Warriors  11/21/16   2016      2                 7
    ## 1037 Golden State Warriors   3/26/17   2016      2                 6
    ## 1038 Golden State Warriors   12/5/16   2016      1                 3
    ## 1039 Golden State Warriors    1/8/17   2016      2                 6
    ## 1040 Golden State Warriors    1/8/17   2016      2                 6
    ## 1041 Golden State Warriors   1/28/17   2016      1                 7
    ## 1042 Golden State Warriors   1/25/17   2016      2                 9
    ## 1043 Golden State Warriors   1/20/17   2016      1                 7
    ## 1044 Golden State Warriors  10/30/16   2016      4                 6
    ## 1045 Golden State Warriors  12/13/16   2016      4                 6
    ## 1046 Golden State Warriors    4/4/17   2016      4                 6
    ## 1047 Golden State Warriors  11/28/16   2016      2                 0
    ## 1048 Golden State Warriors   2/27/17   2016      2                 1
    ## 1049 Golden State Warriors  10/28/16   2016      3                 8
    ## 1050 Golden State Warriors    2/4/17   2016      2                 0
    ## 1051 Golden State Warriors   12/5/16   2016      3                 8
    ## 1052 Golden State Warriors  12/15/16   2016      2                 1
    ## 1053 Golden State Warriors  10/30/16   2016      2                 9
    ## 1054 Golden State Warriors  10/30/16   2016      2                 4
    ## 1055 Golden State Warriors    2/1/17   2016      1                 7
    ## 1056 Golden State Warriors  11/18/16   2016      2                 0
    ## 1057 Golden State Warriors   2/27/17   2016      2                 8
    ## 1058 Golden State Warriors    1/6/17   2016      2                10
    ## 1059 Golden State Warriors  10/28/16   2016      4                 7
    ## 1060 Golden State Warriors   12/3/16   2016      4                10
    ## 1061 Golden State Warriors   3/26/17   2016      3                 4
    ## 1062 Golden State Warriors   12/5/16   2016      1                 5
    ## 1063 Golden State Warriors   3/18/17   2016      2                11
    ## 1064 Golden State Warriors  11/13/16   2016      2                10
    ## 1065 Golden State Warriors   11/4/16   2016      1                 8
    ## 1066 Golden State Warriors    2/2/17   2016      4                 3
    ## 1067 Golden State Warriors    3/2/17   2016      2                 7
    ## 1068 Golden State Warriors    3/2/17   2016      2                 0
    ## 1069 Golden State Warriors   2/15/17   2016      1                 6
    ## 1070 Golden State Warriors  12/17/16   2016      1                 8
    ## 1071 Golden State Warriors  12/17/16   2016      2                 4
    ## 1072 Golden State Warriors    3/2/17   2016      3                 5
    ## 1073 Golden State Warriors  12/25/16   2016      3                 3
    ## 1074 Golden State Warriors   2/25/17   2016      1                 8
    ## 1075 Golden State Warriors   1/28/17   2016      2                 6
    ## 1076 Golden State Warriors   1/20/17   2016      1                 4
    ## 1077 Golden State Warriors  11/16/16   2016      2                10
    ## 1078 Golden State Warriors  11/10/16   2016      4                 8
    ## 1079 Golden State Warriors  10/25/16   2016      4                 7
    ## 1080 Golden State Warriors  11/18/16   2016      1                 6
    ## 1081 Golden State Warriors   1/28/17   2016      2                 5
    ## 1082 Golden State Warriors  12/17/16   2016      1                 5
    ## 1083 Golden State Warriors   12/1/16   2016      4                 5
    ## 1084 Golden State Warriors   1/16/17   2016      3                 8
    ## 1085 Golden State Warriors    2/1/17   2016      2                10
    ## 1086 Golden State Warriors   1/22/17   2016      2                 8
    ## 1087 Golden State Warriors  12/11/16   2016      2                 7
    ## 1088 Golden State Warriors   1/29/17   2016      2                 1
    ## 1089 Golden State Warriors    3/5/17   2016      4                 3
    ## 1090 Golden State Warriors    3/5/17   2016      3                 6
    ## 1091 Golden State Warriors  12/23/16   2016      2                 9
    ## 1092 Golden State Warriors  12/10/16   2016      2                 2
    ## 1093 Golden State Warriors  12/11/16   2016      1                 7
    ## 1094 Golden State Warriors  11/18/16   2016      1                 6
    ## 1095 Golden State Warriors  12/25/16   2016      1                 9
    ## 1096 Golden State Warriors  12/25/16   2016      4                11
    ## 1097 Golden State Warriors    1/2/17   2016      4                 9
    ## 1098 Golden State Warriors   11/7/16   2016      2                 9
    ## 1099 Golden State Warriors    3/2/17   2016      3                 9
    ## 1100 Golden State Warriors  12/22/16   2016      4                 3
    ## 1101 Golden State Warriors  12/20/16   2016      3                 7
    ## 1102 Golden State Warriors    3/2/17   2016      1                 9
    ## 1103 Golden State Warriors  12/17/16   2016      2                 2
    ## 1104 Golden State Warriors   3/14/17   2016      2                11
    ## 1105 Golden State Warriors  12/22/16   2016      3                 8
    ## 1106 Golden State Warriors   2/11/17   2016      1                 8
    ## 1107 Golden State Warriors   3/21/17   2016      1                10
    ## 1108 Golden State Warriors   2/25/17   2016      2                 7
    ## 1109 Golden State Warriors  11/26/16   2016      4                 9
    ## 1110 Golden State Warriors    1/4/17   2016      2                 7
    ## 1111 Golden State Warriors  10/30/16   2016      2                 0
    ## 1112 Golden State Warriors  11/10/16   2016      2                 0
    ## 1113 Golden State Warriors  12/15/16   2016      3                 4
    ## 1114 Golden State Warriors  12/28/16   2016      3                 8
    ## 1115 Golden State Warriors  11/28/16   2016      2                 1
    ## 1116 Golden State Warriors  12/13/16   2016      1                 7
    ## 1117 Golden State Warriors    2/2/17   2016      4                 2
    ## 1118 Golden State Warriors  11/16/16   2016      3                 4
    ## 1119 Golden State Warriors   1/20/17   2016      1                 9
    ## 1120 Golden State Warriors   2/28/17   2016      2                 9
    ## 1121 Golden State Warriors    2/4/17   2016      1                 4
    ## 1122 Golden State Warriors   3/24/17   2016      3                 8
    ## 1123 Golden State Warriors    1/6/17   2016      1                 8
    ## 1124 Golden State Warriors    1/6/17   2016      1                11
    ## 1125 Golden State Warriors    4/4/17   2016      4                10
    ## 1126 Golden State Warriors  10/25/16   2016      3                 9
    ## 1127 Golden State Warriors   12/5/16   2016      3                10
    ## 1128 Golden State Warriors   2/27/17   2016      2                 9
    ## 1129 Golden State Warriors   11/1/16   2016      2                 2
    ## 1130 Golden State Warriors  12/23/16   2016      3                11
    ## 1131 Golden State Warriors   3/28/17   2016      2                 9
    ## 1132 Golden State Warriors    2/2/17   2016      1                 1
    ## 1133 Golden State Warriors   3/31/17   2016      3                11
    ## 1134 Golden State Warriors   12/5/16   2016      3                 3
    ## 1135 Golden State Warriors   11/4/16   2016      2                 1
    ## 1136 Golden State Warriors    2/4/17   2016      4                 8
    ## 1137 Golden State Warriors    1/2/17   2016      3                 6
    ## 1138 Golden State Warriors   2/28/17   2016      4                10
    ## 1139 Golden State Warriors    1/2/17   2016      4                11
    ## 1140 Golden State Warriors    2/8/17   2016      4                 5
    ## 1141 Golden State Warriors   3/31/17   2016      2                 1
    ## 1142 Golden State Warriors    3/5/17   2016      2                 6
    ## 1143 Golden State Warriors  12/17/16   2016      3                11
    ## 1144 Golden State Warriors  12/13/16   2016      3                10
    ## 1145 Golden State Warriors    3/5/17   2016      3                 5
    ## 1146 Golden State Warriors   3/31/17   2016      3                 4
    ## 1147 Golden State Warriors    4/4/17   2016      2                11
    ## 1148 Golden State Warriors  12/13/16   2016      1                 7
    ## 1149 Golden State Warriors    3/5/17   2016      1                11
    ## 1150 Golden State Warriors    2/2/17   2016      1                 1
    ## 1151 Golden State Warriors   3/29/17   2016      3                 3
    ## 1152 Golden State Warriors   11/4/16   2016      3                10
    ## 1153 Golden State Warriors  11/10/16   2016      3                 3
    ## 1154 Golden State Warriors   2/28/17   2016      4                10
    ## 1155 Golden State Warriors   3/18/17   2016      1                10
    ## 1156 Golden State Warriors    2/8/17   2016      3                 9
    ## 1157 Golden State Warriors  11/26/16   2016      4                 9
    ## 1158 Golden State Warriors   11/4/16   2016      1                10
    ## 1159 Golden State Warriors   1/18/17   2016      3                11
    ## 1160 Golden State Warriors  12/13/16   2016      4                10
    ## 1161 Golden State Warriors    1/8/17   2016      3                 5
    ## 1162 Golden State Warriors   11/1/16   2016      2                 2
    ## 1163 Golden State Warriors   12/8/16   2016      1                 5
    ## 1164 Golden State Warriors   11/1/16   2016      2                 2
    ## 1165 Golden State Warriors   12/1/16   2016      1                 3
    ## 1166 Golden State Warriors  10/28/16   2016      2                 3
    ## 1167 Golden State Warriors   11/9/16   2016      2                 1
    ## 1168 Golden State Warriors   3/29/17   2016      4                10
    ## 1169 Golden State Warriors   2/10/17   2016      1                 7
    ## 1170 Golden State Warriors    1/8/17   2016      4                 9
    ## 1171 Golden State Warriors  12/28/16   2016      4                 3
    ## 1172 Golden State Warriors  10/28/16   2016      1                 0
    ## 1173 Golden State Warriors  11/28/16   2016      4                 7
    ## 1174 Golden State Warriors  11/28/16   2016      2                 1
    ## 1175 Golden State Warriors   3/20/17   2016      1                 9
    ## 1176 Golden State Warriors  11/21/16   2016      2                 3
    ## 1177 Golden State Warriors  11/19/16   2016      1                 4
    ## 1178 Golden State Warriors  11/13/16   2016      3                10
    ## 1179 Golden State Warriors   1/29/17   2016      4                 8
    ## 1180 Golden State Warriors  12/11/16   2016      2                10
    ## 1181 Golden State Warriors  12/10/16   2016      1                 8
    ## 1182 Golden State Warriors  11/25/16   2016      3                 0
    ## 1183 Golden State Warriors  11/25/16   2016      1                 4
    ## 1184 Golden State Warriors   2/11/17   2016      4                 3
    ## 1185 Golden State Warriors  12/15/16   2016      1                 0
    ## 1186 Golden State Warriors   3/31/17   2016      3                10
    ## 1187 Golden State Warriors    3/5/17   2016      2                 2
    ## 1188 Golden State Warriors    2/4/17   2016      1                 8
    ## 1189 Golden State Warriors    2/8/17   2016      4                 9
    ## 1190 Golden State Warriors   3/21/17   2016      1                 7
    ## 1191 Golden State Warriors    4/4/17   2016      3                 6
    ## 1192 Golden State Warriors    2/4/17   2016      3                 9
    ## 1193 Golden State Warriors    1/4/17   2016      3                 5
    ## 1194 Golden State Warriors   2/15/17   2016      3                10
    ## 1195 Golden State Warriors   12/3/16   2016      3                10
    ## 1196 Golden State Warriors  12/22/16   2016      2                 0
    ## 1197 Golden State Warriors   12/8/16   2016      1                 8
    ## 1198 Golden State Warriors   3/26/17   2016      3                 6
    ## 1199 Golden State Warriors  10/30/16   2016      4                 8
    ## 1200 Golden State Warriors   12/7/16   2016      2                 5
    ## 1201 Golden State Warriors   12/5/16   2016      1                 3
    ## 1202 Golden State Warriors   12/3/16   2016      3                 8
    ## 1203 Golden State Warriors   2/15/17   2016      3                 5
    ## 1204 Golden State Warriors   3/20/17   2016      1                 5
    ## 1205 Golden State Warriors  11/23/16   2016      1                 0
    ## 1206 Golden State Warriors  10/25/16   2016      1                 4
    ## 1207 Golden State Warriors   3/28/17   2016      1                11
    ## 1208 Golden State Warriors  11/25/16   2016      2                 1
    ## 1209 Golden State Warriors  10/25/16   2016      1                10
    ## 1210 Golden State Warriors   12/5/16   2016      1                 4
    ## 1211 Golden State Warriors    1/6/17   2016      1                 8
    ## 1212 Golden State Warriors   1/29/17   2016      2                10
    ## 1213 Golden State Warriors    2/2/17   2016      1                 1
    ## 1214 Golden State Warriors   3/31/17   2016      3                 8
    ## 1215 Golden State Warriors  12/15/16   2016      3                 4
    ## 1216 Golden State Warriors   3/18/17   2016      4                 6
    ##      seconds_remaining shot_made_flag                     action_type
    ## 1                   51      made shot  Cutting Finger Roll Layup Shot
    ## 2                   14      made shot  Cutting Finger Roll Layup Shot
    ## 3                    8      made shot  Cutting Finger Roll Layup Shot
    ## 4                   27      made shot  Cutting Finger Roll Layup Shot
    ## 5                    4      made shot  Cutting Finger Roll Layup Shot
    ## 6                   36      made shot  Cutting Finger Roll Layup Shot
    ## 7                   51      made shot  Cutting Finger Roll Layup Shot
    ## 8                   40      made shot  Cutting Finger Roll Layup Shot
    ## 9                   59      made shot              Cutting Layup Shot
    ## 10                  54      made shot              Cutting Layup Shot
    ## 11                  36      made shot              Cutting Layup Shot
    ## 12                  34      made shot              Cutting Layup Shot
    ## 13                  12    missed shot              Cutting Layup Shot
    ## 14                  29      made shot              Cutting Layup Shot
    ## 15                  40      made shot              Cutting Layup Shot
    ## 16                  40      made shot              Cutting Layup Shot
    ## 17                  14      made shot              Cutting Layup Shot
    ## 18                  51      made shot              Cutting Layup Shot
    ## 19                   8      made shot              Cutting Layup Shot
    ## 20                  51    missed shot              Cutting Layup Shot
    ## 21                  39      made shot              Cutting Layup Shot
    ## 22                  12      made shot              Cutting Layup Shot
    ## 23                  55      made shot              Cutting Layup Shot
    ## 24                  16      made shot              Cutting Layup Shot
    ## 25                  20      made shot              Cutting Layup Shot
    ## 26                  36      made shot              Cutting Layup Shot
    ## 27                  31      made shot              Cutting Layup Shot
    ## 28                   9    missed shot              Cutting Layup Shot
    ## 29                  41      made shot              Cutting Layup Shot
    ## 30                  22      made shot              Cutting Layup Shot
    ## 31                  28      made shot              Cutting Layup Shot
    ## 32                   3      made shot              Cutting Layup Shot
    ## 33                  41      made shot              Cutting Layup Shot
    ## 34                  28      made shot              Cutting Layup Shot
    ## 35                   6      made shot              Cutting Layup Shot
    ## 36                  38      made shot              Cutting Layup Shot
    ## 37                  41      made shot               Driving Bank shot
    ## 38                  42      made shot  Driving Finger Roll Layup Shot
    ## 39                  53      made shot  Driving Finger Roll Layup Shot
    ## 40                  52      made shot  Driving Finger Roll Layup Shot
    ## 41                  48      made shot  Driving Finger Roll Layup Shot
    ## 42                   6      made shot  Driving Finger Roll Layup Shot
    ## 43                  22      made shot  Driving Finger Roll Layup Shot
    ## 44                  24    missed shot  Driving Finger Roll Layup Shot
    ## 45                  59      made shot  Driving Finger Roll Layup Shot
    ## 46                  22      made shot  Driving Finger Roll Layup Shot
    ## 47                  16    missed shot  Driving Finger Roll Layup Shot
    ## 48                  52      made shot  Driving Finger Roll Layup Shot
    ## 49                  49    missed shot  Driving Finger Roll Layup Shot
    ## 50                  13      made shot  Driving Finger Roll Layup Shot
    ## 51                   0    missed shot  Driving Finger Roll Layup Shot
    ## 52                   9      made shot  Driving Finger Roll Layup Shot
    ## 53                   3      made shot  Driving Finger Roll Layup Shot
    ## 54                  31      made shot  Driving Finger Roll Layup Shot
    ## 55                  49      made shot  Driving Finger Roll Layup Shot
    ## 56                  49      made shot  Driving Finger Roll Layup Shot
    ## 57                  47      made shot  Driving Finger Roll Layup Shot
    ## 58                  18      made shot  Driving Finger Roll Layup Shot
    ## 59                  25      made shot  Driving Finger Roll Layup Shot
    ## 60                  28      made shot  Driving Finger Roll Layup Shot
    ## 61                  29    missed shot  Driving Finger Roll Layup Shot
    ## 62                  54      made shot  Driving Finger Roll Layup Shot
    ## 63                  36      made shot  Driving Finger Roll Layup Shot
    ## 64                  10      made shot  Driving Finger Roll Layup Shot
    ## 65                  57    missed shot  Driving Finger Roll Layup Shot
    ## 66                  15      made shot  Driving Finger Roll Layup Shot
    ## 67                  59      made shot  Driving Finger Roll Layup Shot
    ## 68                  28      made shot Driving Floating Bank Jump Shot
    ## 69                  27    missed shot      Driving Floating Jump Shot
    ## 70                  16      made shot      Driving Floating Jump Shot
    ## 71                  29      made shot      Driving Floating Jump Shot
    ## 72                  46      made shot      Driving Floating Jump Shot
    ## 73                  56      made shot               Driving Hook Shot
    ## 74                  11    missed shot               Driving Hook Shot
    ## 75                  56      made shot              Driving Layup Shot
    ## 76                  44      made shot              Driving Layup Shot
    ## 77                  53      made shot              Driving Layup Shot
    ## 78                  38      made shot              Driving Layup Shot
    ## 79                   4      made shot              Driving Layup Shot
    ## 80                  53      made shot              Driving Layup Shot
    ## 81                   1      made shot              Driving Layup Shot
    ## 82                  47      made shot              Driving Layup Shot
    ## 83                  43      made shot              Driving Layup Shot
    ## 84                  58      made shot              Driving Layup Shot
    ## 85                   8      made shot              Driving Layup Shot
    ## 86                  42      made shot              Driving Layup Shot
    ## 87                  23      made shot              Driving Layup Shot
    ## 88                  33    missed shot              Driving Layup Shot
    ## 89                  48      made shot              Driving Layup Shot
    ## 90                  10    missed shot              Driving Layup Shot
    ## 91                  46    missed shot              Driving Layup Shot
    ## 92                   8      made shot              Driving Layup Shot
    ## 93                  21      made shot              Driving Layup Shot
    ## 94                  44      made shot              Driving Layup Shot
    ## 95                  51      made shot              Driving Layup Shot
    ## 96                  40      made shot              Driving Layup Shot
    ## 97                  41      made shot              Driving Layup Shot
    ## 98                  17      made shot              Driving Layup Shot
    ## 99                  48      made shot              Driving Layup Shot
    ## 100                 42    missed shot              Driving Layup Shot
    ## 101                  2    missed shot              Driving Layup Shot
    ## 102                 20    missed shot              Driving Layup Shot
    ## 103                  6    missed shot              Driving Layup Shot
    ## 104                 45      made shot              Driving Layup Shot
    ## 105                  5      made shot              Driving Layup Shot
    ## 106                  0    missed shot              Driving Layup Shot
    ## 107                 16      made shot              Driving Layup Shot
    ## 108                  7      made shot              Driving Layup Shot
    ## 109                 59      made shot              Driving Layup Shot
    ## 110                 46      made shot              Driving Layup Shot
    ## 111                 29    missed shot              Driving Layup Shot
    ## 112                 34      made shot              Driving Layup Shot
    ## 113                 22    missed shot              Driving Layup Shot
    ## 114                 14      made shot              Driving Layup Shot
    ## 115                 38      made shot              Driving Layup Shot
    ## 116                  3    missed shot              Driving Layup Shot
    ## 117                 40    missed shot              Driving Layup Shot
    ## 118                  6      made shot              Driving Layup Shot
    ## 119                 42      made shot              Driving Layup Shot
    ## 120                 51      made shot              Driving Layup Shot
    ## 121                 36      made shot              Driving Layup Shot
    ## 122                 48      made shot              Driving Layup Shot
    ## 123                 27    missed shot              Driving Layup Shot
    ## 124                 38      made shot              Driving Layup Shot
    ## 125                 46      made shot              Driving Layup Shot
    ## 126                  8      made shot              Driving Layup Shot
    ## 127                 48      made shot              Driving Layup Shot
    ## 128                 29      made shot              Driving Layup Shot
    ## 129                 33      made shot              Driving Layup Shot
    ## 130                 10      made shot              Driving Layup Shot
    ## 131                 40    missed shot              Driving Layup Shot
    ## 132                 23      made shot              Driving Layup Shot
    ## 133                 35      made shot      Driving Reverse Layup Shot
    ## 134                 45    missed shot      Driving Reverse Layup Shot
    ## 135                 28      made shot      Driving Reverse Layup Shot
    ## 136                 16    missed shot      Driving Reverse Layup Shot
    ## 137                 24      made shot      Driving Reverse Layup Shot
    ## 138                 58      made shot      Driving Reverse Layup Shot
    ## 139                 54      made shot      Driving Reverse Layup Shot
    ## 140                 35      made shot      Driving Reverse Layup Shot
    ## 141                 22    missed shot      Driving Reverse Layup Shot
    ## 142                 37      made shot      Driving Reverse Layup Shot
    ## 143                 45      made shot      Driving Reverse Layup Shot
    ## 144                 18      made shot      Driving Reverse Layup Shot
    ## 145                 59      made shot      Driving Reverse Layup Shot
    ## 146                 14      made shot      Driving Reverse Layup Shot
    ## 147                 13      made shot      Driving Reverse Layup Shot
    ## 148                 48    missed shot      Driving Reverse Layup Shot
    ## 149                  6      made shot      Driving Reverse Layup Shot
    ## 150                 18      made shot      Driving Reverse Layup Shot
    ## 151                 11      made shot      Driving Reverse Layup Shot
    ## 152                 33      made shot      Driving Reverse Layup Shot
    ## 153                  0    missed shot      Driving Reverse Layup Shot
    ## 154                 49    missed shot      Driving Reverse Layup Shot
    ## 155                 21      made shot      Driving Reverse Layup Shot
    ## 156                  8      made shot                       Dunk Shot
    ## 157                 49      made shot          Finger Roll Layup Shot
    ## 158                 41      made shot          Finger Roll Layup Shot
    ## 159                 19      made shot          Finger Roll Layup Shot
    ## 160                 18    missed shot              Floating Jump shot
    ## 161                 41    missed shot              Floating Jump shot
    ## 162                 35    missed shot                       Jump Shot
    ## 163                  9    missed shot                       Jump Shot
    ## 164                 10    missed shot                       Jump Shot
    ## 165                 10    missed shot                       Jump Shot
    ## 166                 28    missed shot                       Jump Shot
    ## 167                 24    missed shot                       Jump Shot
    ## 168                 55    missed shot                       Jump Shot
    ## 169                 36    missed shot                       Jump Shot
    ## 170                 16    missed shot                       Jump Shot
    ## 171                 18    missed shot                      Layup Shot
    ## 172                 51    missed shot                      Layup Shot
    ## 173                 10    missed shot                      Layup Shot
    ## 174                 41      made shot                      Layup Shot
    ## 175                 53      made shot                      Layup Shot
    ## 176                 25    missed shot                      Layup Shot
    ## 177                 18    missed shot                      Layup Shot
    ## 178                 44    missed shot                      Layup Shot
    ## 179                 45    missed shot                      Layup Shot
    ## 180                 52    missed shot                      Layup Shot
    ## 181                 41    missed shot                      Layup Shot
    ## 182                 23      made shot                      Layup Shot
    ## 183                 49    missed shot                      Layup Shot
    ## 184                 29      made shot                      Layup Shot
    ## 185                 21    missed shot                      Layup Shot
    ## 186                  0      made shot                      Layup Shot
    ## 187                 35    missed shot                      Layup Shot
    ## 188                 28    missed shot                      Layup Shot
    ## 189                  1    missed shot                      Layup Shot
    ## 190                 26    missed shot                      Layup Shot
    ## 191                 40      made shot                      Layup Shot
    ## 192                 36      made shot                      Layup Shot
    ## 193                  1      made shot                      Layup Shot
    ## 194                 54    missed shot                      Layup Shot
    ## 195                 51    missed shot                      Layup Shot
    ## 196                  6      made shot                      Layup Shot
    ## 197                 33    missed shot                      Layup Shot
    ## 198                  5    missed shot                      Layup Shot
    ## 199                 54    missed shot                      Layup Shot
    ## 200                  2    missed shot                      Layup Shot
    ## 201                 24      made shot                      Layup Shot
    ## 202                 15    missed shot                      Layup Shot
    ## 203                 30    missed shot                      Layup Shot
    ## 204                 44    missed shot                      Layup Shot
    ## 205                  2      made shot                      Layup Shot
    ## 206                 26    missed shot                      Layup Shot
    ## 207                 16      made shot                      Layup Shot
    ## 208                 13    missed shot                      Layup Shot
    ## 209                  8    missed shot                      Layup Shot
    ## 210                 55    missed shot                      Layup Shot
    ## 211                  5    missed shot                      Layup Shot
    ## 212                 47    missed shot                      Layup Shot
    ## 213                 34    missed shot                      Layup Shot
    ## 214                 59    missed shot                      Layup Shot
    ## 215                 36    missed shot                      Layup Shot
    ## 216                 48      made shot                      Layup Shot
    ## 217                 13    missed shot                      Layup Shot
    ## 218                 35    missed shot                      Layup Shot
    ## 219                  3    missed shot                      Layup Shot
    ## 220                 44    missed shot                      Layup Shot
    ## 221                 26      made shot                      Layup Shot
    ## 222                 26      made shot                      Layup Shot
    ## 223                 43    missed shot                      Layup Shot
    ## 224                 32    missed shot                      Layup Shot
    ## 225                 33    missed shot                      Layup Shot
    ## 226                 42    missed shot                      Layup Shot
    ## 227                 21    missed shot                      Layup Shot
    ## 228                 41    missed shot                      Layup Shot
    ## 229                 28      made shot                      Layup Shot
    ## 230                 51    missed shot                      Layup Shot
    ## 231                 19      made shot              Putback Layup Shot
    ## 232                 56      made shot              Putback Layup Shot
    ## 233                  9      made shot              Reverse Layup Shot
    ## 234                 30      made shot              Reverse Layup Shot
    ## 235                 32      made shot              Reverse Layup Shot
    ## 236                 53      made shot              Reverse Layup Shot
    ## 237                 23      made shot              Reverse Layup Shot
    ## 238                  3      made shot              Reverse Layup Shot
    ## 239                 55    missed shot              Reverse Layup Shot
    ## 240                 55    missed shot              Reverse Layup Shot
    ## 241                 19    missed shot              Reverse Layup Shot
    ## 242                 52      made shot              Reverse Layup Shot
    ## 243                 41    missed shot              Reverse Layup Shot
    ## 244                 19      made shot              Reverse Layup Shot
    ## 245                 17    missed shot               Running Dunk Shot
    ## 246                 13      made shot               Running Dunk Shot
    ## 247                 11      made shot               Running Dunk Shot
    ## 248                 58      made shot  Running Finger Roll Layup Shot
    ## 249                  2      made shot  Running Finger Roll Layup Shot
    ## 250                  4      made shot  Running Finger Roll Layup Shot
    ## 251                 45      made shot  Running Finger Roll Layup Shot
    ## 252                  0      made shot  Running Finger Roll Layup Shot
    ## 253                 58      made shot  Running Finger Roll Layup Shot
    ## 254                 29      made shot  Running Finger Roll Layup Shot
    ## 255                 12      made shot  Running Finger Roll Layup Shot
    ## 256                 52      made shot  Running Finger Roll Layup Shot
    ## 257                  5      made shot  Running Finger Roll Layup Shot
    ## 258                  8    missed shot               Running Jump Shot
    ## 259                 42    missed shot               Running Jump Shot
    ## 260                  5      made shot              Running Layup Shot
    ## 261                  4      made shot              Running Layup Shot
    ## 262                 13    missed shot              Running Layup Shot
    ## 263                 21      made shot              Running Layup Shot
    ## 264                  7      made shot              Running Layup Shot
    ## 265                 59    missed shot              Running Layup Shot
    ## 266                  1    missed shot              Running Layup Shot
    ## 267                 51    missed shot              Running Layup Shot
    ## 268                 17      made shot              Running Layup Shot
    ## 269                 47      made shot              Running Layup Shot
    ## 270                 28      made shot              Running Layup Shot
    ## 271                 18      made shot              Running Layup Shot
    ## 272                 30    missed shot              Running Layup Shot
    ## 273                 50    missed shot              Running Layup Shot
    ## 274                 59    missed shot              Running Layup Shot
    ## 275                 16    missed shot              Running Layup Shot
    ## 276                  9      made shot              Running Layup Shot
    ## 277                  0    missed shot              Running Layup Shot
    ## 278                  1      made shot              Running Layup Shot
    ## 279                 37      made shot              Running Layup Shot
    ## 280                 21      made shot              Running Layup Shot
    ## 281                 29      made shot              Running Layup Shot
    ## 282                 18    missed shot              Running Layup Shot
    ## 283                 27      made shot              Running Layup Shot
    ## 284                 19      made shot              Running Layup Shot
    ## 285                  2      made shot              Running Layup Shot
    ## 286                 20      made shot              Running Layup Shot
    ## 287                  1      made shot              Running Layup Shot
    ## 288                 35      made shot      Running Reverse Layup Shot
    ## 289                 51      made shot      Running Reverse Layup Shot
    ## 290                 44      made shot      Running Reverse Layup Shot
    ## 291                  6    missed shot      Running Reverse Layup Shot
    ## 292                 46      made shot      Running Reverse Layup Shot
    ## 293                 28      made shot      Running Reverse Layup Shot
    ## 294                 35    missed shot      Running Reverse Layup Shot
    ## 295                 14      made shot      Running Reverse Layup Shot
    ## 296                 14      made shot      Running Reverse Layup Shot
    ## 297                 48    missed shot      Running Reverse Layup Shot
    ## 298                 30    missed shot      Running Reverse Layup Shot
    ## 299                 40    missed shot                  Tip Layup Shot
    ## 300                  4      made shot                  Tip Layup Shot
    ## 301                 51    missed shot                  Tip Layup Shot
    ## 302                 35    missed shot             Alley Oop Dunk Shot
    ## 303                 51    missed shot             Alley Oop Dunk Shot
    ## 304                  6      made shot             Alley Oop Dunk Shot
    ## 305                 10    missed shot            Alley Oop Layup shot
    ## 306                 32      made shot               Cutting Dunk Shot
    ## 307                 30      made shot               Cutting Dunk Shot
    ## 308                 57      made shot               Cutting Dunk Shot
    ## 309                 31      made shot               Cutting Dunk Shot
    ## 310                 23      made shot               Cutting Dunk Shot
    ## 311                  2      made shot               Cutting Dunk Shot
    ## 312                 44      made shot               Cutting Dunk Shot
    ## 313                 41      made shot               Cutting Dunk Shot
    ## 314                 34      made shot               Cutting Dunk Shot
    ## 315                 56      made shot               Cutting Dunk Shot
    ## 316                 23      made shot               Cutting Dunk Shot
    ## 317                 23      made shot               Cutting Dunk Shot
    ## 318                 36      made shot               Cutting Dunk Shot
    ## 319                  3      made shot               Cutting Dunk Shot
    ## 320                 59      made shot               Cutting Dunk Shot
    ## 321                 24      made shot               Cutting Dunk Shot
    ## 322                 21      made shot  Cutting Finger Roll Layup Shot
    ## 323                  8      made shot  Cutting Finger Roll Layup Shot
    ## 324                 39      made shot              Cutting Layup Shot
    ## 325                 34    missed shot              Cutting Layup Shot
    ## 326                 55      made shot              Cutting Layup Shot
    ## 327                 56      made shot               Driving Dunk Shot
    ## 328                 35      made shot               Driving Dunk Shot
    ## 329                  8      made shot               Driving Dunk Shot
    ## 330                  8      made shot               Driving Dunk Shot
    ## 331                 28      made shot               Driving Dunk Shot
    ## 332                 29      made shot               Driving Dunk Shot
    ## 333                 45      made shot               Driving Dunk Shot
    ## 334                 39      made shot               Driving Dunk Shot
    ## 335                  7      made shot  Driving Finger Roll Layup Shot
    ## 336                 20    missed shot  Driving Finger Roll Layup Shot
    ## 337                 49      made shot              Driving Layup Shot
    ## 338                 13    missed shot              Driving Layup Shot
    ## 339                 45      made shot       Driving Reverse Dunk Shot
    ## 340                 48      made shot      Driving Reverse Layup Shot
    ## 341                 11    missed shot      Driving Reverse Layup Shot
    ## 342                 53      made shot                       Dunk Shot
    ## 343                  1      made shot                       Dunk Shot
    ## 344                  2      made shot                       Dunk Shot
    ## 345                 15      made shot                       Dunk Shot
    ## 346                 29      made shot                       Dunk Shot
    ## 347                 32      made shot                       Dunk Shot
    ## 348                 20    missed shot                       Dunk Shot
    ## 349                 44      made shot                       Dunk Shot
    ## 350                 22      made shot                       Dunk Shot
    ## 351                 43      made shot                       Dunk Shot
    ## 352                 28      made shot                       Dunk Shot
    ## 353                 24      made shot                       Dunk Shot
    ## 354                 10      made shot          Finger Roll Layup Shot
    ## 355                  4      made shot                       Hook Shot
    ## 356                 22    missed shot                       Hook Shot
    ## 357                 11    missed shot                       Jump Shot
    ## 358                 38    missed shot                       Jump Shot
    ## 359                 13    missed shot                       Jump Shot
    ## 360                 14    missed shot                       Jump Shot
    ## 361                  2    missed shot                      Layup Shot
    ## 362                  6    missed shot                      Layup Shot
    ## 363                  6    missed shot                      Layup Shot
    ## 364                  0    missed shot                      Layup Shot
    ## 365                 34    missed shot                      Layup Shot
    ## 366                 42      made shot                      Layup Shot
    ## 367                 10    missed shot                      Layup Shot
    ## 368                 38    missed shot                      Layup Shot
    ## 369                 28      made shot                      Layup Shot
    ## 370                 24    missed shot                      Layup Shot
    ## 371                 54    missed shot                      Layup Shot
    ## 372                 48    missed shot                      Layup Shot
    ## 373                  3    missed shot                      Layup Shot
    ## 374                 27    missed shot                      Layup Shot
    ## 375                 39    missed shot                      Layup Shot
    ## 376                 11      made shot                      Layup Shot
    ## 377                 26    missed shot                      Layup Shot
    ## 378                 41      made shot               Putback Dunk Shot
    ## 379                 39      made shot               Putback Dunk Shot
    ## 380                 25    missed shot               Putback Dunk Shot
    ## 381                 51      made shot               Putback Dunk Shot
    ## 382                 25      made shot              Putback Layup Shot
    ## 383                 48      made shot              Putback Layup Shot
    ## 384                 46      made shot              Putback Layup Shot
    ## 385                 35      made shot               Reverse Dunk Shot
    ## 386                 53      made shot               Reverse Dunk Shot
    ## 387                 47      made shot               Reverse Dunk Shot
    ## 388                 28      made shot               Reverse Dunk Shot
    ## 389                 37    missed shot              Reverse Layup Shot
    ## 390                 29      made shot              Reverse Layup Shot
    ## 391                  3      made shot     Running Alley Oop Dunk Shot
    ## 392                  2      made shot     Running Alley Oop Dunk Shot
    ## 393                 28      made shot               Running Dunk Shot
    ## 394                  2      made shot               Running Dunk Shot
    ## 395                  8      made shot               Running Dunk Shot
    ## 396                 50      made shot               Running Dunk Shot
    ## 397                 28      made shot               Running Dunk Shot
    ## 398                  6      made shot               Running Dunk Shot
    ## 399                 51      made shot               Running Dunk Shot
    ## 400                 36      made shot               Running Dunk Shot
    ## 401                 15      made shot               Running Dunk Shot
    ## 402                 43      made shot               Running Dunk Shot
    ## 403                 28      made shot               Running Dunk Shot
    ## 404                 41      made shot               Running Dunk Shot
    ## 405                 28      made shot               Running Dunk Shot
    ## 406                 10      made shot               Running Dunk Shot
    ## 407                 39      made shot               Running Dunk Shot
    ## 408                  4      made shot               Running Dunk Shot
    ## 409                 13      made shot               Running Dunk Shot
    ## 410                  3      made shot               Running Dunk Shot
    ## 411                 26      made shot  Running Finger Roll Layup Shot
    ## 412                 59      made shot  Running Finger Roll Layup Shot
    ## 413                 25      made shot               Running Jump Shot
    ## 414                  6      made shot               Running Jump Shot
    ## 415                 54      made shot              Running Layup Shot
    ## 416                 27      made shot              Running Layup Shot
    ## 417                 34    missed shot              Running Layup Shot
    ## 418                 54    missed shot              Running Layup Shot
    ## 419                 25      made shot              Running Layup Shot
    ## 420                 19    missed shot              Running Layup Shot
    ## 421                 12      made shot              Running Layup Shot
    ## 422                  4      made shot              Running Layup Shot
    ## 423                 42    missed shot              Running Layup Shot
    ## 424                 11      made shot              Running Layup Shot
    ## 425                 55    missed shot              Running Layup Shot
    ## 426                  9    missed shot              Running Layup Shot
    ## 427                 10      made shot              Running Layup Shot
    ## 428                 55      made shot              Running Layup Shot
    ## 429                 45      made shot              Running Layup Shot
    ## 430                  7      made shot              Running Layup Shot
    ## 431                  0      made shot              Running Layup Shot
    ## 432                 58      made shot              Running Layup Shot
    ## 433                  5    missed shot              Running Layup Shot
    ## 434                 37      made shot       Running Reverse Dunk Shot
    ## 435                 39      made shot      Running Reverse Layup Shot
    ## 436                 31      made shot      Running Reverse Layup Shot
    ## 437                 11    missed shot      Running Reverse Layup Shot
    ## 438                 32    missed shot      Running Reverse Layup Shot
    ## 439                 46      made shot      Running Reverse Layup Shot
    ## 440                 48      made shot      Running Reverse Layup Shot
    ## 441                 24    missed shot                   Tip Dunk Shot
    ## 442                 12      made shot                  Tip Layup Shot
    ## 443                 14      made shot                  Tip Layup Shot
    ## 444                  0    missed shot            Turnaround Hook Shot
    ## 445                 30    missed shot            Turnaround Hook Shot
    ## 446                 41      made shot            Turnaround Jump Shot
    ## 447                 50      made shot            Alley Oop Layup shot
    ## 448                 48      made shot            Alley Oop Layup shot
    ## 449                  6      made shot            Alley Oop Layup shot
    ## 450                 43      made shot               Cutting Dunk Shot
    ## 451                 43      made shot               Cutting Dunk Shot
    ## 452                 27      made shot               Cutting Dunk Shot
    ## 453                  6      made shot               Cutting Dunk Shot
    ## 454                 20      made shot               Cutting Dunk Shot
    ## 455                 57      made shot               Cutting Dunk Shot
    ## 456                 22      made shot               Cutting Dunk Shot
    ## 457                 50      made shot               Cutting Dunk Shot
    ## 458                 31      made shot               Cutting Dunk Shot
    ## 459                 13      made shot               Cutting Dunk Shot
    ## 460                 22      made shot               Cutting Dunk Shot
    ## 461                 36      made shot               Cutting Dunk Shot
    ## 462                 43      made shot               Cutting Dunk Shot
    ## 463                 21      made shot               Cutting Dunk Shot
    ## 464                 48      made shot               Cutting Dunk Shot
    ## 465                 31      made shot               Cutting Dunk Shot
    ## 466                  6      made shot               Cutting Dunk Shot
    ## 467                  8    missed shot               Cutting Dunk Shot
    ## 468                  7      made shot               Cutting Dunk Shot
    ## 469                 19      made shot               Cutting Dunk Shot
    ## 470                 58      made shot               Cutting Dunk Shot
    ## 471                 24      made shot               Cutting Dunk Shot
    ## 472                 48      made shot              Cutting Layup Shot
    ## 473                 54      made shot              Cutting Layup Shot
    ## 474                 39      made shot              Cutting Layup Shot
    ## 475                 28    missed shot              Cutting Layup Shot
    ## 476                 39      made shot              Cutting Layup Shot
    ## 477                 16      made shot              Cutting Layup Shot
    ## 478                 53      made shot              Cutting Layup Shot
    ## 479                 14      made shot              Cutting Layup Shot
    ## 480                 37      made shot              Cutting Layup Shot
    ## 481                 15      made shot              Cutting Layup Shot
    ## 482                 16      made shot              Cutting Layup Shot
    ## 483                 14      made shot              Cutting Layup Shot
    ## 484                 14      made shot              Cutting Layup Shot
    ## 485                 16      made shot              Cutting Layup Shot
    ## 486                 38      made shot              Cutting Layup Shot
    ## 487                 17    missed shot              Cutting Layup Shot
    ## 488                 54    missed shot              Cutting Layup Shot
    ## 489                 40      made shot              Cutting Layup Shot
    ## 490                  1      made shot              Cutting Layup Shot
    ## 491                 41      made shot              Cutting Layup Shot
    ## 492                  5      made shot               Driving Bank shot
    ## 493                 57      made shot               Driving Bank shot
    ## 494                 56      made shot               Driving Dunk Shot
    ## 495                 16      made shot               Driving Dunk Shot
    ## 496                 42      made shot               Driving Dunk Shot
    ## 497                  7      made shot               Driving Dunk Shot
    ## 498                 15    missed shot               Driving Dunk Shot
    ## 499                 11    missed shot               Driving Dunk Shot
    ## 500                 43      made shot               Driving Dunk Shot
    ## 501                  8      made shot               Driving Dunk Shot
    ## 502                 57      made shot               Driving Dunk Shot
    ## 503                  7      made shot               Driving Dunk Shot
    ## 504                 44    missed shot               Driving Dunk Shot
    ## 505                 43      made shot               Driving Dunk Shot
    ## 506                 32      made shot               Driving Dunk Shot
    ## 507                 59      made shot               Driving Dunk Shot
    ## 508                 20      made shot               Driving Dunk Shot
    ## 509                 59    missed shot               Driving Dunk Shot
    ## 510                 17      made shot               Driving Dunk Shot
    ## 511                 21    missed shot  Driving Finger Roll Layup Shot
    ## 512                 48      made shot  Driving Finger Roll Layup Shot
    ## 513                 29      made shot  Driving Finger Roll Layup Shot
    ## 514                 45    missed shot      Driving Floating Jump Shot
    ## 515                 44    missed shot               Driving Hook Shot
    ## 516                 34      made shot              Driving Layup Shot
    ## 517                  5      made shot              Driving Layup Shot
    ## 518                 34      made shot              Driving Layup Shot
    ## 519                 47    missed shot              Driving Layup Shot
    ## 520                 25      made shot              Driving Layup Shot
    ## 521                 29    missed shot              Driving Layup Shot
    ## 522                 15      made shot              Driving Layup Shot
    ## 523                 56    missed shot              Driving Layup Shot
    ## 524                 44    missed shot              Driving Layup Shot
    ## 525                 53    missed shot              Driving Layup Shot
    ## 526                 57      made shot              Driving Layup Shot
    ## 527                 58      made shot              Driving Layup Shot
    ## 528                 40      made shot              Driving Layup Shot
    ## 529                 52      made shot              Driving Layup Shot
    ## 530                 47    missed shot              Driving Layup Shot
    ## 531                 50    missed shot              Driving Layup Shot
    ## 532                 55    missed shot              Driving Layup Shot
    ## 533                 47      made shot              Driving Layup Shot
    ## 534                 37    missed shot              Driving Layup Shot
    ## 535                 29    missed shot              Driving Layup Shot
    ## 536                 17      made shot              Driving Layup Shot
    ## 537                 31    missed shot              Driving Layup Shot
    ## 538                  7    missed shot              Driving Layup Shot
    ## 539                 35    missed shot              Driving Layup Shot
    ## 540                 27    missed shot              Driving Layup Shot
    ## 541                 34      made shot              Driving Layup Shot
    ## 542                 51    missed shot              Driving Layup Shot
    ## 543                 45      made shot              Driving Layup Shot
    ## 544                 51      made shot              Driving Layup Shot
    ## 545                 26      made shot              Driving Layup Shot
    ## 546                  8      made shot              Driving Layup Shot
    ## 547                  8      made shot              Driving Layup Shot
    ## 548                 54    missed shot              Driving Layup Shot
    ## 549                 31      made shot                       Dunk Shot
    ## 550                  7      made shot                       Dunk Shot
    ## 551                 23      made shot                       Dunk Shot
    ## 552                  7      made shot                       Dunk Shot
    ## 553                  1      made shot                       Dunk Shot
    ## 554                 31      made shot                       Dunk Shot
    ## 555                 57      made shot                       Dunk Shot
    ## 556                 16      made shot                       Dunk Shot
    ## 557                 22      made shot          Finger Roll Layup Shot
    ## 558                 29      made shot          Finger Roll Layup Shot
    ## 559                 54    missed shot              Floating Jump shot
    ## 560                 33    missed shot              Floating Jump shot
    ## 561                  7    missed shot                  Jump Bank Shot
    ## 562                 41      made shot                  Jump Bank Shot
    ## 563                 33    missed shot                       Jump Shot
    ## 564                 31    missed shot                       Jump Shot
    ## 565                 19    missed shot                       Jump Shot
    ## 566                 29    missed shot                       Jump Shot
    ## 567                 29    missed shot                       Jump Shot
    ## 568                 37    missed shot                       Jump Shot
    ## 569                  4    missed shot                       Jump Shot
    ## 570                 27    missed shot                       Jump Shot
    ## 571                 19    missed shot                       Jump Shot
    ## 572                  3    missed shot                       Jump Shot
    ## 573                 37    missed shot                       Jump Shot
    ## 574                 28    missed shot                       Jump Shot
    ## 575                 52    missed shot                       Jump Shot
    ## 576                 26    missed shot                       Jump Shot
    ## 577                  6    missed shot                      Layup Shot
    ## 578                 33      made shot                      Layup Shot
    ## 579                 32    missed shot                      Layup Shot
    ## 580                  4    missed shot                      Layup Shot
    ## 581                 49      made shot                      Layup Shot
    ## 582                 45      made shot                      Layup Shot
    ## 583                  2      made shot                      Layup Shot
    ## 584                 17    missed shot                      Layup Shot
    ## 585                 44    missed shot                      Layup Shot
    ## 586                 47      made shot                      Layup Shot
    ## 587                 43    missed shot                      Layup Shot
    ## 588                  4      made shot                      Layup Shot
    ## 589                 38    missed shot                      Layup Shot
    ## 590                  2      made shot                      Layup Shot
    ## 591                 35      made shot                      Layup Shot
    ## 592                 48    missed shot                      Layup Shot
    ## 593                 36    missed shot                      Layup Shot
    ## 594                 42    missed shot                      Layup Shot
    ## 595                  2      made shot                      Layup Shot
    ## 596                 32    missed shot                      Layup Shot
    ## 597                  4    missed shot                      Layup Shot
    ## 598                 31      made shot                      Layup Shot
    ## 599                 32    missed shot                      Layup Shot
    ## 600                 48    missed shot                      Layup Shot
    ## 601                  2    missed shot                      Layup Shot
    ## 602                 55    missed shot                      Layup Shot
    ## 603                 43    missed shot                      Layup Shot
    ## 604                 23    missed shot                      Layup Shot
    ## 605                 11      made shot                      Layup Shot
    ## 606                  6      made shot                      Layup Shot
    ## 607                 48      made shot                      Layup Shot
    ## 608                 19      made shot                      Layup Shot
    ## 609                 50    missed shot                      Layup Shot
    ## 610                  1    missed shot                      Layup Shot
    ## 611                 48    missed shot                      Layup Shot
    ## 612                 27    missed shot                      Layup Shot
    ## 613                 36      made shot                      Layup Shot
    ## 614                 12    missed shot                      Layup Shot
    ## 615                 27    missed shot                      Layup Shot
    ## 616                 44      made shot                      Layup Shot
    ## 617                 12    missed shot                      Layup Shot
    ## 618                 11    missed shot                      Layup Shot
    ## 619                 29      made shot                      Layup Shot
    ## 620                 35      made shot                      Layup Shot
    ## 621                 41      made shot                      Layup Shot
    ## 622                 53    missed shot                      Layup Shot
    ## 623                  3    missed shot                      Layup Shot
    ## 624                 32    missed shot                      Layup Shot
    ## 625                 16      made shot                      Layup Shot
    ## 626                 33    missed shot                      Layup Shot
    ## 627                  6    missed shot                      Layup Shot
    ## 628                 59    missed shot                      Layup Shot
    ## 629                 40      made shot                      Layup Shot
    ## 630                 41      made shot                      Layup Shot
    ## 631                 45      made shot                      Layup Shot
    ## 632                 34    missed shot                      Layup Shot
    ## 633                 17    missed shot                      Layup Shot
    ## 634                 56    missed shot                      Layup Shot
    ## 635                 46    missed shot                      Layup Shot
    ## 636                  6      made shot               Putback Dunk Shot
    ## 637                 42      made shot               Putback Dunk Shot
    ## 638                 16      made shot              Putback Layup Shot
    ## 639                 18    missed shot              Putback Layup Shot
    ## 640                 35      made shot              Putback Layup Shot
    ## 641                  0      made shot              Putback Layup Shot
    ## 642                 48      made shot              Putback Layup Shot
    ## 643                 21      made shot              Putback Layup Shot
    ## 644                 41      made shot              Putback Layup Shot
    ## 645                 11      made shot              Reverse Layup Shot
    ## 646                 52    missed shot              Reverse Layup Shot
    ## 647                 12      made shot               Running Dunk Shot
    ## 648                 25      made shot               Running Dunk Shot
    ## 649                 19      made shot               Running Dunk Shot
    ## 650                 32      made shot               Running Dunk Shot
    ## 651                 45      made shot               Running Dunk Shot
    ## 652                 42      made shot               Running Dunk Shot
    ## 653                 56      made shot               Running Dunk Shot
    ## 654                 59      made shot               Running Dunk Shot
    ## 655                 29      made shot               Running Dunk Shot
    ## 656                  7      made shot               Running Dunk Shot
    ## 657                 50      made shot               Running Dunk Shot
    ## 658                 27      made shot               Running Dunk Shot
    ## 659                 39      made shot               Running Dunk Shot
    ## 660                 51      made shot               Running Dunk Shot
    ## 661                 15      made shot               Running Dunk Shot
    ## 662                  9      made shot  Running Finger Roll Layup Shot
    ## 663                 31    missed shot  Running Finger Roll Layup Shot
    ## 664                 38    missed shot               Running Jump Shot
    ## 665                  0      made shot              Running Layup Shot
    ## 666                 26      made shot              Running Layup Shot
    ## 667                 11      made shot              Running Layup Shot
    ## 668                 33      made shot              Running Layup Shot
    ## 669                  3    missed shot              Running Layup Shot
    ## 670                  0      made shot              Running Layup Shot
    ## 671                 10      made shot              Running Layup Shot
    ## 672                 17      made shot              Running Layup Shot
    ## 673                 59    missed shot              Running Layup Shot
    ## 674                 22      made shot              Running Layup Shot
    ## 675                 41      made shot              Running Layup Shot
    ## 676                  4      made shot              Running Layup Shot
    ## 677                 55    missed shot              Running Layup Shot
    ## 678                 23    missed shot              Running Layup Shot
    ## 679                  4      made shot              Running Layup Shot
    ## 680                 31      made shot              Running Layup Shot
    ## 681                 57      made shot              Running Layup Shot
    ## 682                  5    missed shot              Running Layup Shot
    ## 683                 32    missed shot                  Tip Layup Shot
    ## 684                 32      made shot                  Tip Layup Shot
    ## 685                 29    missed shot                  Tip Layup Shot
    ## 686                 27      made shot                  Tip Layup Shot
    ## 687                 27    missed shot                  Tip Layup Shot
    ## 688                 18    missed shot                  Tip Layup Shot
    ## 689                 19    missed shot                  Tip Layup Shot
    ## 690                 19    missed shot                  Tip Layup Shot
    ## 691                 46    missed shot                  Tip Layup Shot
    ## 692                 28      made shot                  Tip Layup Shot
    ## 693                 29    missed shot                  Tip Layup Shot
    ## 694                 12    missed shot                  Tip Layup Shot
    ## 695                  9      made shot                  Tip Layup Shot
    ## 696                  8    missed shot                  Tip Layup Shot
    ## 697                  7    missed shot                  Tip Layup Shot
    ## 698                  6    missed shot                  Tip Layup Shot
    ## 699                 51    missed shot                  Tip Layup Shot
    ## 700                 51    missed shot                  Tip Layup Shot
    ## 701                 18      made shot                  Tip Layup Shot
    ## 702                  4    missed shot                  Tip Layup Shot
    ## 703                 20      made shot                  Tip Layup Shot
    ## 704                 32    missed shot                  Tip Layup Shot
    ## 705                 57    missed shot                  Tip Layup Shot
    ## 706                 43    missed shot                  Tip Layup Shot
    ## 707                 16    missed shot                  Tip Layup Shot
    ## 708                 35    missed shot                  Tip Layup Shot
    ## 709                 37      made shot             Alley Oop Dunk Shot
    ## 710                  3      made shot             Alley Oop Dunk Shot
    ## 711                 18      made shot             Alley Oop Dunk Shot
    ## 712                 52      made shot             Alley Oop Dunk Shot
    ## 713                 19      made shot            Alley Oop Layup shot
    ## 714                  1    missed shot            Alley Oop Layup shot
    ## 715                 26      made shot               Cutting Dunk Shot
    ## 716                 16      made shot               Cutting Dunk Shot
    ## 717                 25      made shot               Cutting Dunk Shot
    ## 718                 38      made shot               Cutting Dunk Shot
    ## 719                 46      made shot               Cutting Dunk Shot
    ## 720                  0      made shot               Cutting Dunk Shot
    ## 721                 27      made shot               Cutting Dunk Shot
    ## 722                  8      made shot               Cutting Dunk Shot
    ## 723                 17      made shot               Cutting Dunk Shot
    ## 724                  3      made shot               Cutting Dunk Shot
    ## 725                 46      made shot               Cutting Dunk Shot
    ## 726                 39      made shot               Cutting Dunk Shot
    ## 727                 16      made shot               Cutting Dunk Shot
    ## 728                 12      made shot               Cutting Dunk Shot
    ## 729                 19      made shot               Cutting Dunk Shot
    ## 730                 57      made shot               Cutting Dunk Shot
    ## 731                 41      made shot               Cutting Dunk Shot
    ## 732                 59      made shot               Cutting Dunk Shot
    ## 733                 55      made shot               Cutting Dunk Shot
    ## 734                  0      made shot               Cutting Dunk Shot
    ## 735                 27      made shot               Cutting Dunk Shot
    ## 736                 26      made shot               Cutting Dunk Shot
    ## 737                 56      made shot               Cutting Dunk Shot
    ## 738                 33      made shot               Cutting Dunk Shot
    ## 739                 55      made shot               Cutting Dunk Shot
    ## 740                 37      made shot               Cutting Dunk Shot
    ## 741                  3      made shot               Cutting Dunk Shot
    ## 742                  8      made shot               Cutting Dunk Shot
    ## 743                 31      made shot               Cutting Dunk Shot
    ## 744                 28      made shot  Cutting Finger Roll Layup Shot
    ## 745                  3      made shot  Cutting Finger Roll Layup Shot
    ## 746                 10      made shot  Cutting Finger Roll Layup Shot
    ## 747                 23      made shot  Cutting Finger Roll Layup Shot
    ## 748                 34      made shot  Cutting Finger Roll Layup Shot
    ## 749                  5      made shot              Cutting Layup Shot
    ## 750                 58      made shot              Cutting Layup Shot
    ## 751                 44      made shot              Cutting Layup Shot
    ## 752                 54      made shot              Cutting Layup Shot
    ## 753                 34      made shot              Cutting Layup Shot
    ## 754                 50    missed shot              Cutting Layup Shot
    ## 755                 29      made shot              Cutting Layup Shot
    ## 756                 59      made shot              Cutting Layup Shot
    ## 757                 20      made shot              Cutting Layup Shot
    ## 758                 10      made shot              Cutting Layup Shot
    ## 759                 13      made shot               Driving Bank shot
    ## 760                 40    missed shot               Driving Bank shot
    ## 761                 56      made shot               Driving Bank shot
    ## 762                  9      made shot               Driving Dunk Shot
    ## 763                 23      made shot               Driving Dunk Shot
    ## 764                 52      made shot               Driving Dunk Shot
    ## 765                 21      made shot               Driving Dunk Shot
    ## 766                 25      made shot               Driving Dunk Shot
    ## 767                 41      made shot               Driving Dunk Shot
    ## 768                 37      made shot               Driving Dunk Shot
    ## 769                 28      made shot               Driving Dunk Shot
    ## 770                 52      made shot               Driving Dunk Shot
    ## 771                  8      made shot               Driving Dunk Shot
    ## 772                  8      made shot               Driving Dunk Shot
    ## 773                 23      made shot               Driving Dunk Shot
    ## 774                 35      made shot               Driving Dunk Shot
    ## 775                 41      made shot               Driving Dunk Shot
    ## 776                  4      made shot               Driving Dunk Shot
    ## 777                 42      made shot               Driving Dunk Shot
    ## 778                 51      made shot               Driving Dunk Shot
    ## 779                 47      made shot               Driving Dunk Shot
    ## 780                 10      made shot               Driving Dunk Shot
    ## 781                  6      made shot               Driving Dunk Shot
    ## 782                  6      made shot               Driving Dunk Shot
    ## 783                  5      made shot               Driving Dunk Shot
    ## 784                  1      made shot               Driving Dunk Shot
    ## 785                 29      made shot               Driving Dunk Shot
    ## 786                 44      made shot               Driving Dunk Shot
    ## 787                 46      made shot               Driving Dunk Shot
    ## 788                 43      made shot  Driving Finger Roll Layup Shot
    ## 789                  6      made shot  Driving Finger Roll Layup Shot
    ## 790                 35      made shot  Driving Finger Roll Layup Shot
    ## 791                 42      made shot  Driving Finger Roll Layup Shot
    ## 792                  4      made shot  Driving Finger Roll Layup Shot
    ## 793                 18      made shot  Driving Finger Roll Layup Shot
    ## 794                 22      made shot  Driving Finger Roll Layup Shot
    ## 795                 15      made shot  Driving Finger Roll Layup Shot
    ## 796                 14      made shot  Driving Finger Roll Layup Shot
    ## 797                 32      made shot  Driving Finger Roll Layup Shot
    ## 798                 20    missed shot  Driving Finger Roll Layup Shot
    ## 799                  6      made shot  Driving Finger Roll Layup Shot
    ## 800                 47      made shot Driving Floating Bank Jump Shot
    ## 801                 31    missed shot      Driving Floating Jump Shot
    ## 802                 36    missed shot      Driving Floating Jump Shot
    ## 803                 15      made shot      Driving Floating Jump Shot
    ## 804                 37    missed shot               Driving Hook Shot
    ## 805                 26      made shot               Driving Hook Shot
    ## 806                 26    missed shot               Driving Hook Shot
    ## 807                 29    missed shot              Driving Layup Shot
    ## 808                 48      made shot              Driving Layup Shot
    ## 809                 13    missed shot              Driving Layup Shot
    ## 810                 15      made shot              Driving Layup Shot
    ## 811                 38      made shot              Driving Layup Shot
    ## 812                 35    missed shot              Driving Layup Shot
    ## 813                 34    missed shot              Driving Layup Shot
    ## 814                 13      made shot              Driving Layup Shot
    ## 815                 22      made shot              Driving Layup Shot
    ## 816                 31      made shot              Driving Layup Shot
    ## 817                 48      made shot              Driving Layup Shot
    ## 818                 33      made shot              Driving Layup Shot
    ## 819                 37      made shot              Driving Layup Shot
    ## 820                 40    missed shot              Driving Layup Shot
    ## 821                  8      made shot              Driving Layup Shot
    ## 822                  0      made shot              Driving Layup Shot
    ## 823                 22    missed shot              Driving Layup Shot
    ## 824                 51    missed shot              Driving Layup Shot
    ## 825                 10      made shot              Driving Layup Shot
    ## 826                 40      made shot              Driving Layup Shot
    ## 827                 19      made shot              Driving Layup Shot
    ## 828                 38    missed shot              Driving Layup Shot
    ## 829                  7    missed shot              Driving Layup Shot
    ## 830                 50      made shot              Driving Layup Shot
    ## 831                 19      made shot              Driving Layup Shot
    ## 832                 25      made shot              Driving Layup Shot
    ## 833                 14      made shot              Driving Layup Shot
    ## 834                 41      made shot              Driving Layup Shot
    ## 835                 18      made shot              Driving Layup Shot
    ## 836                 32    missed shot              Driving Layup Shot
    ## 837                 23      made shot              Driving Layup Shot
    ## 838                 24    missed shot              Driving Layup Shot
    ## 839                 21      made shot              Driving Layup Shot
    ## 840                 10      made shot              Driving Layup Shot
    ## 841                  5      made shot       Driving Reverse Dunk Shot
    ## 842                 24      made shot       Driving Reverse Dunk Shot
    ## 843                 54      made shot      Driving Reverse Layup Shot
    ## 844                 25      made shot      Driving Reverse Layup Shot
    ## 845                 15      made shot      Driving Reverse Layup Shot
    ## 846                 33      made shot      Driving Reverse Layup Shot
    ## 847                 39      made shot      Driving Reverse Layup Shot
    ## 848                 29      made shot                       Dunk Shot
    ## 849                 51      made shot                       Dunk Shot
    ## 850                 13      made shot                       Dunk Shot
    ## 851                 45      made shot                       Dunk Shot
    ## 852                 25      made shot                       Dunk Shot
    ## 853                 44      made shot                       Dunk Shot
    ## 854                 47      made shot                       Dunk Shot
    ## 855                 40      made shot                       Dunk Shot
    ## 856                 29      made shot                       Dunk Shot
    ## 857                 53    missed shot                       Dunk Shot
    ## 858                 21      made shot                       Dunk Shot
    ## 859                 33      made shot                       Dunk Shot
    ## 860                 43      made shot                       Dunk Shot
    ## 861                 30      made shot              Fadeaway Jump Shot
    ## 862                 41      made shot              Fadeaway Jump Shot
    ## 863                  5    missed shot          Finger Roll Layup Shot
    ## 864                 25      made shot          Finger Roll Layup Shot
    ## 865                 17      made shot              Floating Jump shot
    ## 866                 18    missed shot              Floating Jump shot
    ## 867                 46      made shot                  Jump Bank Shot
    ## 868                 26      made shot                  Jump Bank Shot
    ## 869                 42    missed shot                       Jump Shot
    ## 870                  7    missed shot                       Jump Shot
    ## 871                  1    missed shot                       Jump Shot
    ## 872                 20      made shot                       Jump Shot
    ## 873                 51    missed shot                       Jump Shot
    ## 874                  5      made shot                      Layup Shot
    ## 875                 17    missed shot                      Layup Shot
    ## 876                 33      made shot                      Layup Shot
    ## 877                 26    missed shot                      Layup Shot
    ## 878                  2      made shot                      Layup Shot
    ## 879                 37      made shot                      Layup Shot
    ## 880                 51    missed shot                      Layup Shot
    ## 881                 42      made shot                      Layup Shot
    ## 882                 56      made shot                      Layup Shot
    ## 883                  4    missed shot                      Layup Shot
    ## 884                 50    missed shot                      Layup Shot
    ## 885                 26      made shot                      Layup Shot
    ## 886                 34    missed shot                      Layup Shot
    ## 887                 26      made shot                      Layup Shot
    ## 888                 40    missed shot                      Layup Shot
    ## 889                 37    missed shot                      Layup Shot
    ## 890                 25    missed shot                      Layup Shot
    ## 891                 46    missed shot                      Layup Shot
    ## 892                 33      made shot                      Layup Shot
    ## 893                 43      made shot                      Layup Shot
    ## 894                 16      made shot                      Layup Shot
    ## 895                 16    missed shot                      Layup Shot
    ## 896                 25    missed shot                      Layup Shot
    ## 897                 17      made shot                      Layup Shot
    ## 898                 22      made shot                      Layup Shot
    ## 899                  2    missed shot                      Layup Shot
    ## 900                 44    missed shot                      Layup Shot
    ## 901                 32    missed shot                      Layup Shot
    ## 902                 33    missed shot                      Layup Shot
    ## 903                 54      made shot                      Layup Shot
    ## 904                 45    missed shot                      Layup Shot
    ## 905                 48    missed shot                      Layup Shot
    ## 906                 39      made shot                      Layup Shot
    ## 907                 42    missed shot                      Layup Shot
    ## 908                 15    missed shot                      Layup Shot
    ## 909                 10      made shot               Putback Dunk Shot
    ## 910                  5      made shot               Putback Dunk Shot
    ## 911                 41      made shot               Putback Dunk Shot
    ## 912                 53      made shot              Putback Layup Shot
    ## 913                 31    missed shot              Putback Layup Shot
    ## 914                  1      made shot               Reverse Dunk Shot
    ## 915                  5    missed shot              Reverse Layup Shot
    ## 916                 57      made shot              Reverse Layup Shot
    ## 917                 11      made shot              Reverse Layup Shot
    ## 918                 41      made shot              Reverse Layup Shot
    ## 919                 51      made shot              Reverse Layup Shot
    ## 920                  7      made shot               Running Dunk Shot
    ## 921                 27      made shot               Running Dunk Shot
    ## 922                 43      made shot               Running Dunk Shot
    ## 923                 45    missed shot               Running Dunk Shot
    ## 924                 39      made shot               Running Dunk Shot
    ## 925                 20      made shot               Running Dunk Shot
    ## 926                 11    missed shot               Running Dunk Shot
    ## 927                 58      made shot               Running Dunk Shot
    ## 928                 11      made shot               Running Dunk Shot
    ## 929                  4      made shot               Running Dunk Shot
    ## 930                 53      made shot               Running Dunk Shot
    ## 931                 45      made shot               Running Dunk Shot
    ## 932                  6      made shot               Running Dunk Shot
    ## 933                 43      made shot               Running Dunk Shot
    ## 934                 30      made shot               Running Dunk Shot
    ## 935                  0      made shot               Running Dunk Shot
    ## 936                  4      made shot               Running Dunk Shot
    ## 937                 25      made shot               Running Dunk Shot
    ## 938                 58      made shot               Running Dunk Shot
    ## 939                 14      made shot               Running Dunk Shot
    ## 940                 52      made shot               Running Dunk Shot
    ## 941                 21      made shot               Running Dunk Shot
    ## 942                 16      made shot  Running Finger Roll Layup Shot
    ## 943                 38      made shot  Running Finger Roll Layup Shot
    ## 944                 30      made shot  Running Finger Roll Layup Shot
    ## 945                 52      made shot  Running Finger Roll Layup Shot
    ## 946                 47      made shot  Running Finger Roll Layup Shot
    ## 947                 41      made shot               Running Jump Shot
    ## 948                 51    missed shot               Running Jump Shot
    ## 949                  6      made shot               Running Jump Shot
    ## 950                 51      made shot               Running Jump Shot
    ## 951                 16      made shot              Running Layup Shot
    ## 952                 38    missed shot              Running Layup Shot
    ## 953                 59      made shot              Running Layup Shot
    ## 954                 24    missed shot              Running Layup Shot
    ## 955                 17      made shot              Running Layup Shot
    ## 956                 24      made shot              Running Layup Shot
    ## 957                 21      made shot              Running Layup Shot
    ## 958                 53      made shot              Running Layup Shot
    ## 959                 46      made shot              Running Layup Shot
    ## 960                  6      made shot              Running Layup Shot
    ## 961                  8    missed shot              Running Layup Shot
    ## 962                 49      made shot              Running Layup Shot
    ## 963                 56      made shot              Running Layup Shot
    ## 964                  5    missed shot              Running Layup Shot
    ## 965                 28      made shot              Running Layup Shot
    ## 966                 39      made shot              Running Layup Shot
    ## 967                 44    missed shot              Running Layup Shot
    ## 968                  3      made shot              Running Layup Shot
    ## 969                 56      made shot              Running Layup Shot
    ## 970                 41      made shot              Running Layup Shot
    ## 971                  9      made shot              Running Layup Shot
    ## 972                 32      made shot              Running Layup Shot
    ## 973                 54      made shot              Running Layup Shot
    ## 974                  8    missed shot              Running Layup Shot
    ## 975                  0      made shot              Running Layup Shot
    ## 976                 45      made shot              Running Layup Shot
    ## 977                 30      made shot              Running Layup Shot
    ## 978                  6    missed shot              Running Layup Shot
    ## 979                 25      made shot              Running Layup Shot
    ## 980                 42      made shot              Running Layup Shot
    ## 981                 57    missed shot      Running Reverse Layup Shot
    ## 982                 12    missed shot      Running Reverse Layup Shot
    ## 983                 17      made shot                   Tip Dunk Shot
    ## 984                 33      made shot                  Tip Layup Shot
    ## 985                 35      made shot                  Tip Layup Shot
    ## 986                 43    missed shot                  Tip Layup Shot
    ## 987                 16    missed shot                  Tip Layup Shot
    ## 988                 34      made shot                  Tip Layup Shot
    ## 989                 56      made shot            Turnaround Bank shot
    ## 990                 28      made shot        Turnaround Fadeaway shot
    ## 991                 28      made shot            Alley Oop Layup shot
    ## 992                 16      made shot            Alley Oop Layup shot
    ## 993                 30      made shot               Cutting Dunk Shot
    ## 994                 56      made shot  Cutting Finger Roll Layup Shot
    ## 995                 39      made shot  Cutting Finger Roll Layup Shot
    ## 996                 17      made shot  Cutting Finger Roll Layup Shot
    ## 997                 46      made shot  Cutting Finger Roll Layup Shot
    ## 998                 55      made shot  Cutting Finger Roll Layup Shot
    ## 999                 31      made shot  Cutting Finger Roll Layup Shot
    ## 1000                47      made shot  Cutting Finger Roll Layup Shot
    ## 1001                17      made shot  Cutting Finger Roll Layup Shot
    ## 1002                44      made shot              Cutting Layup Shot
    ## 1003                 5    missed shot              Cutting Layup Shot
    ## 1004                29      made shot              Cutting Layup Shot
    ## 1005                49      made shot              Cutting Layup Shot
    ## 1006                42    missed shot              Cutting Layup Shot
    ## 1007                51      made shot              Cutting Layup Shot
    ## 1008                 2      made shot              Cutting Layup Shot
    ## 1009                30      made shot              Cutting Layup Shot
    ## 1010                 4      made shot              Cutting Layup Shot
    ## 1011                10      made shot              Cutting Layup Shot
    ## 1012                28      made shot              Cutting Layup Shot
    ## 1013                55      made shot              Cutting Layup Shot
    ## 1014                34      made shot              Cutting Layup Shot
    ## 1015                32      made shot              Cutting Layup Shot
    ## 1016                56      made shot              Cutting Layup Shot
    ## 1017                29    missed shot              Cutting Layup Shot
    ## 1018                43      made shot              Cutting Layup Shot
    ## 1019                14      made shot              Cutting Layup Shot
    ## 1020                52      made shot              Cutting Layup Shot
    ## 1021                47      made shot              Cutting Layup Shot
    ## 1022                 5      made shot              Cutting Layup Shot
    ## 1023                18      made shot              Cutting Layup Shot
    ## 1024                58      made shot              Cutting Layup Shot
    ## 1025                38      made shot              Cutting Layup Shot
    ## 1026                45      made shot              Cutting Layup Shot
    ## 1027                21      made shot              Cutting Layup Shot
    ## 1028                24      made shot              Cutting Layup Shot
    ## 1029                 4      made shot              Cutting Layup Shot
    ## 1030                 0      made shot              Cutting Layup Shot
    ## 1031                28      made shot              Cutting Layup Shot
    ## 1032                41      made shot              Cutting Layup Shot
    ## 1033                21      made shot              Cutting Layup Shot
    ## 1034                45      made shot              Cutting Layup Shot
    ## 1035                25    missed shot              Cutting Layup Shot
    ## 1036                11      made shot              Cutting Layup Shot
    ## 1037                41      made shot              Cutting Layup Shot
    ## 1038                26    missed shot              Cutting Layup Shot
    ## 1039                 5      made shot              Cutting Layup Shot
    ## 1040                42      made shot              Cutting Layup Shot
    ## 1041                40    missed shot              Cutting Layup Shot
    ## 1042                59      made shot              Cutting Layup Shot
    ## 1043                 0    missed shot              Cutting Layup Shot
    ## 1044                49      made shot              Cutting Layup Shot
    ## 1045                20      made shot              Cutting Layup Shot
    ## 1046                48      made shot              Cutting Layup Shot
    ## 1047                38      made shot              Cutting Layup Shot
    ## 1048                54      made shot              Cutting Layup Shot
    ## 1049                25      made shot              Cutting Layup Shot
    ## 1050                48      made shot               Driving Dunk Shot
    ## 1051                23      made shot  Driving Finger Roll Layup Shot
    ## 1052                55      made shot  Driving Finger Roll Layup Shot
    ## 1053                26      made shot  Driving Finger Roll Layup Shot
    ## 1054                 3      made shot  Driving Finger Roll Layup Shot
    ## 1055                54    missed shot  Driving Finger Roll Layup Shot
    ## 1056                14      made shot  Driving Finger Roll Layup Shot
    ## 1057                40      made shot  Driving Finger Roll Layup Shot
    ## 1058                12      made shot  Driving Finger Roll Layup Shot
    ## 1059                46      made shot  Driving Finger Roll Layup Shot
    ## 1060                 1      made shot  Driving Finger Roll Layup Shot
    ## 1061                36      made shot  Driving Finger Roll Layup Shot
    ## 1062                 7      made shot  Driving Finger Roll Layup Shot
    ## 1063                35      made shot  Driving Finger Roll Layup Shot
    ## 1064                42      made shot      Driving Floating Jump Shot
    ## 1065                38      made shot              Driving Layup Shot
    ## 1066                56      made shot              Driving Layup Shot
    ## 1067                18    missed shot              Driving Layup Shot
    ## 1068                35    missed shot              Driving Layup Shot
    ## 1069                13      made shot              Driving Layup Shot
    ## 1070                17      made shot              Driving Layup Shot
    ## 1071                37    missed shot              Driving Layup Shot
    ## 1072                55    missed shot              Driving Layup Shot
    ## 1073                46    missed shot              Driving Layup Shot
    ## 1074                40      made shot              Driving Layup Shot
    ## 1075                58      made shot              Driving Layup Shot
    ## 1076                31      made shot              Driving Layup Shot
    ## 1077                16      made shot              Driving Layup Shot
    ## 1078                36      made shot              Driving Layup Shot
    ## 1079                46      made shot              Driving Layup Shot
    ## 1080                26    missed shot              Driving Layup Shot
    ## 1081                43      made shot              Driving Layup Shot
    ## 1082                39      made shot              Driving Layup Shot
    ## 1083                22    missed shot              Driving Layup Shot
    ## 1084                54    missed shot              Driving Layup Shot
    ## 1085                48      made shot              Driving Layup Shot
    ## 1086                 3    missed shot              Driving Layup Shot
    ## 1087                48      made shot              Driving Layup Shot
    ## 1088                55    missed shot              Driving Layup Shot
    ## 1089                 1      made shot              Driving Layup Shot
    ## 1090                54    missed shot              Driving Layup Shot
    ## 1091                16      made shot              Driving Layup Shot
    ## 1092                25    missed shot              Driving Layup Shot
    ## 1093                 7      made shot              Driving Layup Shot
    ## 1094                30    missed shot              Driving Layup Shot
    ## 1095                57      made shot              Driving Layup Shot
    ## 1096                22      made shot              Driving Layup Shot
    ## 1097                 3    missed shot              Driving Layup Shot
    ## 1098                 4    missed shot      Driving Reverse Layup Shot
    ## 1099                 8    missed shot      Driving Reverse Layup Shot
    ## 1100                15    missed shot      Driving Reverse Layup Shot
    ## 1101                20    missed shot      Driving Reverse Layup Shot
    ## 1102                21      made shot      Driving Reverse Layup Shot
    ## 1103                25    missed shot      Driving Reverse Layup Shot
    ## 1104                20      made shot      Driving Reverse Layup Shot
    ## 1105                 7    missed shot      Driving Reverse Layup Shot
    ## 1106                53    missed shot      Driving Reverse Layup Shot
    ## 1107                 2      made shot      Driving Reverse Layup Shot
    ## 1108                 5      made shot          Finger Roll Layup Shot
    ## 1109                54      made shot          Finger Roll Layup Shot
    ## 1110                 5    missed shot                       Hook Shot
    ## 1111                49    missed shot                       Jump Shot
    ## 1112                32      made shot                       Jump Shot
    ## 1113                41    missed shot                       Jump Shot
    ## 1114                27    missed shot                       Jump Shot
    ## 1115                48    missed shot                       Jump Shot
    ## 1116                 0      made shot                       Jump Shot
    ## 1117                13    missed shot                       Jump Shot
    ## 1118                53    missed shot                       Jump Shot
    ## 1119                16    missed shot                       Jump Shot
    ## 1120                 0    missed shot                       Jump Shot
    ## 1121                57    missed shot                       Jump Shot
    ## 1122                 1    missed shot                       Jump Shot
    ## 1123                 2      made shot                       Jump Shot
    ## 1124                15    missed shot                       Jump Shot
    ## 1125                33    missed shot                       Jump Shot
    ## 1126                26      made shot                       Jump Shot
    ## 1127                37    missed shot                       Jump Shot
    ## 1128                30      made shot                      Layup Shot
    ## 1129                39    missed shot                      Layup Shot
    ## 1130                46    missed shot                      Layup Shot
    ## 1131                42      made shot                      Layup Shot
    ## 1132                13      made shot                      Layup Shot
    ## 1133                 4      made shot                      Layup Shot
    ## 1134                 1    missed shot                      Layup Shot
    ## 1135                19    missed shot                      Layup Shot
    ## 1136                 5    missed shot                      Layup Shot
    ## 1137                23    missed shot                      Layup Shot
    ## 1138                59    missed shot                      Layup Shot
    ## 1139                44      made shot                      Layup Shot
    ## 1140                31    missed shot                      Layup Shot
    ## 1141                21      made shot                      Layup Shot
    ## 1142                33    missed shot                      Layup Shot
    ## 1143                15    missed shot                      Layup Shot
    ## 1144                 8    missed shot                      Layup Shot
    ## 1145                18      made shot                      Layup Shot
    ## 1146                56    missed shot                      Layup Shot
    ## 1147                43    missed shot                      Layup Shot
    ## 1148                31    missed shot                      Layup Shot
    ## 1149                44      made shot                      Layup Shot
    ## 1150                18    missed shot                      Layup Shot
    ## 1151                40    missed shot                      Layup Shot
    ## 1152                59    missed shot                      Layup Shot
    ## 1153                48    missed shot                      Layup Shot
    ## 1154                20      made shot                      Layup Shot
    ## 1155                 4    missed shot                      Layup Shot
    ## 1156                 9    missed shot                      Layup Shot
    ## 1157                 7      made shot                      Layup Shot
    ## 1158                26    missed shot                      Layup Shot
    ## 1159                14    missed shot                      Layup Shot
    ## 1160                55    missed shot                      Layup Shot
    ## 1161                32      made shot                      Layup Shot
    ## 1162                36    missed shot                      Layup Shot
    ## 1163                 7      made shot                      Layup Shot
    ## 1164                38    missed shot                      Layup Shot
    ## 1165                30    missed shot                      Layup Shot
    ## 1166                22      made shot                      Layup Shot
    ## 1167                 7      made shot                      Layup Shot
    ## 1168                51      made shot                      Layup Shot
    ## 1169                26      made shot                      Layup Shot
    ## 1170                43    missed shot                      Layup Shot
    ## 1171                40    missed shot                      Layup Shot
    ## 1172                 2      made shot                Pullup Jump shot
    ## 1173                29      made shot              Putback Layup Shot
    ## 1174                45      made shot              Putback Layup Shot
    ## 1175                57    missed shot              Reverse Layup Shot
    ## 1176                19      made shot              Reverse Layup Shot
    ## 1177                40      made shot              Reverse Layup Shot
    ## 1178                20      made shot              Reverse Layup Shot
    ## 1179                56    missed shot              Reverse Layup Shot
    ## 1180                51      made shot              Reverse Layup Shot
    ## 1181                44    missed shot              Reverse Layup Shot
    ## 1182                59      made shot              Reverse Layup Shot
    ## 1183                31      made shot               Running Dunk Shot
    ## 1184                24      made shot               Running Dunk Shot
    ## 1185                13      made shot               Running Dunk Shot
    ## 1186                38      made shot  Running Finger Roll Layup Shot
    ## 1187                 1    missed shot               Running Jump Shot
    ## 1188                 9      made shot              Running Layup Shot
    ## 1189                 4    missed shot              Running Layup Shot
    ## 1190                15    missed shot              Running Layup Shot
    ## 1191                56      made shot              Running Layup Shot
    ## 1192                 3      made shot              Running Layup Shot
    ## 1193                26      made shot              Running Layup Shot
    ## 1194                40      made shot              Running Layup Shot
    ## 1195                54      made shot              Running Layup Shot
    ## 1196                27    missed shot              Running Layup Shot
    ## 1197                 3      made shot              Running Layup Shot
    ## 1198                44      made shot              Running Layup Shot
    ## 1199                 5      made shot              Running Layup Shot
    ## 1200                52      made shot              Running Layup Shot
    ## 1201                 4      made shot              Running Layup Shot
    ## 1202                14    missed shot              Running Layup Shot
    ## 1203                25      made shot              Running Layup Shot
    ## 1204                 9      made shot              Running Layup Shot
    ## 1205                45    missed shot              Running Layup Shot
    ## 1206                50    missed shot              Running Layup Shot
    ## 1207                40      made shot      Running Reverse Layup Shot
    ## 1208                 0      made shot      Running Reverse Layup Shot
    ## 1209                20    missed shot      Running Reverse Layup Shot
    ## 1210                 0      made shot      Running Reverse Layup Shot
    ## 1211                 4    missed shot                  Tip Layup Shot
    ## 1212                 2    missed shot                  Tip Layup Shot
    ## 1213                16    missed shot                  Tip Layup Shot
    ## 1214                15    missed shot            Turnaround Jump Shot
    ## 1215                10      made shot            Turnaround Jump Shot
    ## 1216                23    missed shot            Turnaround Jump Shot
    ##           shot_type shot_distance               opponent   x   y
    ## 1    2PT Field Goal            33        New York Knicks  25  21
    ## 2    2PT Field Goal            28   New Orleans Pelicans   9  26
    ## 3    2PT Field Goal            22 Portland Trail Blazers -22   2
    ## 4    2PT Field Goal             7        Houston Rockets   2   7
    ## 5    2PT Field Goal            26 Minnesota Timberwolves   1  26
    ## 6    2PT Field Goal             7        Milwaukee Bucks   2   7
    ## 7    2PT Field Goal             2         Indiana Pacers  -1   2
    ## 8    2PT Field Goal            28      San Antonio Spurs  28   0
    ## 9    2PT Field Goal            14     Los Angeles Lakers  13   6
    ## 10   2PT Field Goal            12        Toronto Raptors  -9   8
    ## 11   2PT Field Goal             6        Houston Rockets  -4  -5
    ## 12   2PT Field Goal            16 Minnesota Timberwolves  14   7
    ## 13   2PT Field Goal            25      Memphis Grizzlies -24  -6
    ## 14   2PT Field Goal             9     Philadelphia 76ers  -7  -6
    ## 15   2PT Field Goal            21          Atlanta Hawks  20   7
    ## 16   2PT Field Goal            12       Dallas Mavericks  12  -1
    ## 17   2PT Field Goal            11           Phoenix Suns   0  11
    ## 18   2PT Field Goal            14     Philadelphia 76ers  12   7
    ## 19   2PT Field Goal            22   Los Angeles Clippers  -7  21
    ## 20   2PT Field Goal            28          Orlando Magic  22  18
    ## 21   2PT Field Goal            12      Memphis Grizzlies   4  11
    ## 22   2PT Field Goal            18   New Orleans Pelicans  17   7
    ## 23   2PT Field Goal            11     Los Angeles Lakers   1  11
    ## 24   2PT Field Goal            27   Los Angeles Clippers  22  16
    ## 25   2PT Field Goal            23 Portland Trail Blazers  -1  23
    ## 26   2PT Field Goal            16              Utah Jazz  -1  16
    ## 27   2PT Field Goal             8     Los Angeles Lakers   8  -2
    ## 28   2PT Field Goal            23      Memphis Grizzlies   9  21
    ## 29   2PT Field Goal            25      Memphis Grizzlies  24   7
    ## 30   2PT Field Goal            17   Los Angeles Clippers  -7  16
    ## 31   2PT Field Goal             5    Cleveland Cavaliers   1  -5
    ## 32   2PT Field Goal             7        Milwaukee Bucks   0   7
    ## 33   2PT Field Goal            19     Los Angeles Lakers  10  16
    ## 34   2PT Field Goal            17       Sacramento Kings   6  16
    ## 35   2PT Field Goal            25   Los Angeles Clippers  25   2
    ## 36   2PT Field Goal            22   Los Angeles Clippers   6  21
    ## 37   2PT Field Goal            33 Minnesota Timberwolves  12  31
    ## 38   2PT Field Goal            19       Dallas Mavericks  10  16
    ## 39   2PT Field Goal            27 Portland Trail Blazers  -7  26
    ## 40   2PT Field Goal            26         Boston Celtics -25   7
    ## 41   2PT Field Goal            18         Boston Celtics  -9  16
    ## 42   2PT Field Goal            21           Phoenix Suns  -1  21
    ## 43   2PT Field Goal            10      San Antonio Spurs   9  -5
    ## 44   2PT Field Goal            20         Denver Nuggets  17  11
    ## 45   2PT Field Goal            30          Brooklyn Nets  20  23
    ## 46   2PT Field Goal            31        Toronto Raptors -29  11
    ## 47   2PT Field Goal            34 Minnesota Timberwolves  22  26
    ## 48   2PT Field Goal            20   Los Angeles Clippers -12  16
    ## 49   2PT Field Goal             8      San Antonio Spurs  -2   8
    ## 50   2PT Field Goal            16        Toronto Raptors   0  16
    ## 51   2PT Field Goal            29              Utah Jazz  -9  28
    ## 52   2PT Field Goal            26 Minnesota Timberwolves   0  26
    ## 53   2PT Field Goal            17 Minnesota Timberwolves   6  16
    ## 54   2PT Field Goal             7       Dallas Mavericks   2   7
    ## 55   2PT Field Goal            21              Utah Jazz   2  21
    ## 56   2PT Field Goal            19      San Antonio Spurs -19   2
    ## 57   2PT Field Goal            17        Milwaukee Bucks  15   7
    ## 58   2PT Field Goal            10         Boston Celtics   9  -5
    ## 59   2PT Field Goal            16     Philadelphia 76ers   0  16
    ## 60   2PT Field Goal            21        Houston Rockets  -1  21
    ## 61   2PT Field Goal            39 Minnesota Timberwolves  24  31
    ## 62   2PT Field Goal            26          Orlando Magic  19  18
    ## 63   2PT Field Goal            22              Utah Jazz  22   0
    ## 64   2PT Field Goal             8         Indiana Pacers   0   8
    ## 65   2PT Field Goal            26    Cleveland Cavaliers  -1  26
    ## 66   2PT Field Goal            14        Houston Rockets  12   7
    ## 67   2PT Field Goal            16  Oklahoma City Thunder  -4  16
    ## 68   2PT Field Goal            37     Washington Wizards  30  21
    ## 69   2PT Field Goal            33  Oklahoma City Thunder  25  21
    ## 70   2PT Field Goal            38          Chicago Bulls   4  38
    ## 71   2PT Field Goal            46          Atlanta Hawks  -4  46
    ## 72   2PT Field Goal            41      San Antonio Spurs   2  41
    ## 73   2PT Field Goal            41      Memphis Grizzlies  27  31
    ## 74   2PT Field Goal            26         Denver Nuggets   4  26
    ## 75   2PT Field Goal            29        New York Knicks -29   0
    ## 76   2PT Field Goal             1     Los Angeles Lakers   1   1
    ## 77   2PT Field Goal            11         Denver Nuggets   1  11
    ## 78   2PT Field Goal            12        New York Knicks -12   3
    ## 79   2PT Field Goal            18     Los Angeles Lakers -18   1
    ## 80   2PT Field Goal             3        Houston Rockets  -2   2
    ## 81   2PT Field Goal            19        Toronto Raptors -19   2
    ## 82   2PT Field Goal             9   New Orleans Pelicans   4   8
    ## 83   2PT Field Goal             3        Toronto Raptors  -2   2
    ## 84   2PT Field Goal            10     Washington Wizards  -6   8
    ## 85   2PT Field Goal            22   New Orleans Pelicans -22   3
    ## 86   2PT Field Goal            14   New Orleans Pelicans  12   8
    ## 87   2PT Field Goal            28       Dallas Mavericks   0  28
    ## 88   2PT Field Goal             8          Atlanta Hawks   4   7
    ## 89   2PT Field Goal             3        Detroit Pistons  -1   3
    ## 90   2PT Field Goal            20        Houston Rockets  17  11
    ## 91   2PT Field Goal             2        Detroit Pistons  -2  -1
    ## 92   2PT Field Goal            26  Oklahoma City Thunder  -1  26
    ## 93   2PT Field Goal             9          Chicago Bulls   8   5
    ## 94   2PT Field Goal            18      Memphis Grizzlies   2  18
    ## 95   2PT Field Goal            28  Oklahoma City Thunder -11  26
    ## 96   2PT Field Goal             4      Memphis Grizzlies   4   2
    ## 97   2PT Field Goal            14          Chicago Bulls -14   2
    ## 98   2PT Field Goal            13      Charlotte Hornets  -7  11
    ## 99   2PT Field Goal            28  Oklahoma City Thunder  28   0
    ## 100  2PT Field Goal            29         Denver Nuggets -29  -1
    ## 101  2PT Field Goal            20           Phoenix Suns  19   7
    ## 102  2PT Field Goal            21          Chicago Bulls   0  21
    ## 103  2PT Field Goal            28       Dallas Mavericks -20  19
    ## 104  2PT Field Goal            17     Washington Wizards  15   7
    ## 105  2PT Field Goal             7  Oklahoma City Thunder   0   7
    ## 106  2PT Field Goal            14          Orlando Magic -12   8
    ## 107  2PT Field Goal            16      San Antonio Spurs   9  13
    ## 108  2PT Field Goal            34       Sacramento Kings -20  28
    ## 109  2PT Field Goal            25       Sacramento Kings  24   8
    ## 110  2PT Field Goal            14          Chicago Bulls  14   3
    ## 111  2PT Field Goal             5  Oklahoma City Thunder  -4   3
    ## 112  2PT Field Goal             7      San Antonio Spurs  -1   7
    ## 113  2PT Field Goal            14 Minnesota Timberwolves  12   7
    ## 114  2PT Field Goal            22 Minnesota Timberwolves   6  21
    ## 115  2PT Field Goal            13        Milwaukee Bucks   1  13
    ## 116  2PT Field Goal            19     Philadelphia 76ers  19   2
    ## 117  2PT Field Goal            12        New York Knicks   9   8
    ## 118  2PT Field Goal            21           Phoenix Suns   0  21
    ## 119  2PT Field Goal            18     Philadelphia 76ers  -9  16
    ## 120  2PT Field Goal            15     Philadelphia 76ers  10  11
    ## 121  2PT Field Goal            28       Sacramento Kings  28   2
    ## 122  2PT Field Goal            20 Minnesota Timberwolves   9  18
    ## 123  2PT Field Goal            23          Chicago Bulls   9  21
    ## 124  2PT Field Goal             9          Chicago Bulls   9   3
    ## 125  2PT Field Goal            11   Los Angeles Clippers   9   7
    ## 126  2PT Field Goal            10      San Antonio Spurs  -6   8
    ## 127  2PT Field Goal            12      Charlotte Hornets -12   3
    ## 128  2PT Field Goal            16      Charlotte Hornets -14   7
    ## 129  2PT Field Goal            27      Memphis Grizzlies  14  23
    ## 130  2PT Field Goal            27   Los Angeles Clippers  20  18
    ## 131  2PT Field Goal            26   Los Angeles Clippers  20  16
    ## 132  2PT Field Goal             7        New York Knicks  -6   3
    ## 133  2PT Field Goal             9      San Antonio Spurs   8   5
    ## 134  2PT Field Goal            31          Brooklyn Nets   2  31
    ## 135  2PT Field Goal            16           Phoenix Suns  10  13
    ## 136  2PT Field Goal            13        Milwaukee Bucks -11   7
    ## 137  2PT Field Goal            16        Toronto Raptors -12  11
    ## 138  2PT Field Goal            18  Oklahoma City Thunder  -9  16
    ## 139  2PT Field Goal            13  Oklahoma City Thunder  -6  11
    ## 140  2PT Field Goal            15      Memphis Grizzlies  10  11
    ## 141  2PT Field Goal            26          Chicago Bulls  24  11
    ## 142  2PT Field Goal            19        Houston Rockets  15  11
    ## 143  2PT Field Goal             7      Charlotte Hornets  -7   0
    ## 144  2PT Field Goal            29          Orlando Magic  28   7
    ## 145  2PT Field Goal            19          Atlanta Hawks  19   2
    ## 146  2PT Field Goal            15          Orlando Magic  -7  13
    ## 147  2PT Field Goal            13  Oklahoma City Thunder  -2  13
    ## 148  2PT Field Goal            22           Phoenix Suns  -7  21
    ## 149  2PT Field Goal            18         Denver Nuggets   1  18
    ## 150  2PT Field Goal             9 Portland Trail Blazers   9   2
    ## 151  2PT Field Goal            35        Toronto Raptors -34   8
    ## 152  2PT Field Goal             4   Los Angeles Clippers  -4   2
    ## 153  2PT Field Goal            11     Philadelphia 76ers   9   7
    ## 154  2PT Field Goal            27     Los Angeles Lakers  17  21
    ## 155  2PT Field Goal             4  Oklahoma City Thunder  -2   3
    ## 156  2PT Field Goal             1      San Antonio Spurs   0   1
    ## 157  2PT Field Goal            28       Sacramento Kings -22  18
    ## 158  2PT Field Goal            26    Cleveland Cavaliers -20  16
    ## 159  2PT Field Goal            19 Minnesota Timberwolves -11  16
    ## 160  2PT Field Goal            41  Oklahoma City Thunder   1  41
    ## 161  2PT Field Goal            45         Indiana Pacers -27  36
    ## 162  2PT Field Goal            49        New York Knicks  24  43
    ## 163  2PT Field Goal            36          Atlanta Hawks   4  36
    ## 164  2PT Field Goal            34    Cleveland Cavaliers  22  26
    ## 165  2PT Field Goal            31    Cleveland Cavaliers  17  26
    ## 166  2PT Field Goal            26     Philadelphia 76ers -10  24
    ## 167  2PT Field Goal            30        New York Knicks  28  11
    ## 168  2PT Field Goal            31          Brooklyn Nets   4  31
    ## 169  2PT Field Goal            36          Brooklyn Nets  -6  36
    ## 170  2PT Field Goal            31  Oklahoma City Thunder  17  26
    ## 171  2PT Field Goal            13  Oklahoma City Thunder  -7  11
    ## 172  2PT Field Goal            22   New Orleans Pelicans -20   8
    ## 173  2PT Field Goal            23        Milwaukee Bucks   9  21
    ## 174  2PT Field Goal            21     Washington Wizards -20  -6
    ## 175  2PT Field Goal            25   New Orleans Pelicans -24   7
    ## 176  2PT Field Goal            27  Oklahoma City Thunder  17  21
    ## 177  2PT Field Goal             2        Milwaukee Bucks  -1   2
    ## 178  2PT Field Goal            25     Philadelphia 76ers  17  18
    ## 179  2PT Field Goal            22        Toronto Raptors   6  21
    ## 180  2PT Field Goal            23  Oklahoma City Thunder -19  13
    ## 181  2PT Field Goal            20   New Orleans Pelicans  19   7
    ## 182  2PT Field Goal            17        Houston Rockets -16  -5
    ## 183  2PT Field Goal             3        Houston Rockets   0   3
    ## 184  2PT Field Goal             1     Washington Wizards   0  -1
    ## 185  2PT Field Goal            23     Washington Wizards  17 -15
    ## 186  2PT Field Goal            11   New Orleans Pelicans  -1  11
    ## 187  2PT Field Goal            14   New Orleans Pelicans -14   2
    ## 188  2PT Field Goal            21     Los Angeles Lakers -21   0
    ## 189  2PT Field Goal            12      Charlotte Hornets -12   2
    ## 190  2PT Field Goal            22   New Orleans Pelicans -20   8
    ## 191  2PT Field Goal            20        New York Knicks  19  -5
    ## 192  2PT Field Goal            18     Los Angeles Lakers   9  16
    ## 193  2PT Field Goal            21  Oklahoma City Thunder  -1  21
    ## 194  2PT Field Goal            16      Memphis Grizzlies -14   7
    ## 195  2PT Field Goal            18 Portland Trail Blazers   9  16
    ## 196  2PT Field Goal            14      Memphis Grizzlies -14  -1
    ## 197  2PT Field Goal            23      Charlotte Hornets  -1  23
    ## 198  2PT Field Goal            29        New York Knicks  27  11
    ## 199  2PT Field Goal            25       Dallas Mavericks  10  23
    ## 200  2PT Field Goal            11      San Antonio Spurs  -2 -11
    ## 201  2PT Field Goal            13         Denver Nuggets -12  -6
    ## 202  2PT Field Goal             7        Milwaukee Bucks  -3  -6
    ## 203  2PT Field Goal            17 Portland Trail Blazers  17   2
    ## 204  2PT Field Goal             8      Charlotte Hornets   4   7
    ## 205  2PT Field Goal            10     Los Angeles Lakers -10   3
    ## 206  2PT Field Goal            28 Minnesota Timberwolves   9  26
    ## 207  2PT Field Goal            19       Sacramento Kings  15  11
    ## 208  2PT Field Goal            28      San Antonio Spurs  28  -5
    ## 209  2PT Field Goal             7          Brooklyn Nets  -2   7
    ## 210  2PT Field Goal            19     Washington Wizards -19   2
    ## 211  2PT Field Goal            23        New York Knicks -20 -11
    ## 212  2PT Field Goal            14       Sacramento Kings -11   8
    ## 213  2PT Field Goal             7      San Antonio Spurs  -2   7
    ## 214  2PT Field Goal            16   Los Angeles Clippers   4  16
    ## 215  2PT Field Goal            17   New Orleans Pelicans  15   7
    ## 216  2PT Field Goal            26         Denver Nuggets  25   7
    ## 217  2PT Field Goal            19        New York Knicks  19  -1
    ## 218  2PT Field Goal            16         Boston Celtics   1  16
    ## 219  2PT Field Goal            25       Sacramento Kings -14  21
    ## 220  2PT Field Goal            26        Toronto Raptors -19  18
    ## 221  2PT Field Goal            12          Orlando Magic -12   2
    ## 222  2PT Field Goal            17     Philadelphia 76ers  -6  16
    ## 223  2PT Field Goal            18        Houston Rockets   2  18
    ## 224  2PT Field Goal            28          Brooklyn Nets  22  18
    ## 225  2PT Field Goal            20        New York Knicks  19   7
    ## 226  2PT Field Goal            10              Utah Jazz  -7   7
    ## 227  2PT Field Goal            21        Houston Rockets   0  21
    ## 228  2PT Field Goal            27   New Orleans Pelicans -22  16
    ## 229  2PT Field Goal            25   New Orleans Pelicans  24   7
    ## 230  2PT Field Goal             6  Oklahoma City Thunder   4  -5
    ## 231  2PT Field Goal            18   Los Angeles Clippers   1  18
    ## 232  2PT Field Goal            18     Los Angeles Lakers -16   8
    ## 233  2PT Field Goal            18     Los Angeles Lakers -18   1
    ## 234  2PT Field Goal            23 Portland Trail Blazers -22   7
    ## 235  2PT Field Goal            11     Philadelphia 76ers  -9   7
    ## 236  2PT Field Goal            17         Denver Nuggets -17   2
    ## 237  2PT Field Goal            12 Portland Trail Blazers  12   2
    ## 238  2PT Field Goal            10 Portland Trail Blazers  10   3
    ## 239  2PT Field Goal            18     Philadelphia 76ers  -2  18
    ## 240  2PT Field Goal            16   New Orleans Pelicans -14   7
    ## 241  2PT Field Goal            22      San Antonio Spurs -22   2
    ## 242  2PT Field Goal             9 Portland Trail Blazers   9   2
    ## 243  2PT Field Goal            16  Oklahoma City Thunder   1  16
    ## 244  2PT Field Goal            19   Los Angeles Clippers   6  18
    ## 245  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 246  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 247  2PT Field Goal             1 Minnesota Timberwolves   0   1
    ## 248  2PT Field Goal             2              Utah Jazz  -1   2
    ## 249  2PT Field Goal            18         Indiana Pacers  -9  16
    ## 250  2PT Field Goal             3 Minnesota Timberwolves   0   3
    ## 251  2PT Field Goal            12          Orlando Magic  -4  11
    ## 252  2PT Field Goal             4       Sacramento Kings   4   2
    ## 253  2PT Field Goal             4  Oklahoma City Thunder  -4   2
    ## 254  2PT Field Goal            11        Houston Rockets  -2 -11
    ## 255  2PT Field Goal            14      Charlotte Hornets   9  11
    ## 256  2PT Field Goal             8      Memphis Grizzlies   1   8
    ## 257  2PT Field Goal            26 Portland Trail Blazers  -4  26
    ## 258  2PT Field Goal            44           Phoenix Suns   9  43
    ## 259  2PT Field Goal            31      Memphis Grizzlies  30   7
    ## 260  2PT Field Goal             7      Memphis Grizzlies  -7   2
    ## 261  2PT Field Goal            16        Houston Rockets   4  16
    ## 262  2PT Field Goal            23       Dallas Mavericks  -4  23
    ## 263  2PT Field Goal            11   Los Angeles Clippers  -7   8
    ## 264  2PT Field Goal            28   Los Angeles Clippers  -1  28
    ## 265  2PT Field Goal            26   Los Angeles Clippers  25   7
    ## 266  2PT Field Goal            23   Los Angeles Clippers  10  21
    ## 267  2PT Field Goal            18           Phoenix Suns  -2  18
    ## 268  2PT Field Goal            13   Los Angeles Clippers  -6  11
    ## 269  2PT Field Goal            10   Los Angeles Clippers   6   8
    ## 270  2PT Field Goal            21      Memphis Grizzlies  10  18
    ## 271  2PT Field Goal            17       Dallas Mavericks  15   7
    ## 272  2PT Field Goal             8         Boston Celtics  -7   3
    ## 273  2PT Field Goal             7      Memphis Grizzlies   2   7
    ## 274  2PT Field Goal            23          Brooklyn Nets -22  -6
    ## 275  2PT Field Goal            11      Charlotte Hornets   9   7
    ## 276  2PT Field Goal             2        Milwaukee Bucks   2   0
    ## 277  2PT Field Goal            11     Los Angeles Lakers  -2  11
    ## 278  2PT Field Goal            18        Houston Rockets   9  16
    ## 279  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 280  2PT Field Goal            19     Philadelphia 76ers  10  16
    ## 281  2PT Field Goal            19              Utah Jazz  19   2
    ## 282  2PT Field Goal             4    Cleveland Cavaliers   4   2
    ## 283  2PT Field Goal            36        Toronto Raptors  35  -6
    ## 284  2PT Field Goal            12  Oklahoma City Thunder  10   7
    ## 285  2PT Field Goal            21          Brooklyn Nets  20   7
    ## 286  2PT Field Goal             3        Toronto Raptors  -1   3
    ## 287  2PT Field Goal            21       Sacramento Kings  -2  21
    ## 288  2PT Field Goal            16        Milwaukee Bucks   4  16
    ## 289  2PT Field Goal            20        Milwaukee Bucks -19   7
    ## 290  2PT Field Goal            13        Houston Rockets  12  -6
    ## 291  2PT Field Goal            12         Boston Celtics   9   8
    ## 292  2PT Field Goal            13   Los Angeles Clippers  -7  11
    ## 293  2PT Field Goal             7              Utah Jazz  -2   7
    ## 294  2PT Field Goal            19          Chicago Bulls -17   8
    ## 295  2PT Field Goal             9           Phoenix Suns  -9   3
    ## 296  2PT Field Goal            16  Oklahoma City Thunder  14   7
    ## 297  2PT Field Goal            24      Memphis Grizzlies -11  21
    ## 298  2PT Field Goal            25 Minnesota Timberwolves   9  23
    ## 299  2PT Field Goal             2        New York Knicks  -1   2
    ## 300  2PT Field Goal            11   New Orleans Pelicans   1  11
    ## 301  2PT Field Goal             9        Toronto Raptors   0   9
    ## 302  2PT Field Goal             1       Sacramento Kings   0   1
    ## 303  2PT Field Goal            18  Oklahoma City Thunder -12  13
    ## 304  2PT Field Goal             1      San Antonio Spurs   0   1
    ## 305  2PT Field Goal            11  Oklahoma City Thunder  -1  11
    ## 306  2PT Field Goal             1       Sacramento Kings   0   1
    ## 307  2PT Field Goal             1          Orlando Magic   0   1
    ## 308  2PT Field Goal             1         Denver Nuggets   0   1
    ## 309  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 310  2PT Field Goal             1     Washington Wizards   0   1
    ## 311  2PT Field Goal             1      Charlotte Hornets   0   1
    ## 312  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 313  2PT Field Goal             1         Denver Nuggets   0   1
    ## 314  2PT Field Goal             1        Toronto Raptors   0   1
    ## 315  2PT Field Goal             1        Houston Rockets   0   1
    ## 316  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 317  2PT Field Goal             1        Toronto Raptors   0   1
    ## 318  2PT Field Goal             1         Boston Celtics   0   1
    ## 319  2PT Field Goal             1         Denver Nuggets   0   1
    ## 320  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 321  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 322  2PT Field Goal            13      Memphis Grizzlies  10   8
    ## 323  2PT Field Goal            13    Cleveland Cavaliers -11   7
    ## 324  2PT Field Goal             8    Cleveland Cavaliers  -1   8
    ## 325  2PT Field Goal            15      Memphis Grizzlies  10  11
    ## 326  2PT Field Goal            16         Denver Nuggets -14   7
    ## 327  2PT Field Goal             2          Chicago Bulls  -2   0
    ## 328  2PT Field Goal             1          Chicago Bulls   0   1
    ## 329  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 330  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 331  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 332  2PT Field Goal             1          Atlanta Hawks   0   1
    ## 333  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 334  2PT Field Goal             1          Atlanta Hawks   0   1
    ## 335  2PT Field Goal            24        Toronto Raptors  -6  23
    ## 336  2PT Field Goal            14      San Antonio Spurs   9  11
    ## 337  2PT Field Goal            11        Detroit Pistons  -9   7
    ## 338  2PT Field Goal            27        Houston Rockets -17  21
    ## 339  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 340  2PT Field Goal            14       Dallas Mavericks  14   2
    ## 341  2PT Field Goal             7 Portland Trail Blazers   2   7
    ## 342  2PT Field Goal             1       Sacramento Kings   0   1
    ## 343  2PT Field Goal             1       Sacramento Kings   0   1
    ## 344  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 345  2PT Field Goal             1        Houston Rockets   0   1
    ## 346  2PT Field Goal             1        Houston Rockets   0   1
    ## 347  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 348  2PT Field Goal             1     Washington Wizards   0   1
    ## 349  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 350  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 351  2PT Field Goal             1           Phoenix Suns   0   1
    ## 352  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 353  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 354  2PT Field Goal            16        Toronto Raptors  -1  16
    ## 355  2PT Field Goal             1         Boston Celtics   0   1
    ## 356  2PT Field Goal            33 Portland Trail Blazers -32   7
    ## 357  2PT Field Goal            33        Milwaukee Bucks  17  28
    ## 358  2PT Field Goal            39     Philadelphia 76ers -29  26
    ## 359  2PT Field Goal            33   Los Angeles Clippers  -4  33
    ## 360  2PT Field Goal            43 Minnesota Timberwolves   6  43
    ## 361  2PT Field Goal            32       Sacramento Kings  28  16
    ## 362  2PT Field Goal            11              Utah Jazz   0  11
    ## 363  2PT Field Goal            11 Portland Trail Blazers   0  11
    ## 364  2PT Field Goal            16 Minnesota Timberwolves   9  13
    ## 365  2PT Field Goal            11         Indiana Pacers   9   7
    ## 366  2PT Field Goal            15         Denver Nuggets  15   2
    ## 367  2PT Field Goal            14         Indiana Pacers  12   7
    ## 368  2PT Field Goal            26        Houston Rockets   4  26
    ## 369  2PT Field Goal            11         Denver Nuggets  -9   7
    ## 370  2PT Field Goal            13      San Antonio Spurs -12  -6
    ## 371  2PT Field Goal            12           Phoenix Suns  -9   8
    ## 372  2PT Field Goal            10   New Orleans Pelicans   6   8
    ## 373  2PT Field Goal            27  Oklahoma City Thunder  27   2
    ## 374  2PT Field Goal            23         Boston Celtics -22   8
    ## 375  2PT Field Goal            17          Orlando Magic -11  13
    ## 376  2PT Field Goal            27   Los Angeles Clippers -20  18
    ## 377  2PT Field Goal             9   Los Angeles Clippers   4   8
    ## 378  2PT Field Goal             1              Utah Jazz   0   1
    ## 379  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 380  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 381  2PT Field Goal             1          Orlando Magic   0   1
    ## 382  2PT Field Goal            21         Boston Celtics -20   7
    ## 383  2PT Field Goal            12           Phoenix Suns   9   8
    ## 384  2PT Field Goal            13       Sacramento Kings   0  13
    ## 385  2PT Field Goal             1       Sacramento Kings   0   1
    ## 386  2PT Field Goal            25   Los Angeles Clippers  19  16
    ## 387  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 388  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 389  2PT Field Goal            11     Philadelphia 76ers  -7   8
    ## 390  2PT Field Goal            25     Washington Wizards -19  16
    ## 391  2PT Field Goal             1          Orlando Magic   0   1
    ## 392  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 393  2PT Field Goal             1       Sacramento Kings   0   1
    ## 394  2PT Field Goal             1      Charlotte Hornets   0   1
    ## 395  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 396  2PT Field Goal             1        Houston Rockets   0   1
    ## 397  2PT Field Goal             1        Houston Rockets   0   1
    ## 398  2PT Field Goal             1        Toronto Raptors   0   1
    ## 399  2PT Field Goal             1          Orlando Magic   0   1
    ## 400  2PT Field Goal             1        Toronto Raptors   0   1
    ## 401  2PT Field Goal             1          Orlando Magic   0   1
    ## 402  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 403  2PT Field Goal             3          Atlanta Hawks   2   2
    ## 404  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 405  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 406  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 407  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 408  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 409  2PT Field Goal             1        New York Knicks   0   1
    ## 410  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 411  2PT Field Goal            21 Minnesota Timberwolves   2  21
    ## 412  2PT Field Goal            14        New York Knicks -11   8
    ## 413  2PT Field Goal            31          Chicago Bulls  -4  31
    ## 414  2PT Field Goal            43      Memphis Grizzlies   1  43
    ## 415  2PT Field Goal            19   Los Angeles Clippers -14  13
    ## 416  2PT Field Goal            23    Cleveland Cavaliers -16  16
    ## 417  2PT Field Goal            14    Cleveland Cavaliers  14   2
    ## 418  2PT Field Goal            16 Portland Trail Blazers -12  11
    ## 419  2PT Field Goal            13 Portland Trail Blazers  12  -5
    ## 420  2PT Field Goal             8     Los Angeles Lakers  -7   3
    ## 421  2PT Field Goal            25         Denver Nuggets  10  23
    ## 422  2PT Field Goal             8         Boston Celtics   4   7
    ## 423  2PT Field Goal            18  Oklahoma City Thunder  -4  18
    ## 424  2PT Field Goal             7          Atlanta Hawks   1   7
    ## 425  2PT Field Goal            18       Sacramento Kings   9  16
    ## 426  2PT Field Goal            19 Portland Trail Blazers  14  13
    ## 427  2PT Field Goal            13          Chicago Bulls   6  11
    ## 428  2PT Field Goal            16 Portland Trail Blazers  12  11
    ## 429  2PT Field Goal            29         Indiana Pacers  20  21
    ## 430  2PT Field Goal            24        Houston Rockets  -6  23
    ## 431  2PT Field Goal            19 Portland Trail Blazers  19   0
    ## 432  2PT Field Goal            16       Sacramento Kings   0  16
    ## 433  2PT Field Goal            11     Philadelphia 76ers   0  11
    ## 434  2PT Field Goal             1       Sacramento Kings   0   1
    ## 435  2PT Field Goal            16          Orlando Magic  -1  16
    ## 436  2PT Field Goal            23      Charlotte Hornets  -1  23
    ## 437  2PT Field Goal            19     Philadelphia 76ers -11  16
    ## 438  2PT Field Goal            17              Utah Jazz  17   3
    ## 439  2PT Field Goal            14 Minnesota Timberwolves   4  13
    ## 440  2PT Field Goal            16   Los Angeles Clippers   9  13
    ## 441  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 442  2PT Field Goal            12        Houston Rockets   4  11
    ## 443  2PT Field Goal             2      Charlotte Hornets   1   2
    ## 444  2PT Field Goal            34              Utah Jazz  -7  33
    ## 445  2PT Field Goal            48  Oklahoma City Thunder  -7  47
    ## 446  2PT Field Goal            33  Oklahoma City Thunder  20  26
    ## 447  2PT Field Goal            17        New York Knicks  17   2
    ## 448  2PT Field Goal            11       Dallas Mavericks   9   7
    ## 449  2PT Field Goal            12    Cleveland Cavaliers   4  11
    ## 450  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 451  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 452  2PT Field Goal             1          Orlando Magic   0   1
    ## 453  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 454  2PT Field Goal             1          Orlando Magic   0   1
    ## 455  2PT Field Goal             1       Sacramento Kings   0   1
    ## 456  2PT Field Goal             1              Utah Jazz   0   1
    ## 457  2PT Field Goal             1         Denver Nuggets   0   1
    ## 458  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 459  2PT Field Goal             1         Indiana Pacers   0   1
    ## 460  2PT Field Goal             1        Houston Rockets   0   1
    ## 461  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 462  2PT Field Goal             1        Toronto Raptors   0   1
    ## 463  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 464  2PT Field Goal             1       Dallas Mavericks   0   1
    ## 465  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 466  2PT Field Goal             1        Houston Rockets   0   1
    ## 467  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 468  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 469  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 470  2PT Field Goal             1           Phoenix Suns   0   1
    ## 471  2PT Field Goal             1       Dallas Mavericks   0   1
    ## 472  2PT Field Goal             9    Cleveland Cavaliers  -6   7
    ## 473  2PT Field Goal            17      Charlotte Hornets -17   2
    ## 474  2PT Field Goal             8           Phoenix Suns   1   8
    ## 475  2PT Field Goal            19      San Antonio Spurs -19   3
    ## 476  2PT Field Goal            13        New York Knicks   0  13
    ## 477  2PT Field Goal            15 Portland Trail Blazers -15  -1
    ## 478  2PT Field Goal            22         Denver Nuggets   6  21
    ## 479  2PT Field Goal             9        Toronto Raptors   4   8
    ## 480  2PT Field Goal            20  Oklahoma City Thunder -19  -6
    ## 481  2PT Field Goal            11       Dallas Mavericks   1  11
    ## 482  2PT Field Goal             9      Memphis Grizzlies  -9   2
    ## 483  2PT Field Goal            16      San Antonio Spurs   4  16
    ## 484  2PT Field Goal            21     Los Angeles Lakers -21  -2
    ## 485  2PT Field Goal             9        Toronto Raptors  -6   7
    ## 486  2PT Field Goal            20   Los Angeles Clippers  12  16
    ## 487  2PT Field Goal            21     Los Angeles Lakers   1  21
    ## 488  2PT Field Goal            26  Oklahoma City Thunder -16  21
    ## 489  2PT Field Goal            13         Indiana Pacers  -2  13
    ## 490  2PT Field Goal            16   New Orleans Pelicans   1  16
    ## 491  2PT Field Goal            15        Houston Rockets  -7  13
    ## 492  2PT Field Goal            47        Houston Rockets  -9  46
    ## 493  2PT Field Goal            32       Dallas Mavericks  -9  31
    ## 494  2PT Field Goal             1 Minnesota Timberwolves   0   1
    ## 495  2PT Field Goal             1     Washington Wizards   0   1
    ## 496  2PT Field Goal             1         Denver Nuggets   0   1
    ## 497  2PT Field Goal             1        Houston Rockets   0   1
    ## 498  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 499  2PT Field Goal             4      Memphis Grizzlies   4   2
    ## 500  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 501  2PT Field Goal             1          Chicago Bulls   0   1
    ## 502  2PT Field Goal             1          Orlando Magic   0   1
    ## 503  2PT Field Goal             1        Houston Rockets   0   1
    ## 504  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 505  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 506  2PT Field Goal             1          Atlanta Hawks   0   1
    ## 507  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 508  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 509  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 510  2PT Field Goal             1       Dallas Mavericks   0   1
    ## 511  2PT Field Goal             6      San Antonio Spurs   6  -1
    ## 512  2PT Field Goal            22 Minnesota Timberwolves  -7  21
    ## 513  2PT Field Goal             6     Washington Wizards   6   2
    ## 514  2PT Field Goal            38   New Orleans Pelicans  22  31
    ## 515  2PT Field Goal            26         Denver Nuggets   1  26
    ## 516  2PT Field Goal            30       Sacramento Kings -19  23
    ## 517  2PT Field Goal            22        Houston Rockets   6  21
    ## 518  2PT Field Goal            13    Cleveland Cavaliers   2  13
    ## 519  2PT Field Goal            24        Milwaukee Bucks -12  21
    ## 520  2PT Field Goal             7          Chicago Bulls   2   7
    ## 521  2PT Field Goal            28  Oklahoma City Thunder  28   3
    ## 522  2PT Field Goal            20         Indiana Pacers  12  16
    ## 523  2PT Field Goal            27          Chicago Bulls  22  16
    ## 524  2PT Field Goal             6  Oklahoma City Thunder   1  -6
    ## 525  2PT Field Goal            27         Denver Nuggets  22  16
    ## 526  2PT Field Goal            20          Atlanta Hawks  19   7
    ## 527  2PT Field Goal            19      San Antonio Spurs -16  11
    ## 528  2PT Field Goal            15       Sacramento Kings  10  11
    ## 529  2PT Field Goal            21        Houston Rockets  20   7
    ## 530  2PT Field Goal            27      Memphis Grizzlies -17  21
    ## 531  2PT Field Goal            28 Portland Trail Blazers  25  13
    ## 532  2PT Field Goal            14          Atlanta Hawks   6  13
    ## 533  2PT Field Goal            11      Charlotte Hornets   1  11
    ## 534  2PT Field Goal            28              Utah Jazz  27   7
    ## 535  2PT Field Goal            18         Boston Celtics -14  11
    ## 536  2PT Field Goal            12        Houston Rockets  10   7
    ## 537  2PT Field Goal            27        Toronto Raptors -20  18
    ## 538  2PT Field Goal            19     Philadelphia 76ers  14  13
    ## 539  2PT Field Goal            21     Los Angeles Lakers   0  21
    ## 540  2PT Field Goal            33              Utah Jazz  32   8
    ## 541  2PT Field Goal            10        New York Knicks  10  -1
    ## 542  2PT Field Goal            16           Phoenix Suns   4  16
    ## 543  2PT Field Goal             8     Philadelphia 76ers   2   8
    ## 544  2PT Field Goal            40        Houston Rockets -38  11
    ## 545  2PT Field Goal             3          Orlando Magic   1   3
    ## 546  2PT Field Goal            19        New York Knicks -19   3
    ## 547  2PT Field Goal             5       Dallas Mavericks   4   3
    ## 548  2PT Field Goal            29          Chicago Bulls -27  11
    ## 549  2PT Field Goal             1        Detroit Pistons   0   1
    ## 550  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 551  2PT Field Goal             1       Sacramento Kings   0   1
    ## 552  2PT Field Goal             1        Houston Rockets   0   1
    ## 553  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 554  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 555  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 556  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 557  2PT Field Goal            15     Los Angeles Lakers -15   1
    ## 558  2PT Field Goal             6    Cleveland Cavaliers  -4  -5
    ## 559  2PT Field Goal            35          Atlanta Hawks  33  13
    ## 560  2PT Field Goal            47          Atlanta Hawks  -1  47
    ## 561  2PT Field Goal            36        Houston Rockets   4  36
    ## 562  2PT Field Goal            45      San Antonio Spurs -45   3
    ## 563  2PT Field Goal            40     Philadelphia 76ers -40   2
    ## 564  2PT Field Goal            34       Sacramento Kings  25  23
    ## 565  2PT Field Goal            33       Sacramento Kings  33   2
    ## 566  2PT Field Goal            36        Houston Rockets   1  36
    ## 567  2PT Field Goal            36  Oklahoma City Thunder  -2  36
    ## 568  2PT Field Goal            33        Toronto Raptors -25  21
    ## 569  2PT Field Goal            35 Portland Trail Blazers  17  31
    ## 570  2PT Field Goal            43      Memphis Grizzlies  -4  43
    ## 571  2PT Field Goal            33           Phoenix Suns  10  31
    ## 572  2PT Field Goal            48        Houston Rockets -14  46
    ## 573  2PT Field Goal            36   New Orleans Pelicans   1  36
    ## 574  2PT Field Goal            30  Oklahoma City Thunder -20  23
    ## 575  2PT Field Goal            36   New Orleans Pelicans   1  36
    ## 576  2PT Field Goal            48   New Orleans Pelicans -35  33
    ## 577  2PT Field Goal            29        Houston Rockets  -9  28
    ## 578  2PT Field Goal            17         Denver Nuggets  -6  16
    ## 579  2PT Field Goal            23   Los Angeles Clippers  17  16
    ## 580  2PT Field Goal            26          Brooklyn Nets  25   8
    ## 581  2PT Field Goal            22   New Orleans Pelicans -22   3
    ## 582  2PT Field Goal             5     Los Angeles Lakers   5   0
    ## 583  2PT Field Goal             7        Houston Rockets   0   7
    ## 584  2PT Field Goal            34       Dallas Mavericks -19  28
    ## 585  2PT Field Goal            10     Philadelphia 76ers   6   8
    ## 586  2PT Field Goal            25   New Orleans Pelicans   9  23
    ## 587  2PT Field Goal            22 Minnesota Timberwolves -19  11
    ## 588  2PT Field Goal            12     Washington Wizards  -9   8
    ## 589  2PT Field Goal            29     Washington Wizards  27 -11
    ## 590  2PT Field Goal             8 Minnesota Timberwolves   1   8
    ## 591  2PT Field Goal            16     Los Angeles Lakers -16   1
    ## 592  2PT Field Goal            13     Los Angeles Lakers   6  11
    ## 593  2PT Field Goal            17     Los Angeles Lakers  -6  16
    ## 594  2PT Field Goal            25   New Orleans Pelicans  19  16
    ## 595  2PT Field Goal            11         Denver Nuggets   9   7
    ## 596  2PT Field Goal            25   New Orleans Pelicans  22  11
    ## 597  2PT Field Goal            20   New Orleans Pelicans  17  11
    ## 598  2PT Field Goal            10         Indiana Pacers  10  -1
    ## 599  2PT Field Goal            16        New York Knicks -16  -1
    ## 600  2PT Field Goal             4        Milwaukee Bucks  -4  -1
    ## 601  2PT Field Goal             3          Atlanta Hawks   1   3
    ## 602  2PT Field Goal            14        Milwaukee Bucks  -6  13
    ## 603  2PT Field Goal            22  Oklahoma City Thunder -12  18
    ## 604  2PT Field Goal            14   New Orleans Pelicans -12   8
    ## 605  2PT Field Goal             8   New Orleans Pelicans  -6  -6
    ## 606  2PT Field Goal            21   New Orleans Pelicans  -2  21
    ## 607  2PT Field Goal            16         Boston Celtics -16   3
    ## 608  2PT Field Goal            23 Portland Trail Blazers -16  16
    ## 609  2PT Field Goal            17       Sacramento Kings  -6  16
    ## 610  2PT Field Goal            12 Portland Trail Blazers -12   2
    ## 611  2PT Field Goal            13       Sacramento Kings   1  13
    ## 612  2PT Field Goal            22          Orlando Magic   6  21
    ## 613  2PT Field Goal            21       Sacramento Kings  20   7
    ## 614  2PT Field Goal            23        New York Knicks -20  11
    ## 615  2PT Field Goal            37       Sacramento Kings   9  36
    ## 616  2PT Field Goal             5        Houston Rockets   2  -5
    ## 617  2PT Field Goal            30       Sacramento Kings -27  13
    ## 618  2PT Field Goal            21   Los Angeles Clippers  10  18
    ## 619  2PT Field Goal            32  Oklahoma City Thunder  -7  31
    ## 620  2PT Field Goal             5  Oklahoma City Thunder  -4   3
    ## 621  2PT Field Goal            16 Portland Trail Blazers -14   7
    ## 622  2PT Field Goal            22        Milwaukee Bucks   6  21
    ## 623  2PT Field Goal             7      Charlotte Hornets   0   7
    ## 624  2PT Field Goal            23        Toronto Raptors -22   8
    ## 625  2PT Field Goal            17    Cleveland Cavaliers  -7 -16
    ## 626  2PT Field Goal            31          Orlando Magic   1  31
    ## 627  2PT Field Goal            11      Charlotte Hornets   1  11
    ## 628  2PT Field Goal            29          Orlando Magic   9  28
    ## 629  2PT Field Goal            24     Philadelphia 76ers  20  14
    ## 630  2PT Field Goal            23        Toronto Raptors  22  -6
    ## 631  2PT Field Goal            22  Oklahoma City Thunder -22   0
    ## 632  2PT Field Goal            29              Utah Jazz -29   2
    ## 633  2PT Field Goal            47       Sacramento Kings  30  36
    ## 634  2PT Field Goal            16   New Orleans Pelicans  -4  16
    ## 635  2PT Field Goal            24           Phoenix Suns   6  23
    ## 636  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 637  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 638  2PT Field Goal             7      San Antonio Spurs   2   7
    ## 639  2PT Field Goal            16      Memphis Grizzlies  -1  16
    ## 640  2PT Field Goal            21   Los Angeles Clippers  -1  21
    ## 641  2PT Field Goal             7  Oklahoma City Thunder  -2   7
    ## 642  2PT Field Goal            10        Houston Rockets   9  -5
    ## 643  2PT Field Goal            20           Phoenix Suns  -9  18
    ## 644  2PT Field Goal             8     Philadelphia 76ers   1   8
    ## 645  2PT Field Goal            18     Los Angeles Lakers  18  -2
    ## 646  2PT Field Goal            20         Denver Nuggets -17  11
    ## 647  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 648  2PT Field Goal             1       Sacramento Kings   0   1
    ## 649  2PT Field Goal             1       Sacramento Kings   0   1
    ## 650  2PT Field Goal             1       Dallas Mavericks   0   1
    ## 651  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 652  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 653  2PT Field Goal             1      San Antonio Spurs   0   1
    ## 654  2PT Field Goal             4           Phoenix Suns  -4   2
    ## 655  2PT Field Goal             1     Washington Wizards   0   1
    ## 656  2PT Field Goal             1              Utah Jazz   0   1
    ## 657  2PT Field Goal             1      Charlotte Hornets   0   1
    ## 658  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 659  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 660  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 661  2PT Field Goal             1              Utah Jazz   0   1
    ## 662  2PT Field Goal            12        Houston Rockets   4  11
    ## 663  2PT Field Goal            14              Utah Jazz   4  13
    ## 664  2PT Field Goal            33  Oklahoma City Thunder   0  33
    ## 665  2PT Field Goal            19        Toronto Raptors  15  11
    ## 666  2PT Field Goal             8        Toronto Raptors   4   7
    ## 667  2PT Field Goal            12    Cleveland Cavaliers  -4  11
    ## 668  2PT Field Goal            22     Philadelphia 76ers  -6  21
    ## 669  2PT Field Goal             7          Atlanta Hawks   1   7
    ## 670  2PT Field Goal             3        Houston Rockets   0   3
    ## 671  2PT Field Goal             2        Toronto Raptors  -1   2
    ## 672  2PT Field Goal             8     Los Angeles Lakers  -8   1
    ## 673  2PT Field Goal            26          Brooklyn Nets -16  21
    ## 674  2PT Field Goal            18        Houston Rockets   9  16
    ## 675  2PT Field Goal             9       Sacramento Kings   9   3
    ## 676  2PT Field Goal            23       Sacramento Kings -17  16
    ## 677  2PT Field Goal            13         Denver Nuggets   2  13
    ## 678  2PT Field Goal            30         Denver Nuggets -30   2
    ## 679  2PT Field Goal            19          Atlanta Hawks  19  -1
    ## 680  2PT Field Goal            22      San Antonio Spurs  -6  21
    ## 681  2PT Field Goal            14        Milwaukee Bucks -11   8
    ## 682  2PT Field Goal            28         Indiana Pacers  22  18
    ## 683  2PT Field Goal             9        Houston Rockets   3   9
    ## 684  2PT Field Goal             4        Houston Rockets  -2   3
    ## 685  2PT Field Goal             8          Orlando Magic  -6   5
    ## 686  2PT Field Goal             7        Houston Rockets   6   3
    ## 687  2PT Field Goal            15     Philadelphia 76ers -12   9
    ## 688  2PT Field Goal             9        New York Knicks  -1   9
    ## 689  2PT Field Goal             8        New York Knicks   1   8
    ## 690  2PT Field Goal            18        New York Knicks   0  18
    ## 691  2PT Field Goal            17       Dallas Mavericks -17   3
    ## 692  2PT Field Goal            21         Denver Nuggets  -4  21
    ## 693  2PT Field Goal            11         Boston Celtics  -1  11
    ## 694  2PT Field Goal            15         Indiana Pacers   5  14
    ## 695  2PT Field Goal             7         Indiana Pacers   1   7
    ## 696  2PT Field Goal            11  Oklahoma City Thunder  -1  11
    ## 697  2PT Field Goal            12  Oklahoma City Thunder  -4  11
    ## 698  2PT Field Goal            17      San Antonio Spurs  -8  15
    ## 699  2PT Field Goal            15   New Orleans Pelicans   5  14
    ## 700  2PT Field Goal            14   New Orleans Pelicans   3  14
    ## 701  2PT Field Goal            14      Memphis Grizzlies   4  13
    ## 702  2PT Field Goal            26        Houston Rockets  -2  26
    ## 703  2PT Field Goal            13      Memphis Grizzlies  -7  11
    ## 704  2PT Field Goal            21      Memphis Grizzlies  -1  21
    ## 705  2PT Field Goal            16         Indiana Pacers  -1  16
    ## 706  2PT Field Goal            16   New Orleans Pelicans  -8  14
    ## 707  2PT Field Goal            17      Memphis Grizzlies -10  14
    ## 708  2PT Field Goal             7      San Antonio Spurs   0   7
    ## 709  2PT Field Goal             1        Detroit Pistons   0   1
    ## 710  2PT Field Goal             1         Boston Celtics   0   1
    ## 711  2PT Field Goal             1         Indiana Pacers   0   1
    ## 712  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 713  2PT Field Goal             3           Phoenix Suns  -1   3
    ## 714  2PT Field Goal            18          Chicago Bulls   2  18
    ## 715  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 716  2PT Field Goal             1           Phoenix Suns   0   1
    ## 717  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 718  2PT Field Goal             1        Houston Rockets   0   1
    ## 719  2PT Field Goal             1           Phoenix Suns   0   1
    ## 720  2PT Field Goal             1         Denver Nuggets   0   1
    ## 721  2PT Field Goal             1         Denver Nuggets   0   1
    ## 722  2PT Field Goal             1        Toronto Raptors   0   1
    ## 723  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 724  2PT Field Goal             1        Houston Rockets   0   1
    ## 725  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 726  2PT Field Goal             1       Sacramento Kings   0   1
    ## 727  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 728  2PT Field Goal             1         Denver Nuggets   0   1
    ## 729  2PT Field Goal             1       Sacramento Kings   0   1
    ## 730  2PT Field Goal             1        Toronto Raptors   0   1
    ## 731  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 732  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 733  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 734  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 735  2PT Field Goal             1         Denver Nuggets   0   1
    ## 736  2PT Field Goal             1          Atlanta Hawks   0   1
    ## 737  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 738  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 739  2PT Field Goal             1         Denver Nuggets   0   1
    ## 740  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 741  2PT Field Goal             1      San Antonio Spurs   0   1
    ## 742  2PT Field Goal             1         Indiana Pacers   0   1
    ## 743  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 744  2PT Field Goal            16       Sacramento Kings   0  16
    ## 745  2PT Field Goal            16 Portland Trail Blazers  14   7
    ## 746  2PT Field Goal            21         Denver Nuggets  14  16
    ## 747  2PT Field Goal            11     Los Angeles Lakers  -2  11
    ## 748  2PT Field Goal            34       Sacramento Kings  22  26
    ## 749  2PT Field Goal            10      Charlotte Hornets  -7   7
    ## 750  2PT Field Goal             8 Portland Trail Blazers   2   8
    ## 751  2PT Field Goal            14      Charlotte Hornets  14  -1
    ## 752  2PT Field Goal            17         Indiana Pacers   6  16
    ## 753  2PT Field Goal            21              Utah Jazz  -1  21
    ## 754  2PT Field Goal            18       Dallas Mavericks   1  18
    ## 755  2PT Field Goal            16       Sacramento Kings -14   7
    ## 756  2PT Field Goal             7     Los Angeles Lakers   0   7
    ## 757  2PT Field Goal            18        Houston Rockets  14  11
    ## 758  2PT Field Goal            23         Boston Celtics  22   8
    ## 759  2PT Field Goal            42           Phoenix Suns  -9  41
    ## 760  2PT Field Goal            43 Minnesota Timberwolves -32  28
    ## 761  2PT Field Goal            37     Philadelphia 76ers  27  26
    ## 762  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 763  2PT Field Goal             1         Denver Nuggets   0   1
    ## 764  2PT Field Goal             1         Denver Nuggets   0   1
    ## 765  2PT Field Goal             1        New York Knicks   0   1
    ## 766  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 767  2PT Field Goal             1       Sacramento Kings   0   1
    ## 768  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 769  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 770  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 771  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 772  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 773  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 774  2PT Field Goal             1         Denver Nuggets   0   1
    ## 775  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 776  2PT Field Goal             1              Utah Jazz   0   1
    ## 777  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 778  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 779  2PT Field Goal             1      San Antonio Spurs   0   1
    ## 780  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 781  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 782  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 783  2PT Field Goal             6      Charlotte Hornets  -6   2
    ## 784  2PT Field Goal             7  Oklahoma City Thunder   0   7
    ## 785  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 786  2PT Field Goal             1      Memphis Grizzlies   0   1
    ## 787  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 788  2PT Field Goal            13          Chicago Bulls   6  11
    ## 789  2PT Field Goal            11   New Orleans Pelicans   9   7
    ## 790  2PT Field Goal            21       Dallas Mavericks   2  21
    ## 791  2PT Field Goal            13         Indiana Pacers -11   7
    ## 792  2PT Field Goal            18      Memphis Grizzlies  14  11
    ## 793  2PT Field Goal            15        Houston Rockets  10  11
    ## 794  2PT Field Goal            20 Portland Trail Blazers  17  11
    ## 795  2PT Field Goal             8        Houston Rockets   0   8
    ## 796  2PT Field Goal            20 Minnesota Timberwolves  17  11
    ## 797  2PT Field Goal            23          Brooklyn Nets -22  -6
    ## 798  2PT Field Goal            19        New York Knicks   6  18
    ## 799  2PT Field Goal            28     Los Angeles Lakers   9  26
    ## 800  2PT Field Goal            40     Philadelphia 76ers -40  -5
    ## 801  2PT Field Goal            36  Oklahoma City Thunder  -6  36
    ## 802  2PT Field Goal            41     Philadelphia 76ers  -1  41
    ## 803  2PT Field Goal            41     Los Angeles Lakers   2  41
    ## 804  2PT Field Goal            23   Los Angeles Clippers  20  11
    ## 805  2PT Field Goal            26    Cleveland Cavaliers  -1  26
    ## 806  2PT Field Goal            33   Los Angeles Clippers  24  23
    ## 807  2PT Field Goal            31           Phoenix Suns   0  31
    ## 808  2PT Field Goal            14   Los Angeles Clippers   6  13
    ## 809  2PT Field Goal            16         Denver Nuggets -11  11
    ## 810  2PT Field Goal            12  Oklahoma City Thunder -12   2
    ## 811  2PT Field Goal            13        Houston Rockets   6  11
    ## 812  2PT Field Goal            26           Phoenix Suns -20  16
    ## 813  2PT Field Goal            38       Sacramento Kings  22  31
    ## 814  2PT Field Goal            25   Los Angeles Clippers  14  21
    ## 815  2PT Field Goal            14      Charlotte Hornets -14   2
    ## 816  2PT Field Goal            16     Los Angeles Lakers -11 -11
    ## 817  2PT Field Goal            30   New Orleans Pelicans  25  16
    ## 818  2PT Field Goal            21  Oklahoma City Thunder  20  -6
    ## 819  2PT Field Goal            23   New Orleans Pelicans  14  18
    ## 820  2PT Field Goal            23   New Orleans Pelicans  -9  21
    ## 821  2PT Field Goal            16         Denver Nuggets  10  13
    ## 822  2PT Field Goal            11        Houston Rockets   2  11
    ## 823  2PT Field Goal            27          Orlando Magic  27  -5
    ## 824  2PT Field Goal            21     Los Angeles Lakers   3  21
    ## 825  2PT Field Goal            21          Orlando Magic  19   8
    ## 826  2PT Field Goal             3        Detroit Pistons  -1   3
    ## 827  2PT Field Goal            20       Sacramento Kings   9  18
    ## 828  2PT Field Goal            18       Sacramento Kings  15  10
    ## 829  2PT Field Goal            23          Brooklyn Nets -22  -6
    ## 830  2PT Field Goal            21 Portland Trail Blazers  14  16
    ## 831  2PT Field Goal            31        Toronto Raptors -16  26
    ## 832  2PT Field Goal            13         Indiana Pacers   0  13
    ## 833  2PT Field Goal             9   Los Angeles Clippers  -9  -1
    ## 834  2PT Field Goal            17      San Antonio Spurs   8  15
    ## 835  2PT Field Goal            13 Minnesota Timberwolves  -6 -11
    ## 836  2PT Field Goal            21 Portland Trail Blazers  10  18
    ## 837  2PT Field Goal            28          Chicago Bulls   0  28
    ## 838  2PT Field Goal             6 Minnesota Timberwolves   6  -1
    ## 839  2PT Field Goal            20       Dallas Mavericks   9  18
    ## 840  2PT Field Goal             4        Detroit Pistons   2   3
    ## 841  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 842  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 843  2PT Field Goal            14 Portland Trail Blazers  -9  11
    ## 844  2PT Field Goal            11      Charlotte Hornets -11   3
    ## 845  2PT Field Goal            14        Houston Rockets  12   7
    ## 846  2PT Field Goal            15  Oklahoma City Thunder  10  11
    ## 847  2PT Field Goal            16        New York Knicks -16   2
    ## 848  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 849  2PT Field Goal             1         Denver Nuggets   0   1
    ## 850  2PT Field Goal             1        Houston Rockets   0   1
    ## 851  2PT Field Goal             1          Brooklyn Nets   0   1
    ## 852  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 853  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 854  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 855  2PT Field Goal             1        New York Knicks   0   1
    ## 856  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 857  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 858  2PT Field Goal             1   New Orleans Pelicans   0   1
    ## 859  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 860  2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 861  2PT Field Goal            48 Minnesota Timberwolves -37  31
    ## 862  2PT Field Goal            41 Minnesota Timberwolves   2  41
    ## 863  2PT Field Goal             8        Detroit Pistons   4   7
    ## 864  2PT Field Goal            23           Phoenix Suns   2  23
    ## 865  2PT Field Goal            31 Minnesota Timberwolves -16  26
    ## 866  2PT Field Goal            46 Portland Trail Blazers  -1  46
    ## 867  2PT Field Goal            45       Sacramento Kings  43  13
    ## 868  2PT Field Goal            29 Portland Trail Blazers -24  16
    ## 869  2PT Field Goal            38      Charlotte Hornets -22  31
    ## 870  2PT Field Goal            49           Phoenix Suns -17  46
    ## 871  2PT Field Goal            36      Charlotte Hornets   1  36
    ## 872  2PT Field Goal            45              Utah Jazz -37  26
    ## 873  2PT Field Goal            33         Denver Nuggets  25  21
    ## 874  2PT Field Goal             7    Cleveland Cavaliers  -7  -1
    ## 875  2PT Field Goal            20   New Orleans Pelicans  20   0
    ## 876  2PT Field Goal            31   Los Angeles Clippers  17  26
    ## 877  2PT Field Goal            22   New Orleans Pelicans -22   2
    ## 878  2PT Field Goal            20   New Orleans Pelicans -20   3
    ## 879  2PT Field Goal            17   New Orleans Pelicans  15   8
    ## 880  2PT Field Goal            16   Los Angeles Clippers  -4  16
    ## 881  2PT Field Goal             7     Los Angeles Lakers   0   7
    ## 882  2PT Field Goal            12     Los Angeles Lakers  -5 -11
    ## 883  2PT Field Goal            19         Denver Nuggets  15  11
    ## 884  2PT Field Goal             9   Los Angeles Clippers   9   2
    ## 885  2PT Field Goal            13        Toronto Raptors   1  13
    ## 886  2PT Field Goal            22       Sacramento Kings   5  21
    ## 887  2PT Field Goal            12              Utah Jazz  10   7
    ## 888  2PT Field Goal            27     Washington Wizards  25 -10
    ## 889  2PT Field Goal             2        New York Knicks   2   0
    ## 890  2PT Field Goal            18              Utah Jazz   9  16
    ## 891  2PT Field Goal            19      San Antonio Spurs  15  11
    ## 892  2PT Field Goal             7 Minnesota Timberwolves   6   3
    ## 893  2PT Field Goal            18         Indiana Pacers  -9  16
    ## 894  2PT Field Goal             7     Los Angeles Lakers  -7  -2
    ## 895  2PT Field Goal            16           Phoenix Suns   0  16
    ## 896  2PT Field Goal            11          Brooklyn Nets  11  -1
    ## 897  2PT Field Goal            11          Brooklyn Nets  -9   7
    ## 898  2PT Field Goal            21        Houston Rockets   0  21
    ## 899  2PT Field Goal             6           Phoenix Suns   6  -1
    ## 900  2PT Field Goal            21        Houston Rockets  -1  21
    ## 901  2PT Field Goal             2      Memphis Grizzlies  -1   2
    ## 902  2PT Field Goal             8      Charlotte Hornets   2   8
    ## 903  2PT Field Goal            27              Utah Jazz  17  21
    ## 904  2PT Field Goal             9        Milwaukee Bucks  -9   3
    ## 905  2PT Field Goal            10        Milwaukee Bucks -10  -1
    ## 906  2PT Field Goal             2        Detroit Pistons   1   2
    ## 907  2PT Field Goal            26    Cleveland Cavaliers  20  16
    ## 908  2PT Field Goal            26   New Orleans Pelicans -25   7
    ## 909  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 910  2PT Field Goal             1 Portland Trail Blazers   0   1
    ## 911  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 912  2PT Field Goal             8   Los Angeles Clippers   2   8
    ## 913  2PT Field Goal            24        Toronto Raptors  22  10
    ## 914  2PT Field Goal             1              Utah Jazz   0   1
    ## 915  2PT Field Goal            25   New Orleans Pelicans -25   3
    ## 916  2PT Field Goal            18   Los Angeles Clippers -14  11
    ## 917  2PT Field Goal            15 Portland Trail Blazers  14  -5
    ## 918  2PT Field Goal            23      San Antonio Spurs   2  23
    ## 919  2PT Field Goal             7        Milwaukee Bucks  -7   2
    ## 920  2PT Field Goal             1           Phoenix Suns   0   1
    ## 921  2PT Field Goal            14    Cleveland Cavaliers  -6  13
    ## 922  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 923  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 924  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 925  2PT Field Goal             1       Sacramento Kings   0   1
    ## 926  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 927  2PT Field Goal             1        Milwaukee Bucks   0   1
    ## 928  2PT Field Goal             1           Phoenix Suns   0   1
    ## 929  2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 930  2PT Field Goal             1              Utah Jazz   0   1
    ## 931  2PT Field Goal             1         Indiana Pacers   0   1
    ## 932  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 933  2PT Field Goal             1              Utah Jazz   0   1
    ## 934  2PT Field Goal             1        Toronto Raptors   0   1
    ## 935  2PT Field Goal             1     Philadelphia 76ers   0   1
    ## 936  2PT Field Goal             1          Chicago Bulls   0   1
    ## 937  2PT Field Goal             1          Chicago Bulls   0   1
    ## 938  2PT Field Goal             1       Sacramento Kings   0   1
    ## 939  2PT Field Goal             1   Los Angeles Clippers   0   1
    ## 940  2PT Field Goal             1       Sacramento Kings   0   1
    ## 941  2PT Field Goal             1      Charlotte Hornets   0   1
    ## 942  2PT Field Goal            18           Phoenix Suns  12  13
    ## 943  2PT Field Goal            12      Charlotte Hornets   4  11
    ## 944  2PT Field Goal            11          Atlanta Hawks  -1  11
    ## 945  2PT Field Goal            19          Chicago Bulls -16  11
    ## 946  2PT Field Goal            32          Brooklyn Nets  30  11
    ## 947  2PT Field Goal            43 Portland Trail Blazers  42  11
    ## 948  2PT Field Goal            44     Philadelphia 76ers -11  43
    ## 949  2PT Field Goal            34           Phoenix Suns   9  33
    ## 950  2PT Field Goal            46      Charlotte Hornets  20  41
    ## 951  2PT Field Goal            16        Toronto Raptors  -1  16
    ## 952  2PT Field Goal            22          Orlando Magic  22  -1
    ## 953  2PT Field Goal            29          Orlando Magic  27  11
    ## 954  2PT Field Goal            16        New York Knicks  -9  13
    ## 955  2PT Field Goal            14    Cleveland Cavaliers   9 -11
    ## 956  2PT Field Goal             3 Minnesota Timberwolves  -2   2
    ## 957  2PT Field Goal             1        Toronto Raptors  -1   0
    ## 958  2PT Field Goal             7     Los Angeles Lakers  -5  -5
    ## 959  2PT Field Goal            29       Sacramento Kings  27  11
    ## 960  2PT Field Goal             7 Portland Trail Blazers  -1   7
    ## 961  2PT Field Goal            14              Utah Jazz  14   3
    ## 962  2PT Field Goal            25          Brooklyn Nets  24   7
    ## 963  2PT Field Goal            14          Brooklyn Nets -14  -1
    ## 964  2PT Field Goal            17          Brooklyn Nets  17   2
    ## 965  2PT Field Goal             1      Memphis Grizzlies   0  -1
    ## 966  2PT Field Goal            11          Chicago Bulls  -1  11
    ## 967  2PT Field Goal            11          Brooklyn Nets  -1  11
    ## 968  2PT Field Goal            25 Portland Trail Blazers -19  16
    ## 969  2PT Field Goal             4          Orlando Magic   4   2
    ## 970  2PT Field Goal             1 Portland Trail Blazers  -1   0
    ## 971  2PT Field Goal            17 Portland Trail Blazers  17   3
    ## 972  2PT Field Goal             9      Charlotte Hornets   9   2
    ## 973  2PT Field Goal             6  Oklahoma City Thunder   6  -1
    ## 974  2PT Field Goal            27      Memphis Grizzlies  17  21
    ## 975  2PT Field Goal            21         Denver Nuggets  14  16
    ## 976  2PT Field Goal            17  Oklahoma City Thunder  17   2
    ## 977  2PT Field Goal            23 Portland Trail Blazers  10  21
    ## 978  2PT Field Goal            18          Orlando Magic  14  11
    ## 979  2PT Field Goal            12   New Orleans Pelicans   4  11
    ## 980  2PT Field Goal            24          Orlando Magic  24  -1
    ## 981  2PT Field Goal             6        Toronto Raptors  -6  -1
    ## 982  2PT Field Goal            16         Indiana Pacers  14   7
    ## 983  2PT Field Goal             1    Cleveland Cavaliers   0   1
    ## 984  2PT Field Goal            13         Denver Nuggets   6  11
    ## 985  2PT Field Goal            12  Oklahoma City Thunder -12  -1
    ## 986  2PT Field Goal            11      Charlotte Hornets   5  10
    ## 987  2PT Field Goal            17 Portland Trail Blazers  15   7
    ## 988  2PT Field Goal            14         Indiana Pacers   6  13
    ## 989  2PT Field Goal            41      Memphis Grizzlies -30  28
    ## 990  2PT Field Goal            47         Denver Nuggets  46   7
    ## 991  2PT Field Goal            26 Portland Trail Blazers -25   7
    ## 992  2PT Field Goal            11   Los Angeles Clippers   2  11
    ## 993  2PT Field Goal             1          Brooklyn Nets   0   1
    ## 994  2PT Field Goal             1       Dallas Mavericks   0  -1
    ## 995  2PT Field Goal            16         Indiana Pacers   2  16
    ## 996  2PT Field Goal             9       Dallas Mavericks  -6   7
    ## 997  2PT Field Goal            26   New Orleans Pelicans   4  26
    ## 998  2PT Field Goal            26         Indiana Pacers   4  26
    ## 999  2PT Field Goal            16     Los Angeles Lakers   1  16
    ## 1000 2PT Field Goal            26           Phoenix Suns  15  21
    ## 1001 2PT Field Goal            11   New Orleans Pelicans  -1  11
    ## 1002 2PT Field Goal             1        Houston Rockets   1  -1
    ## 1003 2PT Field Goal            20      Memphis Grizzlies -17  11
    ## 1004 2PT Field Goal            22       Sacramento Kings  19  11
    ## 1005 2PT Field Goal             3        Toronto Raptors   2   2
    ## 1006 2PT Field Goal             1         Indiana Pacers   1   0
    ## 1007 2PT Field Goal            16           Phoenix Suns  14   8
    ## 1008 2PT Field Goal            33      Memphis Grizzlies  25  21
    ## 1009 2PT Field Goal            10      Memphis Grizzlies  10   2
    ## 1010 2PT Field Goal            16   New Orleans Pelicans   9  13
    ## 1011 2PT Field Goal             6      Charlotte Hornets   6   2
    ## 1012 2PT Field Goal            19          Orlando Magic -11  16
    ## 1013 2PT Field Goal             9  Oklahoma City Thunder   9   2
    ## 1014 2PT Field Goal            21         Indiana Pacers   4  21
    ## 1015 2PT Field Goal            11         Indiana Pacers   0  11
    ## 1016 2PT Field Goal             7        New York Knicks  -2   7
    ## 1017 2PT Field Goal            16      Memphis Grizzlies   9  13
    ## 1018 2PT Field Goal            34        Houston Rockets  22  26
    ## 1019 2PT Field Goal            12    Cleveland Cavaliers   4  11
    ## 1020 2PT Field Goal            12          Chicago Bulls  10   7
    ## 1021 2PT Field Goal            21          Chicago Bulls -20   7
    ## 1022 2PT Field Goal            16         Denver Nuggets   1  16
    ## 1023 2PT Field Goal            19        Houston Rockets  15 -11
    ## 1024 2PT Field Goal            18        Milwaukee Bucks  12  13
    ## 1025 2PT Field Goal            19       Sacramento Kings  19   3
    ## 1026 2PT Field Goal            28          Brooklyn Nets  27   8
    ## 1027 2PT Field Goal             9   New Orleans Pelicans  -4   8
    ## 1028 2PT Field Goal            17       Sacramento Kings -17  -1
    ## 1029 2PT Field Goal            12           Phoenix Suns   4  11
    ## 1030 2PT Field Goal            16    Cleveland Cavaliers -12 -11
    ## 1031 2PT Field Goal            12  Oklahoma City Thunder  10   7
    ## 1032 2PT Field Goal            19          Brooklyn Nets   3  19
    ## 1033 2PT Field Goal            24 Portland Trail Blazers -24  -1
    ## 1034 2PT Field Goal            19 Minnesota Timberwolves  15  11
    ## 1035 2PT Field Goal            13        Houston Rockets -11  -6
    ## 1036 2PT Field Goal            33         Indiana Pacers  25  21
    ## 1037 2PT Field Goal            22      Memphis Grizzlies  15  16
    ## 1038 2PT Field Goal            29         Indiana Pacers   9  28
    ## 1039 2PT Field Goal            27       Sacramento Kings -22  16
    ## 1040 2PT Field Goal            22       Sacramento Kings -19  11
    ## 1041 2PT Field Goal            16   Los Angeles Clippers   2  16
    ## 1042 2PT Field Goal            16      Charlotte Hornets -16   2
    ## 1043 2PT Field Goal            37        Houston Rockets -20  31
    ## 1044 2PT Field Goal            20           Phoenix Suns  17  11
    ## 1045 2PT Field Goal            23   New Orleans Pelicans  22   7
    ## 1046 2PT Field Goal            13 Minnesota Timberwolves   0  13
    ## 1047 2PT Field Goal            14          Atlanta Hawks  12   8
    ## 1048 2PT Field Goal            24     Philadelphia 76ers  12 -21
    ## 1049 2PT Field Goal            27   New Orleans Pelicans  22  16
    ## 1050 2PT Field Goal             1       Sacramento Kings   0   1
    ## 1051 2PT Field Goal            20         Indiana Pacers  19   7
    ## 1052 2PT Field Goal            15        New York Knicks  -7  13
    ## 1053 2PT Field Goal             8           Phoenix Suns  -1   8
    ## 1054 2PT Field Goal            31           Phoenix Suns  -2  31
    ## 1055 2PT Field Goal            19      Charlotte Hornets   6  18
    ## 1056 2PT Field Goal            20         Boston Celtics  20   3
    ## 1057 2PT Field Goal             9     Philadelphia 76ers   9   2
    ## 1058 2PT Field Goal            29      Memphis Grizzlies  20  21
    ## 1059 2PT Field Goal            23   New Orleans Pelicans   9  21
    ## 1060 2PT Field Goal            12           Phoenix Suns  -4  11
    ## 1061 2PT Field Goal            16      Memphis Grizzlies  12  11
    ## 1062 2PT Field Goal            22         Indiana Pacers   6  21
    ## 1063 2PT Field Goal            23        Milwaukee Bucks  20  11
    ## 1064 2PT Field Goal            48           Phoenix Suns  -9  47
    ## 1065 2PT Field Goal             6     Los Angeles Lakers   0  -6
    ## 1066 2PT Field Goal            16   Los Angeles Clippers -14   7
    ## 1067 2PT Field Goal            23          Chicago Bulls -22   7
    ## 1068 2PT Field Goal            20          Chicago Bulls  20   3
    ## 1069 2PT Field Goal            21       Sacramento Kings  14  16
    ## 1070 2PT Field Goal            11 Portland Trail Blazers   0  11
    ## 1071 2PT Field Goal            18 Portland Trail Blazers   1  18
    ## 1072 2PT Field Goal            13          Chicago Bulls   6  11
    ## 1073 2PT Field Goal             7    Cleveland Cavaliers   1   7
    ## 1074 2PT Field Goal            16          Brooklyn Nets   9  13
    ## 1075 2PT Field Goal            11   Los Angeles Clippers  -9   7
    ## 1076 2PT Field Goal            33        Houston Rockets  32   7
    ## 1077 2PT Field Goal            25        Toronto Raptors -19  16
    ## 1078 2PT Field Goal            25         Denver Nuggets  25  -1
    ## 1079 2PT Field Goal            14      San Antonio Spurs   9  11
    ## 1080 2PT Field Goal            23         Boston Celtics -17  16
    ## 1081 2PT Field Goal            11   Los Angeles Clippers   1  11
    ## 1082 2PT Field Goal             8 Portland Trail Blazers  -1   8
    ## 1083 2PT Field Goal            16        Houston Rockets  12  11
    ## 1084 2PT Field Goal            29    Cleveland Cavaliers  20  21
    ## 1085 2PT Field Goal            18      Charlotte Hornets   9  16
    ## 1086 2PT Field Goal            34          Orlando Magic  28  19
    ## 1087 2PT Field Goal            25 Minnesota Timberwolves -14  21
    ## 1088 2PT Field Goal            12 Portland Trail Blazers  12   2
    ## 1089 2PT Field Goal            19        New York Knicks -19   2
    ## 1090 2PT Field Goal             4        New York Knicks   0   4
    ## 1091 2PT Field Goal             7        Detroit Pistons   4  -6
    ## 1092 2PT Field Goal            26      Memphis Grizzlies -19  18
    ## 1093 2PT Field Goal            11 Minnesota Timberwolves   1  11
    ## 1094 2PT Field Goal             9         Boston Celtics   9   2
    ## 1095 2PT Field Goal            18    Cleveland Cavaliers -14 -11
    ## 1096 2PT Field Goal             5    Cleveland Cavaliers   0  -5
    ## 1097 2PT Field Goal            16         Denver Nuggets  12  11
    ## 1098 2PT Field Goal            13   New Orleans Pelicans  -7  11
    ## 1099 2PT Field Goal            23          Chicago Bulls -22   7
    ## 1100 2PT Field Goal            22          Brooklyn Nets  22   3
    ## 1101 2PT Field Goal            11              Utah Jazz   1  11
    ## 1102 2PT Field Goal            27          Chicago Bulls  25  11
    ## 1103 2PT Field Goal            25 Portland Trail Blazers  19  16
    ## 1104 2PT Field Goal            16     Philadelphia 76ers   0  16
    ## 1105 2PT Field Goal            12          Brooklyn Nets -12   2
    ## 1106 2PT Field Goal             7  Oklahoma City Thunder   6  -4
    ## 1107 2PT Field Goal            15       Dallas Mavericks  15   2
    ## 1108 2PT Field Goal            11          Brooklyn Nets   1  11
    ## 1109 2PT Field Goal            23 Minnesota Timberwolves  14  18
    ## 1110 2PT Field Goal            48 Portland Trail Blazers -32  36
    ## 1111 2PT Field Goal            43           Phoenix Suns -38  21
    ## 1112 2PT Field Goal            31         Denver Nuggets -29  11
    ## 1113 2PT Field Goal            46        New York Knicks  20  41
    ## 1114 2PT Field Goal            45        Toronto Raptors  32  31
    ## 1115 2PT Field Goal            49          Atlanta Hawks -17  46
    ## 1116 2PT Field Goal            39   New Orleans Pelicans   9  38
    ## 1117 2PT Field Goal            43   Los Angeles Clippers -20  38
    ## 1118 2PT Field Goal            46        Toronto Raptors  43  16
    ## 1119 2PT Field Goal            38        Houston Rockets  30  23
    ## 1120 2PT Field Goal            44     Washington Wizards  42  13
    ## 1121 2PT Field Goal            41       Sacramento Kings -37  18
    ## 1122 2PT Field Goal            40       Sacramento Kings -14  38
    ## 1123 2PT Field Goal            36      Memphis Grizzlies -19  31
    ## 1124 2PT Field Goal            39      Memphis Grizzlies  27  28
    ## 1125 2PT Field Goal            36 Minnesota Timberwolves  28  23
    ## 1126 2PT Field Goal            44      San Antonio Spurs  22  38
    ## 1127 2PT Field Goal            33         Indiana Pacers -12  31
    ## 1128 2PT Field Goal            18     Philadelphia 76ers -17  -6
    ## 1129 2PT Field Goal            12 Portland Trail Blazers -12   2
    ## 1130 2PT Field Goal            10        Detroit Pistons  10   0
    ## 1131 2PT Field Goal            12        Houston Rockets  12  -1
    ## 1132 2PT Field Goal            26   Los Angeles Clippers  24  11
    ## 1133 2PT Field Goal            14        Houston Rockets  12   8
    ## 1134 2PT Field Goal            18         Indiana Pacers   9  16
    ## 1135 2PT Field Goal             5     Los Angeles Lakers  -5   1
    ## 1136 2PT Field Goal            18       Sacramento Kings -17   7
    ## 1137 2PT Field Goal            23         Denver Nuggets  10  21
    ## 1138 2PT Field Goal            22     Washington Wizards -22   2
    ## 1139 2PT Field Goal             3         Denver Nuggets   3  -1
    ## 1140 2PT Field Goal            25          Chicago Bulls  19  16
    ## 1141 2PT Field Goal            27        Houston Rockets  -6  26
    ## 1142 2PT Field Goal            16        New York Knicks -11  11
    ## 1143 2PT Field Goal            28 Portland Trail Blazers   0  28
    ## 1144 2PT Field Goal            22   New Orleans Pelicans -19  11
    ## 1145 2PT Field Goal            26        New York Knicks  24 -10
    ## 1146 2PT Field Goal            13        Houston Rockets  -2  13
    ## 1147 2PT Field Goal            11 Minnesota Timberwolves   9   7
    ## 1148 2PT Field Goal            20   New Orleans Pelicans -17  11
    ## 1149 2PT Field Goal            10        New York Knicks   9  -5
    ## 1150 2PT Field Goal            29   Los Angeles Clippers  27  11
    ## 1151 2PT Field Goal            18      San Antonio Spurs -17   7
    ## 1152 2PT Field Goal            29     Los Angeles Lakers -28   6
    ## 1153 2PT Field Goal             8         Denver Nuggets   4   7
    ## 1154 2PT Field Goal            12     Washington Wizards  12  -1
    ## 1155 2PT Field Goal            18        Milwaukee Bucks   2  18
    ## 1156 2PT Field Goal            26          Chicago Bulls   1  26
    ## 1157 2PT Field Goal            13 Minnesota Timberwolves   6  11
    ## 1158 2PT Field Goal             9     Los Angeles Lakers   8   3
    ## 1159 2PT Field Goal            27  Oklahoma City Thunder -25  11
    ## 1160 2PT Field Goal            12   New Orleans Pelicans  10   7
    ## 1161 2PT Field Goal            23       Sacramento Kings -22   7
    ## 1162 2PT Field Goal            22 Portland Trail Blazers -22   2
    ## 1163 2PT Field Goal            18              Utah Jazz   2  18
    ## 1164 2PT Field Goal             4 Portland Trail Blazers  -4   0
    ## 1165 2PT Field Goal            12        Houston Rockets   4  11
    ## 1166 2PT Field Goal            20   New Orleans Pelicans -20   2
    ## 1167 2PT Field Goal            26       Dallas Mavericks   4  26
    ## 1168 2PT Field Goal            12      San Antonio Spurs -12   0
    ## 1169 2PT Field Goal            15      Memphis Grizzlies  15   3
    ## 1170 2PT Field Goal            43       Sacramento Kings  40  16
    ## 1171 2PT Field Goal            15        Toronto Raptors  -7  13
    ## 1172 2PT Field Goal            38   New Orleans Pelicans  37   8
    ## 1173 2PT Field Goal             8          Atlanta Hawks   4   7
    ## 1174 2PT Field Goal            18          Atlanta Hawks  -1  18
    ## 1175 2PT Field Goal             7  Oklahoma City Thunder  -7   2
    ## 1176 2PT Field Goal            14         Indiana Pacers -12   7
    ## 1177 2PT Field Goal             1        Milwaukee Bucks  -1  -1
    ## 1178 2PT Field Goal            18           Phoenix Suns -12  13
    ## 1179 2PT Field Goal            24 Portland Trail Blazers  24   3
    ## 1180 2PT Field Goal            11 Minnesota Timberwolves   9   7
    ## 1181 2PT Field Goal            22      Memphis Grizzlies  22   2
    ## 1182 2PT Field Goal            18     Los Angeles Lakers -18  -2
    ## 1183 2PT Field Goal             1     Los Angeles Lakers   0   1
    ## 1184 2PT Field Goal             1  Oklahoma City Thunder   0   1
    ## 1185 2PT Field Goal             1        New York Knicks   0   1
    ## 1186 2PT Field Goal            23        Houston Rockets  15  18
    ## 1187 2PT Field Goal            46        New York Knicks  46   2
    ## 1188 2PT Field Goal            31       Sacramento Kings  27  16
    ## 1189 2PT Field Goal            26          Chicago Bulls  15  21
    ## 1190 2PT Field Goal             8       Dallas Mavericks   6   5
    ## 1191 2PT Field Goal             9 Minnesota Timberwolves  -6   7
    ## 1192 2PT Field Goal            25       Sacramento Kings  19  16
    ## 1193 2PT Field Goal            19 Portland Trail Blazers  10  16
    ## 1194 2PT Field Goal            11       Sacramento Kings   0  11
    ## 1195 2PT Field Goal             8           Phoenix Suns  -7   3
    ## 1196 2PT Field Goal            27          Brooklyn Nets -27   3
    ## 1197 2PT Field Goal            17              Utah Jazz  17   3
    ## 1198 2PT Field Goal            16      Memphis Grizzlies   9  13
    ## 1199 2PT Field Goal            17           Phoenix Suns -17   2
    ## 1200 2PT Field Goal            26   Los Angeles Clippers  19  18
    ## 1201 2PT Field Goal            11         Indiana Pacers   2  11
    ## 1202 2PT Field Goal            16           Phoenix Suns  14   7
    ## 1203 2PT Field Goal            23       Sacramento Kings   1  23
    ## 1204 2PT Field Goal            19  Oklahoma City Thunder  19   2
    ## 1205 2PT Field Goal            13     Los Angeles Lakers  -6  11
    ## 1206 2PT Field Goal            13      San Antonio Spurs  -1  13
    ## 1207 2PT Field Goal            12        Houston Rockets -12  -1
    ## 1208 2PT Field Goal             8     Los Angeles Lakers   8   1
    ## 1209 2PT Field Goal            23      San Antonio Spurs   1  23
    ## 1210 2PT Field Goal            19         Indiana Pacers -11  16
    ## 1211 2PT Field Goal            16      Memphis Grizzlies   5  15
    ## 1212 2PT Field Goal             3 Portland Trail Blazers   0   3
    ## 1213 2PT Field Goal            22   Los Angeles Clippers  19  11
    ## 1214 2PT Field Goal            40        Houston Rockets -25  31
    ## 1215 2PT Field Goal            44        New York Knicks  15  41
    ## 1216 2PT Field Goal            32        Milwaukee Bucks   6  31
    ##                name minute
    ## 1     Stephen Curry     33
    ## 2     Stephen Curry     27
    ## 3     Stephen Curry     19
    ## 4     Stephen Curry     31
    ## 5     Stephen Curry     34
    ## 6     Stephen Curry     43
    ## 7     Stephen Curry     27
    ## 8     Stephen Curry     23
    ## 9     Stephen Curry     26
    ## 10    Stephen Curry     33
    ## 11    Stephen Curry     39
    ## 12    Stephen Curry     45
    ## 13    Stephen Curry     32
    ## 14    Stephen Curry      3
    ## 15    Stephen Curry     11
    ## 16    Stephen Curry      4
    ## 17    Stephen Curry     27
    ## 18    Stephen Curry     43
    ## 19    Stephen Curry      9
    ## 20    Stephen Curry     31
    ## 21    Stephen Curry      4
    ## 22    Stephen Curry     33
    ## 23    Stephen Curry     34
    ## 24    Stephen Curry     42
    ## 25    Stephen Curry      9
    ## 26    Stephen Curry     23
    ## 27    Stephen Curry     46
    ## 28    Stephen Curry     28
    ## 29    Stephen Curry     10
    ## 30    Stephen Curry      1
    ## 31    Stephen Curry     44
    ## 32    Stephen Curry     28
    ## 33    Stephen Curry     22
    ## 34    Stephen Curry      3
    ## 35    Stephen Curry      5
    ## 36    Stephen Curry     20
    ## 37    Stephen Curry     10
    ## 38    Stephen Curry     16
    ## 39    Stephen Curry      7
    ## 40    Stephen Curry     23
    ## 41    Stephen Curry     48
    ## 42    Stephen Curry     35
    ## 43    Stephen Curry     31
    ## 44    Stephen Curry     10
    ## 45    Stephen Curry     46
    ## 46    Stephen Curry      5
    ## 47    Stephen Curry      2
    ## 48    Stephen Curry     35
    ## 49    Stephen Curry     11
    ## 50    Stephen Curry     48
    ## 51    Stephen Curry     11
    ## 52    Stephen Curry     34
    ## 53    Stephen Curry     43
    ## 54    Stephen Curry      5
    ## 55    Stephen Curry     11
    ## 56    Stephen Curry     24
    ## 57    Stephen Curry     46
    ## 58    Stephen Curry     29
    ## 59    Stephen Curry     36
    ## 60    Stephen Curry     33
    ## 61    Stephen Curry     24
    ## 62    Stephen Curry     36
    ## 63    Stephen Curry     29
    ## 64    Stephen Curry     23
    ## 65    Stephen Curry      8
    ## 66    Stephen Curry     27
    ## 67    Stephen Curry      4
    ## 68    Stephen Curry     22
    ## 69    Stephen Curry     43
    ## 70    Stephen Curry      6
    ## 71    Stephen Curry     45
    ## 72    Stephen Curry     42
    ## 73    Stephen Curry      2
    ## 74    Stephen Curry     23
    ## 75    Stephen Curry     11
    ## 76    Stephen Curry      3
    ## 77    Stephen Curry     12
    ## 78    Stephen Curry     32
    ## 79    Stephen Curry      4
    ## 80    Stephen Curry     21
    ## 81    Stephen Curry     28
    ## 82    Stephen Curry     47
    ## 83    Stephen Curry     22
    ## 84    Stephen Curry     32
    ## 85    Stephen Curry     11
    ## 86    Stephen Curry     26
    ## 87    Stephen Curry     35
    ## 88    Stephen Curry     42
    ## 89    Stephen Curry     45
    ## 90    Stephen Curry     32
    ## 91    Stephen Curry     20
    ## 92    Stephen Curry      5
    ## 93    Stephen Curry     12
    ## 94    Stephen Curry     23
    ## 95    Stephen Curry     10
    ## 96    Stephen Curry     32
    ## 97    Stephen Curry     41
    ## 98    Stephen Curry     30
    ## 99    Stephen Curry     30
    ## 100   Stephen Curry     32
    ## 101   Stephen Curry      8
    ## 102   Stephen Curry     23
    ## 103   Stephen Curry     10
    ## 104   Stephen Curry     35
    ## 105   Stephen Curry     34
    ## 106   Stephen Curry     12
    ## 107   Stephen Curry     35
    ## 108   Stephen Curry     10
    ## 109   Stephen Curry     35
    ## 110   Stephen Curry     42
    ## 111   Stephen Curry     11
    ## 112   Stephen Curry     21
    ## 113   Stephen Curry     26
    ## 114   Stephen Curry     11
    ## 115   Stephen Curry     24
    ## 116   Stephen Curry     28
    ## 117   Stephen Curry     17
    ## 118   Stephen Curry     22
    ## 119   Stephen Curry     45
    ## 120   Stephen Curry     20
    ## 121   Stephen Curry     23
    ## 122   Stephen Curry     35
    ## 123   Stephen Curry     47
    ## 124   Stephen Curry     22
    ## 125   Stephen Curry     27
    ## 126   Stephen Curry     46
    ## 127   Stephen Curry     44
    ## 128   Stephen Curry      7
    ## 129   Stephen Curry      9
    ## 130   Stephen Curry     10
    ## 131   Stephen Curry     20
    ## 132   Stephen Curry     35
    ## 133   Stephen Curry     33
    ## 134   Stephen Curry     44
    ## 135   Stephen Curry     24
    ## 136   Stephen Curry     31
    ## 137   Stephen Curry     43
    ## 138   Stephen Curry     34
    ## 139   Stephen Curry      3
    ## 140   Stephen Curry      7
    ## 141   Stephen Curry     25
    ## 142   Stephen Curry      2
    ## 143   Stephen Curry     35
    ## 144   Stephen Curry      8
    ## 145   Stephen Curry     47
    ## 146   Stephen Curry     33
    ## 147   Stephen Curry     46
    ## 148   Stephen Curry     11
    ## 149   Stephen Curry     33
    ## 150   Stephen Curry     28
    ## 151   Stephen Curry      7
    ## 152   Stephen Curry     12
    ## 153   Stephen Curry     30
    ## 154   Stephen Curry     33
    ## 155   Stephen Curry     32
    ## 156   Stephen Curry     36
    ## 157   Stephen Curry     27
    ## 158   Stephen Curry      1
    ## 159   Stephen Curry     24
    ## 160   Stephen Curry     19
    ## 161   Stephen Curry     11
    ## 162   Stephen Curry     13
    ## 163   Stephen Curry     24
    ## 164   Stephen Curry     36
    ## 165   Stephen Curry     28
    ## 166   Stephen Curry     30
    ## 167   Stephen Curry     26
    ## 168   Stephen Curry      7
    ## 169   Stephen Curry     19
    ## 170   Stephen Curry      8
    ## 171   Stephen Curry     34
    ## 172   Stephen Curry     33
    ## 173   Stephen Curry      3
    ## 174   Stephen Curry     48
    ## 175   Stephen Curry     47
    ## 176   Stephen Curry     19
    ## 177   Stephen Curry     33
    ## 178   Stephen Curry      5
    ## 179   Stephen Curry     46
    ## 180   Stephen Curry     22
    ## 181   Stephen Curry     44
    ## 182   Stephen Curry     19
    ## 183   Stephen Curry     28
    ## 184   Stephen Curry     29
    ## 185   Stephen Curry     42
    ## 186   Stephen Curry     22
    ## 187   Stephen Curry     43
    ## 188   Stephen Curry     25
    ## 189   Stephen Curry     24
    ## 190   Stephen Curry     43
    ## 191   Stephen Curry     33
    ## 192   Stephen Curry     27
    ## 193   Stephen Curry     23
    ## 194   Stephen Curry     42
    ## 195   Stephen Curry     22
    ## 196   Stephen Curry      7
    ## 197   Stephen Curry     46
    ## 198   Stephen Curry      9
    ## 199   Stephen Curry     28
    ## 200   Stephen Curry     22
    ## 201   Stephen Curry     26
    ## 202   Stephen Curry     20
    ## 203   Stephen Curry     34
    ## 204   Stephen Curry     11
    ## 205   Stephen Curry     43
    ## 206   Stephen Curry     21
    ## 207   Stephen Curry     27
    ## 208   Stephen Curry     34
    ## 209   Stephen Curry     12
    ## 210   Stephen Curry     43
    ## 211   Stephen Curry     42
    ## 212   Stephen Curry     27
    ## 213   Stephen Curry     32
    ## 214   Stephen Curry      8
    ## 215   Stephen Curry     35
    ## 216   Stephen Curry      6
    ## 217   Stephen Curry     20
    ## 218   Stephen Curry      5
    ## 219   Stephen Curry     10
    ## 220   Stephen Curry     43
    ## 221   Stephen Curry      8
    ## 222   Stephen Curry     26
    ## 223   Stephen Curry      9
    ## 224   Stephen Curry     34
    ## 225   Stephen Curry      6
    ## 226   Stephen Curry     33
    ## 227   Stephen Curry     30
    ## 228   Stephen Curry      3
    ## 229   Stephen Curry     24
    ## 230   Stephen Curry     21
    ## 231   Stephen Curry     23
    ## 232   Stephen Curry     47
    ## 233   Stephen Curry     21
    ## 234   Stephen Curry     35
    ## 235   Stephen Curry     30
    ## 236   Stephen Curry     21
    ## 237   Stephen Curry     28
    ## 238   Stephen Curry     21
    ## 239   Stephen Curry     31
    ## 240   Stephen Curry     28
    ## 241   Stephen Curry     19
    ## 242   Stephen Curry     36
    ## 243   Stephen Curry     36
    ## 244   Stephen Curry      5
    ## 245   Stephen Curry     20
    ## 246   Stephen Curry      4
    ## 247   Stephen Curry     28
    ## 248   Stephen Curry     28
    ## 249   Stephen Curry     20
    ## 250   Stephen Curry      9
    ## 251   Stephen Curry     14
    ## 252   Stephen Curry     21
    ## 253   Stephen Curry      7
    ## 254   Stephen Curry      9
    ## 255   Stephen Curry     21
    ## 256   Stephen Curry     32
    ## 257   Stephen Curry     24
    ## 258   Stephen Curry     43
    ## 259   Stephen Curry     32
    ## 260   Stephen Curry     29
    ## 261   Stephen Curry      6
    ## 262   Stephen Curry     22
    ## 263   Stephen Curry      6
    ## 264   Stephen Curry     30
    ## 265   Stephen Curry     27
    ## 266   Stephen Curry     34
    ## 267   Stephen Curry     39
    ## 268   Stephen Curry      8
    ## 269   Stephen Curry     29
    ## 270   Stephen Curry      8
    ## 271   Stephen Curry     30
    ## 272   Stephen Curry     44
    ## 273   Stephen Curry     34
    ## 274   Stephen Curry     23
    ## 275   Stephen Curry     21
    ## 276   Stephen Curry      7
    ## 277   Stephen Curry      3
    ## 278   Stephen Curry     31
    ## 279   Stephen Curry     29
    ## 280   Stephen Curry      7
    ## 281   Stephen Curry     47
    ## 282   Stephen Curry     20
    ## 283   Stephen Curry     29
    ## 284   Stephen Curry      9
    ## 285   Stephen Curry     36
    ## 286   Stephen Curry     21
    ## 287   Stephen Curry     33
    ## 288   Stephen Curry     12
    ## 289   Stephen Curry      6
    ## 290   Stephen Curry     10
    ## 291   Stephen Curry      1
    ## 292   Stephen Curry     35
    ## 293   Stephen Curry      9
    ## 294   Stephen Curry      3
    ## 295   Stephen Curry      3
    ## 296   Stephen Curry     24
    ## 297   Stephen Curry      4
    ## 298   Stephen Curry     12
    ## 299   Stephen Curry     17
    ## 300   Stephen Curry      2
    ## 301   Stephen Curry     32
    ## 302  Andre Iguodala     34
    ## 303  Andre Iguodala     14
    ## 304  Andre Iguodala     24
    ## 305  Andre Iguodala     13
    ## 306  Andre Iguodala     48
    ## 307  Andre Iguodala     31
    ## 308  Andre Iguodala     41
    ## 309  Andre Iguodala     39
    ## 310  Andre Iguodala     24
    ## 311  Andre Iguodala     24
    ## 312  Andre Iguodala     12
    ## 313  Andre Iguodala     12
    ## 314  Andre Iguodala     21
    ## 315  Andre Iguodala     31
    ## 316  Andre Iguodala     33
    ## 317  Andre Iguodala     23
    ## 318  Andre Iguodala      9
    ## 319  Andre Iguodala     11
    ## 320  Andre Iguodala     12
    ## 321  Andre Iguodala     21
    ## 322  Andre Iguodala     35
    ## 323  Andre Iguodala     19
    ## 324  Andre Iguodala     39
    ## 325  Andre Iguodala     22
    ## 326  Andre Iguodala     10
    ## 327  Andre Iguodala     48
    ## 328  Andre Iguodala     10
    ## 329  Andre Iguodala     37
    ## 330  Andre Iguodala     24
    ## 331  Andre Iguodala     21
    ## 332  Andre Iguodala     10
    ## 333  Andre Iguodala     12
    ## 334  Andre Iguodala     38
    ## 335  Andre Iguodala     40
    ## 336  Andre Iguodala     46
    ## 337  Andre Iguodala     24
    ## 338  Andre Iguodala     32
    ## 339  Andre Iguodala      9
    ## 340  Andre Iguodala     21
    ## 341  Andre Iguodala      9
    ## 342  Andre Iguodala      9
    ## 343  Andre Iguodala     33
    ## 344  Andre Iguodala     36
    ## 345  Andre Iguodala      9
    ## 346  Andre Iguodala     48
    ## 347  Andre Iguodala     22
    ## 348  Andre Iguodala     43
    ## 349  Andre Iguodala     33
    ## 350  Andre Iguodala     31
    ## 351  Andre Iguodala     24
    ## 352  Andre Iguodala     24
    ## 353  Andre Iguodala     38
    ## 354  Andre Iguodala     35
    ## 355  Andre Iguodala     21
    ## 356  Andre Iguodala     39
    ## 357  Andre Iguodala     40
    ## 358  Andre Iguodala     17
    ## 359  Andre Iguodala     37
    ## 360  Andre Iguodala     33
    ## 361  Andre Iguodala     12
    ## 362  Andre Iguodala     16
    ## 363  Andre Iguodala     31
    ## 364  Andre Iguodala     46
    ## 365  Andre Iguodala     10
    ## 366  Andre Iguodala     33
    ## 367  Andre Iguodala     22
    ## 368  Andre Iguodala     12
    ## 369  Andre Iguodala     13
    ## 370  Andre Iguodala      7
    ## 371  Andre Iguodala     35
    ## 372  Andre Iguodala      9
    ## 373  Andre Iguodala     19
    ## 374  Andre Iguodala     40
    ## 375  Andre Iguodala     40
    ## 376  Andre Iguodala     40
    ## 377  Andre Iguodala     35
    ## 378  Andre Iguodala     16
    ## 379  Andre Iguodala     45
    ## 380  Andre Iguodala      8
    ## 381  Andre Iguodala     31
    ## 382  Andre Iguodala     40
    ## 383  Andre Iguodala     39
    ## 384  Andre Iguodala     39
    ## 385  Andre Iguodala     46
    ## 386  Andre Iguodala     15
    ## 387  Andre Iguodala     36
    ## 388  Andre Iguodala     21
    ## 389  Andre Iguodala     24
    ## 390  Andre Iguodala     15
    ## 391  Andre Iguodala     16
    ## 392  Andre Iguodala     38
    ## 393  Andre Iguodala     33
    ## 394  Andre Iguodala      7
    ## 395  Andre Iguodala     34
    ## 396  Andre Iguodala      4
    ## 397  Andre Iguodala     23
    ## 398  Andre Iguodala     37
    ## 399  Andre Iguodala     35
    ## 400  Andre Iguodala     12
    ## 401  Andre Iguodala     15
    ## 402  Andre Iguodala     47
    ## 403  Andre Iguodala      9
    ## 404  Andre Iguodala     10
    ## 405  Andre Iguodala     24
    ## 406  Andre Iguodala     16
    ## 407  Andre Iguodala     44
    ## 408  Andre Iguodala     29
    ## 409  Andre Iguodala      8
    ## 410  Andre Iguodala     30
    ## 411  Andre Iguodala     11
    ## 412  Andre Iguodala     15
    ## 413  Andre Iguodala     15
    ## 414  Andre Iguodala     14
    ## 415  Andre Iguodala     12
    ## 416  Andre Iguodala     15
    ## 417  Andre Iguodala     38
    ## 418  Andre Iguodala      9
    ## 419  Andre Iguodala     13
    ## 420  Andre Iguodala     22
    ## 421  Andre Iguodala     12
    ## 422  Andre Iguodala      8
    ## 423  Andre Iguodala      8
    ## 424  Andre Iguodala     37
    ## 425  Andre Iguodala     14
    ## 426  Andre Iguodala     41
    ## 427  Andre Iguodala     10
    ## 428  Andre Iguodala     35
    ## 429  Andre Iguodala     10
    ## 430  Andre Iguodala     41
    ## 431  Andre Iguodala     23
    ## 432  Andre Iguodala     12
    ## 433  Andre Iguodala     32
    ## 434  Andre Iguodala     36
    ## 435  Andre Iguodala     32
    ## 436  Andre Iguodala     43
    ## 437  Andre Iguodala     41
    ## 438  Andre Iguodala     41
    ## 439  Andre Iguodala     33
    ## 440  Andre Iguodala     39
    ## 441  Andre Iguodala     35
    ## 442  Andre Iguodala     32
    ## 443  Andre Iguodala     21
    ## 444  Andre Iguodala     12
    ## 445  Andre Iguodala     41
    ## 446  Andre Iguodala     38
    ## 447  Draymond Green     29
    ## 448  Draymond Green     29
    ## 449  Draymond Green      3
    ## 450  Draymond Green     24
    ## 451  Draymond Green      4
    ## 452  Draymond Green     27
    ## 453  Draymond Green     42
    ## 454  Draymond Green     35
    ## 455  Draymond Green     48
    ## 456  Draymond Green     33
    ## 457  Draymond Green      2
    ## 458  Draymond Green     47
    ## 459  Draymond Green      4
    ## 460  Draymond Green     22
    ## 461  Draymond Green      7
    ## 462  Draymond Green     21
    ## 463  Draymond Green     44
    ## 464  Draymond Green     40
    ## 465  Draymond Green      6
    ## 466  Draymond Green      3
    ## 467  Draymond Green     27
    ## 468  Draymond Green     11
    ## 469  Draymond Green      4
    ## 470  Draymond Green     11
    ## 471  Draymond Green     21
    ## 472  Draymond Green     16
    ## 473  Draymond Green     30
    ## 474  Draymond Green     46
    ## 475  Draymond Green      3
    ## 476  Draymond Green     27
    ## 477  Draymond Green      6
    ## 478  Draymond Green     48
    ## 479  Draymond Green     29
    ## 480  Draymond Green     14
    ## 481  Draymond Green     10
    ## 482  Draymond Green     21
    ## 483  Draymond Green     18
    ## 484  Draymond Green      8
    ## 485  Draymond Green     20
    ## 486  Draymond Green     25
    ## 487  Draymond Green     18
    ## 488  Draymond Green      5
    ## 489  Draymond Green     33
    ## 490  Draymond Green      8
    ## 491  Draymond Green      7
    ## 492  Draymond Green     21
    ## 493  Draymond Green      7
    ## 494  Draymond Green     20
    ## 495  Draymond Green     20
    ## 496  Draymond Green     21
    ## 497  Draymond Green     44
    ## 498  Draymond Green     38
    ## 499  Draymond Green     38
    ## 500  Draymond Green     20
    ## 501  Draymond Green     24
    ## 502  Draymond Green     16
    ## 503  Draymond Green      5
    ## 504  Draymond Green     25
    ## 505  Draymond Green     37
    ## 506  Draymond Green      7
    ## 507  Draymond Green     34
    ## 508  Draymond Green     45
    ## 509  Draymond Green     42
    ## 510  Draymond Green     31
    ## 511  Draymond Green      1
    ## 512  Draymond Green     24
    ## 513  Draymond Green     26
    ## 514  Draymond Green      1
    ## 515  Draymond Green     36
    ## 516  Draymond Green     25
    ## 517  Draymond Green      6
    ## 518  Draymond Green     13
    ## 519  Draymond Green     43
    ## 520  Draymond Green     35
    ## 521  Draymond Green     22
    ## 522  Draymond Green     27
    ## 523  Draymond Green     12
    ## 524  Draymond Green     44
    ## 525  Draymond Green     16
    ## 526  Draymond Green      3
    ## 527  Draymond Green     10
    ## 528  Draymond Green     38
    ## 529  Draymond Green     26
    ## 530  Draymond Green     42
    ## 531  Draymond Green     46
    ## 532  Draymond Green     42
    ## 533  Draymond Green     23
    ## 534  Draymond Green      3
    ## 535  Draymond Green     47
    ## 536  Draymond Green     22
    ## 537  Draymond Green     24
    ## 538  Draymond Green     45
    ## 539  Draymond Green     42
    ## 540  Draymond Green     32
    ## 541  Draymond Green     23
    ## 542  Draymond Green     26
    ## 543  Draymond Green     20
    ## 544  Draymond Green     25
    ## 545  Draymond Green     29
    ## 546  Draymond Green     38
    ## 547  Draymond Green     20
    ## 548  Draymond Green      4
    ## 549  Draymond Green     47
    ## 550  Draymond Green     29
    ## 551  Draymond Green     37
    ## 552  Draymond Green     22
    ## 553  Draymond Green     22
    ## 554  Draymond Green     42
    ## 555  Draymond Green     27
    ## 556  Draymond Green     29
    ## 557  Draymond Green     19
    ## 558  Draymond Green     30
    ## 559  Draymond Green     32
    ## 560  Draymond Green     30
    ## 561  Draymond Green     25
    ## 562  Draymond Green     36
    ## 563  Draymond Green     19
    ## 564  Draymond Green      6
    ## 565  Draymond Green     13
    ## 566  Draymond Green     40
    ## 567  Draymond Green     28
    ## 568  Draymond Green     34
    ## 569  Draymond Green     10
    ## 570  Draymond Green      6
    ## 571  Draymond Green      9
    ## 572  Draymond Green     27
    ## 573  Draymond Green     23
    ## 574  Draymond Green      3
    ## 575  Draymond Green     19
    ## 576  Draymond Green     23
    ## 577  Draymond Green     22
    ## 578  Draymond Green     27
    ## 579  Draymond Green     13
    ## 580  Draymond Green     29
    ## 581  Draymond Green      6
    ## 582  Draymond Green     41
    ## 583  Draymond Green     41
    ## 584  Draymond Green      4
    ## 585  Draymond Green     35
    ## 586  Draymond Green     19
    ## 587  Draymond Green     27
    ## 588  Draymond Green     45
    ## 589  Draymond Green      7
    ## 590  Draymond Green      7
    ## 591  Draymond Green      4
    ## 592  Draymond Green      7
    ## 593  Draymond Green      8
    ## 594  Draymond Green     27
    ## 595  Draymond Green     31
    ## 596  Draymond Green     29
    ## 597  Draymond Green     28
    ## 598  Draymond Green     10
    ## 599  Draymond Green      2
    ## 600  Draymond Green     47
    ## 601  Draymond Green      6
    ## 602  Draymond Green     48
    ## 603  Draymond Green     25
    ## 604  Draymond Green     27
    ## 605  Draymond Green     21
    ## 606  Draymond Green     27
    ## 607  Draymond Green      5
    ## 608  Draymond Green      5
    ## 609  Draymond Green     43
    ## 610  Draymond Green     18
    ## 611  Draymond Green     43
    ## 612  Draymond Green     14
    ## 613  Draymond Green     21
    ## 614  Draymond Green      7
    ## 615  Draymond Green     24
    ## 616  Draymond Green      3
    ## 617  Draymond Green      7
    ## 618  Draymond Green      4
    ## 619  Draymond Green     27
    ## 620  Draymond Green     26
    ## 621  Draymond Green     26
    ## 622  Draymond Green     23
    ## 623  Draymond Green      8
    ## 624  Draymond Green     28
    ## 625  Draymond Green     18
    ## 626  Draymond Green     30
    ## 627  Draymond Green     46
    ## 628  Draymond Green     31
    ## 629  Draymond Green     37
    ## 630  Draymond Green     11
    ## 631  Draymond Green     46
    ## 632  Draymond Green      3
    ## 633  Draymond Green     23
    ## 634  Draymond Green     20
    ## 635  Draymond Green      6
    ## 636  Draymond Green     30
    ## 637  Draymond Green      8
    ## 638  Draymond Green     25
    ## 639  Draymond Green     22
    ## 640  Draymond Green     23
    ## 641  Draymond Green      7
    ## 642  Draymond Green     45
    ## 643  Draymond Green     10
    ## 644  Draymond Green     35
    ## 645  Draymond Green     30
    ## 646  Draymond Green     11
    ## 647  Draymond Green     27
    ## 648  Draymond Green     20
    ## 649  Draymond Green     18
    ## 650  Draymond Green      4
    ## 651  Draymond Green      4
    ## 652  Draymond Green     25
    ## 653  Draymond Green     28
    ## 654  Draymond Green     34
    ## 655  Draymond Green     31
    ## 656  Draymond Green     22
    ## 657  Draymond Green     47
    ## 658  Draymond Green     24
    ## 659  Draymond Green     24
    ## 660  Draymond Green      5
    ## 661  Draymond Green     31
    ## 662  Draymond Green     43
    ## 663  Draymond Green      5
    ## 664  Draymond Green      6
    ## 665  Draymond Green     23
    ## 666  Draymond Green      1
    ## 667  Draymond Green     11
    ## 668  Draymond Green     12
    ## 669  Draymond Green     21
    ## 670  Draymond Green     23
    ## 671  Draymond Green     34
    ## 672  Draymond Green     33
    ## 673  Draymond Green      3
    ## 674  Draymond Green      7
    ## 675  Draymond Green     12
    ## 676  Draymond Green     26
    ## 677  Draymond Green     35
    ## 678  Draymond Green      7
    ## 679  Draymond Green      5
    ## 680  Draymond Green     19
    ## 681  Draymond Green     34
    ## 682  Draymond Green      2
    ## 683  Draymond Green     24
    ## 684  Draymond Green     24
    ## 685  Draymond Green      6
    ## 686  Draymond Green     40
    ## 687  Draymond Green     30
    ## 688  Draymond Green     27
    ## 689  Draymond Green     27
    ## 690  Draymond Green     45
    ## 691  Draymond Green     35
    ## 692  Draymond Green     48
    ## 693  Draymond Green     11
    ## 694  Draymond Green     28
    ## 695  Draymond Green     22
    ## 696  Draymond Green      9
    ## 697  Draymond Green      9
    ## 698  Draymond Green     31
    ## 699  Draymond Green     45
    ## 700  Draymond Green     19
    ## 701  Draymond Green     22
    ## 702  Draymond Green     41
    ## 703  Draymond Green     26
    ## 704  Draymond Green     36
    ## 705  Draymond Green     27
    ## 706  Draymond Green      1
    ## 707  Draymond Green     47
    ## 708  Draymond Green     28
    ## 709    Kevin Durant     27
    ## 710    Kevin Durant     28
    ## 711    Kevin Durant     27
    ## 712    Kevin Durant     31
    ## 713    Kevin Durant     47
    ## 714    Kevin Durant     26
    ## 715    Kevin Durant     25
    ## 716    Kevin Durant     31
    ## 717    Kevin Durant      2
    ## 718    Kevin Durant     30
    ## 719    Kevin Durant      1
    ## 720    Kevin Durant     21
    ## 721    Kevin Durant      3
    ## 722    Kevin Durant      3
    ## 723    Kevin Durant     25
    ## 724    Kevin Durant     27
    ## 725    Kevin Durant     43
    ## 726    Kevin Durant     45
    ## 727    Kevin Durant     20
    ## 728    Kevin Durant      4
    ## 729    Kevin Durant     21
    ## 730    Kevin Durant     15
    ## 731    Kevin Durant     28
    ## 732    Kevin Durant     31
    ## 733    Kevin Durant      4
    ## 734    Kevin Durant     26
    ## 735    Kevin Durant     24
    ## 736    Kevin Durant     47
    ## 737    Kevin Durant     43
    ## 738    Kevin Durant     26
    ## 739    Kevin Durant     29
    ## 740    Kevin Durant      6
    ## 741    Kevin Durant     22
    ## 742    Kevin Durant      6
    ## 743    Kevin Durant      6
    ## 744    Kevin Durant     30
    ## 745    Kevin Durant      3
    ## 746    Kevin Durant     25
    ## 747    Kevin Durant     15
    ## 748    Kevin Durant     22
    ## 749    Kevin Durant     41
    ## 750    Kevin Durant     23
    ## 751    Kevin Durant      4
    ## 752    Kevin Durant     22
    ## 753    Kevin Durant     39
    ## 754    Kevin Durant     41
    ## 755    Kevin Durant     32
    ## 756    Kevin Durant     14
    ## 757    Kevin Durant     26
    ## 758    Kevin Durant     37
    ## 759    Kevin Durant     47
    ## 760    Kevin Durant      4
    ## 761    Kevin Durant     47
    ## 762    Kevin Durant      8
    ## 763    Kevin Durant     19
    ## 764    Kevin Durant     30
    ## 765    Kevin Durant     33
    ## 766    Kevin Durant     39
    ## 767    Kevin Durant     41
    ## 768    Kevin Durant     12
    ## 769    Kevin Durant     36
    ## 770    Kevin Durant      8
    ## 771    Kevin Durant     34
    ## 772    Kevin Durant      2
    ## 773    Kevin Durant     48
    ## 774    Kevin Durant      6
    ## 775    Kevin Durant     40
    ## 776    Kevin Durant     42
    ## 777    Kevin Durant     24
    ## 778    Kevin Durant     21
    ## 779    Kevin Durant     22
    ## 780    Kevin Durant     21
    ## 781    Kevin Durant     16
    ## 782    Kevin Durant      5
    ## 783    Kevin Durant      1
    ## 784    Kevin Durant     36
    ## 785    Kevin Durant     22
    ## 786    Kevin Durant      1
    ## 787    Kevin Durant     17
    ## 788    Kevin Durant     10
    ## 789    Kevin Durant     21
    ## 790    Kevin Durant     43
    ## 791    Kevin Durant      5
    ## 792    Kevin Durant     16
    ## 793    Kevin Durant     12
    ## 794    Kevin Durant     32
    ## 795    Kevin Durant     19
    ## 796    Kevin Durant     17
    ## 797    Kevin Durant     39
    ## 798    Kevin Durant     27
    ## 799    Kevin Durant     23
    ## 800    Kevin Durant      5
    ## 801    Kevin Durant      9
    ## 802    Kevin Durant     10
    ## 803    Kevin Durant     28
    ## 804    Kevin Durant     25
    ## 805    Kevin Durant     19
    ## 806    Kevin Durant     31
    ## 807    Kevin Durant      1
    ## 808    Kevin Durant     28
    ## 809    Kevin Durant     31
    ## 810    Kevin Durant      1
    ## 811    Kevin Durant      5
    ## 812    Kevin Durant      3
    ## 813    Kevin Durant     33
    ## 814    Kevin Durant     15
    ## 815    Kevin Durant     28
    ## 816    Kevin Durant     45
    ## 817    Kevin Durant     24
    ## 818    Kevin Durant     28
    ## 819    Kevin Durant     11
    ## 820    Kevin Durant     28
    ## 821    Kevin Durant     28
    ## 822    Kevin Durant     15
    ## 823    Kevin Durant     25
    ## 824    Kevin Durant     39
    ## 825    Kevin Durant     10
    ## 826    Kevin Durant     15
    ## 827    Kevin Durant     20
    ## 828    Kevin Durant     25
    ## 829    Kevin Durant     13
    ## 830    Kevin Durant     10
    ## 831    Kevin Durant     13
    ## 832    Kevin Durant     17
    ## 833    Kevin Durant     20
    ## 834    Kevin Durant     43
    ## 835    Kevin Durant     41
    ## 836    Kevin Durant     22
    ## 837    Kevin Durant     20
    ## 838    Kevin Durant     28
    ## 839    Kevin Durant     21
    ## 840    Kevin Durant     17
    ## 841    Kevin Durant      1
    ## 842    Kevin Durant      6
    ## 843    Kevin Durant      2
    ## 844    Kevin Durant      9
    ## 845    Kevin Durant     25
    ## 846    Kevin Durant     11
    ## 847    Kevin Durant     18
    ## 848    Kevin Durant     32
    ## 849    Kevin Durant     42
    ## 850    Kevin Durant      1
    ## 851    Kevin Durant     25
    ## 852    Kevin Durant     25
    ## 853    Kevin Durant     19
    ## 854    Kevin Durant     12
    ## 855    Kevin Durant     23
    ## 856    Kevin Durant     40
    ## 857    Kevin Durant     22
    ## 858    Kevin Durant     46
    ## 859    Kevin Durant      1
    ## 860    Kevin Durant     30
    ## 861    Kevin Durant      2
    ## 862    Kevin Durant     17
    ## 863    Kevin Durant      4
    ## 864    Kevin Durant     15
    ## 865    Kevin Durant     26
    ## 866    Kevin Durant     41
    ## 867    Kevin Durant     24
    ## 868    Kevin Durant     30
    ## 869    Kevin Durant     46
    ## 870    Kevin Durant     20
    ## 871    Kevin Durant     30
    ## 872    Kevin Durant     25
    ## 873    Kevin Durant     15
    ## 874    Kevin Durant     38
    ## 875    Kevin Durant     12
    ## 876    Kevin Durant     24
    ## 877    Kevin Durant      6
    ## 878    Kevin Durant      9
    ## 879    Kevin Durant     48
    ## 880    Kevin Durant     14
    ## 881    Kevin Durant     24
    ## 882    Kevin Durant      7
    ## 883    Kevin Durant     40
    ## 884    Kevin Durant     19
    ## 885    Kevin Durant      6
    ## 886    Kevin Durant     44
    ## 887    Kevin Durant     15
    ## 888    Kevin Durant      1
    ## 889    Kevin Durant      2
    ## 890    Kevin Durant     25
    ## 891    Kevin Durant     24
    ## 892    Kevin Durant      7
    ## 893    Kevin Durant     23
    ## 894    Kevin Durant     17
    ## 895    Kevin Durant     29
    ## 896    Kevin Durant     16
    ## 897    Kevin Durant     32
    ## 898    Kevin Durant     46
    ## 899    Kevin Durant     26
    ## 900    Kevin Durant     25
    ## 901    Kevin Durant     19
    ## 902    Kevin Durant     27
    ## 903    Kevin Durant     27
    ## 904    Kevin Durant     11
    ## 905    Kevin Durant     13
    ## 906    Kevin Durant     30
    ## 907    Kevin Durant      5
    ## 908    Kevin Durant      3
    ## 909    Kevin Durant     13
    ## 910    Kevin Durant     31
    ## 911    Kevin Durant     35
    ## 912    Kevin Durant     17
    ## 913    Kevin Durant     28
    ## 914    Kevin Durant     43
    ## 915    Kevin Durant     40
    ## 916    Kevin Durant      9
    ## 917    Kevin Durant     27
    ## 918    Kevin Durant     26
    ## 919    Kevin Durant      2
    ## 920    Kevin Durant     29
    ## 921    Kevin Durant      2
    ## 922    Kevin Durant     23
    ## 923    Kevin Durant     23
    ## 924    Kevin Durant     41
    ## 925    Kevin Durant     40
    ## 926    Kevin Durant      2
    ## 927    Kevin Durant     31
    ## 928    Kevin Durant      3
    ## 929    Kevin Durant     24
    ## 930    Kevin Durant      5
    ## 931    Kevin Durant     12
    ## 932    Kevin Durant     19
    ## 933    Kevin Durant     46
    ## 934    Kevin Durant     26
    ## 935    Kevin Durant     26
    ## 936    Kevin Durant     43
    ## 937    Kevin Durant     30
    ## 938    Kevin Durant     30
    ## 939    Kevin Durant     32
    ## 940    Kevin Durant      6
    ## 941    Kevin Durant     22
    ## 942    Kevin Durant     14
    ## 943    Kevin Durant     24
    ## 944    Kevin Durant     44
    ## 945    Kevin Durant      7
    ## 946    Kevin Durant     31
    ## 947    Kevin Durant     26
    ## 948    Kevin Durant     26
    ## 949    Kevin Durant     24
    ## 950    Kevin Durant     29
    ## 951    Kevin Durant      3
    ## 952    Kevin Durant      9
    ## 953    Kevin Durant     29
    ## 954    Kevin Durant     24
    ## 955    Kevin Durant      1
    ## 956    Kevin Durant      3
    ## 957    Kevin Durant     23
    ## 958    Kevin Durant     30
    ## 959    Kevin Durant     46
    ## 960    Kevin Durant     46
    ## 961    Kevin Durant     14
    ## 962    Kevin Durant     41
    ## 963    Kevin Durant     27
    ## 964    Kevin Durant     17
    ## 965    Kevin Durant     11
    ## 966    Kevin Durant     34
    ## 967    Kevin Durant      6
    ## 968    Kevin Durant     37
    ## 969    Kevin Durant     33
    ## 970    Kevin Durant     38
    ## 971    Kevin Durant      4
    ## 972    Kevin Durant     40
    ## 973    Kevin Durant     22
    ## 974    Kevin Durant     13
    ## 975    Kevin Durant      6
    ## 976    Kevin Durant     35
    ## 977    Kevin Durant     19
    ## 978    Kevin Durant      7
    ## 979    Kevin Durant     12
    ## 980    Kevin Durant     23
    ## 981    Kevin Durant      6
    ## 982    Kevin Durant     28
    ## 983    Kevin Durant     20
    ## 984    Kevin Durant     23
    ## 985    Kevin Durant     19
    ## 986    Kevin Durant     11
    ## 987    Kevin Durant     41
    ## 988    Kevin Durant     31
    ## 989    Kevin Durant      8
    ## 990    Kevin Durant     30
    ## 991   Klay Thompson      3
    ## 992   Klay Thompson     38
    ## 993   Klay Thompson     31
    ## 994   Klay Thompson     40
    ## 995   Klay Thompson     20
    ## 996   Klay Thompson     41
    ## 997   Klay Thompson     41
    ## 998   Klay Thompson     16
    ## 999   Klay Thompson     41
    ## 1000  Klay Thompson     16
    ## 1001  Klay Thompson     26
    ## 1002  Klay Thompson      6
    ## 1003  Klay Thompson     19
    ## 1004  Klay Thompson     28
    ## 1005  Klay Thompson      1
    ## 1006  Klay Thompson     22
    ## 1007  Klay Thompson     32
    ## 1008  Klay Thompson      9
    ## 1009  Klay Thompson     17
    ## 1010  Klay Thompson     20
    ## 1011  Klay Thompson     25
    ## 1012  Klay Thompson      5
    ## 1013  Klay Thompson     24
    ## 1014  Klay Thompson      6
    ## 1015  Klay Thompson     27
    ## 1016  Klay Thompson     10
    ## 1017  Klay Thompson      5
    ## 1018  Klay Thompson      7
    ## 1019  Klay Thompson     39
    ## 1020  Klay Thompson     14
    ## 1021  Klay Thompson     13
    ## 1022  Klay Thompson     22
    ## 1023  Klay Thompson      1
    ## 1024  Klay Thompson     39
    ## 1025  Klay Thompson     17
    ## 1026  Klay Thompson     18
    ## 1027  Klay Thompson      1
    ## 1028  Klay Thompson     26
    ## 1029  Klay Thompson     39
    ## 1030  Klay Thompson     12
    ## 1031  Klay Thompson     23
    ## 1032  Klay Thompson     13
    ## 1033  Klay Thompson     11
    ## 1034  Klay Thompson     37
    ## 1035  Klay Thompson      3
    ## 1036  Klay Thompson     17
    ## 1037  Klay Thompson     18
    ## 1038  Klay Thompson      9
    ## 1039  Klay Thompson     18
    ## 1040  Klay Thompson     18
    ## 1041  Klay Thompson      5
    ## 1042  Klay Thompson     15
    ## 1043  Klay Thompson      5
    ## 1044  Klay Thompson     42
    ## 1045  Klay Thompson     42
    ## 1046  Klay Thompson     42
    ## 1047  Klay Thompson     24
    ## 1048  Klay Thompson     23
    ## 1049  Klay Thompson     28
    ## 1050  Klay Thompson     24
    ## 1051  Klay Thompson     28
    ## 1052  Klay Thompson     23
    ## 1053  Klay Thompson     15
    ## 1054  Klay Thompson     20
    ## 1055  Klay Thompson      5
    ## 1056  Klay Thompson     24
    ## 1057  Klay Thompson     16
    ## 1058  Klay Thompson     14
    ## 1059  Klay Thompson     41
    ## 1060  Klay Thompson     38
    ## 1061  Klay Thompson     32
    ## 1062  Klay Thompson      7
    ## 1063  Klay Thompson     13
    ## 1064  Klay Thompson     14
    ## 1065  Klay Thompson      4
    ## 1066  Klay Thompson     45
    ## 1067  Klay Thompson     17
    ## 1068  Klay Thompson     24
    ## 1069  Klay Thompson      6
    ## 1070  Klay Thompson      4
    ## 1071  Klay Thompson     20
    ## 1072  Klay Thompson     31
    ## 1073  Klay Thompson     33
    ## 1074  Klay Thompson      4
    ## 1075  Klay Thompson     18
    ## 1076  Klay Thompson      8
    ## 1077  Klay Thompson     14
    ## 1078  Klay Thompson     40
    ## 1079  Klay Thompson     41
    ## 1080  Klay Thompson      6
    ## 1081  Klay Thompson     19
    ## 1082  Klay Thompson      7
    ## 1083  Klay Thompson     43
    ## 1084  Klay Thompson     28
    ## 1085  Klay Thompson     14
    ## 1086  Klay Thompson     16
    ## 1087  Klay Thompson     17
    ## 1088  Klay Thompson     23
    ## 1089  Klay Thompson     45
    ## 1090  Klay Thompson     30
    ## 1091  Klay Thompson     15
    ## 1092  Klay Thompson     22
    ## 1093  Klay Thompson      5
    ## 1094  Klay Thompson      6
    ## 1095  Klay Thompson      3
    ## 1096  Klay Thompson     37
    ## 1097  Klay Thompson     39
    ## 1098  Klay Thompson     15
    ## 1099  Klay Thompson     27
    ## 1100  Klay Thompson     45
    ## 1101  Klay Thompson     29
    ## 1102  Klay Thompson      3
    ## 1103  Klay Thompson     22
    ## 1104  Klay Thompson     13
    ## 1105  Klay Thompson     28
    ## 1106  Klay Thompson      4
    ## 1107  Klay Thompson      2
    ## 1108  Klay Thompson     17
    ## 1109  Klay Thompson     39
    ## 1110  Klay Thompson     17
    ## 1111  Klay Thompson     24
    ## 1112  Klay Thompson     24
    ## 1113  Klay Thompson     32
    ## 1114  Klay Thompson     28
    ## 1115  Klay Thompson     23
    ## 1116  Klay Thompson      5
    ## 1117  Klay Thompson     46
    ## 1118  Klay Thompson     32
    ## 1119  Klay Thompson      3
    ## 1120  Klay Thompson     15
    ## 1121  Klay Thompson      8
    ## 1122  Klay Thompson     28
    ## 1123  Klay Thompson      4
    ## 1124  Klay Thompson      1
    ## 1125  Klay Thompson     38
    ## 1126  Klay Thompson     27
    ## 1127  Klay Thompson     26
    ## 1128  Klay Thompson     15
    ## 1129  Klay Thompson     22
    ## 1130  Klay Thompson     25
    ## 1131  Klay Thompson     15
    ## 1132  Klay Thompson     11
    ## 1133  Klay Thompson     25
    ## 1134  Klay Thompson     33
    ## 1135  Klay Thompson     23
    ## 1136  Klay Thompson     40
    ## 1137  Klay Thompson     30
    ## 1138  Klay Thompson     38
    ## 1139  Klay Thompson     37
    ## 1140  Klay Thompson     43
    ## 1141  Klay Thompson     23
    ## 1142  Klay Thompson     18
    ## 1143  Klay Thompson     25
    ## 1144  Klay Thompson     26
    ## 1145  Klay Thompson     31
    ## 1146  Klay Thompson     32
    ## 1147  Klay Thompson     13
    ## 1148  Klay Thompson      5
    ## 1149  Klay Thompson      1
    ## 1150  Klay Thompson     11
    ## 1151  Klay Thompson     33
    ## 1152  Klay Thompson     26
    ## 1153  Klay Thompson     33
    ## 1154  Klay Thompson     38
    ## 1155  Klay Thompson      2
    ## 1156  Klay Thompson     27
    ## 1157  Klay Thompson     39
    ## 1158  Klay Thompson      2
    ## 1159  Klay Thompson     25
    ## 1160  Klay Thompson     38
    ## 1161  Klay Thompson     31
    ## 1162  Klay Thompson     22
    ## 1163  Klay Thompson      7
    ## 1164  Klay Thompson     22
    ## 1165  Klay Thompson      9
    ## 1166  Klay Thompson     21
    ## 1167  Klay Thompson     23
    ## 1168  Klay Thompson     38
    ## 1169  Klay Thompson      5
    ## 1170  Klay Thompson     39
    ## 1171  Klay Thompson     45
    ## 1172  Klay Thompson     12
    ## 1173  Klay Thompson     41
    ## 1174  Klay Thompson     23
    ## 1175  Klay Thompson      3
    ## 1176  Klay Thompson     21
    ## 1177  Klay Thompson      8
    ## 1178  Klay Thompson     26
    ## 1179  Klay Thompson     40
    ## 1180  Klay Thompson     14
    ## 1181  Klay Thompson      4
    ## 1182  Klay Thompson     36
    ## 1183  Klay Thompson      8
    ## 1184  Klay Thompson     45
    ## 1185  Klay Thompson     12
    ## 1186  Klay Thompson     26
    ## 1187  Klay Thompson     22
    ## 1188  Klay Thompson      4
    ## 1189  Klay Thompson     39
    ## 1190  Klay Thompson      5
    ## 1191  Klay Thompson     30
    ## 1192  Klay Thompson     27
    ## 1193  Klay Thompson     31
    ## 1194  Klay Thompson     26
    ## 1195  Klay Thompson     26
    ## 1196  Klay Thompson     24
    ## 1197  Klay Thompson      4
    ## 1198  Klay Thompson     30
    ## 1199  Klay Thompson     40
    ## 1200  Klay Thompson     19
    ## 1201  Klay Thompson      9
    ## 1202  Klay Thompson     28
    ## 1203  Klay Thompson     31
    ## 1204  Klay Thompson      7
    ## 1205  Klay Thompson     12
    ## 1206  Klay Thompson      8
    ## 1207  Klay Thompson      1
    ## 1208  Klay Thompson     23
    ## 1209  Klay Thompson      2
    ## 1210  Klay Thompson      8
    ## 1211  Klay Thompson      4
    ## 1212  Klay Thompson     14
    ## 1213  Klay Thompson     11
    ## 1214  Klay Thompson     28
    ## 1215  Klay Thompson     32
    ## 1216  Klay Thompson     42

``` r
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
