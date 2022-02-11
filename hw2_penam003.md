Homewok 2
================
Felipe Pena Mosca
2/10/2022

– 3.2 Data Description (10pts) –

Please describe these data using inline r, including but not limited to:

The BRFSS questionnaire is designed by a working group of BRFSS state
coordinators and CDC staff. The questionnaire is approved by all state
coordinators. Questionnaire participant is selected randomly and
contacted through telephone.

This dataset includes 134203 observations and 23 variables.

Each observation corresponds to a telephone questionnaire including the
following variables:

-   Year : year that questionnaire took place.

-   Locationabbr : state that participant is living.

-   Locationdesc : county that participant is living.

-   Class: type of question.

-   Topic: specific topic of question.

-   Question: question asked by interviewer.

-   Response: answer provided by participant.

-   Sample size: number of participants with that answer.

-   Data value: proportion of participants with that answer.

-   Confidence limit low: lower limit 95 % confidence interval.

-   Confidence limit high: upper limit 95 % confidence interval.

-   Display order: order in which question was asked to participant.

-   Data value unit: unit in which data value was presented.

-   Data value type: type of variable that represented data value.

-   Data value footnote symbol:fFootnote symbol (if present).

-   Data value footnote: comments regarding footnote symbol.

-   DataSource: variable indicating source of data. In this case
    Behavioral Risk Factors Surveillance System

-   Class-Id: id code for class of question.

-   Topic-Id: id code for specific topic of question.

-   Location-Id: if code for location.

-   Resp-Id: if code for participant.

-   Geo Location: geographic location (latitude and longitude)

``` r
#3.3 Do Data Science (50pts)

# 3.3.1 In the year 2004, which states were observed at 6 locations? (10pts)

df_cn_4 %>%
  group_by(state,year) %>%
      filter("2004" %in% year) %>%
        summarise(observed.more.than.6 = n_distinct(county)>6, 
                  n.locations = n_distinct(county)) %>%
                    filter(TRUE %in% observed.more.than.6) %>%
                      select("state","n.locations")
```

    ## `summarise()` has grouped output by 'state'. You can override using the `.groups` argument.

    ## # A tibble: 7 x 2
    ## # Groups:   state [7]
    ##   state n.locations
    ##   <fct>       <int>
    ## 1 LA              9
    ## 2 MA              8
    ## 3 NC             15
    ## 4 NJ             14
    ## 5 NY              7
    ## 6 OH              8
    ## 7 WA             10

``` r
# 3.3.2 Make a “spaghetti plot” that shows the number of observed locations in each state from 2002 to 2010. Which state has the highest mean number of locations over this period? (10pts)

# States in legend were ordered by mean number of locations

df_cn_4 %>%
  group_by(state,year) %>%
  summarise(count = n_distinct(county)) %>%
ggplot(aes(x = year,y = count,group = state,color = fct_reorder(state, count, mean,.desc=TRUE))) +
  geom_line() + 
  labs(title = "Number observed locations per state by year",
       x = "Year",
       y = "Number observed locations",
       color = "State") +
        scale_fill_brewer(palette = "viridis")
```

    ## `summarise()` has grouped output by 'state'. You can override using the `.groups` argument.

<img src="hw2_penam003_files/figure-gfm/Do Data Science 3.3.1 & 3.3.2-1.png" width="90%" style="display: block; margin: auto;" />

New Jersey had the highest mean number of locations over this period
(Note: states were ordered by mean number of locations (descending
order) during the studied period).

``` r
# 3.3.3 Make a table showing, for the years 2002, 2006, and 2010, the mean and standard deviation of sample size and proportion of Excellent, Good, and Poor responses across locations in MN. (15pts)

df_cn_4 %>%
  filter(year %in% c("2002","2006","2010"),
         state %in% "MN", 
         response %in% c("Excellent","Good","Poor")) %>%
          group_by(year,response) %>%
            summarise(n_mean = mean(sample_size),
                n_sd = sd(sample_size),mean_prop_responses = mean(prop_responses),
                sd_prop_reponses = sd(prop_responses)) %>%
                  knitr::kable(caption = "")
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

| year | response  | n_mean |    n_sd | mean_prop_responses | sd_prop_reponses |
|:-----|:----------|-------:|--------:|--------------------:|-----------------:|
| 2002 | Excellent | 116.00 |  83.275 |               24.15 |           3.5407 |
| 2002 | Good      | 123.75 |  84.263 |               23.95 |           1.0472 |
| 2002 | Poor      |  13.75 |   9.570 |                2.40 |           1.1690 |
| 2006 | Excellent | 122.33 |  72.625 |               23.83 |           2.9872 |
| 2006 | Good      | 137.33 |  85.816 |               26.37 |           0.4509 |
| 2006 | Poor      |  15.00 |   6.928 |                2.30 |           0.9539 |
| 2010 | Excellent | 203.80 | 190.598 |               25.44 |           5.2776 |
| 2010 | Good      | 220.00 | 196.100 |               26.04 |           3.5473 |
| 2010 | Poor      |  27.40 |  27.319 |                2.36 |           0.7701 |

``` r
# 3.3.4 Create a ggplot that communicates the results/trends from the table above and stands on its own (15pts) 

  df_cn_4 %>%
  filter(year %in% c("2002","2006","2010"),
         state %in% "MN",
         response %in% c("Excellent","Good","Poor")) %>%
          group_by(year,response) %>%
            summarise(n_mean=mean(sample_size),
                      n_sd = sd(sample_size),
                      mean_prop_responses = mean(prop_responses),
                      sd_prop_responses = sd(prop_responses)) %>%
                        ggplot(aes(x = year , 
                                   y = mean_prop_responses,
                                   groups = year,
                                   fill = response)) +
                                    geom_bar(position = "dodge",
                                             stat = "identity") +
                                              ylab("Proportion (%)") + 
                                                labs(fill = "Response", 
                                                     title = " Mean response by year in Minnesota",
                                                     x = "Year",
                                                     y = "Mean response (%)") + 
                                                      scale_fill_brewer(palette = "viridis")
```

    ## `summarise()` has grouped output by 'year'. You can override using the `.groups` argument.

<img src="hw2_penam003_files/figure-gfm/Do Data Science 3.3.3 & 3.3.4-1.png" width="90%" style="display: block; margin: auto;" />

Our exploratory analysis suggest that the proportion of responders with
excellent, good or poor overall health does not substantially change
over the studied years (2002, 2006 and 2010) across locations in
Minnesota.
