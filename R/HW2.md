---
title: "HW2"
author: "Erik Andersen"
date: "2022-05-20"
output:
  html_document:
    keep_md: yes
    toc: yes
    toc_float: yes
    toc_depth: 5
  pdf_document:
    toc: yes
header-includes: \usepackage{setspace}\doublespacing
always_allow_html: true
---

### Setup




```r
# Load packages

pacman::p_load(tidyverse, broom, haven, data.table, here, magrittr, stargazer, DT)

# Load data

huairiver_df = read_dta(here("data", "huairiver.dta"))
```

### Question 1

A simple comparison of air pollution across northern and southern cities would not measure the causal effect of the policy because of confounding variables. It is unlikely that people to the north and south of the Huai River would use indoor coal furnaces equally even if they had them. The area to the north of the river is likely colder than the area to the south, so they would have more need for heat. So, the coal furnaces are likely uses more in the north than they would be in the south. A simple comparison of the levels of air pollution would miss this, and so over estimate the effect of the policy. 

The regression discontinuity overcomes this problem because it measures the discontinuous jump right at the river. It is unlikely that the areas separated by just the river have wildly different micro climates, so we would expect (in the absence of the policy) that they would have highly comparable heating needs. So, if there is a discontinuous jump in pollution as the authors found, we can meaningfully attribute that to the policy. The RD design means we are comparing very similar populations in which we would expect to find minimal differences, so when we find a difference, we can interpret it as causal. 

### Question 2

The outcome variable in figure 2 is the PM~10~ concentration in the relevant areas. The assignment variable is the number of degrees north of the Huai River. The outcome measures how polluted the various areas are, while the assignment variable measures whether each city was affected by the policy or not. Cities where the assignment variable is greater than 0 received the treatment because they are north of the river; cites where it is less than 0 are south of the river and so did were not allowed to use indoor coal heating. 

### Question 3

A binned scatter plot is a scatter plot that divides the data into a certain amount of groups, takes the mean of each group and plots the means. This is in contrast to a normal scatter plot where each point on the graph represents one point of data. In the binscatter, each point is the mean of say a 10^th^ of the data. 

It is straightforward to construct a binscatter. First, you choose a number of "bins" to divide your data in to. 10-20 is usually a good number. Next, take the average of each of those 10-20 bins. Now you are left with only 10 data points. Now use your favorite plotting function to make a scatter plot of the new data set, and there you go: a binscatter. 

### Question 4

#### Part a


```r
# Add bins to data

huairiver_df %<>% mutate(bin = cut(dist_huai, breaks = quantile(dist_huai, probs = seq(0,1, by = 0.05), na.rm = T)))

# Make the summarized data frame

huairiver_sum_df = huairiver_df |> 
  group_by(bin) |> 
  summarise(dist = mean(dist_huai, na.rm = T), 
            pm10 = mean(pm10, na.rm = T), 
            north_huai = north_huai)

# Now we can make the binscatter more easily with the collapsed data frame

huairiver_sum_df |> 
  ggplot(aes(x = dist, y = pm10, size = pm10, color = as_factor(north_huai))) +
  geom_point() + 
  geom_smooth(data = filter(huairiver_sum_df, dist <0), method = lm, formula = y~poly(x, 2)) + # Different data so its only for the left side of the graph
  geom_smooth(data = filter(huairiver_sum_df, dist >= 0), method = lm, formula = y~poly(x, 2)) + 
  geom_vline(xintercept = 0, size = 5, color = 'light blue') +
  labs(x = "Degrees North of Huai River",
       y = "PM10 Concentration", 
       title = "PM10 as a Function of Degrees North of the Huai River") + 
  scale_x_continuous(labels = c('15S', "10S", "5S", "0", "5N", "10N", "15N")) +
  scale_y_continuous(breaks = seq(40, 160, by = 20)) + 
  cowplot::theme_cowplot() +
  theme(legend.position = 'none')  
```

![](HW2_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Part b

##### Temperature


```r
# Add temp, precipitation, and wind speed to dataframe

huairiver_sum_df = huairiver_df |> 
  group_by(bin) |> 
  summarise(dist = mean(dist_huai, na.rm = T),
            pm10 = mean(pm10, na.rm = T),
            temp = mean(temp, na.rm = T),
            pricip = mean(prcp, na.rm = T),
            wind = mean(wspd, na.rm =T),
            north_huai = north_huai)

# Now we can make the binscatter more easily with the collapsed data frame

huairiver_sum_df |> 
  ggplot(aes(x = dist, y = temp, color = as_factor(north_huai))) +
  geom_point() + 
  geom_smooth(data = filter(huairiver_sum_df, dist <0), method = lm, formula = y~poly(x)) + # Different data so its only for the left side of the graph
  geom_smooth(data = filter(huairiver_sum_df, dist >= 0), method = lm, formula = y~poly(x)) + 
  geom_vline(xintercept = 0, size = 5, color = 'light blue') +
  labs(x = "Degrees North of Huai River",
       y = "Temperature", 
       title = "Temperature as a Function of Degrees North of the Huai River") + 
  scale_x_continuous(labels = c('15S', "10S", "5S", "0", "5N", "10N", "15N")) +
  scale_y_continuous(breaks = seq(40, 160, by = 20)) + 
  cowplot::theme_cowplot() +
  theme(legend.position = 'none')  
```

![](HW2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##### Pricipitation


```r
huairiver_sum_df |> 
  ggplot(aes(x = dist, y = pricip, color = as_factor(north_huai))) +
  geom_point() + 
  geom_smooth(data = filter(huairiver_sum_df, dist <0), method = lm, formula = y~poly(x)) + # Different data so its only for the left side of the graph
  geom_smooth(data = filter(huairiver_sum_df, dist >= 0), method = lm, formula = y~poly(x)) + 
  geom_vline(xintercept = 0, size = 5, color = 'light blue') +
  labs(x = "Degrees North of Huai River",
       y = "Precipitation", 
       title = "Precipitation as a Function of Degrees North of the Huai River") + 
  scale_x_continuous(labels = c('15S', "10S", "5S", "0", "5N", "10N", "15N")) +
  scale_y_continuous(breaks = seq(40, 160, by = 20)) + 
  cowplot::theme_cowplot() +
  theme(legend.position = 'none')  
```

![](HW2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##### Wind Speed


```r
huairiver_sum_df |> 
  ggplot(aes(x = dist, y = wind, color = as_factor(north_huai))) +
  geom_point() + 
  geom_smooth(data = filter(huairiver_sum_df, dist <0), method = lm, formula = y~poly(x)) + # Different data so its only for the left side of the graph
  geom_smooth(data = filter(huairiver_sum_df, dist >= 0), method = lm, formula = y~poly(x)) + 
  geom_vline(xintercept = 0, size = 5, color = 'light blue') +
  labs(x = "Degrees North of Huai River",
       y = "Wind Speed", 
       title = "Wind Speed as a Function of Degrees North of the Huai River") + 
  scale_x_continuous(labels = c('15S', "10S", "5S", "0", "5N", "10N", "15N")) +
  scale_y_continuous(breaks = seq(40, 160, by = 20)) + 
  cowplot::theme_cowplot() +
  theme(legend.position = 'none')  
```

![](HW2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Question 5



### Question 6

The identification assumption for a regression discontinuity design is that the treatment, in this case the Huai River policy, is the only reason for discrete jumps in the outcome variable, in this case PM^10^ concentration around the cutoff. 

The plots from 4b are mostly consistent with that, but not entirely. If the identifying assumption holds, we would expect to see no significant jump around the zero degree north line for each of the three variables. Temperature behaves exactly as we expect; the line of best fit probably wouldn't change at all if we made it fit all the points instead of having two seperate ones. Wind speed also mostly fits the assumption. The lines don't match up exactly as they do for temperature, but that is likely due to noise. Precipitation on the other hand, doesn't match the assumption. There is a large jump at the river with much higher rain fall on the south side. This is particularly concerning in this example because rain can directly affect air pollution levels. Rain can collect the particulate matter on its journey to the ground, and clean the air. This could definitely affect our estimates of particulate matter changing because of the policy. 

### Question 7

The manipulation test checks if the people being studied can sort their location relative to the assignment variable. In this case that would test if people can move across the river if they chose to do so. At the beginning of the Huai River policy, a manipulation test would not have been necessary because China had cracked down on migration; people were stuck where they were born to a large extent. However, those restrictions eventually loosened, so it was possible for people to move around. Because of this, I think a manipulation test should be done here to make sure people aren't sorting around the river. 

To run the manipulation test, I have provided a histogram below showing the density relative to the distance from the Huai River


```r
# Histogram manipulation test

huairiver_df |> 
  ggplot(aes(x = dist_huai)) + 
  geom_histogram(stat = 'density', bins = 6) + 
  labs(x = 'Degrees North of the Huai River',
       y = 'Density',
       title = 'Manipulation Test') +
  cowplot::theme_cowplot()
```

![](HW2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The histogram is smooth, so there is no apparent evidence for manipulation around the cutoff.

### Question 8

#### Part a







































