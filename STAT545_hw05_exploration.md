STAT545\_hw05\_exploration
================

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
library(RColorBrewer)
library(pkgconfig)
```

# Part 1: Factor management

Let’s compare the use of filter() vs droplevels() upon removing
“Oceania” from the gapminder data set.

``` r
gap_filter <- gapminder %>% 
  filter(continent != "Oceania")

gap_drop <- gap_filter %>% 
  droplevels()

str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
str(gap_filter)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

``` r
str(gap_drop)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

*Comments:* Use of filter() does not effect a change in the number of
levels in country (142) or continent (5) factors, whereas droplevels()
displays only the observed levels in country (140) and continent (4)
factors. As expected, the dropped level in continent is “Oceania”;
correspondingly, the dropped levels in country are “Australia” and “New
Zealand”.

Next, let’s compare the use of arrange() and fct\_reorder() to
reorganize continent by GDP per capita.

``` r
#With arrange()
gap_A <- gap_drop %>% 
  arrange(continent, gdpPercap)

#Reordered factors
gap_R <- gap_drop %>% 
  mutate(continent = fct_reorder(continent, gdpPercap, .fun = max))

#Reordered factors and with arrange()
gap_RA <- gap_R %>% 
  arrange(continent, gdpPercap)


head(gap_drop)
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

``` r
head(gap_A)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp      pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Congo, Dem. Rep. Africa     2002    45.0 55379852      241.
    ## 2 Congo, Dem. Rep. Africa     2007    46.5 64606759      278.
    ## 3 Lesotho          Africa     1952    42.1   748747      299.
    ## 4 Guinea-Bissau    Africa     1952    32.5   580653      300.
    ## 5 Congo, Dem. Rep. Africa     1997    42.6 47798986      312.
    ## 6 Eritrea          Africa     1952    35.9  1438760      329.

``` r
head(gap_R)
```

    ## # A tibble: 6 x 6
    ##   country     continent  year lifeExp      pop gdpPercap
    ##   <fct>       <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Afghanistan Asia       1952    28.8  8425333      779.
    ## 2 Afghanistan Asia       1957    30.3  9240934      821.
    ## 3 Afghanistan Asia       1962    32.0 10267083      853.
    ## 4 Afghanistan Asia       1967    34.0 11537966      836.
    ## 5 Afghanistan Asia       1972    36.1 13079460      740.
    ## 6 Afghanistan Asia       1977    38.4 14880372      786.

``` r
head(gap_RA)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp      pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Congo, Dem. Rep. Africa     2002    45.0 55379852      241.
    ## 2 Congo, Dem. Rep. Africa     2007    46.5 64606759      278.
    ## 3 Lesotho          Africa     1952    42.1   748747      299.
    ## 4 Guinea-Bissau    Africa     1952    32.5   580653      300.
    ## 5 Congo, Dem. Rep. Africa     1997    42.6 47798986      312.
    ## 6 Eritrea          Africa     1952    35.9  1438760      329.

``` r
levels(gap_drop$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
levels(gap_A$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
levels(gap_R$continent)
```

    ## [1] "Africa"   "Americas" "Europe"   "Asia"

``` r
levels(gap_RA$continent)
```

    ## [1] "Africa"   "Americas" "Europe"   "Asia"

*Comments:* In the head of the tables displayed above, it is seen that
the use of arrange() reorganizes GDP per capita in ascending order
within each continent grouping. The use of fct\_reorder() has no
observable effect on the table itself. The use of both functions leads
only to the observable effects of arrange().

Usings levels() to check the factor levels, there is no difference after
using arrange(). After fct\_reorder(), the order of the factors changes.
In this case, Europe has the second highest maximum, so fct\_reorder()
has changed its position accordingly from the default alphabetical
ordering of contient factors.

Let’s check the
plots:

``` r
gap_drop %>% ggplot(aes(continent, gdpPercap)) + geom_boxplot() + scale_y_log10()
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
gap_A %>% ggplot(aes(continent, gdpPercap)) + geom_boxplot() + scale_y_log10()
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
gap_R %>% ggplot(aes(continent, gdpPercap)) + geom_boxplot() + scale_y_log10()
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
gap_RA %>% ggplot(aes(continent, gdpPercap)) + geom_boxplot() + scale_y_log10()
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

*Comments:* The use of arrange() only operates on the table output and
has no effect on the plot. This makes sense intuitively, as the data
points are still the same and thus the plot output will be the same, so
long as the grouping is still by continent and plotted against GDP per
capita. The use of fct\_reorder() on the other hand does lead to an
observable difference in the plot. Here, I called the function to
reorder by the summary statistic maximum, so that the boxplot is
arranged in order of increasing maxima. The ordering of the factors is
the same as that observed using levels() above.

# Part 2: Files I/O

Let’s output gap\_RA tibble to a CSV file and then read it back in
again.

``` r
write_csv(gap_RA, "gap_RA.csv")

check_gap_RA <- read_csv("gap_RA.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

``` r
head(gap_RA)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp      pop gdpPercap
    ##   <fct>            <fct>     <int>   <dbl>    <int>     <dbl>
    ## 1 Congo, Dem. Rep. Africa     2002    45.0 55379852      241.
    ## 2 Congo, Dem. Rep. Africa     2007    46.5 64606759      278.
    ## 3 Lesotho          Africa     1952    42.1   748747      299.
    ## 4 Guinea-Bissau    Africa     1952    32.5   580653      300.
    ## 5 Congo, Dem. Rep. Africa     1997    42.6 47798986      312.
    ## 6 Eritrea          Africa     1952    35.9  1438760      329.

``` r
head(check_gap_RA)
```

    ## # A tibble: 6 x 6
    ##   country          continent  year lifeExp      pop gdpPercap
    ##   <chr>            <chr>     <int>   <dbl>    <int>     <dbl>
    ## 1 Congo, Dem. Rep. Africa     2002    45.0 55379852      241.
    ## 2 Congo, Dem. Rep. Africa     2007    46.5 64606759      278.
    ## 3 Lesotho          Africa     1952    42.1   748747      299.
    ## 4 Guinea-Bissau    Africa     1952    32.5   580653      300.
    ## 5 Congo, Dem. Rep. Africa     1997    42.6 47798986      312.
    ## 6 Eritrea          Africa     1952    35.9  1438760      329.

*Comments:* The head of gap\_RA and the newly read-in CSV stored in
check\_gap\_RA look to be the same. Success\!

# Part 2: Visualization design

*Preamble:* Let’s revisit a plot made using `ggplot` from a previous
assignment and make it even better. I have chosen to use the following
plot: Weighted mean of life expectancy (by population) across
continents.

``` r
#Set up parameters of the plot by finding the weighted mean by population, and then storing in a new tibble, A.
A <- gapminder %>% 
      filter(year == 1952 | year == 2002 | year == 1977) %>% 
      group_by(continent, country, year) %>% #Grouping by year in order to facet plot later
      mutate(wm = round(weighted.mean(lifeExp, pop, na.rm = FALSE), digits = 1)) %>% 
      transmute(lifeExp, pop, wm) #transmute removes all columns other than the ones specified

#Create a ggplot (this is the original)

P <- ggplot(A, aes(x = continent, y = wm, color = continent)) +
  geom_jitter(alpha = 0.5) + 
  labs(title = "Figure 2: Weighted mean of life expectancy (by population) across continents", 
       x = "Continent", y = "Life expectancy") + #Add title and axis labels
  facet_grid(.~year) #Facet by year. In this case, 'A' has three years of data.

P
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
#Create an improved ggplot

P_new <- ggplot(A, aes(x = continent, y = wm, color = continent)) +
  geom_jitter() + 
  labs(title = "Figure 2: Weighted mean of life expectancy (by population) across continents", 
       x = "Continent", y = "Life expectancy") + #Add title and axis labels
  facet_grid(.~year) +
  theme_classic() + #Removes grid lines for a cleaner look
  theme(axis.text.x  = element_text(angle=65, vjust=0.5, size=8),
        axis.text = element_text(size = 10),
        strip.background = element_rect(colour = "white"),
        strip.text = element_text(size = 14)
        ) +
  scale_y_continuous(breaks = 1:20 * 5) +
  scale_colour_brewer(palette = "Set1")

P_new
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
#Convert this to a `plotly` plot:
 
ggplotly(P_new)
```

<!--html_preserve-->

<div id="htmlwidget-9eef4c0c3670999408f6" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-9eef4c0c3670999408f6">{"x":{"data":[{"x":[0.688971867226064,1.14711219798774,0.616163690946996,1.09312340244651,1.1450322560966,1.32190565895289,0.920875269919634,0.776173396222293,1.37263402938843,1.05139203928411,1.03868812248111,1.35552176106721,0.63626317102462,0.726554750278592,1.33607893437147,1.28371177464724,0.972145356982946,1.31725042257458,0.877536929212511,1.13173344340175,0.860793824866414,0.80215899925679,0.692017663642764,1.27030186764896,0.874609772674739,1.05680414922535,0.876843109540641,1.36215753201395,0.869906886853278,1.07104595918208,1.1708458552137,1.23595310915262,0.966826556995511,0.773630320839584,1.15910569243133,0.613063021935523,0.861727226339281,0.749920330941677,1.21181975789368,0.867011186294258,1.29400673862547,1.19446386154741,1.29699896965176,1.3423039034009,0.925907237268984,0.95907834880054,1.26534856595099,0.606547860614955,1.21999871879816,0.85746214967221,1.0229758599773,0.868163382448256],"y":[43.1211888341978,30.019654015433,38.1602618972585,47.6395559347235,31.9874605764262,38.9870688669756,38.473465836253,35.4984013661928,38.10907332059,40.6960869173147,39.0958410027623,42.1024862827547,40.5337441930175,34.8274141375534,41.9098748146184,34.5367969181575,35.9030153286271,34.0987220670097,37.0271593957208,29.9649302694574,43.0608668379672,33.5987375428528,32.4837295411341,42.3301302320883,42.0623028821871,38.5213898998685,42.7070018793829,36.6649608931504,36.265315168798,33.7102983852103,40.5090416722186,51.0125050075725,42.924252454415,31.2920908178948,41.6897708106786,37.3753516993299,36.2904661903717,52.7332226966508,39.9994032744318,46.4780745999888,37.2662298062444,30.3216401665285,32.9679612334259,44.9775685458258,38.6225701658614,41.4040621935949,41.1879099787772,38.6245382151566,44.5678558021225,40.0188247299381,42.0282331977785,48.4786651298404],"text":["continent: Africa<br />wm: 43.1<br />continent: Africa","continent: Africa<br />wm: 30.0<br />continent: Africa","continent: Africa<br />wm: 38.2<br />continent: Africa","continent: Africa<br />wm: 47.6<br />continent: Africa","continent: Africa<br />wm: 32.0<br />continent: Africa","continent: Africa<br />wm: 39.0<br />continent: Africa","continent: Africa<br />wm: 38.5<br />continent: Africa","continent: Africa<br />wm: 35.5<br />continent: Africa","continent: Africa<br />wm: 38.1<br />continent: Africa","continent: Africa<br />wm: 40.7<br />continent: Africa","continent: Africa<br />wm: 39.1<br />continent: Africa","continent: Africa<br />wm: 42.1<br />continent: Africa","continent: Africa<br />wm: 40.5<br />continent: Africa","continent: Africa<br />wm: 34.8<br />continent: Africa","continent: Africa<br />wm: 41.9<br />continent: Africa","continent: Africa<br />wm: 34.5<br />continent: Africa","continent: Africa<br />wm: 35.9<br />continent: Africa","continent: Africa<br />wm: 34.1<br />continent: Africa","continent: Africa<br />wm: 37.0<br />continent: Africa","continent: Africa<br />wm: 30.0<br />continent: Africa","continent: Africa<br />wm: 43.1<br />continent: Africa","continent: Africa<br />wm: 33.6<br />continent: Africa","continent: Africa<br />wm: 32.5<br />continent: Africa","continent: Africa<br />wm: 42.3<br />continent: Africa","continent: Africa<br />wm: 42.1<br />continent: Africa","continent: Africa<br />wm: 38.5<br />continent: Africa","continent: Africa<br />wm: 42.7<br />continent: Africa","continent: Africa<br />wm: 36.7<br />continent: Africa","continent: Africa<br />wm: 36.3<br />continent: Africa","continent: Africa<br />wm: 33.7<br />continent: Africa","continent: Africa<br />wm: 40.5<br />continent: Africa","continent: Africa<br />wm: 51.0<br />continent: Africa","continent: Africa<br />wm: 42.9<br />continent: Africa","continent: Africa<br />wm: 31.3<br />continent: Africa","continent: Africa<br />wm: 41.7<br />continent: Africa","continent: Africa<br />wm: 37.4<br />continent: Africa","continent: Africa<br />wm: 36.3<br />continent: Africa","continent: Africa<br />wm: 52.7<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa","continent: Africa<br />wm: 46.5<br />continent: Africa","continent: Africa<br />wm: 37.3<br />continent: Africa","continent: Africa<br />wm: 30.3<br />continent: Africa","continent: Africa<br />wm: 33.0<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 38.6<br />continent: Africa","continent: Africa<br />wm: 41.4<br />continent: Africa","continent: Africa<br />wm: 41.2<br />continent: Africa","continent: Africa<br />wm: 38.6<br />continent: Africa","continent: Africa<br />wm: 44.6<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 48.5<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.14857557397336,1.21152616683394,1.02543559484184,0.734437796473503,1.25912209674716,0.726745018176734,1.38332865200937,0.781000493653119,1.19006295967847,0.898015261441469,1.38355838395655,1.12390995640308,0.955846269242466,0.85294019728899,0.672203598730266,1.24074850510806,1.2112678155303,0.810838645882905,0.958525814302266,0.975754081457853,1.10615448858589,0.657175978273153,0.963510776683688,1.00815478600562,0.95573291182518,1.25192863214761,0.809345882944763,1.11914579309523,1.13860820531845,0.955179779790342,0.928484695591033,0.776036533154547,1.14553676005453,0.945223493687809,1.26251103188843,1.15689897276461,0.667167531698942,0.748008077219129,0.768076644837856,0.81897999048233,1.30068421810865,1.38837023396045,0.649840558320284,0.685811100713909,1.06237449664623,0.885123798809946,0.619917292520404,0.79290187805891,1.01409492883831,1.10722580682486,1.16827683579177,0.758420901931822],"y":[58.0389839690737,39.5345147247799,49.2206643337011,59.2838516123407,46.0727044997737,45.8738378604315,49.4277017285861,46.8129439391755,47.3920062253065,50.9272766831703,47.7681564301625,55.5859168517962,52.3659035618044,46.4779375851154,53.2887284753844,41.998926496692,44.4903049625456,44.4776850572974,52.8355917467363,41.7689402988367,51.8398762634396,40.807276839409,37.5202377555333,56.175444653593,52.1851883645356,43.8015366300195,57.4321371658705,46.9052696119435,43.7771840584464,41.695013703797,50.935833071433,64.8650591026992,55.7075167885982,42.5036165548302,56.4273059016466,41.2944605665468,44.530272487253,67.0603412313759,45.0290262794495,58.5321003007144,48.8618994460255,36.764542352315,42.0161293148436,55.4986209304072,47.8078249357827,52.504407169465,49.8626702934504,52.8716696476378,59.7991914227232,50.4174001927674,51.3931998024508,57.7258508965559],"text":["continent: Africa<br />wm: 58.0<br />continent: Africa","continent: Africa<br />wm: 39.5<br />continent: Africa","continent: Africa<br />wm: 49.2<br />continent: Africa","continent: Africa<br />wm: 59.3<br />continent: Africa","continent: Africa<br />wm: 46.1<br />continent: Africa","continent: Africa<br />wm: 45.9<br />continent: Africa","continent: Africa<br />wm: 49.4<br />continent: Africa","continent: Africa<br />wm: 46.8<br />continent: Africa","continent: Africa<br />wm: 47.4<br />continent: Africa","continent: Africa<br />wm: 50.9<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 55.6<br />continent: Africa","continent: Africa<br />wm: 52.4<br />continent: Africa","continent: Africa<br />wm: 46.5<br />continent: Africa","continent: Africa<br />wm: 53.3<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 52.8<br />continent: Africa","continent: Africa<br />wm: 41.8<br />continent: Africa","continent: Africa<br />wm: 51.8<br />continent: Africa","continent: Africa<br />wm: 40.8<br />continent: Africa","continent: Africa<br />wm: 37.5<br />continent: Africa","continent: Africa<br />wm: 56.2<br />continent: Africa","continent: Africa<br />wm: 52.2<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 57.4<br />continent: Africa","continent: Africa<br />wm: 46.9<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 41.7<br />continent: Africa","continent: Africa<br />wm: 50.9<br />continent: Africa","continent: Africa<br />wm: 64.9<br />continent: Africa","continent: Africa<br />wm: 55.7<br />continent: Africa","continent: Africa<br />wm: 42.5<br />continent: Africa","continent: Africa<br />wm: 56.4<br />continent: Africa","continent: Africa<br />wm: 41.3<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 67.1<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 58.5<br />continent: Africa","continent: Africa<br />wm: 48.9<br />continent: Africa","continent: Africa<br />wm: 36.8<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 55.5<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 52.5<br />continent: Africa","continent: Africa<br />wm: 49.9<br />continent: Africa","continent: Africa<br />wm: 52.9<br />continent: Africa","continent: Africa<br />wm: 59.8<br />continent: Africa","continent: Africa<br />wm: 50.4<br />continent: Africa","continent: Africa<br />wm: 51.4<br />continent: Africa","continent: Africa<br />wm: 57.7<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.16810638252646,0.728161242231727,1.22177297212183,1.36911570671946,1.17057825662196,1.28212967347354,1.17103457301855,0.733372574299574,1.25147420149297,1.00382816940546,0.763115117698908,1.06062159072608,1.0015746396035,0.65066931322217,0.890834692306817,0.77751720957458,1.27226396352053,0.708591471239924,0.917776061408222,0.676436217874289,0.736045588739216,1.08912984877825,0.814306571520865,0.737720094062388,0.820501320250332,1.04232975486666,1.19748432077467,1.39807186350226,1.06157411374152,0.707958403415978,0.659680844843388,0.709038170240819,0.762960953637958,0.680293725989759,0.671802422404289,0.644172651134431,1.016450888291,1.36322478335351,0.725621676817536,0.940860989503562,0.752770850621164,1.28161309249699,1.13681167624891,1.12346468903124,0.953031952865422,1.37095134537667,1.25131435841322,1.16064080372453,0.823824783600867,1.18278241492808,1.15641745999455,0.610775006748736],"y":[70.9760944121145,40.9785509787686,54.3719015012868,46.6211372998171,50.5881036446244,47.4377336885408,49.8630770502239,43.3046736056358,50.471596581284,63.0093814227357,44.9800918102637,53.0108070633002,46.7806223078072,53.3699179526977,69.7775554173253,49.3209459112585,55.1987855686434,50.696489222683,56.8344384595193,57.9974097152986,58.4707080853917,53.686830722522,45.5271247817762,51.035477514118,44.6078224359453,43.7910759118386,72.7187402737886,57.2706079752557,45.026237196885,51.7882534005679,62.228952574674,71.9800960328989,69.6189161923341,43.9646650737338,51.5346668604948,54.5116183309071,46.6349882220477,75.7147504651547,43.4226734566875,64.2669572306983,61.5903683475032,40.9896696212702,45.9119564144313,53.368495427724,56.3982338071056,43.9209908241779,49.6924103553221,57.5818111479841,72.9819037058204,47.7900230288133,39.1987092925049,39.9666683853604],"text":["continent: Africa<br />wm: 71.0<br />continent: Africa","continent: Africa<br />wm: 41.0<br />continent: Africa","continent: Africa<br />wm: 54.4<br />continent: Africa","continent: Africa<br />wm: 46.6<br />continent: Africa","continent: Africa<br />wm: 50.6<br />continent: Africa","continent: Africa<br />wm: 47.4<br />continent: Africa","continent: Africa<br />wm: 49.9<br />continent: Africa","continent: Africa<br />wm: 43.3<br />continent: Africa","continent: Africa<br />wm: 50.5<br />continent: Africa","continent: Africa<br />wm: 63.0<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 53.0<br />continent: Africa","continent: Africa<br />wm: 46.8<br />continent: Africa","continent: Africa<br />wm: 53.4<br />continent: Africa","continent: Africa<br />wm: 69.8<br />continent: Africa","continent: Africa<br />wm: 49.3<br />continent: Africa","continent: Africa<br />wm: 55.2<br />continent: Africa","continent: Africa<br />wm: 50.7<br />continent: Africa","continent: Africa<br />wm: 56.8<br />continent: Africa","continent: Africa<br />wm: 58.0<br />continent: Africa","continent: Africa<br />wm: 58.5<br />continent: Africa","continent: Africa<br />wm: 53.7<br />continent: Africa","continent: Africa<br />wm: 45.5<br />continent: Africa","continent: Africa<br />wm: 51.0<br />continent: Africa","continent: Africa<br />wm: 44.6<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 72.7<br />continent: Africa","continent: Africa<br />wm: 57.3<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 51.8<br />continent: Africa","continent: Africa<br />wm: 62.2<br />continent: Africa","continent: Africa<br />wm: 72.0<br />continent: Africa","continent: Africa<br />wm: 69.6<br />continent: Africa","continent: Africa<br />wm: 44.0<br />continent: Africa","continent: Africa<br />wm: 51.5<br />continent: Africa","continent: Africa<br />wm: 54.5<br />continent: Africa","continent: Africa<br />wm: 46.6<br />continent: Africa","continent: Africa<br />wm: 75.7<br />continent: Africa","continent: Africa<br />wm: 43.4<br />continent: Africa","continent: Africa<br />wm: 64.3<br />continent: Africa","continent: Africa<br />wm: 61.6<br />continent: Africa","continent: Africa<br />wm: 41.0<br />continent: Africa","continent: Africa<br />wm: 45.9<br />continent: Africa","continent: Africa<br />wm: 53.4<br />continent: Africa","continent: Africa<br />wm: 56.4<br />continent: Africa","continent: Africa<br />wm: 43.9<br />continent: Africa","continent: Africa<br />wm: 49.7<br />continent: Africa","continent: Africa<br />wm: 57.6<br />continent: Africa","continent: Africa<br />wm: 73.0<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 39.2<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.91763306651264,1.76531700976193,1.76727534532547,1.81605634614825,1.89763185419142,1.84652656223625,2.32404470965266,1.84996055234224,1.82930997740477,1.82419451214373,2.38608444277197,2.08527088109404,2.09245519451797,2.3497529739514,2.14825457781553,1.67401282805949,1.97497530505061,1.72978553846478,2.37280025053769,2.25091971512884,2.36900091897696,2.07755383234471,1.62711355052888,2.27759834658355,1.67978094238788],"y":[62.4810912088677,40.3780354212783,50.9330830892548,68.8331084824726,54.6864691578969,50.5884168730304,57.1603269811906,59.4181712176464,45.889233128354,48.3847995693237,45.2751279612072,42.0261913070269,37.6368694369681,41.9150257606246,58.4910493486561,50.7695932895876,42.3232451130636,55.2098472294025,62.6116757990606,43.8845997447893,64.2999991999753,59.1050309136882,68.4172333087958,66.0689253121614,55.1074339549616],"text":["continent: Americas<br />wm: 62.5<br />continent: Americas","continent: Americas<br />wm: 40.4<br />continent: Americas","continent: Americas<br />wm: 50.9<br />continent: Americas","continent: Americas<br />wm: 68.8<br />continent: Americas","continent: Americas<br />wm: 54.7<br />continent: Americas","continent: Americas<br />wm: 50.6<br />continent: Americas","continent: Americas<br />wm: 57.2<br />continent: Americas","continent: Americas<br />wm: 59.4<br />continent: Americas","continent: Americas<br />wm: 45.9<br />continent: Americas","continent: Americas<br />wm: 48.4<br />continent: Americas","continent: Americas<br />wm: 45.3<br />continent: Americas","continent: Americas<br />wm: 42.0<br />continent: Americas","continent: Americas<br />wm: 37.6<br />continent: Americas","continent: Americas<br />wm: 41.9<br />continent: Americas","continent: Americas<br />wm: 58.5<br />continent: Americas","continent: Americas<br />wm: 50.8<br />continent: Americas","continent: Americas<br />wm: 42.3<br />continent: Americas","continent: Americas<br />wm: 55.2<br />continent: Americas","continent: Americas<br />wm: 62.6<br />continent: Americas","continent: Americas<br />wm: 43.9<br />continent: Americas","continent: Americas<br />wm: 64.3<br />continent: Americas","continent: Americas<br />wm: 59.1<br />continent: Americas","continent: Americas<br />wm: 68.4<br />continent: Americas","continent: Americas<br />wm: 66.1<br />continent: Americas","continent: Americas<br />wm: 55.1<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.75950626507401,1.88875534441322,1.82698368355632,2.28161041680723,1.79705428797752,1.77795877810568,2.31678581591696,1.97086279224604,1.79262011256069,2.02126184329391,2.23273247033358,1.76087255403399,2.1939994269982,2.01345195509493,1.63994134645909,1.81101410798728,2.07396644353867,2.05547481440008,2.05360758658499,1.96068470589817,2.24007352031767,1.93033682238311,1.98090887758881,2.09891938548535,1.6545268734917],"y":[68.5302843024768,50.027011124678,61.4788660345599,74.1965447206795,67.1360511060804,63.7636730499007,70.8009572257847,72.6102940849028,61.8343035348132,61.3387995095365,56.7089162977226,55.9891467076913,49.9385448684916,57.3886544981971,70.065725007914,65.0238067708164,57.485682036262,68.6817817337625,66.3689166663028,58.3847161250189,73.3947548996098,68.3097881825641,73.434130486846,69.4927524140291,67.4765639432706],"text":["continent: Americas<br />wm: 68.5<br />continent: Americas","continent: Americas<br />wm: 50.0<br />continent: Americas","continent: Americas<br />wm: 61.5<br />continent: Americas","continent: Americas<br />wm: 74.2<br />continent: Americas","continent: Americas<br />wm: 67.1<br />continent: Americas","continent: Americas<br />wm: 63.8<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 72.6<br />continent: Americas","continent: Americas<br />wm: 61.8<br />continent: Americas","continent: Americas<br />wm: 61.3<br />continent: Americas","continent: Americas<br />wm: 56.7<br />continent: Americas","continent: Americas<br />wm: 56.0<br />continent: Americas","continent: Americas<br />wm: 49.9<br />continent: Americas","continent: Americas<br />wm: 57.4<br />continent: Americas","continent: Americas<br />wm: 70.1<br />continent: Americas","continent: Americas<br />wm: 65.0<br />continent: Americas","continent: Americas<br />wm: 57.5<br />continent: Americas","continent: Americas<br />wm: 68.7<br />continent: Americas","continent: Americas<br />wm: 66.4<br />continent: Americas","continent: Americas<br />wm: 58.4<br />continent: Americas","continent: Americas<br />wm: 73.4<br />continent: Americas","continent: Americas<br />wm: 68.3<br />continent: Americas","continent: Americas<br />wm: 73.4<br />continent: Americas","continent: Americas<br />wm: 69.5<br />continent: Americas","continent: Americas<br />wm: 67.5<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.68601466342807,2.03506007324904,2.14890412949026,2.33800658024848,1.81382936257869,2.18036926016212,1.63249722886831,1.77894564401358,2.27028318718076,2.34615157935768,1.66295754220337,2.08921651337296,1.77478472907096,1.94425881132483,2.17156609259546,1.69403647463769,2.38607015069574,2.13108115140349,2.23229869734496,1.91281208898872,2.03849779833108,1.99312274437398,2.36722749341279,2.23092043306679,1.63078262321651],"y":[74.3170533558726,63.8777923641354,71.0009811168537,79.7730495779961,77.8842519670539,71.7021014119685,78.0767079173028,77.2264739135653,70.7721387876943,74.2072566758469,70.6893684167042,69.0251716936193,58.0859470323101,68.6358416549303,71.969541680757,74.8712676727772,70.8309069247544,74.6982018585503,70.767308131922,69.9055657509714,77.7789370211773,68.9979865270294,77.3038644086756,75.2943796549551,72.7760633516684],"text":["continent: Americas<br />wm: 74.3<br />continent: Americas","continent: Americas<br />wm: 63.9<br />continent: Americas","continent: Americas<br />wm: 71.0<br />continent: Americas","continent: Americas<br />wm: 79.8<br />continent: Americas","continent: Americas<br />wm: 77.9<br />continent: Americas","continent: Americas<br />wm: 71.7<br />continent: Americas","continent: Americas<br />wm: 78.1<br />continent: Americas","continent: Americas<br />wm: 77.2<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 74.2<br />continent: Americas","continent: Americas<br />wm: 70.7<br />continent: Americas","continent: Americas<br />wm: 69.0<br />continent: Americas","continent: Americas<br />wm: 58.1<br />continent: Americas","continent: Americas<br />wm: 68.6<br />continent: Americas","continent: Americas<br />wm: 72.0<br />continent: Americas","continent: Americas<br />wm: 74.9<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 74.7<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 69.9<br />continent: Americas","continent: Americas<br />wm: 77.8<br />continent: Americas","continent: Americas<br />wm: 69.0<br />continent: Americas","continent: Americas<br />wm: 77.3<br />continent: Americas","continent: Americas<br />wm: 75.3<br />continent: Americas","continent: Americas<br />wm: 72.8<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2.97203379720449,2.91195215042681,3.34933867286891,2.97962931804359,3.22734383586794,3.36536344345659,2.66661409288645,3.00972939636558,3.14779484309256,2.76712173204869,2.80943631660193,2.7025761898607,2.79845608677715,3.35856367219239,2.877349861525,3.02924044113606,2.86628731060773,3.14837770145386,3.31279876697808,3.28396163303405,2.82315160930157,3.06177634950727,2.80485918018967,2.65832184590399,2.838488179259,3.38791311327368,2.70434137284756,3.07582076750696,3.142356935516,2.65837837215513,2.88813841287047,3.21473951973021,3.15415691565722],"y":[28.8157030487806,50.8806192996539,37.5041769915819,39.4034181249514,43.9903447949328,60.9808641496487,37.3815612095222,37.4966854153387,44.9282880165242,45.3012392660417,65.361978031639,62.9943189661577,43.1815865181945,50.0745684834756,47.5111346526071,55.5671127968095,55.9168005929515,48.4942746872082,42.2267547051422,36.2769740254804,36.2358360726759,37.5985150737688,43.3743744828179,47.8132125235349,39.861004451327,60.377775136251,57.5853345849179,45.8978711984679,58.5137738121487,50.8095073744655,40.4364162559807,43.17799575001,32.504841154255],"text":["continent: Asia<br />wm: 28.8<br />continent: Asia","continent: Asia<br />wm: 50.9<br />continent: Asia","continent: Asia<br />wm: 37.5<br />continent: Asia","continent: Asia<br />wm: 39.4<br />continent: Asia","continent: Asia<br />wm: 44.0<br />continent: Asia","continent: Asia<br />wm: 61.0<br />continent: Asia","continent: Asia<br />wm: 37.4<br />continent: Asia","continent: Asia<br />wm: 37.5<br />continent: Asia","continent: Asia<br />wm: 44.9<br />continent: Asia","continent: Asia<br />wm: 45.3<br />continent: Asia","continent: Asia<br />wm: 65.4<br />continent: Asia","continent: Asia<br />wm: 63.0<br />continent: Asia","continent: Asia<br />wm: 43.2<br />continent: Asia","continent: Asia<br />wm: 50.1<br />continent: Asia","continent: Asia<br />wm: 47.5<br />continent: Asia","continent: Asia<br />wm: 55.6<br />continent: Asia","continent: Asia<br />wm: 55.9<br />continent: Asia","continent: Asia<br />wm: 48.5<br />continent: Asia","continent: Asia<br />wm: 42.2<br />continent: Asia","continent: Asia<br />wm: 36.3<br />continent: Asia","continent: Asia<br />wm: 36.2<br />continent: Asia","continent: Asia<br />wm: 37.6<br />continent: Asia","continent: Asia<br />wm: 43.4<br />continent: Asia","continent: Asia<br />wm: 47.8<br />continent: Asia","continent: Asia<br />wm: 39.9<br />continent: Asia","continent: Asia<br />wm: 60.4<br />continent: Asia","continent: Asia<br />wm: 57.6<br />continent: Asia","continent: Asia<br />wm: 45.9<br />continent: Asia","continent: Asia<br />wm: 58.5<br />continent: Asia","continent: Asia<br />wm: 50.8<br />continent: Asia","continent: Asia<br />wm: 40.4<br />continent: Asia","continent: Asia<br />wm: 43.2<br />continent: Asia","continent: Asia<br />wm: 32.5<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.16214796286076,2.70224058013409,2.95622198712081,3.19472133498639,3.11963512860239,3.09651747345924,2.74694854114205,3.05858035199344,2.86195372752845,3.3170551860705,3.34944620896131,3.32907603848726,3.30332361292094,3.19897981416434,2.90336218718439,2.70593066513538,2.63300147186965,3.38788488227874,2.890492949076,2.9893653139472,3.20780281927437,3.03157041911036,2.7794246410951,2.86914711240679,2.69121539965272,3.01005437541753,3.21364404018968,3.01242114920169,3.0283908335492,3.17930772621185,3.26756772920489,2.63637094758451,3.20327440071851],"y":[38.4091465216316,65.6210191024654,46.9073938669078,31.2159695772454,64.0310808956809,73.5707550380193,54.1919585894793,52.7245767662115,57.6847126578912,60.3630952704884,73.1070802007243,75.3867792106047,61.127669019457,67.2306962173432,64.8275868279673,69.2926728213578,66.1105305205844,65.3065079766512,55.480422494933,56.1361975477263,46.6862806735188,57.3630171858333,54.0317283923738,60.0641167949513,58.7035115991347,70.7732434696332,65.8671991430223,61.18405841019,70.5627592159249,62.4907090680115,55.8215436745621,60.7607939578965,44.1750333507545],"text":["continent: Asia<br />wm: 38.4<br />continent: Asia","continent: Asia<br />wm: 65.6<br />continent: Asia","continent: Asia<br />wm: 46.9<br />continent: Asia","continent: Asia<br />wm: 31.2<br />continent: Asia","continent: Asia<br />wm: 64.0<br />continent: Asia","continent: Asia<br />wm: 73.6<br />continent: Asia","continent: Asia<br />wm: 54.2<br />continent: Asia","continent: Asia<br />wm: 52.7<br />continent: Asia","continent: Asia<br />wm: 57.7<br />continent: Asia","continent: Asia<br />wm: 60.4<br />continent: Asia","continent: Asia<br />wm: 73.1<br />continent: Asia","continent: Asia<br />wm: 75.4<br />continent: Asia","continent: Asia<br />wm: 61.1<br />continent: Asia","continent: Asia<br />wm: 67.2<br />continent: Asia","continent: Asia<br />wm: 64.8<br />continent: Asia","continent: Asia<br />wm: 69.3<br />continent: Asia","continent: Asia<br />wm: 66.1<br />continent: Asia","continent: Asia<br />wm: 65.3<br />continent: Asia","continent: Asia<br />wm: 55.5<br />continent: Asia","continent: Asia<br />wm: 56.1<br />continent: Asia","continent: Asia<br />wm: 46.7<br />continent: Asia","continent: Asia<br />wm: 57.4<br />continent: Asia","continent: Asia<br />wm: 54.0<br />continent: Asia","continent: Asia<br />wm: 60.1<br />continent: Asia","continent: Asia<br />wm: 58.7<br />continent: Asia","continent: Asia<br />wm: 70.8<br />continent: Asia","continent: Asia<br />wm: 65.9<br />continent: Asia","continent: Asia<br />wm: 61.2<br />continent: Asia","continent: Asia<br />wm: 70.6<br />continent: Asia","continent: Asia<br />wm: 62.5<br />continent: Asia","continent: Asia<br />wm: 55.8<br />continent: Asia","continent: Asia<br />wm: 60.8<br />continent: Asia","continent: Asia<br />wm: 44.2<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.14485854599625,2.94438787922263,3.25488395113498,3.07284976355731,3.39863308034837,2.75875338520855,2.72566040605307,2.83162901196629,3.26042134854943,3.32619636449963,2.60622854027897,2.81098825223744,2.78361690267921,2.98749947585166,3.0522256013006,2.93484691344202,2.71048425510526,3.27100472711027,3.04861716404557,3.08613664079458,3.16502246018499,3.17342349998653,3.31513427011669,2.67164441123605,2.980585796386,3.12106457911432,2.70373540539294,3.00410588569939,3.07005413938314,3.38477944657207,2.99086318332702,2.90565399415791,3.16942510008812],"y":[42.0730265968852,74.7922195995227,62.0050742803328,56.8256084134988,72.0381073364243,81.4845907369815,62.8679342499748,68.6336186113953,69.469040329773,56.9867602819577,79.6829082092829,81.9617075830139,71.3279320321046,66.7290971132927,77.0107820926048,76.8614121090062,71.0143228228204,72.9640292603336,64.9756563105434,59.9196463568695,61.3033865233511,74.201676625181,63.5648524313793,70.2602725812979,71.6397675060481,78.7955755367502,70.7794744626991,73.076211957261,77.0321959248558,68.6060881351866,72.981361267101,72.4201584754139,60.2893162805587],"text":["continent: Asia<br />wm: 42.1<br />continent: Asia","continent: Asia<br />wm: 74.8<br />continent: Asia","continent: Asia<br />wm: 62.0<br />continent: Asia","continent: Asia<br />wm: 56.8<br />continent: Asia","continent: Asia<br />wm: 72.0<br />continent: Asia","continent: Asia<br />wm: 81.5<br />continent: Asia","continent: Asia<br />wm: 62.9<br />continent: Asia","continent: Asia<br />wm: 68.6<br />continent: Asia","continent: Asia<br />wm: 69.5<br />continent: Asia","continent: Asia<br />wm: 57.0<br />continent: Asia","continent: Asia<br />wm: 79.7<br />continent: Asia","continent: Asia<br />wm: 82.0<br />continent: Asia","continent: Asia<br />wm: 71.3<br />continent: Asia","continent: Asia<br />wm: 66.7<br />continent: Asia","continent: Asia<br />wm: 77.0<br />continent: Asia","continent: Asia<br />wm: 76.9<br />continent: Asia","continent: Asia<br />wm: 71.0<br />continent: Asia","continent: Asia<br />wm: 73.0<br />continent: Asia","continent: Asia<br />wm: 65.0<br />continent: Asia","continent: Asia<br />wm: 59.9<br />continent: Asia","continent: Asia<br />wm: 61.3<br />continent: Asia","continent: Asia<br />wm: 74.2<br />continent: Asia","continent: Asia<br />wm: 63.6<br />continent: Asia","continent: Asia<br />wm: 70.3<br />continent: Asia","continent: Asia<br />wm: 71.6<br />continent: Asia","continent: Asia<br />wm: 78.8<br />continent: Asia","continent: Asia<br />wm: 70.8<br />continent: Asia","continent: Asia<br />wm: 73.1<br />continent: Asia","continent: Asia<br />wm: 77.0<br />continent: Asia","continent: Asia<br />wm: 68.6<br />continent: Asia","continent: Asia<br />wm: 73.0<br />continent: Asia","continent: Asia<br />wm: 72.4<br />continent: Asia","continent: Asia<br />wm: 60.3<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.84426455199718,4.11232898011804,3.69881943911314,3.94040076546371,4.33920955099165,4.33011510595679,4.21404843684286,3.92289422210306,3.87371984682977,3.68154068198055,3.8706747667864,4.17927504237741,3.91545329205692,3.61204995755106,3.81647352371365,3.74114465750754,3.80963634420186,3.8270366968587,3.62607705295086,4.01373961977661,4.32010061256588,4.13838763795793,3.70673077013344,3.76860003098845,4.18573654107749,3.82988378126174,3.78625868987292,4.01447914335877,4.1085655881092,4.28766924142838],"y":[55.18466854183,66.7872322660498,67.9979154629447,53.8042117995396,59.6357265840843,61.2378808077797,66.8615029818378,70.7661343455128,66.5346248795837,67.3998250867799,67.4721516868286,65.9112996008992,64.0255614996515,72.4909974034689,66.9121100816317,65.8757890366763,59.1630313538946,72.100933509022,72.6789913068898,61.3394905137271,59.7837045435421,60.979324841816,57.9633197318204,64.3911726371572,65.5778151758574,64.9391351667233,71.8621604687534,69.59942646835,43.6106622715481,69.2237495012023],"text":["continent: Europe<br />wm: 55.2<br />continent: Europe","continent: Europe<br />wm: 66.8<br />continent: Europe","continent: Europe<br />wm: 68.0<br />continent: Europe","continent: Europe<br />wm: 53.8<br />continent: Europe","continent: Europe<br />wm: 59.6<br />continent: Europe","continent: Europe<br />wm: 61.2<br />continent: Europe","continent: Europe<br />wm: 66.9<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 66.5<br />continent: Europe","continent: Europe<br />wm: 67.4<br />continent: Europe","continent: Europe<br />wm: 67.5<br />continent: Europe","continent: Europe<br />wm: 65.9<br />continent: Europe","continent: Europe<br />wm: 64.0<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 66.9<br />continent: Europe","continent: Europe<br />wm: 65.9<br />continent: Europe","continent: Europe<br />wm: 59.2<br />continent: Europe","continent: Europe<br />wm: 72.1<br />continent: Europe","continent: Europe<br />wm: 72.7<br />continent: Europe","continent: Europe<br />wm: 61.3<br />continent: Europe","continent: Europe<br />wm: 59.8<br />continent: Europe","continent: Europe<br />wm: 61.0<br />continent: Europe","continent: Europe<br />wm: 58.0<br />continent: Europe","continent: Europe<br />wm: 64.4<br />continent: Europe","continent: Europe<br />wm: 65.6<br />continent: Europe","continent: Europe<br />wm: 64.9<br />continent: Europe","continent: Europe<br />wm: 71.9<br />continent: Europe","continent: Europe<br />wm: 69.6<br />continent: Europe","continent: Europe<br />wm: 43.6<br />continent: Europe","continent: Europe<br />wm: 69.2<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.08422762323171,4.3881361104548,4.16883182991296,3.78613361492753,3.85864182934165,4.20571327637881,4.1223122799769,4.37875776607543,4.22862002551556,3.7057665893808,4.24468668811023,4.20840468220413,3.73357856553048,4.07676997631788,3.80520177315921,3.62653209343553,3.7072776209563,3.89906560759991,3.82225105948746,3.65288894753903,4.31476497426629,4.33826067131013,3.68565511703491,3.73451751563698,4.04371869917959,4.20056594405323,3.78049594331533,3.94012809991837,3.88253150228411,4.24118885640055],"y":[68.8782198012993,72.194256690219,72.7668754521571,69.908161748331,70.7632460799068,70.5754343670793,70.728696955815,74.691278823074,72.4853591105715,73.7950399756431,72.4609310993366,73.7341114792414,69.9913731894828,76.0887207482383,71.9612678847648,73.5133456817269,73.1369163876772,75.207290645875,75.4154539143108,70.6945351942256,70.421732008718,69.5305814485066,70.2765250594728,70.4874150097184,70.9901039372757,74.4212558603659,75.4271594482101,75.4226821720414,59.5147065675817,72.7630952955969],"text":["continent: Europe<br />wm: 68.9<br />continent: Europe","continent: Europe<br />wm: 72.2<br />continent: Europe","continent: Europe<br />wm: 72.8<br />continent: Europe","continent: Europe<br />wm: 69.9<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 70.6<br />continent: Europe","continent: Europe<br />wm: 70.7<br />continent: Europe","continent: Europe<br />wm: 74.7<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 73.8<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 73.7<br />continent: Europe","continent: Europe<br />wm: 70.0<br />continent: Europe","continent: Europe<br />wm: 76.1<br />continent: Europe","continent: Europe<br />wm: 72.0<br />continent: Europe","continent: Europe<br />wm: 73.5<br />continent: Europe","continent: Europe<br />wm: 73.1<br />continent: Europe","continent: Europe<br />wm: 75.2<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 70.7<br />continent: Europe","continent: Europe<br />wm: 70.4<br />continent: Europe","continent: Europe<br />wm: 69.5<br />continent: Europe","continent: Europe<br />wm: 70.3<br />continent: Europe","continent: Europe<br />wm: 70.5<br />continent: Europe","continent: Europe<br />wm: 71.0<br />continent: Europe","continent: Europe<br />wm: 74.4<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 59.5<br />continent: Europe","continent: Europe<br />wm: 72.8<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.29474422819912,3.67098508141935,3.78375491984189,3.65122870653868,3.86171750854701,3.87553839348257,3.92502307146788,4.01612309869379,3.91561513580382,4.15349110644311,3.94213681183755,4.14482165537775,4.00808352027088,3.97628244590014,4.00406863987446,4.29126892611384,4.35156764481217,4.08909030742943,3.79842420704663,4.12968032043427,3.96626004651189,3.77746801935136,4.19157062936574,3.64962630830705,4.26003106553108,3.94715834800154,3.86816353667527,4.17207694258541,3.81675714366138,4.27839735690504],"y":[75.675975297261,78.9762788779847,78.2930563621037,74.1077630335465,72.1077278137021,74.9015709782951,75.4635953033902,77.2137882975861,78.4286168917455,79.5618788382597,78.7006678604521,78.2994074072503,72.6025194585323,80.5091015354916,77.7639622419514,80.2120051471703,74.0212638209574,78.4821987460926,78.9816338863224,74.7130584569834,77.2885457387008,71.2731626653671,73.1625631717965,73.8346473415755,76.660253807269,79.7602180702053,79.9987291595153,80.6162753269635,70.8095267250016,78.5085281488113],"text":["continent: Europe<br />wm: 75.7<br />continent: Europe","continent: Europe<br />wm: 79.0<br />continent: Europe","continent: Europe<br />wm: 78.3<br />continent: Europe","continent: Europe<br />wm: 74.1<br />continent: Europe","continent: Europe<br />wm: 72.1<br />continent: Europe","continent: Europe<br />wm: 74.9<br />continent: Europe","continent: Europe<br />wm: 75.5<br />continent: Europe","continent: Europe<br />wm: 77.2<br />continent: Europe","continent: Europe<br />wm: 78.4<br />continent: Europe","continent: Europe<br />wm: 79.6<br />continent: Europe","continent: Europe<br />wm: 78.7<br />continent: Europe","continent: Europe<br />wm: 78.3<br />continent: Europe","continent: Europe<br />wm: 72.6<br />continent: Europe","continent: Europe<br />wm: 80.5<br />continent: Europe","continent: Europe<br />wm: 77.8<br />continent: Europe","continent: Europe<br />wm: 80.2<br />continent: Europe","continent: Europe<br />wm: 74.0<br />continent: Europe","continent: Europe<br />wm: 78.5<br />continent: Europe","continent: Europe<br />wm: 79.0<br />continent: Europe","continent: Europe<br />wm: 74.7<br />continent: Europe","continent: Europe<br />wm: 77.3<br />continent: Europe","continent: Europe<br />wm: 71.3<br />continent: Europe","continent: Europe<br />wm: 73.2<br />continent: Europe","continent: Europe<br />wm: 73.8<br />continent: Europe","continent: Europe<br />wm: 76.7<br />continent: Europe","continent: Europe<br />wm: 79.8<br />continent: Europe","continent: Europe<br />wm: 80.0<br />continent: Europe","continent: Europe<br />wm: 80.6<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 78.5<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.92839541528374,4.84348256345838],"y":[69.0727765005268,69.3793802101538],"text":["continent: Oceania<br />wm: 69.1<br />continent: Oceania","continent: Oceania<br />wm: 69.4<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[5.16178469881415,4.98133100382984],"y":[73.511336057242,72.1832604767941],"text":["continent: Oceania<br />wm: 73.5<br />continent: Oceania","continent: Oceania<br />wm: 72.2<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.82093978971243,5.34740251135081],"y":[80.3659299053066,79.1138025984354],"text":["continent: Oceania<br />wm: 80.4<br />continent: Oceania","continent: Oceania<br />wm: 79.1<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":62.3594852635948,"r":25.9028642590286,"b":71.9231828878753,"l":38.854296388543},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Figure 2: Weighted mean of life expectancy (by population) across continents","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,0.322461404653185],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"annotations":[{"text":"Continent","x":0.5,"y":-0.113310283984443,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"Life expectancy","x":-0.0360750360750361,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"1952","x":0.161230702326593,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"1977","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"2002","x":0.838769297673407,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"continent","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[26.1584028220689,84.6190078097256],"tickmode":"array","ticktext":["30","35","40","45","50","55","60","65","70","75","80"],"tickvals":[30,35,40,45,50,55,60,65,70,75,80],"categoryorder":"array","categoryarray":["30","35","40","45","50","55","60","65","70","75","80"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.322461404653185,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.322461404653185,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.344205262013481,"x1":0.655794737986519,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.344205262013481,"x1":0.655794737986519,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.677538595346814,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.677538595346814,"x1":1,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.344205262013481,0.655794737986519],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.677538595346814,1],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.913385826771654},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"405d54589f96":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"405d54589f96","visdat":{"405d54589f96":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

# Part 4: Writing figures to file

Now, we save the plots from above to
file.

``` r
ggsave("plot_raster.jpg", plot = P_new, width = 8, height = 4, dpi = 100) #save as a raster file format (.jpg)
ggsave("plot_vector.eps", plot = P_new) #save as a vector file format (.eps)
```

    ## Saving 7 x 5 in image

Here are the plot images, first as a raster and then a
vector:

![plot\_raster.jpg](https://github.com/STAT545-UBC-students/hw05-bassamjaved/blob/master/plot_raster.jpg)

# But I want to do more\!

Let’s explore other functions in the forcats package for manipulating
factors. I will use an excerpt of countries in the gapminder data frame
and add language spoken.

``` r
#Create factor to join with gapminder excerpt
language_table <- tibble(country = 
                           factor(c("Italy",
                             "France",
                             "Spain",
                             "Mexico",
                             "Austria",
                             "Germany",
                             "Morocco",
                             "Angola",
                             "Brazil",
                             "Egypt")),
                         language = 
                           factor(c("Italian",
                             "French",
                             "Spanish",
                             "Spanish",
                             "German",
                             "German",
                             "French",
                             "Portuguese",
                             "Portuguese",
                             "Arabic"))
                           )

gap_small <- left_join(gapminder, language_table, by = "country") %>% 
  filter(year == 2002) %>%
  transmute(country, continent, language) %>% 
  filter(language != "NA") %>% 
  droplevels()
```

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

``` r
#Show the use of collapse (Italian and Arabic into Other), recode (Spanish to Espanol), and relevel (French first)

gap_small_collapse <- gap_small %>% 
  mutate(language = fct_collapse(language,
                                     Other = 
                                       c("Italian",
                                         "Arabic"
                                         )
                                     )
         )

gap_small_recode <- gap_small %>% 
  mutate(language = fct_recode(language, "Espanol" = "Spanish"))

gap_small_relevel <- gap_small %>% 
  mutate(language = fct_relevel(language, "French"))

levels(gap_small_collapse$language)
```

    ## [1] "Other"      "French"     "German"     "Portuguese" "Spanish"

``` r
levels(gap_small_recode$language)
```

    ## [1] "Arabic"     "French"     "German"     "Italian"    "Portuguese"
    ## [6] "Espanol"

``` r
levels(gap_small_relevel$language)
```

    ## [1] "French"     "Arabic"     "German"     "Italian"    "Portuguese"
    ## [6] "Spanish"

*Comments:* fct\_collapse() can collapse multiple factor levels into a
new one. Here I put “Italian” and “Arabic” into a new one called
“Other”. fct\_recode() allows a factor level to be renamed. Here I
changed “Spanish” to “Espanol”. Finally, fct\_relevel() changes the
order manually. Here I put “French” first.
