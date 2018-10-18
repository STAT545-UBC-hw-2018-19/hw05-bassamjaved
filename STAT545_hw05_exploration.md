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
```

# Part 1: Factor management

Let’s compare the use of `filter()` vs `droplevels()` upon removing
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

*Comments:* Use of `filter()` does not effect a change in the number of
levels in country (142) or continent (5) factors, whereas `droplevels()`
displays only the observed levels in country (140) and continent (4)
factors. As expected, the dropped level in continent is “Oceania”;
correspondingly, the dropped levels in country are “Australia” and “New
Zealand”.

Next, let’s compare the use of `arrange()` and `fct_reorder()` to
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
the use of `arrange()` reorganizes GDP per capita in ascending order
within each continent grouping. The use of `fct_reorder()` has no
observable effect on the table itself. The use of both functions leads
only to the observable effects of `arrange()`.

Usings `levels()` to check the factor levels, there is no difference
after using `arrange()`. After `fct_reorder()`, the order of the factors
changes. In this case, Europe has the second highest maximum, so
`fct_reorder()` has changed its position accordingly from the default
alphabetical ordering of contient factors.

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

*Comments:* The use of `arrange()` only operates on the table output and
has no effect on the plot. This makes sense intuitively, as the data
points are still the same and thus the plot output will be the same, so
long as the grouping is still by continent and plotted against GDP per
capita. The use of `fct_reorder()` on the other hand does lead to an
observable difference in the plot. Here, I called the function to
reorder by the summary statistic maximum, so that the boxplot is
arranged in order of increasing maxima. The ordering of the factors is
the same as that observed using `levels()` above.

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
       x = "Continent", y = "Life expectancy") +
  facet_grid(.~year)

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
  theme_classic() +
  theme(axis.text.x  = element_text(angle=65, vjust=0.5, size=8),
        axis.text = element_text(size = 10),
        strip.background = element_rect(colour = "white"),
        strip.text = element_text(size = 14)
        ) + #modify text labels and other plot elements
  scale_y_continuous(breaks = 1:20 * 5) + #set y-axis numbering
  scale_colour_brewer(palette = "Set1") #set color scheme

P_new
```

![](STAT545_hw05_exploration_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
#Convert this to a `plotly` plot:
 
ggplotly(P_new)
```

<!--html_preserve-->

<div id="htmlwidget-ffa2fcb0c38010b3bd13" class="plotly html-widget" style="width:672px;height:480px;">

</div>

<script type="application/json" data-for="htmlwidget-ffa2fcb0c38010b3bd13">{"x":{"data":[{"x":[0.671548173390329,0.895752240344882,0.891462216340005,1.05603944789618,0.859670962579548,1.26809604149312,0.968925757706165,1.11944001503289,0.931238059140742,0.960341576486826,0.981388064287603,1.29658090770245,1.24988972730935,0.973741382919252,1.16948436424136,0.97597177978605,1.32322226017714,0.857271598652005,1.30134635213763,0.759395376220345,1.05730741936713,1.20923637449741,0.774290987476707,0.989746829867363,1.35444178450853,0.830506322532892,1.08095874879509,0.67385599128902,0.917663918994367,0.990463792346418,0.913130499050021,1.02314901109785,1.07559080924839,0.820510348677635,1.32449013311416,0.665012023039162,1.08325119130313,0.628786165267229,1.2198383865878,0.778699618764222,1.22132921218872,0.783329550735652,1.37437884341925,1.12682421654463,0.887272961437702,1.33141674958169,1.14833196029067,1.33819709643722,0.889349002949893,1.22952612899244,1.26967282835394,0.992553216218948],"y":[43.1306918491051,30.0129272702336,38.1882052922435,47.6149843485467,31.9603484242782,39.0008697221056,38.4812696987018,35.5015875100903,38.0754671378993,40.6744927440025,39.0669299117289,42.0900799291395,40.506747188624,34.7829305066727,41.8959226402454,34.5091620275751,35.8727307499386,34.0802553811856,37.026644540485,29.9963902089372,43.1382501654327,33.6302893652953,32.4652232800983,42.2632991360314,42.1192249818146,38.5343856080435,42.7294924219325,36.6742532907613,36.3346779173054,33.6994659508578,40.5284096172266,50.9895487115905,42.9244850921072,31.2906028778479,41.6706988208927,37.4184040554985,36.278459303584,52.7358477928676,40.0180597515963,46.5323770689219,37.3139279673994,30.2826801143028,33.030414939113,45.0374027164653,38.5936737434007,41.3710949814133,41.1879163851216,38.5746309275553,44.565330635868,39.9732909781486,42.0137207002938,48.5314970900118],"text":["continent: Africa<br />wm: 43.1<br />continent: Africa","continent: Africa<br />wm: 30.0<br />continent: Africa","continent: Africa<br />wm: 38.2<br />continent: Africa","continent: Africa<br />wm: 47.6<br />continent: Africa","continent: Africa<br />wm: 32.0<br />continent: Africa","continent: Africa<br />wm: 39.0<br />continent: Africa","continent: Africa<br />wm: 38.5<br />continent: Africa","continent: Africa<br />wm: 35.5<br />continent: Africa","continent: Africa<br />wm: 38.1<br />continent: Africa","continent: Africa<br />wm: 40.7<br />continent: Africa","continent: Africa<br />wm: 39.1<br />continent: Africa","continent: Africa<br />wm: 42.1<br />continent: Africa","continent: Africa<br />wm: 40.5<br />continent: Africa","continent: Africa<br />wm: 34.8<br />continent: Africa","continent: Africa<br />wm: 41.9<br />continent: Africa","continent: Africa<br />wm: 34.5<br />continent: Africa","continent: Africa<br />wm: 35.9<br />continent: Africa","continent: Africa<br />wm: 34.1<br />continent: Africa","continent: Africa<br />wm: 37.0<br />continent: Africa","continent: Africa<br />wm: 30.0<br />continent: Africa","continent: Africa<br />wm: 43.1<br />continent: Africa","continent: Africa<br />wm: 33.6<br />continent: Africa","continent: Africa<br />wm: 32.5<br />continent: Africa","continent: Africa<br />wm: 42.3<br />continent: Africa","continent: Africa<br />wm: 42.1<br />continent: Africa","continent: Africa<br />wm: 38.5<br />continent: Africa","continent: Africa<br />wm: 42.7<br />continent: Africa","continent: Africa<br />wm: 36.7<br />continent: Africa","continent: Africa<br />wm: 36.3<br />continent: Africa","continent: Africa<br />wm: 33.7<br />continent: Africa","continent: Africa<br />wm: 40.5<br />continent: Africa","continent: Africa<br />wm: 51.0<br />continent: Africa","continent: Africa<br />wm: 42.9<br />continent: Africa","continent: Africa<br />wm: 31.3<br />continent: Africa","continent: Africa<br />wm: 41.7<br />continent: Africa","continent: Africa<br />wm: 37.4<br />continent: Africa","continent: Africa<br />wm: 36.3<br />continent: Africa","continent: Africa<br />wm: 52.7<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa","continent: Africa<br />wm: 46.5<br />continent: Africa","continent: Africa<br />wm: 37.3<br />continent: Africa","continent: Africa<br />wm: 30.3<br />continent: Africa","continent: Africa<br />wm: 33.0<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 38.6<br />continent: Africa","continent: Africa<br />wm: 41.4<br />continent: Africa","continent: Africa<br />wm: 41.2<br />continent: Africa","continent: Africa<br />wm: 38.6<br />continent: Africa","continent: Africa<br />wm: 44.6<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 48.5<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.89677537381649,1.04271961282939,1.30061293225735,0.795599167793989,0.940029392950237,0.641106659919023,1.19584109913558,1.39607900828123,0.786048641428351,1.11578385382891,1.03822862338275,1.1611907787621,0.974110634252429,1.28175585065037,0.838838549889624,0.995552177354693,0.79825052600354,0.775997552648187,1.11376154366881,0.881569103896618,1.18590005170554,0.816330449655652,0.975489429198205,1.09372023791075,1.06832072660327,1.28867628611624,1.16163607705384,0.956825482472777,1.1783798944205,0.95176400039345,1.2925243800506,1.2082276199013,1.37726928591728,0.771963684260845,0.87782222237438,0.71962732411921,0.794302225112915,0.896519195474684,0.697748176753521,1.1069936690852,1.12841270118952,0.771166551485658,0.847471909224987,0.6043263444677,1.14728677626699,0.99214393068105,1.09774243123829,1.03974906932563,0.738930486701429,1.20843821223825,1.31948488838971,1.16212181821465],"y":[58.0240909883007,39.5385393304378,49.2339155292511,59.3302094974183,46.1107368454523,45.8891377060115,49.3809796175919,46.7654898632877,47.4101495660469,50.8895650379173,47.7916104373708,55.5836553014629,52.4209009519219,46.5397051687539,53.2789660739712,42.0304268037155,44.485236500036,44.4981136899628,52.8061994160898,41.8078141921945,51.8177233584225,40.7888355869241,37.500074717328,56.2344796847738,52.1956519878469,43.8184594312124,57.3923679599725,46.8788033475727,43.7800626144372,41.6821001012623,50.8603185467236,64.8984956632368,55.7332273726724,42.5167181707919,56.4220804693364,41.2794081104733,44.5015445261262,67.0877721581049,45.0202942265384,58.4987340161204,48.9315066098794,36.8171793886088,42.0184267717227,55.5127012121491,47.7755660741217,52.5100167455897,49.934305368755,52.8890455568209,59.7821510944515,50.4234443149902,51.3952865567245,57.6775661054812],"text":["continent: Africa<br />wm: 58.0<br />continent: Africa","continent: Africa<br />wm: 39.5<br />continent: Africa","continent: Africa<br />wm: 49.2<br />continent: Africa","continent: Africa<br />wm: 59.3<br />continent: Africa","continent: Africa<br />wm: 46.1<br />continent: Africa","continent: Africa<br />wm: 45.9<br />continent: Africa","continent: Africa<br />wm: 49.4<br />continent: Africa","continent: Africa<br />wm: 46.8<br />continent: Africa","continent: Africa<br />wm: 47.4<br />continent: Africa","continent: Africa<br />wm: 50.9<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 55.6<br />continent: Africa","continent: Africa<br />wm: 52.4<br />continent: Africa","continent: Africa<br />wm: 46.5<br />continent: Africa","continent: Africa<br />wm: 53.3<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 52.8<br />continent: Africa","continent: Africa<br />wm: 41.8<br />continent: Africa","continent: Africa<br />wm: 51.8<br />continent: Africa","continent: Africa<br />wm: 40.8<br />continent: Africa","continent: Africa<br />wm: 37.5<br />continent: Africa","continent: Africa<br />wm: 56.2<br />continent: Africa","continent: Africa<br />wm: 52.2<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 57.4<br />continent: Africa","continent: Africa<br />wm: 46.9<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 41.7<br />continent: Africa","continent: Africa<br />wm: 50.9<br />continent: Africa","continent: Africa<br />wm: 64.9<br />continent: Africa","continent: Africa<br />wm: 55.7<br />continent: Africa","continent: Africa<br />wm: 42.5<br />continent: Africa","continent: Africa<br />wm: 56.4<br />continent: Africa","continent: Africa<br />wm: 41.3<br />continent: Africa","continent: Africa<br />wm: 44.5<br />continent: Africa","continent: Africa<br />wm: 67.1<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 58.5<br />continent: Africa","continent: Africa<br />wm: 48.9<br />continent: Africa","continent: Africa<br />wm: 36.8<br />continent: Africa","continent: Africa<br />wm: 42.0<br />continent: Africa","continent: Africa<br />wm: 55.5<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 52.5<br />continent: Africa","continent: Africa<br />wm: 49.9<br />continent: Africa","continent: Africa<br />wm: 52.9<br />continent: Africa","continent: Africa<br />wm: 59.8<br />continent: Africa","continent: Africa<br />wm: 50.4<br />continent: Africa","continent: Africa<br />wm: 51.4<br />continent: Africa","continent: Africa<br />wm: 57.7<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.652437796443701,1.00653676688671,1.25163877177984,1.01504386439919,0.728719782270491,1.38598037064075,0.982701499387622,1.07729365322739,1.38996012695134,1.39461683668196,1.37038071490824,0.945411921851337,0.736779725924134,1.08115537371486,0.706119806878269,0.734622471220791,0.749415847100318,1.32696283515543,1.14356412831694,0.656651516258717,0.782912477478385,1.05701955202967,0.739635057561099,0.755601199343801,1.09295507781208,0.640062063187361,1.04749570507556,1.15855871234089,1.26278437674046,1.25494746863842,1.20958418454975,0.604653330333531,1.38513834290206,1.25222073253244,1.19386274721473,0.677775768004358,1.0012034535408,1.27304288614541,0.842159453034401,1.30807435549796,0.755421921238303,0.791853302530944,1.14661193415523,1.12618114314973,0.832358670793474,0.734308906644583,0.843378417007625,0.80066192727536,1.32733363155276,1.32155130021274,1.23778402600437,1.07937522009015],"y":[71.032564988751,40.9888474787027,54.4394640081562,46.6386166745983,50.6218503227271,47.3626663823053,49.89384917574,43.339612238016,50.5315273968875,63.0161793919094,45.0230390780233,53.0094724647887,46.8278244915232,53.3801030427031,69.8263190356083,49.3065968244523,55.2056735440716,50.6995063041896,56.7617853978649,57.9717199139856,58.5061084377952,53.6924231651239,45.5220859918557,50.996498834379,44.5913779519871,43.7679727333598,72.6879894499853,57.3035442779399,45.02659174053,51.7742841010727,62.2272015387937,72.0158856722713,69.5766284378804,44.0122110891901,51.4767829223722,54.4676260180585,46.573474923633,75.6912089655362,43.4324675256759,64.2887535233423,61.6376390621625,40.9630553617142,45.9067340010777,53.3849766655453,56.3605204815976,43.8719240813889,49.679892414622,57.5616676226258,72.9863758876547,47.8031835751794,39.195383913666,39.9993064894341],"text":["continent: Africa<br />wm: 71.0<br />continent: Africa","continent: Africa<br />wm: 41.0<br />continent: Africa","continent: Africa<br />wm: 54.4<br />continent: Africa","continent: Africa<br />wm: 46.6<br />continent: Africa","continent: Africa<br />wm: 50.6<br />continent: Africa","continent: Africa<br />wm: 47.4<br />continent: Africa","continent: Africa<br />wm: 49.9<br />continent: Africa","continent: Africa<br />wm: 43.3<br />continent: Africa","continent: Africa<br />wm: 50.5<br />continent: Africa","continent: Africa<br />wm: 63.0<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 53.0<br />continent: Africa","continent: Africa<br />wm: 46.8<br />continent: Africa","continent: Africa<br />wm: 53.4<br />continent: Africa","continent: Africa<br />wm: 69.8<br />continent: Africa","continent: Africa<br />wm: 49.3<br />continent: Africa","continent: Africa<br />wm: 55.2<br />continent: Africa","continent: Africa<br />wm: 50.7<br />continent: Africa","continent: Africa<br />wm: 56.8<br />continent: Africa","continent: Africa<br />wm: 58.0<br />continent: Africa","continent: Africa<br />wm: 58.5<br />continent: Africa","continent: Africa<br />wm: 53.7<br />continent: Africa","continent: Africa<br />wm: 45.5<br />continent: Africa","continent: Africa<br />wm: 51.0<br />continent: Africa","continent: Africa<br />wm: 44.6<br />continent: Africa","continent: Africa<br />wm: 43.8<br />continent: Africa","continent: Africa<br />wm: 72.7<br />continent: Africa","continent: Africa<br />wm: 57.3<br />continent: Africa","continent: Africa<br />wm: 45.0<br />continent: Africa","continent: Africa<br />wm: 51.8<br />continent: Africa","continent: Africa<br />wm: 62.2<br />continent: Africa","continent: Africa<br />wm: 72.0<br />continent: Africa","continent: Africa<br />wm: 69.6<br />continent: Africa","continent: Africa<br />wm: 44.0<br />continent: Africa","continent: Africa<br />wm: 51.5<br />continent: Africa","continent: Africa<br />wm: 54.5<br />continent: Africa","continent: Africa<br />wm: 46.6<br />continent: Africa","continent: Africa<br />wm: 75.7<br />continent: Africa","continent: Africa<br />wm: 43.4<br />continent: Africa","continent: Africa<br />wm: 64.3<br />continent: Africa","continent: Africa<br />wm: 61.6<br />continent: Africa","continent: Africa<br />wm: 41.0<br />continent: Africa","continent: Africa<br />wm: 45.9<br />continent: Africa","continent: Africa<br />wm: 53.4<br />continent: Africa","continent: Africa<br />wm: 56.4<br />continent: Africa","continent: Africa<br />wm: 43.9<br />continent: Africa","continent: Africa<br />wm: 49.7<br />continent: Africa","continent: Africa<br />wm: 57.6<br />continent: Africa","continent: Africa<br />wm: 73.0<br />continent: Africa","continent: Africa<br />wm: 47.8<br />continent: Africa","continent: Africa<br />wm: 39.2<br />continent: Africa","continent: Africa<br />wm: 40.0<br />continent: Africa"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(228,26,28,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(228,26,28,1)"}},"hoveron":"points","name":"Africa","legendgroup":"Africa","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.71708494573832,1.64735542554408,2.00128564219922,1.90300206784159,1.80640445165336,2.18826255761087,1.87574050109833,1.78972490411252,1.87786765396595,2.22224616967142,1.79154209215194,1.66641708742827,2.18298496510833,2.31734116636217,2.27540835887194,2.11068745944649,2.06793235391378,2.01507193315774,2.2256261439994,1.6509358888492,1.8243402319029,1.91359501201659,2.280352557078,2.38613860346377,1.64961103592068],"y":[62.5212565215677,40.4177669506893,50.8982333844528,68.7675240577944,54.6661477068439,50.5638790507801,57.1949153407663,59.4134467154555,45.8692781210318,48.4264155817591,45.2669653701223,42.0163993891515,37.5970360075682,41.882060637027,58.5220272205211,50.8177008102834,42.3093368508853,55.2326406975463,62.6230223060772,43.9221594709903,64.3138420957513,59.0968782625347,68.3849442364276,66.0651511356235,55.0771792437695],"text":["continent: Americas<br />wm: 62.5<br />continent: Americas","continent: Americas<br />wm: 40.4<br />continent: Americas","continent: Americas<br />wm: 50.9<br />continent: Americas","continent: Americas<br />wm: 68.8<br />continent: Americas","continent: Americas<br />wm: 54.7<br />continent: Americas","continent: Americas<br />wm: 50.6<br />continent: Americas","continent: Americas<br />wm: 57.2<br />continent: Americas","continent: Americas<br />wm: 59.4<br />continent: Americas","continent: Americas<br />wm: 45.9<br />continent: Americas","continent: Americas<br />wm: 48.4<br />continent: Americas","continent: Americas<br />wm: 45.3<br />continent: Americas","continent: Americas<br />wm: 42.0<br />continent: Americas","continent: Americas<br />wm: 37.6<br />continent: Americas","continent: Americas<br />wm: 41.9<br />continent: Americas","continent: Americas<br />wm: 58.5<br />continent: Americas","continent: Americas<br />wm: 50.8<br />continent: Americas","continent: Americas<br />wm: 42.3<br />continent: Americas","continent: Americas<br />wm: 55.2<br />continent: Americas","continent: Americas<br />wm: 62.6<br />continent: Americas","continent: Americas<br />wm: 43.9<br />continent: Americas","continent: Americas<br />wm: 64.3<br />continent: Americas","continent: Americas<br />wm: 59.1<br />continent: Americas","continent: Americas<br />wm: 68.4<br />continent: Americas","continent: Americas<br />wm: 66.1<br />continent: Americas","continent: Americas<br />wm: 55.1<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.93506069667637,2.30886297784746,2.36381535064429,1.88133763764054,2.07839370910078,1.62183242961764,1.97912423517555,2.11335332226008,1.99798381924629,2.36362392622978,2.31133693028241,2.10155573058873,1.9413138307631,2.21112203299999,2.35245597008616,2.22591812442988,2.20688257776201,2.38665249347687,2.28837328609079,2.14368165340275,2.22432033680379,1.82190680857748,1.75211567394435,1.67768826242536,2.03208165261894],"y":[68.4883786121197,49.971805711221,61.4869538590685,74.1953794014826,67.0885707555897,63.7647253455222,70.82295254739,72.589308309406,61.8026817530207,61.29498010315,56.6723460104317,55.9856481731497,49.8864120411687,57.376090187598,70.1297872200049,64.9784082022682,57.4975882530957,68.6721199049801,66.4086798913591,58.4120933202095,73.376243378371,68.3169126726687,73.4277525499091,69.5377170436457,67.4874376039207],"text":["continent: Americas<br />wm: 68.5<br />continent: Americas","continent: Americas<br />wm: 50.0<br />continent: Americas","continent: Americas<br />wm: 61.5<br />continent: Americas","continent: Americas<br />wm: 74.2<br />continent: Americas","continent: Americas<br />wm: 67.1<br />continent: Americas","continent: Americas<br />wm: 63.8<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 72.6<br />continent: Americas","continent: Americas<br />wm: 61.8<br />continent: Americas","continent: Americas<br />wm: 61.3<br />continent: Americas","continent: Americas<br />wm: 56.7<br />continent: Americas","continent: Americas<br />wm: 56.0<br />continent: Americas","continent: Americas<br />wm: 49.9<br />continent: Americas","continent: Americas<br />wm: 57.4<br />continent: Americas","continent: Americas<br />wm: 70.1<br />continent: Americas","continent: Americas<br />wm: 65.0<br />continent: Americas","continent: Americas<br />wm: 57.5<br />continent: Americas","continent: Americas<br />wm: 68.7<br />continent: Americas","continent: Americas<br />wm: 66.4<br />continent: Americas","continent: Americas<br />wm: 58.4<br />continent: Americas","continent: Americas<br />wm: 73.4<br />continent: Americas","continent: Americas<br />wm: 68.3<br />continent: Americas","continent: Americas<br />wm: 73.4<br />continent: Americas","continent: Americas<br />wm: 69.5<br />continent: Americas","continent: Americas<br />wm: 67.5<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[2.09267565496266,1.87833151314408,1.64997343104333,2.06357800997794,2.29090690333396,2.3040816033259,2.33263004124165,1.79669585078955,2.25569861978292,1.76555430367589,2.23636281825602,2.24817068763077,1.71871434003115,1.87696125414222,2.37716374527663,2.31975242272019,2.15034416206181,2.09895221386105,2.34387526586652,1.96497180033475,2.19235259909183,1.87496055327356,2.10509421434253,1.91423144098371,1.77341983597726],"y":[74.3089802785218,63.8866683982499,71.0184078142792,79.7831231860817,77.9177248646319,71.7238438915461,78.0872636048496,77.2369886359759,70.7653846463934,74.1825623009168,70.692711824961,68.9676851845905,58.1029578615353,68.5730628660135,71.9727209160291,74.9239456657693,70.8180005174316,74.6811682378873,70.7858880108222,69.9295851207525,77.8152740244754,69.0118400772102,77.338259956073,75.2980513905734,72.7865176524781],"text":["continent: Americas<br />wm: 74.3<br />continent: Americas","continent: Americas<br />wm: 63.9<br />continent: Americas","continent: Americas<br />wm: 71.0<br />continent: Americas","continent: Americas<br />wm: 79.8<br />continent: Americas","continent: Americas<br />wm: 77.9<br />continent: Americas","continent: Americas<br />wm: 71.7<br />continent: Americas","continent: Americas<br />wm: 78.1<br />continent: Americas","continent: Americas<br />wm: 77.2<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 74.2<br />continent: Americas","continent: Americas<br />wm: 70.7<br />continent: Americas","continent: Americas<br />wm: 69.0<br />continent: Americas","continent: Americas<br />wm: 58.1<br />continent: Americas","continent: Americas<br />wm: 68.6<br />continent: Americas","continent: Americas<br />wm: 72.0<br />continent: Americas","continent: Americas<br />wm: 74.9<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 74.7<br />continent: Americas","continent: Americas<br />wm: 70.8<br />continent: Americas","continent: Americas<br />wm: 69.9<br />continent: Americas","continent: Americas<br />wm: 77.8<br />continent: Americas","continent: Americas<br />wm: 69.0<br />continent: Americas","continent: Americas<br />wm: 77.3<br />continent: Americas","continent: Americas<br />wm: 75.3<br />continent: Americas","continent: Americas<br />wm: 72.8<br />continent: Americas"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(55,126,184,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(55,126,184,1)"}},"hoveron":"points","name":"Americas","legendgroup":"Americas","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.12722515761852,2.97463767677546,3.29344410970807,2.94461166113615,3.17083828579634,2.72123748082668,3.28246996868402,2.98979334700853,2.62334072031081,2.65675617046654,3.31845297887921,2.93623419422656,3.27048305869102,2.95267940014601,3.08179099168628,2.80851197578013,3.18732195980847,3.37766484934837,2.72708906568587,3.22879393156618,3.26425983645022,2.89350345060229,2.89299077838659,2.83642975315452,3.30044977366924,2.8909799342975,2.75338062904775,2.92526514027268,3.03172033242881,2.88537955991924,3.26332154087722,2.73349320665002,2.78133724387735],"y":[28.7915262434818,50.8968318054825,37.5377521771193,39.4093469071574,43.9601192194037,60.9850542254373,37.380634153951,37.4867744309641,44.8921183022112,45.3208515874669,65.3780665728264,62.9973487249948,43.1788253917731,50.1383279482089,47.4848669107817,55.5605628807098,55.9168829130381,48.5180273402669,42.1790312840417,36.3365279239975,36.2078353082761,37.5889285998978,43.4380523793772,47.7922801314481,39.884993830882,60.365402426552,57.5910897401534,45.9058848614618,58.4811273514293,50.7732514558546,40.4330103839189,43.2128139582276,32.5298805546015],"text":["continent: Asia<br />wm: 28.8<br />continent: Asia","continent: Asia<br />wm: 50.9<br />continent: Asia","continent: Asia<br />wm: 37.5<br />continent: Asia","continent: Asia<br />wm: 39.4<br />continent: Asia","continent: Asia<br />wm: 44.0<br />continent: Asia","continent: Asia<br />wm: 61.0<br />continent: Asia","continent: Asia<br />wm: 37.4<br />continent: Asia","continent: Asia<br />wm: 37.5<br />continent: Asia","continent: Asia<br />wm: 44.9<br />continent: Asia","continent: Asia<br />wm: 45.3<br />continent: Asia","continent: Asia<br />wm: 65.4<br />continent: Asia","continent: Asia<br />wm: 63.0<br />continent: Asia","continent: Asia<br />wm: 43.2<br />continent: Asia","continent: Asia<br />wm: 50.1<br />continent: Asia","continent: Asia<br />wm: 47.5<br />continent: Asia","continent: Asia<br />wm: 55.6<br />continent: Asia","continent: Asia<br />wm: 55.9<br />continent: Asia","continent: Asia<br />wm: 48.5<br />continent: Asia","continent: Asia<br />wm: 42.2<br />continent: Asia","continent: Asia<br />wm: 36.3<br />continent: Asia","continent: Asia<br />wm: 36.2<br />continent: Asia","continent: Asia<br />wm: 37.6<br />continent: Asia","continent: Asia<br />wm: 43.4<br />continent: Asia","continent: Asia<br />wm: 47.8<br />continent: Asia","continent: Asia<br />wm: 39.9<br />continent: Asia","continent: Asia<br />wm: 60.4<br />continent: Asia","continent: Asia<br />wm: 57.6<br />continent: Asia","continent: Asia<br />wm: 45.9<br />continent: Asia","continent: Asia<br />wm: 58.5<br />continent: Asia","continent: Asia<br />wm: 50.8<br />continent: Asia","continent: Asia<br />wm: 40.4<br />continent: Asia","continent: Asia<br />wm: 43.2<br />continent: Asia","continent: Asia<br />wm: 32.5<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.0052395824343,2.83882921487093,3.30271736830473,2.66959011163563,2.83397019784898,2.85168268140405,2.78786246329546,2.96025048196316,2.92508551198989,2.69138075374067,3.3002765905112,2.81605138778687,2.66453501787037,3.29262004178017,2.7555876865983,2.74403491131961,2.99249545112252,3.08161581270397,3.25200045891106,2.72329234145582,2.94731959439814,2.86073850654066,2.99265325814486,2.92155781053007,3.37243958860636,2.93520351313055,2.80965306311846,3.2523851942271,3.07392605729401,3.14826590735465,3.12956776823849,3.29903020001948,3.2479394229129],"y":[38.3741307748854,65.6162393699028,46.925511605218,31.1812728101388,64.0322551696748,73.5948567952588,54.1716634701006,52.7131013469212,57.7096555342153,60.3980453368276,73.1337556715682,75.392190876361,61.1204277384281,67.1823287253082,64.7601642330177,69.2712122615613,66.122193873208,65.3395397137664,55.4678012795933,56.1176707419753,46.6783864559606,57.4218774712086,54.0180948270112,60.1076149442419,58.7197550689615,70.8230768751539,65.9295434466377,61.2379309912212,70.5831154436059,62.5016511145979,55.7843019767106,60.8363725346513,44.1802359007299],"text":["continent: Asia<br />wm: 38.4<br />continent: Asia","continent: Asia<br />wm: 65.6<br />continent: Asia","continent: Asia<br />wm: 46.9<br />continent: Asia","continent: Asia<br />wm: 31.2<br />continent: Asia","continent: Asia<br />wm: 64.0<br />continent: Asia","continent: Asia<br />wm: 73.6<br />continent: Asia","continent: Asia<br />wm: 54.2<br />continent: Asia","continent: Asia<br />wm: 52.7<br />continent: Asia","continent: Asia<br />wm: 57.7<br />continent: Asia","continent: Asia<br />wm: 60.4<br />continent: Asia","continent: Asia<br />wm: 73.1<br />continent: Asia","continent: Asia<br />wm: 75.4<br />continent: Asia","continent: Asia<br />wm: 61.1<br />continent: Asia","continent: Asia<br />wm: 67.2<br />continent: Asia","continent: Asia<br />wm: 64.8<br />continent: Asia","continent: Asia<br />wm: 69.3<br />continent: Asia","continent: Asia<br />wm: 66.1<br />continent: Asia","continent: Asia<br />wm: 65.3<br />continent: Asia","continent: Asia<br />wm: 55.5<br />continent: Asia","continent: Asia<br />wm: 56.1<br />continent: Asia","continent: Asia<br />wm: 46.7<br />continent: Asia","continent: Asia<br />wm: 57.4<br />continent: Asia","continent: Asia<br />wm: 54.0<br />continent: Asia","continent: Asia<br />wm: 60.1<br />continent: Asia","continent: Asia<br />wm: 58.7<br />continent: Asia","continent: Asia<br />wm: 70.8<br />continent: Asia","continent: Asia<br />wm: 65.9<br />continent: Asia","continent: Asia<br />wm: 61.2<br />continent: Asia","continent: Asia<br />wm: 70.6<br />continent: Asia","continent: Asia<br />wm: 62.5<br />continent: Asia","continent: Asia<br />wm: 55.8<br />continent: Asia","continent: Asia<br />wm: 60.8<br />continent: Asia","continent: Asia<br />wm: 44.2<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.20302801840007,2.78272158168256,3.2741324223578,2.99132850375026,3.31359656844288,2.64187440220267,2.88378497958183,3.18306322842836,3.15366106126457,2.63152213785797,2.86066823713481,3.22192128375173,3.13467928301543,2.74163340162486,3.32068678028882,2.61671250611544,2.90833943132311,2.65690692979842,3.25466185864061,2.87220518384129,2.72161006387323,3.19924670122564,2.83650917951018,2.73174902740866,3.38603252768517,2.99056713338941,3.32376980092376,3.19880880042911,3.0516660714522,2.72936344835907,2.72873665895313,3.07296401429921,2.73859914708883],"y":[42.1133803597651,74.8363969845138,61.968460394647,56.8261800133809,71.9691385388747,81.4622939023375,62.8794962552749,68.6376233977824,69.4700161526725,57.007591844704,79.6947945333831,82.028687875662,71.3004984701611,66.707499439083,76.9998564924113,76.920699514728,70.9895409448817,72.9976521101035,65.0310962001979,59.9053988637216,61.2907074936666,74.2018934900314,63.5956685815565,70.3344389589876,71.5744319883548,78.7671284386329,70.7764809916727,73.0851896010525,76.9682063158043,68.6123319128901,72.9976680756174,72.4214190957695,60.2758480953984],"text":["continent: Asia<br />wm: 42.1<br />continent: Asia","continent: Asia<br />wm: 74.8<br />continent: Asia","continent: Asia<br />wm: 62.0<br />continent: Asia","continent: Asia<br />wm: 56.8<br />continent: Asia","continent: Asia<br />wm: 72.0<br />continent: Asia","continent: Asia<br />wm: 81.5<br />continent: Asia","continent: Asia<br />wm: 62.9<br />continent: Asia","continent: Asia<br />wm: 68.6<br />continent: Asia","continent: Asia<br />wm: 69.5<br />continent: Asia","continent: Asia<br />wm: 57.0<br />continent: Asia","continent: Asia<br />wm: 79.7<br />continent: Asia","continent: Asia<br />wm: 82.0<br />continent: Asia","continent: Asia<br />wm: 71.3<br />continent: Asia","continent: Asia<br />wm: 66.7<br />continent: Asia","continent: Asia<br />wm: 77.0<br />continent: Asia","continent: Asia<br />wm: 76.9<br />continent: Asia","continent: Asia<br />wm: 71.0<br />continent: Asia","continent: Asia<br />wm: 73.0<br />continent: Asia","continent: Asia<br />wm: 65.0<br />continent: Asia","continent: Asia<br />wm: 59.9<br />continent: Asia","continent: Asia<br />wm: 61.3<br />continent: Asia","continent: Asia<br />wm: 74.2<br />continent: Asia","continent: Asia<br />wm: 63.6<br />continent: Asia","continent: Asia<br />wm: 70.3<br />continent: Asia","continent: Asia<br />wm: 71.6<br />continent: Asia","continent: Asia<br />wm: 78.8<br />continent: Asia","continent: Asia<br />wm: 70.8<br />continent: Asia","continent: Asia<br />wm: 73.1<br />continent: Asia","continent: Asia<br />wm: 77.0<br />continent: Asia","continent: Asia<br />wm: 68.6<br />continent: Asia","continent: Asia<br />wm: 73.0<br />continent: Asia","continent: Asia<br />wm: 72.4<br />continent: Asia","continent: Asia<br />wm: 60.3<br />continent: Asia"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(77,175,74,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(77,175,74,1)"}},"hoveron":"points","name":"Asia","legendgroup":"Asia","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.3515094269067,3.88659865483642,3.70864865295589,4.2235884655267,4.23324788827449,3.85371158793569,4.06266164109111,4.37383886799216,4.07425974495709,3.74475479889661,4.18139858394861,4.00325470734388,4.26997707784176,4.12754908744246,4.18337057400495,4.17739689815789,4.20865381900221,4.26361653842032,4.06885823775083,3.92883278559893,4.37978039328009,4.22241327986121,4.29756474457681,4.37355203106999,4.21084184907377,4.36036520861089,4.26934124846011,4.27390635851771,3.95998318679631,3.66334886047989],"y":[55.2118334499747,66.8237991317175,68.0167854203843,53.8259762241878,59.5624308397993,61.232290219944,66.9348789094202,70.7883737878129,66.4806252558157,67.4169607418031,67.4636131857149,65.891355626192,63.9987070735171,72.4842203134298,66.8908161580563,65.8607803487033,59.208029718753,72.081298245918,72.6604020430706,61.2693109191209,59.8278648615256,61.0185757081211,57.9736903805472,64.4123171058483,65.5817534722947,64.8888136931136,71.8885885919817,69.6277659489959,43.5918067232147,69.1985871448368],"text":["continent: Europe<br />wm: 55.2<br />continent: Europe","continent: Europe<br />wm: 66.8<br />continent: Europe","continent: Europe<br />wm: 68.0<br />continent: Europe","continent: Europe<br />wm: 53.8<br />continent: Europe","continent: Europe<br />wm: 59.6<br />continent: Europe","continent: Europe<br />wm: 61.2<br />continent: Europe","continent: Europe<br />wm: 66.9<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 66.5<br />continent: Europe","continent: Europe<br />wm: 67.4<br />continent: Europe","continent: Europe<br />wm: 67.5<br />continent: Europe","continent: Europe<br />wm: 65.9<br />continent: Europe","continent: Europe<br />wm: 64.0<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 66.9<br />continent: Europe","continent: Europe<br />wm: 65.9<br />continent: Europe","continent: Europe<br />wm: 59.2<br />continent: Europe","continent: Europe<br />wm: 72.1<br />continent: Europe","continent: Europe<br />wm: 72.7<br />continent: Europe","continent: Europe<br />wm: 61.3<br />continent: Europe","continent: Europe<br />wm: 59.8<br />continent: Europe","continent: Europe<br />wm: 61.0<br />continent: Europe","continent: Europe<br />wm: 58.0<br />continent: Europe","continent: Europe<br />wm: 64.4<br />continent: Europe","continent: Europe<br />wm: 65.6<br />continent: Europe","continent: Europe<br />wm: 64.9<br />continent: Europe","continent: Europe<br />wm: 71.9<br />continent: Europe","continent: Europe<br />wm: 69.6<br />continent: Europe","continent: Europe<br />wm: 43.6<br />continent: Europe","continent: Europe<br />wm: 69.2<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.14573737569153,4.11406410858035,3.76408595666289,3.88782949242741,4.06819653213024,4.02198861148208,3.98955535180867,3.60059853196144,4.37516366895288,3.63290787674487,4.10803115535527,3.91523194350302,3.83127245586365,3.9705125907436,4.19208075646311,3.89468038789928,4.3506356401369,3.84694821741432,4.05326611083001,3.84040712565184,4.20244393181056,3.80148331783712,3.76577738653868,3.74348230268806,3.95344352182001,4.2147506184876,3.75717366393656,4.26870145723224,4.2064483569935,4.07069057971239],"y":[68.9051777508669,72.1651672531292,72.793878310807,69.9040497390926,70.7661244497076,70.5603815272264,70.7252091154829,74.6601733881421,72.4624417386204,73.7847398119979,72.5001173251867,73.700531403292,70.0355593872815,76.0992960118316,71.9668606494367,73.4751491173729,73.1330635349266,75.2245761700906,75.3612383758277,70.683015492782,70.3726278946363,69.498856438864,70.3279421669803,70.4791333205812,71.0359743571095,74.4012037758529,75.4017616613768,75.4212144578248,59.4885261129402,72.7626233336702],"text":["continent: Europe<br />wm: 68.9<br />continent: Europe","continent: Europe<br />wm: 72.2<br />continent: Europe","continent: Europe<br />wm: 72.8<br />continent: Europe","continent: Europe<br />wm: 69.9<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 70.6<br />continent: Europe","continent: Europe<br />wm: 70.7<br />continent: Europe","continent: Europe<br />wm: 74.7<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 73.8<br />continent: Europe","continent: Europe<br />wm: 72.5<br />continent: Europe","continent: Europe<br />wm: 73.7<br />continent: Europe","continent: Europe<br />wm: 70.0<br />continent: Europe","continent: Europe<br />wm: 76.1<br />continent: Europe","continent: Europe<br />wm: 72.0<br />continent: Europe","continent: Europe<br />wm: 73.5<br />continent: Europe","continent: Europe<br />wm: 73.1<br />continent: Europe","continent: Europe<br />wm: 75.2<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 70.7<br />continent: Europe","continent: Europe<br />wm: 70.4<br />continent: Europe","continent: Europe<br />wm: 69.5<br />continent: Europe","continent: Europe<br />wm: 70.3<br />continent: Europe","continent: Europe<br />wm: 70.5<br />continent: Europe","continent: Europe<br />wm: 71.0<br />continent: Europe","continent: Europe<br />wm: 74.4<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 75.4<br />continent: Europe","continent: Europe<br />wm: 59.5<br />continent: Europe","continent: Europe<br />wm: 72.8<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.9785974515602,4.34728690832853,3.97118797991425,4.32880653310567,3.6798679664731,3.79130593668669,3.76893252525479,4.35559279229492,4.12746249213815,4.063306411542,3.66461437586695,3.89441417790949,3.84649140648544,4.03459761757404,4.29121996872127,4.14808197300881,4.19820923451334,3.87316506020725,4.08993669357151,3.85933259427547,3.90765104461461,4.00654564220458,4.18877851329744,3.91728221476078,3.99494481906295,3.85107716843486,4.39574896097183,3.72259942758828,4.27457163501531,3.78920251112431],"y":[75.7169890355505,78.9760685892589,78.2876598553173,74.1313190801814,72.0616028890014,74.9296593264677,75.5345570546761,77.2326394427009,78.3909070709162,79.5797605313174,78.6772889348865,78.3011234269291,72.5894621565193,80.4939753693901,77.8120326601714,80.2007314939238,74.0396922644414,78.4903587303869,78.9700244487263,74.7018016885221,77.2940711048618,71.2983266295679,73.169499146156,73.8030648292415,76.7102930984832,79.7773147571646,80.0119785003923,80.6181408584677,70.7739184515364,78.5375094419159],"text":["continent: Europe<br />wm: 75.7<br />continent: Europe","continent: Europe<br />wm: 79.0<br />continent: Europe","continent: Europe<br />wm: 78.3<br />continent: Europe","continent: Europe<br />wm: 74.1<br />continent: Europe","continent: Europe<br />wm: 72.1<br />continent: Europe","continent: Europe<br />wm: 74.9<br />continent: Europe","continent: Europe<br />wm: 75.5<br />continent: Europe","continent: Europe<br />wm: 77.2<br />continent: Europe","continent: Europe<br />wm: 78.4<br />continent: Europe","continent: Europe<br />wm: 79.6<br />continent: Europe","continent: Europe<br />wm: 78.7<br />continent: Europe","continent: Europe<br />wm: 78.3<br />continent: Europe","continent: Europe<br />wm: 72.6<br />continent: Europe","continent: Europe<br />wm: 80.5<br />continent: Europe","continent: Europe<br />wm: 77.8<br />continent: Europe","continent: Europe<br />wm: 80.2<br />continent: Europe","continent: Europe<br />wm: 74.0<br />continent: Europe","continent: Europe<br />wm: 78.5<br />continent: Europe","continent: Europe<br />wm: 79.0<br />continent: Europe","continent: Europe<br />wm: 74.7<br />continent: Europe","continent: Europe<br />wm: 77.3<br />continent: Europe","continent: Europe<br />wm: 71.3<br />continent: Europe","continent: Europe<br />wm: 73.2<br />continent: Europe","continent: Europe<br />wm: 73.8<br />continent: Europe","continent: Europe<br />wm: 76.7<br />continent: Europe","continent: Europe<br />wm: 79.8<br />continent: Europe","continent: Europe<br />wm: 80.0<br />continent: Europe","continent: Europe<br />wm: 80.6<br />continent: Europe","continent: Europe<br />wm: 70.8<br />continent: Europe","continent: Europe<br />wm: 78.5<br />continent: Europe"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(152,78,163,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(152,78,163,1)"}},"hoveron":"points","name":"Europe","legendgroup":"Europe","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.9637871209532,4.67090329267085],"y":[69.1025924686342,69.3949490147643],"text":["continent: Oceania<br />wm: 69.1<br />continent: Oceania","continent: Oceania<br />wm: 69.4<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[5.12851539589465,5.09692273531109],"y":[73.5010209324956,72.2233386617713],"text":["continent: Oceania<br />wm: 73.5<br />continent: Oceania","continent: Oceania<br />wm: 72.2<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":false,"xaxis":"x2","yaxis":"y","hoverinfo":"text","frame":null},{"x":[4.99259671792388,5.26606295574456],"y":[80.4291106511094,79.1102579840086],"text":["continent: Oceania<br />wm: 80.4<br />continent: Oceania","continent: Oceania<br />wm: 79.1<br />continent: Oceania"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(255,127,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(255,127,0,1)"}},"hoveron":"points","name":"Oceania","legendgroup":"Oceania","showlegend":false,"xaxis":"x3","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":62.3594852635948,"r":25.9028642590286,"b":71.9231828878753,"l":38.854296388543},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Figure 2: Weighted mean of life expectancy (by population) across continents","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,0.322461404653185],"automargin":true,"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"annotations":[{"text":"Continent","x":0.5,"y":-0.113310283984443,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"top","annotationType":"axis"},{"text":"Life expectancy","x":-0.0360750360750361,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-90,"xanchor":"right","yanchor":"center","annotationType":"axis"},{"text":"1952","x":0.161230702326593,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"1977","x":0.5,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"2002","x":0.838769297673407,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(26,26,26,1)","family":"","size":18.5969281859693},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"center","yanchor":"bottom"},{"text":"continent","x":1.02,"y":1,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[26.1296681618728,84.690545957271],"tickmode":"array","ticktext":["30","35","40","45","50","55","60","65","70","75","80"],"tickvals":[30,35,40,45,50,55,60,65,70,75,80],"categoryorder":"array","categoryarray":["30","35","40","45","50","55","60","65","70","75","80"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":13.2835201328352},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":0.322461404653185,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":0.322461404653185,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.344205262013481,"x1":0.655794737986519,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.344205262013481,"x1":0.655794737986519,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"},{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0.677538595346814,"x1":1,"y0":0,"y1":1},{"type":"rect","fillcolor":"rgba(255,255,255,1)","line":{"color":"rgba(255,255,255,1)","width":1.32835201328352,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0.677538595346814,"x1":1,"y0":0,"y1":30.2864259028643,"yanchor":1,"ysizemode":"pixel"}],"xaxis2":{"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.344205262013481,0.655794737986519],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"xaxis3":{"type":"linear","autorange":false,"range":[0.4,5.6],"tickmode":"array","ticktext":["Africa","Americas","Asia","Europe","Oceania"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Africa","Americas","Asia","Europe","Oceania"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":10.6268161062682},"tickangle":-65,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"domain":[0.677538595346814,1],"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"y":0.913385826771654},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"515239256c06":{"x":{},"y":{},"colour":{},"type":"scatter"}},"cur_data":"515239256c06","visdat":{"515239256c06":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>

<!--/html_preserve-->

*Comments:* The improved `ggplot` is more visually impactful by
stripping away unnecessary elements, resizing/relabelling, and providing
more appealing color scheme.

The `plotly` plot is even more impactful in that it is a dynamic image
that allows the viewer to effectively interact with the plotted data.
The viewer can observe the values that underlie each point, zoom to a
specific region of the plot, or remove geometric layers.

# Part 4: Writing figures to file

Now, we save the plots from above to
file.

``` r
ggsave("plot_raster.jpg", plot = P_new, width = 8, height = 4, dpi = 100) #save as a raster file format (.jpg)
ggsave("plot_vector.eps", plot = P_new) #save as a vector file format (.eps)
```

    ## Saving 7 x 5 in image

Here is the raster plot image embedded into the
file:

![plot\_raster.jpg](https://github.com/STAT545-UBC-students/hw05-bassamjaved/blob/master/plot_raster.jpg)

# But I want to do more\!

*Preamble:* Let’s explore other functions in the forcats package for
manipulating factors. I will use an excerpt of countries in the
gapminder data frame and add language spoken.

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

*Comments:* `fct_collapse()` can collapse multiple factor levels into a
new one. Here I put “Italian” and “Arabic” into a new one called
“Other”. `fct_recode()` allows a factor level to be renamed. Here I
changed “Spanish” to “Espanol”. Finally, `fct_relevel()` changes the
order manually. Here I put “French” first.

## End of file
