Where the Money Goes: A Tale of Two Programs at SEC Schools
================
Sabrina Sandy
Tuesday April 4th 2023

### Datasets

**EADA Data** the SEC basketball data I collected came from the Equity
in Athletics Data Analysis (EADA) Cutting Tool from the Office of
Postsecondary Education of the U.S. Department of Education. The tool
provides the ability for users to select specific instituions and
collect, revenue, spending, and participant data across a multitude of
collegiate athletics. The data is drawn from the OPE Equity in Athletics
Disclosure Website database. This database consists of athletics data
that are submitted annually as required by the Equity in Athletics
Disclosure Act (EADA), via a Web-based data collection, by all
co-educational postsecondary institutions that receive Title IV funding
and that have an intercollegiate athletics program.

**Knight Commission Data** Syracuse University’s Newhouse School of
Public Commuications provides a College Athletics Database with an
overview of Athletic and Academic finances from the Knight Commission.
The data in this database is self-reported by institutions on NCAA
financial reports and on the same such EADA data. I used this resource
specifically to get a lower level look at football programs
specifically.

### Research Questions

> To what extent does academic funding affect universities’ sports teams
> funding levels?

> How do changes in athletic recruitment fees at a specific college
> impact the revenue from athletic events?

> What is the difference in spending on facilities, coaching salaries,
> and other resources for women’s basketball and SEC football at a
> particular college, and how has this impacted the success of each
> program?

``` r
# installing packages needed for this project

library(tidyr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(plotly)
library(corrplot)
library(RColorBrewer)
```

``` r
# read in both data sets

#sec data from knight newhouse
SEC_FB_df <- readxl::read_excel("datasets/Custom_Reporting_data_Sec_FB.xls")
SEC_FB_df
```

    ## # A tibble: 52 × 21
    ##    Data    `IPEDS ID`  Year `NCAA Subdivision` `FBS Conference` `Total Expenses`
    ##    <chr>        <dbl> <dbl> <chr>              <chr>                       <dbl>
    ##  1 Auburn…     100858  2016 Football Bowl Sub… Southeastern Co…        124864399
    ##  2 Auburn…     100858  2017 Football Bowl Sub… Southeastern Co…        132885979
    ##  3 Auburn…     100858  2018 Football Bowl Sub… Southeastern Co…        139798191
    ##  4 Auburn…     100858  2019 Football Bowl Sub… Southeastern Co…        139260711
    ##  5 Louisi…     159391  2016 Football Bowl Sub… Southeastern Co…        123952910
    ##  6 Louisi…     159391  2017 Football Bowl Sub… Southeastern Co…        139139017
    ##  7 Louisi…     159391  2018 Football Bowl Sub… Southeastern Co…        145351316
    ##  8 Louisi…     159391  2019 Football Bowl Sub… Southeastern Co…        157925522
    ##  9 Missis…     176080  2016 Football Bowl Sub… Southeastern Co…         87641937
    ## 10 Missis…     176080  2017 Football Bowl Sub… Southeastern Co…         88138943
    ## # ℹ 42 more rows
    ## # ℹ 15 more variables: Recruiting <dbl>, `Game Expenses and Travel` <dbl>,
    ## #   `Facilities and Equipment` <dbl>, `Coaches Compensation` <dbl>,
    ## #   `Athletic Student Aid` <dbl>,
    ## #   `Corporate Sponsorship, Advertising, Licensing` <dbl>,
    ## #   `NCAA/Conference Distributions, Media Rights, and Post-Season Football` <dbl>,
    ## #   `Ticket Sales` <dbl>, `Institutional/Government Support` <dbl>, …

``` r
#equity in athletics data analysis

Basketball_Data <- read_csv("datasets/Sport_Data_2016_2017_2018_2019.csv")
Basketball_Data
```

    ## # A tibble: 1,900 × 48
    ##    `Survey Year` UNITID `OPE ID` `Institution Name`           `State CD`
    ##            <dbl>  <dbl> <chr>    <chr>                        <chr>     
    ##  1          2016 222178 00353700 Abilene Christian University TX        
    ##  2          2016 100654 00100200 Alabama A & M University     AL        
    ##  3          2016 100724 00100500 Alabama State University     AL        
    ##  4          2016 138716 00154400 Albany State University      GA        
    ##  5          2016 138682 00560100 Albany Technical College     GA        
    ##  6          2016 175342 00239600 Alcorn State University      MS        
    ##  7          2016 156189 00195100 Alice Lloyd College          KY        
    ##  8          2016 217624 00341700 Allen University             SC        
    ##  9          2016 217633 00341800 Anderson University          SC        
    ## 10          2016 138761 00154500 Andrew College               GA        
    ## # ℹ 1,890 more rows
    ## # ℹ 43 more variables: `Classification Name` <dbl>,
    ## #   `Classification Other` <chr>, `Sanction Code` <dbl>, `Sanction Name` <chr>,
    ## #   `Male Undergraduates` <dbl>, `Female Undergraduates` <dbl>,
    ## #   `Total Undergraduates` <dbl>, `Sport Code` <dbl>, `Sport Name` <chr>,
    ## #   `# Participants Men's Team` <dbl>, `# Participants Women's Team` <dbl>,
    ## #   `# Participants Men Coed Team` <lgl>, …

#### To what extent do universities with higher funding levels for sports teams spend less on academic programs compared to universities with lower funding levels for sports teams?

``` r
# arrange by football spending so we can hone in on 3 institutions

SEC_FB_df %>%
  arrange(`Total Football Spending`)
```

    ## # A tibble: 52 × 21
    ##    Data    `IPEDS ID`  Year `NCAA Subdivision` `FBS Conference` `Total Expenses`
    ##    <chr>        <dbl> <dbl> <chr>              <chr>                       <dbl>
    ##  1 Univer…     178396  2016 Football Bowl Sub… Southeastern Co…         94323693
    ##  2 Univer…     178396  2017 Football Bowl Sub… Southeastern Co…        102409131
    ##  3 Univer…     157085  2016 Football Bowl Sub… Southeastern Co…        129873437
    ##  4 Missis…     176080  2016 Football Bowl Sub… Southeastern Co…         87641937
    ##  5 Missis…     176080  2017 Football Bowl Sub… Southeastern Co…         88138943
    ##  6 Missis…     176080  2018 Football Bowl Sub… Southeastern Co…         93573141
    ##  7 Univer…     178396  2018 Football Bowl Sub… Southeastern Co…        109158522
    ##  8 Univer…     178396  2019 Football Bowl Sub… Southeastern Co…        108398447
    ##  9 Missis…     176080  2019 Football Bowl Sub… Southeastern Co…        104198109
    ## 10 Univer…     157085  2017 Football Bowl Sub… Southeastern Co…        128829711
    ## # ℹ 42 more rows
    ## # ℹ 15 more variables: Recruiting <dbl>, `Game Expenses and Travel` <dbl>,
    ## #   `Facilities and Equipment` <dbl>, `Coaches Compensation` <dbl>,
    ## #   `Athletic Student Aid` <dbl>,
    ## #   `Corporate Sponsorship, Advertising, Licensing` <dbl>,
    ## #   `NCAA/Conference Distributions, Media Rights, and Post-Season Football` <dbl>,
    ## #   `Ticket Sales` <dbl>, `Institutional/Government Support` <dbl>, …

``` r
# select data

SEC_v_students <- SEC_FB_df %>%
                    filter(Data == "University of Missouri-Columbia" | 
                           Data == "Louisiana State University and Agricultural & Mechanical College"|
                           Data == "The University of Alabama") %>%
                    select(Data, Year, `Total Football Spending`, `Total Academic Spending (University-Wide)`, `Ticket Sales`)


# calculate totals so we can get percentages

SEC_v_students$Total <- rowSums(SEC_v_students[, c("Total Football Spending", "Total Academic Spending (University-Wide)", "Ticket Sales")])


# reshape data so we can create a grouped bar chart

SEC_v_students_longer <- pivot_longer(SEC_v_students, cols = c(
  `Total Football Spending`, `Total Academic Spending (University-Wide)`, `Ticket Sales`),
  names_to = "Category", values_to = "Amount")


# calculate percentages

SEC_v_students_longer$Amount <- SEC_v_students_longer$Amount / SEC_v_students_longer$Total * 100


sec_v_students_graphs <- SEC_v_students_longer %>%
  ggplot(aes(x = Data, y = `Amount`, fill = `Category`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(labels= c("LSU", "U Alabama", "Mizzou")) +
  theme(axis.text.x = element_text(angle = 45, size = 6, vjust = 0.6),
        legend.key.size = unit(0.5, 'cm')) +
  theme_minimal() +
  labs(title = "Do ticket sales change if you spend more on academics?", subtitle = 'Year: {closest_state}', x = 'University', y = 'Percentage')


anim <- sec_v_students_graphs + transition_states(Year, transition_length = 2, state_length = 1) + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim
```

<img src="where_the_money_goes_github_files/figure-gfm/unnamed-chunk-3-1.gif" style="display: block; margin: auto;" />

``` r
anim_save("output/sec_v_students_gganimate.gif")
```

> The audience targeted for this animated ggplot is SEC students
> themselves. I initial aimed to convey that there was a disparity
> between academic spending and atheltics spending but I foolishly
> thought the athletic spending would be disproportionate when in
> reality it’s pretty tame. So now it conveys that increased academic
> spending doesn’t come at the detriment of athletics spending. So one
> can have their cake and eat it too. Again since I thought the data was
> going to look a certain way I wanted the grouped bar charts but I
> think the animation really aides in displaying the passage of time and
> the change in spending that comes with that.

#### How do changes in athletic recruitment fees at a specific college impact the revenue from athletic events?

``` r
# wrangling data

LSU_data <- SEC_FB_df %>%
                    filter(Data == "Louisiana State University and Agricultural & Mechanical College") %>%
                    select(Data, Year, `Recruiting`, `Coaches Compensation`, `Ticket Sales`, `Corporate Sponsorship, Advertising, Licensing`, `NCAA/Conference Distributions, Media Rights, and Post-Season Football`)

LSU_data$Total_Revenue <- rowSums(LSU_data[, c("Ticket Sales", "Corporate Sponsorship, Advertising, Licensing", "NCAA/Conference Distributions, Media Rights, and Post-Season Football")]) 

# reshaping the data to group things again

LSU_data_reshaped <- LSU_data %>%
                      pivot_longer(
                        cols = c("Ticket Sales", "NCAA/Conference Distributions, Media Rights, and Post-Season Football"),
                        names_to = "Rev_Source",
                        values_to = "Rev_Amount"
                      )
LSU_data_reshaped
```

    ## # A tibble: 8 × 8
    ##   Data             Year Recruiting `Coaches Compensation` Corporate Sponsorshi…¹
    ##   <chr>           <dbl>      <dbl>                  <dbl>                  <dbl>
    ## 1 Louisiana Stat…  2016    1948299               22701535                3306735
    ## 2 Louisiana Stat…  2016    1948299               22701535                3306735
    ## 3 Louisiana Stat…  2017    1875822               25370481                3299132
    ## 4 Louisiana Stat…  2017    1875822               25370481                3299132
    ## 5 Louisiana Stat…  2018    2661602               26957740                3073470
    ## 6 Louisiana Stat…  2018    2661602               26957740                3073470
    ## 7 Louisiana Stat…  2019    3200886               28897786                2993051
    ## 8 Louisiana Stat…  2019    3200886               28897786                2993051
    ## # ℹ abbreviated name: ¹​`Corporate Sponsorship, Advertising, Licensing`
    ## # ℹ 3 more variables: Total_Revenue <dbl>, Rev_Source <chr>, Rev_Amount <dbl>

``` r
# create a scatter plot 
LSU_plot <- LSU_data_reshaped %>%
  ggplot(aes(x = Recruiting, y = Rev_Amount, group = Rev_Source, colour = Rev_Source)) + 
    geom_line() +
    scale_x_continuous(labels = scales::comma, 
                     name = "Athletic Recruitment Fees (Millions)") +
    scale_y_continuous(labels = scales::comma, 
                     name = "Revenue (Millions)") +
    scale_color_manual(values = c("#a08ec1","#c1a73b")) +
    labs(title = "The Relationship Between Recruitment Fees and Athletic Event Revenue",
         subtitle = "at Louisiana State University",
         fill = "Rev_Source",
         colour = "Revenue Source") +
    scale_fill_discrete(labels=c('NCAA/Conference Distribution', 'Ticket Sales')) +
    theme_light() +
    theme(axis.text.x = element_text(angle = 45, size = 6, vjust = 0.6),aspect.ratio=1)
    

LSU_plot
```

<img src="where_the_money_goes_github_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
ggsave("output/LSU_plot.png")
```

    ## Saving 8 x 8 in image

> The audience targeted for this plot is Athletic coordinators or
> whoever’s incharge of doling out the funding for the athletic
> programs. I would assume that these kind of people enjoy straight to
> the point cut and dry graphs, with few variables. Also line graphs
> evoke a kind of financial feeling so that’s why I decided to display
> this as such. As well as using the LSU tigers colours of purple and
> gold.

#### What is the difference in spending on facilities, coaching salaries, and other resources for women’s basketball and SEC football at a particular college, and how has this impacted the success of each program?

``` r
Basketball_Data_F <- Basketball_Data %>%
  select(matches("Women|Female|Survey|Institution") & !matches("Coed")) %>%
  filter(`Institution Name` == "Auburn University" |
         `Institution Name` == "Louisiana State University and Agricultural & Mechanical College" |
         `Institution Name` == "Mississippi State University" |
         `Institution Name` == "Texas A & M University" |
         `Institution Name` == "The University of Alabama" |
         `Institution Name` == "The University of Tennessee" |
         `Institution Name` == "University of Arkansas" |
         `Institution Name` == "University of Florida" |
         `Institution Name` == "University of Georgia" |
         `Institution Name` == "University of Kentucky" ) %>%
  rename(Year = `Survey Year`, Data = `Institution Name`)

SEC_FB_df$Total_Revenue <- rowSums(SEC_FB_df[, c("Ticket Sales", "Corporate Sponsorship, Advertising, Licensing", "NCAA/Conference Distributions, Media Rights, and Post-Season Football")]) 


Basketball_v_Football <- Basketball_Data_F %>%
              left_join(SEC_FB_df)
```

    ## Joining with `by = join_by(Year, Data)`

``` r
B_v_F_wrangled <- Basketball_v_Football %>%
  select(`Year`,
         `Data`,
         `Operating Expenses per Team/Women's Team`, 
         `Revenues Women's Team`,
         `Total Football Spending`,
         `Total_Revenue`)
  
Basketball_v_Football_graph <- ggplot(B_v_F_wrangled, aes(x = `Operating Expenses per Team/Women's Team`, y = `Revenues Women's Team`, size = `Total Football Spending`, color = `Total_Revenue`)) +
  geom_point(aes(alpha = 0.2)) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_size_continuous(labels = scales::comma) +
  scale_color_gradient(low = "#e2bf99", high = "#a5dab8", labels = scales::comma) +
  labs(title = "Comparing Spending on Women's Basketball and Men's Football",
       subtitle = "At SEC Conference Schools",
       x = "Operating Expenses Women's Basketball Team (Millions)",
       y = "Revenues Women's Baketball Team (Millions)",
       size = "Total Football Spending",
       color = "Total_Revenue") +
  theme_classic()

Basketball_v_Football_graph
```

![](where_the_money_goes_github_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ggsave("output/Basketball_v_Football_graph.png")
```

    ## Saving 7 x 5 in image

> The audience targeted for this plot is fans of NCAA womens basketball
> and womens atheltics as a whole. I aimed to convey that funding
> women’s sports in the same way these large SEC schools fund their
> massive football programs is beneficial in terms of revenue. The
> bubble map is great for this because you can see multiple variables at
> a time and the information comes across really clearly.

### Conclusion

I do feel like all three of my visualizations answered my research
questions and were appropriate for the tasks and the audience. This data
is source from a government act which adds to the validitity of it but
since the data set was smallish (and in part because I still haven’t
entirely figured out how to display uncertainty in ggplot) I didn’t do
any error calculations which is a bad practice and is nearly like
serving up misinformation! I did really want to use plotly for the
revenue line graph but there’s a bug with tooltips on linegraphs that
made me pivot back to a static plot. I’m very happy with my work done
here and I think with a cleaned, easy, already put together dataset I
could’ve done far more exciting, aesthetic and insightful things but I
tried to challenge myself and came up short.

### Bibliography

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-colorbrewer" class="csl-entry">

Neuwirth, Erich. 2022. *RColorBrewer: ColorBrewer Palettes*. R package
version 1.1-3. https://CRAN.R-project.org/package=RColorBrewer.

</div>

<div id="ref-gganimate" class="csl-entry">

Pedersen, Thomas Lin, and David Robinson. 2022. *Ganimate: A Grammar of
Animated Graphics*. R package version 1.0.8.
https://CRAN.R-project.org/package=gganimate.

</div>

<div id="ref-plotly" class="csl-entry">

Sievert, Carson. 2020. *Interactive Web-Based Data Visualization with r,
Plotly, and Shiny*. Chapman; Hall/CRC.

</div>

<div id="ref-corrplot" class="csl-entry">

Wei, Taiyun, and Viliam Simko. 2021. *R Package ’Corrplot’:
Visualization of a Correlation Matrix*. (Version 0.92).
https://github.com/taiyun/corrplot.

</div>

<div id="ref-ggplot2" class="csl-entry">

Wickham, Hadley. 2016. *Ggplot2: Elegant Graphics for Data Analysis*.
https://ggplot2.tidyverse.org: Springer-Verlag New York.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686.

</div>

<div id="ref-tidyr" class="csl-entry">

Wickham, Hadley, Davis Vaughan, and Maximilian Girlich. 2023. *Tidyr:
Tidy Messy Data*. R package version 1.3.0.
https://CRAN.R-project.org/package=tidyr.

</div>

</div>

(Wickham et al. 2019)

(Sievert 2020)

(Wickham 2016)

(Neuwirth 2022)

(Wei and Simko 2021)

(Pedersen and Robinson 2022)

(Wickham, Vaughan, and Girlich 2023)

Knight-Newhouse College Athletics Database, a joint project of the
Knight Commission on Intercollegiate Athletics and the S.I. Newhouse
School of Public Communications, Syracuse University.

Equity in Athletics Disclosure Act of 1994. Pub. L. No. 103-382, 108
Stat. 3518 (codified as amended at 20 U.S.C. § 1092
