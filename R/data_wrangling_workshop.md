Data Wrangling in R - Learning Analytics Hackathon Workshop
================

``` r
#install.packages("tidyverse")
```

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Warning: package 'dplyr' was built under R version 3.4.2

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
df <- read_csv("all_enrolments.csv") %>% 
  mutate(id_student = as.character(id_student),
         highest_education = factor(highest_education, 
                                    levels = c("No Formal quals",
                                               "Lower Than A Level",
                                               "A Level or Equivalent",
                                               "HE Qualification",
                                               "Post Graduate Qualification")),
         final_exam_score = as.numeric(final_exam_score))
```

A first look at our data.

``` r
df
```

    ## # A tibble: 32,593 x 20
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 16 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>

Let's answer a question!

Is a registrant's likelihood of completing a course related to the socioeconomic status of the place where they live?

There are two columns in our dataset that can help us answer this question:

-   `imd_band`: the Index of Multiple Depravation band of the place where the student lived during the module-presentation.
-   `final_result`: student’s final result in the module-presentation.

Creating new columns with `mutate()`
------------------------------------

![mutate](img/mutate.png)

Our first task is to create a column that tells us whether a registrant completed a course.

Unfortunately, we don't have a column that tells us this information directly. Instead, we have the `final_result` column, which has four values:

-   `"Distinction"`
-   `"Pass"`
-   `"Withdrawn"`
-   `"Fail"`

If we want to get just whether they completed the course, we'll need to aggregate `"Distinction"` and `"Pass"` into a single value, and `"Withdrawn"` and `"Fail"` into another.

To do this, we need to write code that does two things:

1.  Creates a new column.
2.  Fills that column with values we care about.

This is the jumpshot of data wrangling. In R, the easiest way to do it is to use the `mutate()` function, from the `dplyr` package (one of the packages you loaded as part of the `tidyverse`).

``` r
# We can fill our new column with whatever we like!
df %>% 
  mutate(
    new_column = "hello!"
  )
```

    ## # A tibble: 32,593 x 21
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 17 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>, new_column <chr>

``` r
df %>% 
  mutate(
    new_column = 1
  )
```

    ## # A tibble: 32,593 x 21
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 17 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>, new_column <dbl>

``` r
# We can even use the other columns to determine the contents of the new one.
# (This is where the magic happens!)
df %>% 
  mutate(
    activity_per_active_day = total_activity / days_active
  )
```

    ## # A tibble: 32,593 x 21
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 17 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>,
    ## #   activity_per_active_day <dbl>

### Practice time with `mutate()`!

Challenge: using `mutate()` and the `date_registration` and `date_unregistration` columns, create a new column called `length_of_registration_period`.

``` r
df %>% 
  mutate(
    #YOUR_ANSWER_HERE!
  )
```

    ## # A tibble: 32,593 x 20
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 16 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>

Now. What we actually need to do to answer our question is create a column that tells us whether the student passed their course. A good way to handle this is to use the `case_when()` function.

`case_when()` takes a series of two-side formulas. The left-hand side of each formula is a condition, and the right-hand side is the desired output. For example:

``` r
cool_values <- c(TRUE, FALSE, FALSE)

case_when(
  cool_values == TRUE ~ "hey there!",
  cool_values == FALSE ~ "what's up?"
)
```

    ## [1] "hey there!" "what's up?" "what's up?"

``` r
cool_numbers <- c(1,2,3,4,5,6,7,8,9,10)

case_when(
  cool_numbers <= 5 ~ "small",
  cool_numbers > 5 ~ "BIG!!!!"
)
```

    ##  [1] "small"   "small"   "small"   "small"   "small"   "BIG!!!!" "BIG!!!!"
    ##  [8] "BIG!!!!" "BIG!!!!" "BIG!!!!"

All we need to do now is use `case_when()` within `mutate()` to create a new column that tells us whether the student in each row completed their course:

``` r
# Let's save the result in a new dataframe called `df_mutated`.
df_mutated <- df %>% 
  mutate(
    completed_course = case_when(
      final_result == "Pass" ~ TRUE,
      final_result == "Distinction" ~ TRUE,
      final_result == "Fail" ~ FALSE,
      final_result == "Withdrawn" ~ FALSE
    )
  )

# Let's take a look! (To keep just the variables we care about, we can use 
# `select()`.)
df_mutated %>% 
  select(id_student, final_result, completed_course)
```

    ## # A tibble: 32,593 x 3
    ##    id_student final_result completed_course
    ##         <chr>        <chr>            <lgl>
    ##  1      11391         Pass             TRUE
    ##  2      28400         Pass             TRUE
    ##  3      30268    Withdrawn            FALSE
    ##  4      31604         Pass             TRUE
    ##  5      32885         Pass             TRUE
    ##  6      38053         Pass             TRUE
    ##  7      45462         Pass             TRUE
    ##  8      45642         Pass             TRUE
    ##  9      52130         Pass             TRUE
    ## 10      53025         Pass             TRUE
    ## # ... with 32,583 more rows

We're well on our way. But there is another monster to defeat: there are missing values in both `imd_band` and `completed_course`! What to do?

Subsetting rows with `filter()`
-------------------------------

![filter](img/filter.png)

Technically, there are fancy statistical things you can do to deal with missing values. But today, we're just going to remove the rows where they occur in either of our two columns of interest.

The easiest way to do this in R is to use the `filter()` function, which keeps only the rows in a dataframe that match a condition you pass in. For example:

``` r
example_dataframe <- data.frame(
  course  = c("Physics", "Physics", "English", "English", "Math",  "Math"),
  student = c("Alice",   "Bob",     "Alice",   "Bob",     "Alice", "Bob"),
  grade   = c(85,        74,        81,        89,        93,      87)
)

example_dataframe %>% 
  filter(course == "English")
```

    ##    course student grade
    ## 1 English   Alice    81
    ## 2 English     Bob    89

``` r
example_dataframe %>% 
  filter(course == "English" | course == "Physics")
```

    ##    course student grade
    ## 1 Physics   Alice    85
    ## 2 Physics     Bob    74
    ## 3 English   Alice    81
    ## 4 English     Bob    89

``` r
example_dataframe %>% 
  filter(course == "English" | course == "Physics",
         student == "Alice")
```

    ##    course student grade
    ## 1 Physics   Alice    85
    ## 2 English   Alice    81

``` r
example_dataframe %>% 
  filter(course == "English" | course == "Physics",
         student == "Alice",
         grade > 83)
```

    ##    course student grade
    ## 1 Physics   Alice    85

### Practice time with `filter()`!

Challenge: keep only the subset of rows in our dataframe where the code module was `AAA` and the student was over age 55.

``` r
df %>% 
  filter(
    #YOUR_ANSWER_HERE!
  )
```

    ## # A tibble: 32,593 x 20
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      53025              -179
    ## # ... with 32,583 more rows, and 16 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>

To get at the missing values in our two columns of interest, you may be tempted to write conditions like `imd_band == NA` or `completed_course == NA`. However, in R, the best practice for checking whether a value is missing is to use the function `is.na()`. To return the negation of `is.na()` (or any negation, for that matter), you can use `!`. This turns `TRUE` into `FALSE` and `FALSE into`TRUE\`.

In the code snippet below, I keep only the subset of rows where neither `imd_band` or `completed_course` is missing.

``` r
# Let's save the result in a new dataframe called `df_filtered`.
df_filtered <- df_mutated %>% 
  filter(!is.na(imd_band),
         !is.na(completed_course))

df_filtered
```

    ## # A tibble: 31,482 x 21
    ##    code_module code_presentation id_student date_registration
    ##          <chr>             <chr>      <chr>             <dbl>
    ##  1         AAA             2013J      11391              -159
    ##  2         AAA             2013J      28400               -53
    ##  3         AAA             2013J      30268               -92
    ##  4         AAA             2013J      31604               -52
    ##  5         AAA             2013J      32885              -176
    ##  6         AAA             2013J      38053              -110
    ##  7         AAA             2013J      45462               -67
    ##  8         AAA             2013J      45642               -29
    ##  9         AAA             2013J      52130               -33
    ## 10         AAA             2013J      57506              -103
    ## # ... with 31,472 more rows, and 17 more variables:
    ## #   date_unregistration <dbl>, gender <chr>, region <chr>,
    ## #   highest_education <fctr>, imd_band <chr>, age_band <chr>,
    ## #   num_of_prev_attempts <int>, studied_credits <int>, disability <chr>,
    ## #   final_result <chr>, total_activity <dbl>, days_active <dbl>,
    ## #   last_active_date <dbl>, distinct_content_items_accessed <dbl>,
    ## #   final_assessment_score <dbl>, final_exam_score <dbl>,
    ## #   completed_course <lgl>

Computing summaries of subgroups with `group_by()` & `summarise()`
------------------------------------------------------------------

![group\_by & summarise](img/group_by_summarise.png)

Now we need to count how many people completed their courses across different values of `imd_band`.

To do this, we can use two functions: `group_by()` and `summarise()`.

`group_by()` specifies which variable(s) you want to use to compute summaries within, and `summarise()` squishes the dataframe down to just one row per group, and creates a column with whatever summary value you specify. Here's an example:

``` r
# We can group by course and compute the mean grade.
example_dataframe %>% 
  group_by(course) %>% 
  summarise(mean_grade = mean(grade))
```

    ## # A tibble: 3 x 2
    ##    course mean_grade
    ##    <fctr>      <dbl>
    ## 1 English       85.0
    ## 2    Math       90.0
    ## 3 Physics       79.5

``` r
# Or we can group by students and compute the mean grade.
example_dataframe %>% 
  group_by(student) %>% 
  summarise(mean_grade = mean(grade))
```

    ## # A tibble: 2 x 2
    ##   student mean_grade
    ##    <fctr>      <dbl>
    ## 1   Alice   86.33333
    ## 2     Bob   83.33333

``` r
# Of course, we're not limited to means. We can create other summary values too.
example_dataframe %>% 
  group_by(student) %>% 
  summarise(mean_grade = mean(grade),
            min_grade = min(grade),
            max_grade = max(grade))
```

    ## # A tibble: 2 x 4
    ##   student mean_grade min_grade max_grade
    ##    <fctr>      <dbl>     <dbl>     <dbl>
    ## 1   Alice   86.33333        81        93
    ## 2     Bob   83.33333        74        89

### Practice time with `group_by()` + `summarise()`!

Using our full open dataset, compute the mean number of days (`days_active`) students were active in each course (`code_module`).

``` r
df %>% 
  group_by(
    #YOUR_ANSWER_HERE!
  ) %>% 
  summarise(
    #YOUR_ANSWER_HERE!
  )
```

    ## data frame with 0 columns and 0 rows

To compute the proportion of students in each `imd_band` who completed their courses, we can use a neat trick. Remember: we created a column in which rows were labelled `TRUE` if the student completed the course, and `FALSE` if they didn't. In R, just like in most programming languages, `TRUE` is equivalent to `1`, and `FALSE` is equivalent to `0`. So we can group by `imd_band`, and summarise the mean of our new column, `completed_course`. If a particular `imd_band` group has ten students, and four of them have a value of `TRUE` for `completed_course`, the mean will be `0.4`. Our new column will tell us what proportion of students in that `imd_band` completed the course!

``` r
df_summarised <- df_filtered %>% 
  group_by(imd_band) %>%
  summarise(proportion_completed = mean(completed_course))

df_summarised
```

    ## # A tibble: 10 x 2
    ##    imd_band proportion_completed
    ##       <chr>                <dbl>
    ##  1    0-10%            0.3515554
    ##  2    10-20            0.3862344
    ##  3   20-30%            0.4074986
    ##  4   30-40%            0.4690591
    ##  5   40-50%            0.4659091
    ##  6   50-60%            0.4878361
    ##  7   60-70%            0.5191050
    ##  8   70-80%            0.5151094
    ##  9   80-90%            0.5405503
    ## 10  90-100%            0.5753155

Plotting with `ggplot()`
------------------------

The `ggplot2` package is the best way to create visualizations in R. The code for each visualization comes in two main pieces:

1.  Mapping of variables onto aesthetics (the visual properties of the graph)
2.  Selection of a "geom" ("geometric object"), like a bar, a point, or a line, which will appear in the visualization as a representation of each observation.

``` r
example_dataframe %>% # First we pipe in the dataframe of interest.
  ggplot(
    mapping = aes( # Next we map our variables of interest onto aesthetics.
      x = course,
      y = grade
    )
  ) + 
  geom_point() # Finally, we specify a geometric object to represent each observation.
```

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-1.png)

``` r
df %>%
  ggplot(
    mapping = aes(
      x = final_assessment_score
    )
  ) + 
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 6777 rows containing non-finite values (stat_bin).

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-15-2.png)

``` r
df_summarised %>% 
  ggplot(mapping = aes(x = imd_band, y = proportion_completed)) +
  geom_bar(stat = "identity")
```

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-16-1.png)

Putting it all together
-----------------------

``` r
df %>% 
  mutate(completed_course = case_when(
           final_result == "Pass" ~ TRUE,
           final_result == "Distinction" ~ TRUE,
           final_result == "Fail" ~ FALSE,
           final_result == "Withdrawn" ~ FALSE
         )) %>% 
  filter(!is.na(imd_band),
         !is.na(completed_course)) %>% 
  group_by(imd_band) %>%
  summarise(proportion_completed = mean(completed_course)) %>% 
  ggplot(mapping = aes(x = imd_band, y = proportion_completed)) +
  geom_bar(stat = "identity")
```

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-17-1.png)

Bonus visualizations, to get you started!
-----------------------------------------

``` r
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
variables <- colnames(df)
classes <- sapply(df, class)
numeric_variables <- variables[classes == "numeric" | classes == "integer"]
categorical_variables <- variables[
  (classes == "character" | classes == "factor") 
  & variables != "id_student"
]

df %>% 
  melt(measure.vars = numeric_variables) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  ggplot(aes(x = value)) +
  stat_density() + 
  facet_wrap(~variable, scales = "free", nrow = 4)
```

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-1.png)

``` r
df %>% 
  melt(measure.vars = categorical_variables) %>% 
  ggplot(aes(x = value)) +
  stat_count() + 
  facet_wrap(~variable, scales = "free") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](data_wrangling_workshop_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-18-2.png)
