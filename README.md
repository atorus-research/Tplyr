[<img src="https://img.shields.io/badge/Slack-OSTCR-blue?style=flat&logo=slack">](https://ostinclinicalresearch.slack.com)
[<img src="https://img.shields.io/badge/Slack-RValidationHub-blue?style=flat&logo=slack">](https://RValidationHub.slack.com)
[![Build Status](https://travis-ci.com/atorus-research/tplyr.svg?branch=master)](https://travis-ci.com/atorus-research/tplyr)
[<img src="https://img.shields.io/codecov/c/github/atorus-research/tplyr">](https://codecov.io/gh/atorus-research/tplyr)
[<img src="https://img.shields.io/github/license/atorus-research/tplyr">](https://github.com/atorus-research/Tplyr/blob/master/LICENSE)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)


# The Beta Release

Welcome to the beta release of our package, `Tplyr`! This package is still in development, and we're actively working on new features. We decided to release this version early to get community feedback. If you find a bug in our code - please report it! If you'd like to see some particular feature - let us know! The more feedback we collect, the better the end product will be when we publish the first version on CRAN. 

If you've already reviewed the Alhpa release, then to see what's new, skip down the the "New in Beta" section.

# Usage

## Installation

`Tplyr` isn't on CRAN yet. You can download it from GitHub for now.

```
devtools::install_github("atorus-research/Tplyr")
```

# What is Tplyr? 

[`dplyr`](https://dplyr.tidyverse.org/) from tidyverse is a grammar of data manipulation. So what does that allow you to do? It gives you, as a data analyst, the capability to easily and intuitively approach the problem of manipulating your data into an analysis ready form. `dplyr` conceptually breaks things down into verbs that allow you to focus on _what_ you want to do more than _how_ you have to do it.

`Tplyr` is designed around a similar concept, but its focus is on building summary tables within the clinical world. In the pharmaceutical industry, a great deal of the data presented in the outputs we create are very similar. For the most part, most of the tables created can be broken down into a few categories:

- Counting for event based variables or categories
- Shifting, which is just counting a 'from' and a 'to'
- Generating descriptive statistics around some continuous variable. 

For many of the tables that go into a clinical submission, at least when considering safety outputs, the tables are made up of a combination of these approaches. Consider a demographics table - and let's use an example from the PHUSE project Standard Analyses & Code Sharing - [Analyses & Displays Associated with Demographics, Disposition, and Medications in Phase 2-4 Clinical Trials and Integrated Summary Documents](https://www.phusewiki.org/docs/WorkingGroups/New%20Template%20Deliverables/Standard%20Analyses%20&%20Code%20Sharing/Analyses%20and%20Displays%20Associated%20with%20Demographics,%20Disposition%20&%20Medications.pdf). 

![Demographics Table](vignettes/demo_table.png)

When you look at this table, you can begin breaking this output down into smaller, redundant, components. These components can be viewed as 'layers', and the table as a whole is constructed by stacking the layers. The boxes in the image above represent how you can begin to conceptualize this. 

- First we have Sex, which is made up of n (%) counts.
- Next we have Age as a continuous variable, where we have a number of descriptive statistics, including n, mean, standard deviation, median, quartile 1, quartile 3, min, max, and missing values.
- After that we have age, but broken into categories - so this is once again n (%) values.
- Race - more counting, 
- Ethnicity - more counting
- Weight - and we're back to descriptive statistics.

So we have one table, with 6 summaries (7 including the next page, not shown) - but only 2 different approaches to summaries being performed. 
In the same way that `dplyr` is a grammar of data manipulation, `Tplyr` aims to be a grammar of data summary. The goal of `Tplyr` is to allow you to program a summary table like you see it on the page, by breaking a larger problem into smaller 'layers', and combining them together like you see on the page. 

Enough talking - let's see some code. In these examples, we will be using data from the [PHUSE Test Data Factory]( https://www.phusewiki.org/wiki/index.php?title=WG5_Project_09) based on the [original pilot project submission package](https://www.cdisc.org/sdtmadam-pilot-project). Note: You can see our replication of the CDISC pilot using the PHUSE Test Data Factory data [here](https://github.com/atorus-research/CDISC_pilot_replication).

```r

tplyr_table(adsl, TRT01P, where = SAFFL == "Y") %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age Categories n (%)")
  ) %>% 
  build()
# A tibble: 9 x 5
  row_label1           row_label2 var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>                <chr>      <chr>          <chr>                       <chr>                     
1 Age (years)          n          "86"           "84"                        "84"                      
2 Age (years)          Mean (SD)  "75.2 ( 8.59)" "74.4 ( 7.89)"              "75.7 ( 8.29)"            
3 Age (years)          Median     "76.0"         "76.0"                      "77.5"                    
4 Age (years)          Q1, Q3     "69, 82"       "71, 80"                    "71, 82"                  
5 Age (years)          Min, Max   "52, 89"       "56, 88"                    "51, 88"                  
6 Age (years)          Missing    " 0"           " 0"                        " 0"                      
7 Age Categories n (%) <65        "14 ( 16.3%) " "11 ( 13.1%) "              " 8 (  9.5%) "            
8 Age Categories n (%) >80        "30 ( 34.9%) " "18 ( 21.4%) "              "29 ( 34.5%) "            
9 Age Categories n (%) 65-80      "42 ( 48.8%) " "55 ( 65.5%) "              "47 ( 56.0%) "  
```

## The TL;DR Before The Nitty Gritty

Here are some of the high level benefits of using `Tplyr`:

- Easy construction of table data using an intuitive syntax
- Smart string formatting for your numbers that's easily specified by the user
- A great deal of flexibility in what is performed and how it's presented, without specifying hundreds of parameters
- Extensibility (in the future...) - we're going to open doors to allow you some level of customization.

# How Tplyr Works

A `Tplyr` table is constructed of two main objects, a `table_table` object and `tplyr_layer` objects making up the different summaries that are to be performed. 

## The `tplyr_table` Object

The `tplyr_table` object is the main container upon which a `Tplyr` table is constructed. `Tplyr` tables are made up of
one or more layers. Each layer contains an instruction for a summary to be performed. The `tplyr_table` object contains
those layers, and the general data, metadata, and logic necessary.

When a `tplyr_table` is created, it will contain the following bindings:

- target - The dataset upon which summaries will be performed
- pop_data - The data containing population information. This defaults to the target dataset
- cols - A categorical variable to present summaries grouped by column (in addition to `treat_var`)
- table_where - The `where` parameter provided, used to subset the `target` data
- treat_var - Variable used to distinguish treatment groups.
- header_n - Default header N values based on `treat_var`
- pop_treat_var - The treatment variable for `pop_data` (if different)
- layers - The container for individual layers of a `tplyr_table`
- treat_grps - Additional treatment groups to be added to the summary (i.e. Total)

`tplyr_table` allows you a basic interface to instantiate the object. Modifier functions are available to change
individual parameters catered to your analysis.

```r
t <- tplyr_table(adsl, TRT01P, where = SAFFL == "Y")
t
*** tplyr_table ***
Target (data.frame):
	Name:  adsl
	Rows:  254
	Columns:  49 
pop_data (data.frame)
	Name:  target 
	Rows:  254 
	Columns:  49 
treat_var variable (quosure)
	TRT01P

header_n: Placebo: 86, Xanomeline High Dose: 84, Xanomeline Low Dose: 84
treat_grps groupings (list)
Number of layer(s): 0
layer_output: 0
```

## The `tplyr_layer` Object

Users of `Tplyr` interface with `tplyr_layer` objects using the `group_<type>` family of functions. This family specifies the type of summary that is to be performed within a layer. `count` layers are used to create summary counts of some discrete variable. `desc` layers create descriptive statistics, and `shift` layers summaries the counts of different changes in states.

- **Count Layers**
  - Count layers allow you to easily create summaries based on counting values with a variable. Additionally, this layer allows you to create n (%) summaries where you're also summarizing the proportion of instances a value occurs compared to some denominator. Count layers are also capable of producing counts of nested relationships. For example, if you want to produce counts of an overall outside group, and then the subgroup counts within that group, you can simply specify the target variable as vars(OutsideVariable, InsideVariable). This allows you to do tables like Adverse Events where you want to see the Preferred Terms within Body Systems, all in one layer. _NOTE: Currently, % values are calculated on the fly using header N values calculated from the target dataset. This is something that we will be adding enhanced flexibility for in future releases (remember - this is an alpha release :))_
- **Descriptive Statistics Layers**
  - Descriptive statistics layers perform summaries on continuous variables. There are a number of summaries built into `Tplyr` already that you can perform, including n, mean, median, standard deviation, variance, min, max, interquartile range, Q1, Q3, and missing value counts. From these available summaries, the default presentation of a descriptive statistics layer will output 'n', 'Mean (SD)', 'Median', 'Q1, Q3', 'Min, Max', and 'Missing'. You can change these summaries using `set_format_strings`, and you can also add your own summaries using `set_custom_summaries`. This allows you to easily implement any additional summary statistics you want presented.
- **Shift Layers**
  - _NOTE: Shift layers are not yet implemented. Expect this in a future release_.

```r
cnt <- group_count(t, AGEGR1)
cnt

*** tplyr_layer ***
Self:  count_layer < 0x562f9ee89870 >
Parent:  tplyr_table < 0x562f9d935030 >
target_var: 
 AGEGR1
by: 
sort: ascending
where: TRUE
Layer(s): 0

dsc <- group_desc(t, AGE)
dsc

*** tplyr_layer ***
Self:  desc_layer < 0x562f9f278330 >
Parent:  tplyr_table < 0x562f9d935030 >
target_var: 
 AGE
by: 
sort: ascending
where: TRUE
Layer(s): 0
```

## Adding Layers to a Table

Everyone has their own style of coding - so we've tried to be flexible to an extent. Overall, `Tplyr` is built around tidy syntax, so all of our object construction supports piping with [magrittr](https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html) (i.e. `%>%`). 

There are two ways to add layers to a `tplyr_table`: `add_layer` and `add_layers`. The difference is that `add_layer` allows you to construct the layer within the call to `add_layer`, whereas with `add_layers` you can attach multiple layers that have already been constructed upfront:

```r
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)")
  )
```

Within `add_layer`, the syntax to constructing the count layer for Age Categories was written on the fly. `add_layer` is special in that it also allows you to use piping to use modifier functions on the layer being constructed

```r
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)") %>% 
      set_format_strings(f_str("xx (xx.x%)", n, pct)) %>% 
      add_total_row()
  )
```

`add_layers`, on the other hand, lets you isolate the code to construct a particular layer if you wanted to separate things out more. Some might find this cleaner to work with if you have a large number of layers being constructed.

```r
t <- tplyr_table(adsl, TRT01P) 

l1 <- group_count(t, AGEGR1, by = "Age categories n (%)")
l2 <- group_desc(t, AGE, by = "Age (years)")

t <- add_layers(t, l1, l2)
```

Notice that when you construct the layers separately, you need to specify the table to which they belong. `add_layer` does this automatically. `tplyr_table` and `tplyr_layer` objects are built on environments, and the parent/child relationships are very important. This is why, even though the layer knows who its table parent is, the layers still need to be attached to the table (as the table doesn't know who its children are). [Advanced R](https://adv-r.hadley.nz/environments.html) does a very good job at explaining what environments in R are, their benefits, and how to use them. 

## A Note Before We Go Deeper

Notice that when you construct a `tplyr_table` or a `tplyr_layer` that what displays is a summary of information about the table or layer? That's because when you create these objects - it constructs the metadata, but does not process the actual data. This allows you to construct and make sure the pieces of your table fit together before you do the data processing - and it gives you a container to hold all of this metadata, and use it later if necessary. 

To generate the data from a `tplyr_table` object, you use the function `build`:

```r
t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories n (%)")
  )

t %>% build()

# A tibble: 3 x 5
  row_label1           row_label2 var1_Placebo `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>                <chr>      <chr>        <chr>                       <chr>                     
1 Age categories n (%) <65        14 ( 16.3%)  11 ( 13.1%)                 " 8 (  9.5%)"             
2 Age categories n (%) >80        30 ( 34.9%)  18 ( 21.4%)                 "29 ( 34.5%)"             
3 Age categories n (%) 65-80      42 ( 48.8%)  55 ( 65.5%)                 "47 ( 56.0%)"  
```

But there's more you can get from `Tplyr`. It's great to have the formatted numbers, but what about the numeric data behind the scenes? What if you want to calculate your own statistics based off of the counts? You can get that information as well using `get_numeric_data`. This returns the numeric data from each layer as a list of data frames:

```r
get_numeric_data(t)

[[1]]
# A tibble: 9 x 5
  TRT01P               `"Age categories n (%)"` AGEGR1 value Total
  <chr>                <chr>                    <chr>  <dbl> <dbl>
1 Placebo              Age categories n (%)     <65       14    86
2 Placebo              Age categories n (%)     >80       30    86
3 Placebo              Age categories n (%)     65-80     42    86
4 Xanomeline High Dose Age categories n (%)     <65       11    84
5 Xanomeline High Dose Age categories n (%)     >80       18    84
6 Xanomeline High Dose Age categories n (%)     65-80     55    84
7 Xanomeline Low Dose  Age categories n (%)     <65        8    84
8 Xanomeline Low Dose  Age categories n (%)     >80       29    84
9 Xanomeline Low Dose  Age categories n (%)     65-80     47    84
```

By storing pertinent information, you can get more out of a `Tplyr` objects than processed data for display. And by specifying when you want to get data out of `Tplyr`, we can save you from repeatedly processing data while your constructing your outputs - which is particularly useful when that computation starts taking time.

## Constructing Layers

The bulk of `Tplyr` coding comes from constructing your layers and specifying the work you want to be done. Before we get into this, it's important to discuss how `Tplyr` handles string formatting.

### String Formatting in `Tplyr`

String formatting in `Tplyr` is controlled by an object called an `f_str`, which is also the name of function you use to create these formats. To set these format strings into a `tplyr_layer`, you use the function `set_format_strings`, and this usage varies slightly between layer types (more on that later). 

So - why is this object necessary. Consider this example:

```r

t <- tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)") %>% 
      set_format_strings(
        'n' = f_str('xx', n),
        'Mean (SD)' = f_str('xx.xx (xx.xxx)', mean, sd)
      )
  )

t %>% build()

# A tibble: 2 x 5
  row_label1  row_label2 var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>       <chr>      <chr>          <chr>                       <chr>                     
1 Age (years) n          86             84                          84                        
2 Age (years) Mean (SD)  75.21 ( 8.590) 74.38 ( 7.886)              75.67 ( 8.286)   
```

In a perfect world, the `f_str` calls wouldn't be necessary - but in reality they allow us to infer a great deal of information from realistically very few user inputs. In the calls that you see above:

- The row labels in the `row_label2` column are taken from the left side of each `=` in `set_format_strings`
- The string formats, including integer length and decimal precision, and exact presentation formatting are taken from the strings within the first parameter of each `f_str` call
- The second and greater parameters within each f_str call determine the descriptive statistic summaries that will be performed. This is connected to a number of default summaries available within `Tplyr`, but you can also create your own summaries (more on that later). The default summaries that are built in include:
  - `n` = Number of observations
  - `mean` = Mean
  - `sd` = Standard Deviation
  - `var` = Variance
  - `iqr` = Inter Quartile Range
  - `q1` = 1st quartile
  - `q3` = 3rd quartile
  - `min` = Minimum value
  - `max` = Maximum value
  - `missing` = Count of NA values
- When two summaries are placed on the same `f_str` call, then those two summaries are formatted into the same string. This allows you to do a `Mean (SD)` type format where both numbers appear.

This simple user input controls a significant amount of work in the back end of the data processing, and the `f_str` object allows that metadata to be collected.

`f_str` objects are also used with count layers as well to control the data presentation. Instead of specifying the summaries performed, you use `n` and `pct` for your parameters, if you want both n's and percents presented.

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx (xx.x)',n,pct))
  ) %>% 
  build()

tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx',n))
  ) %>% 
  build()
  
# A tibble: 3 x 5
  row_label1     row_label2 var1_Placebo `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>          <chr>      <chr>        <chr>                       <chr>                     
1 Age categories <65        14           11                          " 8"                      
2 Age categories >80        30           18                          "29"                      
3 Age categories 65-80      42           55                          "47"     
```

Really - format strings allow you to present your data however you like.

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      set_format_strings(f_str('xx (•◡•) xx.x%',n,pct))
  ) %>% 
  build()
# A tibble: 3 x 5
  row_label1     row_label2 var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>          <chr>      <chr>          <chr>                       <chr>                     
1 Age categories <65        14 (•◡•) 16.3% 11 (•◡•) 13.1%              " 8 (•◡•)  9.5%"          
2 Age categories >80        30 (•◡•) 34.9% 18 (•◡•) 21.4%              "29 (•◡•) 34.5%"          
3 Age categories 65-80      42 (•◡•) 48.8% 55 (•◡•) 65.5%              "47 (•◡•) 56.0%"    
  
```

But should you? Probably not.

## Descriptive Statistic Layers

As covered under string formatting, `set_format_strings` controls a great deal of what happens within a descriptive statistics layer. Note that there are some built in defaults to what's output:

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (years)")
  ) %>% 
  build()
  
# A tibble: 6 x 5
  row_label1  row_label2 var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>       <chr>      <chr>          <chr>                       <chr>                     
1 Age (years) n          "86"           "84"                        "84"                      
2 Age (years) Mean (SD)  "75.2 ( 8.59)" "74.4 ( 7.89)"              "75.7 ( 8.29)"            
3 Age (years) Median     "76.0"         "76.0"                      "77.5"                    
4 Age (years) Q1, Q3     "69, 82"       "71, 80"                    "71, 82"                  
5 Age (years) Min, Max   "52, 89"       "56, 88"                    "51, 88"                  
6 Age (years) Missing    " 0"           " 0"                        " 0"       
```

To override these defaults, just specify the summaries that you want to be performed using `set_format_strings` as described above. But what if `Tplyr` doesn't have a built in function to do the summary statistic that you want to see? Well - you can make your own! This is where `set_custom_summaries` comes into play. Let's say you want to derive a geometric mean.

```r
tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(AGE, by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(AGE[AGE > 0]), na.rm=TRUE) / length(AGE))
      ) %>%
      set_format_strings(
        'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
      )
  ) %>% 
  build()

# A tibble: 1 x 5
  row_label1   row_label2          var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>        <chr>               <chr>          <chr>                       <chr>                     
1 Sepal Length Geometric Mean (SD) 74.70 ( 8.590) 73.94 ( 7.886)              75.18 ( 8.286)    
```

In `set_custom_summaries`, first you name the summary being performed. This is important - that name is what you use in the f_str call to incorporate it into a format. Next, you program or call the function desired. What happens in the background is that this is used in a call to `dplyr::summarize` - so use similar syntax. Currently, you use the same variable name as your target variable (in this example, `AGE`), but this will likely have to change in the future - to support the next feature of descriptive statistic layers - multi-variable summaries.

Sometimes there's a need to present multiple variables summarized side by side. `Tplyr` makes this easy as well.

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(vars(AGE, AVGDD), by = "Age and Avg. Daily Dose")
  ) %>% 
  build()

# A tibble: 6 x 8
  row_label1      row_label2 var1_Placebo  `var1_Xanomeline Hi… `var1_Xanomeline L… var2_Placebo `var2_Xanomeline H… `var2_Xanomeline L…
  <chr>           <chr>      <chr>         <chr>                <chr>               <chr>        <chr>               <chr>              
1 Age and Avg. D… n          "86"          "84"                 "84"                "86"         "84"                "84"               
2 Age and Avg. D… Mean (SD)  "75.2 ( 8.59… "74.4 ( 7.89)"       "75.7 ( 8.29)"      " 0.0 ( 0.0… "71.6 ( 8.11)"      "54.0 ( 0.00)"     
3 Age and Avg. D… Median     "76.0"        "76.0"               "77.5"              " 0.0"       "75.1"              "54.0"             
4 Age and Avg. D… Q1, Q3     "69, 82"      "71, 80"             "71, 82"            " 0,  0"     "70, 77"            "54, 54"           
5 Age and Avg. D… Min, Max   "52, 89"      "56, 88"             "51, 88"            " 0,  0"     "54, 79"            "54, 54"           
6 Age and Avg. D… Missing    " 0"          " 0"                 " 0"                " 0"         " 0"                " 0"      
```

`Tplyr` summarizes both variables and merges them together. This makes creating tables where you need to compare BASE, AVAL, and CHG next to each other nice and simple. Note the use of `vars` - in any situation where you'd like to use multiple variable names in a parameter, use `dplyr::vars` to specify the variables. You can use text strings in the calls to `dplyr::vars` as well. 

## Count Layers

Count layers generally allow you to create n and n (%) count type summaries. There are a few extra features here as well. Let's say that you want a total row within your counts. This is easy with `add_total_row()`:

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories") %>% 
      add_total_row()
  ) %>% 
  build()

# A tibble: 4 x 5
  row_label1     row_label2 var1_Placebo `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>          <chr>      <chr>        <chr>                       <chr>                     
1 Age categories <65        14 ( 16.3%)  11 ( 13.1%)                 " 8 (  9.5%)"             
2 Age categories >80        30 ( 34.9%)  18 ( 21.4%)                 "29 ( 34.5%)"             
3 Age categories 65-80      42 ( 48.8%)  55 ( 65.5%)                 "47 ( 56.0%)"             
4 Age categories Total      86 (100.0%)  84 (100.0%)                 "84 (100.0%)"  
```

Sometimes it's also necessary to count summaries based on distinct values. `Tplyr` allows you to do this as well with `set_distinct_by`:

```r
tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count('Subjects with at least one adverse event') %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx', n))
  ) %>% 
  build()

# A tibble: 1 x 4
  row_label1                               var1_Placebo `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>                                    <chr>        <chr>                       <chr>                     
1 Subjects with at least one adverse event 69           79                          77     
```

There's another trick going on here - to create a summary with row label text like you see above, text strings can be used as the target variables. Here, we use this in combination with `set_distinct_by` to count distinct subjects. 

Adverse event tables often call for counting AEs of something like a body system and counting actual events within that body system. `Tplyr` has means of making this simple for the user as well.


```r
tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>% 
  build()

# A tibble: 265 x 5
   row_label1                                row_label2                 var1_Placebo  `var1_Xanomeline High … `var1_Xanomeline Low D…
   <chr>                                     <chr>                      <chr>         <chr>                   <chr>                  
 1 GENERAL DISORDERS AND ADMINISTRATION SIT… NA                         " 48 (100.0%… "124 (100.0%)"          "120 (100.0%)"         
 2 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE BLEEDING  "  0 (  0.0%… "  0 (  0.0%)"          "  1 (  0.8%)"         
 3 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE DERMATIT… "  9 ( 18.8%… " 12 (  9.7%)"          " 15 ( 12.5%)"         
 4 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE DESQUAMA… "  0 (  0.0%… "  0 (  0.0%)"          "  1 (  0.8%)"         
 5 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE DISCHARGE "  0 (  0.0%… "  1 (  0.8%)"          "  0 (  0.0%)"         
 6 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE DISCOLOU… "  0 (  0.0%… "  0 (  0.0%)"          "  1 (  0.8%)"         
 7 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE ERYTHEMA  "  3 (  6.2%… " 23 ( 18.5%)"          " 20 ( 16.7%)"         
 8 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE INDURATI… "  1 (  2.1%… "  0 (  0.0%)"          "  0 (  0.0%)"         
 9 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE IRRITATI… "  7 ( 14.6%… " 16 ( 12.9%)"          " 18 ( 15.0%)"         
10 GENERAL DISORDERS AND ADMINISTRATION SIT… APPLICATION SITE PAIN      "  0 (  0.0%… "  2 (  1.6%)"          "  0 (  0.0%)"         
# … with 255 more rows
```

Note that the nesting of variables happens automatically. 

## Other Bells and Whistles

For simplicity of demonstration, some other features have not yet been demonstrated - so let's get into that now! 

First up, it's common to have to present a total group, or a treated group vs. the placebo. `Tplyr` allows you to create these groups as necessary using `add_treat_group` and the abbreviated function `add_total_group`.


```{r treatment_groups}
tplyr_table(adsl, TRT01P) %>% 
  add_total_group() %>% 
  add_treat_group('Treated', c("Xanomeline High Dose", "Xanomeline Low Dose")) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories")
  ) %>% 
  build()

# A tibble: 3 x 7
  row_label1     row_label2 var1_Placebo   var1_Total     var1_Treated   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>          <chr>      <chr>          <chr>          <chr>          <chr>                       <chr>                     
1 Age categories <65        " 14 ( 16.3%)" " 33 ( 13.0%)" " 19 ( 11.3%)" " 11 ( 13.1%)"              "  8 (  9.5%)"            
2 Age categories >80        " 30 ( 34.9%)" " 77 ( 30.3%)" " 47 ( 28.0%)" " 18 ( 21.4%)"              " 29 ( 34.5%)"            
3 Age categories 65-80      " 42 ( 48.8%)" "144 ( 56.7%)" "102 ( 60.7%)" " 55 ( 65.5%)"              " 47 ( 56.0%)"   
```

What about having different by group variables? So far I've only demonstrated `by` to present row label text. It additionally allows you to use grouping variables as well:

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_count(AGEGR1, by = vars("Age categories", SEX))
  ) %>% 
  build()
  
# A tibble: 6 x 6
  row_label1     row_label2 row_label3 var1_Placebo  `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>          <chr>      <chr>      <chr>         <chr>                       <chr>                     
1 Age categories F          <65        " 9 ( 10.5%)" " 5 (  6.0%)"               " 5 (  6.0%)"             
2 Age categories F          >80        "22 ( 25.6%)" " 7 (  8.3%)"               "17 ( 20.2%)"             
3 Age categories F          65-80      "22 ( 25.6%)" "28 ( 33.3%)"               "28 ( 33.3%)"             
4 Age categories M          <65        " 5 (  5.8%)" " 6 (  7.1%)"               " 3 (  3.6%)"             
5 Age categories M          >80        " 8 (  9.3%)" "11 ( 13.1%)"               "12 ( 14.3%)"             
6 Age categories M          65-80      "20 ( 23.3%)" "27 ( 32.1%)"               "19 ( 22.6%)" 
```

Grouping by columns? We've got that to - but this is actually specified at the `tplyr_table` level. Why? It gets complicated if layers don't have consistent columns, and stacking them together would get complex. Similarly, assuming you'd want the `cols` argument the same between layers, it's more tedious to specify it on every layer constructor. 

```r
tplyr_table(adsl, TRT01P, cols=SEX) %>% 
  add_layer(
    group_count(AGEGR1, by = "Age categories")
  ) %>% 
  build()
  
# A tibble: 3 x 8
  row_label1     row_label2 var1_Placebo_F var1_Placebo_M `var1_Xanomeline High Dose_F` `var1_Xanomeline High Dose_M` `var1_Xanomeline Low Dose_F` `var1_Xanomeline Low Dose_M`
  <chr>          <chr>      <chr>          <chr>          <chr>                         <chr>                         <chr>                        <chr>                       
1 Age categories <65        " 9 ( 17.0%)"  " 5 ( 15.2%)"  " 5 ( 12.5%)"                 " 6 ( 13.6%)"                 " 5 ( 10.0%)"                " 3 (  8.8%)"               
2 Age categories >80        "22 ( 41.5%)"  " 8 ( 24.2%)"  " 7 ( 17.5%)"                 "11 ( 25.0%)"                 "17 ( 34.0%)"                "12 ( 35.3%)"               
3 Age categories 65-80      "22 ( 41.5%)"  "20 ( 60.6%)"  "28 ( 70.0%)"                 "27 ( 61.4%)"                 "28 ( 56.0%)"                "19 ( 55.9%)"   
```

Note that in both `by` and `cols` - multiple variable names (or text along with variable names) should be specified using `dplyr::vars`. We've tried to make the error messages explicit when you fail to do this, and help remind you of the proper parameter entry. 

## Preparing Your Table for Presentation

Now that we've gotten through a lot of the functionality of `Tplyr`, what about when you're ready to present your table? Well - we're deferring the cosmetics to packages that already do this exceptionally well. We're big fans of [huxtable](https://hughjonesd.github.io/huxtable/), as you may have seen from our package [pharmaRTF](https://github.com/atorus-research/pharmaRTF). RStudio is continually working on and improving [gt](https://gt.rstudio.com/) - so we're not reinventing the wheel here. But we can do our part to make it easier.

In a `huxtable` - the column headers of a table are just rows in a table. To make this preperation easier, we made the function `add_column_headers` into `Tplyr`. This allows you to simply use a string to created column headers - and it supports nesting. Let's see it in action. 

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(AGE, by = "Age (Years)")
  ) %>% 
  build() %>% 
  add_column_headers(
    ' | Statistic | Placebo | Xanomeline (High) | Xanomeline (Low)'
  )

# A tibble: 7 x 5
  row_label1    row_label2 var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose`
  <chr>         <chr>      <chr>          <chr>                       <chr>                     
1 ""            Statistic  "Placebo"      "Xanomeline (High)"         "Xanomeline (Low)"        
2 "Age (Years)" n          "86"           "84"                        "84"                      
3 "Age (Years)" Mean (SD)  "75.2 ( 8.59)" "74.4 ( 7.89)"              "75.7 ( 8.29)"            
4 "Age (Years)" Median     "76.0"         "76.0"                      "77.5"                    
5 "Age (Years)" Q1, Q3     "69, 82"       "71, 80"                    "71, 82"                  
6 "Age (Years)" Min, Max   "52, 89"       "56, 88"                    "51, 88"                  
7 "Age (Years)" Missing    " 0"           " 0"                        " 0"       
```

Columns are separated with the bar character (|). Nesting is done by adding text in curly brackets (i.e. {}).

```r
tplyr_table(adsl, TRT01P) %>% 
  add_layer(
    group_desc(vars(AGE, AVGDD), by = "Age and Avg. Daily Dose")
  ) %>% 
  build() %>% 
  add_column_headers(
    ' | Statistic | Age {Placebo | Xanomeline (High) | Xanomeline (Low)} | Average Daily Dose {Placebo | Xanomeline (High) | Xanomeline (Low)}'
  )

# A tibble: 8 x 8
  row_label1                row_label2  var1_Placebo   `var1_Xanomeline High Dose` `var1_Xanomeline Low Dose` var2_Placebo        `var2_Xanomeline High Dos… `var2_Xanomeline Low Dos…
  <chr>                     <chr>       <chr>          <chr>                       <chr>                      <chr>               <chr>                      <chr>                    
1 ""                        ""          "Age"          ""                          ""                         "Average Daily Dos… ""                         ""                       
2 ""                        "Statistic" "Placebo"      "Xanomeline (High)"         "Xanomeline (Low)"         "Placebo"           "Xanomeline (High)"        "Xanomeline (Low)"       
3 "Age and Avg. Daily Dose" "n"         "86"           "84"                        "84"                       "86"                "84"                       "84"                     
4 "Age and Avg. Daily Dose" "Mean (SD)" "75.2 ( 8.59)" "74.4 ( 7.89)"              "75.7 ( 8.29)"             " 0.0 ( 0.00)"      "71.6 ( 8.11)"             "54.0 ( 0.00)"           
5 "Age and Avg. Daily Dose" "Median"    "76.0"         "76.0"                      "77.5"                     " 0.0"              "75.1"                     "54.0"                   
6 "Age and Avg. Daily Dose" "Q1, Q3"    "69, 82"       "71, 80"                    "71, 82"                   " 0,  0"            "70, 77"                   "54, 54"                 
7 "Age and Avg. Daily Dose" "Min, Max"  "52, 89"       "56, 88"                    "51, 88"                   " 0,  0"            "54, 79"                   "54, 54"                 
8 "Age and Avg. Daily Dose" "Missing"   " 0"           " 0"                        " 0"                       " 0"                " 0"                       " 0"        
```

# Tying It All Together

Ok - so, this was a lot to cover. But how does this all fit in the grand scheme of things? Let's look at a more complete example - what if we wanted to build that demographics table

```r
t <- tplyr_table(adsl, TRT01P, where = (SAFFL == "Y")) %>%
  add_total_group() %>% 
  add_layer(
    group_count(SEX, by="Sex") %>%
      set_format_strings(f_str("xx (xxx%)",n,pct))
  ) %>%
  add_layer(
    group_desc(AGE, by="Age Categories") 
  ) %>%
  add_layer(
    group_count(RACE, by="Race") %>%
      set_format_strings(f_str("xx (xxx%)",n,pct))
  ) %>%
  add_layer(
    group_count(ETHNIC, by="Ethnicity") %>%
      set_format_strings(f_str("xx (xxx%)",n,pct))
  ) %>%
  add_layer(
    group_desc(WEIGHTBL, by="Weight")
  )

# build the table
t %>%
  build() %>% 
  select(starts_with('row_label'), var1_Placebo, starts_with('var1_X'), var1_Total) %>% 
  add_column_headers(" |  | Placebo | Xanomeline (Low) | Xanomeline (High) | Total") %>% 
  apply_row_masks() %>% 
  huxtable::as_hux(add_colnames=FALSE)

```

|||||||
|--- |--- |--- |--- |--- |--- |
|||Placebo|Xanomeline (Low)|Xanomeline (High)|Total|
|Sex|F|53 ( 62%)|40 ( 48%)|50 ( 60%)|143 ( 56%)|
||M|33 ( 38%)|44 ( 52%)|34 ( 40%)|111 ( 44%)|
|Age Categories|n|86|84|84|254|
||Mean (SD)|75.2 ( 8.59)|74.4 ( 7.89)|75.7 ( 8.29)|75.1 ( 8.25)|
||Median|76.0|76.0|77.5|77.0|
||Q1, Q3|69, 82|71, 80|71, 82|70, 81|
||Min, Max|52, 89|56, 88|51, 88|51, 89|
||Missing|0|0|0|0|
|Race|AMERICAN INDIAN OR ALASKA NATIVE|0 (  0%)|1 (  1%)|0 (  0%)|1 (  0%)|
||BLACK OR AFRICAN AMERICAN|8 (  9%)|9 ( 11%)|6 (  7%)|23 (  9%)|
||WHITE|78 ( 91%)|74 ( 88%)|78 ( 93%)|230 ( 91%)|
|Ethnicity|HISPANIC OR LATINO|3 (  3%)|3 (  4%)|6 (  7%)|12 (  5%)|
||NOT HISPANIC OR LATINO|83 ( 97%)|81 ( 96%)|78 ( 93%)|242 ( 95%)|
|Weight|n|86|84|84|254|
||Mean (SD)|62.8 (12.77)|70.0 (14.65)|67.3 (14.12)|66.6 (14.13)|
||Median|60.5|69.2|64.9|66.7|
||Q1, Q3|54, 74|57, 80|56, 77|55, 77|
||Min, Max|34, 86|42, 108|45, 106|34, 108|
||Missing|0|0|1|1|


And just like that - you have a huxtable table ready to go. 

Wouldn't it be nice if you had some resources for where to take it from here? Oh wait - you do! This is the perfect point to take things to the next step in the workflow: preparing and delivering your final output. We have plenty for you to read in our [pharmaRTF](https://atorus-research.github.io/pharmaRTF.html) vignettes about how to get started with huxtable, and create your RTF output files.

New in Beta
================

Three weeks ago, we released the Alpha release of Tplyr. Since then,
we’ve been hard at work introducing some fresh features to make Tplyr
more flexible, more effective, and more useful as a tool for you and
your organization. Some of these additions are less alluring from the
surface, but introduce critical functionality needed for Tplyr to
accomplish the goals we’ve laid out.

The enhancements that we’re going to cover in this document are as
follows: - General updates: - Calculate your header N counts based on
the population dataset or the target dataset. The alpha release had an
option to set the population data but this wasn’t actually used anywhere
in the internals. - Use these header N counts as token replacements when
using the `add_column_headers` function. - Order variables are now added
to the built dataset to allow you to sort the output dataset as you wish
with numeric variables. - Count layer updates: - Optionally use the
population data N counts as denominators for percent calculation. - For
multi-level count summaries, nest the row label columns together to
present both row labels in a single column - You can now present both
distinct and non-distinct counts instead of one or the other - Sorting
options allow you to order results from the target variable values or
from derived counts within a specified column - Risk difference
calculations can now be added as additional columns, with flexible
options for presentation - Descriptive statistics layer updates: - The
custom summary functionality has been updated to apply to multi-variable
summaries, which results in an interface change - Automatic decimal
precision has been added to allow you to base the presentation on the
precision of the data as collected

Let’s dig into each of these updates one by one.

Count layer updates
-------------------

The general updates are best presented within the context of a count
layer. An important feature that was missing from the alpha release was
the capability to control the denominator being used in a summary. For
example, if you’re summarizing adverse events, not all subjects may have
been included adverse events dataset. A subject only exists in the
adverse events dataset if they had an adverse event. Therefore, if
you’re counting the total subjects for your denominator, the denominator
will be missing subjects. This is exactly why we included bindings for a
separate population dataset in the first place, and now the population
data are properly utilized.

Consider the following:

``` r

t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%)', distinct, distinct_pct)) # Set up the presentation
  )

invisible(build(t))

header_n(t)
#> # A tibble: 3 x 2
#>   TRT01A                   n
#>   <chr>                <dbl>
#> 1 Placebo                 86
#> 2 Xanomeline High Dose    84
#> 3 Xanomeline Low Dose     84
```

You can see that these counts are indeed coming from ADSL and not ADAE:

``` r
adae %>% 
  filter(SAFFL == "Y") %>%
  distinct(TRTA, USUBJID) %>% 
  count(TRTA)
#> # A tibble: 3 x 2
#>   TRTA                     n
#>   <chr>                <int>
#> 1 Placebo                 69
#> 2 Xanomeline High Dose    79
#> 3 Xanomeline Low Dose     77
```

Note also the use of the new `f_str` variable names `distinct` and
`distinct_pct`. We made these names separate from `n` and `pct` to make
it explicit when you want to use one type of count or the other. This
further allows you to create tables where you mix and match. Consider
this common AE table structure:

``` r

t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%) [x]', distinct, distinct_pct, n)) # Set up the presentation - with event counts
  )

kable(head(build(t)))
```

| row\_label1          | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|
|:---------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              1|
| ABDOMINAL PAIN       | 1 ( 1.2%) \[1\] | 1 ( 1.2%) \[2\]            | 3 ( 3.6%) \[3\]           |                  1|              2|
| ACROCHORDON EXCISION | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              3|
| ACTINIC KERATOSIS    | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              4|
| AGITATION            | 2 ( 2.3%) \[2\] | 1 ( 1.2%) \[1\]            | 2 ( 2.4%) \[2\]           |                  1|              5|
| ALCOHOL USE          | 0 ( 0.0%) \[0\] | 1 ( 1.2%) \[1\]            | 0 ( 0.0%) \[0\]           |                  1|              6|

In this table, we present the distinct counts by subject, the distinct
percent (i.e. number of subjects who experienced an adverse event within
the total treatment group, as determined by the population dataset), and
then the total number of events. When the population dataset is
specified, those N counts from each treatment group will be used within
the denominator for `distinct_pct`. Otherwise, the count within target
dataset (in this case, ADAE), of the subjects will be used for
`distinct_pct`.

In addition to population data updates, we’ve also enhanced the
capabilities of `add_column_headers` to work with the `header_n` values.
It’s extremely common to print the header\_n counts within your column
headers. Now, we’ve made that easy.

``` r
t <- tplyr_table(adae, TRTA, where= SAFFL == "Y") %>%
  set_pop_data(adsl) %>%  # Specify the population dataset
  set_pop_treat_var(TRT01A) %>% # Specify the treatment variable within the population
  add_layer(
    group_count(AEDECOD) %>% # Create the count layer
      set_distinct_by(USUBJID) %>% # Specify the variable to determine a distinct count by
      set_format_strings(f_str('xx (xx.x%)', distinct, distinct_pct)) # Set up the presentation
  )

build(t) %>% 
  select(-starts_with('ord')) %>% 
  add_column_headers(' | Placebo N=(**Placebo**) | Xan.Low N=(**`Xanomeline High Dose`**) | Xan.High N=(**`Xanomeline Low Dose`**)',
                     header_n(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |
|:---------------------|:---------------|:---------------------------|:--------------------------|
|                      | Placebo N=(86) | Xan.Low N=(84)             | Xan.High N=(84)           |
| ABDOMINAL DISCOMFORT | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| ABDOMINAL PAIN       | 1 ( 1.2%)      | 1 ( 1.2%)                  | 3 ( 3.6%)                 |
| ACROCHORDON EXCISION | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| ACTINIC KERATOSIS    | 0 ( 0.0%)      | 1 ( 1.2%)                  | 0 ( 0.0%)                 |
| AGITATION            | 2 ( 2.3%)      | 1 ( 1.2%)                  | 2 ( 2.4%)                 |

After the table is built and you order your columns, using
`add_column_headers`, you can extract the header\_n’s from your
`tplyr_table` object using the `header_n` function. Using this as a
parameter to `add_column_headers`, the function will now use the names
and values from the `header_n` output as token replacements. Simply use
the names of the `header_n` vector as your token, surrounded by "\*\*"
on both sides. Those values will be replaced as you see in the output
above. Any named numeric vector will work in the `header_n` parameter of
`add_column_headers`, but the `header_n` function in Tplyr makes this
simple.

Sorting
-------

You may have noticed the addition of `ord` columns on the datasets
output so far. Tplyr now provides order columns built in to the table
returned to you via the `build` function. This follows a few basic
principles:

-   Each layer will have its own index based on the order in which it
    was attached to the `tplyr_table` object.
-   `by` variables will be sorted based on:
    -   If there there is a ‘&lt;VAR&gt;N’ variable in the dataset
        (i.e. RACE &lt;-&gt; RACEN, ETHNIC &lt;-&gt; ETHNICN), then the
        N variable will used
    -   If no ‘&lt;VAR&gt;N’ variable exists, but the variable is a
        factor, the factor level order will be used
    -   Otherwise, alphanumeric sorting of the `by` variable will be
        used.

All layers will function similarly up until this point. The sorting of
results is where things differ based on the layer type. Descriptive
statistic layers are simple - the order in which the format strings are
presented controls the order in which the results will be displayed.
This means that if you enter the format strings in the order ‘n’, ‘Mean
(SD)’, ‘Median’ - then the rows will order to present the summaries in
that order.

Count layers are where things get more complicated. There are multiple
scenarios for how one might wish to sort a count table. You may want:

-   Alphanumeric sorting of the target variable
-   A pre-specified sort order, for things like a disposition table, or
    AEs of special interest.
-   To order based on the result of one of the summary count in
    particular, within a specific treatment group (i.e. descending
    number of events from the Xanomeline High treatment group).

Tplyr’s got your back! We support each of these different scenarios.
Let’s use count layers to explore how all of this works

``` r
t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX)
  ) %>% 
  add_layer(
    group_count(ETHNIC, by = SEX)
  )

build(t) %>%
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              1|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              2|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              3|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              1|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              3|
| F           | HISPANIC OR LATINO               | 2 ( 2.3%)     | 1 ( 1.2%)                  | 4 ( 4.8%)                 |                  2|              1|              1|
| F           | NOT HISPANIC OR LATINO           | 51 ( 59.3%)   | 39 ( 46.4%)                | 46 ( 54.8%)               |                  2|              1|              2|
| M           | HISPANIC OR LATINO               | 1 ( 1.2%)     | 2 ( 2.4%)                  | 2 ( 2.4%)                 |                  2|              2|              1|
| M           | NOT HISPANIC OR LATINO           | 32 ( 37.2%)   | 42 ( 50.0%)                | 32 ( 38.1%)               |                  2|              2|              2|

In this very simple example, you see two order columns:

-   ord\_layer\_index
    -   This indicates the layer order. RACE was added first, so the
        value is 1. ETHNIC was added second, so its layer value is 2.
-   ord\_layer\_1
    -   This ties back to the by variable SEX. It is indicated as 1 to
        relate back to row\_label1. No SEXN variable is in ADSL, so it
        sorts alphanumerically, with F coming before M.
-   order\_layer\_2
    -   This associates with the target variable, which is displayed in
        row\_label2. Note that alphanumeric sorting was using, as these
        orders do not actually coincide with the RACEN values in the
        data, and there’s no ETHNICN in the data. The target variable
        sort order is an area where more caution should be used, so we
        have helper functions to let you be more explicit in how the
        target variable sorting should be handled:

``` r

t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX) %>% 
      set_order_count_method('byvarn') # Specify to look for a RACEN for result sorting
  )

build(t) %>% 
  arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% # Sort using the Tplyr order variables
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              1|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              2|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              6|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              1|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              6|

RACEN already exists within ADSL. Using the `byvarn` method of sorting,
`ord_layer_2` is built by simply using the RACEN variable values. This
allows you to leverage common variable values already built into CDISC.
If you’d like to work in a more R like world, then converting your input
variables to ordered factors works as well:

``` r
# Create a ordered factor for RACE
adsl$RACE <- factor(adsl$RACE, c('BLACK OR AFRICAN AMERICAN', 'AMERICAN INDIAN OR ALASKA NATIVE', 'WHITE'))

t <- tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_count(RACE, by = SEX) %>% 
      set_order_count_method('byfactor') # Specify to use RACE's factor order for the Tplyr order variable
  )

build(t) %>% 
  arrange(ord_layer_index, ord_layer_1, ord_layer_2) %>% 
  kable()
```

| row\_label1 | row\_label2                      | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:---------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| F           | BLACK OR AFRICAN AMERICAN        | 5 ( 5.8%)     | 6 ( 7.1%)                  | 6 ( 7.1%)                 |                  1|              1|              1|
| F           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 0 ( 0.0%)                  | 0 ( 0.0%)                 |                  1|              1|              2|
| F           | WHITE                            | 48 ( 55.8%)   | 34 ( 40.5%)                | 44 ( 52.4%)               |                  1|              1|              3|
| M           | BLACK OR AFRICAN AMERICAN        | 3 ( 3.5%)     | 3 ( 3.6%)                  | 0 ( 0.0%)                 |                  1|              2|              1|
| M           | AMERICAN INDIAN OR ALASKA NATIVE | 0 ( 0.0%)     | 1 ( 1.2%)                  | 0 ( 0.0%)                 |                  1|              2|              2|
| M           | WHITE                            | 30 ( 34.9%)   | 40 ( 47.6%)                | 34 ( 40.5%)               |                  1|              2|              3|

And finally, let’s jump to an AE table for the last example - sorting
based on the calculated counts. Here, we need some more input from the
user to understand what they want:

-   What column do we want a number from
-   What number within the column should be used

Let’s take a look.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx%) [x]', distinct, pct, n)) %>% 
      set_order_count_method('bycount') %>% # Specify to use the resulting counts for sorting
      set_ordering_cols('Xanomeline High Dose') %>% # Use the counts from Xanomeline High Dose treatment group
      set_result_order_var(n) # Use the raw numeric value of the n counts
  )

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|
|:---------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| ABDOMINAL PAIN       | 1 ( 0%) \[1\] | 1 ( 0%) \[2\]              | 3 ( 1%) \[3\]             |                  1|              2|
| ACROCHORDON EXCISION | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| ACTINIC KERATOSIS    | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|
| AGITATION            | 2 ( 1%) \[2\] | 1 ( 0%) \[1\]              | 2 ( 0%) \[2\]             |                  1|              1|
| ALCOHOL USE          | 0 ( 0%) \[0\] | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|

Take a look at `order_layer_1` and compare it to the values within
`var1_Xanomeline High Dose` in the third position. Tplyr pulled out the
raw numeric values, so you can use them to sort. Looking through each of
the methods:

-   First, you need to specify that you want to use the `bycount` method
    of sorting. This tells Tplyr that you’re looking for the raw numeric
    values.
-   Next, you need to specify which column from the result variables you
    want to use. If you use the `cols` argument, simply provide multiple
    values, first by using the treatment group value desired, and then
    the `cols` arguments in order.
-   Last, you need to specify which value from the formatted results you
    want to extract. If not specified, this will default to the first
    value. Also note, the raw numeric value will be used - not the
    string formatted value converted back to numeric.

Let’s look at one last situation - multi-level count summaries.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% # Now doing a multi-level summary
      set_distinct_by(USUBJID) %>% 
      set_format_strings(f_str('xx (xx%) [x]', distinct, pct, n)) %>% 
      set_order_count_method('bycount') %>% # Specify to use the resulting counts for sorting
      set_ordering_cols('Xanomeline High Dose') %>% # Use the counts from Xanomeline High Dose treatment group
      set_result_order_var(n) # Use the raw numeric value of the n counts
  )

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1                                          | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:-----------------------------------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 21 (16%) \[48\] | 40 (27%) \[124\]           | 47 (28%) \[120\]          |                  1|              1|            Inf|
| APPLICATION SITE BLEEDING                            | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|
| APPLICATION SITE DERMATITIS                          | 5 ( 3%) \[9\]   | 7 ( 3%) \[12\]             | 9 ( 3%) \[15\]            |                  1|              1|             12|
| APPLICATION SITE DESQUAMATION                        | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|
| APPLICATION SITE DISCHARGE                           | 0 ( 0%) \[0\]   | 1 ( 0%) \[1\]              | 0 ( 0%) \[0\]             |                  1|              1|              1|
| APPLICATION SITE DISCOLOURATION                      | 0 ( 0%) \[0\]   | 0 ( 0%) \[0\]              | 1 ( 0%) \[1\]             |                  1|              1|              0|

Tplyr can handle these as well, but this is an area we’re going to
improve in the next release. Currently, Tplyr defaults to using the
VARN/Factor/alphanumeric sorting method for the outer variable, AEBODSYS
(just like the `by` variables, starting with searching for a
&lt;VAR&gt;N variable, then to factor variable, last to alphanumeric).
For the inside variable, the specified sorting method is used. This
currently covers many situations just fine - but we’re going to add the
additional flexibility to allow count based sorting for both the inside
and outside variable.

Figuring out how we wanted to approach sorting was itself quite a large
task, as we constantly try to balance flexibility with complexity of
use. Implementing these order methods was an even larger task. Kudos to
Eli for getting this as far as we have!

While we’re on the subject of multi-level counts, we’ve also added some
enhanced capabilities for presenting these situations by offering
nesting. The second variable is nested inside the first. You’re able to
turn this setting off:

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD)) %>% 
      set_nest_count(TRUE) 
  ) 

build(t) %>% 
  head() 
#> # A tibble: 6 x 7
#> # Groups:   ord_layer_1 [1]
#>   row_label1                        var1_Placebo  `var1_Xanomeline Hig… `var1_Xanomeline Low… ord_layer_index ord_layer_1 ord_layer_2
#>   <chr>                             <chr>         <chr>                 <chr>                           <int>       <dbl>       <dbl>
#> 1 "GENERAL DISORDERS AND ADMINISTR… " 48 ( 15.9%… "124 ( 27.3%)"        "120 ( 27.6%)"                      1           1         Inf
#> 2 "\tAPPLICATION SITE BLEEDING"     "  0 (  0.0%… "  0 (  0.0%)"        "  1 (  0.2%)"                      1           1           0
#> 3 "\tAPPLICATION SITE DERMATITIS"   "  9 (  3.0%… " 12 (  2.6%)"        " 15 (  3.4%)"                      1           1           9
#> 4 "\tAPPLICATION SITE DESQUAMATION" "  0 (  0.0%… "  0 (  0.0%)"        "  1 (  0.2%)"                      1           1           0
#> 5 "\tAPPLICATION SITE DISCHARGE"    "  0 (  0.0%… "  1 (  0.2%)"        "  0 (  0.0%)"                      1           1           0
#> 6 "\tAPPLICATION SITE DISCOLOURATI… "  0 (  0.0%… "  0 (  0.0%)"        "  1 (  0.2%)"                      1           1           0
```

You can also change the character used to set the indentation, which
defaults to `\t` for a tab.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(vars(AEBODSYS, AEDECOD))
  ) %>% 
  set_indentation("--->")

build(t) %>% 
  head() %>% 
  kable()
```

| row\_label1                                          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:-----------------------------------------------------|:--------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS | 48 ( 15.9%)   | 124 ( 27.3%)               | 120 ( 27.6%)              |                  1|              1|            Inf|
| —&gt;APPLICATION SITE BLEEDING                       | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DERMATITIS                     | 9 ( 3.0%)     | 12 ( 2.6%)                 | 15 ( 3.4%)                |                  1|              1|              9|
| —&gt;APPLICATION SITE DESQUAMATION                   | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DISCHARGE                      | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|              1|              0|
| —&gt;APPLICATION SITE DISCOLOURATION                 | 0 ( 0.0%)     | 0 ( 0.0%)                  | 1 ( 0.2%)                 |                  1|              1|              0|

Risk difference
---------------

Tplyr does not support, nor do we intended to support, a wide array of
statistical methods. Our goal is rather to take your focus as an analyst
off the mundane summaries so you can focus on the interesting analysis.
That said, there are some things that are common enough that we feel
that it’s reasonable for us to include. So let’s take a look at risk
difference.

Our current implementation of risk difference is solely built on top of
the base R function `prop.test`. For a any and all questions about this
method, please review the `prop.test` documentation within R.

Risk difference is built on top of count layers, as it’s a comparison of
proportions. To add risk difference into a count layer, you simply use
the function `add_risk_diff`. We made a large effort to make this flow
very naturally with the count layer construction, so let’s walk through
it step by step.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:--------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3%)     | 2 ( 0.4%)                  | 3 ( 0.7%)                 |                  1| -0.001 (-0.010, 0.008)               | -0.001 (-0.010, 0.008)              |              2|
| ACROCHORDON EXCISION | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              4|
| AGITATION            | 2 ( 0.7%)     | 1 ( 0.2%)                  | 2 ( 0.5%)                 |                  1| -0.001 (-0.010, 0.008)               | -0.001 (-0.010, 0.008)              |              5|
| ALCOHOL USE          | 0 ( 0.0%)     | 1 ( 0.2%)                  | 0 ( 0.0%)                 |                  1|                                      |                                     |              6|

Comparisons are specified with two-element character vectors. These are
simply you comparison group - the first element, and your reference
group - the second. This coincides with how you might see risk
difference specified in the header of your mock, where you’ll see
something like T1-Placebo. You can provide as many comparisons as you
want - the values specified in the comparison just need to be valid
treatment groups within your data. This works with any treatment group
built using `add_treat_group` or `add_total_group` as well.

The risk difference calculation are displayed in the `rdiff` columns.
There will be an `rdiff` column for every comparison that is made,
following the convention `rdiff_<comparison>_<reference>`.

Note the use of `suppressWarnings` - if the counts used in `prop.test`
are too low, you’ll get a warning that says “Chi-squared approximation
may be incorrect” for every time `prop.test` is run with counts that are
too low… This could happen a lot, but the warning is perfectly valid.

The default values that are displayed will be:

-   The difference
-   95% confidence interval low
-   95% confidence interval high

You have a good bit of control over these values though, and this can be
controlled in the same way you format the count summaries - using
`set_format_strings`.

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo')
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:---------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3) \[1\] | 1 ( 0.2) \[2\]             | 1 ( 0.2) \[3\]            |                  1| 0.002, 0.003, -0.001, -0.010, 0.008  | 0.002, 0.003, -0.001, -0.010, 0.008 |              2|
| ACROCHORDON EXCISION | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              4|
| AGITATION            | 1 ( 0.3) \[2\] | 1 ( 0.2) \[1\]             | 1 ( 0.2) \[2\]            |                  1| 0.002, 0.003, -0.001, -0.010, 0.008  | 0.002, 0.003, -0.001, -0.010, 0.008 |              5|
| ALCOHOL USE          | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              6|

Take a look at the `rdiff` columns now - you’ll see they have 5 values.
These are:

-   The comparison proportion (i.e. the estimate\[1\] output from a
    `prop.test` object)
-   The reference proportion (i.e. the estimate\[2\] output from a
    `prop.test` object)
-   The difference (i.e. estimate\[1\] - estimate\[2\])
-   The lower end of the confidence interval
-   The upper end of the confidence interval

You have the same control over the formatting of the display of these
values here as you do with the count summaries. Taking things a step
further, you can also pass forward arguments to `prop.test` using a
named list and the `args` argument in `add_risk_diff`. This wasn’t done
using the ellipsis (i.e. `...`) like typical R functions because it’s
already used to capture a varying number of comparisons, but it’s not
much more difficult to use:

``` r
t <- tplyr_table(adae, TRTA) %>% 
  add_layer(
    group_count(AEDECOD) %>% 
      set_distinct_by('USUBJID') %>% 
      add_risk_diff(
        c('Xanomeline High Dose', 'Placebo'),
        c('Xanomeline Low Dose', 'Placebo'),
        args = list(conf.level=0.95, alternative='less', correct=FALSE)
      ) %>% 
      set_format_strings(
        'n_counts' = f_str('xx (xx.x) [x]', distinct, distinct_pct, n),
        'riskdiff' = f_str('xx.xxx, xx.xxx, xx.xxx, xx.xxx, xx.xxx', comp, ref, dif, low, high)
      )
  )

suppressWarnings(build(t)) %>% 
  head() %>% 
  kable()
```

| row\_label1          | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index| rdiff\_Xanomeline High Dose\_Placebo | rdiff\_Xanomeline Low Dose\_Placebo |  ord\_layer\_1|
|:---------------------|:---------------|:---------------------------|:--------------------------|------------------:|:-------------------------------------|:------------------------------------|--------------:|
| ABDOMINAL DISCOMFORT | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              1|
| ABDOMINAL PAIN       | 1 ( 0.3) \[1\] | 1 ( 0.2) \[2\]             | 1 ( 0.2) \[3\]            |                  1| 0.002, 0.003, -0.001, -1.000, 0.005  | 0.002, 0.003, -0.001, -1.000, 0.006 |              2|
| ACROCHORDON EXCISION | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              3|
| ACTINIC KERATOSIS    | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              4|
| AGITATION            | 1 ( 0.3) \[2\] | 1 ( 0.2) \[1\]             | 1 ( 0.2) \[2\]            |                  1| 0.002, 0.003, -0.001, -1.000, 0.005  | 0.002, 0.003, -0.001, -1.000, 0.006 |              5|
| ALCOHOL USE          | 0 ( 0.0) \[0\] | 1 ( 0.2) \[1\]             | 0 ( 0.0) \[0\]            |                  1|                                      |                                     |              6|

One more note - the default of `add_risk_diff` works on the distinct
counts available within the count summary. If for whatever reason you’d
like to run risk difference on the non-distinct counts, switch the
`distinct` argument to FALSE. `add_risk_diff` also will function on
multi-level summaries no different than single level, so no concerns
there either.

Descriptive Statistic Layer Updates
-----------------------------------

You can see that there was a lot of attention given to counting over the
past three weeks - but descriptive statistics weren’t neglected either.
Let’s start simple - custom summaries no work properly on multi-variable
summaries:

``` r
tplyr_table(adsl, TRT01P) %>%
  add_layer(
    group_desc(vars(AGE, HEIGHTBL), by = "Sepal Length") %>%
      set_custom_summaries(
        geometric_mean = exp(sum(log(.var[.var > 0]), na.rm=TRUE) / length(.var))
      ) %>%
      set_format_strings(
        'Geometric Mean (SD)' = f_str('xx.xx (xx.xxx)', geometric_mean, sd)
      )
  ) %>% 
  build() %>% 
  kable()
```

| row\_label1  | row\_label2         | var1\_Placebo  | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose | var2\_Placebo   | var2\_Xanomeline High Dose | var2\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_2|
|:-------------|:--------------------|:---------------|:---------------------------|:--------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|
| Sepal Length | Geometric Mean (SD) | 74.70 ( 8.590) | 73.94 ( 7.886)             | 75.18 ( 8.286)            | 162.17 (11.522) | 165.51 (10.131)            | 163.11 (10.419)           |                  1|              1|

Not much more to it! Just use `.var` instead of the a distinct variable
name.

### Auto Precision

The more interesting development for descriptive statistics was the
addition of auto-precision. Auto-precision allows you to format your
numeric summaries based on the precision of the data collected.
Particularly when working with labs results, different tests may have
difference necessities for decimal precision depending on the numeric
range of the tests, the units the data are collected in, etc. So it is
common practice to vary the precision of the data being presented based
on the data collected. Furthermore, depending on the summary being
presented, you may wish to increase the precision further. For example,
you may want the mean to be at collected precision +1 decimal place, for
standard deviation +2.

Tplyr now handles these cases, and in Tplyr style, it’s intuitive and
easy to control. This has all been built into the format strings,
because a natural place to specify your desired format is where you
specify how you want your data presented. Now - if you wish to use
auto-precision, use `a` instead of `x` when creating your summaries.
Note that only one `a` is needed. To use increased precision, use `a+n`
where `n` is the number of additional spaces you wish to add.

``` r

tplyr_table(adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd)
      )
  ) %>% 
  build() %>% 
  kable()
```

| row\_label1 | row\_label2 | var1\_Placebo         | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:------------|:----------------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| ALB         | Mean (SD)   | 39.5 ( 3.22)          | 39.7 ( 2.61)               | 39.1 ( 2.79)              |                  1|              1|              1|
| ALP         | Mean (SD)   | 78.2 ( 66.55)         | 71.6 ( 43.11)              | 71.9 ( 22.01)             |                  1|              2|              2|
| ALT         | Mean (SD)   | 17.7 ( 11.89)         | 20.5 ( 10.08)              | 18.2 ( 8.93)              |                  1|              3|              3|
| AST         | Mean (SD)   | 23.5 ( 13.10)         | 23.5 ( 7.26)               | 23.0 ( 8.94)              |                  1|              4|              4|
| BILI        | Mean (SD)   | 10.485 ( 9.7213)      | 11.156 ( 5.5878)           | 9.417 ( 4.3167)           |                  1|              5|              5|
| BUN         | Mean (SD)   | 5.8354 ( 1.48851)     | 5.8530 ( 1.97958)          | 6.3194 ( 1.84470)         |                  1|              6|              6|
| CA          | Mean (SD)   | 2.283812 (0.0934973)  | 2.280342 (0.0994236)       | 2.286532 (0.1075204)      |                  1|              7|              7|
| CHOL        | Mean (SD)   | 5.566333 ( 0.9900913) | 5.510214 ( 0.9425105)      | 5.449229 ( 0.9427905)     |                  1|              8|              8|
| CK          | Mean (SD)   | 97.9 ( 99.81)         | 98.8 ( 92.46)              | 95.5 ( 64.31)             |                  1|              9|              9|
| CL          | Mean (SD)   | 105.7 ( 3.26)         | 105.2 ( 3.27)              | 105.7 ( 3.05)             |                  1|             10|             10|
| CREAT       | Mean (SD)   | 99.374 ( 17.2946)     | 104.902 ( 20.1506)         | 103.441 ( 19.6494)        |                  1|             11|             11|
| GGT         | Mean (SD)   | 24.0 ( 41.34)         | 23.7 ( 19.12)              | 22.6 ( 15.10)             |                  1|             12|             12|
| GLUC        | Mean (SD)   | 5.686409 ( 1.8253883) | 5.878607 ( 2.2279225)      | 5.507944 ( 1.6561578)     |                  1|             13|             13|
| K           | Mean (SD)   | 4.24 (0.403)          | 4.25 (0.364)               | 4.28 (0.390)              |                  1|             14|             14|
| PHOS        | Mean (SD)   | 1.156331 (0.1540930)  | 1.166697 (0.1637434)       | 1.145270 (0.1533291)      |                  1|             15|             15|
| PROT        | Mean (SD)   | 70.0 ( 4.58)          | 70.2 ( 4.23)               | 69.5 ( 4.63)              |                  1|             16|             16|
| SODIUM      | Mean (SD)   | 140.8 ( 2.66)         | 140.3 ( 3.08)              | 140.6 ( 2.62)             |                  1|             17|             17|
| URATE       | Mean (SD)   | 288.3687 ( 70.64847)  | 295.0609 ( 79.47892)       | 295.3140 ( 72.15064)      |                  1|             18|             18|

As you can see, the decimal precision is now varying depending on the
test being performed. Notice that both the integer and the decimal side
of each number fluctuate as well. Tpylr collects both the integer and
decimal precision, and you can specify both separately. For example, you
could use `x`’s to specify a default number of spaces for your integers
that are used consistently across by variables, but vary the decimal
precision based on collected data. You can also increment the number of
spaces for both integer and decimal separately.

But - this is kind of ugly, isn’t it? Do we really need all 5 decimal
places collected for CA? For this reason, you’re able to set a cap on
the precision that’s displayed:

``` r
tplyr_table(adlb, TRTA) %>% 
  add_layer(
    group_desc(AVAL, by = PARAMCD) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      )
  ) %>% 
  build() %>% 
  head() %>% 
  kable()
```

| row\_label1 | row\_label2 | var1\_Placebo    | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|
|:------------|:------------|:-----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|
| ALB         | Mean (SD)   | 39.5 ( 3.22)     | 39.7 ( 2.61)               | 39.1 ( 2.79)              |                  1|              1|              1|
| ALP         | Mean (SD)   | 78.2 ( 66.55)    | 71.6 ( 43.11)              | 71.9 ( 22.01)             |                  1|              2|              2|
| ALT         | Mean (SD)   | 17.7 ( 11.89)    | 20.5 ( 10.08)              | 18.2 ( 8.93)              |                  1|              3|              3|
| AST         | Mean (SD)   | 23.5 ( 13.10)    | 23.5 ( 7.26)               | 23.0 ( 8.94)              |                  1|              4|              4|
| BILI        | Mean (SD)   | 10.485 ( 9.7213) | 11.156 ( 5.5878)           | 9.417 ( 4.3167)           |                  1|              5|              5|
| BUN         | Mean (SD)   | 5.835 ( 1.4885)  | 5.853 ( 1.9796)            | 6.319 ( 1.8447)           |                  1|              6|              6|

Now that looks better. The `cap` argument is part of
`set_format_strings`. You need to specify the integer and decimal caps
separately. Note that integer precision might not behave like you expect
- it doesn’t make sense to truncate an integer if it’s value is too
high, so if the integer exceeds the allotted space, then the length of
the string will increase and the full value will be displayed. But
values that are short enough will only pad to the capped number of
spaces. We plan to implement a warning in future releases if integers
exceed the set display space allocation.

This was a basic situation, but if you’re paying close attention, you
may have some questions. What if you have more by variables, like by
visit AND test. Do we then calculate precision by visit and test? What
if collected precision is different per visit and we don’t want that?
What about multiple summary variable? How do we determine precision
then? We have modifier functions for this:

``` r
tplyr_table(adlb, TRTA, where = SAFFL=='Y' & AVISIT != '') %>% 
  add_layer(
    group_desc(vars(AVAL, CHG, BASE), by = vars(AVISIT,PARAMCD)) %>% 
      set_format_strings(
        'Mean (SD)' = f_str('a.a+1 (a.a+2)', mean, sd),
        cap = c(int=3, dec=2)
      ) %>% 
      set_precision_on(AVAL) %>% 
      set_precision_by(PARAMCD)
  ) %>%
  build() %>% 
  head() %>% 
  kable()
```

| row\_label1 | row\_label2 | row\_label3 | var1\_Placebo   | var1\_Xanomeline High Dose | var1\_Xanomeline Low Dose | var2\_Placebo | var2\_Xanomeline High Dose | var2\_Xanomeline Low Dose | var3\_Placebo   | var3\_Xanomeline High Dose | var3\_Xanomeline Low Dose |  ord\_layer\_index|  ord\_layer\_1|  ord\_layer\_2|  ord\_layer\_3|
|:------------|:------------|:------------|:----------------|:---------------------------|:--------------------------|:--------------|:---------------------------|:--------------------------|:----------------|:---------------------------|:--------------------------|------------------:|--------------:|--------------:|--------------:|
| Baseline    | ALB         | Mean (SD)   | 39.8 ( 2.81)    | 40.3 ( 2.84)               | 39.8 ( 2.56)              | ()            | ()                         | ()                        | 39.8 ( 2.81)    | 40.3 ( 2.84)               | 39.8 ( 2.56)              |                  1|              0|              1|              1|
| Baseline    | ALP         | Mean (SD)   | 77.7 ( 58.11)   | 71.0 ( 38.85)              | 73.3 ( 20.72)             | ()            | ()                         | ()                        | 77.7 ( 58.11)   | 71.0 ( 38.85)              | 73.3 ( 20.72)             |                  1|              0|              2|              2|
| Baseline    | ALT         | Mean (SD)   | 17.6 ( 9.22)    | 19.2 ( 10.05)              | 18.0 ( 8.72)              | ()            | ()                         | ()                        | 17.6 ( 9.22)    | 19.2 ( 10.05)              | 18.0 ( 8.72)              |                  1|              0|              3|              3|
| Baseline    | AST         | Mean (SD)   | 23.2 ( 7.50)    | 23.1 ( 6.61)               | 23.4 ( 8.24)              | ()            | ()                         | ()                        | 23.2 ( 7.50)    | 23.1 ( 6.61)               | 23.4 ( 8.24)              |                  1|              0|              4|              4|
| Baseline    | BILI        | Mean (SD)   | 9.703 ( 3.9645) | 11.034 ( 5.3512)           | 9.447 ( 4.0146)           | ()            | ()                         | ()                        | 9.703 ( 3.9645) | 11.034 ( 5.3512)           | 9.447 ( 4.0146)           |                  1|              0|              5|              5|
| Baseline    | BUN         | Mean (SD)   | 5.538 ( 1.3851) | 5.754 ( 1.8837)            | 6.374 ( 1.9691)           | ()            | ()                         | ()                        | 5.538 ( 1.3851) | 5.754 ( 1.8837)            | 6.374 ( 1.9691)           |                  1|              0|              6|              6|

Three variables are being summarized here - AVAL, CHG, and BASE. So
which should be used for precision? `set_precision_on` allows you to
specify this, where the `precision_on` variable must be one of the
variables within `target_var`. Similarly, `set_precision_by` changes the
`by` variables used to determine collected precision. If no
`precision_on` variable is specified, the first variable in `target_var`
is used. If not `precision_by` variables are specified, then the default
`by` variables are used.

Next Steps
==========

Our next release of Tplyr will be our first full release. This means
that:

-   We will be posting to CRAN
-   We’re going to include a full and indpendently tested UAT document
-   We’ll have a full suite of vignettes included to enhance our
    available documentation

The next wave of updates is mostly going to be tweaks and sugar, where
we will try to make sure that Tplyr is as user friendly and practical as
possible. In the mean time, if you have any comments, feedback, or find
some bugs - drop us an issue. We’d love to hear any feedback if you’ve
taken Tplyr for a test drive.


