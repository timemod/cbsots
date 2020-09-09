cbsots
================
Rob van Harrevelt
2020-09-09

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/timemod/cbsots.svg?branch=master)](https://travis-ci.org/timemod/cbsots)

Retrieve timeseries from the [CBS open data
interface](http://www.cbs.nl/nl-NL/menu/cijfers/statline/open-data/default.htm)
interface, employing package
[cbsodataR](https://github.com/edwindj/cbsodataR).

# Introduction

Package [cbsodataR](https://github.com/edwindj/cbsodataR) can be used to
retrieve data of CBS tables using R code. Most tables are complex
multi-dimensional tables, and extracting timeseries with appropriate
names will usually require some further R programming. The purpose of
`cbsots` is to take this programming off your hands.

The packages provides the Shiny App “CBS Timeseries Coding” for
specifying which portions of the table you want to retrieve and how the
names of the individual timeseries (the so-called “timeseries coding”)
should be constructed, and a separate function `get_ts` for actually
creating the timeseries based on the timeseries coding created with the
Shiny App.

The workflow when using package `cbsots` to retrieve timeseries thus
consists of two separate steps:

1.  Create or modify the timeseries coding for one or more CBS tables
    with Shiny App “CBS Timeseries Coding”. The timeseries coding is
    stored in an rds file.
2.  Use function `get_ts` to retrieve the timeseries, using the
    timeseries coding read from the rds file created in step 1.

Step 1 (using the Shiny App) is only necessary if you add a new table,
or if you want to modify the timeseries coding for an existing table.

# Installation

``` r
# first install library drat if you have not installed that library yet:
install.packages("drat")

# now install cbsots with the timemod drat Repository
drat::addRepo("timemod")
install.packages("cbsots")
```

# The Shiny App “CBS Timeseries Coding”

The Shiny application is started by calling the function `edit_ts_code`.
For example,

``` r
library(cbsots)
edit_ts_code("tscode.rds")
```

The first argument (`"tscode.rds"`) specifies the name of the rds file
where the timeseries coding is stored. The file does not have to exist
yet. If it does not exist, then the file is created when the timeseries
coding is saved.

More information about the Shiny App “CBS Timeseries Coding” will be
provided in a future Tutorial and a Manual, which unfortunately still
have to be written. In this README file the Shiny App will be presentled
briefly to give you an idea about the basic functionality. Here is an
example of the Shiny App with already specified timeseries coding for
table `70076NED` (consumption in the Netherlands) (some texts are quite
small and may be difficult to read, but below I will zoom in on relevant
parts of the Shiny App):

![The Shiny App](readme_data/CBS%20Timeseries%20Coding.png)

The left panel contains several buttons. The right panel shows, among
others, two tabs containing a table. Let us have a closer look at the
table in the selected tab:

![](readme_data/CBS%20Timeseries%20Coding_Topic.png)

CBS table `70076NED` contains two dimensions: “Topic” and
“GoederenEnDiensten” (Goods and Services), besides the time dimension
(years, quarters etc.) that is disregarded in the Shiny App. Every CBS
table has a dimension called “Topic”, and usually has one or more extra
dimensions, such as “GoederenEnDiensten” for this table.

For this particular table, the different “Topics” correspond to
different types of consumption timeseries, for example volumemutaties
(volume changes), waardemutaties (value changes), and indexcijfers
(indices) for both volume and value. Each Topic has a *Key* (a short
identifier) and a *Title* (a more detailed description). The *Select*
and *Code* columns are the columns that can be modified. In this
example, we are only interest in indices for both volumes and values, so
only the corresponding entries have been selected. The text in the Code
column is used to create the timeseries names, as explained below.

In the next figure the table for dimension “GoederenEnDiensten” (Goods
and Services) is shown:

![](readme_data/CBS%20Timeseries%20Coding_GoederenEnDiensten.png)

For this dimension, I have selected “Consumptie binnenland” (domestic
consumption), “Goederen” (Goods) and “Diensten” (Services).

Function `get_ts` will create a timeseries for each combination of
selected “Topic” and “GoederenEnDiensten”. The names of the timeseries
are created by pasting the texts in the Code columns for each dimension.
In this case, the codes for “Topic” are the suffixes and the codes for
“GoederenEnDiensten” are the prefixes. So the names of the timeseries
will be `c___vi`, `c_govi`, `c_divi`, `c___wi`, `c_gowi`, `c_divi`.
Which dimension is used for the prefixes and which dimension for the
suffixes of the timeseries names is determined by the ordering of the
buttons above the tabs with dimension table, above the Search text
field. See the selection of the Shiny App below:

![](readme_data/CBS%20Timeseries%20Coding_order_buttons.png)

The ordering the these buttons can be changed in the Shiny App: move the
mouse cursor to the button `GoederenEnDiensten`, then press the left
mouse button and drag it to the right of button `Topic`. Now you can
release the button. As a result you would get:

![](readme_data/CBS%20Timeseries%20Coding_order_buttons_2.png)

For this ordering, the timeseries names would become `vic___`, `vic_go`,
etc.

After making any changes to the timeseries coding, you should press the
`Save codes` button on the lower left corner of the Shiny App. The Shiny
App does not automatically save changes.

# Function get\_ts

To create the timeseries, first read the timeseries coding created by
the Shiny App:

``` r
ts_code <- readRDS("tscode.rds")
```

Then function `get_ts` can be used to finally create the timeseries:

``` r
# Return only yearly and quarterly series (skip monthly series) and 
# for years 2017 and later
data <- get_ts("70076NED", ts_code, frequencies = "yq",
               min_year = "2017")
```

    ## Downloading table 70076ned ...
    ## Filters:
    ## $Perioden
    ##  [1] "2017KW01" "2017KW02" "2017KW03" "2017KW04" "2017JJ00" "2018KW01"
    ##  [7] "2018KW02" "2018KW03" "2018KW04" "2018JJ00" "2019KW01" "2019KW02"
    ## 
    ## $GoederenEnDiensten
    ## [1] "      8000" "      4005" "      5000"

    ## Retrieving data from table '70076ned'

    ## http://opendata.cbs.nl/ODataFeed/odata/70076ned/TypedDataSet?$format=json&$filter=(Perioden%20eq%20'2017KW01'%20or%20Perioden%20eq%20'2017KW02'%20or%20Perioden%20eq%20'2017KW03'%20or%20Perioden%20eq%20'2017KW04'%20or%20Perioden%20eq%20'2017JJ00'%20or%20Perioden%20eq%20'2018KW01'%20or%20Perioden%20eq%20'2018KW02'%20or%20Perioden%20eq%20'2018KW03'%20or%20Perioden%20eq%20'2018KW04'%20or%20Perioden%20eq%20'2018JJ00'%20or%20Perioden%20eq%20'2019KW01'%20or%20Perioden%20eq%20'2019KW02')%20and%20(GoederenEnDiensten%20eq%20'%20%20%20%20%20%208000'%20or%20GoederenEnDiensten%20eq%20'%20%20%20%20%20%204005'%20or%20GoederenEnDiensten%20eq%20'%20%20%20%20%20%205000')

    ## Done!

The result is a list with class `table_ts`, with components `"Y"`
(annual timeseries) and `"Q"` (quarterly timeseries):

``` r
data
```

    ## table_ts object
    ## Frequency Y :
    ## Topleft part of the result (the first 6 rows and 10 columns):
    ##      c___vi c___wi c_divi c_diwi c_govi c_gowi
    ## 2017  111.6  147.4  120.6  170.9  102.5  124.6
    ## 2018  114.4  153.8  123.5  178.0  105.2  130.3
    ## Frequency Q :
    ## Topleft part of the result (the first 6 rows and 10 columns):
    ##        c___vi c___wi c_divi c_diwi c_govi c_gowi
    ## 2017Q1  108.6  144.9  117.6  167.9   99.5  122.6
    ## 2017Q2  111.3  147.1  120.6  171.6  101.8  123.3
    ## 2017Q3  111.0  145.8  121.6  172.5  100.3  119.7
    ## 2017Q4  115.6  153.8  122.5  171.8  108.7  136.2
    ## 2018Q1  111.8  151.5  120.4  174.5  103.2  129.3
    ## 2018Q2  114.2  153.5  123.7  178.8  104.4  128.8

# Further Documentation

[Reference manual](cbsots.pdf)

<!--
[Vignette](pkg/vignettes/cbsots.pdf)
-->
