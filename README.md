# **Germany Temperature and Precipitation Infographic**
In this repository you will found how to extract temporal temperature and precipitation data using R from monthly datasets from ftp://opendata.dwd.de/. The process from the data download to the plot of the information is described step by step in the sections below. The final result consist in the summary of all graphs generated from the data as an Infographics. Inside [Opendata](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/) more monthly datasets can be used with this code like [drought_index](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/drought_index/) and [sunshine_duration](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/sunshine_duration/). Once the data extraction is performed all graphs can be produced independently due to they extract the values either from the .csv generated or the grid files. The aim of this code is to extract information from multitemporal datasets in Germany to describe how the temperature and the precipitations have variated between 1881 and 2019.  The final infographics is achieved with [Inkscape](https://inkscape.org/en/) by combining the exported plots.

![InfoGraph](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/InfoGraph.png)

## **1.Data Extraction**

In this section and using the script  [1.Data_Extraction](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/1.Data_Extraction.R) the information of surface air temperature and precipitation from the webpage will be  downloaded and converted into .csv files. The key points in this code are: First, the directory selection due to the whole folder system for the rest of the code and graphs is created in this step. In line #27 "Main_Fo <- choose.dir()" can be changed for any empty previously created directory. 

Second, the data download section starting on  line #91 will loop over all month folders from the web page and download all .asc.gz files keeping the same structure. In this step the years is obtained so its important to running it each time to assure the Years variable is correctly created. To avoid downloading data over and over, before each download step the existence of each file is checked so the loop can be run many times and  no duplicates will be downloaded, instead, "grid file already exist..." message will be printed.   

Third,  read ASCII-Grid-File step is executed in other big loop. In this step all .asc.gz files are read  and particular modifications are performed such as appliance of coordinate system (EPSG: 31467) and values modifications like Temp/10 accordingly to Description.pdf files found in each dataset. This part of the code was inspired and based on [this](https://github.com/wegmann/R_scripts/blob/master/Summer_Weather_Statistics.R) GitHub repository. As well, in this step Large RasterStack is created and saved for each month containing all years as layers. 

Finally, a data frame is created and for each month in each year the mean temperature in all Germany is calculated, as well the minimum, maximum and mean value in all months is calculated per year. The data frame structure looks like the following table and its exported as a .csv file. Once the values are calculated the whole procedure is repeated for the precipitation dataset. 

| Year | January | February | March | April | May   | June  | July  | August | …    | Min   | Max   | Mean  |
| ---- | ------- | -------- | ----- | ----- | ----- | ----- | ----- | ------ | ---- | ----- | ----- | ----- |
| 1881 | -5.41   | 0.06     | 2.80  | 5.55  | 11.86 | 15.11 | 18.66 | 15.89  | …    | -5.41 | 18.66 | 7.35  |
| 1882 | 0.43    | 1.71     | 6.16  | 7.73  | 12.01 | 14.36 | 16.78 | 14.89  | …    | 0.43  | 16.78 | 8.39  |
| 1883 | -0.29   | 2.48     | -1.46 | 6.09  | 12.42 | 16.28 | 16.67 | 15.92  | …    | -1.46 | 16.67 | 7.94  |
| 1884 | 2.92    | 2.85     | 4.87  | 6.08  | 12.71 | 13.12 | 18.24 | 17.02  | …    | 1.72  | 18.24 | 8.62  |
| …    | …       | …        | …     | …     | …     | …     | …     | …      | …    | …     | …     | …     |
| 2015 | 2.19    | 0.72     | 5.18  | 8.42  | 12.35 | 15.82 | 19.39 | 19.87  | …    | 0.72  | 19.87 | 9.94  |
| 2016 | 1.01    | 3.33     | 4.01  | 7.89  | 13.69 | 16.99 | 18.61 | 17.72  | …    | 1.01  | 18.61 | 9.55  |
| 2017 | -2.15   | 2.90     | 7.20  | 7.42  | 14.13 | 17.76 | 18.06 | 17.87  | …    | -2.15 | 18.06 | 9.58  |
| 2018 | 3.72    | -1.87    | 2.36  | 12.32 | 16.03 | 17.73 | 20.29 | 19.91  | …    | -1.87 | 20.29 | 10.45 |
| 2019 | 0.63    | 3.95     | 6.58  | 9.62  | 10.99 | 19.78 | 18.90 | 19.06  | …    | 0.63  | 19.78 | 10.28 |

## **2.Big Circles Plot**

In this section, using the script  [2.Circles.R](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/2.Circles.R) the data extracted before will be plotted. The graph is a   [Bar Chart](https://ggplot2.tidyverse.org/reference/geom_bar.html) wrapped in the y axis by the usage of [Polar coordinates](https://ggplot2.tidyverse.org/reference/coord_polar.html).  It is mainly portraying in different colors the variations  of the temperature by month through years. Each concentric ring represent a single year as shown in the legend of the image, being the smallest ring 1881 and the outer ring 2019. Due to the need of a full year cycle to close each ring, the data frame is filtered and all new data will not be shown until December of that year is included. To show the monthly behavior, each month has the mean temperature value though all years as a white line; a black area to track the data variations and a gray curve to show the general trend of each month. In this graph each "month slice" has different minimum and maximum values but those are adjusted at the span of the slice, the actual values can be checked in the data frames.   

![Mill_Graph](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Mill_Graph.png)

## **3.Stripes Plot**

This plot is achieved though [3.Stripes.R](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/3.Stripes.R) script and was made to plot the temperature and precipitation mean values variations through the years. Its based on [Dr. Dominic Royé](https://dominicroye.github.io/en) [Post](https://dominicroye.github.io/en/2018/how-to-create-warming-stripes-in-r/) and includes in color the value variations, in a black line the actual data scaled to the height of the stripes and in a curve the trend of the data. 

![Year_Stripes](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Year_Stripes.png)

## **4.Bivariate Map**

Once all data is gathered by its yearly and monthly variations,  the distribution of the variables inside different  administrative areas is made based on a [Biscale](https://github.com/slu-openGIS/biscale) maps using the [4.Bivariate.R](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/4.Bivariate.R) script. The administrative areas are downloaded from [gadm](https://gadm.org/) and consist in 4 levels being the level 1 the states and in level 4  municipalities in Germany. 

![Bivariate](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Bivariate.png)




