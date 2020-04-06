# **Germany Temperature and Precipitation Infographic**
In this repository you will found how to extract temporal temperature and precipitation data using R from monthly datasets like ftp://opendata.dwd.de/. The process from the data download to the plot of the information is described step by step in the sections below. The final result consist in the summary of all graphs generated from the data as an Infographics. Inside [Opendata](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/) more monthly datasets can be used with this code like [drought_index](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/drought_index/) and [sunshine_duration](ftp://opendata.dwd.de/climate_environment/CDC/grids_germany/monthly/sunshine_duration/).

![InfoGraph](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/InfoGraph.png)

1.**Data Extraction**

In this section and using the script  [1.Data_Extraction](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/1.Data_Extraction.R) the information of surface air temperature and precipitation from the webpage will be  downloaded and converted into .csv files. The key points in this code are: First, the directory creation due to the hole file and folder system for the rest of the code is created in this step. 

2.**Big Circles Plot**

![Mill_Graph](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Mill_Graph.png)

3.**Stripes Plot**

![Year_Stripes](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Year_Stripes.png)

2.**Bivariate Map**
![Bivariate](https://github.com/ajcastanedag/Germany_TP_infographic_MB2/blob/master/Graph_Sample/Bivariate.png)




