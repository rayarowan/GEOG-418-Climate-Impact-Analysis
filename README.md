# GEOG 418 Final Assignment - Climate Impact Analytsis

By Raya Rowan

November, 2024

## Introduction

Each summer BC experiences raging wildfire activity across the province, destroying everything in its path such as wildlife habitats and residential communities. Damage such as altered drainage systems, unstabilized exposed soils, and destroyed infrastructure posed to these communities is detrimental and can last for years before full recovery is achieved (BC Wildfire Service, 2023). Furthermore, due to wildfire resulting in ground instability, post-fire hazards such as soil erosion, floods, landslides, and avalanches become an increasing concern (BC Wildfire Service, 2023). It is hypothesized that the rising temperatures experienced from May to September can be correlated with increased wildfire frequency and size in BC, suggesting wildfires may be a direct concequence of climate (Xu, 2014). Using this theory, high temperature factors such as drier soils and reduced fuel moisture can be linked to the location of wildfires in BC, providing a deeper understanding of what may have contributed to the dramatic rise in fire activity during the summer (Xu, 2014).

The objective of this tutorial is to use RStudio to determine if temperature is correlated to the location and size of wildfires in BC in the summer of 2021. To conduct the following research, descriptive statistics, point pattern analysis, spatial autocorrelation, spatial interpolation and geographically weighted regression techniques will be conducted. Given the results of this study ideally we can answer further questions such as what patterns we might expect to see moving forward with inreasing temperatures each year due to anthropogenic climate change. 

Results from the above statistical techniques will be displayed as tables, maps, and graphs throughout this tutorial.

The first step is to install the desired packages. When a package is installed using the function intsall.packages("package") it is stored in a directory called the library (https://www.datacamp.com/doc/r/packages, 2024). These libraries are collections of pre-written code used to complete specific tasks while still maintaining control over R's flow (Woke, 2023). They are beneficial as they act like a short cut, reducing the amount of code needed to be written and can be called upon in the script when needed (Woke, 2023). To install a library we use the code library("package"). After installation of the library R will contain documentation specifying proper syntax and use for that package. This information can be found under Packages in the panel on the bottom right side of the page. The below packages are what will be used for this studies statistical analyses.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Install packages if not already installed:
install.packages("knitr")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("e1071")
install.packages("spgwr")

#Load in libraries:
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("e1071")
library(spgwr)
library(sf)     
library(gstat)   
library(ggplot2) 
library(dplyr)   
library(viridis)
```
The next step is to set the working directory. This will be the folder or location on your computer that R pulls files from and saves your work to. Do set the working directory use the code dir <- "name of folder/location" followed by setwd(dir). An example of how this is done can be viewed below. I am pulling my data stored in a folder labeled Assignment 4 within a folder labelled GEOG 418 located on my desktop.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
dir <- "~/Desktop/GEOG 418/Assignment 4"
setwd(dir)
```

## Study Area
British Columbia, Canada was selected to determine temperature impacts on wildfires from May to September, 2021 because of its dense forested area and ongoing history of wildfire occurance. 

### Data
To conduct this report historical fire point data was collected from the BC Data Catalogue database, retrieved from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical. The fire data includes relevant information such as fire number, fire year, ignition date, latitude, longitude and fire size (ha). Temperature data was collected from the Weather Station Data Portal from the Pacific Climate Impacts Consortium (PCIC), retreived from https://services.pacificclimate.org/met-data-portal-pcds/app/. The temperature data was parameterized to be collected from May 1st, 2021 to September 1st, 2021 from the WMB network and includes wind direction, relative humidity, average wind speed, precipitation, temperature and time. 

### Descriptive Statistics
The first step in conducting this research is to perform descriptive statistics on the fire point data. 

The below code displays how we read the fire point .csv from the current working directory into a dataframe called data. Since the dataframe contains dates ranging across 2021, we want to filter the data to be from May 1st, 2021 to September 1st, 2021 and store the filtered data in a new object called fire_point. To do this we must extract the IGN_DATE from the dataframe and take the first 8 characters of the IGN_DATE column. We then convert the extracted substring to the date type using the format %Y%m%d (year, month, day).

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
# Load wildfire point data and set CRS if needed
data <- read.csv("H_FIRE_PNT.csv")
# Extract just the date part from IGN_DATE and convert it to Date type
data$IGN_DATE <- as.Date(substr(data$IGN_DATE, 1, 8), format = "%Y%m%d")
# Filter data for dates between May 1, 2021, and September 1, 2021
fire_point <- data %>%
  filter(IGN_DATE >= as.Date("2021-05-01") & IGN_DATE <= as.Date("2021-09-01"))
```
We then create a dataframe (df) from the csv file.
```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
df <- as.data.frame(fire_point) 
```

We are now ready to calcualte our descriptive statistics (mean, mode, standard deviation, median, skewness, kurtosis, Coefficient of Variation and normal distribution).
```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
# Mean
mean_size <- mean(df$SIZE_HA, na.rm = TRUE)
# Mode 
mode_size <- as.numeric(names(sort(table(df$SIZE_HA), decreasing = TRUE))[1])
# Standard Deviation
sd_size <- sd(df$SIZE_HA, na.rm = TRUE) 
# Median
med_size <- median(df$SIZE_HA, na.rm = TRUE)
# Skewness
skew_size <- skewness(df$SIZE_HA, na.rm = TRUE)[1]
# Kurtosis
kurt_size <- kurtosis(df$SIZE_HA, na.rm = TRUE)[1]
# Coefficient of Variation
cov_size <- (sd_size / mean_size) * 100
# Normal Distribution test
norm_size <- shapiro.test(df$SIZE_HA)$p.value
```

To display these results in a table use the following code.
```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
# Data for Table 1
data.for.table1 <- data.frame(
  Sample = c("Fire Size (ha)"),
  Mean = round(mean_size, 3),
  SD = round(sd_size, 3),
  Median = round(med_size, 3),
  Mode = round(mode_size, 3)
)

# Data for Table 2
data.for.table2 <- data.frame(
  Sample = c("Fire Size (ha)"),
  Skewness = round(skew_size, 3),
  Kurtosis = round(kurt_size, 3),
  CoV = round(cov_size, 3),
  Normality = round(norm_size, 5)
)

# Create Table 1 with Caption
table1 <- tableGrob(data.for.table1, rows = c(""), theme = ttheme_default(base_size = 12))
t1Caption <- textGrob("Table 1: Descriptive Statistics for Summer 2021 Fire Size.",
                      gp = gpar(fontsize = 12))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)

# Create Table 2 with Caption
table2 <- tableGrob(data.for.table2, rows = c(""), theme = ttheme_default(base_size = 12))
t2Caption <- textGrob("Table 2: Descriptive Statistics for Summer 2021 Fire Size.",
                      gp = gpar(fontsize = 12))

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)

# Export Table 1 as PNG
png("Output_Table1.png", width = 1000, height = 500, res = 150)  # Adjusted size and resolution
grid.arrange(table1, newpage = TRUE)
dev.off()

# Export Table 2 as PNG
png("Output_Table2.png", width = 1000, height = 500, res = 150)  # Adjusted size and resolution
grid.arrange(table2, newpage = TRUE)
dev.off()
```
![Output_Table1](https://github.com/user-attachments/assets/9a8ab686-fc92-4e70-b5d9-0f177b5e03b1)

![Output_Table2](https://github.com/user-attachments/assets/533ccb07-eac1-4e3a-836b-ae1c0c3022ce)



## References

Woke, G. (2023, March 24). The difference between libraries and frameworks. Simple Talk. https://www.red-gate.com/simple-talk/development/other-development/the-difference-between-libraries-and-frameworks/

Xu, Z. (2014). Predicting wildfires and measuring their impacts: Case studies in British Columbia (Doctoral dissertation). University of Victoria.
