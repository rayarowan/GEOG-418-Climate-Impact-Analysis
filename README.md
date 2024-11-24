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

To display the study area we will now create a map of BC which includes all the fire points as well as the mean center. The first step will be to load in the boundary shapefile and set the coordinate reference system (CRS) of the BC bounadry to EPSG:4326, which corresponds to the WGS84 geographic coordinate system (latitude and longitude).

```{r Load in Shapefile, echo=TRUE, eval=TRUE, warning=FALSE}
#maps package
bc_boundary <- st_read("BC_bound.shp")
bc_boundary <- st_set_crs(bc_boundary, 4326)
```
After this we create spatial points for wildfire locations by extracting the longitude and latitude columns from the df data frame to create a set of coordinates. The CRS is then set to 4326 and 'firePoints' is created to represent wildfire locations.
```{r Create Spatial Points, echo=TRUE, eval=TRUE, warning=FALSE}
#tm package
coords <- df[, c("LONGITUDE", "LATITUDE")] #store coordinates in new object
crs <- CRS("+init=epsg:4326") #store the coordinate system (CRS) in a new object
firePoints <- SpatialPointsDataFrame(coords = coords, data = df, proj4string = crs) #make new spatial points object using coodinates, data, and projection
```
The mean center point is then calculated and a spatial points object (meanCenterPoint) is created to represent the mean center of the wildfire points.
```{r Mean Center Point, echo=TRUE, eval=TRUE, warning=FALSE}
#calculate and add mean to map center 
meanCenter <- data.frame(name = "Mean Center of fire points", long = mean(df$LONGITUDE), lat = mean(df$LATITUDE))
coords2 <- meanCenter[, c("long", "lat")]
crs2 <- CRS("+init=epsg:4326")
meanCenterPoint <- SpatialPointsDataFrame(coords = coords2, data = meanCenter, proj4string = crs2)
```
Using the tmap package we installed earlier we now create and print the map as a PNG with a caption (TmMap.png). This output will be found in your working directory.
```{r Creating Map, echo=TRUE, eval=TRUE, warning=FALSE}
map_TM <- tm_shape(bc_boundary) + 
  tm_fill(col = "gray50") +  
  tm_shape(firePoints) +
  tm_symbols(col = "red", alpha = 0.3) +
  tm_shape(meanCenterPoint) +
  tm_symbols(col = "blue", alpha = 0.8) +
  tm_add_legend(type = "symbol", labels = c("Fire Points", "Mean Center"), col = c(adjustcolor( "red", alpha.f = 0.3), adjustcolor( "blue", alpha.f = 0.8)), shape = c(19,19)) +
  tm_layout(title = "BC Fire Locations Summer 2021", title.position = c("LEFT", "BOTTOM"), legend.position = c("RIGHT", "TOP"))

map_TM

# Save the map with caption
png("TmMap.png", width = 1200, height = 900, res = 150)  # Adjust size/resolution as needed

# Combine map and caption
map_grob <- tmap_grob(map_TM)  # Convert tmap object to grob
caption <- textGrob("Figure 1: BC Fire Locations and Mean Center for Summer 2021",
                    gp = gpar(fontsize = 12), hjust = 0.5, x = 0.5)

# Arrange map and caption vertically
grid.arrange(map_grob, caption, ncol = 1, heights = c(0.9, 0.1))  # 90% map, 10% caption

dev.off()
```
![TmMap](https://github.com/user-attachments/assets/880fd163-f7f2-4a26-bf87-debe3a70d3da)

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
```{r DataFrame, echo=TRUE, eval=TRUE, warning=FALSE}
df <- as.data.frame(fire_point) 
```
We are now ready to calcualte our descriptive statistics (mean, mode, standard deviation, median, skewness, kurtosis, Coefficient of Variation and normal distribution). The descripitve statistics will be based on wildfire sizes (ha). 

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

To display these results in a table use the below code.
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
![Output_Table1](https://github.com/user-attachments/assets/9a8ab686-fc92-4e70-b5d9-0f177b5e03b1) ![Output_Table2](https://github.com/user-attachments/assets/533ccb07-eac1-4e3a-836b-ae1c0c3022ce)

To analyse the size frequency of fires as well as the frequency of fires per month in BC during the summer of 2021 we can create a histogram and bar graph using the following code. 
```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
# Histogram to display the size frequency of fires
hist(df$SIZE_HA, breaks = 30, main = "Summer 2021: Frequency of Wild Fire Sizes", xlab = "Size of Wild Fire (ha)")
png("Output_Histogram.png") 
dev.off()

# Histogram to display the frequency of fires/month
# Convert IGN_DATE to Date format
df$IGN_DATE <- as.Date(substr(df$IGN_DATE, 1, 8), format = "%Y%m%d"
# Add a new column for the month of the ignition date
df$IGN_Month <- format(df$IGN_DATE, "%B")  # Converts to full month names (e.g., "January")

# Bar Graph
sumMay = sum(subset(df, IGN_Month == "May")$SIZE_HA, na.rm = TRUE) 
sumJun = sum(subset(df, IGN_Month == "June")$SIZE_HA, na.rm = TRUE) 
sumJul = sum(subset(df, IGN_Month == "July")$SIZE_HA, na.rm = TRUE) 
sumAug = sum(subset(df, IGN_Month == "August")$SIZE_HA, na.rm = TRUE) 
sumSep = sum(subset(df, IGN_Month == "September")$SIZE_HA, na.rm = TRUE) 
months = c("May","June","July", "August", "September")

barGraph <- df %>% # store graph in bar graph variable and pass data frame as first argument in next line
  group_by(IGN_Month) %>% # use data frame and group by month and pass to first argument in next line
  summarise(sumSize = sum(SIZE_HA, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  mutate(IGN_Month = factor(IGN_Month, levels = month.name)) %>% # Order by full month names
  ggplot(aes(x = IGN_Month, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity") + 
  labs(
    title = "Total Burned Area by Month 2021", 
    x = "Month", 
    y = "Total Burned Area (ha)", 
    caption = "Figure 2: Total fire size by month in 2021"
  ) + 
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5)
  )

barGraph
png("Output_BarGraph.png")
barGraph
dev.off()
```
![Output_Histogram](https://github.com/user-attachments/assets/bbd20d26-e350-49e4-8d75-411aac615637) ![Output_BarGraph](https://github.com/user-attachments/assets/764fc372-88a0-4d16-b9f3-fe41abdb5317)



### Data
To conduct this report historical fire point data was collected from the BC Data Catalogue database, retrieved from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical. The fire data includes relevant information such as fire number, fire year, ignition date, latitude, longitude and fire size (ha). Temperature data was collected from the Weather Station Data Portal from the Pacific Climate Impacts Consortium (PCIC), retreived from https://services.pacificclimate.org/met-data-portal-pcds/app/. The temperature data was parameterized to be collected from May 1st, 2021 to September 1st, 2021 from the WMB network and includes wind direction, relative humidity, average wind speed, precipitation, temperature and time.

Following the collection of data, we must clean it so it is relevant and useable for our desired outcomes. To begin this process we must create an empty CSV file dataframe with columns Native.ID, TEMP, Longitude, Latitude and save it as BC_AVG_TEMP.csv for future appending.
```{r Cleaning Data echo=TRUE, eval=TRUE, warning=FALSE}
# Create an empty data frame with specified columns
empty_data <- data.frame(Native.ID = character(), TEMP = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)

csv_file_name <- "BC_AVG_TEMP.csv"
write.csv(empty_data, file = csv_file_name, row.names = FALSE)
```
We now process the temperature data from files saved to our working directory.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
csv_files <- list.files(path = "./WMB_temp", pattern = "\\.csv$", full.names = TRUE)
```
Each CSV file is then looped through, reading the file into the datafram hourly_data.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
for (file in csv_files) {
    hourly_data <- read.csv(file, skip = 1, header = TRUE)
```
Time is then converted into a desired datetime format. 
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
hourly_data$temperature <- as.numeric(hourly_data$temperature)
hourly_data <- hourly_data %>%
  filter(!is.na(temperature))
```
The daily average temperature is then calculated and converted into monthly average temperature.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
daily_avg_temp <- hourly_data %>%
  group_by(date = as.Date(time)) %>%
  summarize(daily_avg_temp = mean(temperature, na.rm = TRUE))

monthly_avg_temp <- hourly_data %>%
  group_by(year = year(time), month = month(time), day = day(time)) %>%
  summarize(monthly_avg_temp = mean(temperature, na.rm = TRUE)) %>%
  ungroup()
```
Finally, we filter the data for the months May to September and calculate the average temperature of these months.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
average_temp_may_september <- hourly_data %>%
  filter(month(time) >= 5 & month(time) <= 9) %>%
  summarize(TEMP = mean(temperature, na.rm = TRUE))
```
The file name is then extracted with its extension. The extension is then removed and a new row is created and attatched to the existing CSV file with the file name and calculated temperature.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
# Extract the filename (with extension)
file_name <- basename(file_name)
# Remove the file extension
file_name_no_ext <- sub("\\.[^.]*$", "", file_name)
# Read the existing CSV file
file_path <- csv_file_name
data <- read.csv(file_path)

# Round the temperature values to two decimals
Roundedtemp <- round(average_temp_may_september,2)

# Convert the weather station ID column to character
data$Native.ID <- as.character(data$Native.ID)

# Step 3: Append new rows
new_values <- data.frame(Native.ID = file_name_no_ext, 
                         TEMP = Roundedtemp, 
                         stringsAsFactors = FALSE)

data <- bind_rows(data, new_values)

# Save the updated data frame back to a new CSV file
output_file_path <- csv_file_name
write.csv(data, file = output_file_path, row.names = FALSE)
}
```
We then merge station metadata and the aggregated temperature data using the common row Native.ID. After this the last two columns which are duplicate Latitude and Longitude are removed. 
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
metadata <- read.csv("./station_metadata.csv")
climatedata <- read.csv("./BC_AVG_TEMP.csv")
merged_data <- merge(metadata, climatedata, by = "Native.ID")
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")
```
Now omit NA and erroneous temperature values.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
merged_data <- na.omit(merged_data)
merged_data <- merged_data[merged_data$TEMP <= 100, ]
```
Finally save the cleaned, merged dataset to your working directory as ClimateData.csv.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
write.csv(merged_data, file = "ClimateData.csv", row.names = FALSE)
```


## References

Woke, G. (2023, March 24). The difference between libraries and frameworks. Simple Talk. https://www.red-gate.com/simple-talk/development/other-development/the-difference-between-libraries-and-frameworks/

Xu, Z. (2014). Predicting wildfires and measuring their impacts: Case studies in British Columbia (Doctoral dissertation). University of Victoria.
