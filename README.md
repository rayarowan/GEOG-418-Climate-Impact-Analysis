# GEOG 418 Final Assignment - Climate Impact Analysis

By Raya Rowan

November, 2024

## Introduction

Each summer BC experiences raging wildfire activity across the province, destroying everything in its path such as wildlife habitats and residential communities. Damage such as altered drainage systems, unstabilized exposed soils, and destroyed infrastructure posed to these communities is detrimental and can last for years before full recovery is achieved (BC Wildfire Service, 2023). Furthermore, due to wildfires resulting in ground instability, post-fire hazards such as soil erosion, floods, landslides, and avalanches have become an increasing concern (BC Wildfire Service, 2023). It is hypothesized that the rising temperatures experienced from May to September can be correlated with increased wildfire frequency and size in BC, suggesting wildfires may be a direct consequence of climate (Xu, 2014). Using this theory, the byproducts of high temperatures such as drier soils and reduced fuel moisture can be linked to the location of wildfires in BC, providing a deeper understanding of what may have contributed to the dramatic rise in fire activity during the summer (Xu, 2014).

This tutorial aims to utilize RStudio and its tools to analyze the correlation between temperature, location, and wildfire size in British Columbia during the summer of 2021. Descriptive statistics, point pattern analysis, spatial autocorrelation, spatial interpolation, and geographically weighted regression techniques will be used to conduct the following research. Building on the results of this study, we can aim to address further questions, such as identifying potential patterns in wildfire behavior as temperatures continue to rise annually due to anthropogenic climate change.

Results from the above statistical techniques will be displayed as tables, maps, and graphs using RStudio libraries throughout this tutorial.

The first step is to install the desired packages. When a package is installed using the function intsall.packages("package") it is stored in a directory called the library (https://www.datacamp.com/doc/r/packages, 2024). These libraries are collections of pre-written code used to complete specific tasks while still maintaining control over R's flow (Woke, 2023). They are beneficial as they act like a shortcut, reducing the amount of code needed to be written, and can be called upon in the script when needed (Woke, 2023). To install a library we use the code library("package"). After installation of the library R will contain documentation specifying proper syntax and use for that package. This information can be found under Packages in the panel on the bottom right side of the page. The below packages are what will be used for this study's statistical analyses.

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Install packages if not already installed:
install.packages("knitr")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("e1071")
install.packages("spgwr")
install.packages("sf")
install.packages("gstat")
install.packages(""ggplot2")
install.packages("dplyr")
install.packages("viridis")
install.packages("grid")

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
library(grid)
```
The next step is to set the working directory. This will be the folder or location on your computer that R pulls files from and saves your work. To set the working directory use the code dir <- "name of folder/location" followed by setwd(dir). An example of how this is done can be viewed below. I am pulling my data stored in a folder labeled Assignment 4 within a folder labeled GEOG 418 located on my desktop.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
dir <- "~/Desktop/GEOG 418/Assignment 4"
setwd(dir)
```

## Study Area
British Columbia, Canada was selected to determine temperature impacts on wildfires from May to September 2021 because of its dense forested area and ongoing history of wildfire occurrence.
.  

To display the study area we will create a map of BC which includes all the fire points as well as the mean center. The first step will be to load in the boundary shapefile and set the coordinate reference system (CRS) of the BC boundary to EPSG:4326, which corresponds to the WGS84 geographic coordinate system (latitude and longitude).

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
firePoints <- SpatialPointsDataFrame(coords = coords, data = df, proj4string = crs) #make new spatial points object using coordinates, data, and projection
```
The mean center point is then calculated and a spatial points object (meanCenterPoint) is created to represent the mean center of the wildfire points.
```{r Mean Center Point, echo=TRUE, eval=TRUE, warning=FALSE}
#calculate and add mean to map center 
meanCenter <- data.frame(name = "Mean Center of fire points", long = mean(df$LONGITUDE), lat = mean(df$LATITUDE))
coords2 <- meanCenter[, c("long", "lat")]
crs2 <- CRS("+init=epsg:4326")
meanCenterPoint <- SpatialPointsDataFrame(coords = coords2, data = meanCenter, proj4string = crs2)
```
Using the tmap package installed earlier, we can now create and print the map as a PNG with a caption (TmMap.png). This map output will be found in your working directory under the designated name. For example, in my case, it will be a png file named TmMap.
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

The below code displays how we read the fire point .csv from the current working directory into a data frame called data. Since the data frame contains dates ranging across 2021, we want to filter the data from May 1st, 2021 to September 1st, 2021, and store the filtered data in a new object called fire_point. To do this we must extract the IGN_DATE from the dataframe and take the first 8 characters of the IGN_DATE column. We then convert the extracted substring to the date type using the format %Y%m%d (year, month, day).

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
We are now ready to calculate our descriptive statistics (mean, mode, standard deviation, median, skewness, kurtosis, Coefficient of Variation, and normal distribution). The descriptive statistics will be based on wildfire sizes (ha). 

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

The descriptive statistics suggest that the mean wildfire size is approximately 588 ha. However, due to a high standard deviation (5172.356) and extremely right-skewed distribution (13.886), the mean is not an accurate representation of the average wildfire size. Instead, it was likely that during the 2021 summer, most fires were relatively small (0.009 ha) with a few very large fires representing the outlier data points and skewing the data.

To analyze the size frequency of fires as well as the frequency of fires per month in BC during the summer of 2021 we can create a histogram and bar graph using the following code. 
```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
# Histogram to display the size frequency of fires
png("Output_Histogram.png")
hist(log10(df$SIZE_HA), breaks = 30,
     main = "Summer 2021: Frequency of Wild Fire Sizes",
     xlab = "Log(Size of Wild Fire, ha)")
mtext("Figure 2. Frequency of wildfire sizes (ha) during the Summer of 2021 using a log scale.", 
      side = 1, line = 5, outer = FALSE, adj = 0)
dev.off()

# Bar Graph to display frequency of fires per month
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
    caption = "Figure 3: Total fire size by month in 2021"
  ) + 
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5), 
    plot.caption = element_text(hjust = 0.5)
  )

png("Output_BarGraph.png")
barGraph
dev.off()
```
![Output_Histogram](https://github.com/user-attachments/assets/b01b4443-aca1-4eff-b7f1-3c1d04fa807a) ![Output_BarGraph](https://github.com/user-attachments/assets/f58e62c6-3356-4643-8d80-202b407ea7a1)

Based on the histogram results it is apparent that there were wildfires of varying sizes, with larger fires having occurred less frequently than smaller fires and the average fire size being 0.009 ha (representative of the calculated mode). The bar graph indicates that during summer 2021 the month of July experienced the highest frequency of fires, followed by June, August then May. 

### Data
To conduct this report historical fire point data was collected from the BC Data Catalogue database, retrieved from https://catalogue.data.gov.bc.ca/dataset/bc-wildfire-fire-perimeters-historical. The fire data includes relevant information such as fire number, fire year, ignition date, latitude, longitude, and fire size (ha). Temperature data was collected from the Weather Station Data Portal from the Pacific Climate Impacts Consortium (PCIC), retrieved from https://services.pacificclimate.org/met-data-portal-pcds/app/. The temperature data was parameterized to be collected from May 1st, 2021 to September 1st, 2021 from the WMB network and includes wind direction, relative humidity, average wind speed, precipitation, temperature,e and time.

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
Time is then converted into a desired datetime format. In this case, we want hourly temperature data from May to September 2021.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
hourly_data$temperature <- as.numeric(hourly_data$temperature)
hourly_data <- hourly_data %>%
  filter(!is.na(temperature))
```
The daily average temperature is then calculated and converted into the monthly average temperature.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
daily_avg_temp <- hourly_data %>%
  group_by(date = as.Date(time)) %>%
  summarize(daily_avg_temp = mean(temperature, na.rm = TRUE))

monthly_avg_temp <- hourly_data %>%
  group_by(year = year(time), month = month(time), day = day(time)) %>%
  summarize(monthly_avg_temp = mean(temperature, na.rm = TRUE)) %>%
  ungroup()
```
Finally, we filter the data for the months of May to September and calculate the average temperature of these months.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
average_temp_may_september <- hourly_data %>%
  filter(month(time) >= 5 & month(time) <= 9) %>%
  summarize(TEMP = mean(temperature, na.rm = TRUE))
```
The file name is then extracted with its extension. The extension is then removed and a new row is created and attached to the existing CSV file with the file name and calculated temperature.
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
We then merge station metadata and the aggregated temperature data using the common column, Native.ID. After this, the last two columns which are duplicate Latitude and Longitude are removed. 
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
# Read in metadata
metadata <- read.csv("./station_metadata.csv")

# Read in climatedata
climatedata <- read.csv("./BC_AVG_TEMP.csv")

# Merge meta and climate data using Native.ID column
merged_data <- merge(metadata, climatedata, by = "Native.ID")
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

# Remove duplicate columns
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")
```
Omit NA and erroneous temperature values.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
merged_data <- na.omit(merged_data)
merged_data <- merged_data[merged_data$TEMP <= 100, ]
```
Finally save the cleaned, merged dataset to your working directory as ClimateData.csv.
```{r Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
write.csv(merged_data, file = "ClimateData.csv", row.names = FALSE)
```
To ensure the data has been correctly cleaned we will now map our newly created climate data and visualize the temperature points across BC. To begin this process read in the 'ClimateData' CSV file.
```{r Mapping Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
climate_data <- read.csv("ClimateData.csv")
```
Next we must format the latitude and longitude by using the function mutate() from the dplyr package. This is to ensure that the Latitude and Longitude columns are numeric for spatial processing.
```{r Mapping Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
climate_data <- climate_data %>%
   mutate(Latitude = as.numeric(Latitude),
        Longitude = as.numeric(Longitude))
```
Following this step we will convert the data to a simple feature object, set the CRS to 4326, and save the new simple feature as a shapefile. 
```{r Mapping Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)
st_write(climate_sf, "ClimateData.shp")
```
Load in the shapefiles that will be used to create our map.
```{r Mapping Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
# Load in climate shapefile
climate_sf <- st_read("ClimateData.shp")

# Load in BC boundary shapefile
bc_boundary <- st_read("BC_bound.shp")
bc_boundary <- st_set_crs(bc_boundary, 4326)
```
Create the map and save it as a png to the working directory.
```{r Mapping Cleaning Data, echo=TRUE, eval=TRUE, warning=FALSE}
# Create the map
map <- ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) + 
  scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient as needed
  theme_minimal() +
  labs(
    title = "Map of Temperature Data Points in British Columbia",
    subtitle = "Overlayed on BC Boundary",
    x = "Longitude",  # Use Longitude for x-axis
    y = "Latitude",   # Use Latitude for y-axis
    color = "Temperature (°C)", # Label for color legend
    caption = "Figure 4: Spatial distribution of temperature data points across British Columbia."
  ) +
  theme(legend.position = "bottom")

# Save the map as a PNG file
ggsave("Temperature_Map_BC.png", plot = map, width = 10, height = 8, dpi = 300)
```
![Temperature_Map_BC](https://github.com/user-attachments/assets/25feec0e-8916-41f5-8707-cf32ddd22922)


## Methods
### Evaluating Spatial Distribution of Wildfires
To evaluate the spatial distribution of wildfires across BC using fire point data from from May 1st to September 1st, 2021, a density map of wildfires over this period will be created followed by a point pattern analysis of the wildfire data. 

#### Density Map
The density map will be a raster dataset representation of points per unity area across the province. The desired outcome will show us the general spread of wildfire points as well as where they are concentrated in BC.

To begin, using the pre-loaded fire dataset (data) convert the fire points to a spatial object. Then transform the points to CRS projection 3005 so that they align and plot on our BC boundary map.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
# Convert to sf object
fire_point <- st_as_sf(fire_point, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
fire_point <- st_transform(fire_point, crs = 3005) 
```
Using the pre-loaded BC boundary shapefile (bc_bound) extract the bounding box of BC and create a raster template. Note that the raster resolution can be changed to any desired resolution.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(bc_bound)
# Creating raster templace
raster_res <- 50000  # This resolution in 50000 meters 
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))
```
Calculate the density of fire points using kernel density estimation, fill missing values (NA) in the raster with zeros and convert fire points into a raster grid.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
density_raster <- raster::rasterize(st_as_sf(fire_point), raster_template, fun = "count", field = 1)
density_raster[is.na(density_raster)] <- 0
```
Now convert the raster (density_raster) into a data frame (density_df) and replace NA values in the data frame with zeros. The raster's column for fire values is renamed from layer to fires. After this, future spatial analysis requires us to convert the density data frame back to an sf object.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0

# Step to rename the 'layer' column to 'fires'
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(bc_bound))
```
Export the density points as a shapefile.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
st_write(density_sf, "density_points.shp", delete_dsn = TRUE)
```
We are now ready to plot our rasterized density points on a simple map using the following code. Again, the map will save to your working directory as a png file.
```{r Density Map, echo=TRUE, eval=TRUE, warning=FALSE}
# Create a simple map
# Plotting the density map with the polygon boundary
fire_density_map <- ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = fires)) +  # Use 'fires' from the data frame
  geom_sf(data = bc_bound, fill = NA, color = "white") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(
    title = "Density Map of Fire Points",
    x = "Longitude",
    y = "Latitude",
    fill = "Density",
    caption = "Figure 5: The spatial distribution of fire density across BC during the summer of 2021."
  )

# Save the plot as a PNG file
ggsave("density_of_fires_map.png", plot = fire_density_map, width = 10, height = 7, dpi = 300)
```

#### Point Pattern Analysis
##### Is the Relative Size and Frequency of Wildfires Location Dependant Across BC in Summer 2021?
To answer this question, this tutorial will guide you through three statistical tests—nearest neighbor analysis, quadrat analysis, and K-function—to evaluate whether the wildfire size data exhibits random, dispersed, or clustered spatial patterns. Additionally, to provide a comprehensive summary of point pattern analysis, we will perform a kernel density estimation based on the results of these statistical tests.

#### Nearest Neighbour Analysis
Nearest Neighbour Analysis is a method used to assess whether points in a spatial distribution exhibit a random, dispersed, or clustered pattern (Le Corvec et al., 2013). This technique works by calculating and comparing the average distance between each point and its nearest neighbor (Le Corvec et al., 2013). In this tutorial, this distance is utilized to evaluate the similarity or dissimilarity between data points. To determine whether our fire pattern is clustered or dispersed, we compared the observed distances to the average nearest neighbour distance expected from a random pattern with the same spatial density.

To conduct a nearest neighbour analysis for wildfire size (ha) across BC in the 2021 summer, we want to acquire the  average nearest neighbour value for a spatially random distribution ($$\bar{NND_R} = \frac{1}{2\sqrt{Density}}$$), the average nearest neighbour value for a perfectly dispersed pattern ($$\bar{NND_D} = \frac{1.07453}{\sqrt{Density}}$$), and a z-score ($$Z_n = \frac{\bar{NND} - \bar{NND_R}}{\sigma\bar{NND}}$$). The density is calculated by dividing the number of points by the area of BC. 

To calculate these results the following code can be used.
```{r Nearest Neighbour, echo=TRUE, eval=TRUE, warning=FALSE}
# Conduct Nearest Neighbour Analysis.
nearestNeighbour <- nndist(df$SIZE_HA)

# Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

# Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

# Calculate the nearest neighbor statistic to test for a random spatial distribution.
# First calculate the mean nearest neighbour
nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)

library(units)

nnd <- set_units(nnd, "m")  # Assign units of meters to nnd

# Next, calculate the mean nearest neighbour for random spatial distribution
studyArea <- st_area(bc_boundary)
pointDensity <- nrow(nearestNeighbour) / studyArea
r.nnd = 1 / (2 * sqrt(pointDensity))
d.nnd = 1.07453 / sqrt(pointDensity)
R = nnd / r.nnd

# Calculate the standard deviation
SE.NND = .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)

# Calculate the Z score
z = (nnd - r.nnd) / SE.NND
```
We can then display our results in a table.
```{r Nearest Neighbout, echo=TRUE, eval=TRUE, warning=FALSE}
# Create the table
table3 <- tableGrob(nndResults)

# Add the caption as a textGrob
caption <- textGrob("Table 3: NND results for the size of fires in summer 2021.", 
                  gp = gpar(fontsize = 14, fontface = "bold"), 
                    x = 0, hjust = 0)

# Combine the caption and the table
nnd_table <- arrangeGrob(caption, table3, ncol = 1, heights = c(0.2, 1))

# Export the table with the caption as a PNG file
png("Output_Table3.png", width = 1000, height = 600, res = 150)  # Adjust size and resolution
grid.draw(nnd_table)
dev.off()
```

#### Quadrat Analysis
Quadrat analysis can be defined as a mathematical and statistical technique to measure properties of point patterns (Thomas, 1977). This method divides our BC study area into quadrats and analyzes the density of wildfire points in each quadrat (Thomas, 1977). It does this to test if our spatial patterns are significantly different from random.

For the quadrat analysis performed in this study, we are using a study area of approximately 947848424073 square meters divided into 144 quadrats (12x12). By using 144 quadrats each quadrat has an area of approximately 1 square kilometere. The following code converts the fire points to a planar point pattern object, a main class for point pattern analysis in spatstat. It then divides the study area into a grid of quadrats and counts the number of fire points in each quadrat. The density of points is stored in a data frame (qcount.df).
```{r Quadrat Analysis, echo=TRUE, eval=TRUE, warning=FALSE}
# Convert fire_point to an sf object using LATITUDE and LONGITUDE
fire_point_sf <- st_as_sf(fire_point, coords = c("LONGITUDE", "LATITUDE"), crs = 3005)
# Extract the bounding box of the spatial data (fire_ext) to define the study area's extent
fire_ext <- as.matrix(st_bbox(fire_point_sf))

# Now create the observation window. 
#Spatstat needs this for calculating the area of your study site, which is needed 
#for the various statistics like NND
window <- as.owin(list(xrange = c(fire_ext[1], fire_ext[3]), 
                       yrange = c(fire_ext[2], fire_ext[4])))

# Finally, create a ppp object for fire.
fire.ppp <- ppp(x = st_coordinates(fire_point_sf)[,1], 
                y = st_coordinates(fire_point_sf)[,2], 
                window = window)

# Determine number of quadrats
# Study area = 947.8 km^2 and number of points = 1472
quads <- 12

qcount <- quadratcount(fire.ppp, nx = quads, ny = quads)

# Count the number of quadrats with a distinct number of points, this will be used in your Quadra Analysis formula below
qcount.df <- plyr::count(qcount.df,'Freq')

# Change the column names so that x=number of points and f=frequency of quadrats with x point
colnames(qcount.df) <- c("x","f")
```
We are now ready to calculate the variance ($$VAR = \frac{\Sigma f_ix_i^2 - [\frac{(\Sigma f_ix_i)^2}{m}]}{m-1}$$), variance to mean ($$VMR = \frac{\text{VAR}}{\text{MEAN}}$$), and chisquare quadrat statistics.
```{r Quadrat Analysis, echo=TRUE, eval=TRUE, warning=FALSE}
#Caluclate the Quadrat Analysis statistics
sum.f.x2 <- sum(qcount.df$f * (qcount.df$x^2))
M <- sum(qcount.df$f)
N <- sum(qcount.df$x * qcount.df$f)
sum.fx.2 <- (sum(qcount.df$x * qcount.df$f)) ^ 2
VAR <- ((sum.f.x2) - (sum.fx.2 / M)) / (M - 1)
MEAN <- N/M
VMR <- VAR/MEAN

# Finally, perform the test statistic to test for the existence of a random spatial pattern
chi.square = VMR * (M - 1)
p = 1 - pchisq(chi.square, (M - 1))

quadResults <- data.frame(Quadrats = quads * quads, 
                          Variance = round(VAR, 2), 
                          Mean = round(MEAN, 2), 
                          VMR = round(VMR, 2), 
                          Chisquare = round(chi.square, 2))

# Create the table
table4 <- tableGrob(quadResults)

# Add the caption as a textGrob
caption <- textGrob("Table 4: Quadrat analysis results for the frequency of fires in summer 2021.", 
                    gp = gpar(fontsize = 14, fontface = "bold"), 
                    x = 0, hjust = 0)

# Combine the caption and the table
quad_table <- arrangeGrob(caption, table4, ncol = 1, heights = c(0.2, 1))

# Export the table with the caption as a PNG file
png("Output_Table4.png", width = 1000, height = 600, res = 150)  # Adjust size and resolution
grid.draw(quad_table)
dev.off()
```

#### K-Function
The K function is a unique spatial statistic that can be used to assess the dependence between locations at varying distances (Moraga, 2024). The formula for the K-function is as follows:

$$K_{d} = \lambda^{-1}E(N_d)$$

Where λ is the intensity function of the spatial point process. We can see that the K-function also requires the input of $$N_d$$, the number of points within a distance (d) of a randomly selected point. Putting the equation together we can determine if there is either more clustering or dispersion than expected compared to complete spatial randomness (Moraga, 2024). When the k-function plots above $$K_{CSR}$$ the distribution is clustered while plotting below means it is dispersed.

To calculate complete spatial randomness the following equation is used:

$$K_{CSR}(d) = \pi d^2$$

The code used to obtain the K-function for wildfire points is below. 
```{r K Function, echo=TRUE, eval=TRUE, warning=FALSE}
#Create a basic k-function
k.fun <- Kest(fire.ppp, correction = "Ripley")

# Use simulation to test the point pattern against CSR
k.fun.e <- envelope(fire.ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.fun.e, main = "Basic K-Function of Fires")
```

#### Where are the Fire Hotspots Located?
#### Kernel Density Estimation
The kernel density estimation (KDE) is used to estimate the probability density function of a random variable (Węglarczyk et al., 2018). The KDE uses a kernel (mathematical function) to produce a smooth estimate that uses all locations of data points (Węglarczyk et al., 2018). The estimation allows us to identify clustering or dispersion features of the data more accurately than via histogram (Węglarczyk et al., 2018). The formula used to obtain the KDE is as follows: 

$$\hat\lambda_p = \frac{no. [S \in C(\boldsymbol{p, r})]}{\pi r^2}$$

Each location (p) has a circle (C) with a radius (r) drawn around it. Next the number (no.) of points (S) within the circle are divided by the area of the circle. The radius of the circle is known as sigma. A smaller sigma (and smaller bandwidth) shows much more detail and allows for easier identification of hotspots than a larger sigma. While larger sigma values make hotspots less obvious, they produce a smoother image. This option may come in handy if working with noisy data or a small sample size. The outcome of this estimate is a grid of cells containing values that estimate the number of points that fall within the circle around it. 

The code to obtain the kernel density estimation is below.
```{r Kernel Density, echo=TRUE, eval=TRUE, warning=FALSE}
# Kernel Density using the cross-validation bandwidth
kde.fire <- density.ppp(fire.ppp, sigma = bw.diggle(fire.ppp))
plot(kde.fire, main = "Kernel Density Estimation of Fires")
```

### Creating Temperature Surface
To generate a temperature surface across British Columbia for the period from May 1 to September 1, 2021, two interpolation methods, Inverse Distance Weighting (IDW) and kriging, will be employed to create a continuous representation of the temperature variable.
#### Inverse Distance Weighting
IDW is a surface interpolation technique in which the value at an unsampled location is estimated as a weighted average of values from nearby sampled points (Lu & Wong, 2008). The relationship is inverse as with more distance between the unknown and known point, the less influence the known point has when estimating the unknown value (Lu & Wong, 2008). A power or distance-decay parameter is typically applied, adjusting how rapidly the influence diminishes with distance (Lu & Wong, 2008). 

The following formula is used to calculate IDW:

$$Z_i = \frac{\sum_{j=1}^{n}\frac{z_j}{d_{ij}^p}}{\sum_{j=1}^{n}\frac{1}{d_{ij}^p}}$$

In R, we can create an IDW temperature surface and clip that to the boundary of BC. This allows us to create a map of BC that displays our interpolated IDW temperature surface. 
To begin, make sure the climate data shapefile is read in. For this interpolation, I am using a CRS of 3005.
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
# Read the shapefile
climate_data <- st_read("ClimateData.shp")
climate_data <- st_set_crs(climate_data, 4326)
climate_data <- st_transform(climate_data, crs = 3005)
```
The first step in our interpolation is to create a grid. I chose to use a resolution 50km, you can change this value to obtain the desired outcome. 
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
# Create a grid for the interpolation
bbox <- st_bbox(climate_data)
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(50000, 50000), crs = st_crs(climate_data))
```
The interpolation can now be performed. To better visualize and process the result we can convert it into a sf object.
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
# Interpolate using IDW
idw_result <- gstat::idw(TEMP ~ 1, 
                         locations = climate_data, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

# Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)
```
Now we will clip the interpolated data to the BC boundary. Load in the BC boundary as we did above in this tutorial and transform the CRS of the IDW results to match the CRS of the boundary. This ensures compatibility for clipping.
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
# Load the polygon shapefile for clipping
bc_boundary <- st_read("BC_bound.shp")
bc_boundary <- st_set_crs(bc_boundary, 4326)

# Step to transform the CRS of either shapefile if they do not match
if (crs_idw != crs_polygon) {
  # Transform the IDW result to match the CRS of the polygon
  idw_sf <- st_transform(idw_sf, crs = crs_polygon)  # Transform IDW result to polygon's CRS
  message("Transformed IDW result CRS to match the polygon.")
} else {
  message("CRS of IDW result and polygon already match.")
}
```
Now clip and intersect the data.
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
idw_clipped <- st_intersection(idw_sf, bc_boundary)
```
The last step is to make a map of the clipped results and save our output.
```{r IDW, echo=TRUE, eval=TRUE, warning=FALSE}
# Create the map of the clipped results
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis_c(option = "D") +  # Use viridis color scale for better readability
  labs(
    title = "Clipped IDW Interpolation of Temperature",
    fill = "Temperature (°C)",  # Legend label
    x = "Longitude", 
    y = "Latitude",
    caption = "Figure 8: IDW interpolation clipped to BC boundary showing predicted temperature distribution."
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Save the map as an image file (optional)
ggsave("Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)
```

#### Ordinary Kriging
Ordinary kriging is an interpolation method in which the predictor is an optimal linear predictor, and the result is an exact interpolation (Dumas et al., 2013). This simply means that each point being predicted using kriging will be the best linear unbiased estimate and that predictions at sampled points are the same as observed values with minimal variance (Dumas et al., 2013). Kriging involves the use of a semivariogram to assign weights to known data points to make predictions at unsampled locations (O’sullivan & Unwin, 2010). The main assumption of kriging is a constant mean across the domain (O’sullivan & Unwin, 2010). In this tutorial, it means that the average temperature is constant across the entire domain of BC. 

The purpose of performing kriging in this tutorial is to interpolate an accurate spatial representation of temperature across BC and account for areas where there are no temperature measurements. To perform kriging we will be working with our climate shapefile that was created earlier in this tutorial. Load in this file and use the function f.0 to define a formula for the kriging model with temperature as our variable of interest. 
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
# Read the shapefile
climate_data <- st_read("ClimateData.shp")
# Define formula for kriging
f.0 <- as.formula(TEMP ~ 1) # The 1 indicates no covariates (ordinary kriging)
```
Now create the semivariogram based on spatial distances and differences in temperature then fit the model by adjusting nugget, psill and range until the best fit is achieved. My model uses the following values below for each of these parameters.
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
var.smpl <- variogram(f.0, climate_data, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(model="Sph", nugget = 5, psill = 10, 
                              range = 7000)) 
plot(var.smpl, dat.fit)

# Save the variogram plot to a PNG file
png("Variogram.png", width = 800, height = 600, res = 100)

# Plot the variogram with fitted model
plot(var.smpl, dat.fit, main = "Semivariogram of Temperature Data with Fitted Spherical Model")

# Add a figure caption below the plot
mtext("Figure 9: Semivariogram of temperature data for British Columbia.",
      side = 1, line = 5, outer = FALSE, adj = 0)

# Close the PNG device
dev.off()
```
![Variogram](https://github.com/user-attachments/assets/f397677a-6584-4903-a079-606adcc0b484)

Define the prediction grid. For this grid I am chosing to have n = 5000.
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
# Define the grid
xmin <- st_bbox(climate_data)$xmin
xmax <- st_bbox(climate_data)$xmax
ymin <- st_bbox(climate_data)$ymin
ymax <- st_bbox(climate_data)$ymax

# Create a regular grid
n <- 50000  # Number of points
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(climate_data))
```
Now we will perform kriging using RStudio kriging function (krige) and convert the results to a raster format for spatial analysis.
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
# Perform kriging
dat.krg <- krige(f.0, climate_data, grd, dat.fit, debug.level=0)

# Convert the kriging output to an sf object
kriging_results_sf <- st_as_sf(dat.krg)

# Create a Raster from Kriging Results
# Convert to a data frame with coordinates for raster creation
coords_df <- as.data.frame(st_coordinates(kriging_results_sf))
coords_df$predicted_temp <- kriging_results_sf$var1.pred  # Replace with your prediction column

# Create the raster from the resulting data frame
predicted_raster <- rasterFromXYZ(coords_df)
```
We will now match the CRS projections and clip the newly created raster to the BC boundary.
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
bc_boundary <- st_read("BC_bound.shp")
bc_boundary <- st_set_crs(bc_boundary, 4326)

crs(predicted_raster) <- crs(bc_boundary)

# Clipping raster and bc boundary
clipped_raster <-mask(predicted_raster,bc_boundary)
```
We can now create a map to visualize how temperature varies across BC.
```{r Kriging, echo=TRUE, eval=TRUE, warning=FALSE}
# Visualize the raster
tmap_save(
  tm_shape(clipped_raster) +
    tm_raster(palette = viridis(10), title = "Predicted Temperature (clipped)") + 
    tm_shape(bc_boundary) +
    tm_borders(col = "black", lwd = 1) +
    tm_layout(
      title = "Clipped Kriging Results for Temperature",  
      title.size = 1.2,
      title.position = c("center", "top"),
      legend.position = c("left", "bottom"),  
      legend.width = 2.5  # Adjust the width of the legend to make labels larger
    ) +
    tm_compass(position = c("right", "top")) +
    tm_scale_bar(position = c("left", "bottom")),
  filename = "Raster.png",
  width = 800,
  height = 600,
  dpi = 300
)
```

### Combining Temperature and Wildfire Surfaces
Before we can perform further statistics to analyze the correlation between temperature and wildfire location in British Columbia during the summer of 2021, we must combine the temperature and fire data by adding the density values from the fire dataset to the polygons in the interpolated temperature surface.

To acquire these results we must first align the CRS of the IDW clipped and density results.
```{r Combining data, echo=TRUE, eval=TRUE, warning=FALSE}
# If they are different, transform one to match the other
if (st_crs(idw_clipped) != st_crs(density_sf)) {
  # Transform density_sf to match the CRS of idw_clipped
  density_sf <- st_transform(density_sf, st_crs(idw_clipped))
}
```
Then we will perform the spatial join.
```{r Combining data, echo=TRUE, eval=TRUE, warning=FALSE}
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)
```
Select the relevant columns and rename for clarity.
```{r Combining data, echo=TRUE, eval=TRUE, warning=FALSE}
# Select needed columns
final_data <- joined_data[, c("var1.pred", "fires")]

# Rename column
final_data <- final_data %>%
  rename(temperature = var1.pred)
```
Next, replace NA values in the fires column with 0 so that areas with no data for fires represent zero fire events.
```{r Combining data, echo=TRUE, eval=TRUE, warning=FALSE}
# Replace NA values in the fires column with 0
final_data <- final_data %>%
  mutate(fires = ifelse(is.na(fires), 0, fires))
```
Now we can plot the data for visualization and save the final combined data as a shapefile for future Ordinary Least Squares and Global Weighted Regression models. 
```{r Combining data, echo=TRUE, eval=TRUE, warning=FALSE}
# Create the map
temp_map <- ggplot(data = final_data) +
  geom_sf(aes(fill = temperature)) +  # Use the correct column name, e.g., 'var1.pred' or 'temperature'
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(
    title = "Predicted Temperature Map",
    fill = "Temperature (°C)",
    caption = "Figure 11: Predicted Temperature Map Across British Columbia. Colors represent temperature variations (°C)."
  ) +
  theme(
    legend.position = "right",
    plot.caption = element_text(hjust = 0)  # Left-aligned caption without italic
  )

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)

# Save the map as a PNG
ggsave("Temperature_Combo_Map.png", plot = temp_map, width = 10, height = 8, dpi = 300)
```

### Determining if Temperature Explains Wildfire Spatial Variability
This section of the tutorial's methodology will employ Ordinary Least Squares (OLS), Global Moran's I (based on OLS results), and Geographically Weighted Regression (GWR) models. These statistical analyses will assess, on a global scale across British Columbia, the extent to which the temperature variable explains the variability in fire density during the summer months of 2021.

#### Ordinary Least Squares
Ordinary Least Squares (OLS) regression determines whether temperature explains wildfire spatial variability by modeling the relationship between the independent variable (temperature) and the dependent variable (fire) across BC. It does so by minimizing the squared differences between observed fire occurrence and the values predicted based on temperature, fitting a linear model to the data (Majka, 2024). The model evaluates how much of the variation in wildfire occurrence is explained by temperature using the proportion of variance accounted for by the independent variable. The higher the R squared value the more influence temperature has on fire spatial variability (Majka, 2024). The slope of the model determines if there is a positive, negative, or negligible relationship between the variables. If the slope is positive it means that  temperature strongly influences the spatial distribution of wildfires (Majka, 2024). A negative slope indicates an inverse relationship, where an increase in temperature would result in less fire occurrence (Majka, 2024). A slope of approximately zero suggests no relationship between the variables (Majka, 2024).

This tutorial will now demonstrate how to calculate the residuals using OLS regression. After which the residuals will be mapped for visualization.

First, we will read in the final data shapefile created during the combination of temperature and fire data.
```{r OLS, echo=TRUE, eval=TRUE, warning=FALSE}
final_data_sf <- st_read("final_data.shp")
```
We then fit a linear regression model to examine the relationship between temperature and fire location, remove rows with NA values in the fires column and add residuals to the original spatial data frame.
```{r OLS, echo=TRUE, eval=TRUE, warning=FALSE}
ols_model <- lm(fires ~ temprtr, data = final_data_sf)

# Handling missing data
final_data_sf <- final_data_sf[complete.cases(final_data_sf$fires), ]

# Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)
```
Now we will create a map of residuals from the OLS regression.
```{r OLS, echo=TRUE, eval=TRUE, warning=FALSE}
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Save the plot if desired
ggsave("residuals_map.png", width = 10, height = 8, dpi = 300)
```

#### Global Moran's I
The next step in this tutorial is to calculate the Global Moran’s I statistic using the residuals obtained from OLS. This is a measure of overall spatial autocorrelation across a given area (Getis and Ord, 1992). It assesses the degree of similarity between neighbouring spatial units based on a given variable (Getis and Ord, 1992). In this tutorial the Global Moran’s I will be determining if the residuals (differences between the observed and predicted values of fires) from the OLS regression across BC are spatially autocorrelated. It is a global scale measurement because we are considering the entirety of BC in assessing spatial autocorrelation. To determine Moran's I, we must first choose a spatial weighting matrix. In this tutorial, we will be using Queen's weight. A positive Moran’s I value tells us that there is positive spatial autocorrelation which translates into a clustered distribution (Getis and Ord,1992). A negative Moran’s I value indicates negative spatial autocorrelation and a dispersed distribution, and a Moran’s I value of exactly 0 means the data is randomly distributed (Getis and Ord, 1992).
 
We can mathematically calculate the Global Moran’s I statistic using the following equation:

$$I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}$$

Where x is the variable being determined and (i) is our point of interest, xi is the value of the variable at (i). xj is a neighbour to xi. (xi - x) displays how similar the point is to the mean and (xj - x) displays how similar the neighbour is to the mean. Wi,j is the spatial weighting matrix (  both the differences are multiplied by. To find the Global Moran’s I we sum the differences between the points and neighbours across the study area to measure covariance in the numerator. The denominator is used to standardize our values.

The following code is how Moran's I can be determined for the OLS regression residuals using the Queen's weighting scheme. 

The first step is to prepare the residual data by removing any NA values.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Using residuals from OLS regression to see if they are spatially autocorrelated
# Remove NA
residual_noNA <- final_data_sf[!is.na(final_data_sf$residuals), ]
```
Next we will define the queen's weight neighbours.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Neighborhood Matrix - Queen's Weights
residual.nb <- poly2nb(residual_noNA)
```
Following this and using the nb2lines command, spatial network lines are created and ensure the same coordinate reference system is being used for residual.net as residual_noNA.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
residual.net <- nb2lines(residual.nb, coords=st_geometry(st_centroid(residual_noNA)))
st_crs(residual.net) <- st_crs(residual_noNA)
```           
Generate the queen's weight map. 
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Make queen's map
queens_map <- tm_shape(residual_noNA) +
  tm_borders(col = 'lightgrey') +
  tm_shape(residual.net) +
  tm_lines(col = 'blue', lwd = 1) +
  tm_layout(title = "Queens") +
  tm_credits("Figure 13: Queen's weight scheme across BC to display residual data points", position = c("LEFT", "BOTTOM"))

# Save the map to a PNG file
tmap_save(queens_map, filename = "queens_map.png", width = 800, height = 600, dpi = 300)
```
![queens_map](https://github.com/user-attachments/assets/b85837c9-5b1f-45b2-86f9-f5f93802cbfe)

Now create a queen's weight matrix using the nb2listw command which converts the structure of the queen’s neighborhood object into a spatial weights matrix.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Create residual weights matrix
residual.lw <- nb2listw(residual.nb, zero.policy = TRUE, style = "W")
```
We are now ready to calculate the Global Moran's I and extract the Moran's I result.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Calculating Global Morans I
residual_mi <- moran.test(residual_noNA$`residuals`, residual.lw, zero.policy = TRUE)

# Extract Global Moran's I results for residuals
mI_residual <- residual_mi$estimate[[1]]
eI_residual <- residual_mi$estimate[[2]]
var_residual <- residual_mi$estimate[[3]]
# Calculate z-test for residuals
zresidual <- (mI_residual - eI_residual) / (sqrt(var_residual))
# Calculate the p-value
pvalue_residual <- residual_mi$p.value
```
Create a summary table of the results.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Create a data frame
residual_data <- data.frame(
  Metric = c("Moran's I", "Expected Value", "Variance", "Z-Score", "P-Value"),
  Value = c(
    round(mI_residual, 5),
    round(eI_residual, 5),
    round(var_residual, 5),
    round(zresidual, 5),
    round(pvalue_residual, 5)
  )
)
```
Now make a Global Moran's I scatter plot to view the spatial autocorrelation.
```{r Moran's I, echo=TRUE, eval=TRUE, warning=FALSE}
# Start a PNG device
png("moran_scatter_plot_fixed.png", width = 1800, height = 1500, res = 300)

# Generate Moran's I scatter plot
moran.plot(residual_noNA$residuals, residual.lw, zero.policy = TRUE, 
           xlab = "Observed Residuals", 
           ylab = "Spatially Lagged Residuals")

# Add a title to the plot
title(main = "Moran's I Scatter Plot", cex.main = 1.5, font.main = 2)

# Add a caption
mtext("Figure 14: Moran's I Scatter Plot of Residuals", side = 1, line = 4, cex = 0.8, font = 2)

# Close the graphics device
dev.off()
```

#### Geographically Weighted Regression
Since the Global Moran's I indicated positive spatial autocorrelation, the main OLS assumption stating that residuals are independent is violated and a geographically weighted regression (GWR) must be conducted. This analysis is more complex than the OLS regression as it allows for the regression coefficients to vary spatially, meaning the relationship between variables can change depending on the location (Páez & Wheeler, 2009). It does this by fitting regression models at each location instead of using a single global regression model and therefore is not as influenced by spatial autocorrelation (Páez & Wheeler, 2009). 

Using GWR we will be able to identify where across BC temperature has a significant influence on fire occurrence  compared to where it has negligible effects.

The following steps are done to prepare the data for GWR.
```{r GWR, echo=TRUE, eval=TRUE, warning=FALSE}
# Read the shapefile (with residuals included)
final_data_sf <- st_read("final_data.shp")

# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

final_data_sp@data[["fires"]][is.na(final_data_sp@data[["fires"]])] <- mean(final_data_sp@data[["fires"]], na.rm = TRUE)

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$fires
independent_vars <- final_data_sp@data$temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}
```
Now we can run our GWR model.
```{r GWR, echo=TRUE, eval=TRUE, warning=FALSE}
# Run GWR with a fixed bandwidth of 200 km
fixed_bandwidth <- 200000  # Bandwidth in meters (200 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sp, 
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE)

# Validate that the model ran successfully
if (is.null(gwr_model_fixed)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_fixed$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_fixed))
```
Extract the GWR results and centroids of the spatial features.
```{r GWR, echo=TRUE, eval=TRUE, warning=FALSE}
# Extract coefficients and create a dataframe for visualization
gwr_results_fixed <- as.data.frame(gwr_model_fixed$SDF)

# Extract coordinates from the original spatial data
coordinates_fixed <- st_coordinates(final_data_sf)  # Get coordinates from the original data

# Get centroids of each feature in final_data_sf
coordinates_fixed <- st_coordinates(st_centroid(final_data_sf))
```
Now combine the centroids with the GWR results to enable mapping then convert the combined data back into an sf object for compatibility with ggplot2.
```{r GWR, echo=TRUE, eval=TRUE, warning=FALSE}
# Combine the GWR results with the coordinates
gwr_results_fixed <- cbind(gwr_results_fixed, coordinates_fixed)

# Convert to an sf object for visualization
gwr_output_sf_fixed <- st_as_sf(gwr_results_fixed, coords = c("X", "Y"), crs = st_crs(final_data_sf))
```
For visualization of the GWR results create a plot displaying the R-squares values across BC. In the code below note that 'localR2' is specifically selected for. 
```{r GWR, echo=TRUE, eval=TRUE, warning=FALSE}
# Plotting GWR coefficients with the fixed bandwidth
ggplot(data = gwr_output_sf_fixed) +
  geom_sf(aes(color = localR2)) +
  scale_color_viridis_c(option = "C") +
  labs(
    title = "GWR R2 Coefficients",
    fill = "GWR R2",
      caption = "Figure 15: Visualizing local regression coefficients (R squared) from GWR analysis."
  ) +
  theme_minimal()

# Optional: Save the plot
ggsave("gwr_coefficients_fixed_bandwidth.png", width = 15, height = 8, dpi = 300)
```

## Results

![density_of_fires_map](https://github.com/user-attachments/assets/f63b2d04-0c38-41fa-ad75-a33cd682c248)

Based on the density map we can see that the location of fires during the summer of 2021 were most compact in south-central BC, as indicated by the yellow point on the map. This is followed by the orange points, which plot in a similar region.

#### Is the Relative Size and Frequency of Wildfires Location Dependant Across BC in Summer 2021?

This section of the report will outline the results of the three statistical tests (nearest neighbour analysis, quadrat analysis and k-function). 

![Output_Table3](https://github.com/user-attachments/assets/f4d82e8c-59f1-436a-a6d8-c1e3f77259f9)

Beginning with the nearest neighbour analysis, we can see in Table 3 that on average fires are approximately 27.3 km apart, however, if they were to be randomly distributed, we would expect them to be 12.7 km apart. The outputted NND test value shows a clustered pattern when compared to the values for NNDd and NNDr. This is because the NND value of 57.2 is smaller than that for NNDr. According to the nearest neighbour method, the z-value of -73.07 confirms that there is strong evidence for fires of similar size (ha) in BC during the summer of 2021 being extremely clustered.

![Output_Table4](https://github.com/user-attachments/assets/de68401a-3a92-4492-aaee-57ac481ad108)

When the study area (BC) was divided into 144 quadrats, the results obtained from quadrat analysis in Table 4 indicate that there was a high variance of fires across the quadrats. The VMR being greater than 1 (38.33) suggests strong clustering and the chi-square value of 5481.28 confirms that this clustering is statistically significant.

![Basic K-function](https://github.com/user-attachments/assets/bb82b9c6-958d-4a8f-8067-4cf8e802b151)

The K-function further suggests spatial clustering of fires in the summer of 2021 across BC. In Figure 6 we can see that the observed K-function ($$K(d)$$) is represented by the black line plots above the theoretical K-function ($$K_{CSR}(d)$$) represented by the red dotted line. This indicates that fire locations are more clustered than would be expected under random distribution.

#### Where are the Fire Hotspots Located?
![Kernal Density](https://github.com/user-attachments/assets/db3954a7-88c1-4a2c-9f7b-693e9f70a9a5)

This kernel density estimation in Figure 7 confirms the clustering patterns indicated by the K-function. The yellow/red areas display the hotspots in BC where fires were concentrated/clustered. These results align well with the density map.

![Clipped_IDW_Interpolation_Map](https://github.com/user-attachments/assets/01a078fc-cd47-47a8-9479-8294bf175eed)

The clipped IDW results displayed in Figure 8 show that the highest temperatures (temperatures above 16°C) occur around the south-central region of BC. 

![Raster](https://github.com/user-attachments/assets/3717a904-a7dd-49da-9257-756952ca95ed)

Figure 10 displays the interpolated temperature surface using the kriging technique to capture gradual spatial trends in temperature across BC. The map indicates that the predicted temperature ranges from 13°C to 16.5°C and that cooler temperatures dominate higher altitude regions in northern BC while warmer temperatures are predicted in the lower altitude and southern regions.

![Temperature_Combo_Map](https://github.com/user-attachments/assets/24fb9640-ef15-4d92-9aff-425a37881f68)

Figure 11 overlays the interpolated temperature surface (acquired using clipped IDW results) with fire density results, allowing us to identify potential relationships between temperature and fire density. Yellow to orange regions likely correspond to areas with higher fire densities while purple regions likely represent areas with lower fire density. Again, we can see southern regions displaying warmer temperatures and northern regions colder temperatures.

![residuals_map](https://github.com/user-attachments/assets/7458aaf2-bea9-43f3-be6d-d1584e1caef0)

The mapped result of this tutorials OLS regression can be seen in Figure 12. This map displays the difference between observed fire density and predicted values based on temperature as 'residuals'. The purple/blue regions are where the model overpredicted fire density and the yellow/orange regions are where the model underpredicted fire density and indicate where the interpolated temperature surface did not accurately predict fire density. Areas with low residuals represent regions in BC where temperature is a good predictor of fire density and areas with high residuals indicate where temperature is not as accurate in predicting fire density.

![moran_scatter_plot_fixed](https://github.com/user-attachments/assets/41b2d9e9-43e6-4027-9d9f-e71d2f9daa53)

A moans scatter plot (Figure 14) was created to determine if there was any spatial autocorrelation in the residuals. The upward slope of the trendline in this scatter plot indicates positive spatial autocorrelation, meaning that the yellow/orange areas in Figure 12 where there are high residuals are surrounded by neighbours that also have high residuals. The same explanation applies to blue/purple areas with low residuals as they will be surrounded by neighbours that also contain low residuals. This result contradicts the main assumption of the OLS regression model and suggests the residuals are not independent. For this reason, to better address spatial dependence a geographically weighted regression was run.

<img width="539" alt="GWR_R2" src="https://github.com/user-attachments/assets/1e864864-e8bc-4e69-9570-4788c79d1b2d">

Figure 15 displays the GWR results. As displayed in the code, this model used a fixed bandwidth of 200km. This means that in each local regression data from a 200km radius around a centroid data point was incorporated. The yellow/orange points are areas with high R-squared values indicating that the residuals plotted farther from the linear regression line. This means that the model was not a good predictor at these locations. The blue/purple points are areas with lower R-squared values. The closer to zero these values are the better the residuals plot along the linear regression line at these localized areas meaning the temperature was a good predictor of fire occurrence. 

## Discussion
The primary goal of this climate impact analysis was to assess whether rising temperatures were correlated with increased wildfire frequency and size. The study successfully determined that in the southern central regions of British Columbia during the period from May to September 2021, there was a statistically significant correlation between temperature and wildfire occurrence. This was evident from the clustering of fires in areas with elevated temperatures. In contrast, the northern regions of BC showed a weaker relationship between temperature and wildfire occurrence. This is ultimately supported by the GWR results illustrated in Figure 15, which reveal significantly higher R-squared values further north. Notably, the month of July saw the largest area burned by wildfires, followed by June, aligning with BC's peak yearly temperatures. During this period, the majority of fires were relatively small, averaging around 0.09 hectares in size.  Organizations such as BC Wildfire Service could utilize the results from this tutorial to determine future spatial trends in wildfires based on temperature highs and mitigate the potential damage, protecting wildlife habitats and residential communities. The consideration of anthropogenic climate change resulting in global warming may also provide useful information for predicting future fire patterns. 

A recent study by Dastour et al. (2024) concluded similar findings, demonstrating a strong correlation between fire occurrence and air temperature. Their research also incorporated additional temperature-related variables, such as thunder and lightning. Analyzing data from May 2015 to July 2017, they identified two peaks in fire occurrence, both ignited by lightning activity, during periods of significantly high air temperatures. The study concluded that elevated temperatures contributed to increased evaporation and drying of fuels, creating favorable conditions for wildfires (Dastour et al. 2024). Additionally, higher temperatures enhanced convective activity and thunderstorm development, which served as primary sources of natural ignitions in boreal ecosystems (Dastour et al. 2024). A study conducted by Xing et al. (2023) examined historical wildfire trends and identified significant warming trends over recent decades that coincide with the frequency and severity of wildfires. Their findings revealed a positive correlation between the number of wildfire incidents and temperature anomalies, with pronounced fire activity observed during heatwaves. Xing et al. (2023) attributed this relationship to elevated temperatures creating drier environments, which foster optimal conditions for wildfire ignition and spread. The results of Xing et al.'s (2023) study reinforce the findings of this tutorials analysis, supporting its validity by demonstrating similar patterns across diverse temporal and spatial scales. These findings highlight the consistent global relationship between temperature increases and wildfire activity and suggest that fires are not a direct consequence of temperature but rather a byproduct of temperature creating environments that favor their presence. 

This study faced several significant limitations, particularly concerning data availability and quality, as well as the Geographically Weighted Regression (GWR) model's assumption of linear relationships. Both temperature and fire datasets included numerous areas with missing observations, creating gaps in the analyses that led to errors such as over or under-predictions in the interpolated surface models. Since these models were integral to the GWR analysis, the resulting outputs also contained inaccuracies. Additionally, the BC fire datasets typically exhibit bias, as larger, more destructive fires are often prioritized in reporting, leading to an underrepresentation of smaller incidents.
The GWR model's assumption of a linear relationship further posed challenges, as it may oversimplify the complex interactions between temperature and fire variables, rendering it insufficient for robust modeling. To address this, incorporating additional climate variables such as wind, precipitation, or humidity could provide a more comprehensive understanding of fire patterns in northern BC. Moreover, the GWR results highlighted the tutorial's inefficiencies in accurately correlating temperature and fire in northern regions. Future analyses should leverage more complete and reliable data to better clarify the relationship between temperature and fire activity across BC's diverse landscapes. 

## Conclusion
In conclusion, while this study successfully highlighted the impact of rising temperatures on wildfire patterns in BC, it also emphasized the need for more robust models and higher quality data to refine these insights. Incorporating additional temperature-dependent variables such as soil moisture or thunderstorms and determining how they correlate with wildfires across BC could pose as relevant future research and allow for better prediction of fire patterns. Nonetheless, these findings contribute to a growing body of research that highlights the importance of addressing climate change leading to increasing global temperatures to mitigate the risks associated with wildfires globally. 

## References

Dastour, H., Ahmed, M. R., & Hassan, Q. K. (2024). Analysis of forest fire patterns and their relationship with climate variables in Alberta’s natural subregions. Ecological Informatics, 80, 102531. https://doi.org/10.1016/j.ecoinf.2024.102531

Dumas, A., Echard, B., Gayton, N., Rochat, O., Dantan, J.-Y., & Van Der Veen, S. (2013). AK-ILS: An Active learning method based on Kriging for the Inspection of Large Surfaces. Precision Engineering, 37(1), 1–9. https://doi.org/10.1016/j.precisioneng.2012.07.007

Getis, A., & Ord, J. K. (1992). The Analysis of Spatial Association by Use of Distance Statistics. Geo-graphical Analysis, 24(3), 189–206. https://doi.org/10.1111/j.1538-4632.1992.tb00261.x

Le Corvec, N., Spörli, K. B., Rowland, J., & Lindsay, J. (2013). Spatial distribution and alignments of volcanic centers: Clues to the formation of monogenetic volcanic fields. Earth-Science Reviews, 124, 96–114. https://doi.org/10.1016/j.earscirev.2013.05.005

Lu, G. Y., & Wong, D. W. (2008). An adaptive inverse-distance weighting spatial interpolation technique. Computers & Geosciences, 34(9), 1044–1055. https://doi.org/10.1016/j.cageo.2007.07.010

Majka, M. (2024). Ordinary Least Squares. ResearchGate.

Moraga, P. (2024). Chapter 22 The K-function | Spatial Statistics for Data Science: Theory and Practice with R. Paulamoraga.com. https://www.paulamoraga.com/book-spatial/the-k-function.html

O’sullivan, D., & D Unwin. (2010). Geographic information analysis. John Wiley & Sons.

Páez, A., & Wheeler, D. C. (2009). Geographically weighted regression. In Encyclopedia of GIS. Elsevier. https://doi.org/10.1016/B978-008044910-400447-8

Thomas, R. W. (1977). An Introduction to Quadrat Analysis.

Węglarczyk, S. (2018). Kernel density estimation and its application. ITM Web of Conferences, 23, 00037. https://doi.org/10.1051/itmconf/20182300037

What is wildfire land-based recovery? – BC Wildfire Service. (2023, January 24). https://blog.gov.bc.ca/bcwildfire/what-is-wildfire-land-based-recovery/

Woke, G. (2023, March 24). The difference between libraries and frameworks. Simple Talk. https://www.red-gate.com/simple-talk/development/other-development/the-difference-between-libraries-and-frameworks/

Xu, Z. (2014). Predicting wildfires and measuring their impacts: Case studies in British Columbia (Doctoral dissertation). University of Victoria.

Xing, H., Fang, K., Yao, Q., Zhou, F., Ou, T., Liu, J., Zhou, S., Jiang, S., Chen, Y., Bai, M., & Jing Ming Chen. (2023). Impacts of changes in climate extremes on wildfire occurrences in China. Ecological Indicators, 157, 111288–111288. https://doi.org/10.1016/j.ecolind.2023.111288
