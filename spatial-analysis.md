Spatial Data Analysis
================
Caitlin Mothes
2023-01-31

In the first lesson this week you were exposed to various databases you
can pull spatial data from and worked through importing, wrangling, and
saving those spatial data types. Today we are going to use those data
sets to perform a range of spatial analyses.

You have briefly used the `sf` and `terra` packages so far in this
course, but today we will be exploring them much more in depth using the
wide range of spatial analysis operations they provide.

You shouldn’t need to install any new packages for today:

``` r
source("setup.R")
```

## Load in spatial data

To carry out today’s lesson you will need to read in the data you saved
to your ‘data/’ folder from the ‘get-spatial’ lesson:

``` r
#load in all your vector data
load("data/spatDat.RData")

#read in the elevation and landcover rasters
landcover <- terra::rast("data/NLCD_CO.tif")

elevation <- terra::rast("data/elevation.tif")
```

## Distance Calculations

We’re going to start off today with some distance calculations. Using
our species occurrence data, say we want to compare each species average
distance to the nearest river.

Throughout today we are going to be mapping our spatial data to quickly
inspect it and get a visual of the data’s extent and characteristics, so
lets set our `tmap` mode to interactive.

``` r
tmap_mode("view")
```

Quick view of all our points, colored by species:

``` r
qtm(occ, symbols.col = "Species")
```

Now, calculating the distance to the nearest river involves point to
line distance calculations, which we can perform with the `sf` package.

Before performing any spatial operations, remember all of our spatial
objects must be in the same CRS.

Using what you learned in week one, check the CRS of the occurrences and
rivers, and perform a spatial transformation if needed (***for the sake
of this lesson, keep data in NAD83***).

``` r
st_crs(rivers)
st_crs(occ)

#complete this operation
occ <- st_transform()
```

### Exercise \#1

Our occurrence data set covers all of Colorado, but rivers are only for
Larimer County. So, we have to first filter our points to Larimer
County. Explore the use of `st_filter()` and use it to filter points
that are found within the Larimer County polygon (which you can
filter/index from your `counties` object). Call the new
object`'occ_larimer` and include a quick plot of the filtered points.

<hr>

Great, now we just have species occurrences within Larimer County.

Now for each point we want to calculate its distance to the nearest
river. The most efficient way is to first find the nearest line feature
for each point. We can do this with the `st_nearest_feature()` function.

This function returns the index values (row number) of the river feature
in the `rivers` spatial data frame that is closest in distance to each
point. Here we are saving these index values in a new column of our
Larimer occurrences that we will use later to calculate distances.

``` r
occ_larimer$nearest_river <- st_nearest_feature(occ_larimer, rivers)
```

Now, for each point we can use the `st_distance()` function to calculate
the distance to the nearest river feature, using the index value in our
new “nearest_river” column. Adding `by_element = TRUE` is necessary to
tell the function to perform the distance calculations by element (row),
which we will fill into a new column “river_dist_m”.

``` r
occ_larimer$river_dist_m <-
  st_distance(occ_larimer, rivers[occ_larimer$nearest_river, ], by_element = TRUE)
```

Notice that the new column is more than just a numeric class, but a
“units” class, specifying that the values are in meters.

``` r
str(occ_larimer)
```

### Exercise \#2

Cool, now you have the distance to the nearest river (in meters) for
each individual species occurrence, but you want the average distance
for each species. Using what you know of the `dplyr` functions,
calculate the species average distance, then re-create the bar plot
below with `ggplot2` to compare the averages (feel free to add more
customization!):

![](images/ex2_barplot.png)

*Hint*: remember that the new distance column is a ‘units’ data type and
may throw an error. You will need to coerce that data type in order to
complete the operation.

<hr>

## Buffers

Alternatively, say you want to know what percentage of species’
occurrences (points) were found within a specified distance of a river
(calculated buffer).

To do this we could add a buffer around our line features and filter the
points that fall within that buffer zone. For this example let’s say we
are interested in the 100 m buffer zone around a river. However, if you
try this you’ll notice this operation takes quite a while.

``` r
river_buffer <- st_buffer(rivers, dist = 100)
```

Instead, a more efficient way would be to make a 100 m buffer around
each point, and see how many of those buffers intersect with a river.

``` r
occ_buffer <- st_buffer(occ_larimer, dist = 100)
```

Still takes a little bit of run time, but much faster than buffering
each line feature. Our `occ_buffer` object is now a spatial polygon data
frame, where each feature is an occurrence buffer with 100 m radius.

## Spatial Intersect

We can conduct spatial intersect operations using the function
`st_intersects()`. This function checks if each individual buffer
intersects with a river, and if so it returns an index value (row
number) for each river feature it intersects. This function returns a
list object for each buffer polygon, that will be empty if there are no
intersections. We will add this as a column to our buffer data set, and
then create a binary yes/no river intersection column based on those
results (is the list empty or not?).

``` r
river_intersections <- st_intersects(occ_buffer, rivers)
```

If we inspect this object, we see it is a list of the same length as our
`occ_buffer` object, where each list element is either empty (no
intersections) or a list of index numbers for the river features that do
intersect that buffer.

### Exercise \#3

Create a new column in `occ_buff` that returns TRUE/FALSE if the buffer
intersects with a river.

*Hint*: make use of the `length()` function..we aren’t interested at
this point in how many river features are within 100m of a species
occurrence, just whether or not there was a river within the buffer or
not.

Second, calculate what percentage of occurrences are within 100 m of a
river for each species using `dplyr` operations. The below code will get
you started, however it doesn’t quite work. Why not? There is one line
of code you need to add to the pipe operations for this to work, what is
it?

``` r
occ_buffer %>% 
  group_by(Species) %>% 
  summarise(total_occ = n(), percent_river = (sum(river_100m == TRUE)/total_occ)*100)
```

<hr>

#### Reflection

This analysis is just for teaching purposes, why would you be cautious
about these results for answering real research questions? Think about
how we filtered everything to a political boundary, what’s wrong with
this method?

## Raster Reclassification

So far we’ve dealt with a bunch of vector data and associated analyses
with the `sf` package. Now lets work through some raster data analysis
using the `terra` package.

First, lets explore the landcover raster by making a quick plot.

``` r
qtm(landcover)
```

This land cover data set includes attributes (land cover classes)
associated with raster values. We can quickly view the frequency of each
land cover type with the `freq()` function.

``` r
freq(landcover)
```

Use `ggplot2` to turn this into a bar chart

<details>
<summary>
Show Answer
</summary>

``` r
freq(landcover) %>% 
  ggplot(aes(reorder(value, count), count)) +
  labs(x = "") +
  geom_col() +
  coord_flip() # switch the axes to better view land cover class names
```

</details>

<br>

Say we want to explore some habitat characteristics of our species of
interest, and we are specifically interested in forest cover. Our first
step is to create a new raster layer from our land cover layer
representing percent forest cover. This will involve multiple
operations, including raster reclassification and focal statistics.
Specifically, say we want to calculate the average percentage of forest
cover and urbanization within a 9x9 pixel moving window (remember since
rasters are made up of pixels, the distances we use are dependent on the
resolution of the raster).

First , reclassify the land cover raster creating a new raster
representing just forest/non-forest pixels.

Since rasters are technically matrices, we can index and change values
using matrix operations. Given this particular raster uses character
names associated with values (thanks to the .aux file!), we can index by
those names.

``` r
#first assign landcover to a new object name so we can manipulate it while keeping the origian
forest <- landcover

#where the raster equals any of the forest categories, set that value to 1
forest[forest %in% c("Deciduous Forest", "Evergreen Forest", "Mixed Forest")] <- 1

#SPELLING IS IMPORTANT

#now set all non forest pixels to NA
forest[forest != 1] <- NA
```

Now plot the new forest layer to get a quick sense if it looks accurate
or not.

``` r
plot(forest)
```

## Focal Statistics

Now we are going to perform focal statistics with the `focal()`
function, which is a spatial operation that calculates new values for
each cell based on a specified moving window. For this example we are
going to calculate within a 9x9km moving window (since our pixel
resolution is 1km). We supply this to the `w =` argument as a matrix,
where the first value is the weight of each pixel, and the second two
are the number of rows and columns. Second we use the “sum” function,
since each forest pixel has a value of 1 we will get the total number of
forest pixels within the moving window, and then later divide the values
by the total number of pixels in the window (81) to get the percentage.
The final raster values will represent for each pixel the surrounding
forest percentage (within \~4.5 km radius).

``` r
forest_pct <- terra::focal(forest, w=matrix(1,9,9), fun = "sum", na.rm = TRUE)


forest_pct <- forest_pct/81


plot(forest_pct)
```

Next, we wanted to know the percent forest cover associated with each
species occurrence. Since we are now working with multiple spatial
objects, we have to first check they are all in the same CRS and if not
transform the data before any spatial operations.

``` r
crs(forest_pct)

st_crs(occ)
```

### Exercise \#4

Looks like the raster layer is in a different CRS. Reproject this so we
can use it with our vector data (which are all in NAD83) using the
`project()` function from `terra`, and write a line of code that checks
whether or not the new object and the `occ` object have the same CRS

<hr>

## Raster Extract

Now we can use the `extract()` function to extract the raster pixel
value at each occurrence.

``` r
terra::extract(forest_pct, occ)
```

### Exercise \#5

Notice that this returns a 2 column data frame, with an ID for each
feature (occurrence) and the extracted raster value in the second
column. How would you write this line of `extract()` code to add a
single column to the occurrence data set of just the forest percentage
value?

*Hint:* Use your knowledge of indexing!

Then calculate the average forest cover for each species. On average,
which species is associated with the highest percentage of forest cover?

Create a boxplot to compare the spread of values across species.

<hr>

That’s one way to use the `extract()` function. We can also extract
raster values within polygons, and supply a function to summarize those
raster values.

### Exercise \#6

Calculate the most common landcover type in each Colorado county,
working through the following steps **filling in the necessary code**:

Project the landcover raster to the CRS of the counties shapefile

``` r
landcover_prj <- terra::project()
```

Add a new column to `counties` that is the most common land cover type,
using the function `modal` within `extract()`.

``` r
counties$common_landcover <- terra::extract()
```

Notice however that this returns the raw raster values, which are not
informative to us without knowing the associated landcover classes.

Since we have the associated auxillary file with this raster, we can
extract metadata to get the value/class pairs with the `cats()`
function.

``` r
cats(landcover)
```

Look at what class this function returns though. Coerce this into a data
frame (there are multiple ways you could do this). Once you have have it
as a data frame, use some `dplyr` operations to select just the value
and land cover class columns, and remove all the empty rows (i.e., those
without a landcover class). This should return a data frame of 17 rows.
Call it `nlcd_classes`.

Then tie `nlcd_classes` to the `counties` data frame with `left_join()`,
which will join two data frames by a common variable. In this case, your
common variable is the raw raster value. Look at the documentation for
`left_join()` and how you use the `by =` argument to complete this step.

Finally, create a map of Colorado counties that is colored by the most
common landcover type (using the class, not the raw value) in each
county. The map can be interactive or static, but must include a legend.
