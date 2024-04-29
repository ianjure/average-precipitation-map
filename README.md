<img src="Philippines Average Precipitation Map.png" width="128"/>

## About the Project

Precipitation refers to the amount of water released from clouds in the
form of rain, freezing rain, sleet, snow, or hail. It is the main way
atmospheric water returns to the surface of the Earth. Since the
Philippines is a tropical country that experiences an average of 20
typhoons every year, we want to visualize how each province is affected
by these precipitations.

## About the Data

Multi-Source Weighted-Ensemble Precipitation (MSWEP) is a global
precipitation dataset with a 3‑hourly 0.1° resolution available from
1979 to \~3 hours from real-time. The dataset is unique in that it
merges gauge, satellite, and reanalysis data to obtain the highest
quality precipitation estimates at every location.

# How to Create a 3D Average Precipitation Map in R

#### **1. Install and Load Required Packages**:

You need to install the necessary packages. Run the code chunk to
install them, it might require restarting the R-session several times.
Then load the required libraries.

```{r message=FALSE}
if(
    !require("pacman")
){
    install.packages("pacman")
}

pacman::p_load(
    pRecipe,
    giscoR,
    terra,
    tidyverse,
    ggpubr,
    rayshader,
    sf,
    classInt,
    magick
)
```

This section checks if the Pacman package is installed, and if not,
installs it. Then it loads several packages using the *p_load()*
function.

#### **2. Load and Rasterize the Data**:

You'll need to select a country of choice, then download and rasterize
the precipitation data.

```{r message=FALSE}
country_sf <- giscoR::gisco_get_countries(
    country = "PH", #Change Country Code
    resolution = "1"
)

pRecipe::download_data(
    dataset = "mswep",
    path = getwd(),
    domain = "raw",
    timestep = "yearly"
)

mswep_data <- terra::rast(
    "mswep_tp_mm_global_197902_202301_025_yearly.nc"
) |>
terra::crop(
    country_sf
)

terra::plot(mswep_data[[1]])
plot(sf::st_geometry(country_sf), add = TRUE)
```

This section retrieves the spatial data for the Philippines (the country
code is based on ISO 3166-2) at a resolution of 1 using the Giscor
package. Then it downloads precipitation data from the MSWEP dataset,
using the *download_data()* function, to the current working directory.
Then it will rasterize the data, crop it to the extent of the country,
and plot the data. The country codes can be found at:
<https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes>

#### **3. Format the Data**:

To visualize the precipitation over time you need to format the data
into a tidy form suitable for plotting.

```{r}
names(mswep_data) <- 1979:2023

mswep_df <- mswep_data |>
    as.data.frame(xy = TRUE) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "year",
        values_to = "precipitation"
    ) |>
    dplyr::filter(year != 2023)

head(mswep_df)
```

This section uses the *names()* function to set the names of the objects
to years, starting from 1979 to 2023. Then it transforms the rasterized
data into a data frame for proper plotting and filters the year 2023
since its data is still incomplete.

#### **4. Calculate Average Precipitation:**

Since the goal is to visualize the average precipitation, you need to
first calculate the mean precipitation value of every coordinates from
1979 to 2022.

```{r message=FALSE}
mswep_average_df <- mswep_df |>
    dplyr::group_by(
        x, y, .drop = FALSE
    ) |>
    dplyr::summarise(
        mean = mean(precipitation)
    )

head(mswep_average_df)
```

This section calculates the average precipitation for each location,
then shows these average values using the *head()* function.

#### **5. Create Theme and Define Color Palette**:

You'll need to define a custom theme for ggplot, calculate the legend
breaks, and determine the color palette for the project based on the
precipitation data.

```{r}
theme_precipitation <- function(){
    theme_minimal() +
    theme(
        axis.line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "left",
        legend.title = element_text(
            size = 18, color = "#005E5E"
        ),
        legend.text = element_text(
            size = 16, color = "#005E5E"
        ),
        panel.grid.major = element_line(
            color = NA
        ),
        panel.grid.minor = element_line(
            color = NA
        ),
        plot.background = element_rect(
            fill = NA, color = NA
        ),
        legend.background = element_rect(
            fill = "white", color = NA
        ),
        panel.border = element_rect(
            fill = NA, color = NA
        ),
        plot.margin = unit(
            c(
                t = 0, r = 0,
                b = 0, l = 0
            ), "lines"
        )
    )
}

breaks <- classInt::classIntervals(
    mswep_average_df$mean,
    n = 5,
    style = "equal"
)$brks

colors <- hcl.colors(
    n = length(breaks),
    palette = "Batlow", #Choose Color Palette
    rev = TRUE
)
```

This section creates a function that customizes a ggplot theme. It also
defines the breaks and colors that will be used for the legend. The
palettes can be found at:
<https://colorspace.r-forge.r-project.org/articles/hcl_palettes.html>

#### **6. Check and Plot the Data**:

Visualize the calculated average precipitation data.

```{r}
map <- ggplot(
    data = mswep_average_df
) +
geom_raster(
    aes(
        x = x,
        y = y,
        fill = mean
    )
) +
geom_contour(
    aes(
       x = x,
       y = y,
       z = mean 
    ), color = "white"
) +
geom_sf(
    data = country_sf,
    fill = "transparent",
    color = "#323232",
    linewidth = 0.5
) +
scale_fill_gradientn(
    name = "mm",
    colors = colors,
    breaks = breaks, 
    labels = round(breaks, 0), # use round(breaks, 0)
    limits = c(
        min(mswep_average_df$mean),
        max(mswep_average_df$mean)
    )
) +
theme_precipitation() + theme(legend.position = "none")

map #Show Plot
```

This section uses ggplot's functionality to show the average
precipitation data.

#### **7. Save Legend as Image**:

Export the plot's legend as an image using ggpubr.

```{r eval=FALSE}
lgnd <- ggplot(
    data = mswep_average_df
) +
geom_raster(
    aes(
        x = x,
        y = y,
        fill = mean
    )
) +
geom_contour(
    aes(
       x = x,
       y = y,
       z = mean 
    ), color = "white"
) +
geom_sf(
    data = country_sf,
    fill = "transparent",
    color = "#323232",
    linewidth = 0.5
) +
scale_fill_gradientn(
    name = "mm",
    colors = colors,
    breaks = breaks, 
    labels = round(breaks, 0),
    limits = c(
        min(mswep_average_df$mean),
        max(mswep_average_df$mean)
    )
) +
guides(
    fill = guide_colourbar(
        direction = "vertical",
        barheight = unit(80, "mm"),
        barwidth = unit(8, "mm"),
        title.position = "bottom",
        label.position = "right",
        title.hjust = 0,
        label.hjust = .5,
        title.vjust = 0,
        ncol = 1,
        byrow = FALSE
    )
) +
theme_precipitation()

legend <- get_legend(lgnd)
as_ggplot(legend)
ggsave("Legend.png", path=getwd(),width=1900, height=1900, limitsize = FALSE, units="px", bg="white")
```

This section will extract the legend using the *get_legend()* function
from ggpubr, then it will plot and save the legend as an image using
ggplot's functions.

#### **8. Render 3D Map**:

Use Rayshader to create a 3D representation of the average
precipitation.

```{r message=FALSE, eval=FALSE}
rayshader::plot_gg(
    ggobj = map,
    width = 7,
    height = 7,
    scale = 250,
    solid = FALSE,
    shadow = TRUE,
    shadowcolor = "white",
    shadowwidth = 0,
    shadow_intensity = 1,
    windowsize = c(1049,1569),
)
```

```{r message=FALSE, eval=FALSE}
rayshader::render_camera(
    phi = 55,
    theta = 15,
    zoom = .58
)
```

This section converts the 2D map created in step 6 into a 3D contour map
using the rayshader package.

#### **9. Render in High-Quality and Save Image**:

Download an HDRI file and render a high-quality image of the 3D map.

```{r eval=FALSE}
hdri_url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"

hdri_file <- basename(hdri_url)

download.file(
    url = hdri_url,
    destfile = hdri_file,
    mode = "wb"
)

rayshader::render_highquality(
    filename = "PH_3D_Render.png",
    preview = TRUE,
    interactive = FALSE,
    parallel = TRUE,
    light = TRUE,
    environment_light = "air_museum_playground_4k.hdr",
    rotate_env = 90,
    width = 3138,
    height = 4437
)
```

This section will download a High Dynamic Range Image (HDRI) file, then
uses rayshader's *render_highquality()* function to generate a
high-quality PNG image of the 3D contour map with added lighting
effects.

#### **10. Annotate the Image:**

You can add names and more details about your generated visualization.

```{r eval=FALSE, tidy=TRUE}
pop_raster <- image_read("PH_3D_Render.png")

pop_raster %>%
  image_annotate("PHILIPPINES",
                 gravity = "northeast",
                 location = "+90+60",
                 kerning = 0,
                 color = "#005E5E",
                 size = 330,
                 weight = 750,
  ) %>%
  image_annotate("AVERAGE PRECIPITATION MAP (1979-2022)",
                 gravity = "northeast",
                 location = "+110+410",
                 color = "#005E5E",
                 size = 85,
                 weight = 550,
                 boxcolor = "white"
  ) %>%
  image_annotate("Surigao del Sur",
                 gravity = "southeast",
                 location = "+120+1000",
                 color = "#005E5E",
                 size = 85,
                 weight = 500,
  ) %>%
  image_annotate("Eastern\nSamar",
                 gravity = "southeast",
                 location = "+120+1900",
                 color = "#005E5E",
                 size = 85,
                 weight = 500,
  ) %>%
  image_annotate("Sulu",
                 gravity = "southwest",
                 location = "+500+400",
                 color = "#005E5E",
                 size = 85,
                 weight = 500,
  ) %>%
  image_annotate("Palawan",
                 gravity = "northwest",
                 location = "+120+2200",
                 color = "#005E5E",
                 size = 85,
                 weight = 500,
  ) %>%
  image_annotate("Visualization by: Ayad, Cariño, Macalisang and Santos\n
                 Data: Multi-Source Weighted-Ensemble Precipitation (MSWEP) 1979-2022",
                 gravity = "southwest",
                 location = "+75+65",
                 color = "#005E5E",
                 size = 68,
  ) %>%
  image_write("PH_3D_Render_Annotated.png", format = "png", quality = 1000)


image <- image_read("PH_3D_Render_Annotated.png")
get_legend <- image_read("Legend.png")
legend <- image_trim(get_legend)
output <- image_composite(image, legend, offset = "+195+151")

get_logo <- image_read("ds_logo.png")
logo_scale <- image_scale(get_logo, "550")
output_final <- image_composite(output, logo_scale, gravity = "southeast", offset = "+100+50")

image_write(output_final, "PH_3D_Average_Precipitation_Map.png", format = "png", quality = 1000)
```

This section will add the annotations, other details and the exported
legend image to the 3D-rendered image using magick. Then it will export
the final output of the project.

## Analysis

As shown by the legend breaks, the average precipitation of the country
gathered from 1979 to 2022 ranges from 1359 to 4279 millimeters.

Using the visualization, we can infer that the province of Surigao del
Sur has the highest average precipitation, varying from 3695 to 4279
millimeters. It is then followed by the Eastern Samar province, with an
average precipitation between 3111 and 3695 millimeters. Furthermore,
Palawan and Sulu are some of the provinces that show low average
precipitation. And among the three major island groups, Mindanao has the
highest average precipitation, where more than 50% of the island's
provinces show medium to high level average.

We can also suggest that these results can be due to a province's
geographical location. Provinces with high average precipitation tend to
be in the eastern part of the country, which is adjacent to the Pacific
Ocean, where most tropical cyclones are more likely to form. It can also
be due to a province's land area, since provinces with low average
precipitation are typically islands with smaller land areas.

## Project Inspiration

This project is inspired by https://github.com/milos-agathon/precipitation-maps
