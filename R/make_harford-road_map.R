library(dplyr)
library(purrr)
library(ggplot2)
library(here)
# pak::pkg_install("elipousson/mapbaltimore")
# pak::pkg_install("elipousson/mapmaryland")
# pak::pkg_install("elipousson/sfext")
# pak::pkg_install("elipousson/maplayer")
# pak::pkg_install("elipousson/esri2sf")
# pak::pkg_install("elipousson/getdata")

# Load street data ----

# Get data from Councilman Dorsey's Google Map
sample_route <-
  sfext::read_sf_gmap(
    url = "https://www.google.com/maps/d/u/0/viewer?ll=39.354697704756845%2C-76.55921359999999&z=14&mid=18o0ZyDKVz86egaIh7TFkUMsm591VDWM"
  )

# Get a street named Harford Road but not Old Harford Road within the bounding box
street <-
  mapbaltimore::get_streets(
    street_name = "Harford Road",
    exclude_name = "Old Harford Road",
    bbox = sample_route
  ) %>%
  # mapbaltimore using the 2804 crs by default
  sf::st_transform(3857)

# Set buffer distances
buffer_dist <- c(100, 250, 500)

# Bufffer street centerline
street_buffered <-
  purrr::map_dfr(
    buffer_dist,
    ~ sfext::st_buffer_ext(
      street,
      dist = .x,
      unit = "ft"
    )
  ) %>%
  # Trim buffered polygons to the city boundary and remove parks
  sfext::st_trim(mapbaltimore::baltimore_city) %>%
  mapbaltimore:::erase_parks()

# Add buffer_dist as a column
street_buffered$buffer_dist <- buffer_dist

# Load parcel data ----

# NOTE: I commented out the function to download the data since I already cached
# the data but it would be good to run once to see how it works.

# Get parcels from Maryland iMap
# parcels <-
#   mapmaryland::get_parcel_data(
#     # Use the street with the largest buffer to define the location
#     location = street_buffered[3, ]
#   )
#
# Save parcels to GeoJSON file
# sfext::write_sf_ext(
#   data = parcels,
#   label = "Harford Road corridor",
#   name = "Parcels",
#   postfix = "md_imap",
#   filetype = "geojson"
# )

# Read file from disk
parcels <- sfext::read_sf_ext(
  sfext::make_filename(
    label = "Harford Road corridor",
    name = "Parcels",
    postfix = "md_imap",
    filetype = "geojson",
    path = here("data")
  )
)

# Transform back in 3857
parcels <- sf::st_transform(parcels, 3857)

# Combine parcels and buffered street data ----

# Create a sf object with boundary for parcels intersecting each of the possible
# buffers
parcel_boundary <-
  street_buffered %>%
  group_by(buffer_dist) %>%
  group_map(
    ~ sfext::st_union_ext(
      sfext::st_filter_ext(
        parcels,
        .x
      ),
      name_col = NULL
    ),
    .keep = TRUE
  ) %>%
  bind_rows() %>%
  mutate(buffer_dist = buffer_dist)


# Create a sf object with boundary around combined parcels intersecting each of the possible
# buffers
area_boundary <-
  parcel_boundary %>%
  group_by(buffer_dist) %>%
  group_map(
    ~ sfext::st_concave_hull(.x),
    .keep = TRUE
  ) %>%
  bind_rows()


# Create boundary maps using parcel data ----

boundary_maps <-
  area_boundary %>%
  group_by(buffer_dist) %>%
  group_map(
    ~ ggplot2::ggplot() +
      maplayer::layer_mapbox(
        data = bind_rows(street, .x),
        dist = 250,
        style_url = "mapbox://styles/mapbox/light-v10",
        asp = 8.5 / 11,
        scale = 1,
        scaling_factor = "2x",
        neatline = FALSE
      ) +
      maplayer::layer_location(
        data = .x,
        linewidth = 0.65,
        linetype = "solid",
        smooth_params = FALSE
      )
  )

titled_boundary_maps <- map2(
  boundary_maps,
  buffer_dist,
  ~ .x +
    maplayer::labs_ext(
      title = "Boundary for parcels within {.y} feet of Harford Road"
    ) +
    maplayer::layer_neatline(
      data = street,
      dist = 100,
      asp = 8.5 / 11,
      expand = FALSE
    ) +
    theme(
      title = element_text(family = "Roboto Condensed")
    ) +
   maplayer::stamp_inset_img(
     path = here("files", "baltimore-dop_logo_538x541.png"),
     img_margin = ggplot2::margin(0, 8, 8, 0)
   )
)

# map_ggsave_ext
maplayer::map_ggsave_ext(
  plot = titled_boundary_maps,
  filename = "harford_road_corridor_property_maps",
  path = "output",
  filetype = "pdf",
  paper = "letter",
  single_file = FALSE
)

