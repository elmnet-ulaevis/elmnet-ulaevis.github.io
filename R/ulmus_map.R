# ============================================================
# ULMUS LAEVIS – INTERACTIVE DISTRIBUTION MAP
# Corrected version for Quarto / GitHub Pages
# ============================================================

# ------------------------------------------------------------
# 0. REQUIRED PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "sf",
  "terra",
  "raster",
  "leaflet",
  "rnaturalearth",
  "rnaturalearthdata",
  "dplyr",
  "htmlwidgets",
  "tibble",
  "sp"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "The following packages are missing:\n",
    paste(missing_packages, collapse = ", "),
    "\n\nInstall them with:\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))
terra::terraOptions(progress = 0)

# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

base_path <- paste0(
  "Z:/Arbeit/Kooperationspaper & andere Projekte/",
  "ElmNET/ILTER"
)

mapping_path <- file.path(base_path, "Work", "Mapping")

dir_ulmus <- file.path(
  mapping_path,
  "Chorological data for the main European woody species",
  "chorological_maps_dataset",
  "Ulmus laevis",
  "shapefiles"
)

eea_tif <- file.path(
  mapping_path,
  "eea_r_3035_1_km_env-zones_p_2018_v01_r00.tif"
)

# ------------------------------------------------------------
# 2. CHECK PATHS
# ------------------------------------------------------------

if (!dir.exists(base_path)) {
  stop(
    "Base directory not found:\n",
    base_path,
    "\n\nCheck whether drive Z: is connected."
  )
}

if (!dir.exists(dir_ulmus)) {
  stop("Shapefile directory not found:\n", dir_ulmus)
}

if (!file.exists(eea_tif)) {
  stop("EEA raster not found:\n", eea_tif)
}

# ------------------------------------------------------------
# 3. OPTIONS
# ------------------------------------------------------------

# Larger value = faster/smaller map, but coarser raster.
agg_fact <- 8

env_opacity <- 0.85
show_ulmus_outline <- TRUE

map_extent <- c(
  xmin = -25,
  ymin = 34,
  xmax = 45,
  ymax = 72
)

# Slightly larger than the visible extent. This limits horizontal
# world repetition and still allows a little panning.
map_max_bounds <- c(
  xmin = -35,
  ymin = 29,
  xmax = 58,
  ymax = 78
)

# ------------------------------------------------------------
# 4. COLORS
# ------------------------------------------------------------

col_native_range <- "#B22222"
col_isolated <- "#CC79A7"
col_intro <- "#E6D8AD"
col_intro_outline <- "#8A7A45"

col_available <- "#40E0D0"
col_planned <- "darkorange"
col_marker_outline <- "black"

col_country_borders <- "#444444"

# ------------------------------------------------------------
# 5. MEASUREMENT LOCATIONS
# ------------------------------------------------------------

meas <- tibble::tribble(
  ~Latitude,  ~Longitude, ~Country,          ~Data,
  56.618194,  16.512806, "Sweden",          "available",
  47.516366,  27.463937, "Moldova",         "available",
  45.232972,  27.930434, "Romania",         "available",
  44.841872,  28.527873, "Romania",         "available",
  43.772894,  25.850950, "Romania",         "available",
  57.478083,  26.362750, "Latvia",          "available",
  54.077611,  13.473722, "Germany",         "available",
  54.232611,  12.879722, "Germany",         "available",
  54.178417,  13.291611, "Germany",         "available",
  50.432500,  22.284900, "Poland",          "available",
  52.921400,  22.140700, "Poland",          "available",
  49.817500,  15.472900, "Czech Republic",  "planned",
  56.263900,   9.501800, "Denmark",         "planned",
  49.000000,   5.000000, "France",          "planned",
  51.165700,  10.451500, "Germany",         "planned",
  53.700000,   8.000000, "Germany",         "planned",
  46.621056,  10.958472, "Italy",           "planned",
  46.818200,   8.227500, "Switzerland",     "planned",
  58.885000,  25.557000, "Estonia",         "planned",
  48.669000,  19.699000, "Slovakia",        "planned",
  49.839700,  24.029700, "Ukraine",         "planned",
  49.567667,  29.118361, "Ukraine",         "planned",
  60.354000,  24.714778, "Finland",         "planned",
  61.383917,  24.299167, "Finland",         "planned"
)

required_columns <- c("Latitude", "Longitude", "Country", "Data")
missing_columns <- setdiff(required_columns, names(meas))

if (length(missing_columns) > 0) {
  stop("Missing columns in meas:\n", paste(missing_columns, collapse = ", "))
}

invalid_status <- setdiff(unique(meas$Data), c("available", "planned"))

if (length(invalid_status) > 0) {
  stop("Unknown values in meas$Data:\n", paste(invalid_status, collapse = ", "))
}

if (
  any(meas$Latitude < -90 | meas$Latitude > 90) ||
  any(meas$Longitude < -180 | meas$Longitude > 180)
) {
  stop("Invalid latitude or longitude in meas.")
}

# ------------------------------------------------------------
# 6. SVG ICON HELPERS
# ------------------------------------------------------------

svg_to_data_uri <- function(svg) {
  paste0(
    "data:image/svg+xml;utf8,",
    utils::URLencode(svg, reserved = TRUE)
  )
}

cross_icon_svg <- function(
    col = "red",
    size_px = 24,
    stroke_px = 4,
    outline_col = "black",
    outline_px = 9
) {
  mid <- size_px / 2
  
  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" ',
      'width="%d" height="%d" viewBox="0 0 %d %d">',
      '<line x1="%f" y1="0" x2="%f" y2="%d" ',
      'stroke="%s" stroke-width="%d" stroke-linecap="round" />',
      '<line x1="0" y1="%f" x2="%d" y2="%f" ',
      'stroke="%s" stroke-width="%d" stroke-linecap="round" />',
      '<line x1="%f" y1="0" x2="%f" y2="%d" ',
      'stroke="%s" stroke-width="%d" stroke-linecap="round" />',
      '<line x1="0" y1="%f" x2="%d" y2="%f" ',
      'stroke="%s" stroke-width="%d" stroke-linecap="round" />',
      '</svg>'
    ),
    size_px, size_px, size_px, size_px,
    mid, mid, size_px, outline_col, outline_px,
    mid, size_px, mid, outline_col, outline_px,
    mid, mid, size_px, col, stroke_px,
    mid, size_px, mid, col, stroke_px
  )
  
  leaflet::makeIcon(
    iconUrl = svg_to_data_uri(svg),
    iconWidth = size_px,
    iconHeight = size_px,
    iconAnchorX = size_px / 2,
    iconAnchorY = size_px / 2
  )
}

point_icon_svg <- function(
    col = "#40E0D0",
    size_px = 24,
    outline_col = "black",
    outline_px = 5
) {
  mid <- size_px / 2
  radius <- (size_px - outline_px) / 2
  
  svg <- sprintf(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg" ',
      'width="%d" height="%d" viewBox="0 0 %d %d">',
      '<circle cx="%f" cy="%f" r="%f" ',
      'fill="%s" stroke="%s" stroke-width="%d" />',
      '</svg>'
    ),
    size_px, size_px, size_px, size_px,
    mid, mid, radius, col, outline_col, outline_px
  )
  
  leaflet::makeIcon(
    iconUrl = svg_to_data_uri(svg),
    iconWidth = size_px,
    iconHeight = size_px,
    iconAnchorX = size_px / 2,
    iconAnchorY = size_px / 2
  )
}

icon_available <- point_icon_svg(
  col = col_available,
  size_px = 24,
  outline_col = col_marker_outline,
  outline_px = 5
)

icon_planned <- cross_icon_svg(
  col = col_planned,
  size_px = 24,
  stroke_px = 4,
  outline_col = col_marker_outline,
  outline_px = 9
)

# ------------------------------------------------------------
# 7. FIND AND LOAD ULMUS SHAPEFILES
# ------------------------------------------------------------

shp_files <- list.files(
  path = dir_ulmus,
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

find_shapefile <- function(filename) {
  result <- shp_files[tolower(basename(shp_files)) == tolower(filename)]
  if (length(result) == 0) return(character(0))
  result[1]
}

plg_file <- find_shapefile("Ulmus_laevis_plg.shp")
pnt_file <- find_shapefile("Ulmus_laevis_pnt.shp")
syn_file <- find_shapefile("Ulmus_laevis_syn_pnt.shp")

if (length(plg_file) == 0) {
  stop(
    "Required polygon shapefile not found:\n",
    file.path(dir_ulmus, "Ulmus_laevis_plg.shp")
  )
}

ulmus_plg <- sf::st_read(plg_file, quiet = TRUE) |>
  sf::st_make_valid()

if (is.na(sf::st_crs(ulmus_plg))) {
  stop("Polygon shapefile has no CRS:\n", plg_file)
}

ulmus_union <- ulmus_plg |>
  sf::st_union() |>
  sf::st_make_valid()

ulmus_4326 <- sf::st_transform(ulmus_union, 4326)

load_optional_sf <- function(file) {
  if (length(file) == 0) return(NULL)
  
  object <- sf::st_read(file, quiet = TRUE) |>
    sf::st_make_valid()
  
  if (is.na(sf::st_crs(object))) {
    stop("Shapefile has no CRS:\n", file)
  }
  
  sf::st_transform(object, 4326)
}

ulmus_pnt <- load_optional_sf(pnt_file)
ulmus_syn <- load_optional_sf(syn_file)

# ------------------------------------------------------------
# 8. LOAD AND RECLASSIFY EEA ENVIRONMENTAL-ZONE RASTER
# ------------------------------------------------------------

r3035 <- terra::rast(eea_tif)

if (terra::nlyr(r3035) != 1) {
  stop(
    "Expected one categorical raster layer, but found ",
    terra::nlyr(r3035),
    " layers."
  )
}

if (is.na(terra::crs(r3035)) || terra::crs(r3035) == "") {
  stop("EEA raster has no CRS:\n", eea_tif)
}

r_category <- r3035[[1]]

if (!terra::is.factor(r_category)[1]) {
  stop("The EEA raster is not recognized as a categorical raster.")
}

category_levels <- terra::levels(r_category)[[1]]

if (is.null(category_levels) || ncol(category_levels) < 2) {
  stop("No valid category table was found in the EEA raster.")
}

category_table <- data.frame(
  raw_id = category_levels[[1]],
  code = trimws(as.character(category_levels[[2]])),
  stringsAsFactors = FALSE
)

zone_code_lookup <- tibble::tribble(
  ~code, ~new, ~name,
  "BOR", 1L, "Boreal",
  "NEM", 2L, "Temperate (Nemoral + Continental)",
  "CON", 2L, "Temperate (Nemoral + Continental)",
  "ATN", 3L, "Atlantic",
  "ATC", 3L, "Atlantic",
  "LUS", 4L, "Mediterranean (+ Lusitanian)",
  "MDN", 4L, "Mediterranean (+ Lusitanian)",
  "MDS", 4L, "Mediterranean (+ Lusitanian)",
  "MDM", 4L, "Mediterranean (+ Lusitanian)",
  "PAN", 5L, "Pannonian"
)

category_mapping <- category_table |>
  dplyr::left_join(zone_code_lookup, by = "code")

used_mapping <- category_mapping |>
  dplyr::filter(!is.na(new))

if (nrow(used_mapping) == 0) {
  stop("None of the EEA category codes could be assigned.")
}

missing_codes <- setdiff(zone_code_lookup$code, category_table$code)

if (length(missing_codes) > 0) {
  warning(
    "The following expected EEA codes are not present:\n",
    paste(missing_codes, collapse = ", ")
  )
}

r_raw <- r_category
levels(r_raw) <- NULL
names(r_raw) <- "environmental_zone_id"

if (terra::is.factor(r_raw)[1]) {
  stop("The category table could not be removed from the EEA raster.")
}

r_small <- terra::aggregate(
  r_raw,
  fact = agg_fact,
  fun = "modal",
  na.rm = TRUE
)

ulmus_raster_crs <- sf::st_transform(
  ulmus_union,
  crs = terra::crs(r_raw)
)

ulmus_v <- terra::vect(ulmus_raster_crs)

r_crop <- terra::crop(r_small, terra::ext(ulmus_v))
r_mask_raw <- terra::mask(r_crop, ulmus_v)

raw_frequency <- as.data.frame(terra::freq(r_mask_raw))
present_raw_values <- raw_frequency$value[!is.na(raw_frequency$value)]
matching_raw_values <- intersect(present_raw_values, used_mapping$raw_id)

if (length(matching_raw_values) == 0) {
  stop(
    "No mapped environmental-zone IDs occur inside the Ulmus distribution polygon.\n\n",
    "Raster values found:\n",
    paste(present_raw_values, collapse = ", "),
    "\n\nMapped IDs:\n",
    paste(used_mapping$raw_id, collapse = ", ")
  )
}

r_mask <- terra::subst(
  r_mask_raw,
  from = used_mapping$raw_id,
  to = used_mapping$new,
  others = NA
)

# Project once with Leaflet's own helper. This avoids the repeated-world
# artefact caused by supplying a metre-based EPSG:3857 extent as longitude/
# latitude while using project = FALSE.
r_mask_leaflet <- leaflet::projectRasterForLeaflet(
  raster::raster(r_mask),
  method = "ngb"
)

raster_values <- raster::getValues(r_mask_leaflet)

# ------------------------------------------------------------
# 9. ENVIRONMENTAL-ZONE GROUPS AND COLORS
# ------------------------------------------------------------

zone_info <- data.frame(
  id = c(1L, 2L, 3L, 5L, 4L),
  group = c(
    "Environmental zones: Boreal",
    "Environmental zones: Temperate",
    "Environmental zones: Atlantic",
    "Environmental zones: Pannonian",
    "Environmental zones: Mediterranean"
  ),
  label = c(
    "Boreal",
    "Temperate (Nemoral + Continental)",
    "Atlantic",
    "Pannonian",
    "Mediterranean (+ Lusitanian)"
  ),
  color = c(
    "#5E4FA2",
    "#66C2A5",
    "#3288BD",
    "#FEE08B",
    "#F46D43"
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# 25. LOAD COUNTRY BORDERS INCLUDING WESTERN RUSSIA
# ------------------------------------------------------------

world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  sf::st_transform(4326) |>
  sf::st_make_valid()


countries <- world |>
  dplyr::filter(
    continent == "Europe" |
      admin %in% c(
        "Turkey",
        "Georgia",
        "Kazakhstan",
        "Armenia",
        "Azerbaijan",
        "Russia"
      )
  )


# Split geometries crossing the international date line.
countries <- suppressWarnings(
  sf::st_wrap_dateline(
    countries,
    options = c(
      "WRAPDATELINE=YES",
      "DATELINEOFFSET=10"
    ),
    quiet = TRUE
  )
) |>
  sf::st_make_valid()


# Create a valid bounding box with the correct names and CRS.
border_crop_extent <- sf::st_bbox(
  c(
    xmin = map_max_bounds[["xmin"]],
    ymin = map_max_bounds[["ymin"]],
    xmax = map_max_bounds[["xmax"]],
    ymax = map_max_bounds[["ymax"]]
  ),
  crs = sf::st_crs(4326)
)


# Crop the borders to Europe and western Russia.
countries <- suppressWarnings(
  sf::st_crop(
    countries,
    border_crop_extent
  )
)


countries <- countries[
  !sf::st_is_empty(countries),
]


borders <- sf::st_boundary(
  countries
)
# ------------------------------------------------------------
# 11. CREATE BASE MAP
# ------------------------------------------------------------

m <- leaflet::leaflet(
  width = "100%",
  height = 760,
  options = leaflet::leafletOptions(
    zoomControl = TRUE,
    attributionControl = FALSE,
    zoomSnap = 0.05,
    zoomDelta = 0.05,
    wheelPxPerZoomLevel = 450,
    worldCopyJump = FALSE,
    minZoom = 3,
    maxBoundsViscosity = 1.0
  )
)

# Add each environmental zone as its own toggleable raster layer.
for (i in seq_len(nrow(zone_info))) {
  zone_id <- zone_info$id[i]
  zone_color <- zone_info$color[i]
  
  zone_values <- ifelse(
    raster_values == zone_id,
    zone_id,
    NA_real_
  )
  
  zone_raster <- raster::setValues(
    r_mask_leaflet,
    zone_values
  )
  
  zone_palette <- leaflet::colorFactor(
    palette = zone_color,
    domain = zone_id,
    na.color = "#00000000"
  )
  
  m <- m |>
    leaflet::addRasterImage(
      zone_raster,
      colors = zone_palette,
      opacity = env_opacity,
      project = FALSE,
      maxBytes = 20 * 1024 * 1024,
      group = zone_info$group[i],
      options = leaflet::gridOptions(
        noWrap = TRUE,
        zIndex = 1
      )
    )
}

# Country borders, now including the western part of Russia.
m <- m |>
  leaflet::addPolylines(
    data = borders,
    color = col_country_borders,
    weight = 0.9,
    opacity = 0.9,
    options = leaflet::pathOptions(
      clickable = FALSE,
      interactive = FALSE
    )
  )

# ------------------------------------------------------------
# 12. DISTRIBUTION LAYERS
# ------------------------------------------------------------

if (show_ulmus_outline) {
  m <- m |>
    leaflet::addPolygons(
      data = ulmus_4326,
      fill = FALSE,
      color = col_native_range,
      weight = 3.2,
      opacity = 1,
      group = "Distribution: Native range",
      options = leaflet::pathOptions(
        clickable = FALSE,
        interactive = FALSE
      )
    )
}

if (!is.null(ulmus_pnt) && nrow(ulmus_pnt) > 0) {
  isolated_coordinates <- sf::st_coordinates(ulmus_pnt)
  
  m <- m |>
    leaflet::addCircleMarkers(
      lng = isolated_coordinates[, 1],
      lat = isolated_coordinates[, 2],
      radius = 6,
      color = col_isolated,
      fillColor = col_isolated,
      fillOpacity = 0.85,
      stroke = TRUE,
      weight = 1.3,
      group = "Distribution: Isolated populations",
      options = leaflet::pathOptions(
        clickable = FALSE,
        interactive = FALSE
      )
    )
}

if (!is.null(ulmus_syn) && nrow(ulmus_syn) > 0) {
  introduced_coordinates <- sf::st_coordinates(ulmus_syn)
  
  m <- m |>
    leaflet::addCircleMarkers(
      lng = introduced_coordinates[, 1],
      lat = introduced_coordinates[, 2],
      radius = 6,
      color = col_intro_outline,
      fillColor = col_intro,
      fillOpacity = 0.90,
      stroke = TRUE,
      weight = 1.3,
      group = "Distribution: Introduced / naturalized",
      options = leaflet::pathOptions(
        clickable = FALSE,
        interactive = FALSE
      )
    )
}

# ------------------------------------------------------------
# 13. DATA-AVAILABILITY LAYERS
# ------------------------------------------------------------

available_sites <- meas |>
  dplyr::filter(Data == "available")

planned_sites <- meas |>
  dplyr::filter(Data == "planned")

if (nrow(available_sites) > 0) {
  m <- m |>
    leaflet::addMarkers(
      data = available_sites,
      lng = ~Longitude,
      lat = ~Latitude,
      icon = icon_available,
      group = "Data availability: Available",
      popup = ~paste0(
        "<b>", Country, "</b><br/>Available"
      )
    )
}

if (nrow(planned_sites) > 0) {
  m <- m |>
    leaflet::addMarkers(
      data = planned_sites,
      lng = ~Longitude,
      lat = ~Latitude,
      icon = icon_planned,
      group = "Data availability: Planned",
      popup = ~paste0(
        "<b>", Country, "</b><br/>Planned"
      )
    )
}

# ------------------------------------------------------------
# 14. LAYER CONTROL, MAP BOUNDS, AND EXTERNAL CLICKABLE LEGEND
# ------------------------------------------------------------

m <- m |>
  leaflet::addLayersControl(
    overlayGroups = c(
      zone_info$group,
      "Distribution: Native range",
      "Distribution: Isolated populations",
      "Distribution: Introduced / naturalized",
      "Data availability: Available",
      "Data availability: Planned"
    ),
    position = "topright",
    options = leaflet::layersControlOptions(
      collapsed = FALSE,
      autoZIndex = TRUE
    )
  ) |>
  leaflet::fitBounds(
    lng1 = unname(map_extent["xmin"]),
    lat1 = unname(map_extent["ymin"]),
    lng2 = unname(map_extent["xmax"]),
    lat2 = unname(map_extent["ymax"])
  ) |>
  leaflet::setMaxBounds(
    lng1 = unname(map_max_bounds["xmin"]),
    lat1 = unname(map_max_bounds["ymin"]),
    lng2 = unname(map_max_bounds["xmax"]),
    lat2 = unname(map_max_bounds["ymax"])
  ) |>
  htmlwidgets::onRender(
    "
function(el, x) {
  var map = this;

  el.style.backgroundColor = 'white';
  var mapContainer = map.getContainer ? map.getContainer() : el;
  if (mapContainer) {
    mapContainer.style.backgroundColor = 'white';
  }

  var attempts = 0;

  function buildExternalLayerPanel() {
    attempts += 1;

    var control = el.querySelector('.leaflet-control-layers');
    var panel = document.getElementById('elmnet-layer-panel');

    if (!panel) {
      panel = document.createElement('div');
      panel.id = 'elmnet-layer-panel';
      panel.setAttribute('aria-label', 'Map layers');
      el.parentNode.insertBefore(panel, el.nextSibling);
    }

    if (!control) {
      if (attempts < 100) {
        window.setTimeout(buildExternalLayerPanel, 100);
      }
      return;
    }

    if (control.dataset.elmnetReady === 'true') {
      return;
    }

    control.dataset.elmnetReady = 'true';
    control.classList.add('elmnet-external-layers');
    control.classList.add('leaflet-control-layers-expanded');

    panel.innerHTML = '';
    panel.appendChild(control);

    var toggleLink = control.querySelector('.leaflet-control-layers-toggle');
    if (toggleLink) {
      toggleLink.style.display = 'none';
    }

    var overlayContainer = control.querySelector(
      '.leaflet-control-layers-overlays'
    );

    if (!overlayContainer) {
      return;
    }

    var labels = Array.from(
      overlayContainer.querySelectorAll('label')
    );

    overlayContainer.innerHTML = '';

    var sections = [
      {
        title: 'Environmental zones',
        prefix: 'Environmental zones:',
        symbols: {
          'Boreal': {type: 'square', color: '#5E4FA2'},
          'Temperate': {type: 'square', color: '#66C2A5'},
          'Atlantic': {type: 'square', color: '#3288BD'},
          'Pannonian': {type: 'square', color: '#FEE08B'},
          'Mediterranean': {type: 'square', color: '#F46D43'}
        }
      },
      {
        title: 'Distribution (U. laevis)',
        prefix: 'Distribution:',
        symbols: {
          'Native range': {type: 'line', color: '#B22222'},
          'Isolated populations': {
            type: 'dot', color: '#CC79A7', border: '#777777'
          },
          'Introduced / naturalized': {
            type: 'dot', color: '#E6D8AD', border: '#8A7A45'
          }
        }
      },
      {
        title: 'Data availability',
        prefix: 'Data availability:',
        symbols: {
          'Available': {
            type: 'dot', color: '#40E0D0', border: '#000000'
          },
          'Planned': {type: 'cross', color: 'darkorange'}
        }
      }
    ];

    sections.forEach(function(sectionDefinition) {
      var section = document.createElement('section');
      section.className = 'elmnet-layer-section';

      var heading = document.createElement('h3');
      heading.innerHTML = sectionDefinition.title === 'Distribution (U. laevis)'
        ? 'Distribution (<em>U. laevis</em>)'
        : sectionDefinition.title;
      section.appendChild(heading);

      labels.forEach(function(label) {
        var fullLabel = label.textContent.trim();

        if (!fullLabel.startsWith(sectionDefinition.prefix)) {
          return;
        }

        var shortLabel = fullLabel
          .substring(sectionDefinition.prefix.length)
          .trim();

        var input = label.querySelector('input');
        if (!input) {
          return;
        }

        var specification = sectionDefinition.symbols[shortLabel];

        /* Preserve the original Leaflet checkbox node and its event listener. */
        label.innerHTML = '';
        label.className = 'elmnet-layer-option';
        label.appendChild(input);

        if (specification) {
          var symbol = document.createElement('span');
          symbol.className =
            'elmnet-layer-symbol elmnet-symbol-' + specification.type;
          symbol.style.setProperty(
            '--elmnet-symbol-color',
            specification.color
          );

          if (specification.border) {
            symbol.style.setProperty(
              '--elmnet-symbol-border',
              specification.border
            );
          }

          if (specification.type === 'cross') {
            symbol.textContent = '+';
          }

          label.appendChild(symbol);
        }

        var text = document.createElement('span');
        text.className = 'elmnet-layer-label';
        text.textContent = shortLabel;
        label.appendChild(text);

        section.appendChild(label);
      });

      overlayContainer.appendChild(section);
    });

    window.setTimeout(function() {
      map.invalidateSize();
    }, 50);
  }

  if (map.whenReady) {
    map.whenReady(function() {
      window.setTimeout(buildExternalLayerPanel, 50);
    });
  } else {
    window.setTimeout(buildExternalLayerPanel, 50);
  }
}
"
  )

# Return the widget when this script is sourced by data.qmd.
m
