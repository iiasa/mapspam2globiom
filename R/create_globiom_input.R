#'@title Creates GLOBIOM crop distribution and land cover input using output
#'  from the `mapspamc` package
#'
#'@description `create_globiom_input` creates two gdx files. One file updates
#'  the global land cover data in GLOBIOM for the target country, while the
#'  other file replaces global land use data. Both files will be saved in the
#'  `processed_data/results` folder created by `mapspamc`. Note that the area
#'  will be expressed in 1000 ha, which is common in GLOBIOM. Before
#'  `create_globiom_input()` can be run, you need to prepare four input files.
#'
#'  First, you need to collect a new country-level land cover map. Any product
#'  can be used as long as it contains information on the six GLOBIOM land cover
#'  classes. The most obvious choice would be to take a national land cover map
#'  for the year 2000 or any other map that is close to the year for which the
#'  subnational statistics are available. If such map is not available it is
#'  also possible to use a global land cover product and use the country polygon
#'  to mask the relevant area.
#'
#'  Second, you need to prepare a mapping between the
#'  land cover map and GLOBIOM land use classes. An example mapping is included
#'  for the ESACCI land cover files and can be opened by calling esacci2globiom.
#'
#'  Third, you need to prepare a mapping between the `mapspamc` crop and GLOBIOM
#'  crop classes. The standard mapping is and can be opened by calling
#'  crop2globiom. In case the user wants to add an additional crop in GLOBIOM
#'  (See vignette), the mapping can be changed.
#'
#'  Finally, a polygon file is
#'  needed with the location of the GLOBIOM simulation units (simu), which can
#'  be clipped from the global GLOBIOM simu shapefile that is stored in
#'  mapspamc_db.
#'
#'@param lc_map Data.frame with the mapping between country and GLOBIOM land
#'  cover codes. The country land cover codes must have column name 'lc_code'
#'  and the GLOBIOM land cover codes must have column name 'globiom_lc_code'.
#'  Country land cover codes can correspond to multiple GLOBIOM land cover
#'  codes.
#'@param crop_map Data.frame with the mapping between mapspamc and GLOBIOM crop
#'  codes. The country crop must have column name 'crop' and the GLOBIOM crop
#'  codes must have column name 'globiom_crop'. Country crop codes can
#'  correspond to multiple GLOBIOM crop codes.
#'@param lc National land cover map that is consistent with the mapping
#'  data.frame.
#'@param simu National simu polygon file.
#'@param simu grid file created by `mapspamc.
#'@param area_type Selection of crop distribution maps produced by `mapspamc`:
#'  "pa" (physical area - default) or "ha" (harvested area).
#'@param param param Object of type `mapspamc_par` that bundles all `mapspamc`
#'  parameters, including core model folders, alpha-3 country code, year,
#'  spatial resolution, most detailed level at which subnational statistics are
#'  available, administrative unit level at which the model is solved and type
#'  of model.
#'
#'@export
create_globiom_input <- function(lc_map, crop_map, lc, simu, grid, param, area_type = "pa") {

  # Test if gdxrrw and gams are installed.
  setup_gams(param)

  # Aggregate land cover map to GLOBIOM land cover classes at simu level
  lc_df <- calc_lc_area(lc_map, lc, simu, param)

  # Aggregate mapspam crop distribution tif files to GLOBIOM crop classes at simu
  # level. We do this for physical area but if needed it can also be done for
  # harvested area by replacing "pa", with "ha".
  # Not that the area is expressed in 1000 ha, which is common in GLOBIOM!
  simu_r <- terra::rasterize(terra::vect(simu), grid, field = "SimUID")
  crop_df <- spam2simu(area_type, crop_map, simu_r, param)

  # Merge simu_lc_df with simu_crop_df.
  # CrpLnd and OthAgri in the lc_map are replaced by CrpLnd and OthAgri
  # from from SPAM using the following rules:
  # If SimUarea > sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel,
  # Grass), the surplus is added to OthNatLnd.

  # If SimUarea < sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel, Grass),
  # the shortage is subtracted from from the classes in this order:
  # 1. OthNatLnd; 2. take from NotRel; 3. WetLnd; 4 Forest; 5 Grass
  lc_upd <- merge_lc_crop(lc_df, crop_df)

  # Land cover gdx file
  create_land_cover_gdx(lc_upd, simu, param)

  # Crop distribution gdx file
  create_crop_distribution_gdx(crop_df, simu, param)
}
