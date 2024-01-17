#
#   Prepare the King County data
#
#***************************************************************************************************

  library(tidyverse)
  library(kingCoData)

  ## Set Paths
  data_path <- file.path(getwd(), 'data')
  raw_path <- file.path(data_path, 'raw')
  ready_path <- file.path(data_path, 'ready')

  CURR_YEAR = as.numeric(substr(Sys.Date(), 1, 4)) - 1

### Load Data --------------------------------------------------------------------------------------

  # Parcel Data
  par99_df <- read.csv(file.path(raw_path, 'parcel_1999.csv'))
  parcur_df <- read.csv(file.path(raw_path, 'EXTR_parcel.csv'))

  # Parcel Data
  rb99_df <- read.csv(file.path(raw_path, 'resbldg_1999.csv'))
  rbcur_df <- read.csv(file.path(raw_path,'EXTR_resbldg.csv'))

  # Tax Data
  tax99_df <- readRDS(file.path(ready_path, 'tax_1999.rds'))
  taxcur_df <- readRDS(file.path(ready_path, 'tax_current.rds'))

  # Tax Data
  geo99_df <- readRDS(file.path(ready_path, 'geo_99.rds'))
  geocur_df <- readRDS(file.path(ready_path, 'geo_new.rds'))

  # Changes
  changes_df <- readRDS(file.path(ready_path, 'major_changes.rds'))

  # Sales
  sales_df <- readRDS(file.path(ready_path, 'sales.RDS'))

  # Submarkets
  subm_df <- read.csv(file.path(ready_path, 'submarkets.csv'))

### Prepare Parcel, ResBldg and Tax Files ----------------------------------------------------------

 ## Parcel
par99_df <- par99_df %>%
  dplyr::select(Major, Minor, prop_type = PropType, area=Area, sub_area = SubArea,
                city = DistrictName, zoning = CurrentZoning, present_use = PresentUse,
                sqft_lot = SqFtLot, view_rainier = MtRainier, view_olympics = Olympics,
                view_cascades = Cascades, view_territorial = Territorial,
                view_skyline = SeattleSkyline, view_sound = PugetSound,
                view_lakewash = LakeWashington, view_lakesamm = LakeSammamish,
                view_otherwater = SmallLakeRiverCreek, view_other = OtherView,
                wfnt = WfntLocation, golf = AdjacentGolfFairway, greenbelt = AdjacentGreenbelt,
                noise_traffic = TrafficNoise, subdivision = PlatName) %>%
  utilAddPinx(.)

parcur_df <- parcur_df %>%
  dplyr::select(Major, Minor, prop_type = PropType, area=Area, sub_area = SubArea,
                city = DistrictName, zoning = CurrentZoning, present_use = PresentUse,
                sqft_lot = SqFtLot, view_rainier = MtRainier, view_olympics = Olympics,
                view_cascades = Cascades, view_territorial = Territorial,
                view_skyline = SeattleSkyline, view_sound = PugetSound,
                view_lakewash = LakeWashington, view_lakesamm = LakeSammamish,
                view_otherwater = SmallLakeRiverCreek, view_other = OtherView,
                wfnt = WfntLocation, golf = AdjacentGolfFairway, greenbelt = AdjacentGreenbelt,
                noise_traffic = TrafficNoise, subdivision = PlatName) %>%
  utilAddPinx(.)

 ## Res Building
rbcur_df <- rbcur_df %>%
  utilAddPinx(.) %>%
  dplyr::select(pinx, bldg_nbr = BldgNbr, units = NbrLivingUnits, zip = ZipCode, stories = Stories,
                grade = BldgGrade, sqft = SqFtTotLiving, sqft_1 = SqFt1stFloor,
                sqft_fbsmt = SqFtFinBasement, fbsmt_grade = FinBasementGrade,
                garb_sqft = SqFtGarageBasement, gara_sqft = SqFtGarageAttached,
                beds = Bedrooms, bath_half = BathHalfCount, bath_3qtr = Bath3qtrCount,
                bath_full = BathFullCount, condition = Condition,
                year_built = YrBuilt, year_reno = YrRenovated, view_util = ViewUtilization) %>%
  dplyr::group_by(pinx) %>%
  dplyr::mutate(bldgs = dplyr::n()) %>%
  dplyr::filter(bldgs == 1) %>%
  dplyr::filter(bldg_nbr == 1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(units == 1)

rb99_df <- rb99_df %>%
  utilAddPinx(.) %>%
  dplyr::select(pinx, bldg_nbr = BldgNbr, units = NbrLivingUnits, stories = Stories,
                grade = BldgGrade, sqft = SqFtTotLiving, sqft_1 = SqFt1stFloor,
                sqft_fbsmt = SqFtFinBasement, fbsmt_grade = FinBasementGrade,
                garb_sqft = SqFtGarageBasement, gara_sqft = SqFtGarageAttached,
                beds = Bedrooms, bath_half = BathHalfCount, bath_3qtr = Bath3qtrCount,
                bath_full = BathFullCount, condition = Condition,
                year_built = YrBuilt, year_reno = YrRenovated, view_util = ViewUtilization)  %>%
  dplyr::group_by(pinx) %>%
  dplyr::mutate(bldgs = dplyr::n()) %>%
  dplyr::filter(bldgs == 1) %>%
  dplyr::filter(bldg_nbr == 1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(units == 1)

### Add matching year ------------------------------------------------------------------------------

  # Simple year buit/Reno data
  rbs_df <- rb99_df %>%
    dplyr::select(pinx, yb99 = year_built, yr99 = year_reno) %>%
    dplyr::full_join(rbcur_df %>%
                       dplyr::select(pinx, ybnew = year_built, yrnew = year_reno),
                     by = 'pinx')

  # Add RBS to sales and filter by property type
  trim_df <- sales_df %>%
    inner_join(rbs_df, by = 'pinx') %>%
    dplyr::filter(property_type %in% c(2, 3, 10, 11))

### Split by match type and add data ---------------------------------------------------------------

  # No change
  nochg_df <- trim_df %>%
    dplyr::filter(!is.na(yb99) & !is.na(ybnew) &
                    yb99 == ybnew & yrnew == 0) %>%
    dplyr::mutate(match_type = 'nochg',
                  match_year = CURR_YEAR)

  x_df <- trim_df %>%
    dplyr::filter(!sale_id %in% nochg_df$sale_id)

  # Demolished
  demo_df <- x_df %>%
    dplyr::filter(is.na(ybnew)) %>%
    dplyr::mutate(match_type = 'demo',
                  match_year = 1999)

  x_df <- x_df %>%
    dplyr::filter(!sale_id %in% demo_df$sale_id)

  # New construction
  new_df <- x_df %>%
    dplyr::filter(is.na(yb99)) %>%
    dplyr::mutate(match_type = ifelse(ybnew < 1999, 'miss99', 'new'),
                  match_year = CURR_YEAR)

  x_df <- x_df %>%
    dplyr::filter(!sale_id %in% new_df$sale_id)

  # Rebuilt home
  rebuilt_df <- x_df %>%
    dplyr::filter(yb99 != ybnew) %>%
    dplyr::mutate(match_type = ifelse(ybnew > sale_year, 'rebuilt - after',
                                      ifelse(ybnew < sale_year, 'rebuilt - before', 'rebuilt - ?')),
                  match_year = ifelse(match_type == 'rebuilt - after', CURR_YEAR,
                                      ifelse(match_type == 'rebuilt - before', 1999, -1)))

  x_df <- x_df %>%
    dplyr::filter(!sale_id %in% rebuilt_df$sale_id)

  # Renovated
  reno_df <- x_df %>%
    dplyr::mutate(match_type = ifelse(yrnew > sale_year, 'reno - after',
                                      ifelse(yrnew < sale_year, 'reno - before', 'reno - ?')),
                  match_year = ifelse(match_type == 'reno - after', 1999,
                                      ifelse(match_type == 'reno - before',
                                             ifelse(yrnew < 1999, 1999, CURR_YEAR), -1)))

### Join and Row Bind ------------------------------------------------------------------------------

 # Row Binds
  sale_df <- nochg_df %>%
    dplyr::bind_rows(., demo_df, new_df, rebuilt_df, reno_df) %>%
    dplyr::filter(match_year != -1)

  # Matched to 99
  sale99_df <- sale_df %>%
    dplyr::filter(match_year == 1999) %>%
    dplyr::select(-c(yb99, yr99, ybnew, yrnew)) %>%
    dplyr::left_join(., par99_df, by = 'pinx') %>%
    dplyr::left_join(., rb99_df, by = 'pinx') %>%
    dplyr::left_join(., tax99_df %>%
                       dplyr::select(-tax_year), by = 'pinx') %>%
    dplyr::left_join(., geo99_df %>%
                       dplyr::mutate(pinx = paste0('..', PIN)) %>%
                       dplyr::select(pinx, latitude, longitude), by = 'pinx')

 # Matched to 2019
  salecur_df <- sale_df %>%
    dplyr::filter(match_year == CURR_YEAR) %>%
    dplyr::select(-c(yb99, yr99, ybnew, yrnew)) %>%
    dplyr::left_join(., parcur_df, by = 'pinx') %>%
    dplyr::left_join(., rbcur_df, by = 'pinx') %>%
    dplyr::left_join(., taxcur_df %>%
                     dplyr::select(-tax_year), by = 'pinx') %>%
    dplyr::left_join(., geocur_df %>%
                       dplyr::mutate(pinx = paste0('..', PIN)) %>%
                       dplyr::select(pinx, latitude, longitude), by = 'pinx')

 # Bind Together
 kingco_sales <- dplyr::bind_rows(sale99_df, salecur_df) %>%
   dplyr::select(-c(Major, Minor, prop_type)) %>%
   dplyr::filter(present_use %in% c(2, 6, 29)) %>%
   dplyr::filter(property_class == 8) %>%
   dplyr::filter(principal_use == 6) %>%
   dplyr::filter(!is.na(latitude)) %>%
   dplyr::filter(!is.na(land_val)) %>%
   dplyr::filter(sale_price > 50000) %>%
   dplyr::mutate(sale_date = as.Date(sale_date),
                 golf = ifelse(golf == 'Y', 1, 0),
                 greenbelt = ifelse(greenbelt == 'Y', 1, 0)) %>%
   dplyr::select(sale_id, pinx, sale_date, sale_price, sale_nbr, sale_warning,
                 join_status = match_type, join_year = match_year,
                 latitude, longitude, area, city, zoning, subdivision,
                 present_use, land_val, imp_val,
                 year_built, year_reno, sqft_lot, sqft, sqft_1, sqft_fbsmt, grade, fbsmt_grade,
                 condition, stories, beds, bath_full, bath_3qtr, bath_half, garb_sqft, gara_sqft,
                 wfnt, golf, greenbelt, noise_traffic, view_rainier, view_olympics, view_cascades,
                 view_territorial, view_skyline, view_sound, view_lakewash, view_lakesamm,
                 view_otherwater, view_other) %>%
   dplyr::left_join(.,
                    subm_df %>% dplyr::select(area, submarket),
                    by = 'area') %>%
   dplyr::distinct(sale_id, .keep_all = TRUE)

### Write out data ---------------------------------------------------------------------------------

 usethis::use_data(kingco_sales, overwrite = T)

### Inference Universe -----------------------------------------------------------------------------

 homes_df <- parcur_df %>%
   dplyr::left_join(., rbcur_df, by = 'pinx') %>%
   dplyr::left_join(., taxcur_df %>%
                      dplyr::select(-tax_year), by = 'pinx') %>%
   dplyr::left_join(., geocur_df %>%
                      dplyr::mutate(pinx = paste0('..', PIN)) %>%
                      dplyr::select(pinx, latitude, longitude), by = 'pinx')

 # Bind Together
 kingco_homes <- homes_df %>%
   dplyr::select(-c(Major, Minor, prop_type)) %>%
   dplyr::filter(present_use %in% c(2, 6, 29)) %>%
   dplyr::filter(!is.na(latitude)) %>%
   dplyr::filter(!is.na(land_val)) %>%
   dplyr::filter(!is.na(grade)) %>%
   dplyr::mutate(golf = ifelse(golf == 'Y', 1, 0),
                 greenbelt = ifelse(greenbelt == 'Y', 1, 0)) %>%
   dplyr::select(pinx, latitude, longitude, area, city, zoning, subdivision,
                 present_use, land_val, imp_val,
                 year_built, year_reno, sqft_lot, sqft, sqft_1, sqft_fbsmt, grade, fbsmt_grade,
                 condition, stories, beds, bath_full, bath_3qtr, bath_half, garb_sqft, gara_sqft,
                 wfnt, golf, greenbelt, noise_traffic, view_rainier, view_olympics, view_cascades,
                 view_territorial, view_skyline, view_sound, view_lakewash, view_lakesamm,
                 view_otherwater, view_other) %>%
   dplyr::left_join(.,
                    subm_df %>% dplyr::select(area, submarket),
                    by = 'area')

   usethis::use_data(kingco_homes, overwrite = T)


####################################################################################################
####################################################################################################
