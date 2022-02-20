#***************************************************************************************************
#
#  Create the Geo data (x,y coords) for all parcels in king county in 1999 and current
#
#***************************************************************************************************

  ## Load Libraries

  library(tidyverse)
  library(kingCoData)

  ## Set Paths
  data_path <- file.path(getwd(), 'data')
  raw_path <- file.path(data_path, 'raw')
  ready_path <- file.path(data_path, 'ready')

  # Set Cutoffs
  sale_min <- 1999
  sale_max <-  as.numeric(substr(Sys.Date(), 1, 4)) - 1

  # Sales data
  sales_df <- read.csv(file.path(file.path(data_path, 'raw', 'sales',
                                          'extr_rpsale.csv')))

  # base clean
  clean_df <- sales_df %>%
    dplyr::filter(Major > 0 & SalePrice > 0)

  clean_df <- clean_df %>%
    dplyr::mutate(doc_date = paste(substr(DocumentDate, 4, 5), substr(DocumentDate, 1, 2),
                                   substr(DocumentDate, 7, 10), sep=""),
                  sale_date = as.POSIXct(strptime(doc_date, "%d%m%Y")),
                  sale_year = as.numeric(format(sale_date, "%Y"))) %>%
    dplyr::filter(!is.na(sale_date))

  # eliminate Transactions prior to Sales Year Limit
  clean_df <- clean_df %>%
    dplyr::filter(sale_year >= sale_min & sale_year <= sale_max)

  # add PINX
  clean_df <- utilAddPinx(clean_df)

  # add trans count and limit by paramter
  clean_df <- clean_df %>%
    dplyr::arrange(sale_date) %>%
    dplyr::group_by(pinx) %>%
    dplyr::mutate(sales_cnt = dplyr::n(),
                  sale_nbr = 1:sales_cnt) %>%
    dplyr::ungroup()

  # add MultiParcel sale designation
  clean_df <- clean_df %>%
    dplyr::group_by(ExciseTaxNbr) %>%
    dplyr::mutate(parcel_cnt = dplyr::n(),
                  multi_parcel = ifelse(parcel_cnt > 1, 1, 0)) %>%
    dplyr::ungroup()

  # Add unique IDs
  clean_df <- clean_df %>%
    dplyr::arrange(ExciseTaxNbr) %>%
    dplyr::mutate(temp_id = paste0(sale_year, '..', ExciseTaxNbr)) %>%
    dplyr::group_by(sale_year) %>%
    dplyr::mutate(sale_id = paste0(sale_year, '..', as.numeric(as.factor(temp_id)))) %>%
    dplyr::ungroup()

  # Fix the "Warning" Field.  Add a leading/trailing space for the grep()
  clean_df <- clean_df %>%
    dplyr::filter(!SaleReason %in% 2:19) %>%
    dplyr::filter(!SaleInstrument %in% c(0,1,4:28)) %>%
    dplyr::mutate(sale_warning = paste0(' ', SaleWarning, ' ')) %>%
    dplyr::filter(!grepl(' 1 ', sale_warning) &
                    !grepl(' 2 ', sale_warning) &
                    !grepl(' 5 ', sale_warning) &
                    !grepl(' 6 ', sale_warning) &
                    !grepl(' 7 ', sale_warning) &
                    !grepl(' 8 ', sale_warning) &
                    !grepl(' 9 ', sale_warning) &
                    !grepl(' 11 ', sale_warning) &
                    !grepl(' 12 ', sale_warning) &
                    !grepl(' 13 ', sale_warning) &
                    !grepl(' 14 ', sale_warning) &
                    !grepl(' 18 ', sale_warning) &
                    !grepl(' 19 ', sale_warning) &
                    !grepl(' 20 ', sale_warning) &
                    !grepl(' 21 ', sale_warning) &
                    !grepl(' 22 ', sale_warning) &
                    !grepl(' 23 ', sale_warning) &
                    !grepl(' 25 ', sale_warning) &
                    !grepl(' 27 ', sale_warning) &
                    !grepl(' 31 ', sale_warning) &
                    !grepl(' 32 ', sale_warning) &
                    !grepl(' 33 ', sale_warning) &
                    !grepl(' 37 ', sale_warning) &
                    !grepl(' 39 ', sale_warning) &
                    !grepl(' 43 ', sale_warning) &
                    !grepl(' 46 ', sale_warning) &
                    !grepl(' 48 ', sale_warning) &
                    !grepl(' 49 ', sale_warning) &
                    !grepl(' 50 ', sale_warning) &
                    !grepl(' 51 ', sale_warning) &
                    !grepl(' 52 ', sale_warning) &
                    !grepl(' 53 ', sale_warning) &
                    !grepl(' 59 ', sale_warning) &
                    !grepl(' 61 ', sale_warning) &
                    !grepl(' 63 ', sale_warning) &
                    !grepl(' 64 ', sale_warning) &
                    !grepl(' 66 ', sale_warning))

  ## Remove multiparcel and limit
  clean_df <- clean_df %>%
    dplyr::filter(multi_parcel == 0) %>%
    dplyr::select(sale_id, pinx, sale_date, sale_year, sale_price = SalePrice,
                  property_type = PropertyType, principal_use = PrincipalUse,
                  property_class = PropertyClass, sales_cnt, sale_nbr, sale_warning)

  # Write out temporary sales files
  saveRDS(clean_df, file.path(file.path(data_path, 'ready', 'sales.rds')))
