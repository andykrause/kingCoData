#***************************************************************************************************
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


  CURR_YEAR = as.numeric(substr(Sys.Date(), 1, 4))

### Tax Data --------------------------------------------------------------------------------------

 # Tax Data
 taxraw_df <- read.csv(file.path(raw_path, 'EXTR_taxacctreceivable_v.csv'))

 # Create two annual snapshots
 tax_df <- taxraw_df %>%
   dplyr::select(AcctNbr, BillYr, LandVal, ImpsVal)

 tax_df$AcctNbr <- as.character(tax_df$AcctNbr)
 for (k in 1:5){
   a <- which(nchar(tax_df$AcctNbr) < 12)
   tax_df$AcctNbr [a]<- paste0('0', tax_df$AcctNbr[a])
 }

 tax_df <- tax_df %>%
   dplyr::filter(BillYr == 2000 | BillYr == CURR_YEAR) %>%
   dplyr::mutate(pinx = paste0('..', substr(AcctNbr, 1, 10))) %>%
   dplyr::select(pinx, tax_year = BillYr, land_val = LandVal, imp_val = ImpsVal) %>%
   dplyr::mutate(tax_year = tax_year - 1)

 # Save
 saveRDS(tax_df %>% dplyr::filter(tax_year == 1999),
         file.path(ready_path, 'tax_1999.RDS'))
 saveRDS(tax_df %>% dplyr::filter(tax_year != 1999),
         file.path(ready_path, 'tax_current.RDS'))

### Change Data ------------------------------------------------------------------------------------

 # Load data
 chraw_df <- read.csv(file.path(raw_path, 'extr_changehist_v.csv'))

 # Create limited change data
 ch_df <- chraw_df %>%
  dplyr::filter(Type %in% c(3,6,7,13)) %>%
  dplyr::arrange(desc(EventDate)) %>%
  dplyr::distinct(Major, Minor, .keep_all = TRUE) %>%
  utilAddPinx(.) %>%
  dplyr::select(pinx, type = Type, event_date = EventDate) %>%
  dplyr::mutate(event_date = as.Date(event_date)) %>%
  dplyr::filter(event_date >= as.Date('1999-01-01'))

 # Save
 saveRDS(ch_df, file.path(ready_path, 'major_changes.RDS'))

#***************************************************************************************************
#***************************************************************************************************


