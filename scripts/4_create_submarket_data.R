#***************************************************************************************************
#
#  Create the King County Submarkets
#
#***************************************************************************************************

 ## Set Paths
  data_path <- file.path(getwd(), 'data')
  raw_path <- file.path(data_path, 'raw')
  ready_path <- file.path(data_path, 'ready')

  kingco_submarkets <- data.frame(submarket = c(rep('A', 4), rep('B', 8), rep('C', 6), rep('D', 5),
                                             rep('E', 5), rep('F', 6), rep('G', 3), rep('H', 1),
                                             rep('I', 7), rep('J', 4), rep('K', 9), rep('L', 4),
                                             rep('M', 6), rep('N', 5), rep('O', 3), rep('P', 3),
                                             rep('Q', 5), rep('R', 7), rep('S', 4)),
                                  area = c(1,2,3,4,
                                    5,6,9,19,39,42,43,82,
                                    7,8,10,44,45,46,
                                    11:15,
                                    20,21,79,81,78,
                                    16,17,18,48,76,77,
                                    23,49,96,
                                    100,
                                    26,27,52,55,88,53,54,
                                    22,24,25,50,
                                    29,30,32,51,66,59,60,85,86,
                                    28,61,62,87,
                                    40,41,57,56,58,84,
                                    70,75,80,94,90,
                                    35,69,71,
                                    36,72,95,
                                    37,38,73,74,93,
                                    67,91,65,31,68,64,47,
                                    33,34,63,92)) %>%

  dplyr::arrange(area) %>%
  tibble::as_tibble() %>%
  dplyr::select(area, submarket)

usethis::use_data(kingco_submarkets, overwrite = TRUE)

write.csv(kingco_submarkets, file.path(ready_path, 'submarkets.csv'))
