#***************************************************************************************************
#
#   Functions for cleaning King County Data
#
#***************************************************************************************************

#'
#' Add Pinx field to data
#'
#' @param x_df Dataset to add pinx to
#' @param condoComp [FALSE] Is this condoComp data?
#' @export

utilAddPinx <- function(x_df,
                        condoComp = FALSE){

  # If condoComp fix minor to 0000
  if (condoComp) {
    x_df <- as.data.frame(x_df)
    x_df$Minor <- '0000'
  }

  # convert names
  oldNames <- colnames(x_df)
  newNames <- tolower(oldNames)
  colnames(x_df) <- newNames

  # ensure value is numeric
  x_df$major <- as.numeric(x_df$major)
  if(!condoComp) x_df$minor <- as.numeric(x_df$minor)

  # remove NA columns
  x_df <- x_df[!is.na(x_df$major),]
  if(!condoComp) x_df <- x_df[!is.na(x_df$minor),]

  # Remove those with invalid values
  x_df <- x_df[x_df$major < 1000000, ]
  if(!condoComp) x_df <- x_df[x_df$minor < 10000, ]

  # Add leading to major
  x_df$major <- utilLeadZeros(x_df$major, max_char = 6)

  # Add leading to minor
  if(!condoComp) x_df$minor <- utilLeadZeros(x_df$minor, max_char = 4)

  # Combine
  x_df$pinx <- paste0("..", x_df$major, x_df$minor)

  # Reorder
  x_df <- x_df[ ,c("pinx", newNames)]
  colnames(x_df)[2:ncol(x_df)] <- oldNames

  # Return x_df
  return(x_df)
}

#'
#' Add leading zeros
#'
#' @param x_nbr Number to add zeros to
#' @param max_char [6] Maximum number of characters
#' @export

utilLeadZeros <- function(x_nbr,
                          max_char = 6){

  miss_zero <- max_char - nchar(x_nbr)
  paste0(unlist(as.character(lapply(miss_zero, utilBuildLeadZero))), x_nbr)
}

#'
#' Add leading zeros
#'
#' @param x Number to add zeros to
#' @export

utilBuildLeadZero <- function(x){

  if(length(x) == 0){
    return(0)
  } else {
    gsub('x', '0', paste0(unlist(rep("x", x)), collapse=''))
  }
}

