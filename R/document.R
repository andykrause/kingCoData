#***************************************************************************************************
#
#   Functions for explaining data
#
#***************************************************************************************************

#'
#' Add Pinx field to data
#'
#' @param field Field to explain
#' @export

explain <- function(field){

  if (grepl('view_', field)) field <- 'view'

  field <- structure(field, class = field)
  UseMethod("explain", field)
}


#'
#' Explain the join_status field
#' @method explain join_status
#' @param field Field to explain

explain.join_status <- function(field){

  data.frame(factor = c('demo', 'new', 'nochg', 'miss99', 'reno - after', 'reno - before',
                        'rebuilt - after', 'rebuilt - before'),
             description = c('Home was demolished after the sale',
                             'New home on a parcel that did not exist in 1999',
                             'Home that existed in 1999 and still exists with no major changes',
                             'Home that existed in 1999 but wasnt found in 1999 data',
                             'Home renovated after the sale',
                             'Home renovated before the sale',
                             'Home rebuilt after the sale (might be a land sale)',
                             'Home rebuilt before sale'))
}

#'
#' Explain the present_use field
#' @method explain present_use
#' @param field Field to explain

explain.present_use <- function(field){

  data.frame(factor = c(2, 6, 29),
             description = c('Single Family Detached',
                             'Single Family Detached in non-SFR zoning',
                             'Townhome'))
}

#'
#' Explain the present_use field
#' @method explain grade
#' @param field Field to explain

explain.grade <- function(field){

  data.frame(factor = c(1:13, 20),
             description = c('Cabin', 'Substandard', 'Poor', 'Low', 'Fair', 'Low Average',
                             'Average', 'Good', 'Better', 'Very Good', 'Excellent', 'Luxury',
                             'Mansion', 'Exceptional Property'))
}

#'
#' Explain the fbsmt_grade field
#' @method explain fbsmt_grade
#' @param field Field to explain

explain.fbsmt_grade <- function(field){

  data.frame(factor = c(0:13, 20),
             description = c('None', 'Cabin', 'Substandard', 'Poor', 'Low', 'Fair', 'Low Average',
                             'Average', 'Good', 'Better', 'Very Good', 'Excellent', 'Luxury',
                             'Mansion', 'Exceptional Property'))
}

#'
#' Explain the fbsmt_grade field
#' @method explain condition
#' @param field Field to explain

explain.condition <- function(field){

  data.frame(factor = c(1:5),
             description = c('Poor', 'Fair', 'Average', 'Good', 'Very Good'))
}

#'
#' Explain the fbsmt_grade field
#' @method explain wfnt
#' @param field Field to explain

explain.wfnt <- function(field){

  data.frame(factor = c(0:9),
             description = c('None', 'Duwamish', 'Elliot Bay', 'Puget Sound', 'Lake Union',
                             'Ship Canal', 'Lake Washington', 'Lake Sammammish', 'Other Lake',
                             'River/Slough'))
}


#'
#' Explain the fbsmt_grade field
#' @method explain traffic_noise
#' @param field Field to explain

explain.traffic_noise <- function(field){

  data.frame(factor = c(0:3),
             description = c('None', 'Moderate', 'High', 'Extreme'))
}


#'
#' Explain the view field
#' @method explain view
#' @param field Field to explain

explain.view <- function(field){

  data.frame(factor = c(0:4),
             description = c('None', 'Fair', 'Average', 'Good', 'Excellent'))
}


