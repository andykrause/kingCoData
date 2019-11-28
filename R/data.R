
#' King County Sales 1999 to 2019
#'
#' King County residential transactions of single family and townhomes.
#'
#' @docType data
#' @usage data(kingco_sales)
#' @source King County Assessor: http://info.kingcounty.gov/assessor/DataDownload/
#' @format A \code{"data.frame"} with 480,011 rows and 47 variables
#' \describe{
#'   \item{pinx}{The unique property identifying code.  Original value is preceded by
#'   two '..'s to prevent the dropping of leading zeros}
#'   \item{sale_id}{The unique transaction identifying code.}
#'   \item{sale_price}{Price of the home}
#'   \item{sale_date}{Date of sale}
#'   \item{use_type}{Property use type}
#'   \item{area}{Assessment area or zone}
#'   \item{lot_sf}{Size of lot in square feet}
#'   \item{wfnt}{Is property waterfront?}
#'   \item{bldg_grade}{Quality of the building construction (higher is better)}
#'   \item{tot_sf}{Size of home in square feet}
#'   \item{beds}{Number of bedrooms}
#'   \item{baths}{Number of bathrooms}
#'   \item{age}{Age of home}
#'   \item{eff_age}{Age of home, considering major remodels}
#'   \item{longitude}{Longitude}
#'   \item{latitude}{Latitude }
#'}
#'
"kingco_sales"
