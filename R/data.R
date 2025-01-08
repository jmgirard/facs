#' List of Official FACS Codes
#'
#' Information about each of the official FACS codes including action units
#' (AUs), action descriptors (ADs), and supplementary codes (SCs).
#'
#' @format ## `facs_codes`
#' A data frame with 73 rows and 5 columns:
#' \describe{
#'   \item{type}{Code type}
#'   \item{number}{Code number}
#'   \item{name}{Code name}
#'   \item{group}{Code group}
#'   \item{chapter}{Primary chapter from FACS 2002 manual}
#' }
#' @source Ekman, P., Friesen, W. V., & Hager, J. (2002). Facial action coding
#'   system: A technique for the measurement of facial movement. Research Nexus.
"facs_codes"

#' List of all available emotion prototypes
#'
#' Description
#'
#' @format ## `prototypes`
#' A data frame with 606 rows and 7 columns:
#' \describe{
#'   \item{source}{A string describing the source of the prototype.}
#'   \item{emotion}{A string describing the name of the emotion.}
#'   \item{emotion_type}{An optional string describing the emotion type.}
#'   \item{config_num}{A numerical ID within each source-emotion combo.}
#'   \item{config_type}{A string describing the configuration type.}
#'   \item{code}{FACS coding string describing each configuration.}
#'   \item{special}{A string with additional non-FACS information.}
#' }
"prototypes"


#' Full coding scheme will all FACS codes
#'
#' For user convenience, `scheme_full` is provided that contains occurrence,
#' intensity, and asymmetry coding for all codes possible in FACS 2002.
#'
#' @format An object created by [scheme()]
"scheme_full"
