#' Table of gear types available in PacFIN data
#'
#' @name GearTable
#' @docType data
#' @format A data frame with 65 rows and 6 columns
#' \describe{
#'   \item{TYPE}{A numeric}
#'   \item{PACFIN_GEAR_CODE}{A three letter character string denoting the gear type}
#'   \item{GRID}{A three letter character string denoting the gear type}
#'   \item{GROUP}{A three letter character string that groups gear type into general groups}
#'   \item{SHORT}{Character string describing the gear type}
#'   \item{DESCRIPTION}{Character string describing the gear type}
#' }
#' @source <https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt>
"GearTable"

#' Table of INPFC areas available in PacFIN data
#'
#' @name INPFCTable
#' @docType data
#' @format A data frame with 69 rows and 7 columns
#' \describe{
#'   \item{INPFC_AREA_TYPE_CODE}{A character string}
#'   \item{ARID}{A character string}
#'   \item{INPFC}{A three letter character string denoting the INPFC area}
#'   \item{TYPE}{A character string denoting the INPFC type}
#'   \item{COUNCIL}{A character string denoting the Fishery Managment Council}
#'   \item{SHORT}{Character string describing the area}
#'   \item{DESCRIPTION}{Character string describing the area}
#' }
#' @source <https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/ar_tree.txt>
"INPFCTable"

#' Table of ports available in PacFIN data
#'
#' @name PortTable
#' @docType data
#' @format A data frame with 567 rows and 4 columns
#' \describe{
#'   \item{PACFIN_PORT_CODE}{A character string of port codes}
#'   \item{AGENCY_CODE}{Character denoting the collection agency}
#'   \item{AGENCY_PORT_CODE}{A string denoting the sampling agency port code}
#'   \item{DESCRIPTION}{A character string denoting the full port anme}
#' }
#' @source <https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/agency_ports_pcid.txt>
"PortTable"

#' Example table of catches by year, state, and geargroup
#'
#' @name catch_for_expansions
#' @docType data
#' @format A data frame with 36 rows and 4 columns
#' \describe{
#'   \item{year}{A numeric string for landing year}
#'   \item{state}{Character string for the landed state}
#'   \item{geargroup}{A string denoting the gear type}
#'   \item{catch_mt}{A numeric value for landings in metric tons}
#' }
"catch_for_expansions"
