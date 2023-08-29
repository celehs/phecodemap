#' Mappings of PheCodes to ICD codes.
#'
#' A dataset containing the mappings of PheCodes to ICD-9 and ICD-10-cm Codes.
#'
#' @format A data frame with 106393 rows and 6 variables:
#' \describe{
#'   \item{ICD_version}{ICD version, ICD-9 or ICD-10-cm}
#'   \item{ICD_id}{ICD-9 and ICD-10-cm Codes}
#'   \item{ICD_str}{Description of ICD Codes}
#'   \item{Phecode}{PheCode ID}
#'   \item{Phenotype}{Description of PheCode ID}
#'   \item{Rollup}{Rollup}
#' }
#' @source \url{https://phewascatalog.org/}
"icdmap"




#' Information of ICD codes
#'
#' A datatable of the information of icd codes from UMLS version 2021AB
#'
#' @format A data frame with 99765 rows and 3 columns:
#' \describe{
#'   \item{id}{ICD code}
#'   \item{term}{ICD string}
#'   \item{version}{ICD version, ICD-9 or ICD-10-cm}
#' }
#' @source \url{https://uts.nlm.nih.gov/uts/umls/}
"dict_icd"



utils::globalVariables(c("icdmap", "dict_icd"))

