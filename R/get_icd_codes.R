#' Get ICD codes for PheWAS
#'
#' @param table databricks table that has grid, code, date, vocabulary_id, sex as columns
#'
#' @export
#' @import DBI odbc stringr
get_icd_codes <- function(table="workspace_victr.phenotemplate_sd.all_mega_icd_codes_full")
{
  conn = dbConnect(drv = odbc(), dsn = "Databricks")
  codes <- as.data.table(dbGetQuery(conn,str_glue("SELECT * FROM {table}")))
  setnames(codes,"grid","id")
  setnames(codes,"all_icds_code","code")
  setnames(codes,"all_icds_date","date")
  codes<- codes[!duplicated(codes)]
  return(codes)
}
