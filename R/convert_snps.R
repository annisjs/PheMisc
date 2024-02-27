#' Convert SNPs to ranges
#'
#' @param snps list of SNPs. One per line.
#' @param output_file Output file of ranges
#'
#' @export
#' @import xml2 stringr
convert_snps <- function(snp_file,output_file,sleep_time)
{
  snps <- readLines(snp_file)
  url <- stringr::str_glue("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=SNP&id={snps}")
  df <- NULL
  for (i in 1:length(snps)) {
    result <- xml2::read_xml(url[i])
    result <- xml2::as_list(result)
    out <- tryCatch(
      {
        assm_pos <- strsplit(result$DocumentSummary$CHRPOS_PREV_ASSM[[1]],"[:]")[[1]]
      },
      error=function(cond) {
        message(cond)
        message("\n",url[i])
        return(NULL)
      })
    if(!is.null(out))
    {
      chr <- assm_pos[1]
      pos <- assm_pos[2]
      df <- rbind(df,c(snp_id = result$DocumentSummary$SNP_ID[[1]], pos = pos, chr = chr))
      Sys.sleep(sleep_time)
      cat(i,"/",length(snps),"\n")
    }
  }
  df <- as.data.frame(df)
  data.table::fwrite(df[,c("chr","pos","pos","snp_id")],output_file,sep = " ",col.names = F)
}
