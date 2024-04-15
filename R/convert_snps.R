#' Convert SNPs to ranges
#'
#' @param snps list of SNPs. One per line.
#' @param output_file Output file of ranges
#'
#' @export
#' @import xml2 stringr
convert_snps <- function(snps,output_file=NULL,sleep_time=1)
{
  if (length(snps) == 1)
  {
    if (file.exists(snps))
    {
        snps <- readLines(snps)
    }
  }
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
  df <- df[,c("chr","pos","pos","snp_id")]
  if (!is.null(output_file))
  {
    data.table::fwrite(df,output_file,sep = " ",col.names = F)
  } else {
    return(df)
  }
}
