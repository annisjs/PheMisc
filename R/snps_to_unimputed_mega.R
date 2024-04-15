#' Convert SNPs to unimputed MEGA call names
#'
#' @param snps list of SNPs. One per line.
#' @param output_csv Output csv with full info
#' @param output_txt Simple text file with new SNPs to be used with Plink
#'
#' @export
#' @import data.table stringr
snps_to_unimputed_mega <- function(snps,output_csv,output_txt,
                  bim_file = "/scratch/mega_data/unimputed_mega_data/MEGAv1-1_ALL.bim",
                  sleep_time = 1)
{
  dat <- convert_snps(snps,sleep_time = sleep_time)
  dat <- as.data.table(dat)
  
  bim <- data.table::fread(bim_file)
  pos <- dat[,2]
  
  mega_snps <- NULL
  for (i in 1:nrow(pos))
  {
    temp <- bim[V4 == pos[i][[1]]]
    temp$original_snp <- dat[i,"snp_id"][[1]]
    mega_snps <- rbind(mega_snps,temp)
  }
  mega_snps <- mega_snps[!duplicated(V4)]
  
  data.table::fwrite(mega_snps,output_csv)
  snps <- mega_snps$V2
  writeLines(snps,output_txt)
}