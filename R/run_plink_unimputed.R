#' Run Plink using unimputed data
#'
#' @param snp_file a text file specifying the SNPs to be extracted
#' @param output_folder plink log and raw will be output to <output_folder>/plink2_out
#' @param plink2_input_folder imputed mega data location. Default is "/scratch/mega_data/imputed_mega_data/"
#' @param maf filters out all variants with minor allele frequency below the provided threshold (default 0.00005)
#' @param max_maf_val imposes an upper MAF bound
#'
#' @importFrom stringr str_glue
#'
#' @export
run_plink_unimputed <- function(snp_file,output_folder,bfile="/scratch/mega_data/unimputed_mega_data/MEGAv1-1_ALL",run_in_curent_dir=F)
{
  output_file_name <- strsplit(basename(snp_file),"[.]")[[1]][1]
  if (run_in_curent_dir)
  {
    cmd <- str_glue("./plink --recodeA --bfile {bfile} --out {output_folder}/{output_file_name} --extract {snp_file} -d !")
  } else {
    cmd <- str_glue("plink --recodeA --bfile {bfile} --out {output_folder}/{output_file_name} --extract {snp_file} -d !")
  }
  system(cmd)
}

