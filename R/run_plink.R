#' Run Plink
#'
#' @param range_file a text file specifying the chromosome, start, end, name values
#' @param output_folder plink log and raw will be output to <output_folder>/plink2_out
#' @param plink2_input_folder imputed mega data location. Default is "/scratch/mega_data/imputed_mega_data/"
#' @param maf filters out all variants with minor allele frequency below the provided threshold (default 0.00005)
#' @param max_maf_val imposes an upper MAF bound
#'
#' @importFrom stringr str_glue
#'
#' @export
run_plink <- function(range_file,output_folder,plink2_input_folder=NULL,maf=NULL,max_maf_val=NULL)
{
  if (is.null(maf))
  {
    maf <- 0.00005
  }
  if (is.null(plink2_input_folder))
  {
    plink2_input_folder <- "/scratch/mega_data/imputed_mega_data/"
  }
  output <- paste0(output_folder,"/plink2_out")
  dat <- read.delim(range_file,FALSE," ")
  colnames(dat) <- c("Chr","Start","End","Name")
  for (chrn in unique(dat$Chr))
  {
    rp(chrn,range_file,plink2_input_folder,output,maf,max_maf_val)
  }
}

rp <- function(chrn,range_file,plink2_input_folder,output,maf,max_maf_val)
{


  if (!file.exists(str_glue("{plink2_input_folder}/chr{chrn}_1000G.pgen")))
  {
    pgen <- str_glue("zstd -d {plink2_input_folder}/chr{chrn}_1000G.pgen.zst")
    system(pgen)
  }

  if (!file.exists(str_glue("{plink2_input_folder}/chr{chrn}_1000G.pvar")))
  {
    pvar <-  str_glue("zstd -d {plink2_input_folder}/chr{chrn}_1000G.pvar.zst")
    system(pvar)
  }

  if (!file.exists(str_glue("{plink2_input_folder}/chr{chrn}_1000G.psam")))
  {
    psam <-  str_glue("zstd -d {plink2_input_folder}/chr{chrn}_1000G.psam.zst")
    system(psam)
  }

  if (is.null(max_maf_val))
  {
    plink2_cmds <- str_glue("plink2 --export A --pfile {plink2_input_folder}/chr{chrn}_1000G --extract range {range_file} --maf {maf} --out {output}_{chrn}")
  } else {
    plink2_cmds <- str_glue("plink2 --export A --pfile {plink2_input_folder}/chr{chrn}_1000G --extract range {range_file} --maf {maf} --max-maf {max_maf_val} --out {output}_{chrn}")
  }

  system(plink2_cmds)

  print("Created the following files:")
  system(str_glue("rm {plink2_input_folder}/chr{chrn}_1000G.pgen"))
  system(str_glue("rm {plink2_input_folder}/chr{chrn}_1000G.pvar"))
  system(str_glue("rm {plink2_input_folder}/chr{chrn}_1000G.psam"))
}



