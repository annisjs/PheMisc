#' Run PheWAS
#'
#' @param codes dataset with the following columns: id, code, date, vocabulary_id, sex
#' @param genotypes the file location of the raw plink2_out file produced by run_plink
#' @param output_folder output will be stored in <output_folder>/phewas_<Sys.Date()>output_chr_<chromosome>.csv
#' @param n_cores number of cores to run on. Default is 8
#' @param additive.genotypes should genotypes be additive. Default is FALSE
#' @param subset1 File containing subset of SNPs from the plink2_out file.
#'
#' @export
#' @import data.table PheWAS DBI odbc
run_phewas <- function(codes_dat,genotypes,output_folder,n_cores=8,additive.genotypes=FALSE,subset1=NULL)
{
  date_of_run <- Sys.Date()
  for (i in 1:length(genotypes))
  {
    print(genotypes[i])
    chr <- str_split(basename(genotypes[i]),"_|[.]")[[1]][3]
    output_file <- paste0(output_folder,"/phewas_",date_of_run,"_output_chr_",chr,".csv")
    cat("\nWriting to ",output_file )
    cat("\n")
    rph(codes_dat,genotypes[i],output_file,n_cores,additive.genotypes,subset1)
  }
}

rph <- function(codes,genotypes,output_file,n_cores,additive.genotypes,subset1)
{
  genotypes <- data.table::fread(genotypes)
  genotypes <- genotypes[,.SD,.SDcols=c(1,7:ncol(genotypes))]

  if (!is.null(subset1))
  {
    print("Subset SNPS: ")
    print(subset1)
    print(paste0("Found ",length(which(colnames(genotypes) %in% subset1))," SNPs in file"))
    genotypes = genotypes[,.SD,.SDcols=c("FID",colnames(genotypes)[colnames(genotypes) %in% subset1])]
  }
  genotypes <- as.data.frame(genotypes)
  if (ncol(genotypes) > 1)
  {
    code_count <- codes[,.(count = length(date),
                           vocabulary_id = vocabulary_id[1]),by=.(id,code)]
    code_count <- code_count[,c(1,4,2,3)]
    dem <- codes[,c("id","sex")]
    dem <- dem[!duplicated(dem)]
    names(genotypes)[1] <- "id"

    #hardcall
    hardcall <- function(v){
      v[v<=0.1] <- 0
      v[v>=1.9] <- 2
      v[v>=0.9 & v<=1.1] <- 1
      v[v!=1 & v!=2 & v!=0] <- NA
      v
    }
    print("Running PheWAS")
    genotypes[,2:ncol(genotypes)] <- apply(genotypes[2:ncol(genotypes)],2,hardcall)
    gender_restriction <<- PheWAS::gender_restriction
    phenotypes <- PheWAS::createPhenotypes(code_count, aggregate.fun=sum, id.sex=dem)
    results <- PheWAS::phewas(phenotypes=phenotypes,genotypes=genotypes,significance.threshold=c("bonferroni"),cores=as.numeric(n_cores),
                      additive.genotypes = additive.genotypes)
    pheinfo <<- PheWAS::pheinfo
    results <- PheWAS::addPhecodeInfo(results)
    data.table::fwrite(results,file=output_file)
  }
}
