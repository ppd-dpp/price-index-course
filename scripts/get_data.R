evalq(
  for(f in files){
    tryCatch({
      name <- f
      while(name %in% ls(envir = .GlobalEnv)){
        name <- paste(name, "copy", sep = "_")
      }
      assign(name, read.csv(paste0(root, f, ".csv"), stringsAsFactors = FALSE), envir = .GlobalEnv)
    },
    condition = function(e) message("Something went wrong loading ", f, ".", " Please contact the instructor.")
    )
  },
  envir = list(files = c("dat_gps", "dat_micro", "weights", "index_crea", "index_mw"),
               root = "https://raw.githubusercontent.com/ppd-dpp/price-index-course/master/csv/")
)

