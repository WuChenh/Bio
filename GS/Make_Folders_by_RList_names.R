# Make Folders by list names
Make_Dir_by_RList_names <- function(RList=NA, wDir="~/gs/rice/_data", grepW="tt", loaded=FALSE) {
  setwd(wDir)
  if (is.na(RList)) {
    RData_path <- dir()[grep(grepW, dir())]
  }
  print(RData_path)
  for (p in RData_path) {
    setwd(wDir)
    if (!loaded) { load(p, .GlobalEnv) }
    listNam <- strsplit(p, ".RData")[[1]][1]
    RList <- .GlobalEnv[[listNam]]
    make_folder_and_setwd(listNam, TRUE)
    make_dir_recur(RList)
    if (!loaded) {
      rm(list=names(globalenv())[which(names(globalenv())==listNam)], envir = .GlobalEnv)
    }
    print(noquote(listNam))
  }
}

make_folder_and_setwd <- function(fname, isSetwd=FALSE) {
  if (file.exists(fname)) {
  } else {
    dir.create(fname)
  }
  if (isSetwd) {
    setwd(fname)
  }
}

make_dir_recur <- function(listIn) {
  if (class(listIn) == "list") {
    len <- length(listIn)
    if (len > 0) {
      # Obj names
      if (is.null(names(listIn))) {
        tmp_nam <- as.character(seq(1:len))
      } else {
        tmp_nam <- names(listIn)
      }
      # if listIn[[n]] is list
      for (n in 1:len) {
        if (class(listIn[[n]]) == "list") {
          make_folder_and_setwd(tmp_nam[n], TRUE)
          make_dir_recur(listIn[[n]])
          setwd("../")
        } else {
          write.csv(listIn[[n]], paste(tmp_nam[n], ".csv", sep = ""), quote = FALSE)
        }
      }
    }
  } else {
    return()
  }
}
