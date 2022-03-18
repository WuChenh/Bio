# Make Folders by list names
Make_Dir_by_RList_names <- function(RList=NA, wDir="~/gs/rice/_data", grepW="tt", loaded=FALSE) {
  setwd(wDir)
  if (is.na(RList)) {
    RData_path <- dir()[grep(".RData", dir())]
    RData_path <- RData_path[grep(grepW, RData_path)]
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
  setwd(wDir)
}
# Make Files by list names
Make_File_by_RList_names <- function(RList=NA, wDir="~/gs/rice/_data", grepW="tt", loaded=FALSE) {
  setwd(wDir)
  if (is.na(RList)) {
    RData_path <- dir()[grep(".RData", dir())]
    RData_path <- RData_path[grep(grepW, RData_path)]
  }
  print(RData_path)
  for (p in RData_path) {
    setwd(wDir)
    if (!loaded) { load(p, .GlobalEnv) }
    listNam <- strsplit(p, ".RData")[[1]][1]
    RList <- .GlobalEnv[[listNam]]
    make_folder_and_setwd(paste(listNam, "_s", sep = ""), TRUE)
    make_file_recur(RList)
    if (!loaded) {
      rm(list=names(globalenv())[which(names(globalenv())==listNam)], envir = .GlobalEnv)
    }
    print(noquote(listNam))
  }
  setwd(wDir)
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

make_file_recur <- function(listIn, fileName='') {
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
      #fileName_o <- fileName
      for (n in 1:len) {
        if (class(listIn[[n]]) == "list") {
          fileName_o <- paste(c(fileName, "_", tmp_nam[n]), collapse = "")
          make_file_recur(listIn[[n]], fileName_o)
        } else {
          write.csv(listIn[[n]], paste(c(fileName, "_", tmp_nam[n], ".csv"), collapse = ""), quote = FALSE)
        }
      }
    }
  } else {
    return()
  }
}
