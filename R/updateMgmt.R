#' Update functionality locally
#'
#' @param none
#' @return none
#' @export
download2fix <- function(
    repoClone = "C:/jaac/WORK/R utils/plotly/plotlyjs",
    localPlayground = "C:/jaac/WORK/R utils/plotly"){
  
  # Update local repo
  path_was <- getwd()
  setwd(repoClone)
  system("git pull")
  setwd(path_was)
  
  # Fetch files locally
  files2fetch <- list.files(paste0(repoClone,"/R"))
  repoFiles <- paste0(repoClone,"/R/",files2fetch) #plotlyGraphs.R")
  localFiles <- paste0(localPlayground,"/", files2fetch) #plotlyGraphs.R")
  
  # Copy fresh code version into playground
  file.copy(from = repoFiles, 
            to   = localFiles,
            overwrite = T)
  
  # Break link to library
  if ("package:plotlyjs" %in% search()) {
    detach("package:plotlyjs", unload=TRUE)
  }
  
  # Establish link to playground
  sapply(localFiles, source)
  
  print(paste0("!!! You are now sourced to files in ",localPlayground))
  print("!!! Before playing change the working directory where tmp.html")
  print("    can be sitting together with generated 'js' folder")
  print("!!! Start e.g. with View(figure) to set browser() and debug")
  print("!!! use uploadFixed() when done...")

}

#' Update functionality locally (export not needed for this fcn!)
#'
#' @param none
#' @return none
uploadFixed <- function(
    repoClone = "C:/jaac/WORK/R utils/plotly/plotlyjs",
    localPlayground = "C:/jaac/WORK/R utils/plotly"){
  
  # Export files back to local repo
  # -> All .R files get exported, make sure 
  # -> you do not have temp .R files in export folder!!!
  files2export <- list.files(paste0(localPlayground))
  files2export <- files2export[stringr::str_detect(files2export, '\\.R')]
  repoFiles <- paste0(repoClone,"/R/",files2export) #plotlyGraphs.R")
  localFiles <- paste0(localPlayground,"/", files2export) #plotlyGraphs.R")
  
  # Dependencies
  resp <- readline("Do you have new library() dependencies? [y/n]")
  if (resp=='n'){
    
    # Copy fresh code version into playground
    file.copy(from = localFiles, 
              to   = repoFiles,
              overwrite = T)
    
    print("--> DONE... Now pushing to github")
    
    # Re-run document()
    path_was <- getwd()
    setwd(repoClone)
    devtools::document()
    
    # Push results to github
    push2github(repoClone)
    setwd(path_was)
    
  }else if(resp=='y'){
    
    resp2 <- readline("Did you mark all dependencies at the beginning of .R files? [y/n]")
    if (resp2=='n'){
      print("...do it and try again.")
      
    }else if (resp2=='y'){
      print("--- Now you need to list new dependencies in DESCRIPTION file...")
      print(paste0("--- This is the file: ",repoClone,"/DESCRIPTION"))
      shell.exec(paste0(repoClone,"/DESCRIPTION"))
      resp3 <- readline("Are you finished editing DESCRIPTION file? [y]")
      if (resp3=='y'){
        
        # Copy fresh code version into playground
        file.copy(from = localFiles, 
                  to   = repoFiles,
                  overwrite = T)
        
        # Go to repo folder and update documentation
        path_was <- getwd()
        setwd(repoClone)
        devtools::document()
        print("--> DONE... Now pushing to github")
        
        # Push results to github
        push2github(repoClone)
        
        setwd(path_was)
        
      }else{
        print("...Quitting function, 'y' wanted as answer, try again.")
      }
    }else{
      print("...Quitting function, y/n wanted as answer, try again.")
    }
  }else{
    print("...Quitting function, y/n wanted as answer, try again.")
  }
  
}

#' Update functionality locally (export not needed for this fcn!)
#'
#' @param none
#' @return none
push2github <- function(repoClone){
  system('git add *')
  system('git commit -a -m "Autopush after a fix"')
  system('git push')
  
  # Re-install from github
  if ("plotlyjs" %in% list.files(.libPaths()[1])) {
    remove.packages("plotlyjs")
  }
  devtools::install_github("lazypomelo/plotlyjs")
  
  print("DONE... library(plotlyjs) should now work!")
}

#<eof>