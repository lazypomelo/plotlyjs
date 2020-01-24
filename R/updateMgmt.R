#' Update functionality locally
#'
#' @param none
#' @return none
#' @export
download2fix <- function(
    repoClone = "C:/jaac/WORK/R utils/plotly/plotlyjs",
    localPlayground = "C:/jaac/WORK/R utils/plotly"){
  
  repoFile <- paste0(repoClone,"/R/plotlyGraphs.R")
  localFile <- paste0(localPlayground,"/plotlyGraphs.R")
  
  # Update local repo
  path_was <- getwd()
  setwd(repoClone)
  system("git pull")
  setwd(path_was)
  
  # Copy fresh code version into playground
  file.copy(from = repoFile, 
            to   = localFile,
            overwrite = T)
  
  # Break link to library
  if ("package:plotlyjs" %in% search()) {
    detach("package:plotlyjs", unload=TRUE)
  }
  
  # Establish link to playground
  source(localFile)
  
  print(paste0("!!! [1] Resourcing: source('",localFile,"')"))
  print("!!! [0] Start e.g. with View(figure) to set browser()")
  print("!!! [1] use uploadFixed() when done...")

}

#' Update functionality locally (export not needed for this fcn!)
#'
#' @param none
#' @return none
uploadFixed <- function(
    repoClone = "C:/jaac/WORK/R utils/plotly/plotlyjs",
    localPlayground = "C:/jaac/WORK/R utils/plotly"){
  
  repoFile <- paste0(repoClone,"/R/plotlyGraphs.R")
  localFile <- paste0(localPlayground,"/plotlyGraphs.R")
  
  # Dependencies
  resp <- readline("Do you have new library() dependencies? [y/n]")
  if (resp=='n'){
    
    # ONLY copy fresh code version into playground
    file.copy(from = localFile, 
              to   = repoFile,
              overwrite = T)
    
    print("--> DONE... Now pushing to github")
    
    # Push results to github
    path_was <- getwd()
    setwd(repoClone)
    devtools::document()
    push2github(repoClone)
    setwd(path_was)
    
  }else if(resp=='y'){
    
    resp2 <- readline("Did you mark all dependencies at the beginning of each function definition? [y/n]")
    if (resp2=='n'){
      print("...do it and try again.")
      
    }else if (resp2=='y'){
      print("--- Now you need to list new dependencies in DESCRIPTION file...")
      print(paste0("--- This is the file: ",repoClone,"/DESCRIPTION"))
      shell.exec(paste0(repoClone,"/DESCRIPTION"))
      resp3 <- readline("Are you finished editing DESCRIPTION file? [y]")
      if (resp3=='y'){
        
        # Copy fresh code version into playground
        file.copy(from = localFile, 
                  to   = repoFile,
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
  system('git add -a')
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