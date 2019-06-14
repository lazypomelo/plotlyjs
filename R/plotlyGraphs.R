
#' Folder, file and extension parts of a file name.
#' 
#' @param string input character vector
#' @return list of file parts.
#' @import stringr
#' @import jsonlite
#' @examples
#' fileparts("filePath")
fileparts <- function(filepath){
  parts <- stringr::str_split(filepath,'/')
  l <- length(parts[[1]])
  if ( l==1 ){
    nameExt <- parts[[1]]
    name <- stringr::str_replace(nameExt,'\\.\\w+$','')
    ext <- stringr::str_match(nameExt,'(?<=\\.)\\w+$')
    path <- '' # getwd() more suitable for local apps, but '' for relative paths needed
  }else{
    nameExt <- parts[[1]][l]
    name <- stringr::str_replace(nameExt,'\\.\\w+$','')
    ext <- stringr::str_match(nameExt,'(?<=\\.)\\w+$')
    path <- paste(parts[[1]][1:l-1],collapse='/')  
  }
  return(c(path,name,ext,nameExt))
}

#' Detection of alone numbers preceded/followed by NAs
#'
#' @param string input character vector
#' @return true/false if an orphan found.
#' @examples
#' aloneNumber(c(1,2,3,NA,99,NA))
aloneNumber <- function(x) {
  # Returns T for x <- c(1,2,3,NA,99,NA); -> alone 99
  xmat <- cbind(x,lag(x),lead(x))
  return( any(rowSums(is.na(xmat))==2 & !is.na(xmat[,1])) )
}

#' Initializer for plotly graphs (deletes all preceding graphs)
#'
#' @param none.
#' @return none, just pre-allocate in the main workspace
#' @export
#' @examples
#' plotlyIni()
plotlyIni <- function() {
  
  # Plotly containers in global workspace
  preallocateBuffer <- 200
  assign("plotly_obj_graphs", vector("list",preallocateBuffer), envir = .GlobalEnv)
  assign("plotly_obj_info", list("buffer"=preallocateBuffer, "ngraphs"=0), envir = .GlobalEnv)

}

#' Generates HTML/plotly code for a new graph
#'
#' @param none.
#' @return none, just pre-allocate in the main workspace
#' @export
#' @examples
#' addGraph(list(periods, values))
#' addGraph(list(periods, values),options)
addGraph <- function(..., # Expected input: list(periods,values)
                          #                 -> list per each line
                     x=c(), # Sequence of periods, 
                            # -> if not default
                            # -> if not specified inside list(periods,values)
                     type="line", # Graph type (line/bubble/bar)
                     title="", 
                     vline=c(), # Vertical bar separators (list of dates)
                     legend=c(), # List of line labels
                     xlabel="",
                     ylabel="",
                     colors=c(), # Set of line colors in 'rgba(#,#,#,#)' format
                    #markerSize="", # Slow rendering - only generated fo NA values
                     size=6,     # Bubble size for bubble type
                     lineWidth=2){

  # Fetch graph container
  if (!exists('plotly_obj_graphs', envir = .GlobalEnv)) {
    stop("Initialization failed: Run plotlyIni() first...")
  }
  grObj  <- get('plotly_obj_graphs', envir=.GlobalEnv)
  grInfo <- get('plotly_obj_info', envir=.GlobalEnv)
  
  # Preallocate if needed
  if ( !any(as.logical(lapply(t, is.null))) ) {
    grObj <- c(grObj, vector("list",grInfo$buffer))
  }
  
  # Current graph id
  grInfo$ngraphs <- grInfo$ngraphs+1
  igr <- grInfo$ngraphs
  
  # Input objects
  objs <- list(...)
  nobjs <- length(objs)
  # browser()
  if (nobjs==1 & class(objs[[1]])!="list") {
    # Input can be numeric, matrix, data frame
    if (is.numeric(objs[[1]])){
      objs[[1]] <- as.matrix(objs[[1]]) 
    }
    obj <- objs[[1]]
    nobjs <- ncol(obj)
    objs <- vector("list",ncol(obj))
    if (length(x)>0){
      periods <- x 
    }else{
      periods <- c(1:nrow(obj))+2000 # Start period is 2001 year
    }
    for (ii in 1:nobjs) {
      objs[[ii]] <- vector("list",2)
      objs[[ii]][[1]] <- periods
      objs[[ii]][[2]] <- as.numeric(obj[,ii])
    }
  }
  
  # Content creation
  objChunk <- 9
  # 1   var trace1 = {
  # 2     type: "scatter",
  # 3     mode: "lines", mode: "lines+markers", mode: "markers",
  # 4     name: 'AAPL High',
  # 5     x: unpack(rows, 'Date'),
  # 6     y: unpack(rows, 'AAPL.High'),
  # 7     [OPTIONAL] line: {color: '#17BECF', width: 2},
  # 8     [OPTIONAL] marker: {color: 'rgb(128, 0, 128)', size: 8}
  # 9   }
  JSglue <- vector("character",objChunk*nobjs) # 9 lines per segment
  
  # Obj colors
  if (length(colors)==0){
    
    # Default color set
    cols <- c('rgba(23,190,207,0.9)',
              'rgba(127,127,127,0.9)',
              'rgba(244,194,66,0.9)',
              'rgba(220,130,130,0.9)') 
    
    # Generate random colors if more lines needed
    if (length(cols)<nobjs){
      missing_ <- nobjs-length(cols)
      randCols <- stringr::str_replace("rgba(#,#,#,0.9)",
                                       "#",
                                       as.character(floor(runif(missing_)*255)))
      randCols <- stringr::str_replace(randCols,
                                       "#",
                                       as.character(floor(runif(missing_)*255)))
      randCols <- stringr::str_replace(randCols,
                                       "#",
                                       as.character(floor(runif(missing_)*255)))
      cols <- c(cols,randCols)
    }
    
  }else{
    if (length(colors)!=nobjs){
      stop(sprintf(paste0("You have %g objects and provided %g colors",
                          " - these numbers must match..."),
                    length(colors),nobjs))
    }
    cols <- colors
  }

  # Legend entries
  nlgnd <- length(legend)
  if (nlgnd==0) {
    legend <- rep("Line",nobjs)
    legend <- stringr::str_replace(legend,"Line",paste0("Line",c(1:nobjs)))
  }else if (nlgnd<nobjs) {
    stop("# of legend entries does not match # of drawn objects")
  }
  
  # Register all lines one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nobjs) {
    
    # Graph type junction
    if (type=="line") {
      
      # Draw markers only if alone values present (for faster rendering)
      vals <- objs[[ii]][[2]]
      lineSpec <- sprintf("\tline: {color: '%s', width: %g},", cols[ii], lineWidth)
      if ( aloneNumber(vals) ) {
        objType <- "\tmode: 'lines+markers',"
        markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], lineWidth) # Marker size matches lineWidth
      }else{
        objType <- "\tmode: 'lines',"
        markerSpec <- ""
      }
      
    }else if (type=="bubble") {
      objType <- "\tmode: 'markers',"
      lineSpec <- ""
      markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], size)
    }
    
    JSglue[start_:end_] <- 
      c(sprintf("var gr_%d_trace_%d = {",igr,ii),
        "\ttype: 'scatter',",
        objType,
        sprintf("\tname: '%s',",legend[ii]),
        sprintf("\tx: %s,",jsonlite::toJSON(objs[[ii]][[1]])),
        sprintf("\ty: %s,",jsonlite::toJSON(vals)),
        lineSpec,
        markerSpec,
        "}")
   
    start_ <- start_+objChunk
    end_ <- end_+objChunk
  }
  # browser()
  # var data = [trace1,trace2,...];
  dataObjs <- rep(sprintf("gr_%d_trace_##",igr),nobjs)
  dataObjs <- stringr::str_replace(dataObjs,'##',as.character(c(1:nobjs)))
  dataObjs <- c(sprintf("var gr_%d_data = [",igr),
                c('\t',paste(dataObjs, collapse=',')),
                '];')
  
  # Add optional vertical separators
  #   shapes: [{
  #     type: 'line',
  #     x0: '2000-01-11',
  #     y0: 0,
  #     x1: '2000-01-11',
  #     yref: 'paper',
  #     y1: 1,
  #     line: {
  #       color: 'grey',
  #       width: 1.5,
  #       dash: 'dot'
  #     }
  #   },{
  #     ...
  #   }],
  if (length(vline)!=0){
    #gmin <- as.character( min(rapply(objs,min)[seq(2,2*nobjs,by=2)], na.rm=T) )
    #gmax <- as.character( max(rapply(objs,max)[seq(2,2*nobjs,by=2)], na.rm=T) )
    # yref: 'paper' wants relative min/max
    sh <- "\tshapes: ["
    for (ii in 1:length(vline)){
      sh <- c(sh,'\t{',
              "\t\ttype: 'line',",
              sprintf("\t\tx0: '%s',",vline[ii]),
              sprintf("\t\ty0: %s,","0"), #gmin),
              sprintf("\t\tx1: '%s',",vline[ii]),
              "\t\tyref: 'paper',",
              sprintf("\t\ty1: %s,","1"),#gmax),
              "\t\tline: {",
              "\t\t\tcolor: 'grey',",
              "\t\t\twidth: 1.5,",
              "\t\t\tdash: 'dot'",
              "\t\t}",
              "\t}")
      if (ii<length(vline)){
        sh[length(sh)] <- paste0(sh[length(sh)],',') 
      }else{
        sh <- c(sh,'],')
      }
    }
  }else{
    sh <- "" 
  }
  
  # var layout = {
  #   xaxis: {
  #     type: 'date',
  #     title: 'January Weather'
  #   },
  #   yaxis: {
  #     title: 'Daily Mean Temperature'
  #   },
  #   shapes: [{...},{...},...],
  #   title: 'Some title'
  # };
  layout <- c("var layout = {",
                "\txaxis: {",
                  "\t\ttype: 'date',",
                  sprintf("\t\ttitle: '%s'",xlabel),
                "\t},",
                "\tyaxis: {",
                  sprintf("\t\ttitle: '%s'",ylabel),
                "\t},",
                sh,
                sprintf("\ttitle: '%s'",title),
              "};")
  
  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue
  
  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", grInfo, envir = .GlobalEnv)
    
}

#' Compilation of HTML/plotly document
#'
#' @param all optional.
#' @return none, modifies the given HTML template file.
#' @export
#' @examples
#' plotlyCompile()
plotlyCompile <- function(reportFile="tmp.html",
                          libFile="model_exploration/plotly-v1.47.3.min.js",
                          lightWeight = F){

  # Fetch graph container
  if (!exists('plotly_obj_graphs', envir = .GlobalEnv)) {
    stop("Report compilation failed: Run plotlyIni() first, then add some graphs using addGraph()...")
  }
  grObj  <- get('plotly_obj_graphs', envir=.GlobalEnv)
  grInfo <- get('plotly_obj_info', envir=.GlobalEnv)
  
  grObj <- grObj[1:grInfo$ngraphs]
  
  # Generate HTML div tags
  divs <- vector("character",grInfo$ngraphs)
  divs <- stringr::str_replace("<div id='#'></div>",
                               "#",
                               paste0('gr',c(1:grInfo$ngraphs)))

  # Parse input file name
  out <- fileparts(reportFile)
  report_folder <- out[1]
  fileName <- out[2]
  if (report_folder==''){
    report_folder = getwd() 
  }
  if (!dir.exists(report_folder)){
    dir.create(report_folder)
  }
  
  if (lightWeight==F) { 
    # Create the regular folder/file structure
    # + use local plotly library

    # Library folder
    if (!dir.exists(paste0(report_folder,"/js"))){
      dir.create(paste0(report_folder,"/js"))
      file.copy(libFile,
                paste0(report_folder,"/js/plotly-v1.47.3.min.js"))
    }
    # JSfile <- paste0(report_folder,"/js/",fileName,"_data.js") 
    # JSfileInject <- paste0("js/",fileName,"_data.js") # Relative reference here
    
    libLink <- "<script src=\"js/plotly-v1.47.3.min.js\"></script>"
    
  }else{ # light-weight version (single HTML output file)
    libLink <- "<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
    
  }
  
  # Rewrite the report file from scratch
  # divStr <- rep("\t<div id='gr'></div>",prealloc)
  # divStr <- stringr::str_replace(divStr,"gr",paste0("gr",c(1:prealloc)))
  fileConn<-file(reportFile)
  writeLines(c("<html>",
               "\t<head>",
               paste0("\t\t",libLink),
               "\t</head>",
               "<body>",
               "\t<style>",
               "\t\tdiv{max-width: 600px; float: left}",
               "\t</style>",
               paste0("<p>Report creation timestamp: ",Sys.time(),"</p>\n"),
               divs, #"\t<!-- content here -->", # divStr, # 
               "</body>",
              #paste0("\t<script src=\"",JSfileInject,"\"></script>"),
               "<script>",
               unlist(grObj),
               "</script>",
               "</html>"), fileConn)
  close(fileConn)
  
  # Create an empty data file, or clear exising data file
  # fileConn<-file(JSfile)
  # writeLines(c("// Report data"), fileConn)
  # writeLines(unlist(grObj), fileConn)
  # close(fileConn)
  
  if (lightWeight==F) { 
    cat(" ----> Report created in fast mode...\n")
    cat("       ",reportFile,'\n')
    
  }else{ 
    cat(" ----> Lightweight HTML report created...\n")
    cat("       ",reportFile,'\n')
    
  }
  
}

#' Compilation of HTML/plotly document
#'
#' @param numeric.
#' @return none, modifies the given HTML template file.
#' @export
#' @examples
#' figure(c(1:5))
figure <- function(d,x=c()) {
  if (length(x)==0){
    
  }
  plotlyIni()
  addGraph(d)
  plotlyCompile(reportFile="tmp.html")
}