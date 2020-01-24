
#' Folder, file and extension parts of a file name.
#'
#' @param string input character vector
#' @return list of file parts.
#' @import stringr
#' @import jsonlite
#' @import dplyr
#' @import zoo
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
  xmat <- cbind(x,dplyr::lag(x),dplyr::lead(x))
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
  #plotly_obj_viewer not to be reset in plotlyIni(), open reports must be tracked
}

#' Generates HTML/plotly code for a new graph
#'
#' @param none.
#' @return none, just pre-allocate in the main workspace
#' @export
#' @examples
#' addGraph(list(periods, values))
#' addGraph(list(periods, values),options)
addGraph <- function(..., # Expected input: ts: list(periods,values)
                          #                 line: list(x,y), or just y
                          #                 bar:  list()
                          #                 bubble: list(x,y,[siz])
                          #                 -> list per each line
                     x=c(), # Sequence of periods,
                            # -> if not default
                            # -> if not specified inside list(periods,values)
                     type=c(), # Graph type (line/bubble/bar)
                     title="",
                     vline=c(), # Vertical bar separators (list of dates)
                     legend=c(), # List of line labels
                     xlabel="",
                     ylabel="",
                     colors=c(), # Set of line colors in 'rgba(#,#,#,#)' format
                    #markerSize="", # Slow rendering - only generated if alone data surrounded by NA exist
                    #size=6,     # Bubble size for bubble type
                     by="",     # Grouping data by existing column, used for 'bubble' type
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

    # Guess graph type if not user-supplied
    if (length(type)==0){
        guineapig <- objs[[1]]
        if (is.ts(guineapig)){
            type = "ts"

        }else if ((is.list(guineapig))){
            if (length(guineapig)==3){
                # list(x,y,siz)
                type = "bubble"
            }else if (length(guineapig)==2){
                if (is.character(guineapig[[1]])){
                    type = "ts"
                }else{
                    type = "line"
                }
           #}else if (length(guineapig)==1){ -> not needed, most likely numeric input, not a list()
            }else{
                type = "line"
            }

        }else if ((is.numeric(guineapig))){
            if (length(x)!=0){
                if (is.character(x)){
                    type = "ts"
                }else{
                    type = "line"
                }
            }else{
                type = "line"
            }

        }else if (is.data.frame(guineapig) | is.matrix(guineapig)){
            # if (length(by)!=0){
            #     # Column name (string) for group by expected here
            #     type = "bar"
            # }else
            if (length(x)!=0){
                if (is.numeric(x)){
                    type = "line"
                }else{
                    type = "ts"
                }
            }else{
                type = "line"
            }
        }
    }

    # Graph type junction
    switch(tolower(type),
        "ts"  ={graphTypeTS(grObj,grInfo,igr,
                            objs,nobjs,
                            x,title,vline,legend,xlabel,ylabel,colors,lineWidth)},
        "line"={graphTypeLine(grObj,grInfo,igr,
                            objs,nobjs,
                            x,title,vline,legend,xlabel,ylabel,colors,lineWidth)},
        "bar"={graphTypeBar(grObj,grInfo,igr,
                            objs,nobjs,
                            x,title,legend,xlabel,ylabel,colors)},
        "bubble"={graphTypeBubble(grObj,grInfo,igr,
                            objs,nobjs,
                            title,legend,xlabel,ylabel,colors,by)},
        stop("ts/line/bar/bubble are the only supported graph types...")
    )

}

#' Time series graph
#'
#' @param none
#' @return none
graphTypeTS <- function(grObj,grInfo,igr,
                        objs,nobjs,
                        x,title,vline,legend,xlabel,ylabel,colors,lineWidth){

    if (nobjs>1){ # Input has to be list(), list(), ...
                  # Or ts(), ts(), ts()

        if (is.list(objs[[1]])) {
            # User-supplied periods info
            # -> Input is: data1, data2, data3, x=periods
            # -> Need: list(list(periods,data1),list(.),list(.))
            if (length(x)!=0) {
                objsInput <- objs
                for (ii in 1:nobjs) {
                  objs[[ii]] <- vector("list",2)
                  objs[[ii]][[1]] <- x # periods
                  objs[[ii]][[2]] <- as.numeric(objsInput[[ii]][[1]])
                }
            }

        } else if (is.ts(objs[[1]])){
            objsInput <- objs
            for (ii in 1:nobjs) {
              objs[[ii]] <- vector("list",2)
              objs[[ii]][[1]] <- zoo::as.Date(time(objsInput[[ii]])) # periods
              objs[[ii]][[2]] <- as.numeric(objsInput[[ii]])
            }
        } else if (is.numeric(objs[[1]])){
            if (length(x)==0) {
                stop("x=... option is mandatory for time series graph AND input values in numeric format...")
            }
            objsInput <- objs
            for (ii in 1:nobjs) {
              objs[[ii]] <- vector("list",2)
              objs[[ii]][[1]] <- x # periods
              objs[[ii]][[2]] <- as.numeric(objsInput[[ii]]) # should be already numeric
            }
        }

    # !!!-> list(y) together with x=... case is highly unlikely,
    # y as numeric would be more natural input, if branch left here for completeness
    } else if (nobjs==1 & class(objs[[1]])[1]=="list") { # ...[1] for multi classes

        # Only values on input, periods should be separately as x=...
        if (length(objs[[1]])==1){
            if (length(x)==0){
                stop("Periods not specified for time series graph type...")
            }else{
                objsInput <- objs
                objs[[1]] <- vector("list",2)
                objs[[1]][[1]] <- x # periods
                objs[[1]][[2]] <- as.numeric(objsInput[[1]][[1]])
            }
        }

    } else if (nobjs==1 & class(objs[[1]])[1]!="list") { # Non-list input treatment

        # Input can be numeric, matrix, data frame, ts
        if (is.ts(objs[[1]])){
            periods <- zoo::as.Date(time(objs[[1]]))
            objs[[1]] <- as.matrix(objs[[1]]) # because univariate ts not in matrix format

        }else if (is.numeric(objs[[1]])){
            objs[[1]] <- as.matrix(objs[[1]])

        }else if (is.data.frame(objs[[1]])) {
            if (ncol(objs[[1]])>1){
                if (length(legend)==0){
                    legend <- colnames(objs[[1]])
                }
            } else { # Only a single line to draw => title instead
                if (title==""){
                    title <- colnames(objs[[1]]) # Only 1 obj here
                }
            }
        }

        obj <- objs[[1]]
        nobjs <- ncol(obj)
        objs <- vector("list",nobjs)
        if (length(x)>0 & !is.ts(obj)){
          periods <- x
        }
        for (ii in 1:nobjs) {
          objs[[ii]] <- vector("list",2)
          objs[[ii]][[1]] <- periods
          objs[[ii]][[2]] <- as.numeric(obj[,ii])
        }

    } # All possible cases resolution

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
    legend <- rep("Series",nobjs)
    legend <- stringr::str_replace(legend,"Series",paste0("Series ",c(1:nobjs)))
  }else if (nlgnd<nobjs) {
    stop("# of legend entries does not match # of drawn objects")
  }

  # Register all lines one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nobjs) {

    # Graph type junction
    # if (type=="line") {

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

    # }else if (type=="bubble") {
    #   objType <- "\tmode: 'markers',"
    #   lineSpec <- ""
    #   markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], size)
    # }

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

#' Line graph
#'
#' @param none
#' @return none
graphTypeLine <- function(grObj,grInfo,igr,
                          objs,nobjs,
                          x,title,vline,legend,xlabel,ylabel,colors,lineWidth){

    if (nobjs>1){ # Input has to be list(), list(), ...
                  # or c(), c(), c(), ...

        if (is.list(objs[[1]])){
            # Already in requested format if list(x,y), list(x,y)

            if (length(objs[[1]])==1){ # list(y) format (rarely used)

                # User-supplied x info
                # -> Input is: y1, y2, y3, x=x
                # -> Need: list(list(x,y1),list(.),list(.))
                if (length(x)==0) {
                    x <- c(1:length(objs[[1]][[1]]))
                }
                objsInput <- objs
                for (ii in 1:nobjs) {
                  objs[[ii]] <- vector("list",2)
                  objs[[ii]][[1]] <- x # periods
                  objs[[ii]][[2]] <- as.numeric(objsInput[[ii]][[1]])
                }

            }

        }else if (is.numeric(objs[[1]])){

            if (length(x)==0) {
                x <- c(1:length(objs[[1]]))
            }
            objsInput <- objs
            for (ii in 1:nobjs) {
              objs[[ii]] <- vector("list",2)
              objs[[ii]][[1]] <- x # periods
              objs[[ii]][[2]] <- as.numeric(objsInput[[ii]]) # should be already numeric
            }
        }
    # !!!-> list(y) together with x=... case is highly unlikely,
    # y as numeric would be more natural input, if branch left here for completeness
    } else if (nobjs==1 & class(objs[[1]])[1]=="list") { # ...[1] for multi classes

        if (length(x)==0){
            x <- c(1:length(objs[[1]][[1]]))
        }
        objsInput <- objs
        objs[[1]] <- vector("list",2)
        objs[[1]][[1]] <- x
        objs[[1]][[2]] <- as.numeric(objsInput[[1]][[length(objsInput[[1]])]])

    } else if (nobjs==1 & class(objs[[1]])[1]!="list") { # Non-list input treatment



        # Input can be numeric, matrix, data frame
        if (is.numeric(objs[[1]])){
            objs[[1]] <- as.matrix(objs[[1]])

        }else if (is.data.frame(objs[[1]])) {
            if (ncol(objs[[1]])>1){
                if (length(legend)==0){
                    legend <- colnames(objs[[1]])
                }
            } else { # Only a single line to draw => title instead
                if (title==""){
                    title <- colnames(objs[[1]]) # Only 1 obj here
                }
            }
        }

        obj <- objs[[1]]
        if (length(x)==0){
            x <- c(1:nrow(obj))
        }
        nobjs <- ncol(obj)
        objs <- vector("list",ncol(obj))
        for (ii in 1:nobjs) {
          objs[[ii]] <- vector("list",2)
          objs[[ii]][[1]] <- x
          objs[[ii]][[2]] <- as.numeric(obj[,ii])
        }

    } # All possible cases resolution

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
    legend <- rep("Series",nobjs)
    legend <- stringr::str_replace(legend,"Series",paste0("Series ",c(1:nobjs)))
  }else if (nlgnd<nobjs) {
    stop("# of legend entries does not match # of drawn objects")
  }

  # Register all lines one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nobjs) {

    # Graph type junction
    # if (type=="line") {

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

    # }else if (type=="bubble") {
    #   objType <- "\tmode: 'markers',"
    #   lineSpec <- ""
    #   markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], size)
    # }

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
                  # "\t\ttype: 'date',",
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

#' Bar graph
#'
#' @param none
#' @return none
graphTypeBar <- function(grObj,grInfo,igr,
                         objs,nobjs,
                         x,title,legend,xlabel,ylabel,colors){

    if (!is.data.frame(objs[[1]]) | nobjs>1){
        stop("A single data frame should be passed for 'bar' graphs ")
    }
    df <- objs[[1]]

    # Groupping
    # if (length(by)==0){ # No groupping, all data are contributions

        nX <- nrow(df)
        nY <- ncol(df)
        objs <- vector("list",nX)

        # Horizontal categories
        if (length(x)==0) {
            x <- rep("Cat.",nX)
            x <- stringr::str_replace(x,"Cat\\.",paste0("Cat. ",c(1:nX)))
        }

        # Legend entries
        if (length(legend)==0){
            legend <- colnames(df)
        }
        if (title=="" & nY==1){
            title <- colnames(df) # Only 1 obj here
        }

        # Populate input lists
        for (ii in 1:nY) {
          objs[[ii]] <- vector("list",2)
          objs[[ii]][[1]] <- x
          objs[[ii]][[2]] <- as.numeric(df[,ii])
        }

    # }

    # Content creation
    objChunk <- 7
    # 1   var trace1 = {
    # 2     type: "bar",
    # 3     name: 'AAPL High',
    # 4     x: unpack(rows, 'Date'),
    # 5     y: unpack(rows, 'AAPL.High'),
    # 6     [OPTIONAL] marker: {color: ['rgba(204,204,204,1)']}
    # 7   }
    JSglue <- vector("character",objChunk*nY) # 9 lines per segment

  # Obj colors
  if (length(colors)==0){

    # Default color set
    cols <- c('rgba(23,190,207,0.9)',
              'rgba(127,127,127,0.9)',
              'rgba(244,194,66,0.9)',
              'rgba(220,130,130,0.9)')

    # Generate random colors if more lines needed
    if (length(cols)<nY){
      missing_ <- nY-length(cols)
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
    if (length(colors)!=nY){
      stop(sprintf(paste0("You have %g objects and provided %g colors",
                          " - these numbers must match..."),
                    length(colors),nY))
    }
    cols <- colors
  }

  # Legend entries
  nlgnd <- length(legend)
  if (nlgnd<nY) {
    stop("# of legend entries does not match # of drawn objects")
  }

  # Register all contrubutions one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nY) {

    # Draw markers only if alone values present (for faster rendering)
    vals <- objs[[ii]][[2]]
    # lineSpec <- sprintf("\tline: {color: '%s', width: %g},", cols[ii], lineWidth)
    # if ( aloneNumber(vals) ) {
    #     objType <- "\tmode: 'lines+markers',"
        markerSpec <- sprintf("\tmarker: {color: '%s'}", cols[ii])
    # }else{
    #     objType <- "\tmode: 'lines',"
    #     markerSpec <- ""
    # }

    JSglue[start_:end_] <-
      c(sprintf("var gr_%d_trace_%d = {",igr,ii),
        "\ttype: 'bar',",
        sprintf("\tname: '%s',",legend[ii]),
        sprintf("\tx: %s,",jsonlite::toJSON(objs[[ii]][[1]])),
        sprintf("\ty: %s,",jsonlite::toJSON(vals)),
        markerSpec,
        "}")

    start_ <- start_+objChunk
    end_ <- end_+objChunk
  }
  # browser()
  # var data = [trace1,trace2,...];
  dataObjs <- rep(sprintf("gr_%d_trace_##",igr),nY)
  dataObjs <- stringr::str_replace(dataObjs,'##',as.character(c(1:nY)))
  dataObjs <- c(sprintf("var gr_%d_data = [",igr),
                c('\t',paste(dataObjs, collapse=',')),
                '];')

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
                "\tbarmode: 'relative',",
                "\txaxis: {",
                  "\t\ttype: 'category',",
                  sprintf("\t\ttitle: '%s'",xlabel),
                "\t},",
                "\tyaxis: {",
                  sprintf("\t\ttitle: '%s'",ylabel),
                "\t},",
                sprintf("\ttitle: '%s'",title),
              "};")

  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue

  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", grInfo, envir = .GlobalEnv)
}

#' Bubble graph
#'
#' @param none
#' @return none
graphTypeBubble <- function(grObj,grInfo,igr,
                         objs,nobjs,
                         title,legend,xlabel,ylabel,colors,by){

    if (!is.data.frame(objs[[1]]) | nobjs>1){
        stop("A single data frame should be passed for 'bar' graphs ")
    }
    df <- objs[[1]]

    # Grouping
    # if (length(by)==0){ # No groupping, all data are contributions

        nX <- nrow(df)
        nY <- ncol(df)
        if (by==""){

            # Input dimensions
            if (!any(nY==c(2,3))){
                stop("Bubble chart needs 2 data columns, or 3 including the bubble sizes + 4th column as a grouping variable")
            }

            objs <- vector("list",1)
            # for (ii in 1:nY) {
              objs[[1]] <- vector("list",3)
              objs[[1]][[1]] <- as.numeric(df[,1])
              objs[[1]][[2]] <- as.numeric(df[,2])
              if (nY==3){
                  sizVals <- as.numeric(df[,3])
                  sizMin <- min(sizVals)
                  sizMax <- max(sizVals)
                  if (sizMax>sizMin){
                      scaleMin <- 6
                      scaleMax <- 30
                      sizVals <- round(((sizVals-sizMin)/(sizMax-sizMin))*(scaleMax-scaleMin)+scaleMin)
                  }else{
                      sizVals <- rep(11,nX) # Default value
                  }

                  objs[[1]][[3]] <- sizVals
              }else{
                  objs[[1]][[3]] <- rep(11,nX) # Default value
              }
            # }
              ngr <- 1

        }else{
               # browser()
            # Input dimensions
            if (!any(nY==c(3,4))){
                stop("Bubble chart needs 2 data columns, or 3 including the bubble sizes + 4th column as a grouping variable")
            }

            cols <- colnames(df)
            if (!any(by==cols)){
                stop("Grouping variable does not seem to exist in the input data frame...")
            }

            # Populate lists
            groups <- unique(df[,by]) # can be factor here
            # df <- df %>% select(-by)
            nY <- nY-1
            ngr <- length(groups)
            objs <- vector("list",ngr)
            for (ii in 1:ngr) {

              segm <- df %>% filter(UQ(as.name(by))==groups[ii])  %>% select(-UQ(as.name(by)))
              nX <- nrow(segm)
              objs[[ii]] <- vector("list",2)
              objs[[ii]][[1]] <- as.numeric(segm[,1])
              objs[[ii]][[2]] <- as.numeric(segm[,2])
              if (nY==3){
                  sizVals <- as.numeric(segm[,3])
                  sizMin <- min(sizVals)
                  sizMax <- max(sizVals)
                  if (sizMax>sizMin){
                      scaleMin <- 6
                      scaleMax <- 30
                      sizVals <- round(((sizVals-sizMin)/(sizMax-sizMin))*(scaleMax-scaleMin)+scaleMin)
                  }else{
                      sizVals <- rep(11,nX) # Default value
                  }

                  objs[[ii]][[3]] <- sizVals

              }else{
                  objs[[ii]][[3]] <- rep(11,nX) # Default value

              }

            }

            # Legend entries
            if (length(legend)==0){
                legend <- as.character(groups)
            }

        }

        # # Horizontal categories
        # if (length(x)==0) {
        #     x <- rep("Cat.",nX)
        #     x <- stringr::str_replace(x,"Cat\\.",paste0("Cat. ",c(1:nX)))
        # }

    # }

    # Content creation
    objChunk <- 7
    # 1   var trace1 = {
    # 2     mode: "markers",
    # 3     name: 'AAPL High',
    # 4     x: unpack(rows, 'Date'),
    # 5     y: unpack(rows, 'AAPL.High'),
    # 6     marker: {color: ['rgb(93, 164, 214)'], size: [40, 60, 80, 100]}
    # 7   }
    JSglue <- vector("character",objChunk*ngr) # 9 lines per segment

  # Obj colors
  if (length(colors)==0){

    # Default color set
    cols <- c('rgba(23,190,207,0.9)',
              'rgba(127,127,127,0.9)',
              'rgba(244,194,66,0.9)',
              'rgba(220,130,130,0.9)')

    # Generate random colors if more lines needed
    if (length(cols)<ngr){
      missing_ <- ngr-length(cols)
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
    if (length(colors)!=ngr){
      stop(sprintf(paste0("You have %g objects and provided %g colors",
                          " - these numbers must match..."),
                    length(colors),ngr))
    }
    cols <- colors
  }

  # Legend entries
  nlgnd <- length(legend)
  if (nlgnd<ngr) {
    stop("# of legend entries does not match # of drawn objects")
  }

  # Register all contrubutions one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:ngr) {

    # Draw markers only if alone values present (for faster rendering)
    xvals <- objs[[ii]][[1]]
    yvals <- objs[[ii]][[2]]
    zvals <- objs[[ii]][[3]]

    # lineSpec <- sprintf("\tline: {color: '%s', width: %g},", cols[ii], lineWidth)
    # if ( aloneNumber(vals) ) {
    #     objType <- "\tmode: 'lines+markers',"
        markerSpec <- sprintf("\tmarker: {color: '%s', size: %s}", cols[ii], as.character(jsonlite::toJSON(zvals)))
    # }else{
    #     objType <- "\tmode: 'lines',"
    #     markerSpec <- ""
    # }

    JSglue[start_:end_] <-
      c(sprintf("var gr_%d_trace_%d = {",igr,ii),
        "\tmode: 'markers',",
        sprintf("\tname: '%s',",legend[ii]),
        sprintf("\tx: %s,",jsonlite::toJSON(xvals)),
        sprintf("\ty: %s,",jsonlite::toJSON(yvals)),
        markerSpec,
        "}")

    start_ <- start_+objChunk
    end_ <- end_+objChunk
  }
  # browser()
  # var data = [trace1,trace2,...];
  dataObjs <- rep(sprintf("gr_%d_trace_##",igr),ngr)
  dataObjs <- stringr::str_replace(dataObjs,'##',as.character(c(1:ngr)))
  dataObjs <- c(sprintf("var gr_%d_data = [",igr),
                c('\t',paste(dataObjs, collapse=',')),
                '];')

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
                "\tbarmode: 'relative',",
                "\txaxis: {",
                  # "\t\ttype: 'date',",
                  sprintf("\t\ttitle: '%s'",xlabel),
                "\t},",
                "\tyaxis: {",
                  sprintf("\t\ttitle: '%s'",ylabel),
                "\t},",
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
                          libFile="path/to/file/plotly.min.js",
                          lightWeight = F,
                          reopen = F){

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

  # Link to the plotlyJS library
  if (lightWeight==F) {
    # Create the regular folder/file structure
    # + use local plotly library

    # Library folder
    if (!dir.exists(paste0(report_folder,"/js"))){
      dir.create(paste0(report_folder,"/js"))
      if (libFile!="path/to/file/plotly.min.js"){
        localLibName <- fileparts(libFile)[4]
        file.copy(libFile,
           paste0(report_folder,"/js/",localLibName))
      }else{
        download.file("https://cdn.plot.ly/plotly-latest.min.js",
               paste0(report_folder,"/js/plotly-latest.min.js"))
      }
    }

    # Pick the JS library file from the folder contents
    contents <- list.files(paste0(report_folder,"/js"))
    JScontents <- stringr::str_match(contents,'plotly.*?\\.js')
    pickedFile <- JScontents[!is.na(JScontents)][1]
    libLink <- paste0("<script src=\"js/",pickedFile,"\"></script>")

  }else{ # light-weight version (single HTML output file)
    libLink <- "<script src=\"https://cdn.plot.ly/plotly-latest.min.js\"></script>"
  }

  # Rewrite the report file from scratch
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

  # Console info
  if (lightWeight==F) {
    cat(" ----> Report created in fast mode...\n")
    cat("       ",reportFile,'\n')

  }else{
    cat(" ----> Lightweight HTML report created...\n")
    cat("       ",reportFile,'\n')

  }

  # Open up the report in a web browser only if not yet opened
  openReport(reportFile, reopen)
  
}

#' Automatic report opening
#'
#' @param none
#' @return none
openReport <- function(reportFile, reopen){

  # Open up the report in browser only if not done previously
  if (!any("plotly_obj_viewer"==ls(envir = .GlobalEnv))){

      # Not everyone uses Rstudio
      if (Sys.getenv("RSTUDIO")=="1"){
        rstudioapi::viewer(reportFile)
      }

      # Mark the current report file as already opened
      assign("plotly_obj_viewer", reportFile, envir = .GlobalEnv)

  }else{
     # Opened reports
     reports  <- get('plotly_obj_viewer', envir = .GlobalEnv)

     # Current report not yet opened
     if (reopen==T || !any(reportFile==reports)){

         # Not everyone uses Rstudio
         if (Sys.getenv("RSTUDIO")=="1"){
            rstudioapi::viewer(reportFile)
         }

         reports <- c(reports,reportFile)
         assign("plotly_obj_viewer", reports, envir = .GlobalEnv)
     }
  }

}

#' Compilation of HTML/plotly document
#'
#' @param numeric.
#' @return none, modifies the given HTML template file.
#' @export
#' @examples
#' figure(c(1:5))
figure <- function(d, reportFile = "tmp.html",
                      reopen = F) {

  plotlyIni()
  addGraph(d)
  plotlyCompile(reportFile=reportFile, 
                reopen=reopen)

}

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
  system('git commit -a -m "Autopush after a fix"')
  system('git push')
  
  # Re-install from github
  if ("plotlyjs" %in% list.files(.libPaths()[1])) {
    remove.packages("plotlyjs")
  }
  devtools::install_github("lazypomelo/plotlyjs")
  
  print("DONE... library(plotlyjs) should now work!")
}
  
  