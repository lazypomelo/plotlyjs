
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
#' @export
#' @examples
#' aloneNumber(c(1,2,3,NA,99,NA))
aloneNumber <- function(x) {
  # Returns T for x <- c(1,2,3,NA,99,NA); -> alone 99
  xmat <- cbind(x,dplyr::lag(x),dplyr::lead(x))
  return( any(rowSums(is.na(xmat))==2 & !is.na(xmat[,1])) )
}

#' Extend pre-allocated arrays if needed
#'
#' @param none
#' @return extended array
addBuffer <- function(x, objInfo) {
    if ( !is.null(x[[length(x)]]) ) {
        x <- c(x, vector("list",objInfo$buffer))
    }
    return(x)
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
  assign("plotly_obj_paragraphs", vector("list",preallocateBuffer), envir = .GlobalEnv)
  assign("plotly_obj_info", list("buffer" = preallocateBuffer,
                                 "maxGraphs" = 1e+05,
                                 "ngraphs" = 0,
                                 "nparagraphs" = 0), envir = .GlobalEnv)
  #plotly_obj_viewer not to be reset in plotlyIni(), open reports must be tracked
  assign("plotly_obj_graphs_sty", list(grID=c(), style=c()), envir = .GlobalEnv)
  assign("plotly_obj_order", vector("numeric",preallocateBuffer), envir = .GlobalEnv)
}

#' Generates HTML text paragraphs
#'
#' @param none.
#' @return none, just pre-allocate in the main workspace
#' @export
#' @examples
#' addText("<h1 style="...">Heading</h1>","<p>Content</p>", list(bullets, style=c("...","...")), style="<for div>")
addText <- function(..., clear = T, style=""){
    # Syntax:
    # addText("<h1 style="...">Heading</h1>",
    #         "<p style="...">Content</p>",
    #         list(bullets, style=c("...","...")), 
    #         style="<for div>")

    # Global containers
    if (!exists('plotly_obj_info', envir = .GlobalEnv)) {
        stop("Initialization failed: Run plotlyIni() first...")
    }
    txtObj  <- get('plotly_obj_paragraphs', envir=.GlobalEnv)
    objInfo <- get('plotly_obj_info', envir=.GlobalEnv)
    objOrd <- get('plotly_obj_order', envir=.GlobalEnv)

    # Preallocate if needed
    txtObj <- addBuffer(txtObj, objInfo)
    objOrd <- addBuffer(objOrd, objInfo)
    
    # Current graph id
    objInfo$nparagraphs <- objInfo$nparagraphs+1
    igr <- objInfo$nparagraphs
    objOrd[igr+objInfo$ngraphs] <- 
        igr + objInfo$maxGraphs # to distinguish ordering of graphs/text
    
    # Input objects
    objs <- list(...)
    nobjs <- length(objs)  
    
    # Content glued
    buffer_ <- 10
    txtGlue <- vector("character", buffer_)
    start_ <- 1
    tab <- strrep('\t',1)
    
    defaultDIVstyle <- paste0("border-left: 1px solid #ddd;",
                              "border-right: 1px solid #ddd;",
                              "margin: 0 auto; ",
                              "max-width: 800px; ",
                              "padding: 20px;")
    if (clear==T){
        defaultDIVstyle <- paste0(defaultDIVstyle, "clear: both; ")
    }
    if (style!=""){
        txtGlue[start_] <- paste0("<div style=\"",
                                    defaultDIVstyle,
                                    style,"\">")
    }else{
        txtGlue[start_] <- paste0("<div style=\"",
                                    defaultDIVstyle,"\">")
    }
    start_ <- start_ + 1
    
    # Register traces one by one
    for (ii in 1:nobjs){
        if (is.character(objs[[ii]])){
            # Cases:
            #   "<h1>...</h1>",
            #   "<p style="...>...</p>"
            if (start_>length(txtGlue)){
                txtGlue <- c(txtGlue, vector("character", buffer_))
            }
            txtGlue[start_] <- paste0(tab, objs[[ii]])
            start_ <- start_ + 1
            
        } else if (is.list(objs[[ii]])){
            # Bullet points
            bullets <- objs[[ii]]
            isStyle <- names(bullets)=="style"
            if (any(isStyle)){
                ulli_style <- bullets[["style"]]
                bullets <- bullets[!isStyle]
                ulli_cont <- vector("character",length(bullets)+2)
                if (length(ulli_style)==1){
                    # <ul style="...">
                    ulli_cont[1] <- paste0(tab,"<ul style=\"",ulli_style,"\">")
                    ulli_cont[2:(length(ulli_cont)-1)] <- 
                        paste0(tab, tab, "<li>",unlist(bullets),"</li>")
                }else if (length(ulli_style)==length(bullets)){
                    # <li style="...">
                    ulli_cont[1] <- paste0(tab,"<ul>")
                    ulli_cont[2:(length(ulli_cont)-1)] <- 
                        paste0(tab, tab, "<li style=\"",ulli_style,"\">",unlist(bullets),"</li>")                                        
                }else{
                    stop("Reporting: <li> styling must match number of input bullets...")   
                }
                ulli_cont[length(ulli_cont)] <- paste0(tab,"</ul>")
                
            }else{
                ulli_cont <- vector("character",length(bullets)+2)
                ulli_cont[1] <- paste0(tab,"<ul>")
                ulli_cont[2:(length(ulli_cont)-1)] <- 
                    paste0(tab,tab,"<li>",unlist(bullets),"</li>")
                ulli_cont[length(ulli_cont)] <- paste0(tab,"</ul>")                
            }
            
            # Add to container
            end_ <- start_ + length(ulli_cont) -1
            if (end_>length(txtGlue)){
                txtGlue <- c(txtGlue, vector("character", 
                            max(end_-length(txtGlue), buffer_)) )
            }
            txtGlue[start_:end_] <- ulli_cont
            start_ <- end_ + 1
            
        }else{
            stop("Reporting: addText() needs character/list as input...")   
        }
    }   
      
    # Closing
    if (start_>length(txtGlue)){
        txtGlue <- c(txtGlue, "")
    }
    txtGlue[start_] <- "</div>\n"
    
    # Drop empty pre-allocated space
    txtGlue <- txtGlue[1:max(which(txtGlue!=""))]
    
    # Compilation
    txtObj[[igr]] <- txtGlue
    
    # Update global containers
    assign("plotly_obj_paragraphs", txtObj, envir = .GlobalEnv)
    assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
    assign("plotly_obj_order", objOrd, envir = .GlobalEnv)
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
                     type=c(), # Graph type (line/bubble/bar/custom)
                     title="",
                     vline=c(), # Vertical bar separators (list of dates)
                     legend=c(), # List of line labels
                     xlabel="",
                     ylabel="",
                     colors=c(), # Set of line colors in 'rgba(#,#,#,#)' format
                    #markerSize="", # Slow rendering - only generated if alone data surrounded by NA exist
                    #size=6,     # Bubble size for bubble type - now solved via data frame input
                     by="",      # Grouping data by existing column, used for 'bubble' type, not available in 'custom' mode
                     lineWidth=NA, # user-defined value to be tracked 
                     width = 600,
                     height = 450,
                     style = "",
                     clear = F,
                    ######################
                    #Custom graphs options
                     layout = "",
                     config = ""
                     ){
 
    # Fetch graph container
    if (!exists('plotly_obj_graphs', envir = .GlobalEnv)) {
        stop("Initialization failed: Run plotlyIni() first...")
    }
    grObj  <- get('plotly_obj_graphs', envir=.GlobalEnv)
    objInfo <- get('plotly_obj_info', envir=.GlobalEnv)
    objOrd <- get('plotly_obj_order', envir=.GlobalEnv)

    # Preallocate if needed
    grObj <- addBuffer(grObj, objInfo)
    objOrd <- addBuffer(objOrd, objInfo)
    
    # Current graph id
    objInfo$ngraphs <- objInfo$ngraphs+1
    igr <- objInfo$ngraphs
    objOrd[igr+objInfo$nparagraphs] <- igr
    assign("plotly_obj_order", objOrd, envir = .GlobalEnv)
    
    # Input objects
    objs <- list(...)
    nobjs <- length(objs)

    # Optional styling on graph <div> level
    if (clear==T){
        style <- paste0(style,"clear: both; ")
    }
    if (style!=""){
        grObjSty  <- get('plotly_obj_graphs_sty', envir=.GlobalEnv)    
        grObjSty[["grID"]] <- c(grObjSty[["grID"]], igr)
        grObjSty[["style"]] <- c(grObjSty[["style"]], style)
        assign("plotly_obj_graphs_sty", grObjSty, envir = .GlobalEnv)
    }
    
    # Guess graph type if not user-supplied
    if (length(type)==0){
        if (layout!="" | config!=""){
            type = "custom"
        }else{
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
    }

    # Graph type junction
    switch(tolower(type),
        "ts"  ={graphTypeTS(grObj,objInfo,igr,
                            objs,nobjs,
                            x,title,vline,legend,xlabel,ylabel,colors,lineWidth,
                            width,height)},
        "line"={graphTypeLine(grObj,objInfo,igr,
                            objs,nobjs,
                            x,title,vline,legend,xlabel,ylabel,colors,lineWidth,
                            width,height)},
        "bar"={graphTypeBar(grObj,objInfo,igr,
                            objs,nobjs,
                            x,title,legend,xlabel,ylabel,colors,
                            width,height)},
        "bubble"={graphTypeBubble(grObj,objInfo,igr,
                            objs,nobjs,
                            title,legend,xlabel,ylabel,colors,by,
                            width,height)},
        "custom"={graphCustom(grObj,objInfo,igr,
                              objs,nobjs,
                              x, title, vline, legend, xlabel, ylabel, colors, by, lineWidth,
                              layout,config,
                              width,height)},
        stop("ts/line/bar/bubble/custom are the only supported graph types...")
    )

}

#' Time series graph
#'
#' @param none
#' @return none
graphTypeTS <- function(grObj,objInfo,igr,
                        objs,nobjs,
                        x,title,vline,legend,xlabel,ylabel,colors,lineWidth,
                        width,height){

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

    # Line width(s)
    if (is.na(lineWidth)){
        lineWidth <- rep(2,nobjs) # Default value
    }else if (length(lineWidth)==1){
        lineWidth <- rep(lineWidth,nobjs) # User-supplied for all lines
    }else if (length(lineWidth)!=nobjs){
        stop("plotlyjs: # of supplied lineWidths property does not match # of plotted objs...")
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
  cols <- genColors(colors, nobjs)
  
  # Legend entries
  legend <- legendValidate(legend, nobjs)
  # nlgnd <- length(legend)
  # if (nlgnd==0) {
  #   legend <- rep("Series",nobjs)
  #   legend <- stringr::str_replace(legend,"Series",paste0("Series ",c(1:nobjs)))
  # }else if (nlgnd<nobjs) {
  #   stop("# of legend entries does not match # of drawn objects")
  # }

  # Register all lines one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nobjs) {

    # Graph type junction
    # if (type=="line") {

      # Draw markers only if alone values present (for faster rendering)
      vals <- objs[[ii]][[2]]
      lineSpec <- sprintf("\tline: {color: '%s', width: %g},", cols[ii], lineWidth[ii])
      if ( aloneNumber(vals) ) {
        objType <- "\tmode: 'lines+markers',"
        markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], lineWidth[ii]) # Marker size matches lineWidth
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
                sprintf("\ttitle: '%s',",title),
               "\tautosize: false,",
                sprintf("\theight: %g,",height),
                sprintf("\twidth: %g",width),
              "};")

  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue

  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
}

#' Line graph
#'
#' @param none
#' @return none
graphTypeLine <- function(grObj,objInfo,igr,
                          objs,nobjs,
                          x,title,vline,legend,xlabel,ylabel,colors,lineWidth,
                          width,height){

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
    
    # y as numeric would be more natural input, if branch left here for completeness
    } else if (nobjs==1 & class(objs[[1]])[1]=="list"){ # ...[1] for multi classes
        if (length(objs[[1]])==2){
          # list(x,y) case
          if (length(x)==0){
            x <- objs[[1]][[1]] #c(1:length(objs[[1]][[1]]))
          }

        }else if (length(objs[[1]])==1){
          # list(y) together with x=... case is highly unlikely, left here for completeness
          if (length(x)==0){
              x <- c(1:length(objs[[1]][[1]]))
          }
          
        }else{
          stop("Unexpected input...")
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

    # Line width(s)
    if (is.na(lineWidth)){
        lineWidth <- rep(2,nobjs) # Default value
    }else if (length(lineWidth)==1){
        lineWidth <- rep(lineWidth,nobjs) # User-supplied for all lines
    }else if (length(lineWidth)!=nobjs){
        stop("plotlyjs: # of supplied lineWidths property does not match # of plotted objs...")
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
  cols <- genColors(colors, nobjs)
  
  # Legend entries
  legend <- legendValidate(legend, nobjs)
  # nlgnd <- length(legend)
  # if (nlgnd==0) {
  #   legend <- rep("Series",nobjs)
  #   legend <- stringr::str_replace(legend,"Series",paste0("Series ",c(1:nobjs)))
  # }else if (nlgnd<nobjs) {
  #   stop("# of legend entries does not match # of drawn objects")
  # }

  # Register all lines one by one
  start_ <- 1
  end_ <- objChunk
  for (ii in 1:nobjs) {

    # Graph type junction
    # if (type=="line") {

      # Draw markers only if alone values present (for faster rendering)
      vals <- objs[[ii]][[2]]
      lineSpec <- sprintf("\tline: {color: '%s', width: %g},", cols[ii], lineWidth[ii])
      if ( aloneNumber(vals) ) {
        objType <- "\tmode: 'lines+markers',"
        markerSpec <- sprintf("\tmarker: {color: '%s', size: %g}", cols[ii], lineWidth[ii]) # Marker size matches lineWidth
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
                sprintf("\ttitle: '%s',",title),
                "\tautosize: false,",
                sprintf("\theight: %g,",height),
                sprintf("\twidth: %g",width),
              "};")

  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue

  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
}

#' Bar graph
#'
#' @param none
#' @return none
graphTypeBar <- function(grObj,objInfo,igr,
                         objs,nobjs,
                         x,title,legend,xlabel,ylabel,colors,
                         width,height){

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
  cols <- genColors(colors, nY)
  # if (length(colors)==0){
  # 
  #   # Default color set
  #   cols <- c('rgba(23,190,207,0.9)',
  #             'rgba(127,127,127,0.9)',
  #             'rgba(244,194,66,0.9)',
  #             'rgba(220,130,130,0.9)')
  # 
  #   # Generate random colors if more lines needed
  #   if (length(cols)<nY){
  #     missing_ <- nY-length(cols)
  #     randCols <- stringr::str_replace("rgba(#,#,#,0.9)",
  #                                      "#",
  #                                      as.character(floor(runif(missing_)*255)))
  #     randCols <- stringr::str_replace(randCols,
  #                                      "#",
  #                                      as.character(floor(runif(missing_)*255)))
  #     randCols <- stringr::str_replace(randCols,
  #                                      "#",
  #                                      as.character(floor(runif(missing_)*255)))
  #     cols <- c(cols,randCols)
  #   }
  # 
  # }else{
  #   if (length(colors)!=nY){
  #     stop(sprintf(paste0("You have %g objects and provided %g colors",
  #                         " - these numbers must match..."),
  #                   length(colors),nY))
  #   }
  #   cols <- colors
  # }

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
                sprintf("\ttitle: '%s',",title),
               "\tautosize: false,",
                sprintf("\theight: %g,",height),
                sprintf("\twidth: %g",width),
              "};")

  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue

  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
}

#' Bubble graph
#'
#' @param none
#' @return none
graphTypeBubble <- function(grObj,objInfo,igr,
                         objs,nobjs,
                         title,legend,xlabel,ylabel,colors,by,
                         width,height){

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
  cols <- genColors(colors, ngr)
  
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
                sprintf("\ttitle: '%s',",title),
               "\tautosize: false,",
                sprintf("\theight: %g,",height),
                sprintf("\twidth: %g",width),
              "};")

  JSglue <- c(JSglue,dataObjs,layout,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,{'displayModeBar': false});",igr,igr))
  grObj[[igr]] <- JSglue

  # Update global containers
  assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
  assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
}

#' Custom graph
#'
#' @param none
#' @return none
graphCustom <- function(grObj,objInfo,igr,
                  objs,nobjs,
                  x, title, vline, legend, xlabel, ylabel, colors, by, lineWidth,
                  layout,config,
                  width,height){
    
    # Grouping by column banned
    if (by!=""){
        stop("plotlyjs: 'by' option does not work in 'custom' mode")   
    }
    
    # User defined options
    # Trace params
    if (!is.null(x)){
        for (iobj in 1:nobjs){
            if (!('x' %in% names(objs[[iobj]]))){
                objs[[iobj]]$x <- x
            }
        }
    }
    if (!is.null(legend)){
        legend <- legendValidate(legend, nobjs)
        for (iobj in 1:nobjs){
            objs[[iobj]]$name <- legend[iobj]
        }
    }
    if (!is.null(colors)){
        cols <- genColors(colors, nobjs)
        for (iobj in 1:nobjs){
            objs[[iobj]]$line$color <- cols[iobj]
        }
    }
    if (!is.na(lineWidth)){
        if (length(lineWidth)==1){
            lineWidth <- rep(lineWidth,nobjs)
        }
        for (iobj in 1:nobjs){
            objs[[iobj]]$line$width <- lineWidth[iobj]
        }            
    }
    # Layout param
    if (is.character(layout)){
        if (layout==""){
            layout <- list()
        }
    }else if (!is.list(layout)){
        stop("Plotly layout param must be a list...")
    }
    if (title!=""){
        layout$title <- title
    }
    if (xlabel!=""){
        layout$xaxis$title <- xlabel
    }
    if (ylabel!=""){
        layout$yaxis$title <- ylabel
    }
    if (!is.numeric(width) | !is.numeric(height)){
        stop("Plotly graph dimensions - numeric input needed...")   
    }
    layout$autosize <- F
    layout$width <- width
    layout$height <- height
    
    # Config param
    if (config==""){
        config <- list(displayModeBar = F)
    }else{
        if (is.list(config)){
            if ( !('displayModeBar' %in% names(config)) ){
                config$displayModeBar <- F
            }
            config <- processOpts(config, 2)  
        }else{
            stop("Plotly config param needs to be in list(field: val, ...) format")
        }
    }
    
    # Content glue
    buffer_ <- 100
    trGlue <- vector("character", buffer_)
    start_ <- 1
    
    # Register traces one by one
    for (ii in 1:nobjs){
        
        trGlue[start_] <- sprintf("var gr_%d_trace_%d = {",igr,ii)
        start_ <- start_ + 1
        
       #opts <- names(objs[[ii]])
        
        res <- processOpts(objs[[ii]], 1)
        end_ <- start_ + length(res) -1
        if (end_>length(trGlue)){
            trGlue <- c(trGlue, 
                        vector("character", max(end_-length(trGlue), buffer_)) )
        }
        trGlue[start_:end_] <- res
        
        trGlue[end_+1] <- '}'
        start_ <- end_+2
        
    }   
        
    # Drop empty pre-allocated space
    trGlue <- trGlue[1:max(which(trGlue!=""))]
    
    # [2] Data object
    # var data = [trace1,trace2,...];
    dataObjs <- rep(sprintf("gr_%d_trace_##",igr),nobjs)
    dataObjs <- stringr::str_replace(dataObjs,'##',as.character(c(1:nobjs)))
    dataObjs <- c(sprintf("var gr_%d_data = [",igr),
                c('\t',paste(dataObjs, collapse=',')),
                '];')
    
    # [3] Layout
    # Add optional vertical separators
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
    
    # layout is never empty, at least height/width exist
    layGlue <- c("var layout = {",
                 sh, # better listed as first, overruling on next line
                 processOpts(layout, 1),
                 "}")
    
    # [4] Config options
    # -> config is never empty, at least displayModeBar exists
    confOpt <- c("var config = {",
                 processOpts(config, 1),
                 "}")
    
    # Compilation
    grObj[[igr]] <- c(trGlue,dataObjs,layGlue,confOpt,
              sprintf("Plotly.newPlot('gr%d',gr_%d_data,layout,config);",igr,igr))
    
    # Update global containers
    assign("plotly_obj_graphs", grObj, envir = .GlobalEnv)
    assign("plotly_obj_info", objInfo, envir = .GlobalEnv)
}

#' list() -> JSON generator (mainly for graph trace defs)
#'
#' @param none
#' @return JSON structure
processOpts <- function(input, level){
    # browser()
    buffer_ <- 10
    segmGlue <- vector("character", buffer_)
    start_ <- 1
    
    tabs <- strrep('\t',level)
    opts <- names(input)
    
    # Treat shapes/annotations in [{...},{...}] format
    if ( all(sapply(input, is.list)) & is.null(opts) ){
        # input = list of lists without names
        # output = [{...},{...}]
        for (ii in 1:length(input)){
            
            # list closing
            if (ii==length(input)){
                endL <- '' # \n not necessary for writeLines()       
            }else{
                endL <- '},{'
            }
            
            res <- processOpts(input[[ii]], level+1)
            end_ <- start_ + length(res) # including }] line
            if (end_>length(segmGlue)){
                segmGlue <- c(segmGlue, 
                              vector("character", 
                                     max(end_-length(segmGlue),buffer_)) )
            }
            segmGlue[(start_):(start_+length(res)-1)] <- res
            segmGlue[start_+length(res)] <- paste0(tabs, endL)
            start_ <- start_ +length(res)+1
        }
        
        # Drop empty pre-allocated space
        segmGlue <- segmGlue[1:max(which(segmGlue!=""))]
    
        return(segmGlue) 
    }
    
    for (iopt in opts){
        
        # Line ending
        if (iopt==opts[length(opts)]){
            endL <- '' # \n not necessary for writeLines()       
        }else{
            endL <- ','
        }
        
        # Value processing
        val <- get(iopt, input)
        if (is.list(val)){
            res <- processOpts(val, level+1)
            end_ <- start_ + length(res) +1
            if (end_>length(segmGlue)){
                segmGlue <- c(segmGlue, 
                              vector("character", 
                                     max(end_-length(segmGlue),buffer_)) )
            }
            if ( all(sapply(val, is.list)) & is.null(names(val)) ){
                # input was list of lists without names
                # output = [{...},{...}]
                segmGlue[start_] <- paste0(tabs, iopt, ": [{")
                segmGlue[(start_+1):(start_+length(res))] <- res
                segmGlue[start_+length(res)+1] <- paste0(tabs, '}]', endL)
            }else{
                # Usual cases {...}
                segmGlue[start_] <- paste0(tabs, iopt, ": {")
                segmGlue[(start_+1):(start_+length(res))] <- res
                segmGlue[start_+length(res)+1] <- paste0(tabs, '}', endL)
            }
            
            start_ <- start_ +length(res)+2
        }else{
            if(any( (iopt==c('x','y','z')) & level==1) | length(val)>1 ){
                # 'x' in shapes/annotations must have scalar values, not jsonized (solved by level==1)
                # c(1,2,3) -> [1,2,3]
                val <- jsonlite::toJSON(val)   
            }else if (is.character(val) | is.Date(val)){
                # if (substr(val,1,1)=='#'){
                #     # #colors -> colors[ii]
                #     s <- substr(val,2,nchar(val))
                #     s <- paste0(toupper(substring(s, 1,1)), substring(s, 2))
                #     paste0("gen",s)
                #     val <- get(substr(val,2,nchar(val)))[tracenum]
                # }
                val <- paste0("\"", val, "\"")
            }else if (is.logical(val)){
                # true/false lower case needed, no quotes
                val <- tolower(as.character(val)) 
            }
            if (start_>length(segmGlue)){
                segmGlue <- c(segmGlue, vector("character", buffer_))
            }
            segmGlue[start_] <- paste0(tabs, iopt, ": ", val, endL)
            start_ <- start_ + 1
        }
        
    }
    
    # Drop empty pre-allocated space
    segmGlue <- segmGlue[1:max(which(segmGlue!=""))]
    
    return(segmGlue) 
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
                          css = "",
                          font = "",
                          name = "Report name",
                          reopen = F,
                          debug = F){

  # Fetch graph container
  if (!exists('plotly_obj_graphs', envir = .GlobalEnv)) {
    stop("Report compilation failed: Run plotlyIni() first, then add some graphs using addGraph()...")
  }
  grObj  <- get('plotly_obj_graphs', envir=.GlobalEnv)
  txtObj  <- get('plotly_obj_paragraphs', envir=.GlobalEnv)
  objInfo <- get('plotly_obj_info', envir=.GlobalEnv)
  grObjSty <- get('plotly_obj_graphs_sty', envir=.GlobalEnv)
  objOrd <- get('plotly_obj_order', envir=.GlobalEnv)
  
  nobj <- objInfo$ngraphs + objInfo$nparagraphs
  if (nobj==0){
    stop("plotlyjs: No objects on input, nothing to compile...")   
  }
  
  # Cut off preallocated space
  if (objInfo$ngraphs>0){
    grObj <- grObj[1:objInfo$ngraphs]
  }else{
    grObj <- list("// No graph objects on input")  
  }
  objOrd <- objOrd[1:nobj]
  
  ###############################
  # Generate HTML div tags
  
  # [1] Graphs
  if (objInfo$ngraphs>0){
    grDivs <- stringr::str_replace("<div id='#'></div>",
                                   "#",
                                   paste0('gr',c(1:objInfo$ngraphs)))
    
    # User-imposed style for some graphs
    if (length(grObjSty[["grID"]])!=0){
      grDivs[grObjSty[["grID"]]] <- 
          stringr::str_replace(grDivs[grObjSty[["grID"]]],
               "><",
               paste0(" style=\"",grObjSty[["style"]],"\"><"))
    }
    
  }else{
    grDivs <- c()
  }
  
  # browser()
  # [2] Text fields
  #ord <- c(1,2,10001,3,4)
  if (objInfo$nparagraphs>0){
    txtObj <- txtObj[1:objInfo$nparagraphs]
    vals <- c(as.list(grDivs),txtObj)
    ids <- c(1:length(grDivs), (1:length(txtObj))+objInfo$maxGraphs)
  }else{
    vals <- as.list(grDivs) # some graphs exist for sure, 0 treated before
    ids <- c(1:length(grDivs))
  }
  allDivs <- unlist(vals[match(objOrd,ids)])
  
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
    
  ##############
  # Default CSS styling
  
  # Graphs
  CSS <- "div[id^=\"gr\"] {float: left}"
  
  # Debugging
  if (debug==T){
    CSS <- c(CSS,"\n\t\t/* Debugging */","div {border: 1px solid red}\n")
  }
  
  # CSS - loader
  CSS <- c(CSS,"#loader-back {
            position: fixed;
            height: 100%;
            width: 100%;
            z-index: 1;
        }
        #loader {
            position: fixed; 
            top:35%; left:50%; 
            margin-top: -60px;
            margin-left: -80px;
            z-index: 2;
            /*border: 16px solid #f3f3f3;*/
            border-radius: 80%;
            border-top: 26px solid #3498db;
            border-bottom: 26px solid #3a91ab;
            border-left: 6px solid #3a91ab;
            border-right: 6px solid #3a91ab;
            width: 160px;
            height: 120px;
            
            -webkit-animation: spin 3s linear infinite; 
            animation: spin 3s linear infinite;
        }
        
        /* Safari */
        @-webkit-keyframes spin {
            0% { -webkit-transform: rotate(0deg); }
            100% { -webkit-transform: rotate(360deg); }
        }
        
        @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
        }
        #loader-msg {
            position: fixed; 
            top:35%; left:50%; 
            text-align: center;
            vertical-align: middle;
            line-height: 172px;
            width: 172px;
            height: 172px;
            /*border: 1px solid red;*/
            margin-top: -60px;
            margin-left: -80px;
            z-index: 3;
        }")
  
  # External fonts
  # - always to be downloaded, base64 encoding currently not implemented
  # - can also be copied <link href=...> from Google APIs
  if (is.numeric(font)){
    if (font==1){
       #font <- "<link href=\"https://fonts.googleapis.com/css?family=Open+Sans&display=swap\" rel=\"stylesheet\">"
       #family <- "font-family: \"Open Sans\", sans-serif;" 
        font <- "<link href=\"https://fonts.googleapis.com/css?family=PT+Sans&display=swap\" rel=\"stylesheet\">"
        family <- "font-family: 'PT Sans', sans-serif;"
    }else if (font==2){
        font <- "<link href=\"https://fonts.googleapis.com/css?family=Roboto+Slab&display=swap\" rel=\"stylesheet\">"
        family <- "font-family: 'Roboto Slab', serif;"
    }else{
        stop("Font specification - Predefined values: 1, 2 available only")   
    }
    # Update styling
    CSS <- c(CSS, paste0("div {", family, "}"))
    
  }else if (is.list(font)){
        if (all(c('font','family') %in% names(font))){
            family <- font$family      
            font <- font$font
            
            # Update styling
            CSS <- c(CSS, paste0("div {font-family: ", 
                            stringr::str_replace(family,"font-family: ",""), "}"))
        }else{
            stop("Font specification: Example input - list(font=\"<link href=...fonts google APIs>\", family=\"font-family: 'Roboto Slab', serif;\")")   
        }
  }else if (font==""){
      font <- "\t\t<!-- Default fonts used -->\n"
  }else{
    stop("Font specification: numeric input (1,2,...) or list input needed")
  }
      
  # User-defined CSS can overwrite default setup
  CSS <- c(CSS,paste0("\t\t",css))
  
  # Background SVG
  
  # Rewrite the report file from scratch
  fileConn<-file(reportFile)
  writeLines(c("<!DOCTYPE html>",
               "<html>", # '<html lang="en">'
               "\t<head>",
               paste0("\t\t", libLink),
               "\t\t<!-- Loader -->",
	           "\t\t<script language=\"javascript\" type=\"text/javascript\">",
 		       "\t\t\t\twindow.addEventListener('load', function () {",
			   "\t\t\t\t\tdocument.getElementById(\"loader\").style.display='none';",
			   "\t\t\t\t\tdocument.getElementById(\"loader-msg\").style.display='none';",
			   "\t\t\t\t\tdocument.getElementById(\"loader-back\").style.display='none';",
			   "\t\t\t\t\tdocument.getElementById(\"content\").style.display='block';",
		       "\t\t\t\t}, false);",
	           "\t\t</script>",
			   "\t\t<!-- Font -->",
			   paste0("\t\t", font),
               "\t</head>",
               "<body>",
               "\t<style>",
               paste0("\t\t", CSS),
               "\t</style>\n",
			   "<div id=\"loader\"></div>",
               "<div id=\"loader-msg\">Loading...</div>",
               "<div id=\"loader-back\"></div>\n",
			   "<div id=\"content\" style=\"display: none;\">", # Needed for loader (hides everything initially)
               "<!-- Header -->",
               "<div id='header' style=\"margin-left: 5px;",
			   paste0("\t\t\tbackground-repeat: no-repeat; background-size: 100% 100%;\">"),
			   paste0("\t<h1 style=\"margin-top: 5px; margin-bottom: 5px\">", name, "</h1>"),
			   paste0("\t<p style=\"font-size: 10px; margin-top: 5px;\">Report creation timestamp: ",Sys.time(),"</p>"),
               "</div>\n",
               "<!-- Page content -->",
			   allDivs, 
			   "\n</div> <!-- 'content' closing -->",
               "</body>",
              #paste0("\t<script src=\"",JSfileInject,"\"></script>"),
               "<script>",
               unlist(grObj),
			   paste0("\ndocument.getElementById('header').style.backgroundImage = \"url('",encodedBackground(),"')\";"),
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
      }else{
        browseURL(reportFile) 
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
         }else{
            browseURL(reportFile) 
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
figure <- function(..., # Expected input: ts: list(periods,values)
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
                   lineWidth=2,
                   width = 600,
                   height = 450,
                  ######################
                  #Custom graphs options
                   layout = "",
                   config = "",
                   style = "",
                  ####################
                  #Compilation options
                   reportFile="tmp.html",
                   libFile="path/to/file/plotly.min.js",
                   lightWeight = F,
                   name = "Report name",
                   css = "",
                   font="",
                   reopen = F,
                   debug = F){                      
  plotlyIni()
  addGraph(...,x=x,
               type=type,
               title=title,
               vline=vline,
               legend=legend,
               xlabel=xlabel,
               ylabel=ylabel,
               colors=colors,
               by=by,
               lineWidth=lineWidth,
               width=width,
               height=height,
               layout=layout,
               config=config,
               style=style)
  plotlyCompile(reportFile=reportFile,
                libFile=libFile,
                lightWeight = lightWeight,
                name=name,
                css=css,
                font=font,
                reopen=reopen,
                debug=debug)
}

#' Colors generator for given number of objects
#'
#' @param none
#' @return none
genColors <- function(colors, ngr){
    
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
    return(cols)
}

#' Legend shape validation
#'
#' @param none
#' @return none
legendValidate <- function(legend, nobjs){
    nlgnd <- length(legend)
    if (nlgnd==0) {
        legend <- rep("Series",nobjs)
        legend <- stringr::str_replace(legend,"Series",paste0("Series ",c(1:nobjs)))
    }else if (nlgnd<nobjs) {
        stop("# of legend entries does not match # of drawn objects")
    }
    return(legend)
}

#<eof>