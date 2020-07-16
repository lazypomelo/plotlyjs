# plotlyjs for R
R wrapper for javascript version of [plotly](https://plot.ly/javascript/) graphing library. Suitable for fast generation of reports that contain many graphs (even hundreds), much faster than CRAN plotly library. 

Basic graph types with simplified input syntax:
- Time series graphs,
- Line graphs,
- Bar graphs,
- Bubble graphs.

Custom graphs featuring all plotly features are also covered.

Generated reports are stored in a self-contained HTML file that can be opened in a web browser. Besides plotly graphics, the reports can contain textual input as well (headings, paragraphs, bullet lists, etc.). 

## Installation
``` r
devtools::install_github("lazypomelo/plotlyjs")
library(plotlyjs)
```
## Fast data visualization
The easiest way to visualize data, similar to standard *plot()*, is to use *figure()* function:
``` r
data <- c(1:5)
figure(data)
```
The result is stored in **tmp.html** file located in the the current working directory (path to be checked in R with *getwd()* function).
Optionally, the user can specify another report name, or ask RStudio to open up the report file in browser:
``` r
data <- c(1:5)
figure(data, reportFile = "path/to/file.html",
	     reopen = T)
```
Each use of *figure()* function always overrides the previous graph. A refresh of the web browser is needed after each update.
 
## Intended workflow
Fast visualization via *figure()* helps create simple graphs for quick data checks. When building complex reports, one is expected to utilize sequence of functions *plotlyIni()*, *addText()*, *addGraph()* (multiple graphs/paragraphs can be added to the report), *plotlyCompile()*, as described below.
1) Initialize/Clear previous report contents
``` r
> plotlyIni()
```

2) Generate a graph using *addGraph()* - here, in this example, we create a time series graph for which we need the time axis info together with the values:
``` r
> periods <- seq(as.Date('2010-01-01'),as.Date('2010-08-01'),by="month")
> values <- runif(8, min=0, max=10) # Random values
> addGraph(list(periods,values), type="ts")
```
> !!! Expected input format: list(periods,values), time series object, numeric vector, matrix, or data frame.

In most cases here we would like to create many graphs, perhaps inside a *for* loop to visualize, say, each column of a dataframe:
``` r
> dt <- data.frame(replicate(5,runif(8))) # Create a random dataframe
> for (ii in 1:ncol(dt)){
      addGraph(list(periods,dt[,ii]),
	       title=colnames(dt)[ii],
	       type="ts"
      )
  }
```
Note that graph titles can be inherited from the input data frame.

Multiple lines in one plot (series by series syntax):
``` r
> dt1 <- data.frame(replicate(5,runif(8)))
> dt2 <- data.frame(replicate(5,runif(8)))
> for (ii in 1:ncol(dt1)){
      addGraph(list(periods,dt1[,ii]), # 1st line
      	       list(periods,dt2[,ii]), # 2nd line in the same plot
	       title = colnames(dt1)[ii],
	       legend = c("Model 1", "Model 2"),
	       type="ts"
      )
  }
```
Multiple lines in one plot (all columns from a data frame)
 - separate *x* axis definition mandatory for time series:
``` r
> dt <- data.frame(replicate(5,runif(8)))
> addGraph(dt, 
           x = seq(as.Date('2010-01-01'), as.Date('2010-08-01'), by="month") 
               # horizontal axis values,
               # dates used here, any other sequence of values 
               # (even string categories) for other graph types possible
  )
```
3) Textual input
``` r
> addText("<h1>Some heading</h2>",
          "<p> Some paragraph</p>",
          list("Bullet list - item1","item2","item3")
  )
```
4) As a last step generate the HTML report:
``` r
> plotlyCompile()
```
Leaving the function options blank results in generation of a **tmp.html** file located in the the current working directory (just like the result of **figure()** command).

## Optional function arguments
Graph/paragraph creation and report compilation functions have optional arguments. Default values exist but the user is welcome to override them.
``` r
addGraph(data1, data2, data3, ...
         x = c(1,2,3), # Horizontal axis values, if not entered before as data=list(x, values)
                       # Can be a vector of numeric values, dates, or textual categories
	       type = "ts|line|bar|bubble", # also there is flexible 'custom' type, as described below
         title  = "myTitle",
         vline  = c('2010-01-01','2013-01-01',...),
         legend = c("lg entry 1","lg entry 2",...),
         xlabel = "X axis label",
         ylabel = "Y axis label",
         colors = c(rgba(100,100,100,0.9),...)
        #Bubble graph coloring grouped by a column name values
         by = "column name", # works with input data in data frame format
        #For line graphs/time series graphs only
         lineWidth = 2,
        #Dimensions of generated graph in pixels
         width = 600,
         height = 450,
        #CSS style of the <div> graph envelope
         style = "margin-left: 100px",
         clear = F
)
```
**Notes:**
- *vline* - Set of vertical lines (useful for time series graphs to separate history from the forecasting range)  
- *colors* - set of RGB/RGBA color codes for each of the input data series
- *clear* - By default the graphs are placed next to each other till the screen space is used up. Forcing ```clear = T ``` puts the currently generated graph below previously generated content, all the way to the left.

``` r
addText("<h1>Heading</h1>",
        "<p>Some paragraph</p>",
        list("bullet list - item1","item2","item3",...),
       #CSS styling applied to this paragraph
        "<p style='text-align: right;'>Some paragraph</p>",
       #CSS styling applied to 1st and 3rd bullet
        list("item1","item2","item3", style=c("line-height: 20px;","","line-height: 20px;")),
        ..., # other input objects
        style = "CSS style on <div> level",
        clear = T,
)
```
**Notes:**
- *clear* - By default, all text paragraphs are placed below the previously generated content, expanding from the left. Setting ```clear = F ``` makes the text paragraphs appear to the right of previously generated object (if  the space permits).

``` r
plotlyCompile(reportFile = "path/to/your/file.html",
              libFile = "path/to/file/plotly.min.js",
              lightWeight = F,
              css = "styling on the level of entire document"), # e.g. div{padding: 0px;}
              font = 1, # Predefined google fonts: =1 sans-serif, =2 serif
              name = "My report name", # to be displayed at the top of report
              reopen = F,
              debug = F
)
```
 **Notes:**
- All arguments are optional
- *lightWeight*=T ...plotly library linked via a web URL to CDN repository (slower to re-run results but easy to share the report file since it is small in size)
- *lightWeight*=F ...local copy of Plotly library embedded into the report (faster to generate/re-load in a browser, but the report is quite large in size)
- *libFile* ...[to be used together with *lightWeight*=F] optional path to the local copy of plotly JS library file (if omitted, a fresh download of plotly library is performed)
- *font* - besides the default font types, the user can utilize e.g. google open fonts using the following syntax:
``` r
  font = list("<link href='https://fonts.googleapis.com/css?family=Roboto&display=swap' rel='stylesheet'>",
              "font-family: 'Roboto', sans-serif;")
```
- *reopen* - The generated report opens up in the web browser automatically. In case the report is then closed by the user we can force to reopen it again by this parameter.
- *debug* - CSS styling can be a struggle, debug mode draws red frames around the generated objects to allow for easier positioning.

## Other graph types
Here we describe the syntax for graph types other than time series. *addGraph()* function can still be used together with proper *type* specification.

### Line graph
The syntax for line graphs is identical as for the time series graphs. 
Values on horizontal axis are optional. The type option should be set to "line"
``` r
data <- matrix(runif(15),5,3))
addGraph(data,        # 1 or more list(x,y), or a numeric vector/matrix, or a data frame
         type="line",
	       x = c(1:5)   # optional x axis values
)
```
### Bar graph
Input data must be inside a data frame.
Type option should be set to "bar". The rest of options stays the same as for line/ts graphs.
``` r
data <- matrix(runif(15),5,3))
addGraph(as.data.frame(data),
         type="bar"
)
```
  
### Bubble graph
Input data must be inside a data frame. 
Type option should be set to "bubble". "by" option is not mandatory, but helps to visualize different data sets in different colors.
If 3 data columns are used, the 3rd (optional) column will be used for bubble sizes.
``` r
data <- data.frame(
		x=runif(100),
		y=runif(100),
		siz=runif(100), # Bubble sizes here [optional column]
		group=c(rep('Group 1',40),rep('Group 2',60))
)
addGraph(data,
	 type="bubble",
	 by="group"
)
```
### Custom graphs
In case the above graph types with simplified R syntax are not serving well, it is possible to generate any kind of graph while respecting the plotly syntax directly and setting *type* to 'custom'.

> *Example*:
> We want to plot two series into a single graph, one as regular line graph, one with markers only.
> This would be the plotly syntax (inspired by [plotly](https://plot.ly/javascript/)):
``` js
var trace1 = {
  x: [1, 2, 3, 4],
  y: [10, 15, 13, 17],
  mode: 'markers'
};

var trace2 = {
  x: [2, 3, 4, 5],
  y: [16, 5, NaN, 9],
  mode: 'lines'
};

var data = [trace1, trace2];

var layout = {
  title:'Line and Scatter Plot'
};
var config = {
  displayModeBar: false
};
Plotly.newPlot('myDiv', data, layout, config);
```
> ...which can be mimiced by the following way R syntax:
``` r
addGraph(list(x = c(1, 2, 3, 4),
              y = c(10, 15, 13, 17),
              mode = 'markers'),
         list(x = c(2, 3, 4, 5),
              y = c(16, 5, NA, 9),
              mode = 'lines'),
         layout = list(title = "Line and Scatter Plot"),
         config = list(displayModeBar = F)
        #All other parameters as documented above work for custom graphs as well
)
```
Custom type graphs also allow for a flexible number of input objects, organized as nested lists:
``` r
dt <- list()
dt[[1]] <- list(x = c(1,2,3,4),
                y = c(10,15,13,17),
                mode = 'markers')
dt[[2]] <- list(x = c(2,3,4,5),
                y = c(16,5,11,9),
                mode = 'lines+markers')
addGraph(dt,
				 layout = ...,
				 config = ...
)
```