# plotlyjs for R
R wrapper for javascript version of [plotly](https://plot.ly/javascript/) graphing library. Suitable for fast generation of reports that contain many graphs (even hundreds). 

Only basic graph types are handled:
- Time series graphs,
- Line graphs,
- Bar graphs,
- Bubble graphs.

Generated reports are stored in a self-contained HTML file that needs to be opened externally in a browser.

## Installation
``` r
devtools::install_github("lazypomelo/plotlyjs")
library(plotlyjs)
```
## Fast data visualization
The easiest way to visualize data is to use *figure()* function:
``` r
data <- c(1:5)
figure(data)
```
The result is the **tmp.html** file located in the the current working directory (path to be checked by R *getwd()* function):

Each use of *figure()* function always overrides the previous graph + refresh of the web browser needed.
 
## Intended workflow

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
Multiple lines in one plot (e.g. model comparison):
``` r
> dt1 <- data.frame(replicate(5,runif(8)))
> dt2 <- data.frame(replicate(5,runif(8)))
> for (ii in 1:ncol(dt1)){
      addGraph(list(periods,dt1[,ii]),
      	       list(periods,dt2[,ii]),
	       title = colnames(dt1)[ii],
	       legend = c("Model 1", "Model 2"),
	       type="ts"
      )
  }
```
3) As a last step generate the HTML report:
``` r
> plotlyCompile()
```
Leaving the function options blank results in generation of a **tmp.html** file located in the the current working directory (just like the result of **figure()** command).

## Optional function arguments
Both graph creation and report compilation functions have optional arguments.
``` r
addGraph(data1, data2, data3, ...
	 type="ts|line|bar|bubble" -> 1 type to be selected here
         title="myTitle",
         legend=c("lg entry 1","lg entry 2",...),
         vline=c('2010-01-01','2013-01-01',...),
         xlabel="...",ylabel="...",
         colors=c(rgba(100,100,100,0.9),...)
)
  ```
**Notes:**
- *vline* - Set of vertical lines (useful for time series graphs to separate history from the forecasting range)  
- *colors* - set of RGB/RGBA color codes for each of the input data series

``` r
> plotlyCompile(reportFile="path/to/your/file.html",
		libFile="path/to/file/plotly.min.js",
                lightWeight=F)
```
 **Notes:**
- All arguments are optional
- *lightWeight*=T ...plotly library downloaded from CDN (slower to re-run results but easy to share the report file since it is small in size)
- *lightWeight*=F ...local copy of Plotly library embedded into the report (faster to generate/re-load in a browser, but the report is quite large in size)
- *libFile* ...path to local copy of plotly JS library file 

## Other graph types
Here we describe the syntax for graph types other than time series. *addGraph()* function can still be used together with *type* option.

### Line graph
The syntax for line graphs is identical as for the time series graphs. 
Values on horizontal axis are optional. The type option should be set to "line"
``` r
data <- matrix(runif(15),5,3))
addGraph(data,        # 1 or more list(x,y), or a numeric vector/matrix, or a data frame
         type="line",
	 x = c(1:5)   # values on horizontal axis are optional
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
