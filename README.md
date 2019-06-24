# plotlyjs for R
R wrapper for javascript version of [plotly](https://plot.ly/javascript/) graphing library. Suitable for fast generation of reports that contain many graphs (even hundreds). 

Only basic graph types are handled:
- Time series graphs,
- Line graphs,
- Bar graphs,
- Bubble graphs.

Generated reports are stored in a self-contained HTML file that needs to be opened externally in a browser.

## Installation
```js
devtools::install_github("lazypomelo/plotlyjs")
library(plotlyjs)
```
## Fast data visualization
The easiest way to visualize data is to use *figure()* function:
```js
data <- c(1:5)
figure(data)
```
The result is the **tmp.html** file located in the the current working directory (path to be checked by R *getwd()* function):

Each use of *figure()* function always overrides the previous graph + refresh of the web browser needed.
 
## Intended workflow

1) Initialize/Clear previous report contents
```js
> plotlyIni()
```

2) Generate a graph using *addGraph()* - here, in this example, we create a time series graph for which we need the time axis info together with the values:
```js
> periods <- seq(as.Date('2010-01-01'),as.Date('2010-08-01'),by="month")
> values <- runif(8, min=0, max=10) # Random values
> addGraph(list(periods,values))
```
> Input format can be list(periods,values), time series, numeric, matrix, or data frame.

In most cases here we would like to create many graphs, perhaps inside a *for* loop to visualize, say, each column of a dataframe:
```js
> dt <- data.frame(replicate(5,runif(8))) # Create a random dataframe
> for (ii in 1:ncol(dt)){
      addGraph(list(periods,dt[,ii]),
	       title=colnames(dt)[ii]
      )
  }
```
3) As a last step generate the HTML report:
```js
> plotlyCompile()
```
Leaving the function options blank results in generation of a **tmp.html** file located in the the current working directory (just like the result of **figure()** command).

## Optional function arguments
Both graph creation and report compilation functions have optional arguments.
```js
addGraph(list(periods,values), -> time series
	 list(periods,values), -> ts #2
         ...,                  -> other ts
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

```js
> plotlyCompile(reportFile="path/to/your/file.html",
                lightWeight=F)
```
 **Notes:**
- *lightWeight*=T ...plotly library downloaded from CDN (slower to re-run results but easy to share the report file since it is small in size)
- *lightWeight*=F ...local copy of Plotly library embedded into the report (faster to generate/re-load in a browser, but the report is quite large in size)

## Other graph types
Here we describe the syntax for graph types other than time series. *addGraph()* function can still be used together with *type* option.

### Line graph
The syntax for line graphs is identical as for the time series graphs. 
Values on horizontal axis are optional. The type option should be set to "line"
```js
data <- matrix(runif(15),5,3))
addGraph(data,        # 1 or more list(x,y), or a numeric vector/matrix, or a data frame
         type="line",
	 x = c(1:5)   # values on horizontal axis are optional
)
```
### Bar graph
Input data must be inside a data frame.
Type option should be set to "bar". The rest of options stays the same as for line/ts graphs.
```js
data <- matrix(runif(15),5,3))
addGraph(as.data.frame(data),
         type="bar"
)
```
  
### Bubble graph
Input data must be inside a data frame. 
Type option should be set to "bubble". "by" option is not mandatory, but helps to visualize different data sets in different colors.
If 3 data columns are used, the 3rd (optional) column will be used for bubble sizes.
```js
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
