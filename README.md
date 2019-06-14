# plotlyjs
R wrapper for plotly JS

Intended use:
1) Clear previous report contents
      > plotlyIni()

2) Create graphs in a for loop using addGraph()
  for (ii in 1:ngraphs)\{
      addGraph(list(periods,values), -> time series #1
               list(periods,values), -> ts #2
               ...,                  -> other ts
               title="myTitle",
               legend=c("lg entry 1","lg entry 2",...),
               vline=c('2010-01-01','2013-01-01',...),
               xlabel="...",ylabel="...",
               colors=c(rgba(100,100,100,0.9),...))
  \}
  !!! All arguments, except for input time series, are optional 

3) Generate the report file
      > plotlyCompile(reportFile="path/to/file.html",
                      lightWeight=F)
      -> lightWeight==T ...Plotly library downloaded fro CDN (slower to re-run results but easy to share)
      -> lightWeight==F ...Local copy of Plotly library (faster to generate/load in browser)

Package prepared for Merck Data Science Competition 2019
Author: jakub.rysanek@merck.com
