# plotlycsv

Add a Plotly modebar button to download trace data as CSV in static HTML outputs (Quarto / R Markdown).

## Install

From a local folder (e.g. after unzipping):

```r
install.packages("plotlycsv", repos = NULL, type = "source")
# or
remotes::install_local("plotlycsv")
```

## Use

```r
library(ggplot2)
library(plotly)
library(magrittr)

p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()

ggplotly(p) %>%
  decorate_plotly_export_csv(filename = "mtcars.csv")
```
