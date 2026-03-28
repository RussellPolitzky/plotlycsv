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

### Individual plots

```r
library(ggplot2)
library(plotly)
library(plotlycsv)

p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()

ggplotly(p) |>
  add_plotly_export_csv(filename = "mtcars.csv")
```

### Global (all plots in a document)

In Quarto or R Markdown, you can automatically add the export button to every Plotly chart on the page without piping `add_plotly_export_csv()` to each one:

```r
library(plotlycsv)
use_plotly_csv()
```

By default, the global decorator automatically names downloaded CSV files after the code chunk name or figure label (e.g. `your-chunk-name.csv` or `fig-your-label.csv`), prioritizing the figure label over the chunk name.
