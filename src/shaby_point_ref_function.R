
library(classInt) # breakpoints for color plotting
library(fields) # used here for color scheme

plot.point.ref <- function(spatialdata, vals) {

  pal <- tim.colors(10)
  ints <- classIntervals(vals, n = 8, style = "pretty") # Determine breakpoints
  # also see style options "quantile" and "fisher"
  intcols <- findColours(ints, pal) # vector of colors
  # if pal doesn't have the same length as # classes, findColours will interpolate

  par(mar = rep(0, 4))
  plot(spatialdata, col = intcols, pch = 19)
  points(spatialdata, pch = 1)
  legend("topleft", fill = attr(intcols, "palette"),
         legend = names(attr(intcols, "table")), bty = "n")
}


ploteqc <- function(spobj, z, breaks, ...){
  pal <- tim.colors(length(breaks)-1)
  fb <- classIntervals(z, n = length(pal), 
                       style = "fixed", fixedBreaks = breaks)
  col <- findColours(fb, pal)
  plot(spobj, col = col, ...)
  image.plot(legend.only = TRUE, zlim = range(breaks), col = pal)
}