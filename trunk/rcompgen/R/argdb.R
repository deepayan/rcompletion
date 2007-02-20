

## usage:

## .addFunctionInfo(foo = c("arg1", "arg2"), bar = c("a", "b"))

.addFunctionInfo <- function(...)
{
    dots <- list(...)
    for (nm in names(dots))
        .FunArgEnv[[nm]] <- dots[[nm]]
}

.initialize.argdb <-
    function()
{
    ## lattice

    lattice.common <-
        c("data", "allow.multiple", "outer", "auto.key", "aspect",
          "panel", "prepanel", "scales", "strip", "groups", "xlab",
          "xlim", "ylab", "ylim", "drop.unused.levels", "...",
          "default.scales", "subscripts", "subset", "formula", "cond",
          "aspect", "as.table", "between", "key", "legend", "page",
          "main", "sub", "par.strip.text", "layout", "skip", "strip",
          "strip.left", "xlab.default", "ylab.default", "xlab",
          "ylab", "panel", "xscale.components", "yscale.components",
          "axis", "index.cond", "perm.cond", "...", "par.settings",
          "plot.args", "lattice.options")

    densityplot <-
        c("plot.points", "ref", "groups", "jitter.amount",
          "bw", "adjust", "kernel", "weights", "window", "width",
          "give.Rkern", "n", "from", "to", "cut", "na.rm")

    panel.xyplot <-
        c("type", "groups", "pch", "col", "col.line",
          "col.symbol", "font", "fontfamily", "fontface", "lty",
          "cex", "fill", "lwd", "horizontal")

    .addFunctionInfo(xyplot.formula = c(lattice.common, panel.xyplot),
                     densityplot.formula = c(lattice.common, densityplot))

    ## grid

    grid.clip <-
        c("x", "y", "width", "height", "just", "hjust", "vjust",
          "default.units", "name", "vp")
    grid.curve <-
        c("x1", "y1", "x2", "y2", "default.units", "curvature",
          "angle", "ncp", "shape", "square", "squareShape", "inflect",
          "arrow", "open", "debug", "name", "gp", "vp")
    grid.polyline <-
        c("x", "y", "id", "id.lengths", "default.units", "arrow",
          "name", "gp", "vp")
    grid.xspline <-
        c("x", "y", "id", "id.lengths", "default.units", "shape",
          "open", "arrow", "repEnds", "name", "gp", "vp")
    
    .addFunctionInfo(grid.clip = grid.clip,
                     grid.curve = grid.curve,
                     grid.polyline = grid.polyline,
                     grid.xspline = grid.xspline)
}


