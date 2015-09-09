    get_biplot_data <- function(...) UseMethod('get_biplot_data')
    get_biplot_data.prcomp <- function(x, scale = 1, pc.biplot = FALSE) {
      # taken from stats:::biplot.prcomp
      if (!scale) {
        # `scale = 0` recovers original scores anyway, but this is a shortcut
        list(
          scores = x$x,
          axes = x$rotation
        )
      } else {
        scores <- x$x
        root_n <- sqrt(nrow(scores))
        lam <- (x$sdev * root_n) ^ scale
        if (pc.biplot) lam <- lam / root_n
        list(
          scores = t(t(scores) / lam),
          axes = t(t(x$rotation) / lam)
        )
      }
    }
