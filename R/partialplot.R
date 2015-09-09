predict_at_value <- function(obj, new_data, x, xname, ...,
                             inherits_dt = inherits(new_data, "data.table")) {
  if (inherits_dt) {
    tmp_data <- copy(new_data)
    tmp_data[, c(xname) := list(x)]
  } else {
    tmp_data <- new_data
    tmp_data[, xname] <- x
  }
  predict(obj, tmp_data, ...)
}

# iris.rf <- randomForest(Sepal.Width ~ Sepal.Length + Petal.Width + Petal.Length,
#                         data = iris)
# predict_at_value(iris.rf, iris, max(iris$Sepal.Length), "Sepal.Length")
# vapply(5:7, predict_at_value, numeric(nrow(iris)),
#        obj = iris.rf, new_data = iris, xname = "Sepal.Length")
# pd <- partialdep(iris.rf, iris, "Sepal.Length")
# headm(pd)
# partialplot(pd)
# partialplot(pd, ylab = "Width units", xlab = "Length units",
#             main = "Partial dependence of\nlength on width",
#             low_func = function(x) apply(x, 2, quantile, 0.1),
#             high_func = function(x) apply(x, 2, quantile, 0.9))

partialdep <- function(obj, ...) UseMethod("partialdep")

partialdep.randomForest <- function(obj, data, xname, which_class,
                                    n = NULL, w = NULL, na.rm = na.rm,
                                    formatC_args = NULL, ...) {
  inherits_dt <- inherits(data, 'data.table')
  xvar <- if (inherits_dt) data[, xname, with = FALSE] else data[, xname]
  if (is.null(n)) {
    n_unique_x <- if ("data.table" %in% installed.packages())
      uniqueN(xvar)
    else
      length(unique(xvar))
    n <- min(n_unique_x, 51)
  }
  n_rows <- nrow(data)
  if (is.factor(xvar) && !is.ordered(xvar)) {
    stop("Unordered factors (categorical variables) are not yet implemented")
  }
  else {
    if (is.ordered(xvar)) xvar <- as.numeric(xvar)
    if (obj$type == "classification") {
      stop("Classification not yet implemented")
    }
    else {
      x_seq <- seq(min(xvar), max(xvar), length = n)
      pred_y <- vapply(x_seq, predict_at_value, numeric(n_rows),
                       obj = obj, new_data = data, xname = xname,
                       ..., inherits_dt = inherits_dt)
    }
  }
  x_labels <- do.call(formatC, c(list(x = x_seq), formatC_args))
  colnames(pred_y) <- x_labels
  names(dimnames(pred_y)) <- c("Obs. number", "X value")
  structure(pred_y, x_values = x_seq, x_name = xname,
            class = c("partialdep", class(pred_y)))
}

partialplot <- function(obj, ...) UseMethod("partialplot")

partialplot.partialdep <- function(obj, central_func = colMeans,
                                   low_func = function(x) apply(x, 2, quantile, 0.25),
                                   high_func = function(x) apply(x, 2, quantile, 0.75),
                                   ghost = TRUE, rug = TRUE, ...) {
  # to do: allow multiple fences, multiple centers, and "sampled" ghosting
  x_values <- attr(obj, "x_values")
  n_x <- length(x_values)
  center <- central_func(obj)
  assert_that(length(center) == n_x)
  low_fence <- low_func(obj)
  assert_that(length(low_fence) == n_x)
  high_fence <- high_func(obj)
  assert_that(length(high_fence) == n_x)
  y_lim <- if (ghost)
    c_funcs(obj, min, max)
  else
    c_funcs(c(center, low_fence, high_fence), min, max)
  x_name <- attr(obj, "x_name")
  plot_args <- list(...)
  if ("xlab" %!in% names(plot_args))
    plot_args$xlab <- x_name
  if ("ylab" %!in% names(plot_args))
    ylab <- "Response"
  if ("main" %!in% names(plot_args))
    plot_args$main <- sprintf('Partial dependence plot for "%s"', x_name)
  if ("ylim" %!in% names(plot_args))
    plot_args$ylim <- y_lim
  plot_args <- c(list(x = x_values, y = center, type = "n"), plot_args)
  # pass_dots_with_default(func = plot, dots = plot_args,
  #                        default_args = list(x = x_values, y = center, type = "n"))
  if (is.character(rug))
    rug <- match.arg(rug, c("none", "rug", "segments", "axis"))
  if (rug == "axis") {
    plot_args$xaxt <- "n"
    do.call(plot, plot_args)
    axis(1, x_values)
  } else {
    do.call(plot, plot_args)
    if (rug == "rug" || rug == TRUE) {
      rug(x_values, 0.02)
    } else if (rug == "segments") {
      segments(x_values, 0, x_values, center, lty = "dotted", lwd = 0.75)
    }
  }
  if (ghost) {
    ghost_col <- rgb(0.75, 0.75, 0.75, 1 / min(nrow(obj), 50))
    apply(obj, 1, lines, x = x_values, col = ghost_col)
  }
  lines(x_values, center, col = "red", lwd = 1.5)
  lines(x_values, low_fence, col = "blue", lty = "dashed", lwd = 1.5)
  lines(x_values, high_fence, col = "blue", lty = "dashed", lwd = 1.5)
  invisible(cbind(center = center, low_fence = low_fence, high_fence = high_fence))
}

# partialplot <- function(x, ...) UseMethod("partialplot")

# partialplot.randomForest <- function(x, pred.data, x.var, which.class, w, plot = TRUE,
#           add = FALSE, n.pt = min(length(unique(pred.data[, xname])), 51),
#           rug = TRUE, xlab = deparse(substitute(x.var)), ylab = "",
#           main = paste("Partial Dependence on", deparse(substitute(x.var))),
#           lower = x.qt[1], upper = x.qt[2], ...) {
#   classRF <- x$type != "regression"
#   if (is.null(x$forest))
#     stop("The randomForest object must contain the forest.\n")
#   x.var <- substitute(x.var)
#   xname <- if (is.character(x.var))
#     x.var
#   else {
#     if (is.name(x.var))
#       deparse(x.var)
#     else {
#       eval(x.var)
#     }
#   }
#   xv <- pred.data[, xname]
#   n <- nrow(pred.data)
#   if (missing(w))
#     w <- rep(1, n)
#   if (classRF) {
#     if (missing(which.class)) {
#       focus <- 1
#     }
#     else {
#       focus <- charmatch(which.class, colnames(x$votes))
#       if (is.na(focus))
#         stop(which.class, "is not one of the class labels.")
#     }
#   }
#   if (is.factor(xv) && !is.ordered(xv)) {
#     x.pt <- levels(xv)
#     y.pt <- numeric(length(x.pt))
#     for (i in seq(along = x.pt)) {
#       x.data <- pred.data
#       x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
#       if (classRF) {
#         pr <- predict(x, x.data, type = "prob")
#         y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] >
#                                               0, pr[, focus], .Machine$double.eps)) - rowMeans(log(ifelse(pr >
#                                                                                                             0, pr, .Machine$double.eps))), w, na.rm = TRUE)
#       }
#       else y.pt[i] <- weighted.mean(predict(x, x.data),
#                                     w, na.rm = TRUE)
#     }
#     if (add) {
#       points(1:length(x.pt), y.pt, type = "h", lwd = 2,
#              ...)
#     }
#     else {
#       if (plot)
#         barplot(y.pt, width = rep(1, length(y.pt)),
#                 col = "blue", xlab = xlab, ylab = ylab, main = main,
#                 names.arg = x.pt, ...)
#     }
#   }
#   else {
#     if (is.ordered(xv))
#       xv <- as.numeric(xv)
#     x.pt <- seq(min(xv), max(xv), length = n.pt)
#     y.pt <- numeric(length(x.pt))
#     for (i in seq(along = x.pt)) {
#       x.data <- pred.data
#       x.data[, xname] <- rep(x.pt[i], n)
#       if (classRF) {
#         pr <- predict(x, x.data, type = "prob")
#         y.pt[i] <- weighted.mean(log(ifelse(pr[, focus] ==
#                                               0, .Machine$double.eps, pr[, focus])) - rowMeans(log(ifelse(pr ==
#                                                                                                             0, .Machine$double.eps, pr))), w, na.rm = TRUE)
#       }
#       else {
#         y.pt[i] <- weighted.mean(predict(x, x.data),
#                                  w, na.rm = TRUE)
#       }
#     }
#     if (add) {
#       lines(x.pt, y.pt, ...)
#     }
#     else {
#       if (plot)
#         plot(x.pt, y.pt, type = "l", xlab = xlab, ylab = ylab,
#              main = main, ...)
#     }
#     if (rug && plot) {
#       if (n.pt > 10) {
#         rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
#       }
#       else {
#         rug(unique(xv, side = 1))
#       }
#     }
#   }
#   invisible(list(x = x.pt, y = y.pt))
# }
