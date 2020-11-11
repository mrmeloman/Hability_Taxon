function (x.results, x.dim.results, x.cat.results, parameters, 
          procedure) 
{
  if (procedure == "MAMBAC") {
    n <- parameters$n.cuts
    x.values <- 1:n
    y.values <- apply(x.results$MAMBAC, 2, mean)
    x.label <- "Cut"
    y.label <- "Mean Difference"
  }
  if (procedure == "MAXEIG") {
    n <- parameters$windows
    x.values <- apply(x.results$MAXEIG.x, 2, mean)
    y.values <- apply(x.results$MAXEIG.y, 2, mean)
    x.label <- "Score"
    y.label <- "Eigenvalue"
  }
  if (procedure == "MAXSLOPE") {
    n <- parameters$n - 1
    x.values <- apply(x.results$MAXSLOPE.x, 2, mean)
    y.values <- apply(x.results$MAXSLOPE.y, 2, mean)
    x.label <- "Score"
    y.label <- "LOWESS Slope"
  }
  if (procedure == "LMode") {
    n <- 512
    x.values <- x.results$LMode.x
    y.values <- x.results$LMode.y
    x.label <- "Factor Score"
    y.label <- "Density"
  }
  cat.sum <- matrix(0, nrow = 6, ncol = n)
  dim.sum <- matrix(0, nrow = 6, ncol = n)
  for (i in 1:n) {
    if (procedure == "MAMBAC") {
      cat.sum[, i] <- summary(x.cat.results$MAMBAC[, i])
      dim.sum[, i] <- summary(x.dim.results$MAMBAC[, i])
    }
    if (procedure == "MAXEIG") {
      cat.sum[, i] <- summary(x.cat.results$MAXEIG.y[, 
                                                     i])
      dim.sum[, i] <- summary(x.dim.results$MAXEIG.y[, 
                                                     i])
    }
    if (procedure == "MAXSLOPE") {
      cat.sum[, i] <- summary(x.cat.results$MAXSLOPE.y[, 
                                                       i])
      dim.sum[, i] <- summary(x.dim.results$MAXSLOPE.y[, 
                                                       i])
    }
    if (procedure == "LMode") {
      cat.sum[, i] <- summary(x.cat.results$LMode.y[, 
                                                    i])
      dim.sum[, i] <- summary(x.dim.results$LMode.y[, 
                                                    i])
    }
  }
  y.range <- c(min(c(y.values, cat.sum[1, ], dim.sum[1, ])), 
               max(c(y.values, cat.sum[6, ], dim.sum[6, ])))
  main.title <- paste(procedure, "\nCategorical Comparison Data")
  plot(x.values, y.values, type = "l", ylim = y.range, xlab = x.label, 
       ylab = y.label, main = main.title, cex.main = 0.75, 
       cex.axis = 0.75, cex.lab = 0.75)
  n <- length(x.values)
  cat(main.title)
  cat("\nx.values\n")
  cat(x.values)
  cat("\ny.values\n")
  cat(y.values)
  cat("\ny.range\n")
  cat(y.range)
  cat("\npolygon.x\n")
  cat(c(x.values[1:n], x.values[n:1]))
  cat("\npolygon.y\n")
  cat(c(cat.sum[5, 1:n], cat.sum[2, n:1]))
  cat("\nline 1.y\n")
  cat(cat.sum[1, ])
  cat("\nline 2.y\n")
  cat(cat.sum[6, ])
  polygon(x = c(x.values[1:n], x.values[n:1]), y = c(cat.sum[5, 
                                                             1:n], cat.sum[2, n:1]), col = 8, lty = 0)
  lines(x.values, cat.sum[1, ])
  lines(x.values, cat.sum[6, ])
  lines(x.values, y.values, lwd = 3)
  if (procedure == "LMode") {
    max.l <- max(smooth(y.values)[x.values <= parameters$mode.l])
    loc.l <- (1:n)[smooth(y.values) == max.l]
    if (length(loc.l) > 1) {
      loc.l <- loc.l[1]
    }
    max.r <- max(smooth(y.values)[x.values >= parameters$mode.r])
    loc.r <- (1:n)[smooth(y.values) == max.r]
    if (length(loc.r) > 1) {
      loc.r <- loc.r[1]
    }
    cat("lmode.line1.x")
    cat(rep(x.values[loc.l], 2))
    cat("lmode.line1.y")
    cat(c(0, y.values[loc.l]))
    cat("lmode.line2.x")
    cat(rep(x.values[loc.r], 2))
    cat("lmode.line2.y")
    cat(c(0, y.values[loc.r]))
    lines(x = rep(x.values[loc.l], 2), y = c(0, y.values[loc.l]))
    lines(x = rep(x.values[loc.r], 2), y = c(0, y.values[loc.r]))
  }
  main.title <- paste(procedure, "\nDimensional Comparison Data")
  plot(x.values, y.values, type = "l", ylim = y.range, xlab = x.label, 
       ylab = y.label, main = main.title, cex.main = 0.75, 
       cex.axis = 0.75, cex.lab = 0.75)
  n <- length(x.values)
  cat(main.title)
  cat("\npolygon.y\n")
  cat(c(dim.sum[5, 1:n], dim.sum[2, n:1]))
  cat("\nline 1.y\n")
  cat(dim.sum[1, ])
  cat("\nline 2.y\n")
  cat(dim.sum[6, ])
  polygon(x = c(x.values[1:n], x.values[n:1]), y = c(dim.sum[5, 
                                                             1:n], dim.sum[2, n:1]), col = 8, lty = 0)
  lines(x.values, dim.sum[1, ])
  lines(x.values, dim.sum[6, ])
  lines(x.values, y.values, lwd = 3)
  if (procedure == "LMode") {
    lines(x = rep(x.values[loc.l], 2), y = c(0, y.values[loc.l]))
    lines(x = rep(x.values[loc.r], 2), y = c(0, y.values[loc.r]))
  }
}
