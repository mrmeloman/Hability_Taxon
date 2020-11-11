library("RTaxometrics")
library("dplyr")

PlotPanel <-
  function(x.results, x.dim.results, x.cat.results, parameters, 
           procedure) {
    # 
    # Plots two-panel graph with results for empirical and comparison data.
    #
    # Args:
    #       x.results: Empirical data results (list).
    #   x.dim.results: Dimensional comparison data results (list).
    #   x.cat.results: Categorical comparison data results (list).
    #      parameters: Data and program parameters (list).
    #       procedure: Name of taxometric procedure (text).
    #
    # Returns:
    #   Nothing; graphical output only.
    #
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
        cat.sum[, i] <- summary(x.cat.results$MAXEIG.y[, i])
        dim.sum[, i] <- summary(x.dim.results$MAXEIG.y[, i])
      }
      if (procedure == "MAXSLOPE") {
        cat.sum[, i] <- summary(x.cat.results$MAXSLOPE.y[, i])
        dim.sum[, i] <- summary(x.dim.results$MAXSLOPE.y[, i])
      }
      if (procedure == "LMode") {
        cat.sum[, i] <- summary(x.cat.results$LMode.y[, i])
        dim.sum[, i] <- summary(x.dim.results$LMode.y[, i])
      }
    }
    y.range <- c(min(c(y.values, cat.sum[1, ], dim.sum[1, ])), 
                 max(c(y.values, cat.sum[6, ], dim.sum[6, ])))
    main.title <- paste(procedure, "\nCategorical Comparison Data")
    plot(x.values, y.values, type = "l", ylim = y.range, xlab = x.label, 
         ylab = y.label, main = main.title, cex.main = .75, cex.axis = .75, 
         cex.lab = .75)
    n <- length(x.values)
    polygon(x = c(x.values[1:n], x.values[n:1]), 
            y = c(cat.sum[5, 1:n], cat.sum[2, n:1]), col = 8, lty = 0)
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
      lines(x = rep(x.values[loc.l], 2), y = c(0, y.values[loc.l]))
      lines(x = rep(x.values[loc.r], 2), y = c(0, y.values[loc.r]))
    }
    main.title <- paste(procedure, "\nDimensional Comparison Data")
    plot(x.values, y.values, type = "l", ylim = y.range, xlab = x.label, 
         ylab = y.label, main = main.title, cex.main = .75, cex.axis = .75, 
         cex.lab = .75)
    n <- length(x.values)
    polygon(x = c(x.values[1:n], x.values[n:1]), 
            y = c(dim.sum[5, 1:n], dim.sum[2, n:1]), col = 8, lty = 0)
    lines(x.values, dim.sum[1, ])
    lines(x.values, dim.sum[6, ])
    lines(x.values, y.values, lwd = 3)
    if (procedure == "LMode") {
      lines(x = rep(x.values[loc.l], 2), y = c(0, y.values[loc.l]))
      lines(x = rep(x.values[loc.r], 2), y = c(0, y.values[loc.r]))
    }
  }

data <- read.csv("oxford_hgshsa_260820_modded.csv", sep = ";")

data <- na.omit(data)

invol <- select(data, INV.1:INV.12)

invol15 <- ClassifyCases(invol, p = 0.15)

invol10 <- ClassifyCases(invol, p = 0.10)

invol_purified <- select(data, INV.3:INV.10)

invol_pur15 <- ClassifyCases(invol_purified, p = 0.15)

invol_pur10 <- ClassifyCases(invol_purified, p = 0.10)

## Involuntariness data: subscales

#Create subscales:
invol$ideomotor <- invol$INV.1 + invol$INV.2 + invol$INV.3 + invol$INV.7
invol$challenge <- invol$INV.4 + invol$INV.5 + invol$INV.6 + invol$INV.8 + invol$INV.10
invol$cogper <- invol$INV.9 + invol$INV.11
invol$amnesia <- invol$INV.12


invol_subscales15 <- select(invol, ideomotor, challenge, cogper, amnesia)
invol_subscales15 <- ClassifyCases(invol_subscales15, p = 0.15)


invol_subscales10 <- select(invol, ideomotor, challenge, cogper, amnesia)
invol_subscales10 <- ClassifyCases(invol_subscales10, p = 0.10)

## Involuntariness data: subscales ("bad" items removed)

#Create subscales

invol_purified$ideomotor <- invol_purified$INV.3 + invol_purified$INV.7
invol_purified$challenge <- invol_purified$INV.4 + invol_purified$INV.5 + invol_purified$INV.6 + invol_purified$INV.8 + invol_purified$INV.10
invol_purified$cogper <- invol_purified$INV.9


invol_subscales_pur15 <- select(invol_purified, ideomotor, challenge, cogper)
invol_subscales_pur15 <- ClassifyCases(invol_subscales_pur15, p = 0.15)


invol_subscales_pur10 <- select(invol_purified, ideomotor, challenge, cogper)
invol_subscales_pur10 <- ClassifyCases(invol_subscales_pur10, p = 0.10)



## Hypnotic suggestibility data: separate items

#Create a H. suggestibility subset
hab <- select(data, HGSHS.A1:HGSHS.A11, HGSHS.A12)

hab15 <- ClassifyCases(hab, p = 0.15)

hab10 <- ClassifyCases(hab, p = 0.10)


## Hypnotic suggestibility data: separate items ("bad" items removed)

#Create a H. suggestibility subset without "bad" items
hab_purified <- select(data,  HGSHS.A3:HGSHS.A10)

hab_pur15 <- ClassifyCases(hab_purified, p = 0.15)

hab_pur10 <- ClassifyCases(hab_purified, p = 0.10)



## Hypnotic suggestibility data: subscales

#Create subscales
hab$ideomotor <- hab$HGSHS.A1 + hab$HGSHS.A2 + hab$HGSHS.A3 + hab$HGSHS.A7
hab$challenge <- hab$HGSHS.A4 + hab$HGSHS.A5 + hab$HGSHS.A6 + hab$HGSHS.A8 + hab$HGSHS.A10
hab$cogper <- hab$HGSHS.A9 + hab$HGSHS.A11
hab$amnesia <- hab$HGSHS.A12


hab_subsc15 <- select(hab, ideomotor, challenge, cogper, amnesia)
hab_subsc15 <- ClassifyCases(hab_subsc15, p = 0.15)


hab_subsc10 <- select(hab, ideomotor, challenge, cogper, amnesia)
hab_subsc10 <- ClassifyCases(hab_subsc10, p = 0.10)


## Hypnotic suggestibility data: subscales ("bad" items removed)

#Create subscales
hab_purified$ideomotor <- hab_purified$HGSHS.A3 + hab_purified$HGSHS.A7
hab_purified$challenge <- hab_purified$HGSHS.A4 + hab_purified$HGSHS.A5 + hab_purified$HGSHS.A6 + hab_purified$HGSHS.A8 + hab_purified$HGSHS.A10
hab_purified$cogper <- hab_purified$HGSHS.A9


hab_subsc_pur15 <- select(hab, ideomotor, challenge, cogper)
hab_subsc_pur15 <- ClassifyCases(hab_subsc_pur15, p = 0.15)


hab_subsc_pur10 <- select(hab, ideomotor, challenge, cogper)
hab_subsc_pur10 <- ClassifyCases(hab_subsc_pur10, p = 0.10)


# Performing the analysis

## Involuntariness data: separate items

### P = 0.15

cat("invol15")
RunTaxometrics(invol15) # Я дома

cat("invol10")
RunTaxometrics(invol10) # Я дома

cat("invol_pur15")
RunTaxometrics(invol_pur15) #Драгон

cat("invol_pur10")
RunTaxometrics(invol_pur10) #Драгон

cat("invol_subscales15")
RunTaxometrics(invol_subscales15) # Я ноут

cat("invol_subscales10")
RunTaxometrics(invol_subscales10) # Я ноут

cat("invol_subscales_pur15")
RunTaxometrics(invol_subscales_pur15) # Шляпа

cat("invol_subscales_pur10")
RunTaxometrics(invol_subscales_pur10) # Я дома

cat("hab15")
RunTaxometrics(hab15) #Шляпа

cat("hab10")
RunTaxometrics(hab10) #Драгон

cat("hab_pur15")
RunTaxometrics(hab_pur15) # Вика

cat("hab_pur10")
RunTaxometrics(hab_pur10) # Вика

cat("hab_subsc15")
RunTaxometrics(hab_subsc15) # Я ноут

cat("hab_subsc10")
RunTaxometrics(hab_subsc10) # Я дома

cat("hab_subsc_pur15")
RunTaxometrics(hab_subsc_pur15) # Я дома

cat("hab_subsc_pur10")
RunTaxometrics(hab_subsc_pur10) # Я дома

#CCFI Profile for selected models:

RunCCFIProfile(select(invol, ideomotor, challenge, cogper, amnesia))

RunCCFIProfile(select(hab, ideomotor, challenge, cogper, amnesia))

