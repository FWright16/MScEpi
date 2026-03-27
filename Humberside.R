# load packages
if (!require("pacman")) install.packages("pacman")

pkgs <- c(
  "spatstat.data",
  "spatstat.geom",
  "spatstat.explore",
  "spatstat.core",
  "dplyr",
  "sf",
  "mapview",
  "sparr"
)
pacman::p_load(char = pkgs)

# load the Humberside dataset
data(humberside, package = "spatstat.data")

# inspect the object
humberside
summary(humberside)

humberside_df <- data.frame(
  X = humberside$x,
  Y = humberside$y,
  case = as.character(marks(humberside))
)

head(humberside_df, 10)
table(humberside_df$case)

humberside_sf <- humberside_df %>%
  st_as_sf(coords = c("X", "Y"), crs = NA)

mapview(humberside_sf, zcol = "case", cex = 3, homebutton = FALSE)

plot(humberside,cols=c('red3','navy'))
summary(humberside)

# count cases and controls
table(marks(humberside))

# total intensity
n <- humberside$n
A <- area.owin(Window(humberside))
lambda <- n / A
lambda

# intensity for cases
n_case <- sum(marks(humberside) == "case")
lambda_case <- n_case / A
lambda_case

# intensity for controls
n_control <- sum(marks(humberside) == "control")
lambda_control <- n_control / A
lambda_control

split_h <- split(humberside)
cases_h <- split_h$case
controls_h <- split_h$control

summary(cases_h)
summary(controls_h)

Kcases <- Kest(cases_h, correction = "isotropic", rmax = 60)
plot(Kcases)

set.seed(123)

plot(
  envelope(cases_h, nsim = 1000, rmax = 60),
  main = "Ripley's K for cases with simulation envelope"
)

Kcontrols <- Kest(controls_h, correction = "isotropic", rmax = 60)
plot(Kcontrols)

set.seed(123)

plot(
  envelope(controls_h, nsim = 1000, rmax = 60),
  main = "Ripley's K for controls with simulation envelope"
)

D <- Kcases$iso - Kcontrols$iso
r <- Kcases$r

plot(
  r, D, type = "l", lwd = 2,
  xlab = "Distance (100 m units)",
  ylab = "D(r) = Kcases(r) - Kcontrols(r)",
  main = "Difference between case and control K-functions"
)
abline(h = 0, lty = 2, col = "red")


envelopeD_local <- function(pp, nsim = 99, rmax = 60, correction = "isotropic") {
  
  obs_split <- split(pp)
  K_case_obs <- Kest(obs_split$case, correction = correction, rmax = rmax)
  K_ctrl_obs <- Kest(obs_split$control, correction = correction, rmax = rmax)
  corr_col <- if (correction == "isotropic") "iso" else correction
  
  r <- K_case_obs$r
  D_obs <- K_case_obs[[corr_col]] - K_ctrl_obs[[corr_col]]
  
  m <- marks(pp)
  
  sim_D <- replicate(nsim, {
    pp_perm <- pp
    marks(pp_perm) <- sample(m, replace = FALSE)
    sp <- split(pp_perm)
    K_case <- Kest(sp$case, correction = correction, rmax = rmax)
    K_ctrl <- Kest(sp$control, correction = correction, rmax = rmax)
    K_case[[corr_col]] - K_ctrl[[corr_col]]
  })
  
  lower <- apply(sim_D, 1, quantile, probs = 0.025, na.rm = TRUE)
  upper <- apply(sim_D, 1, quantile, probs = 0.975, na.rm = TRUE)
  
  plot(
    r, D_obs, type = "l", lwd = 2,
    xlab = "Distance (100 m units)",
    ylab = "D(r)",
    main = "Difference in K-functions with permutation envelope"
  )
  lines(r, lower, lty = 2, col = "grey40")
  lines(r, upper, lty = 2, col = "grey40")
  abline(h = 0, lty = 2, col = "red")
  
  invisible(data.frame(r = r, D_obs = D_obs, lower = lower, upper = upper))
}

set.seed(123)
D_env <- envelopeD_local(humberside, nsim = 99, rmax = 60)

h <- LSCV.risk(f = cases_h, g = controls_h)
h

lambda_cases <- bivariate.density(cases_h, h0 = h)
lambda_controls <- bivariate.density(controls_h, h0 = h)

rr_humber <- risk(f = lambda_cases, g = lambda_controls, tolerate = TRUE)
plot(rr_humber, main = "Log-relative risk")

plot(rr_humber, main = "Log-relative risk")
plot(cases_h, add = TRUE, cex=0.4, cols = "green")


table(marks(humberside))

split_h <- split(humberside)
names(split_h)

cases_h <- split_h$case
controls_h <- split_h$control

summary(cases_h)
summary(controls_h)

h <- LSCV.risk(f = cases_h, g = controls_h)
h

lambda_cases <- bivariate.density(cases_h, h0 = h)
lambda_controls <- bivariate.density(controls_h, h0 = h)

rr_humber <- risk(f = lambda_cases, g = lambda_controls, tolerate = TRUE)

plot(rr_humber, main = "Log-relative risk")
plot(cases_h, add = TRUE, pch = 16, cols = "black")

plot(cases_h, main = "Cases")
plot(controls_h, main = "Controls")

plot(lambda_cases, main = "Case intensity")
plot(lambda_controls, main = "Control intensity")


controls_h_u <- unique.ppp(controls_h)

h2 <- LSCV.risk(f = cases_h, g = controls_h_u)
h2

lambda_cases2 <- bivariate.density(cases_h, h0 = h2)
lambda_controls2 <- bivariate.density(controls_h_u, h0 = h2)

rr_humber2 <- risk(f = lambda_cases2, g = lambda_controls2, tolerate = TRUE)

plot(rr_humber2, main = "Log-relative risk (unique controls)")
plot(cases_h, add = TRUE, pch = 16, cols = "black")

lambda_cases3 <- bivariate.density(cases_h, h0 = 60)
lambda_controls3 <- bivariate.density(controls_h, h0 = 60)

rr_humber3 <- risk(f = lambda_cases3, g = lambda_controls3, tolerate = TRUE)

plot(rr_humber3, main = "Log-relative risk (h = 60)")
plot(cases_h, add = TRUE, pch = 16, cols = "black")

lambda_cases4 <- bivariate.density(cases_h, h0 = 80)
lambda_controls4 <- bivariate.density(controls_h, h0 = 80)

rr_humber4 <- risk(f = lambda_cases4, g = lambda_controls4, tolerate = TRUE)

plot(rr_humber4, main = "Log-relative risk (h = 80)")
plot(cases_h, add = TRUE, pch = 16, cols = "black")




