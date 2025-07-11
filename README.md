# Metabolic-syndrome
Global metabolic syndrome prevalence modelling

##Women
ff <- bf(Cases | trials(Sample) ~ ns(I(Ages-50), knots = c(-10, 10, 15), Boundary.knots = c(-30, 30)) + Sites*log(GDP) + Urban + Definition + (1 + Ages | Region) +  (1 | Subregion/Country), family = binomial("logit"))

priorsf <- c(prior(normal(-1.4, 0.5), class = "Intercept"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM101015Boundary.knotsEQcM30301"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM101015Boundary.knotsEQcM30302"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM101015Boundary.knotsEQcM30303"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM101015Boundary.knotsEQcM30304"),
prior(normal(0.3, 0.2), class = "b", coef = "logGDP"),
  prior(normal(0.3, 0.2), class = "b", coef = "Urban"),
  prior(normal(0, 0.5), class = "b", coef = "SitesOthers"),
  prior(normal(0, 0.5), class = "b", coef = "SitesOthers:logGDP"),
  prior(normal(0, 0.5), class = "b", coef = "DefinitionJISMF80"),
  prior(lkj(2), class = "cor"),
  prior(student_t(3, 0, 1), class = "sd", group = "Region"),
  prior(student_t(3, 0, 1), class = "sd", group = "Region", coef = "Ages"),
  prior(student_t(3, 0, 1), class = "sd", group = "Subregion"),
  prior(student_t(3, 0, 1), class = "sd", group = "Subregion:Country"))

mf <- brm(formula = ff, data = datawomen, prior = priorsf, chains = 4, cores = 8, iter = 5000,      warmup = 2500, control = list(adapt_delta = 0.999, max_treedepth = 20),  threads = threading(2), backend = "cmdstanr", save_pars = save_pars(all = T), refresh = 1000, seed = 1234, init = NULL, silent = T)

##Men
fm <- bf(Cases | trials(Sample) ~ ns(I(Ages-50), knots = c(-15, 15, 25), Boundary.knots = c(-30, 30)) + Representativeness*log(GDP) + Urban + Definition + (1 + Ages | Region) + (1 | Subregion/Country), family = binomial("logit"))

priorsm <- c(prior(normal(-1.4, 0.5), class = "Intercept"),
 prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM151525Boundary.knotsEQcM30301"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM151525Boundary.knotsEQcM30302"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM151525Boundary.knotsEQcM30303"),
  prior(normal(0, 0.5), class = "b", coef = "nsIAgesM50knotsEQcM151525Boundary.knotsEQcM30304"),
prior(normal(0.3, 0.2), class = "b", coef = "logGDP"),
  prior(normal(0.3, 0.2), class = "b", coef = "Urban"),
  prior(normal(0, 0.5), class = "b", coef = "SitesOthers"),
  prior(normal(0, 0.5), class = "b", coef = "SitesOthers:logGDP"),
  prior(normal(0, 0.5), class = "b", coef = "DefinitionJISMM94F80"),
  prior(normal(0, 0.5), class = "b", coef = "DefinitionOthers"),
  prior(lkj(2), class = "cor"),
  prior(student_t(3, 0, 1), class = "sd", group = "Region"),
  prior(student_t(3, 0, 1), class = "sd", group = "Region", coef = "Ages"),
  prior(student_t(3, 0, 1), class = "sd", group = "Subregion"),
  prior(student_t(3, 0, 1), class = "sd", group = "Subregion:Country"))

mm <- brm(formula = fm, data = datamen, prior = priorsm, chains = 4, cores = 8, iter = 5000, warmup = 2500, control = list(adapt_delta = 0.999, max_treedepth = 20),  threads = threading(2), backend = "cmdstanr", save_pars = save_pars(all = T), refresh = 1000, seed = 1234, init = NULL, silent = T)
