#' @export

reg_table <- function(model, title, dep.var.labels){

  TidyModel <- model%>%
    lmtest::coeftest(vcov = sandwich::vcovHC(., type = "HC1")) %>%
    broom::tidy()

  robust_se <- TidyModel$std.error %>%
    set_names(TidyModel$term)

  tnum <- seq(1:length(robust_se))[names(robust_se) == "ZonaReserva"]

  robust_se <- c(robust_se[1:tnum], Post1 = NA, robust_se[(tnum+1):(length(robust_se)-1)], `**Efect**` = robust_se[length(robust_se)])

  covariate.labels <- model %>%
    coefficients() %>%
    names() %>%
    gsub(pattern = "ZonaReserva:Post1", replacement = "**Efecto**") %>%
    gsub(pattern = "Intercept", replacement = "Constante")

  print(robust_se)

  # Adjust F statistic
  # wald_results <- waldtest(output, vcov = cov1


  stargazer::stargazer(model,
                       title = title,
                       dep.var.caption = "Indicador:",
                       dep.var.labels = dep.var.labels,
                       covariate.labels = covariate.labels,
                       se = list(robust_se),
                       t.auto = T,
                       p.auto = T,
                       header = F,
                       type = "latex",
                       single.row = T,
                       intercept.bottom = F,
                       star.cutoffs = c(0.1, 0.05, 0.001),
                       report = c("vcsp*"))
}
