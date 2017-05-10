reg_table <- function(model, title, dep.var.labels){

  TidyModel <- model%>%
    lmtest::coeftest(vcov = sandwich::vcovHC(., type = "HC1")) %>%
    broom::tidy()

  robust_se <- TidyModel$std.error %>%
    set_names(TidyModel$term)

  covariate.labels <- model %>%
    coefficients() %>%
    names() %>%
    gsub(pattern = "ZonaReserva:Post1", replacement = "**Efecto**") %>%
    gsub(pattern = "Intercept", replacement = "Constante")


  stargazer::stargazer(model,
                       se = list(robust_se),
                       title = title,
                       dep.var.labels = dep.var.labels,
                       covariate.labels = covariate.labels,
                       t.auto = T,
                       p.auto = T,
                       header = F,
                       type = "latex",
                       single.row = T,
                       intercept.bottom = F,
                       dep.var.caption = "Indicador:")
}
