robust_se <- function(model){
  model %>%
    lmtest::coeftest(vcov = sandwich::vcovHC(., type = "HC1")) %>%
    broom::tidy()
}
