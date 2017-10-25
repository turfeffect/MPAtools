#' Create a regression table
#'
#' @name reg_table
#'
#' @param model An object of class lm
#'
#' @param title A title for the table
#' @param dep.var.labels Labels for dependent variables
#'
#' @export
#'
#' @author Juan Carlos Villasenor
#'
#' @importFrom magrittr %>%

reg_table <- function(model, title = "", dep.var.labels = ""){

  terms <- length(coef(model))-1

  glance <- broom::glance(model)

  RSE <- paste0(formatC(glance$sigma, digits = 3, format = "f"), " (df = ", glance$df.residual, ")")

  R2 <- formatC(glance$r.squared, digits = 3, format = "f")

  N <- nobs(model)

  descriptors <- data.frame(Variable = c("Obs.", "RSE", "R2"),
                            Valor = c(N, RSE, R2))

  robust_se(model) %>%
    rename(Variable = term) %>%
    mutate(Variable = ifelse(Variable == "(Intercept)", "Constante", Variable),
           Variable = ifelse(Variable == "ZonaReserva:Post1", "Efecto", Variable),
           p1 = ifelse(p.value < 0.1, "*", ""),
           p2 = ifelse(p.value < 0.05, "*", ""),
           p3 = ifelse(p.value < 0.001, "*", ""),
           p = paste0(p1, p2, p3),
           estimate = formatC(estimate, digits = 3, format = "f"),
           std.error = formatC(std.error, digits = 3, format = "f"),
           Valor = paste0(estimate, " (", std.error, ")", p)) %>%
    select(Variable, Valor) %>%
    rbind(descriptors) %>%
    knitr::kable(caption = title, col.names = c("", dep.var.labels), format = "latex", booktabs = T) %>%
    kableExtra::kable_styling() %>%
    kableExtra::group_rows("Coeficientes", 1, terms) %>%
    kableExtra::group_rows("Estadisticos", terms+1, terms+3)
}
