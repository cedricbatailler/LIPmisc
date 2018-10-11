#' Convinience function to convert F value to partial eta squared.
#'
#' @param f Fisher's F value.
#' @param df_1 Degree of freedom for the effect.
#' @param df_2 Degree of freedom for the error term.
#'
#' @return A numeric.
#' @export

f_to_p_eta <- function(f, df_1 = 1, df_2) {
  
  p_eta <- 
    (f * df_1) / (f * df_1 + df_2)
  
  p_eta
}
