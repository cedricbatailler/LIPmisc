#' Returns outlier indices
#'
#' Returns a \code{data.frame} with a specific \code{lm} model's studentized residuals,
#' residuals' cook's distance, and residuals' hat values.
#'
#' @param data A data.frame.
#' @param formula A model formula.
#' @param id A column name from \code{data} used to identify observations.
#'
#' @examples
#' library(magrittr)
#' library(LIPmisc)
#'
#' mtcars %T>%
#'   lm_outliers(mpg ~ disp) %>%
#'   lm(mpg ~ disp,
#'      data = .)
#'
#' @author Dominique Muller, \email{dominique.muller@@univ-grenoble-alpes.fr}
#' @author Cédric Batailler, \email{cedric.batailler@@univ-grenoble-alpes.fr}
#'
#' @references Judd, C. M., McClelland, G. H., & Ryan, C. S. (2009). Data analysis: a model comparison approach (2nd ed). New York ; Hove: Routledge.
#'
#' @keywords outliers
#'
#' @import dplyr
#' @import rlang
#' @importFrom stats model.frame
#' @importFrom stats lm
#' @importFrom stats hatvalues
#' @importFrom stats cooks.distance
#' @importFrom stats rstudent
#'
#' @export

lm_outliers <- function(data, formula, id) {
  # Packages check
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Function
  name_id <- enquo(id)

  fit <- lm(formula, data)
  Out <- model.frame(fit)
  Out$sdr      <- rstudent(fit)
  Out$cookd    <- cooks.distance(fit)
  Out$leverage <- hatvalues(fit)

  data %>%
    select(!!name_id) %>%
    cbind(Out) %>%
    arrange(desc(cookd)) %>%
    print()
}