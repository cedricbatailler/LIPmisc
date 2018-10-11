#' Export a plot to a powerpoint document.
#'
#' @param plot_code Plot code. Defalut to ggplot2::last_plot()
#' @param file Path where the .pptx file should be saved.
#' @param w_r Wide to height ratio. Numeric.
#'
#' @return NULL
#' @export
#' 
plot_to_pptx <- 
  function(plot_code = ggplot2::last_plot(), file, w_r = 1, template = NULL) {
  
    if(is.null(template)) {
      template_location <- 
        system.file('pptx', 'template.pptx', package = "LIPmisc")
    }
    else {
      template_location <- template
    }
  
  
  officer::read_pptx(template_location) %>% 
    rvg::ph_with_vg_at(code   = print({plot_code}), 
                       left   = 0.1,
                       top    = 0.1,
                       width  = 9.8 / w_r,
                       height = 7.3) %>% 
    print(target = file) %>% 
    invisible()
}
