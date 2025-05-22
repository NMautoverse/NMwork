# Take script path and test if it is too long to fit on a typical page. If it is
# too long, wrap it onto multiple lines. This is more involved than one may
# think as shown here.

##' @import stringr
##' @import magrittr
##' @import data.table
##' @import dplyr
##' @export
##' 
##' @param text string to be wrapped
##' @param page_width  
##' @param font_size 
##' @param font_family 

wrapTextToLength = function(
  text = "Source code: path/to/some/file/thatIsVery/Very/Veryvery/looooooooonnnnng/scripts/testing.R", 
  page_width = 6.26,
  font_size = 7,
  font_family = "sans"
  ){
  # A4 paper = 210 mm = 21cm = 8.267 inch
  # margins of 1 inch on left/right = 6.267 inches of space
  # default_fig_width = 6.26
  default_fig_width = page_width
  default_script_label_font_size = font_size
  default_script_label_font_family = font_family
  
  # this section of code is within a call to pdf() which looks strange, but is required because you can only calculate the size of space required for a string of a certain font using the graphics device
  pdf(NULL)
  par(ps = default_script_label_font_size, family = default_script_label_font_family)
  len = strwidth(text, units = "inches")
  
  if(len > default_fig_width) {
    # widths of individual characters in inches at the given font size
    charwidths = stringr::str_split_1(text, pattern = "") %>% strwidth( units = "inches")
    char_length_to_wrap_to = 
      data.table::data.table(charindex = 1:length(charwidths), charwidths) %>% 
      dplyr::mutate(cumlen = cumsum(charwidths)) %>% 
      dplyr::mutate(over = cumlen > default_fig_width*0.98) %>% 
      dplyr::filter(over==FALSE) %>% 
      dplyr::slice(n()) %>% 
      dplyr::pull(charindex)
    wrapped_text <- stringr::str_wrap(text, width = char_length_to_wrap_to, whitespace_only = FALSE)  
  } else {
    wrapped_text = text 
  }
  dev.off()
  return(wrapped_text)
}
