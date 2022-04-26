#' Convert a grViz object to SVG-text
#' 
#' @param gv grViz object
#' @param width output width in pixels
#' @param height output height in pixels
#' @param clean clean the output of unnecessary stuff
#' 
#' @details width and height allow you to specify the dimensions of the output SVG text. If you only specify one
#' dimension, the other will be computed based on the aspect ratio of the diagram.
#' 
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom xml2 read_xml xml_find_first xml_attr
#' @importFrom magrittr `%>%`
#' @export

gv2svg <- function(gv, width = NULL, height = NULL, clean = TRUE) {

  if (!inherits(gv, "grViz")) stop("gv must be a grViz HTML widget")
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) stop("DiagrammeRsvg is required")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("xml2 is required")
  library(magrittr)

  ## Set default values
  if (is.null(width) && is.null(height)) width <- 300
  
  svg_char <- DiagrammeRsvg::export_svg(gv)

  ## Create an xml object so we can parse it
  svg_xml <- xml2::read_xml(svg_char)
  
  ## Get the <svg> node
  svg_node <- svg_xml %>% xml2::xml_find_first("//d1:svg")
  if (length(svg_node) == 0) stop("Can't find the <svg> tag")

  ## Get the SVG width. We throw away the units because all we care about is the aspect ratio.
  width_orig <- svg_node %>%
    xml2::xml_attr("width") %>%
    gsub("[a-zA-Z]*", "", .) %>% 
    as.numeric()

  ## Get the height
  height_orig <- svg_node %>%
    xml2::xml_attr("height") %>%
    gsub("[a-zA-Z]*", "", .) %>% 
    as.numeric()
  
  ## Get the aspect ratio
  hw_ratio <- height_orig / width_orig

  ## Compute the output width if needed
  if (is.null(width)) {
    ## Height must have been passed, which we can use to set the width
    width <- round(height / hw_ratio)
  }
  
  ## Compute the required width and height
  if (is.null(height)) {
    ## Width must have been passed, which we can use to set the width
    height <- (width * hw_ratio) %>% round()
  }
  
  
  ## Update the height and width attributes
  xml2::xml_attr(svg_node, "height") <- paste0(height, "px")
  xml2::xml_attr(svg_node, "width") <- paste0(width, "px")
  
  ## Get the SVG-text to return
  svg_return <- svg_xml %>% as.character() 

  ## Remove unneeded tags and carriage returns
  if (clean) {
    svg_return <- svg_return %>% gsub("<!--.*?-->", "", .) %>% gsub("\\n", "", .)
  }
  
  svg_return

}
