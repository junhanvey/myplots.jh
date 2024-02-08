#' Create a histogram
#'
#' This will graph a single numerical variable given as a vector. 
#'
#' @param x This is the vector to be plotted.
#' @param color This is the color of the outline of the bars 
#' @param fill This is the color of the filled in bars
#' @param bins This is the number of bins to make
#'
#' @return This function returns a ggplot histogram
#'
#' @examples
#' ## Create a histogram of x
#' x <- rnorm(100, mean = 10, sd = 5)
#' 
#' ghist(x)
#' 
#' ## Default of binwidth is 5. If we want more or less:
#' ghist(x, bins = 2)
#'
#' @import
#'   ggplot2
#'   magrittr
#'  
#'
#' @export 

ghist <- function(x, color = "hotpink",
                   fill = 'lightpink', bins = 5) {
  data.frame(x) |> 
    ggplot(aes(x = x)) +
    geom_histogram(color = color,
               fill = fill,
               binwidth = bins
               ) +
    labs(x = "X", y = "Count")
}
