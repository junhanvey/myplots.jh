#' Create a quick box plot in ggplot.
#'
#' This will graph a given vector as a ggplot boxplot object
#' @param x This is vector to be plotted
#' @param y This is an optional categorical variable
#' @param color This is the color of the outline 
#' @param fill This is the color that will fill in the boxplot (if categorical variable is given, the colors will also correspond to this option.)
#' @param horizontal TRUE or FALSE
#'
#'
#' @return This function returns a ggplot box plot object.
#'
#' @examples
#' ## Create a box plot of one vector
#' library(datasets)
#' data(iris)
#' gbox(iris$Petal.Length)
#' 
#' ## Create a Box plot of a numeric variable split by a categorical one.
#' 
#' gbox(iris$Petal.Width, iris$Species, hz = TRUE)
#' 
#'
#' @import
#'   ggplot2
#'   magrittr
#'
#' @export

gbox <- function(x, y = "", color = "hotpink", fill = "lightpink", hz = FALSE) {
  if(is.null(y)){
  plt <- data.frame(x) |>
    ggplot(aes(x = x)) +
    geom_boxplot(color = color, fill = fill)

  }
  if(!is.null(y)) {
    plt <- data.frame(x) |>
      ggplot(aes(x = x, y = y, fill = y)) +
      geom_boxplot(color = "Black") +
      scale_fill_manual(values = c("lightpink","cornflowerblue", "palegreen", "lightyellow",
                                   "plum1", "cadetblue3"))
  }
  
  if(!is.null(y) && hz == TRUE) {
    plt <- data.frame(x) |> 
      ggplot(aes(x = x, y = y, fill = y)) +
      geom_boxplot(color = "Black") +
      scale_fill_manual(values = c("lightpink","cornflowerblue", "palegreen", "lightyellow",
                                   "plum1", "cadetblue3")) +
      coord_flip()
  }
  
  plt
 
}

