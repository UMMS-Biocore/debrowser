#' plotSizeMarginsUI
#'
#' Size and margins module for plotly plots
#'
#' @note \code{plotSizeMarginsUI}
#' @param id, id
#' @param h, height
#' @param w, width
#' @param t, top margin	
#' @param b, bottom margin	
#' @param l, left margin	
#' @param r, right margin
#' @return size and margins controls
#' @examples
#'     x <- plotSizeMarginsUI("heatmap")
#' @export
#'
plotSizeMarginsUI <- function(id, w=800, h=640, t=20, b=100, l=100, r=20) {
    shinydashboard::menuItem(paste0(id, " - Size & Margins"),
    plotSizeUI(id, w, h),
    plotMarginsUI(id, t, b, l, r),
    plotTypeUI(id)
    )
}

#' plotSizeUI
#'
#' Size module for plotly plots
#'
#' @note \code{plotSizeUI}
#' @param id, id
#' @param h, height
#' @param w, width
#' @return size and margins controls
#' @examples
#'     x <- plotSizeUI("heatmap")
#' @export
#'
plotSizeUI <- function(id, w=800, h=600){
    ns <- NS(id)
    list(
    checkboxInput(r(ns('size'), "-"), paste0('Plot Size'), value = FALSE),
    conditionalPanel(paste0('input.', r(ns('size'), "-")),
    sliderInput(ns("width"), "width",
    min = 100, max = 2000, step = 10, value = w),
    sliderInput(ns("height"), "height",
    min = 100, max = 2000, step = 10, value = h)
    )
    )
}

r <- function(str, chr)
{
    gsub(chr, '', str)
}

#' plotMarginsUI
#'
#' Margins module for plotly plots
#'
#' @note \code{plotMarginsUI}
#' @param id, id
#' @param t, top margin
#' @param b, bottom margin
#' @param l, left margin
#' @param r, right margin
#' @return size and margins controls
#' @examples
#'     x <- plotMarginsUI("heatmap")
#' @export
#'
plotMarginsUI <- function(id, t=20, b=100, l=100, r=20){
    ns <- NS(id)
    list(
        checkboxInput(r(ns('margins'), "-"), 'Margins', value = FALSE),
        conditionalPanel(paste0('input.', r(ns('margins'), "-")),
        sliderInput(ns("top"), "Margin Top", min = 0, max = 200, value = t),
        sliderInput(ns("bottom"), "Margin Bottom", min = 0, max = 200, value = b),
        sliderInput(ns("left"), "Margin Left", min = 0, max = 200, value = l),
        sliderInput(ns("right"), "Margin Right", min = 0, max = 200, value = r))
    )
}

#' plotTypeUI
#'
#' Plot download type
#'
#' @note \code{plotTypeUI}
#' @param id, id
#' @return size and margins controls
#' @examples
#'     x <- plotTypeUI("heatmap")
#' @export
#'
plotTypeUI <- function(id){
    ns <- NS(id)
    list(
        checkboxInput(ns('svg'), paste0('Download SVG'), value = FALSE)
    )
}