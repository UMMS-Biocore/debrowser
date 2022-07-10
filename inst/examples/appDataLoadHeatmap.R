library(debrowser)
library(plotly)
library(gplots)
library(heatmaply)
library(RColorBrewer)


if (interactive()) {
    options(shiny.maxRequestSize = 30*1024^2, shiny.sanitize.errors = TRUE)
    environment(heatmapServer) <- environment()
    app <- shinyApp( ui = shinyUI(heatmapUI),
        server = shinyServer(heatmapServer))
    runApp(app)
}
