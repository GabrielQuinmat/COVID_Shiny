#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(RSQLite)
library(plotly)
library(lubridate)
library(leaflet)
library(geojsonio)
library(reactable)
library(glue)
library(htmltools)

source("utils.R")

conn <- dbConnect(RSQLite::SQLite(), "data/brazil_covid.db")

# geomun <- geojson_read('data/brasil_mun.geojson', what = 'sp')


homeServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      geouf <- geojson_read("data/brasil_uf.json", what = "sp")

      output$totalHistoric <- renderPlotly({
        totalDF <- dbData.table(conn, "
                               select data fecha, casosAcumulado, obitosAcumulado
                               from global;")
        totalDF <- totalDF[, fecha := as.Date(fecha, origin = "1970-01-01")][
          year(fecha) >= year(max(fecha))
        ]
        plot_ly(totalDF, x = ~fecha) %>%
          add_bars(y = ~obitosAcumulado, marker = list(color = gradient_palette[1]), name = "Acumulated Defunctions") %>%
          add_bars(y = ~casosAcumulado, marker = list(color = gradient_palette[2]), name = "Acumulated Cases") %>%
          layout(
            barmode = "stack",
            xaxis = list(title = "Time"),
            yaxis = list(title = ""),
            legend = list(x = 0, y = 1),
            title = "",
            margin = list(b = 10, r = 20, l = 20, t = 50)
          )
      })

      ufDF <- dbData.table(conn, "
          select
            data fecha, casosAcumulado, obitosAcumulado, estado,
            coduf
          from estado;")
      ufDF <- ufDF[, `:=`(
        fecha = as.Date(fecha, origin = "1970-01-01")
      )]

      output$totalMap <- renderLeaflet({
        df <- ufDF[fecha == max(fecha)][, -c('NOME_UF')]
        geouf@data <- data.table(geouf@data)
        geouf@data[, ":="(id_ord = 1:.N, GEOCODIGO = as.numeric(GEOCODIGO))]

        mergedDT <- merge(geouf@data, df,
          by.x = c("UF_05", "GEOCODIGO"),
          by.y = c("estado", "coduf")
        )
        mergedDT[, CT := norm_vector(casosAcumulado, min(casosAcumulado))]
        setorder(mergedDT, id_ord)
        geouf@data <- mergedDT

        colorpal <- colorNumeric(gradient_palette, c(0, geouf@data[, max(casosAcumulado)]))

        labels <- glue("
        <strong style = 'font-size: 13px'>{geouf@data[, NOME_UF]} -
        {geouf@data[, UF_05]}</strong><br>
        <span style = 'font-size: 11px; text-align: center; color: red'>{geouf@data[,
                         format(round(casosAcumulado, 2),
                         big.mark = ',')]}</span>
                           ") %>%
          lapply(htmltools::HTML)

        leaflet(geouf, options = leafletOptions(zoomControl = F)) %>%
          addProviderTiles(providers$CartoDB.DarkMatter,
            options = providerTileOptions(minZoom = 4, maxZoom = 15)
          ) %>%
          addPolygons(
            stroke = FALSE, smoothFactor = 0.5,
            fillOpacity = 0.70, weight = 1,
            color = ~ color_func(CT),
            label = labels, dashArray = "3",
            highlightOptions = highlightOptions(
              weight = 2, dashArray = "",
              color = "white",
              fillOpacity = 1,
              bringToFront = TRUE
            )
          ) %>%
          addLegend("bottomright",
            values = ~casosAcumulado,
            title = "",
            pal = colorpal,
            opacity = 1
          )
      })

      output$totalTable <- renderReactable({
        ufDF <- merge(ufDF, data.table(geouf@data)[, .(NOME_UF, UF_05)], by.x = "estado", by.y = "UF_05")
        currentDF <- ufDF[fecha >= max(fecha) - 7][, .(
          Cases = sum(casosAcumulado),
          Defunctions = sum(obitosAcumulado)
        ), by = .(estado, NOME_UF)]
        lastDF <- ufDF[fecha >= max(fecha) - 15 & fecha < max(fecha) - 7][, .(
          Cases = sum(casosAcumulado),
          Defunctions = sum(obitosAcumulado)
        ), by = .(estado, NOME_UF)]

        mergedDT <- merge(currentDF, lastDF,
          by.x = c("estado", 'NOME_UF'),
          by.y = c("estado", 'NOME_UF'),
          suffixes = c("_current", "_last")
        )
        mergedDT[, `:=`(
          Cases_DIFF = Cases_current / Cases_last - 1,
          Defunctions_DIFF = Defunctions_current / Defunctions_last - 1,
          CurrentCases_Status = fifelse(Cases_current - Cases_last > 0, "Growing", "Decreasing"),
          CurrentDefunc_Status = fifelse(Defunctions_current - Defunctions_last > 0, "Growing", "Decreasing")
        )]
        setorder(mergedDT, Cases_DIFF)

        reactable(
          mergedDT[1:10, .(
            State = NOME_UF,
            Cases = Cases_current,
            Defunctions = Defunctions_current
          )],
          fullWidth = T,
          resizable = T,
          highlight = T,
          columns = list(
            State = colDef(
              name = "State",
              align = "left",
              width = 280,
              cell = function(value, index) {
                if (mergedDT[index, CurrentCases_Status] == "Growing") {
                  ico <- "chevron circle up"
                  className <- 'state-up'
                } else {
                  ico <- "chevron circle down"
                  className <- 'state-down'
                }

                tagList(
                  span(icon(ico), class = className),
                  span(value, class = "state-name")
                )
              }
            ),
            Cases = colDef(
              name = "Cases",
              align = "left",
              format = colFormat(separators = T, digits = 0),
              cell = function(value, index){
                if (mergedDT[index, CurrentCases_Status] == "Growing") {
                  className <- 'state-up-text'
                } else {
                  className <- 'state-down-text'
                }

                value <- format(value, big.mark = ",")
                val <- mergedDT[index, Cases_DIFF] * 100
                val <- ifelse(val < 0.01 & val >= 0, 
                        ifelse(val > -0.01 & val <= 0, "> -0.01%", "< 0.01%"), 
                        glue("{format(val, digits=1, nsmall = 2)}%"))

                tagList(
                  span(value, class = "state-cases"),
                  span('  '),
                  span(val, class = className)
                )
              }
            ),
            Defunctions = colDef(
              name = "Defunctions",
              align = "left",
              format = colFormat(separators = T, digits = 0),
              cell = function(value, index){
                if (mergedDT[index, CurrentDefunc_Status] == "Growing") {
                  className <- 'state-up-text'
                } else {
                  className <- 'state-down-text'
                }

                value <- format(value, big.mark = ",")
                val <- mergedDT[index, Defunctions_DIFF] * 100
                val <- ifelse(val < 0.01 & val >= 0, 
                        ifelse(val > -0.01 & val <= 0, "> -0.01%", "< 0.01%"), 
                        glue("{format(val, digits=1, nsmall = 2)}%"))

                tagList(
                  span(value, class = "state-cases"),
                  span('  '),
                  span(val, class = className)
                )
              }
            )
          )
        )
      })
    }
  )
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  homeServer("homeTab")
})
