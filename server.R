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
library(stringr)
library(htmltools)
library(purrr)

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
            xaxis = list(title = "Time", color = "#ffffff"),
            yaxis = list(title = "", color = "#ffffff"),
            legend = list(x = 0, y = 1, font = list( color = "#ffffff")),
            title = "",
            margin = list(b = 10, r = 20, l = 20, t = 20),
            paper_bgcolor = "#101010",
            plot_bgcolor = "#101010"
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

stateServer <- function(id) {
  moduleServer(
    id, 
    function(input, output, session){
      rm(ufDF, totalDF, mergedDF)
      geomun <- geojson_read("data/brasil_mun.geojson", what = "sp")
      munDF <- dbData.table(conn, glue("
          select
            codmun, casosAcumulado, obitosAcumulado
          from granular
          where data == (
            select max(data)
            from granular
            );"))

      output$stateMap <- renderLeaflet({
        geomun@data <- data.table(geomun@data)
        geomun@data[, ":="(id_ord = 1:.N, id = as.numeric(id), codmun = map_chr(id, ~str_sub(., 1, 6)) %>% as.numeric())]

        mergedDT <- merge(geomun@data, munDF,
          by = c("codmun"), all.x = T
        )

        mergedDT[, CT := norm_vector(casosAcumulado, min(casosAcumulado)), .(UF)]
        setorder(mergedDT, id_ord)
        geomun@data <- mergedDT
        geomun <- geomun[geomun$UF == input$stateSelect,]

        colorpal <- colorNumeric(gradient_palette, c(0, geomun@data[, max(casosAcumulado)]))

        labels <- glue("
        <strong style = 'font-size: 13px'>{geomun@data[, NOME_ESTADO]} -
        {geomun@data[, name]}</strong><br>
        <span style = 'font-size: 11px; text-align: center; color: red'>{geomun@data[,
                         format(round(casosAcumulado, 2),
                         big.mark = ',')]}</span>
                           ") %>%
          lapply(htmltools::HTML)

        leaflet(geomun, options = leafletOptions(zoomControl = F)) %>%
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

      output$stateTable <- renderReactable({
        latest.day <- dbData.table(conn, glue("
          select
            max(data) as fecha
          from granular
          ;")) %>% .[, fecha := as.Date(fecha, origin = "1970-01-01")]
          

        ufDF <- dbData.table(conn, glue("
          select
            *
          from granular
          where (data == {as.numeric(latest.day$fecha - 7)}
          or data == {as.numeric(latest.day$fecha)})
          and estado == '{input$stateSelect}';")) %>% 
          .[, fecha := as.Date(data, origin = "1970-01-01")]
        

        df <- dcast(ufDF, estado + municipio ~ fecha, value.var = c("casosAcumulado", "obitosAcumulado"))
        setnames(df, c("State", "Counties", "Cases_Last", "Cases_Current", "Defunctions_Last", "Defunctions_Current"))

        df[, `:=`(
          Cases_DIFF = Cases_Current - Cases_Last,
          Defunctions_DIFF = Defunctions_Current - Defunctions_Last,
          CurrentCases_Status = fifelse(Cases_Current - Cases_Last > 0, "Growing", "Decreasing"),
          CurrentDefunc_Status = fifelse(Defunctions_Current - Defunctions_Last > 0, "Growing", "Decreasing")
          )]

        setorder(df, -Cases_DIFF)

        reactable(
          df[, .(Counties, Cases_Current, Defunctions_Current)],
          fullWidth = T,
          resizable = T,
          highlight = T,
          defaultPageSize = 16,
          columns = list(
            Counties = colDef(
              name = "Counties",
              align = "left",
              width = 280,
              cell = function(value, index) {
                if (df[index, CurrentCases_Status] == "Growing") {
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
            Cases_Current = colDef(
              name = "Cases",
              align = "left",
              format = colFormat(separators = T, digits = 0),
              cell = function(value, index){
                if (df[index, CurrentCases_Status] == "Growing") {
                  className <- 'state-up-text'
                } else {
                  className <- 'state-down-text'
                }

                tagList(
                  span(format(value, big.mark=","), class = "state-cases"),
                  span('  '),
                  span(df[index, Cases_DIFF], ' New Cases', class = className)
                )
                }
              ),
            Defunctions_Current = colDef(
              name = "Defunctions",
              align = "left",
              format = colFormat(separators = T, digits = 0),
              cell = function(value, index){
                if (df[index, CurrentCases_Status] == "Growing") {
                  className <- 'state-up-text'
                } else {
                  className <- 'state-down-text'
                }

                tagList(
                  span(format(value, big.mark=","), class = "state-cases"),
                  span('  '),
                  span(df[index, Cases_DIFF], ' New', class = className)
                )
                }
              )
          )
        )

    })
    })
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  homeServer("homeTab")
  stateServer("stateTab")
})
