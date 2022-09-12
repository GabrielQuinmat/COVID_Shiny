options(reactable.theme = reactableTheme(
  color = "white",
  backgroundColor = "rgb(55, 55, 58)",
  borderColor = "rgba(218, 218, 218, 0.292)",
  highlightColor = "rgb(83, 83, 90)"
))

gradient_palette <- c("#0377A8", "#1FA6B8", "#3EC4D6", "#63D4CC", "#A0F1DA",
  "#B4FADC")

color_func <- function(x) {
  rgb(colorRamp(gradient_palette)(x),
    maxColorValue = 255
  )
}
norm_vector <- function(x, min.v = NULL) {
  if (is.null(min)) min <- min.v(x)
  (x - min.v) / (max(x) - min.v)
}

dbData.table <- function(conn, query) {
  dbSendQuery(conn, query) %>%
    dbFetch() %>%
    data.table()
}
