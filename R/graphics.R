#' Returns a simple bar plot of enumeration data.table
#' 
#' given an enumeration data table and title, will plot a simple
#' bar chart, returning the ggplot object.
#' 
#' @param enumdf enumeration data table
#' @param title the title/label for the plot
#' @param solidfill a color to fill the bars with
#' @param ... other arguments ignored (for compatibility with generic)
#' @import ggplot2
#' @export
simplebar <- function(enumdf, title=NULL, solidfill="steelblue", ...) {
  if ('data.table' %in% class(enumdf)) {
    enumdf <- as.data.frame(enumdf)
  }
  # requires "enum", "x", "n" and "freq" in df
  yexp <- max(enumdf$freq)*1.4
  enumdf$reallabel <- paste0(round(enumdf$freq, 2)*100, "%")
  enumdf$reallabel[enumdf$reallabel=="0%"] <- "<1%"
  theme_vbar <- theme(
    axis.line = element_line(colour = "black", size = 0.2),
    axis.line.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color="black", size=12),
    legend.title=element_blank(),
    axis.text.x = element_blank(),
    ...)
  gg <- ggplot(enumdf, aes(x=enum, y=freq, label=reallabel))
  if (!is.null(title)) {
    gg <- gg + ggtitle(title)
  }
  gg <- gg + geom_bar(width=0.90, stat="identity", fill=solidfill)
  gg <- gg + geom_text(hjust=-0.1,color="black", size=4) + coord_flip()
  gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, yexp))
  gg <- gg + theme_vbar
  gg
}

a4grid <- function(veris) {
  foo <- getenumby(veris, enum=c('actor', 'action', 'asset.variety', 'attribute'), fillzero=F)
  
}
