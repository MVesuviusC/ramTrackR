#' Plot RAM Usage (Generic)
#'
#' @param monitor A RamMonitor object
#' @param line_size Numeric. Thickness of the RAM usage line (default: 1.5).
#' @param line_color Character. Color of the RAM usage line (default: "black").
#' @param font_size_title Numeric. Font size for the plot title (default: 16).
#' @param font_size_axis Numeric. Font size for the axis labels (default: 14).
#' @return A ggplot2 object
#' @export
#' @import ggplot2
setGeneric("plot_ram_usage", function(
             monitor, 
             line_size = 1.5, 
             line_color = "black", 
             font_size_title = 16, 
             font_size_axis = 14
           ) standardGeneric("plot_ram_usage"))

#' @rdname plot_ram_usage
#' @export
setMethod("plot_ram_usage", "RamMonitor", function(
  monitor, 
  line_size = 1.5, 
  line_color = "black", 
  font_size_title = 16, 
  font_size_axis = 14
) {
  ram_data <- read_monitor_data(monitor)
  if (nrow(ram_data) == 0) {
    warning("No data recorded yet")
    return(
      ggplot2::ggplot() + 
        ggplot2::theme_void() + 
        ggplot2::ggtitle("No data")
    )
  }
  
  ram_data$time <- as.POSIXct(
    ram_data$timestamp, 
    origin = "1970-01-01"
  )
  ram_data$memory_gb <- ram_data$memory_bytes / 1024^3
  
  ram_plot <- ggplot2::ggplot(
    ram_data, 
    ggplot2::aes(
      x = time, 
      y = memory_gb
    )
  ) +
    ggplot2::geom_line(
      linewidth = line_size, 
      color = line_color
    ) +
    ggplot2::labs(
      x = "Time", 
      y = "Memory Usage (GB)", 
      title = "RAM Usage Over Time"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = font_size_title, 
        hjust = 0.5
      ),
      axis.title = ggplot2::element_text(size = font_size_axis),
      axis.text = ggplot2::element_text(
        size = max(font_size_axis - 2, 8)
      )
    )
    
  bookmarks_list <- as.list(monitor@bookmarks)
  if (length(bookmarks_list) > 0) {
    bookmarks_data <- data.frame(
      label = names(bookmarks_list),
      timestamp = unlist(bookmarks_list)
    )
    bookmarks_data$time <- as.POSIXct(
      bookmarks_data$timestamp, 
      origin = "1970-01-01"
    )
    
    ram_plot <- ram_plot + 
      ggplot2::geom_vline(
        data = bookmarks_data, 
        ggplot2::aes(xintercept = time), 
        color = "red", 
        linetype = "dashed"
      ) +
      ggplot2::geom_text(
        data = bookmarks_data, 
        ggplot2::aes(
          x = time, 
          y = max(ram_data$memory_gb, na.rm = TRUE), 
          label = label
        ), 
        angle = 90, 
        vjust = -0.5, 
        hjust = 1, 
        color = "red"
      )
  }
  
  return(ram_plot)
})
