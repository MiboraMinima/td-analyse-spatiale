source("renv/activate.R")

# Load selected utility packages
require("cli")
require("colorout")

show_named_colors <- function(pattern = "", ncol = 10, text_size = 2.5) {
  cols <- grep(pattern, colors(), value = TRUE, ignore.case = TRUE)

  if (length(cols) == 0) {
    stop("No colors found matching pattern: ", pattern)
  }

  df <- data.frame(
    name = cols,
    x    = ((seq_along(cols) - 1) %% ncol) + 1,
    y    = ((seq_along(cols) - 1) %/% ncol) + 1
  )

  # Use white or black text depending on color brightness
  brightness <- apply(col2rgb(cols), 2, function(c) sum(c * c(0.299, 0.587, 0.114)))
  df$text_color <- ifelse(brightness > 128, "black", "white")

  ggplot(df, aes(x, -y)) +
    geom_tile(aes(fill = name), color = "white", linewidth = 0.5) +
    geom_text(aes(label = name, color = text_color), size = text_size) +
    scale_fill_manual(values = setNames(cols, cols)) +
    scale_color_identity() +
    labs(title = if (pattern == "") "All R named colors" else paste0("R colors matching \"", pattern, "\""),
          subtitle = paste(length(cols), "colors found")) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40")
    )
}
