library(gridExtra)
library(grid)
library(ggplot2)



pdf("economic_res/model_tables_nopolicy.pdf", width = 8, height = 22)


for (model_name in names(model_results_nopolicy)) {
  grid.newpage()

  # Title
  grid.text(paste("Model specification:", model_name),
            gp = gpar(fontsize = 16, fontface = "bold"),
            y = 0.98)
  grid.text(paste("Model specification:", model_name),
            gp = gpar(fontsize = 16, fontface = "bold"),
            y = 0.48)

  # Create Direkt and Indirekt grobs
  direkt_table <- tableGrob(model_results_nopolicy[[model_name]]$Direkt,
                            rows = rownames(model_results_nopolicy[[model_name]]$Direkt))
  indirekt_table <- tableGrob(model_results_nopolicy[[model_name]]$Indirekt,
                              rows = rownames(model_results_nopolicy[[model_name]]$Indirekt))

  # Add subtitles
  direkt_grob <- grobTree(
    textGrob("Direkt", x = 0.5, y = 0.92, just = "top", gp = gpar(fontsize = 14, fontface = "bold")),
    direkt_table
  )
  indirekt_grob <- grobTree(
    textGrob("Indirekt", x = 0.5, y = 0.92, just = "top", gp = gpar(fontsize = 14, fontface = "bold")),
    indirekt_table
  )

  # Arrange both tables vertically
  grid.draw(arrangeGrob(direkt_grob, indirekt_grob, ncol = 1, heights = c(0.5, 0.5)))
}

dev.off()








pdf("economic_res/model_tables_policy.pdf", width = 8, height = 24)


for (model_name in names(model_results_policy)) {
  grid.newpage()

  # Title
  grid.text(paste("Model specification:", model_name),
            gp = gpar(fontsize = 16, fontface = "bold"),
            y = 0.98)
  grid.text(paste("Model specification:", model_name),
            gp = gpar(fontsize = 16, fontface = "bold"),
            y = 0.48)

  # Create Direkt and Indirekt grobs
  direkt_table <- tableGrob(model_results_policy[[model_name]]$Direkt,
                            rows = rownames(model_results_policy[[model_name]]$Direkt))
  indirekt_table <- tableGrob(model_results_policy[[model_name]]$Indirekt,
                              rows = rownames(model_results_policy[[model_name]]$Indirekt))

  # Add subtitles
  direkt_grob <- grobTree(
    textGrob("Direkt", x = 0.5, y = 0.92, just = "top", gp = gpar(fontsize = 14, fontface = "bold")),
    direkt_table
  )
  indirekt_grob <- grobTree(
    textGrob("Indirekt", x = 0.5, y = 0.92, just = "top", gp = gpar(fontsize = 14, fontface = "bold")),
    indirekt_table
  )

  # Arrange both tables vertically
  grid.draw(arrangeGrob(direkt_grob, indirekt_grob, ncol = 1, heights = c(0.5, 0.5)))
}

dev.off()
