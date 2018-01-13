#' Visualize a structure graph
#'
#' For now, this function is really inflexible because it is only intended for
#' demonstration purposes in the shiny app.
#'
#' @export

visualize_struct <- function(struct) {
  relations <- attr(struct, "relations") %>%
    mutate(index = seq_len(nrow(relations)))
  struct <- struct %>%
    activate(edges) %>%
    rename(index = name) %>%
    left_join(relations, by = "index")
  ggraph(struct) +
    theme_graph() +
    geom_edge_diagonal(aes(label = name),
                       angle_calc = "along",
                       label_dodge = unit(5, "mm")) +
    geom_node_label(aes(label = name),
                    fill = "steelblue",
                    colour = "white")
}
