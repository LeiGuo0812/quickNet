#' @title Export stability plots and tables
#' @importFrom stringr str_sub
#' @importFrom fs path_join
#' @importFrom dplyr select mutate everything
#' @param stability output from \code{quickNet::Stability}.
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device 'pdf' or 'svg', deciding the output plot format.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch.
#' @param get.table Logical. Export available custom edge-bootstrap and case-drop
#'   tables, plus bootnet's CS-coefficient table when present. Default is TRUE.
#' @param ... other parameter from \code{pdf} or \code{svg}.
#' @details EBICglasso results include native bootnet plots and CS coefficients.
#'   Other supported cross-sectional models provide custom stability tables;
#'   these are exported as CSV files when \code{get.table = TRUE}. A custom
#'   case-drop correlation table is not a bootnet CS coefficient. If no plots
#'   or requested tables are available, the function reports an error.
#' @return Exports available plots and tables to the specified path, then
#'   returns \code{NULL} invisibly. Custom table filenames end in
#'   \code{edge_bootstrap_stability_table.csv} and
#'   \code{case_drop_centrality_stability_table.csv}.
#' @export
#'
#' @examples
#'data('mtcars')
#'stability <- Stability(mtcars, nboot = 10)
#'get_stability_plot(stability, prefix = 'test', path = tempdir())
#'

get_stability_plot <- function(stability, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.table = TRUE, ...){

  if (!is.logical(get.table) || length(get.table) != 1L || is.na(get.table)) {
    stop("get.table must be TRUE or FALSE.", call. = FALSE)
  }

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  device <- match.arg(device, c("pdf", "svg"))
  plot_specs <- list(
    edge_weight_CI_plot = stability$edge_weight_CI_plot,
    edge_weight_diff_plot = stability$edge_weight_diff_plot,
    centrality_stability_plot = stability$centrality_stability_plot,
    centrality_diff_plot = stability$centrality_diff_plot
  )
  if (!is.null(stability$bridge_stability_plot)) {
    plot_specs$bridge_stability_plot <- stability$bridge_stability_plot
  }
  plot_specs <- Filter(Negate(is.null), plot_specs)
  table_specs <- Filter(is.data.frame, list(
    edge_bootstrap_stability_table = stability$edge_bootstrap_stability,
    case_drop_centrality_stability_table = stability$case_drop_centrality_stability
  ))
  if (!length(plot_specs) && (!get.table ||
      (!length(table_specs) && is.null(stability$CS_coefficient)))) {
    message <- if (length(table_specs) || !is.null(stability$CS_coefficient)) {
      "No stability plots are available; use get.table = TRUE to export available stability tables."
    } else "No stability plots or tables are available."
    stop(message, call. = FALSE)
  }
  for (plot_name in names(plot_specs)) {
    if (is.null(plot_specs[[plot_name]])) next
    quicknet_plot_to_device(
      filename = path_join(c(path, paste0(prefix, plot_name, ".", device))),
      device = device,
      width = width,
      height = height,
      plot_function = local({
        current_plot <- plot_specs[[plot_name]]
        function() print(current_plot)
      }),
      ...
    )
  }

  if (get.table) {
    for (table_name in names(table_specs)) {
      write.csv(table_specs[[table_name]],
                path_join(c(path, paste0(prefix, table_name, ".csv"))),
                row.names = FALSE)
    }
  }

  if (get.table && !is.null(stability$CS_coefficient)) {
    stability$CS_coefficient %>%
      as.data.frame() %>%
      `colnames<-`('CS-coefficient') %>%
      mutate(Measure = rownames(.)) %>%
      select(Measure, everything()) %>%
      write.csv(path_join(c(path,
                            paste0(prefix,
                                   'CS_coefficient_table.csv'))),
                row.names = FALSE)
  }
  invisible(NULL)
}

