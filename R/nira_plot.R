#' Plot quickNet NIRA results
#'
#' Visualize model-implied intervention effects, intervention rankings,
#' Monte Carlo rank stability, or moderation estimates from a
#' \code{quicknet_nira} object. If intervention simulation was blocked by
#' stable moderation, or if a requested result is unavailable, the function
#' returns an explanatory \code{ggplot} information panel instead of failing.
#'
#' @param x A \code{quicknet_nira} object.
#' @param type Plot type. \code{"effect"} shows directional intervention
#'   effects and adjusted-significance markers; \code{"rank"} shows one or
#'   more node-ranking measures; \code{"stability"} shows the cumulative
#'   probability of attaining each rank or better across Monte Carlo
#'   repetitions; and \code{"moderation"} shows moderation estimates with
#'   confidence intervals.
#' @param top_n Optional positive integer giving the maximum number of nodes or
#'   moderation effects to display. Nodes are selected by absolute intervention
#'   effect, mean reported rank, or mean stability rank, as appropriate.
#'
#' @details A star in an effect plot denotes an adjusted permutation
#'   \eqn{p < .05}. Stability is Monte Carlo simulation stability, not bootstrap
#'   stability. All plots carry an explicit reminder that the displayed
#'   intervention results are model-implied simulations rather than causal
#'   treatment effects.
#'
#' @return A \code{ggplot} object. Unavailable and moderation-blocked results
#'   are represented by an information panel with the same return class.
#' @export
get_nira_plot <- function(x,
                          type = c("effect", "rank", "stability", "moderation"),
                          top_n = NULL) {
  if (!inherits(x, "quicknet_nira")) {
    stop("x must be a quicknet_nira object.", call. = FALSE)
  }
  type <- match.arg(type)
  top_n <- quicknet_nira_plot_validate_top_n(top_n)

  if (type != "moderation" && quicknet_nira_plot_is_blocked(x)) {
    return(quicknet_nira_plot_information(
      title = "NIRA intervention analysis blocked",
      message = paste(
        "Stable moderation was detected, so intervention effects were not",
        "simulated. Use type = \"moderation\" to inspect the moderation results."
      )
    ))
  }

  plot <- switch(
    type,
    effect = quicknet_nira_plot_effect(x, top_n = top_n),
    rank = quicknet_nira_plot_rank(x, top_n = top_n),
    stability = quicknet_nira_plot_stability(x, top_n = top_n),
    moderation = quicknet_nira_plot_moderation(x, top_n = top_n)
  )
  if (identical(x$status, "completed_fixed_edge_assumption_violated")) {
    plot <- plot + ggplot2::labs(
      caption = paste(
        quicknet_nira_plot_caption(),
        "Stable moderation was detected; the fixed-edge assumption is violated."
      )
    )
  }
  plot
}

#' @rdname get_nira_plot
#' @param ... Additional arguments reserved for compatibility with the
#'   \code{plot} generic.
#' @method plot quicknet_nira
#' @export
plot.quicknet_nira <- function(x,
                               type = c("effect", "rank", "stability", "moderation"),
                               top_n = NULL,
                               ...) {
  get_nira_plot(x, type = type, top_n = top_n)
}

quicknet_nira_plot_caption <- function() {
  "Model-implied simulation; not a causal treatment effect."
}

quicknet_nira_plot_validate_top_n <- function(top_n) {
  if (is.null(top_n)) return(NULL)
  if (length(top_n) != 1L || !is.numeric(top_n) || is.na(top_n) ||
      !is.finite(top_n) || top_n < 1 || top_n != floor(top_n) ||
      top_n > .Machine$integer.max) {
    stop("top_n must be NULL or a positive integer.", call. = FALSE)
  }
  as.integer(top_n)
}

quicknet_nira_plot_is_blocked <- function(x) {
  status <- x$status
  if (is.list(status)) {
    status <- status$status
  }
  blocked_status <- length(status) > 0L &&
    identical(as.character(status[[1L]]), "blocked_by_moderation")
  blocked_status ||
    isTRUE(x$blocked_by_moderation)
}

quicknet_nira_plot_information <- function(title, message) {
  wrapped <- paste(strwrap(message, width = 68L), collapse = "\n")
  ggplot2::ggplot(
    data.frame(x = 0, y = 0),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 0,
      label = wrapped,
      size = 4.2,
      lineheight = 1.15,
      colour = "#374151"
    ) +
    ggplot2::coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1), clip = "off") +
    ggplot2::labs(
      title = title,
      caption = quicknet_nira_plot_caption()
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.caption = ggplot2::element_text(
        colour = "#6b7280",
        hjust = 0
      ),
      plot.margin = ggplot2::margin(18, 18, 18, 18)
    )
}

quicknet_nira_plot_effect <- function(x, top_n) {
  interventions <- x$interventions
  if (!is.data.frame(interventions) || nrow(interventions) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA intervention effects unavailable",
      message = "No intervention-effect results are available for this object."
    ))
  }
  if (!"node" %in% names(interventions)) {
    return(quicknet_nira_plot_information(
      title = "NIRA intervention effects unavailable",
      message = "The intervention results do not contain node identifiers."
    ))
  }
  required_columns <- c("mean_total_score", "ci_lower", "ci_upper")
  if (!all(required_columns %in% names(interventions))) {
    return(quicknet_nira_plot_information(
      title = "NIRA intervention effects unavailable",
      message = paste(
        "The intervention results do not contain condition means and",
        "normal-approximation confidence intervals."
      )
    ))
  }

  selected <- interventions
  selected$node <- as.character(selected$node)
  ordering_value <- if ("absolute_mean_difference" %in% names(selected)) {
    suppressWarnings(as.numeric(selected$absolute_mean_difference))
  } else {
    abs(suppressWarnings(as.numeric(selected$raw_mean_difference)))
  }
  selected <- selected[
    nzchar(selected$node) &
      is.finite(selected$mean_total_score) &
      is.finite(selected$ci_lower) &
      is.finite(selected$ci_upper),
    ,
    drop = FALSE
  ]
  ordering_value <- ordering_value[match(selected$node, interventions$node)]
  selected <- selected[order(ordering_value, decreasing = TRUE), , drop = FALSE]
  if (!is.null(top_n)) selected <- utils::head(selected, top_n)
  if (nrow(selected) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA intervention effects unavailable",
      message = "No finite condition means and confidence intervals are available."
    ))
  }

  baseline_statistics <- if (is.list(x$baseline)) x$baseline$statistics else NULL
  if (is.data.frame(baseline_statistics) && nrow(baseline_statistics) > 0L &&
      all(c("mean_total_score", "ci_lower", "ci_upper") %in%
          names(baseline_statistics))) {
    original_mean <- as.numeric(baseline_statistics$mean_total_score[[1L]])
    original_lower <- as.numeric(baseline_statistics$ci_lower[[1L]])
    original_upper <- as.numeric(baseline_statistics$ci_upper[[1L]])
  } else {
    original_mean <- as.numeric(selected$original_mean[[1L]])
    original_lower <- original_mean
    original_upper <- original_mean
  }

  original_rows <- data.frame(
    node = selected$node,
    condition = "Original",
    mean = original_mean,
    lower = original_lower,
    upper = original_upper,
    stringsAsFactors = FALSE
  )
  intervention_rows <- data.frame(
    node = selected$node,
    condition = "Intervention",
    mean = as.numeric(selected$mean_total_score),
    lower = as.numeric(selected$ci_lower),
    upper = as.numeric(selected$ci_upper),
    stringsAsFactors = FALSE
  )
  plot_data <- rbind(original_rows, intervention_rows)
  node_levels <- rev(selected$node)
  plot_data$node <- factor(plot_data$node, levels = node_levels)
  plot_data$condition <- factor(
    plot_data$condition,
    levels = c("Original", "Intervention")
  )
  significance <- quicknet_nira_plot_significance(x, selected$node)
  marker_range <- range(c(plot_data$lower, plot_data$upper), finite = TRUE)
  marker_offset <- max(diff(marker_range) * 0.025, 0.02)
  marker_data <- data.frame(
    node = factor(selected$node, levels = node_levels),
    marker_position = selected$ci_upper + marker_offset,
    marker = ifelse(significance$significant, "*", ""),
    stringsAsFactors = FALSE
  )
  subtitle <- if (significance$available) {
    "* adjusted permutation p < .05"
  } else {
    "Adjusted permutation significance is unavailable"
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = node,
      y = mean,
      colour = condition,
      group = condition
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.18,
      linewidth = 0.65,
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::geom_point(
      size = 2.8,
      position = ggplot2::position_dodge(width = 0.45)
    ) +
    ggplot2::geom_text(
      data = marker_data,
      ggplot2::aes(x = node, y = marker_position, label = marker),
      inherit.aes = FALSE,
      size = 5,
      fontface = "bold",
      colour = "#8b1e1e"
    ) +
    ggplot2::scale_colour_manual(
      values = c(Original = "#365f7f", Intervention = "#b7654a")
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::labs(
      title = "NIRA original and intervention conditions",
      subtitle = subtitle,
      x = NULL,
      y = "Mean total score with normal-approximation 95% CI",
      colour = "Condition",
      caption = quicknet_nira_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(colour = "#6b7280", hjust = 0),
      plot.margin = ggplot2::margin(8, 18, 8, 8),
      legend.position = "top"
    )
}

quicknet_nira_plot_significance <- function(x, nodes) {
  permutation <- x$permutation
  significant <- rep(FALSE, length(nodes))
  available <- FALSE
  if (!is.data.frame(permutation) || nrow(permutation) == 0L ||
      !"node" %in% names(permutation)) {
    return(list(significant = significant, available = available))
  }

  permutation_nodes <- as.character(permutation$node)
  matched <- match(nodes, permutation_nodes)
  if ("significant" %in% names(permutation)) {
    values <- quicknet_nira_plot_as_logical(permutation$significant[matched])
    significant[!is.na(values)] <- values[!is.na(values)]
    available <- any(!is.na(values))
  } else if ("p_adjusted" %in% names(permutation)) {
    values <- suppressWarnings(as.numeric(permutation$p_adjusted[matched]))
    significant[is.finite(values)] <- values[is.finite(values)] < 0.05
    available <- any(is.finite(values))
  }
  list(significant = significant, available = available)
}

quicknet_nira_plot_rank <- function(x, top_n) {
  ranking_data <- quicknet_nira_plot_rank_data(x)
  if (!is.data.frame(ranking_data) || nrow(ranking_data) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA rankings unavailable",
      message = "No node-ranking results are available for this object."
    ))
  }

  ranking_data <- ranking_data[
    nzchar(ranking_data$node) & is.finite(ranking_data$rank),
    ,
    drop = FALSE
  ]
  if (nrow(ranking_data) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA rankings unavailable",
      message = "No finite node ranks are available."
    ))
  }

  node_order <- stats::aggregate(
    ranking_data$rank,
    list(node = ranking_data$node),
    mean,
    na.rm = TRUE
  )
  names(node_order)[[2L]] <- "mean_rank"
  node_order <- node_order[order(node_order$mean_rank, node_order$node), , drop = FALSE]
  if (!is.null(top_n)) {
    node_order <- utils::head(node_order, top_n)
  }
  ranking_data <- ranking_data[
    ranking_data$node %in% node_order$node,
    ,
    drop = FALSE
  ]
  ranking_data$node <- factor(
    ranking_data$node,
    levels = rev(node_order$node)
  )
  ranking_data$measure <- factor(
    ranking_data$measure,
    levels = unique(ranking_data$measure)
  )

  ggplot2::ggplot(
    ranking_data,
    ggplot2::aes(x = rank, y = node)
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 1, xend = rank, yend = node),
      linewidth = 0.55,
      colour = "#b8c2cc"
    ) +
    ggplot2::geom_point(
      size = 2.8,
      colour = "#365f7f"
    ) +
    ggplot2::facet_wrap(
      ~measure,
      scales = "free_x",
      labeller = ggplot2::label_wrap_gen(width = 24)
    ) +
    ggplot2::scale_x_continuous(
      breaks = function(limits) {
        lower <- max(1L, ceiling(limits[[1L]]))
        upper <- floor(limits[[2L]])
        if (upper < lower) return(lower)
        seq.int(lower, upper)
      }
    ) +
    ggplot2::labs(
      title = "NIRA node rankings",
      subtitle = "Lower ranks indicate greater model-implied importance",
      x = "Rank",
      y = NULL,
      caption = quicknet_nira_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(colour = "#6b7280", hjust = 0)
    )
}

quicknet_nira_plot_rank_data <- function(x) {
  rankings <- x$rankings
  if (is.data.frame(rankings) && nrow(rankings) > 0L &&
      all(c("node", "rank") %in% names(rankings))) {
    measure <- if ("measure" %in% names(rankings)) {
      as.character(rankings$measure)
    } else if ("metric" %in% names(rankings)) {
      as.character(rankings$metric)
    } else {
      rep("rank", nrow(rankings))
    }
    return(data.frame(
      node = as.character(rankings$node),
      measure = measure,
      rank = suppressWarnings(as.numeric(rankings$rank)),
      stringsAsFactors = FALSE
    ))
  }

  if (is.data.frame(rankings) && nrow(rankings) > 0L &&
      "node" %in% names(rankings)) {
    rank_columns <- names(rankings)[
      grepl("rank", names(rankings), ignore.case = TRUE) &
        vapply(rankings, is.numeric, logical(1))
    ]
    if (length(rank_columns) > 0L) {
      rows <- lapply(rank_columns, function(column) {
        data.frame(
          node = as.character(rankings$node),
          measure = gsub("_", " ", column, fixed = TRUE),
          rank = suppressWarnings(as.numeric(rankings[[column]])),
          stringsAsFactors = FALSE
        )
      })
      return(do.call(rbind, rows))
    }
  }

  interventions <- x$interventions
  if (!is.data.frame(interventions) || nrow(interventions) == 0L ||
      !"node" %in% names(interventions)) {
    return(data.frame())
  }
  effect_column <- quicknet_nira_plot_first_column(
    interventions,
    c("absolute_mean_difference", "directional_effect", "raw_mean_difference"),
    numeric_only = TRUE
  )
  if (is.null(effect_column)) return(data.frame())
  effects <- suppressWarnings(as.numeric(interventions[[effect_column]]))
  data.frame(
    node = as.character(interventions$node),
    measure = "absolute intervention effect",
    rank = rank(-abs(effects), ties.method = "min", na.last = "keep"),
    stringsAsFactors = FALSE
  )
}

quicknet_nira_plot_stability <- function(x, top_n) {
  stability <- x$stability
  frequencies <- if (is.list(stability)) stability$rank_frequencies else NULL
  if (!is.data.frame(frequencies) || nrow(frequencies) == 0L ||
      !all(c("node", "rank") %in% names(frequencies))) {
    return(quicknet_nira_plot_information(
      title = "NIRA rank stability unavailable",
      message = "No Monte Carlo rank-frequency results are available."
    ))
  }

  frequencies$node <- as.character(frequencies$node)
  frequencies$rank <- suppressWarnings(as.numeric(frequencies$rank))
  if ("cumulative_proportion" %in% names(frequencies)) {
    frequencies$cumulative <- suppressWarnings(
      as.numeric(frequencies$cumulative_proportion)
    )
  } else if ("proportion" %in% names(frequencies)) {
    frequencies$proportion <- suppressWarnings(as.numeric(frequencies$proportion))
    frequencies <- frequencies[
      order(frequencies$node, frequencies$rank),
      ,
      drop = FALSE
    ]
    frequencies$cumulative <- stats::ave(
      frequencies$proportion,
      frequencies$node,
      FUN = cumsum
    )
  } else {
    return(quicknet_nira_plot_information(
      title = "NIRA rank stability unavailable",
      message = "Rank frequencies do not contain cumulative or per-rank proportions."
    ))
  }

  frequencies <- frequencies[
    nzchar(frequencies$node) &
      is.finite(frequencies$rank) &
      is.finite(frequencies$cumulative),
    ,
    drop = FALSE
  ]
  if (nrow(frequencies) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA rank stability unavailable",
      message = "No finite cumulative rank-stability estimates are available."
    ))
  }

  node_order <- quicknet_nira_plot_stability_order(stability, frequencies)
  if (!is.null(top_n)) {
    node_order <- utils::head(node_order, top_n)
  }
  frequencies <- frequencies[
    frequencies$node %in% node_order,
    ,
    drop = FALSE
  ]
  frequencies$node <- factor(frequencies$node, levels = node_order)

  ggplot2::ggplot(
    frequencies,
    ggplot2::aes(
      x = rank,
      y = cumulative,
      colour = node,
      group = node
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(frequencies$rank))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.2),
      labels = scales::label_percent(accuracy = 1)
    ) +
    ggplot2::labs(
      title = "NIRA Monte Carlo rank stability",
      subtitle = "Cumulative probability of attaining each rank or better",
      x = "Rank threshold",
      y = "Cumulative occurrence",
      colour = "Node",
      caption = quicknet_nira_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right",
      plot.caption = ggplot2::element_text(colour = "#6b7280", hjust = 0)
    )
}

quicknet_nira_plot_stability_order <- function(stability, frequencies) {
  summary <- if (is.list(stability)) stability$node_summary else NULL
  if (is.data.frame(summary) && nrow(summary) > 0L &&
      all(c("node", "mean_rank") %in% names(summary))) {
    summary$node <- as.character(summary$node)
    summary$mean_rank <- suppressWarnings(as.numeric(summary$mean_rank))
    summary <- summary[
      summary$node %in% frequencies$node & is.finite(summary$mean_rank),
      ,
      drop = FALSE
    ]
    ordered <- summary$node[order(summary$mean_rank, summary$node)]
    missing_nodes <- setdiff(unique(frequencies$node), ordered)
    return(c(ordered, sort(missing_nodes)))
  }

  by_node <- split(frequencies, frequencies$node)
  weighted_rank <- vapply(by_node, function(rows) {
    rows <- rows[order(rows$rank), , drop = FALSE]
    proportion <- if ("proportion" %in% names(rows)) {
      suppressWarnings(as.numeric(rows$proportion))
    } else {
      c(rows$cumulative[[1L]], diff(rows$cumulative))
    }
    sum(rows$rank * proportion, na.rm = TRUE)
  }, numeric(1))
  names(sort(weighted_rank))
}

quicknet_nira_plot_moderation <- function(x, top_n) {
  moderation <- x$moderation
  moderation_table <- if (is.data.frame(moderation)) {
    moderation
  } else if (is.list(moderation)) {
    moderation$table
  } else {
    NULL
  }
  if (!is.data.frame(moderation_table) || nrow(moderation_table) == 0L) {
    stable_detected <- is.list(moderation) && isTRUE(moderation$stable_detected)
    message <- if (stable_detected) {
      paste(
        "Stable moderation was reported, but no moderation-estimate table is",
        "available for plotting."
      )
    } else if (is.list(moderation)) {
      "No reportable stable moderation effects were detected."
    } else {
      "Moderation results are not available for this object."
    }
    return(quicknet_nira_plot_information(
      title = "NIRA moderation results",
      message = message
    ))
  }

  estimate_column <- quicknet_nira_plot_first_column(
    moderation_table,
    c(
      "mean_moderation_estimate", "mean_estimate", "estimate",
      "mod_mean", "effect"
    ),
    numeric_only = TRUE
  )
  if (is.null(estimate_column)) {
    return(quicknet_nira_plot_information(
      title = "NIRA moderation results unavailable",
      message = "The moderation table does not contain a plottable estimate."
    ))
  }
  lower_column <- quicknet_nira_plot_first_column(
    moderation_table,
    c("ci_lower", "lower", "mod_qtl_low"),
    numeric_only = TRUE
  )
  upper_column <- quicknet_nira_plot_first_column(
    moderation_table,
    c("ci_upper", "upper", "mod_qtl_high"),
    numeric_only = TRUE
  )

  plot_data <- moderation_table
  plot_data$estimate <- suppressWarnings(as.numeric(plot_data[[estimate_column]]))
  plot_data$lower <- if (is.null(lower_column)) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(plot_data[[lower_column]]))
  }
  plot_data$upper <- if (is.null(upper_column)) {
    NA_real_
  } else {
    suppressWarnings(as.numeric(plot_data[[upper_column]]))
  }
  stability_column <- intersect(
    c("stable_moderation", "stable"),
    names(plot_data)
  )
  plot_data$stable_plot <- if (length(stability_column) > 0L) {
    values <- quicknet_nira_plot_as_logical(
      plot_data[[stability_column[[1L]]]]
    )
    ifelse(is.na(values), FALSE, values)
  } else {
    is.finite(plot_data$lower) &
      is.finite(plot_data$upper) &
      (plot_data$lower > 0 | plot_data$upper < 0)
  }
  plot_data$label <- quicknet_nira_plot_moderation_labels(plot_data)
  if ("estimate_scale" %in% names(plot_data)) {
    magnitude_only <- !is.na(plot_data$estimate_scale) &
      plot_data$estimate_scale == "magnitude"
    plot_data$label[magnitude_only] <- paste0(
      plot_data$label[magnitude_only], " [magnitude]"
    )
  }
  plot_data <- plot_data[
    nzchar(plot_data$label) & is.finite(plot_data$estimate),
    ,
    drop = FALSE
  ]
  if (nrow(plot_data) == 0L) {
    return(quicknet_nira_plot_information(
      title = "NIRA moderation results unavailable",
      message = "No finite moderation estimates are available."
    ))
  }

  plot_data <- plot_data[
    order(abs(plot_data$estimate), decreasing = TRUE),
    ,
    drop = FALSE
  ]
  if (!is.null(top_n)) {
    plot_data <- utils::head(plot_data, top_n)
  }
  plot_data$label <- factor(
    plot_data$label,
    levels = rev(unique(plot_data$label))
  )
  plot_data$stability <- factor(
    ifelse(plot_data$stable_plot, "Stable", "Not stable"),
    levels = c("Stable", "Not stable")
  )
  stable_detected <- any(plot_data$stable_plot) ||
    (is.list(moderation) && isTRUE(moderation$stable_detected))
  subtitle <- if (stable_detected) {
    "Stable moderation blocks interpretation of NIRA intervention effects"
  } else {
    "No displayed moderation estimate met the stability criterion"
  }
  if ("estimate_scale" %in% names(plot_data) &&
      any(plot_data$estimate_scale == "magnitude", na.rm = TRUE)) {
    subtitle <- paste0(
      subtitle, "\nMagnitude-only estimates do not indicate direction."
    )
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = label, y = estimate, colour = stability)
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#6b7280"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lower, ymax = upper),
      width = 0.18,
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::scale_colour_manual(
      values = c("Stable" = "#a33f35", "Not stable" = "#4f718c"),
      drop = FALSE
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "NIRA moderation estimates",
      subtitle = subtitle,
      x = NULL,
      y = "Mean moderation estimate (95% CI)",
      colour = "Moderation",
      caption = quicknet_nira_plot_caption()
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(colour = "#6b7280", hjust = 0)
    )
}

quicknet_nira_plot_moderation_labels <- function(data) {
  moderator <- if ("moderator" %in% names(data)) {
    as.character(data$moderator)
  } else {
    rep("Moderator", nrow(data))
  }
  edge_one <- quicknet_nira_plot_first_column(
    data,
    c("moderated_node_1", "edge_node_1", "from", "node_1")
  )
  edge_two <- quicknet_nira_plot_first_column(
    data,
    c("moderated_node_2", "edge_node_2", "to", "node_2")
  )
  if (!is.null(edge_one) && !is.null(edge_two)) {
    return(paste0(
      moderator,
      ": ",
      as.character(data[[edge_one]]),
      " \u2013 ",
      as.character(data[[edge_two]])
    ))
  }
  if ("edge" %in% names(data)) {
    return(paste0(moderator, ": ", as.character(data$edge)))
  }
  moderator
}

quicknet_nira_plot_first_column <- function(data,
                                            candidates,
                                            numeric_only = FALSE) {
  available <- candidates[candidates %in% names(data)]
  if (numeric_only && length(available) > 0L) {
    available <- available[vapply(
      data[available],
      function(column) is.numeric(column) || is.integer(column),
      logical(1)
    )]
  }
  if (length(available) == 0L) NULL else available[[1L]]
}

quicknet_nira_plot_as_logical <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(ifelse(is.na(x), NA, x != 0))
  text <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(text))
  out[text %in% c("true", "t", "yes", "y", "1", "stable", "significant")] <- TRUE
  out[text %in% c("false", "f", "no", "n", "0", "unstable", "not stable")] <- FALSE
  out
}

utils::globalVariables(c(
  "condition", "cumulative", "estimate", "lower", "marker",
  "marker_position", "stability", "upper", "x", "y"
))
