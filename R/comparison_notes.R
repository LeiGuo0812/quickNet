quicknet_ising_comparison_notes <- function(model, gamma) {
  if (identical(model, "confirmatory_ising")) {
    return(paste(
      "This is a confirmatory Ising fit; gamma = 0.25 in the exploratory",
      "IsingFit example of Wang et al. (2026) is not directly applicable."
    ))
  }
  if (!identical(model, "ising")) return(character())

  gamma_recorded <- is.numeric(gamma) && length(gamma) == 1L && is.finite(gamma)
  actual <- if (gamma_recorded) format(gamma, digits = 7L) else "not recorded"
  paste0(
    "Wang et al. (2026) used gamma = 0.25 in their IsingFit example; ",
    "quickNet() defaults to gamma = ", quicknet_default_gamma("ising"),
    " for Ising models. The gamma used for this fit is ",
    actual, "."
  )
}

quicknet_nira_comparison_notes <- function(x) {
  notes <- quicknet_ising_comparison_notes(x$model, x$provenance$estimation_gamma)
  if (!is.null(x$moderation)) {
    notes <- c(notes, paste(
      "Wang et al. (2026) used runMgmmAnalysis() with the MGM default",
      "binarySign = FALSE, summarizing moderation magnitudes; this function",
      "uses binarySign = TRUE and retains signed effects when defined",
      "(see estimate_scale for magnitude-only results). The original",
      "Mod_propLtZ implementation counts nonzero estimates; this function",
      "reports positive_proportion and nonzero_proportion separately."
    ))
  } else {
    notes <- c(notes, "No moderation results are available for this analysis.")
  }
  if (!is.null(x$interventions) && nrow(x$interventions) > 0L) {
    convention <- if (identical(x$settings$perturbation_type, "alleviating")) {
      "For this alleviating intervention, cohen_d = (original - intervention) / pooled SD, matching their sign convention."
    } else {
      "For this aggravating intervention, cohen_d = (intervention - original) / pooled SD, opposite to their sign convention."
    }
    notes <- c(notes, paste(
      "Wang et al. (2026, Table 4) calculated Cohen's d as",
      "(original - intervention) / pooled SD.", convention,
      "raw_cohen_d always uses (intervention - original) / pooled SD;",
      "use -raw_cohen_d to reproduce their convention."
    ))
  } else {
    notes <- c(notes, "No intervention effect sizes were calculated.")
  }
  notes
}

quicknet_print_comparison_notes <- function(notes) {
  if (length(notes) > 0L) {
    cat(paste(notes, collapse = " "), "\n", sep = "")
  }
  invisible(NULL)
}

quicknet_nira_reference <- function() {
  paste(
    "Wang, F., Wu, Y., Wu, Y., & Zhu, T. (2026).",
    "Simulation Intervention for Cross-Sectional Network Models:",
    "Based on the R Packages NodeIdentifyR and NIRApost.",
    "Advances in Methods and Practices in Psychological Science, 9(3), 1-18.",
    "https://doi.org/10.1177/25152459261452944"
  )
}
