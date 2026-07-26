#' Show the quickNet model registry
#'
#' @param model Optional model or function name. If \code{NULL}, all registered
#' models and analysis modules are returned.
#'
#' @return A data frame with model family, backend, data type, network layers,
#' reportable quantities, key references, and known limitations.
#' @export
model_registry <- function(model = NULL) {
  registry <- quicknet_model_registry()
  if (!is.null(model)) {
    key <- quicknet_input_model_key(model)
    if (!key %in% registry$model) {
      stop("Unknown model/function: ", model, call. = FALSE)
    }
    registry <- registry[registry$model == key, , drop = FALSE]
  }
  rownames(registry) <- NULL
  registry
}

quicknet_model_registry <- function() {
  rows <- lapply(quicknet_model_registry_rows(), as.data.frame, stringsAsFactors = FALSE)
  do.call(quicknet_bind_rows_fill, rows)
}

quicknet_model_info <- function(model) {
  registry <- quicknet_model_registry()
  key <- quicknet_input_model_key(model)
  out <- registry[registry$model == key, , drop = FALSE]
  if (nrow(out) == 0) return(data.frame())
  rownames(out) <- NULL
  out
}

quicknet_model_registry_rows <- function() {
  list(
    quicknet_model_registry_row("EBICglasso", "cross_sectional", "quickNet", "bootnet::estimateNetwork(default = 'EBICglasso')", "exploratory", FALSE, "default", "edges; node predictability; network summary", "Epskamp et al. 2018; Friedman et al. 2008", "Regularization depends on tuning and sample size."),
    quicknet_model_registry_row("correlation", "cross_sectional", "quickNet", "stats::cor", "exploratory", FALSE, "default", "edges; node predictability; network summary", "Epskamp et al. 2012", "Pairwise associations are not conditional dependencies."),
    quicknet_model_registry_row("partial", "cross_sectional", "quickNet", "inverse correlation / precision matrix", "exploratory", FALSE, "default", "edges; node predictability; network summary", "Lauritzen 1996; Epskamp et al. 2018", "Can be unstable when variables approach sample size."),
    quicknet_model_registry_row("ordinal", "cross_sectional", "quickNet", "psych::polychoric / stats::cor", "exploratory", FALSE, "default", "edges; node predictability; network summary", "Epskamp et al. 2018", "Sparse or highly imbalanced categories can destabilize polychoric estimates."),
    quicknet_model_registry_row("ising", "cross_sectional", "quickNet", "IsingFit::IsingFit", "exploratory", FALSE, "default", "edges; thresholds; binary predictability; network summary", "van Borkulo et al. 2014", "Requires binary 0/1 variables with variation."),
    quicknet_model_registry_row("mgm", "cross_sectional", "quickNet", "mgm::mgm", "exploratory", FALSE, "default", "edges; node type/level; network summary", "Haslbeck and Waldorp 2020", "Variable type and level coding must match mgm conventions."),
    quicknet_model_registry_row("clpn", "panel", "PanelNet", "glmnet::cv.glmnet", "exploratory", TRUE, "default, cross_lagged", "cross-lagged paths; node prediction; network summary", "Rhemtulla et al. 2022", "Does not separate stable between-person differences."),
    quicknet_model_registry_row("ri_clpm", "panel", "PanelNet", "psychonetrics::ri_clpm", "confirmatory", TRUE, "default, temporal, cross_lagged, contemporaneous, random_intercept", "fit indices; parameters; modification indices; network summary", "Hamaker et al. 2015; Epskamp 2020", "Needs enough waves and subjects for stable RI interpretation."),
    quicknet_model_registry_row("panel_gvar", "panel", "PanelNet", "psychonetrics::panelgvar", "confirmatory", TRUE, "default, temporal, within, between", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Between-person network can be unstable with small samples."),
    quicknet_model_registry_row("panel_var", "panel", "PanelNet", "psychonetrics::panelvar", "confirmatory", TRUE, "default, temporal, within, between", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Contemporaneous layers are covariance based."),
    quicknet_model_registry_row("panel_sem", "panel", "PanelSEMNet", "lavaan::sem", "confirmatory", TRUE, "default, cross_lagged, contemporaneous", "fit indices; paths; network summary", "Rosseel 2012", "Model identification and residual structure must be checked by users."),
    quicknet_model_registry_row("graphicalVAR", "intensive_longitudinal", "LongitudinalNet", "graphicalVAR::mlGraphicalVAR", "exploratory", TRUE, "default, temporal, contemporaneous, between", "temporal/contemporaneous/between networks; network summary", "Epskamp et al. 2018", "Requires ordered repeated measures with sufficient observations."),
    quicknet_model_registry_row("mlVAR", "intensive_longitudinal", "LongitudinalNet", "mlVAR::mlVAR", "exploratory", TRUE, "default, temporal, temporal_lag_*, contemporaneous, between", "lag-specific temporal/contemporaneous/between networks; network summary", "Bringmann et al. 2013", "Estimation can be slow and sensitive to random-effects structure."),
    quicknet_model_registry_row("psychonetrics_gvar", "intensive_longitudinal", "LongitudinalNet", "psychonetrics::gvar", "confirmatory", TRUE, "default, temporal, contemporaneous", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Current wrapper targets lag-1 GVAR workflows."),
    quicknet_model_registry_row("confirmatory_ggm", "cross_sectional", "ConfirmatoryNet", "psychonetrics::ggm", "confirmatory", FALSE, "default", "fit indices; parameters; modification indices; constraints; network summary", "Epskamp 2020", "Template constraints should be theoretically justified."),
    quicknet_model_registry_row("confirmatory_ising", "cross_sectional", "ConfirmatoryNet", "psychonetrics::Ising", "confirmatory", FALSE, "default", "fit indices; parameters; modification indices; thresholds; network summary", "Epskamp 2020; van Borkulo et al. 2014", "Requires binary 0/1 variables and interpretable threshold parameterization."),
    quicknet_model_registry_row("confirmatory_cor", "cross_sectional", "ConfirmatoryNet", "psychonetrics::varcov(type = 'cor')", "confirmatory", FALSE, "default", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Edges are marginal correlations, not conditional associations."),
    quicknet_model_registry_row("confirmatory_covariance", "cross_sectional", "ConfirmatoryNet", "psychonetrics::varcov(type = 'cov')", "confirmatory", FALSE, "default", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Scale dependent unless variables are standardized."),
    quicknet_model_registry_row("confirmatory_precision", "cross_sectional", "ConfirmatoryNet", "psychonetrics::varcov(type = 'prec')", "confirmatory", FALSE, "default", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Precision parameters require careful positive-definiteness checks."),
    quicknet_model_registry_row("latent_network", "latent", "LatentNet", "lavaan::cfa", "confirmatory", FALSE, "default, latent, residual", "fit indices; loadings; latent/residual networks", "Rosseel 2012", "Depends on CFA model validity."),
    quicknet_model_registry_row("lvm", "latent", "LatentNet", "psychonetrics::lvm", "confirmatory", FALSE, "default, latent, residual", "fit indices; parameters; loadings; modification indices", "Epskamp 2020", "Requires a valid loading matrix and identified latent model."),
    quicknet_model_registry_row("lnm", "latent", "LatentNet", "psychonetrics::lnm", "confirmatory", FALSE, "default, latent", "fit indices; parameters; loadings; modification indices", "Epskamp 2020", "Latent network interpretation depends on measurement model quality."),
    quicknet_model_registry_row("rnm", "latent", "LatentNet", "psychonetrics::rnm", "confirmatory", FALSE, "default, residual, latent", "fit indices; parameters; loadings; modification indices", "Epskamp 2020", "Residual network can absorb measurement-model misspecification."),
    quicknet_model_registry_row("lrnm", "latent", "LatentNet", "psychonetrics::lrnm", "confirmatory", FALSE, "default, latent, residual", "fit indices; parameters; loadings; modification indices", "Epskamp 2020", "Needs enough indicators and sample size for two network layers."),
    quicknet_model_registry_row("meta_ggm", "meta", "MetaNet", "psychonetrics::meta_varcov(type = 'ggm')", "meta_analysis", FALSE, "default", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Requires comparable variables and study-level sample sizes."),
    quicknet_model_registry_row("meta_cor", "meta", "MetaNet", "psychonetrics::meta_varcov(type = 'cor')", "meta_analysis", FALSE, "default", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Pooled correlations are marginal associations."),
    quicknet_model_registry_row("meta_gvar", "meta", "MetaNet", "psychonetrics::meta_gvar", "meta_analysis", TRUE, "default, temporal, contemporaneous", "fit indices; parameters; modification indices; network summary", "Epskamp 2020", "Requires harmonized intensive longitudinal designs across studies."),
    quicknet_model_registry_row("mixedVAR", "time_series", "MixedVARNet", "mgm::mvar", "exploratory", TRUE, "default, temporal, lag_*", "lag-specific temporal mixed VAR networks; network summary", "Haslbeck and Waldorp 2020", "Rows must be correctly time ordered."),
    quicknet_model_registry_row("time_varying_mvar", "time_series", "TimeVaryingNet", "mgm::tvmvar", "exploratory", TRUE, "default, estpoint_*, estpoint_*_lag_*", "time-varying lag-specific mixed VAR networks; network summary", "Haslbeck and Waldorp 2020", "Bandwidth and estimation points strongly affect results."),
    quicknet_model_registry_row("power", "planning", "NetworkPower", "simulation / powerly", "simulation", FALSE, "not applicable", "sample-size recommendation; recovery metrics", "Constantin et al. 2023", "Results depend on assumed true-network structure."),
    quicknet_model_registry_row("perturbation", "simulation", "Perturbation", "model-implied simulation", "simulation", NA, "not applicable", "perturbation metrics; rankings; plots", "Lunansky et al. 2022", "In silico perturbations are not causal intervention effects.")
  )
}

quicknet_model_registry_row <- function(model,
                                        family,
                                        function_name,
                                        backend,
                                        analysis_type,
                                        directed,
                                        network_layers,
                                        reportable_parameters,
                                        references,
                                        limitations) {
  list(
    model = model,
    family = family,
    function_name = function_name,
    backend = backend,
    analysis_type = analysis_type,
    directed = directed,
    network_layers = network_layers,
    reportable_parameters = reportable_parameters,
    references = references,
    limitations = limitations
  )
}
