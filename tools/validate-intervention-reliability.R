#!/usr/bin/env Rscript
# Independent accuracy and finite-simulation audit. Run from the package root:
# OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1 Rscript --vanilla \
#   tools/validate-intervention-reliability.R [stage] [output]
# Stages: sampling, stability, permutation, moderation, failure_guard,
#         symperturb, bootstrap, reference, all.
args <- commandArgs(trailingOnly = TRUE)
stage <- if (length(args)) args[[1L]] else "all"
script <- normalizePath(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1L]))
root <- normalizePath(file.path(dirname(script), ".."))
output <- if (length(args) > 1L) args[[2L]] else file.path(root, "..", "output", "audit", "intervention-reliability")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
pkgload::load_all(root, quiet = TRUE)
options(mc.cores = 1L)
write_table <- function(x, name) utils::write.csv(x, file.path(output, paste0(name, ".csv")), row.names = FALSE)
store_result <- function(x, name) saveRDS(x, file.path(output, paste0(name, ".rds")), version = 2)
prob_summary <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(c(rate = NA_real_, mcse = NA_real_, lower = NA_real_, upper = NA_real_, repetitions = 0))
  p <- mean(x); ci <- binom.test(sum(x), length(x))$conf.int
  c(rate = p, mcse = sqrt(p * (1-p) / length(x)), lower = ci[1L], upper = ci[2L], repetitions = length(x))
}
summarize_groups <- function(data, keys, fun) {
  groups <- split(seq_len(nrow(data)), interaction(data[keys], drop = TRUE, lex.order = TRUE))
  do.call(rbind, lapply(groups, function(i) cbind(data[i[1L], keys, drop = FALSE], fun(data[i, , drop = FALSE]))))
}

# Enumerate the joint mass directly from the exponential-family energy.
exact_ising <- function(W, thresholds, beta = 1, triple = 0) {
  p <- length(thresholds)
  states <- as.matrix(expand.grid(rep(list(0:1), p)))
  logmass <- beta * (drop(states %*% thresholds) + rowSums((states %*% W) * states) / 2)
  if (triple != 0) logmass <- logmass + triple * apply(states[, 1:3, drop = FALSE], 1L, prod)
  mass <- exp(logmass - max(logmass)); mass <- mass / sum(mass)
  score <- rowSums(states); mean_score <- sum(mass * score)
  list(states = states, probability = mass, means = colSums(states * mass),
       total_mean = mean_score, total_variance = sum(mass * (score - mean_score)^2))
}
sample_exact <- function(distribution, n) distribution$states[sample.int(nrow(distribution$states), n, TRUE, distribution$probability), , drop = FALSE]
ising_designs <- function() {
  pos <- matrix(4, 4, 4); diag(pos) <- 0
  mixed <- matrix(c(0, .8, -.9, .2, .8, 0, .5, -.4, -.9, .5, 0, -.7, .2, -.4, -.7, 0), 4)
  tie <- matrix(.15, 4, 4); diag(tie) <- 0
  out <- list(strong_positive = list(W = pos, thresholds = c(-6, -5.9, -6.1, -5.95)),
    mixed_sign = list(W = mixed, thresholds = c(-.8, -.2, .4, .9)),
    extreme_thresholds = list(W = mixed / 3, thresholds = c(-12, 10, -8, 7)),
    near_tie = list(W = tie, thresholds = c(-.3, -.3001, -.2999, .3)))
  lapply(out, function(x) {dimnames(x$W) <- list(LETTERS[1:4], LETTERS[1:4]); names(x$thresholds) <- LETTERS[1:4]; x})
}

run_sampling <- function() {
  designs <- ising_designs()
  budgets <- data.frame(label = c("small", "default_iterations", "longer_iterations"), n = c(500L, 3000L, 3000L), iterations = c(10L, 100L, 500L))
  outer <- 12L
  rows <- list(); details <- list()
  for (scenario in names(designs)) {
    design <- designs[[scenario]]; W <- design$W; thresholds <- design$thresholds
    original <- exact_ising(W, thresholds); delta <- 2 * sd(thresholds)
    exact <- lapply(seq_along(thresholds), function(j) {t <- thresholds; t[j] <- t[j] - delta; exact_ising(W, t)})
    true_effect <- original$total_mean - vapply(exact, `[[`, numeric(1), "total_mean")
    true_d <- true_effect / sqrt((original$total_variance + vapply(exact, `[[`, numeric(1), "total_variance"))/2)
    exact_rank <- rank(-true_effect, ties.method = "min")
    details[[scenario]] <- list(original = original, interventions = exact, directional_effect = true_effect, cohen_d = true_d, rank = exact_rank, threshold_delta = delta)
    for (engine in c("literature", "native")) for (b in seq_len(nrow(budgets))) {
      for (repetition in seq_len(outer)) {
        stream <- quicknet_nira_make_streams(720000L + 10000L * match(scenario, names(designs)) + 100L*b + repetition, 1L)[[1L]]
        fit <- quicknet_nira_run_conditions(list(weight_matrix = W, thresholds = thresholds, beta = 1),
          "alleviating", 2, budgets$n[b], engine, stream, TRUE, budgets$iterations[b])
        table <- fit$interventions
        baseline_error <- fit$baseline$statistics$mean_total_score - original$total_mean
        for (j in seq_along(thresholds)) {
          rows[[length(rows)+1L]] <- data.frame(scenario, engine, budget = budgets$label[b],
            n_samples = budgets$n[b], iterations = budgets$iterations[b], repetition, target = names(thresholds)[j],
            baseline_error, baseline_mcse = sqrt(original$total_variance / budgets$n[b]),
            intervention_mean_error = table$mean_total_score[j] - exact[[j]]$total_mean,
            max_node_probability_error = max(abs(colMeans(fit$samples$interventions[[j]]) - exact[[j]]$means)),
            effect = table$directional_effect[j], exact_effect = true_effect[j], effect_error = table$directional_effect[j] - true_effect[j],
            effect_mcse = sqrt((original$total_variance + exact[[j]]$total_variance)/budgets$n[b]),
            cohen_d = table$cohen_d[j], exact_cohen_d = true_d[j], cohen_d_error = table$cohen_d[j] - true_d[j],
            rank = rank(-table$directional_effect, ties.method = "min")[j], exact_rank = exact_rank[j],
            selected_top1 = j == which.max(table$directional_effect), exact_top1 = j == which.max(true_effect))
        }
      }
      message("Sampling completed: ", scenario, "/", engine, "/", budgets$label[b])
      write_table(do.call(rbind, rows), "sampling-raw")
    }
  }
  raw <- do.call(rbind, rows)
  summary <- summarize_groups(raw, c("scenario", "engine", "budget", "target"), function(x) {
    finite_d <- is.finite(x$cohen_d_error)
    data.frame(repetitions = nrow(x), effect_bias = mean(x$effect_error), bias_mcse = sd(x$effect_error)/sqrt(nrow(x)),
      effect_rmse = sqrt(mean(x$effect_error^2)), mean_simulation_mcse = mean(x$effect_mcse),
      probability_rmse = sqrt(mean(x$max_node_probability_error^2)),
      d_rmse = if (any(finite_d)) sqrt(mean(x$cohen_d_error[finite_d]^2)) else NA_real_,
      undefined_d = sum(!finite_d), mean_rank_error = mean(abs(x$rank-x$exact_rank)), top1_frequency = mean(x$selected_top1),
      top1_mcse = sqrt(mean(x$selected_top1)*(1-mean(x$selected_top1))/nrow(x)))
  })
  write_table(summary, "sampling-summary"); store_result(list(design = designs, budgets = budgets, outer = outer, exact = details, raw = raw), "sampling")
}

run_permutation <- function() {
  d <- ising_designs()$mixed_sign; null <- exact_ising(d$W, d$thresholds)
  alternative <- d$thresholds; alternative[1L] <- alternative[1L] - .3
  alt <- exact_ising(d$W, alternative)
  rows <- list(); outer <- 300L; B <- 199L
  for (scenario in c("null", "fixed_weak_effect")) for (n in c(100L, 1000L)) {
    dist <- if (scenario == "null") null else alt
    for (r in seq_len(outer)) {
      set.seed(840000L + 10000L*match(scenario,c("null","fixed_weak_effect")) + n + r)
      x <- rowSums(sample_exact(null,n)); y <- rowSums(sample_exact(dist,n))
      result <- quicknet_nira_permutation_one(x,y,B,quicknet_nira_make_streams(850000L+n+r,1L)[[1L]])
      rows[[length(rows)+1L]] <- data.frame(scenario,n_samples=n,repetition=r,permutations=B,
        exact_mean_difference=dist$total_mean-null$total_mean, p_value=result$p_value, reject=result$p_value < .05)
    }
  }
  raw <- do.call(rbind,rows); write_table(raw,"permutation-raw")
  summary <- summarize_groups(raw,c("scenario","n_samples"),function(x) as.data.frame(as.list(prob_summary(x$reject))))
  write_table(summary,"permutation-summary"); store_result(list(raw=raw,summary=summary,outer=outer,permutations=B),"permutation")
}

run_stability <- function() {
  design <- ising_designs()$near_tie
  design$thresholds[1:3] <- -.3
  result <- quicknet_nira_run_stability(list(weight_matrix=design$W,thresholds=design$thresholds,beta=1),
    "alleviating",2*sd(design$thresholds),500L,120L,1L,"native",quicknet_nira_make_streams(86101L,1L)[[1L]],FALSE,1L,100L)
  write_table(result$node_summary,"stability-summary");write_table(result$rank_frequencies,"stability-rank-frequencies")
  store_result(list(design=design,n_samples=500L,iterations=100L,repetitions=120L,result=result),"stability")
}

run_moderation <- function() {
  outer <- 12L; B <- 19L; n <- 300L
  W <- matrix(0,3,3)
  designs <- list(null_pairwise=list(thresholds=rep(-.25,3),triple=0),
    positive_three_way=list(thresholds=rep(-1,3),triple=3),
    negative_three_way=list(thresholds=rep(.4,3),triple=-4))
  rows <- list(); full <- list()
  for (scenario in names(designs)) {
    design <- designs[[scenario]]; distribution <- exact_ising(W,design$thresholds,triple=design$triple)
    for (r in seq_len(outer)) {
      set.seed(920000L+1000L*match(scenario,names(designs))+r)
      data <- sample_exact(distribution,n); colnames(data) <- LETTERS[1:3]
      warnings <- character(); elapsed <- system.time(result <- tryCatch(withCallingHandlers(
        quicknet_nira_run_moderation(data,colnames(data),"AND",.25,B,quicknet_nira_make_streams(930000L+r,1L)[[1L]],FALSE,1L),
        warning=function(w) {warnings <<- c(warnings,conditionMessage(w)); invokeRestart("muffleWarning")}), error=function(e) list(error=conditionMessage(e))))[["elapsed"]]
      ok <- is.null(result$error)
      rows[[length(rows)+1L]] <- data.frame(scenario,repetition=r,n=n,nboot=B,completed=ok,
        detected=if(ok)result$stable_detected else NA, valid_reps=if(ok)result$valid_reps else NA,
        failed_reps=if(ok)result$failed_reps else NA, error=if(ok)"" else result$error,
        warnings=paste(unique(warnings),collapse=" | "),elapsed_seconds=elapsed)
      full[[paste(scenario,r,sep="/")]] <- list(data=data,result=result)
      write_table(do.call(rbind,rows),"moderation-raw"); store_result(full,"moderation-details")
      message("Moderation completed: ",scenario,"/",r," of ",outer)
    }
  }
  raw <- do.call(rbind,rows)
  summary <- summarize_groups(raw,"scenario",function(x) cbind(data.frame(requested=nrow(x),failed_runs=sum(!x$completed)),as.data.frame(as.list(prob_summary(x$detected)))))
  write_table(summary,"moderation-summary")
  # One rare category produces genuine bootstrap refit failures; preserve the
  # task-level reasons even when the public >20% failure guard must stop.
  set.seed(9511); sparse <- cbind(A=c(1,rep(0,39)),B=rbinom(40,1,.5),C=rbinom(40,1,.5))
  context <- list(data=sparse,node_names=colnames(sparse),p=3L,role_grid=quicknet_nira_moderation_role_grid(colnames(sparse)),rule="AND",lambda=.25)
  streams <- quicknet_nira_make_streams(9512,40L)
  failures <- lapply(seq_len(40L),function(i) quicknet_nira_moderation_worker(list(task_index=i,resample=TRUE,stream=streams[[i]]),context))
  failure_table <- data.frame(repetition=seq_len(40L),ok=vapply(failures,`[[`,logical(1),"ok"),error=vapply(failures,`[[`,character(1),"error"))
  write_table(failure_table,"moderation-sparse-failures"); store_result(list(data=sparse,results=failures),"moderation-sparse")
}

run_failure_guard <- function() {
  set.seed(9511); data <- cbind(A=c(1,rep(0,39)),B=rbinom(40,1,.5),C=rbinom(40,1,.5))
  warnings <- character()
  result <- tryCatch(withCallingHandlers(quicknet_nira_run_moderation(data,colnames(data),"AND",.25,19L,
    quicknet_nira_make_streams(9612L,1L)[[1L]],FALSE,1L),
    warning=function(w){warnings <<- c(warnings,conditionMessage(w));invokeRestart("muffleWarning")}),
    error=function(e)list(error=conditionMessage(e)))
  store_result(list(data=data,nboot=19L,result=result,warnings=warnings),"moderation-public-failure-guard")
  jsonlite::write_json(list(nboot=19L,stopped=!is.null(result$error),error=result$error,
    valid_reps=result$valid_reps,failed_reps=result$failed_reps,warnings=warnings),
    file.path(output,"moderation-public-failure-guard.json"),auto_unbox=TRUE,pretty=TRUE,null="null")
}

gaussian_design <- function(near_tie=FALSE) {
  loadings <- if(near_tie) c(.5,.50001,.49999,.50002,.49998) else c(.85,.7,.5,.3,.15)
  covariance <- outer(loadings,loadings); diag(covariance) <- 1
  mu <- if(near_tie) rep(1.2,5) else c(2.2,1.8,1.2,.9,.6)
  names(mu) <- colnames(covariance) <- rownames(covariance) <- LETTERS[1:5]
  list(mu=mu,covariance=covariance)
}
make_context <- function(data, config=list()) quicknet_sym_context(data,quicknet_sym_config(config),
  modules=setNames(c("m1","m1","m2","m2","m2"),colnames(data)))
draw_gaussian <- function(design,n) sweep(matrix(rnorm(n*length(design$mu)),n)%*%chol(design$covariance),2,design$mu,"+")

run_symperturb <- function() {
  # Construct X_K = mu_K + B (X_S-mu_S) + epsilon. B and the residual
  # covariance are known independently of a covariance-block inverse.
  B <- matrix(c(.6,-.3,.2,.5,-.2,.4),3,2); V <- matrix(c(1,.25,.25,2),2)
  residual <- diag(c(.7,.9,.8)); loading <- rbind(diag(2),B)
  covariance <- loading%*%V%*%t(loading); covariance[3:5,3:5] <- covariance[3:5,3:5]+residual
  mu <- c(.8,1.4,1.1,.6,2); anchor <- c(.1,.2,0,0,0)
  checks <- list()
  for (map in c("linked","location_only","scale_only","independent")) for (dose in c(0,.35,1)) {
    dm <- if(map=="scale_only")1 else (1-dose)^(if(map=="independent")1.4 else 1)
    ds <- if(map=="location_only")1 else (1-dose)^(if(map=="independent").7 else 1)
    target_mu <- anchor[1:2]+dm*(mu[1:2]-anchor[1:2])
    expected_mu <- c(target_mu,mu[3:5]+drop(B%*%(target_mu-mu[1:2])))
    expected_cov <- loading%*%(ds^2*V)%*%t(loading); expected_cov[3:5,3:5] <- expected_cov[3:5,3:5]+residual
    actual <- quicknet_sym_moments(mu,covariance,1:2,dose,anchor,map,1.4,.7)
    checks[[length(checks)+1L]] <- data.frame(check=paste(map,dose),mean_error=max(abs(actual$mean-expected_mu)),covariance_error=max(abs(actual$covariance-expected_cov)),minimum_eigenvalue=min(eigen(actual$covariance,symmetric=TRUE)$values))
  }
  write_table(do.call(rbind,checks),"gaussian-independent-moments")
  grid <- expand.grid(mu=c(-8,-.1,0,2,4,4.1,12),sd=c(0,.01,.5,4,20))
  # E[clip(Y,l,u)] = l + integral_l^u P(Y > t) dt; this avoids
  # the implementation's density/CDF expression and includes tail cases.
  grid$integral <- mapply(function(m,s) if(s==0)min(4,max(0,m)) else integrate(function(t) pnorm(t,m,s,lower.tail=FALSE),0,4,rel.tol=1e-12)$value,grid$mu,grid$sd)
  grid$implemented <- quicknet_sym_observed(grid$mu,diag(grid$sd^2),c(0,4))
  grid$error <- grid$implemented-grid$integral; write_table(grid,"bounded-normal-integral")
  special <- list(constant=matrix(rep(c(1,2,3,4,5),each=20),20),zero=matrix(0,20,5))
  set.seed(991); near <- matrix(rnorm(100),20); near[,2] <- near[,1]+rnorm(20,sd=1e-8); special$near_singular <- near
  special_rows <- list()
  for(name in names(special)) for(ridge in c(0,.02)) {
    data <- special[[name]]; colnames(data) <- LETTERS[1:5]
    ctx <- make_context(data,list(ridge=ridge,edge_threshold=100,run_robustness_scenarios=FALSE,bootstrap_replicates=2L,bootstrap_top_k=1L))
    scores <- quicknet_sym_scores(ctx,ctx$nodes)$target_scores
    boot <- quicknet_sym_bootstrap(ctx,ctx$nodes,matrix(rep(c(rep(1L,20),seq_len(20)),1),2,byrow=TRUE))
    zero <- quicknet_sym_post(ctx,"A",0)
    special_rows[[length(special_rows)+1L]] <- data.frame(case=name,ridge=ridge,pseudoinverse=ctx$network$used_pseudoinverse,
      zero_mean_error=max(abs(zero$mean-ctx$network$mu)),zero_covariance_error=max(abs(zero$covariance-ctx$network$covariance)),
      min_eigenvalue=min(eigen(zero$covariance,symmetric=TRUE)$values),finite_scores=all(is.finite(scores$vpps)),
      max_communication=max(abs(scores$communication_block)),degenerate_top1_count=sum(boot$draws$top_k[boot$draws$replicate==1L]))
  }
  write_table(do.call(rbind,special_rows),"degenerate-cases")
  # Independent sequence oracle uses the Gaussian precision conditional mean
  # at the unit-dose endpoint; no quicknet_sym_* state/benefit routine is used.
  all_paths <- function(pool,length) {
    if(length==1L)return(lapply(pool,identity))
    unlist(lapply(pool,function(first)lapply(all_paths(setdiff(pool,first),length-1L),function(rest)c(first,rest))),recursive=FALSE)
  }
  sequence_rows <- list(); full <- list()
  for(r in seq_len(20L)) {
    set.seed(100100L+r); data <- matrix(rnorm(100*5),100)%*%matrix(rnorm(25),5)+matrix(runif(5,.5,2),100,5,byrow=TRUE)
    colnames(data)<-LETTERS[1:5]
    ctx<-make_context(data,list(bounds=NULL,sequence_length=3L,sequence_pool=5L,sequence_beam_width=60L,sequence_eta=.8,sequence_cost_lambda=.1,run_robustness_scenarios=FALSE))
    ctx$costs <- setNames(runif(5),ctx$nodes); precision <- solve(ctx$network$covariance)
    benefit <- function(selected) {
      outcome <- setdiff(ctx$nodes,selected)
      delta <- drop(solve(precision[outcome,outcome,drop=FALSE],precision[outcome,selected,drop=FALSE]%*%(ctx$anchors[selected]-ctx$network$mu[selected])))
      mean(delta / sqrt(diag(ctx$network$covariance)[outcome]))
    }
    paths <- all_paths(ctx$nodes,3L)
    objective <- vapply(paths,function(path) {
      benefits <- vapply(seq_along(path),function(j)benefit(path[seq_len(j)]),numeric(1))
      sum(.8^(0:2)*diff(c(0,benefits)))-.1*sum(ctx$costs[path])
    },numeric(1))
    base <- quicknet_sym_scores(ctx,ctx$nodes)$target_scores
    wide <- quicknet_sym_sequence(ctx,base)$table
    oracle <- setNames(objective,vapply(paths,paste,collapse=" -> ",character(1)))
    ctx$config$sequence_beam_width<-1L; narrow<-quicknet_sym_sequence(ctx,base)$table
    sequence_rows[[r]]<-data.frame(repetition=r,paths=length(paths),wide_error=max(abs(wide$objective-oracle[wide$sequence])),
      exact_best=max(objective),wide_best=max(wide$objective),narrow_best=max(narrow$objective),narrow_loss=max(objective)-max(narrow$objective))
    full[[r]]<-list(data=data,costs=ctx$costs,oracle=oracle,wide=wide,narrow=narrow)
  }
  write_table(do.call(rbind,sequence_rows),"sequence-exhaustive");store_result(full,"sequence-details")
}

run_bootstrap <- function() {
  outer<-20L;B<-39L;n<-120L;rows<-list();draws<-list()
  for(scenario in c("separated","near_tie")) {
    design<-gaussian_design(scenario=="near_tie")
    set.seed(110001L);ctx<-make_context(draw_gaussian(design,20),list(bounds=NULL,bootstrap_replicates=B,bootstrap_top_k=1L,run_robustness_scenarios=FALSE))
    ctx$network$mu<-design$mu;ctx$network$covariance<-design$covariance+.02*diag(diag(design$covariance))
    ctx$network$precision<-solve(ctx$network$covariance)
    prec<-ctx$network$precision;adj<--prec/outer(sqrt(diag(prec)),sqrt(diag(prec)));diag(adj)<-0;adj[abs(adj)<.03]<-0
    ctx$network$adjacency<-adj;ctx$observed<-design$mu;ctx$denominator<-sqrt(diag(ctx$network$covariance))
    population<-quicknet_sym_scores(ctx,ctx$nodes)$target_scores
    for(r in seq_len(outer)) {
      set.seed(111000L+100L*match(scenario,c("separated","near_tie"))+r)
      data<-draw_gaussian(design,n);current<-make_context(data,list(bounds=NULL,bootstrap_replicates=B,bootstrap_top_k=1L,random_seed=112000L+r,run_robustness_scenarios=FALSE))
      scored<-quicknet_sym_scores(current,current$nodes)$target_scores;boot<-quicknet_sym_bootstrap(current,current$nodes)
      tab<-merge(merge(scored[,c("target","vpps","rank")],boot$summary,by="target"),population[,c("target","vpps","rank")],by="target",suffixes=c("","_population"))
      tab$scenario<-scenario;tab$repetition<-r;tab$n<-n;tab$bootstrap_replicates<-B
      tab$vpps_covered<-tab$vpps_q025<=tab$vpps_population & tab$vpps_q975>=tab$vpps_population
      tab$rank_covered<-tab$rank_q025<=tab$rank_population & tab$rank_q975>=tab$rank_population
      rows[[length(rows)+1L]]<-tab;draws[[paste(scenario,r,sep="/")]]<-list(data=data,bootstrap=boot,scored=scored,population=population)
      message("Bootstrap completed: ",scenario,"/",r)
    }
  }
  raw<-do.call(rbind,rows);write_table(raw,"bootstrap-raw");store_result(draws,"bootstrap-details")
  summary<-summarize_groups(raw,c("scenario","target"),function(x) {
    cover<-prob_summary(x$vpps_covered);rankcover<-prob_summary(x$rank_covered)
    data.frame(repetitions=nrow(x),population_rank=x$rank_population[1],mean_rank=mean(x$rank),rank_sd=sd(x$rank),
      empirical_top1=mean(x$rank<=1),mean_bootstrap_top1=mean(x$top_k_selection_probability),
      vpps_coverage=cover[1],vpps_coverage_mcse=cover[2],vpps_coverage_lower=cover[3],vpps_coverage_upper=cover[4],
      rank_coverage=rankcover[1],rank_coverage_mcse=rankcover[2])
  });write_table(summary,"bootstrap-summary")
}

run_reference <- function() {
  node_source <- Sys.getenv("QUICKNET_NODEIDENTIFYR_SOURCE", file.path(root,"..","tmp","nodeIdentifyR-reference"))
  post_source <- Sys.getenv("QUICKNET_NIRAPOST_SOURCE", file.path(root,"..","tmp","nira-reference"))
  files <- c(node=file.path(node_source,"R","simulateResponses.R"),post=file.path(post_source,"R","permutationTest.R"))
  if(!all(file.exists(files)))stop("Obtain the fixed NodeIdentifyR/NIRApost revisions and set QUICKNET_NODEIDENTIFYR_SOURCE and QUICKNET_NIRAPOST_SOURCE.")
  env <- new.env(parent=globalenv());for(file in files)sys.source(file,env)
  design <- ising_designs()$mixed_sign
  set.seed(10401);samples <- env$simulateResponses(design$W,design$thresholds,"alleviating",2)
  current <- quicknet_nira_run_conditions(list(weight_matrix=design$W,thresholds=design$thresholds,beta=1),
    "alleviating",2,5000L,"literature",quicknet_nira_make_streams(10402L,1L)[[1L]],TRUE,100L)
  current_samples <- c(list(original=current$samples$original),current$samples$interventions)
  tab <- do.call(rbind,lapply(names(samples),function(name) {
    thresholds <- design$thresholds;if(name!="original") thresholds[name]<-thresholds[name]-2*sd(design$thresholds)
    exact <- exact_ising(design$W,thresholds)
    data.frame(condition=name,n_samples=5000L,iterations=100L,exact_mean=exact$total_mean,
      source_mean=mean(rowSums(samples[[name]])),quicknet_mean=mean(rowSums(current_samples[[name]])),
      exact_mcse=sqrt(exact$total_variance/5000))
  }))
  write_table(tab,"nodeidentifyr-direct-reference")
  x<-c(0,0,1);y<-c(1,2,3);B<-19999L
  set.seed(10403);ref<-env$permutationTest(x,y,nPerm=B)
  actual<-quicknet_nira_permutation_one(x,y,B,quicknet_nira_make_streams(10404L,1L)[[1L]])
  pooled<-c(x,y);allocations<-combn(seq_along(pooled),length(y))
  exact<-mean(abs(apply(allocations,2,function(i)mean(pooled[i])-mean(pooled[-i])))>=abs(mean(y)-mean(x))-1e-12)
  write_table(data.frame(exact_p=exact,source_p=ref$p_value,quicknet_p=actual$p_value,
    per_estimate_mcse=sqrt(exact*(1-exact)/B),permutations=B,source_d=ref$cohens_d,
    quicknet_raw_d=(mean(y)-mean(x))/quicknet_nira_pooled_sd(x,y)),"nirapost-direct-reference")
  store_result(list(files=files,sha256=vapply(files,function(f)digest::digest(file=f,algo="sha256"),character(1)),
    NodeIdentifyR_samples=samples,NIRApost_result=ref,quicknet_result=actual),"direct-reference-details")
  set.seed(991);near<-matrix(rnorm(100),20);near[,2]<-near[,1]+rnorm(20,sd=1e-8);colnames(near)<-LETTERS[1:5]
  write_table(as.data.frame(near),"near-singular-input")
}

stages<-list(sampling=run_sampling,stability=run_stability,permutation=run_permutation,moderation=run_moderation,failure_guard=run_failure_guard,symperturb=run_symperturb,bootstrap=run_bootstrap,reference=run_reference)
if(!stage %in% c(names(stages),"all"))stop("Unknown stage")
versions<-vapply(c("quickNet","IsingSampler","IsingFit","mgm"),function(p)as.character(packageVersion(p)),character(1))
manifest<-list(date=as.character(Sys.time()),R=R.version.string,versions=as.list(versions),stage=stage,
  symperturb_commit="76dd4178b285b80beb69f14a642e84ed1cabc7a0",NIRApost_commit="6231832736df7c693b630820729f73f64c30ec92",
  NodeIdentifyR_commit="22ceb4c9c19d6c95a4030ea4ce13d8545cbbfdb3",
  thread_policy="Launch R with OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 MKL_NUM_THREADS=1; in-process environment changes may be too late for an initialized BLAS.",
  source_sha256=as.list(setNames(vapply(c("R/nira_simulation.R","R/nira_moderation.R","R/symperturb_core.R","R/symperturb_analysis.R", "tools/validate-intervention-reliability.R"),function(p)digest::digest(file=file.path(root,p),algo="sha256"),character(1)),
    c("nira_simulation","nira_moderation","symperturb_core","symperturb_analysis","audit_script"))))
store_result(manifest,paste0("manifest-",stage))
jsonlite::write_json(manifest,file.path(output,paste0("manifest-",stage,".json")),auto_unbox=TRUE,pretty=TRUE)
for(name in if(stage=="all")names(stages) else stage) {
  timing<-system.time(stages[[name]]());write_table(data.frame(stage=name,user=timing[[1]],system=timing[[2]],elapsed=timing[[3]]),paste0("timing-",name))
}
