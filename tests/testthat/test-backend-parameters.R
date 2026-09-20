test_that("direct MGM arguments and defaults match the source estimator", {
  set.seed(802)
  d <- as.data.frame(matrix(rnorm(900), 300, 3))
  set.seed(88)
  direct <- mgm::mgm(as.matrix(d), type = rep("g", 3), level = rep(1, 3), pbar = FALSE, signInfo = FALSE)
  set.seed(88)
  wrapped <- quickNet(d, model = "mgm", pie = FALSE, types = rep("g",3), levels = rep(1,3), signInfo = FALSE, DoNotPlot = TRUE)
  expect_equal(unname(wrapped$graph), unname(quicknet_apply_signs(direct$pairwise$wadj, direct$pairwise$signs)))
  expect_identical(wrapped$fit$call$lambdaSel, direct$call$lambdaSel)
  expect_identical(wrapped$fit$call$ruleReg, direct$call$ruleReg)
  expect_null(quicknet_refit_gamma(wrapped))
  ebic <- quickNet(d, model = "mgm", pie = FALSE, types = rep("g",3), levels = rep(1,3), lambdaSel = "EBIC", ruleReg = "OR", gamma = 0.4, threshold = "none", signInfo = FALSE, DoNotPlot = TRUE)
  expect_equal(ebic$fit$call$lambdaGam, .4)
  refit <- quicknet_refit_like(d, ebic)
  expect_equal(refit$graph, ebic$graph)
  expect_identical(refit$fit$call$ruleReg, "OR")
  expect_identical(refit$fit$call$threshold, "none")
})

test_that("EBIC correlations, missingness and tuning controls reach bootnet", {
  set.seed(803)
  d <- as.data.frame(matrix(rnorm(1200), 300, 4))
  d[1:10,1] <- NA
  direct <- bootnet::estimateNetwork(d, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"), nlambda = 20, verbose = FALSE)
  wrapped <- EBICglassoNet(d, corMethod = "cor", corArgs = list(method = "spearman"), nlambda = 20)
  expect_equal(wrapped$graph, direct$graph)
  expect_equal(nrow(wrapped$data), 300)
  expect_identical(wrapped$meta$missing, "pairwise")
  expect_equal(quicknet_refit_like(d, wrapped)$graph, wrapped$graph)
  expect_error(EBICglassoNet(d, nlamdba = 20), "Unknown or unsupported")
  expect_error(quicknet_fit_cross_sectional(d, "correlation"), "non-finite")
  expect_equal(quicknet_fit_cross_sectional(d,"correlation",missing="pairwise")$fit$correlation,
               stats::cor(d,use="pairwise.complete.obs"))
})

test_that("CLPN native controls and grouped folds are both retained", {
  skip_if_not_installed("glmnet")
  set.seed(804)
  d <- as.data.frame(matrix(rnorm(900), 100, 9))
  colnames(d) <- as.vector(outer(paste0("x",1:3),1:3,paste,sep="_t"))
  f <- PanelNet(d,nodes=paste0("x",1:3),waves=1:3,nfolds=5,seed=7,nlambda=25,intercept=FALSE)
  design <- f$fit$design
  raw <- glmnet::cv.glmnet(design$predictors,design$outcomes[,1],foldid=f$fit$glmnet$foldid,nlambda=25,intercept=FALSE)
  expect_equal(as.numeric(stats::coef(raw,s="lambda.1se")[-1,1]),as.numeric(f$graph[1,]))
  expect_equal(f$meta$nfolds,length(unique(f$fit$glmnet$foldid)))
  expect_true(all(vapply(split(f$fit$glmnet$foldid,design$meta$id),function(x) length(unique(x))==1,logical(1))))
  expect_true(f$meta$standardize)
  expect_false(f$meta$standardize_data)
  expect_error(PanelNet(d,nodes=paste0("x",1:3),waves=1:3,nlamdba=25),"Unknown or unsupported")
  expect_error(PanelNet(d,nodes=paste0("x",1:3),waves=1:3,nfolds=2),"nfolds must")
  expect_error(PanelNet(d,nodes=paste0("x",1:3),waves=1:3,alpha=2),"alpha must")
})

test_that("lavaan estimator and identification defaults are honored", {
  skip_if_not_installed("lavaan")
  set.seed(805)
  z <- matrix(rnorm(800),400,2)
  d <- as.data.frame(cbind(z[,1]+matrix(rnorm(1200,sd=.5),400),z[,2]+matrix(rnorm(1200,sd=.5),400)))
  colnames(d) <- paste0("x",1:6)
  syntax <- "f1 =~ x1+x2+x3\nf2 =~ x4+x5+x6"
  raw <- lavaan::cfa(syntax,data=d,estimator="MLR")
  f <- LatentNet(d,syntax,estimator="MLR",residual=FALSE)
  expect_equal(f$fit$model@Options$std.lv,raw@Options$std.lv)
  expect_equal(f$fit$model@Options$se,raw@Options$se)
  expect_identical(f$meta$estimator,"MLR")
  expect_equal(f$fit$model@Options$test,raw@Options$test)
  expected <- lavaan::lavInspect(raw,"cor.lv"); diag(expected) <- 0
  expect_equal(unname(f$graph),unname(as.matrix(expected)))
  expect_identical(f$meta$backend_version,as.character(utils::packageVersion("lavaan")))
})

test_that("psychonetrics chooses its estimator and missing-data defaults", {
  skip_if_not_installed("psychonetrics")
  set.seed(806)
  d <- as.data.frame(matrix(rnorm(800),200,4)); d[1:8,1] <- NA
  raw <- quicknet_psychonetrics_run(psychonetrics::ggm(d,verbose=FALSE))
  f <- ConfirmatoryNet(d)
  expect_equal(nrow(f$data),nrow(d))
  expect_equal(f$meta$estimator,raw@estimator)
  expected <- psychonetrics::getmatrix(raw,"omega"); diag(expected) <- 0
  expect_equal(unname(f$graph),unname(expected))
  expect_error(ConfirmatoryNet(d,optimzer="nlminb"),"Unknown or unsupported")
})

test_that("unsupported time-varying defaults and row-specific refits fail clearly", {
  d <- data.frame(x=rnorm(60),y=rnorm(60))
  expect_error(TimeVaryingNet(d,types=c("g","g"),levels=c(1,1)),"estpoints and bandwidth")
  expect_error(quicknet_check_row_args(list(weights=rep(1,60)),"Stability"),"realign")
  expect_error(NetworkPower(method="powerly",nodes=4,density=.3),"range_lower and range_upper")
  expect_equal(formals(Bridge)$normalize,formals(networktools::bridge)$normalize)
  expect_equal(formals(netCor)$nperm,formals(ape::mantel.test)$nperm)
})

test_that("mixed VAR layers match native fits with direct non-default controls", {
  set.seed(807)
  d <- as.data.frame(matrix(rnorm(450),150,3))
  args <- list(data=as.matrix(d),type=rep("g",3),level=rep(1,3),lags=1,
    lambdaSel="EBIC",scale=FALSE,threshold="none",pbar=FALSE,signInfo=FALSE)
  native <- do.call(mgm::mvar,args)
  fit <- MixedVARNet(d,types=rep("g",3),levels=rep(1,3),lags=1,
    lambdaSel="EBIC",scale=FALSE,threshold="none",signInfo=FALSE)
  expect_equal(unname(fit$graph),unname(quicknet_dynamic_extract_mvar_networks(native,names(d),1)[[1]]))
  expect_identical(fit$meta$backend_settings$threshold,native$call$threshold)
  native_tv <- do.call(mgm::tvmvar,c(args,list(estpoints=c(.3,.7),bandwidth=.5)))
  tv <- TimeVaryingNet(d,types=rep("g",3),levels=rep(1,3),lags=1,estpoints=c(.3,.7),bandwidth=.5,
    scale=FALSE,threshold="none",signInfo=FALSE)
  expected <- quicknet_dynamic_extract_tvmvar_networks(native_tv,names(d),c(.3,.7),1)
  expect_equal(unname(tv$networks$estpoint_1),unname(expected$estpoint_1))
  expect_equal(unname(tv$networks$estpoint_2),unname(expected$estpoint_2))
  expect_error(MixedVARNet(d,types=rep("g",3),levels=rep(1,3)),"lags must be specified")
})

test_that("powerly forwards direct controls and reports effective targets", {
  skip_if_not_installed("powerly")
  seen <- NULL
  stub <- function() {
    seen <<- list(samples=samples,replications=replications,boots=boots,
      measure=measure,measure_value=measure_value,statistic_value=statistic_value,dots=list(...))
    list()
  }
  formals(stub) <- formals(powerly::powerly)
  testthat::local_mocked_bindings(powerly=stub,.package="powerly")
  testthat::local_mocked_bindings(
    quicknet_power_powerly_recommendation=function(fit,target_probability) data.frame(recommended_n=200,reached=TRUE,achieved_probability=target_probability),
    quicknet_power_powerly_summary=function(fit,target_metric,target_value) data.frame(metric=target_metric,target=target_value)
  )
  fit <- NetworkPower(method="powerly",nodes=4,density=.3,range_lower=100,range_upper=400)
  expect_equal(seen$samples,30)
  expect_equal(seen$replications,30)
  expect_equal(seen$boots,10000)
  expect_equal(seen$measure,"sen")
  expect_equal(fit$settings$backend_gamma,.5)
  expect_equal(seen$dots$positive,.9)
  expect_equal(seen$dots$range,c(.5,1))
  fit <- NetworkPower(method="powerly",nodes=4,density=.3,range_lower=100,range_upper=400,
    replications=7,measure="mcc",measure_value=.4,statistic_value=.9,boots=11)
  expect_equal(seen$replications,7)
  expect_equal(seen$boots,11)
  expect_equal(fit$settings$target_metric,"mcc")
  expect_equal(fit$settings$target_probability,.9)
  expect_equal(fit$summary$metric,"mcc")
  expect_equal(fit$summary$target,.4)
})

test_that("unknown arguments and inherited comparison controls are explicit", {
  expect_error(quickNet(mtcars[,1:3],pie=FALSE,lamdbaSel="EBIC",DoNotPlot=TRUE),"Unknown or unsupported")
  expect_equal(formals(NetCompare)$it,formals(NCT_gl)$it)
  expect_equal(formals(NetCompare)$test.edges,formals(NCT_gl)$test.edges)
  expect_equal(formals(NetCompare)$test.centrality,formals(NCT_gl)$test.centrality)
  expect_equal(formals(NetCompare)$centrality,formals(NCT_gl)$centrality)
})

test_that("longitudinal source defaults and explicit controls reach their backends", {
  skip_if_not_installed("mlVAR")
  skip_if_not_installed("graphicalVAR")
  set.seed(808)
  d <- data.frame(id=rep(1:10,each=30),a=rnorm(300),b=rnorm(300),c=rnorm(300))
  native <- suppressWarnings(mlVAR::mlVAR(d,vars=c("a","b","c"),idvar="id",verbose=FALSE,scaleWithin=TRUE))
  f <- suppressWarnings(LongitudinalNet(d,vars=c("a","b","c"),model="mlVAR",scaleWithin=TRUE))
  expect_equal(f$meta$estimator,native$input$estimator)
  expect_equal(f$meta$temporal,native$input$temporal)
  expect_true(f$meta$backend_settings$scaleWithin)
  expect_equal(unname(f$networks$temporal),unname(quicknet_mlvar_get_net(native,"temporal",c("a","b","c"))))
  expect_equal(unname(f$networks$contemporaneous),unname(quicknet_mlvar_get_net(native,"contemporaneous",c("a","b","c"))))
  native_g <- suppressWarnings(graphicalVAR::mlGraphicalVAR(d,vars=c("a","b","c"),idvar="id",nLambda=3,verbose=FALSE))
  g <- suppressWarnings(LongitudinalNet(d,vars=c("a","b","c"),nLambda=3))
  expect_true(g$meta$backend_settings$subjectNetworks)
  expect_equal(unname(g$networks$temporal),unname(quicknet_from_qgraph_matrix(native_g$fixedPDC,directed=TRUE)))
  expect_error(LongitudinalNet(d,vars=c("a","b","c"),model="mlVAR",centerWithin=TRUE),"not an mlVAR argument")
})

test_that("older MGM objects keep their fitted criterion and edge rule", {
  set.seed(809)
  d <- as.data.frame(matrix(rnorm(600),200,3))
  old <- quicknet_fit_cross_sectional(d,"mgm",types=rep("g",3),levels=rep(1,3),
    backend_args=list(lambdaSel="EBIC",ruleReg="OR",signInfo=FALSE))
  old$meta$backend_args <- old$meta$backend_settings <- NULL
  refit <- quicknet_refit_like(d,old)
  expect_identical(refit$fit$call$lambdaSel,"EBIC")
  expect_identical(refit$fit$call$ruleReg,"OR")
  expect_equal(refit$graph,old$graph)
  compare <- NetCompare(old,old,it=1,progressbar=FALSE)
  expect_equal(compare$nw1,old$graph)
  expect_equal(compare$info$call$gamma,.25)
})


test_that("legacy EBIC metadata cannot replace the original correlation estimator", {
  d <- mtcars[,1:4]
  f <- suppressWarnings(quicknet_fit_cross_sectional(d))
  f$meta$backend_args <- NULL
  f$meta$cor_method <- "spearman" # Previously recorded but never forwarded.
  refit <- suppressWarnings(quicknet_refit_like(d,f))
  expect_equal(refit$graph,f$graph)
  expect_identical(refit$meta$cor_method,"pearson")
})

test_that("legacy association refits preserve their matrix-repair policy", {
  d <- data.frame(a=1:20,b=2*(1:20),c=3*(1:20))
  repaired <- quicknet_make_positive_definite(stats::cor(d))
  graph <- -stats::cov2cor(solve(repaired));diag(graph)<-0
  old <- quicknet_fit("partial",data=d,networks=list(default=graph),fit=list(correlation=repaired),
    meta=list(missing="listwise",cor_method="pearson",call=quote(quickNet(d,model="partial"))))
  refit <- quicknet_refit_like(d,old)
  expect_equal(refit$graph,old$graph)
  expect_true(refit$meta$repair_pd)
  expect_error(quicknet_fit_cross_sectional(d,"partial"),"singular")
})
