#' Two Group Tests for Mean Differences
#'
#' Compute and return information on tests for two group differences in the data.
#'
#' @details
#' Tests require independent data unless otherwise specified.
#' *t*: t-test and a Welch modified t-test for uneven variances
#' *wilcox*: A Wilcoxon test
#' *bayes*: Bayesian t-test and a Welch modified Bayesian t-test for uneven
#'  variances
#' *factor*: Bayesian factor approach to a t-test
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param tests Vector of strings, or a string, indicating the tests to check.
#'  Options include 't', 'wilcox', 'bayes', and 'factor'
#' @param alpha Significance for confidence intervals, defaults to 0.05
#'
#' @returns A list of two group difference statistics and related information
#' @export
#'
#' @seealso [group_tests()], [stats::t.test()], [stats::wilcox.test()],
#'  [Bolstad::bayes.t.test()], [BayesFactor::ttestBF()]
#'
#' @examples
#' data <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(10,mean=2)),
#'                    'group'=c(rep('A',14),rep('B',10)) )
#' two_group_tests(data)
two_group_tests <- function(data,
                        tests=c('t','wilcox','bayes','factor'),
                        alpha=0.05){
  # Prepare Data
  tmp <- .prepare_data(data)
  data <- tmp$data
  groups <- tmp$groups
  form <- tmp$form

  if(length(groups)!=2) stop('The parameter data must have exactly two unique groups')

  # Two-group Tests
  res <- list()

  if('t' %in% tolower(tests) ){
    t_test <- stats::t.test(formula=form, data=data, var.equal=TRUE,
                     conf.level=1-alpha)
    res <- append(res, list('ttest_student'=list('pvalue'=t_test$p.value,
                                         'means'=t_test$estimate,
                                         'interval'=t_test$conf.int) ) )

    t_test1 <- stats::t.test(formula=form, data=data, var.equal=FALSE,
                      conf.level=1-alpha)
    res <- append(res, list('ttest_welch'=list('pvalue'=t_test1$p.value,
                                         'means'=t_test1$estimate,
                                         'interval'=t_test1$conf.int)) )
  }
  if('wilcox' %in% tolower(tests) ){
    tmp <- stats::wilcox.test(formula=form, data=data)
    res <- append(res, list('wilcox'=list('pvalue'=tmp$p.value) ) )
  }
  if('bayes' %in% tolower(tests) ){
    bayes_t_test <- Bolstad::bayes.t.test(formula = form, data=data,
                                          var.equal=TRUE, conf.level=1-alpha)
    res <- append(res, list('bayes'=list('pvalue'=bayes_t_test$p.value,
                                         'means'=bayes_t_test$estimate,
                                         'interval'=bayes_t_test$conf.int) ) )

    bayes_t_test1 <- Bolstad::bayes.t.test(formula = form, data=data,
                                          var.equal=FALSE, conf.level=1-alpha)
    res <- append(res, list(
      'bayes_welch'=list('pvalue'=bayes_t_test1$p.value,
                               'means'=bayes_t_test1$estimate,
                               'interval'=bayes_t_test1$conf.int) ) )
  }
  if('factor' %in% tolower(tests) ){
    factor_t_test <- BayesFactor::ttestBF(formula = form, data=data)

    res <- append(res, list('bayes_factor'=factor_t_test@bayesFactor$bf))
  }


  res
}


#' Group Tests for Mean/Median Differences
#'
#' Compute and return information on tests for group differences in the data.
#'
#' @details
#' Tests require independent data unless otherwise specified.
#'
#' Normally distributed data with equal variances and reasonably similarly sized
#'  groups.
#' *tukey*: Tukey honest significant differences
#' *snk*: Student-Newman-Keuls test
#' *lsd*: Least significant difference test
#' *bt*: Bonferroni corrected pairwist t-tests (for pooled and unpooled variances)
#'
#' The following typically permit inbalance in the data group sizes.
#' *kramer*: Tukey-Kramer test
#' *duncan*: Duncan's all-pairs comparisons
#' *scheffe*: Scheffe's test
#'
#' The following typically permit more inbalance in data sizes and unequal variances
#' *tamhaneT2*: Tamhane's T2 all-pairs comparison test
#' *uwh*: Ury-Wiggins and Hochberg's all-pairs comparison test
#'
#' The following typically permit inbalance in data sizes, unequal variances,
#'  and some non-normality
#' *gh*: Games-Howell test
#' *d3*: Dunnett's T3 test
#'
#' The following are non-parametric
#' *dunn*: Dunn's test of multiple comparisions using rank sums
#' *dscf*: Dwass, Steel, Critchlow and Fligner all-pairs comparision test
#' *kwc*: Kruskal-Wallis type,  Conover's non-parametric all-pairs comparison test
#' *kwd*: Kruskal-Wallis type,  Dunn's non-parametric all-pairs comparison test
#' *kwn*: Kruskal-Wallis type,  Nemeyi's non-parametric all-pairs comparison test
#' *median*: Brown-Mood all paris median test
#'
#' References to specific functions are given in the seealso section
#'
#' @param data Data.frame with the first column the values and the second column
#'  the group names
#' @param tests Vector of strings, or a string, indicating the tests to check.
#'  Options include 'tukey','snk','lsd','bt','kramer', 'duncan', 'scheffe',
#'  'tamhaneT2', 'uwh', 'gh', 'd3', 'dunn', 'dscf', 'kwc', 'kwd', 'kwn', and
#'  'median'
#' @param alpha Significance for confidence intervals / signficance of some
#'  tests, defaults to 0.05
#'
#' @returns A list of group difference statistics and related information
#' @export
#'
#' @seealso [two_group_tests()], [stats::TukeyHSD()], [PMCMRplus::snkTest()],
#'  [PMCMRplus::lsdTest()], [stats::pairwise.t.test()], [agricolae::HSD.test()],
#'  [PMCMRplus::duncanTest()], [PMCMRplus::scheffeTest()],
#'  [PMCMRplus::tamhaneT2Test()], [PMCMRplus::uryWigginsHochbergTest()],
#'  [games_howell()], [PMCMRplus::dunnettT3Test()], [dunn.test::dunn.test()],
#'  [PMCMRplus::dscfAllPairsTest()], [PMCMRplus::kwAllPairsConoverTest()],
#'  [PMCMRplus::kwAllPairsDunnTest()], [PMCMRplus::kwAllPairsNemenyiTest()],
#'  [PMCMRplus::medianAllPairsTest()]
#'
#' @examples
#' data <- data.frame('value'=c(rnorm(14,sd = 2), rnorm(6), rnorm(20,mean=2)),
#'                    'group'=c(rep('A',14),rep('B',6), rep('C',20)))
#' group_tests(data)
group_tests <- function(data,
                        tests=c('tukey','snk','lsd','bt','kramer', 'duncan',
                                'scheffe', 'tamhaneT2', 'uwh', 'gh', 'd3',
                                'dunn', 'dscf', 'kwc', 'kwd', 'kwn', 'median'),
                        alpha=0.05){
  # Prepare Data
  tmp <- .prepare_data(data)
  data <- tmp$data
  groups <- tmp$groups
  form <- tmp$form

  lm_mod <- stats::lm(form, data=data)
  anova_res <- stats::aov(form,data)

  # Group Tests
  means <- tapply(data[,1], data[,2], mean)
  medians <- tapply(data[,1], data[,2], stats::median)
  res <- list('means'=means,
              'medians'=medians)

  # TODO:: Add code for grouping

  # Equal Var and no/mild imbalance
  if('tukey' %in% tolower(tests) ){
    # TukeyHSD(anova_res)
    tukey <- stats::TukeyHSD(anova_res,conf.level = 1-alpha)
    tukey_info <- as.data.frame(tukey$group)
    # TODO:: Do this in-package
    tmp <- multcomp::cld(
      multcomp::glht(lm_mod,multcomp::mcp(group='Tukey')))
    tmp_group <- trimws( tmp$mcletters$monospacedLetters )

    res <- append(res, list('tukey'=list('details'=tukey_info,
                                         'groups'=tmp_group) ) )
  }
  if('snk' %in% tolower(tests) ){
    tmp <- PMCMRplus::snkTest(formula=form, data=data)
    res <- append(res, list('snk'=list('pvalues'=tmp$p.value,
                                       'groups'=
                                         .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('lsd' %in% tolower(tests) ){
    tmp <- PMCMRplus::lsdTest(formula=form, data=data)
    res <- append(res, list('lsd'=list('pvalues'=tmp$p.value,
                                       'groups'=
                                         .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }


  if('bt' %in% tolower(tests) ){
    tmp <- stats::pairwise.t.test(x = data[,1],g = data[,2],
                                  p.adjust.method = "bonferroni")
    tmp1 <- stats::pairwise.t.test(x = data[,1],g = data[,2],pool.sd = FALSE,
                                   p.adjust.method = "bonferroni")
    res <- append(res, list('bt'=list('pvalues'=tmp$p.value,
                                      'pvalues_nonpool'= tmp1$pvalues )) )
  }



  # Equal Var, imbalance (normal)
  if('kramer' %in% tolower(tests) ){
    tukey_kramer <- agricolae::HSD.test(lm_mod,'group',unbalanced = TRUE, alpha=alpha)
    res <- append(res, list('tukey_kramer'=tukey_kramer$groups ) )
  }
  if('duncan' %in% tolower(tests) ){
    tmp <- PMCMRplus::duncanTest(formula=form, data=data)#data[,1],g=data[,2])
    res <- append(res, list('duncan'=list('pvalues'=tmp$p.value,
                                          'groups'=
                                            .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('scheffe' %in% tolower(tests) ){
    tmp <- PMCMRplus::scheffeTest(formula=form, data=data)#data[,1],g=data[,2])
    res <- append(res, list('scheffe'=list('pvalues'=tmp$p.value,
                                           'groups'=
                                             .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }

  # Unequal var, unbalanced, normal
  if('tamhaneT2' %in% tolower(tests) ){
    tmp <- PMCMRplus::tamhaneT2Test(formula=form, data=data)#data[,1],g=data[,2])
    res <- append(res, list('tamhane_t2'=list('pvalues'=tmp$p.value,
                                              'groups'=
                                                .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('uwh' %in% tolower(tests) ){
    tmp <- PMCMRplus::uryWigginsHochbergTest(formula=form, data=data)#data[,1],g=data[,2])
    res <- append(res, list('uwh'=list('pvalues'=tmp$p.value,
                                       'groups'=
                                         .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }

  # Unequal var, unbalanced, non-normal
  if('gh' %in% tolower(tests) ){
    tmp <- games_howell(data)
    res <- append(res, list('games_howell'=tmp ) )
  }
  if('d3' %in% tolower(tests) ){
    tmp <- PMCMRplus::dunnettT3Test(formula=form, data=data)#data[,1], g=data[,2])
    res <- append(res, list('dunnett_t3'=list('pvalues'=tmp$p.value,
                                              'groups'=
                                                .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }

  # Non-parametric
  if('dunn' %in% tolower(tests) ){
    sink(nullfile())
    tmp <- dunn.test::dunn.test(data[,1],g=data[,2],alpha = alpha)
    sink()
    res <- append(res,
                  list('dunn'=
                         data.frame('comparisons'=tmp$comparisons,
                                    'adj.p'=tmp$P.adjusted) ) )
  }
  if('dscf' %in% tolower(tests) ){
    tmp <- PMCMRplus::dscfAllPairsTest(formula=form, data=data)
    res <- append(res, list('dscf'=list('pvalues'=tmp$p.value,
                                        'groups'=
                                          .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('kwc' %in% tolower(tests) ){
    tmp <- PMCMRplus::kwAllPairsConoverTest(formula=form, data=data)
    res <- append(res, list('kw_conover'=list('pvalues'=tmp$p.value,
                                              'groups'=
                                                .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('kwd' %in% tolower(tests) ){
    tmp <- PMCMRplus::kwAllPairsDunnTest(formula=form, data=data)
    res <- append(res, list('kw_dunn'=list('pvalues'=tmp$p.value,
                                           'groups'=
                                             .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('kwn' %in% tolower(tests) ){
    tmp <- PMCMRplus::kwAllPairsNemenyiTest(formula=form, data=data)
    res <- append(res, list('kw_nemenyi'=list('pvalues'=tmp$p.value,
                                              'groups'=
                                                .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }
  if('median' %in% tolower(tests) ){
    tmp <- suppressWarnings( PMCMRplus::medianAllPairsTest(formula=form, data=data,
                                         p.adjust.method = "bonferroni") )
    res <- append(res, list('median'=list('pvalues'=tmp$p.value,
                                          'groups'=
                                            .PMCMRplus_summary(tmp, alpha = alpha)
    )))
  }

  res
}
