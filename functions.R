# define functions --------------------------------------------------------

# define negation of %in%
`%!in%` <- Negate(`%in%`)

#' descr_table: function for making marginal or conditional APA-formatted
#'   descriptive statistics tables 
#'
#' @param data a data frame
#' @param vars a vector of variable names used for column selection 
#' @param group a quoted column name identifying a grouping variable for conditional
#'   descriptive statistics. if NA, marginal statistics are computed. Defaults to NA
#' @param digits numeric scalar; selects the number of decimal places for printed values
#' @param missing_char string used to replace NA / NaN / Inf / -Inf computed values
#'
#' @return a data frame
#' 
descr_table = function(data, vars, group=NA, digits=2, reference_grp=NA,
                       missing_char="") {
  
  # define helper functions with desirable default NA behavior and formatted output
  Validn <- function(x) {length(which(!is.na(x)))}
  Mean <- function(x) {mean(x, na.rm=T) %>% formatC(digits=digits, format="f")}
  Sd <- function(x) {sd(x, na.rm=T) %>% formatC(digits=digits, format="f")}
  Min <- function(x) {min(x, na.rm=T) %>% formatC(digits=digits, format="f")}
  Max <- function(x) {max(x, na.rm=T) %>% formatC(digits=digits, format="f")}
  
  if (all(!is.na(group))) {
    
    all_vars = c(vars, paste0(group))
    
    dt_group = c(group, "variable")
    
    dtable = data.table::melt(
      data.table::as.data.table(
        data %>% dplyr::select(!!!all_vars)),
      id=group)[, .(mean = Mean(value),
                    sd = Sd(value),
                    n = Validn(value),
                    min = Min(value),
                    max = Max(value)),
                by = dt_group] %>%
      # reorder cols
      dplyr::select("variable", group, "n", "mean", "sd", "min", "max") 
    
    # rename cols
    names(dtable) <- c("Variable", group, "Valid n", "Mean", "Std Dev", "Min", "Max")                
    
    
    # # how many levels are there of the grouping variable?
    # n_levels = length(levels(factor(data[[group]])))
    # 
    # # replace the Variable field with blanks for every other row
    # dtable$Variable[(1:nrow(dtable))[1:nrow(dtable) %% n_levels != 1]] <- ""
    
  } else {
    dtable = data.table::melt(
      data.table::setDT(
        data %>% dplyr::select(!!!vars)))[, .(mean = Mean(value),
                                              sd = Sd(value),
                                              n = Validn(value),
                                              min = Min(value),
                                              max = Max(value)),
                                          by=variable] %>%
      # reorder cols
      dplyr::select("variable", "n", "mean", "sd", "min", "max") 
    
    # rename cols
    names(dtable) <- c("Variable", "Valid n", "Mean", "Std Dev", "Min", "Max")                
    
    
  }
  
  # convert Variable to character (somehow it became a factor)
  dtable[, Variable := as.character(Variable)]
  
  # coerce back to a data frame
  dtable = as.data.frame(dtable)
  
  # replace any NAs, NaN, -Inf,  with missing_Char
  dtable[sapply(dtable, function(x) {x %in% c("Inf", "-Inf", "NA", "NaN")})] = missing_char
  
  if (!is.na(reference_grp)) {
    
    # select the row and relevant columns for the reference group 
    reference_stats = dtable %>% filter(eval(parse(text=reference_grp))) %>% 
                        dplyr::select('Variable', 'Mean', 'Std Dev', 'Valid n')
    
    # rename for merge
    reference_stats = reference_stats %>% dplyr::rename(ref_Mean=Mean, ref_SD=`Std Dev`, ref_n = `Valid n`)
     
    # merge the columns containing the reference stats onto the table
    dtable = merge(dtable, reference_stats, by=c("Variable"))
    
    # calculate pooled standard deviation
    dtable = dtable %>% dplyr::mutate(SDpooled = sqrt((((as.numeric(`Valid n`)-1)*as.numeric(`Std Dev`)^2)  +
                                                ((as.numeric(ref_n)-1)*as.numeric(ref_SD)^2)) / 
                                               (as.numeric(`Valid n`) + as.numeric(ref_n) - 2)) )
    
    # calculate Cohen's D
    dtable = dtable %>% dplyr::mutate(`Cohen D` = (as.numeric(Mean) - 
                                                     as.numeric(ref_Mean)) / SDpooled)
    
    # format Cohen's D for nice printing
    dtable = dtable %>% dplyr::mutate(`Cohen D` = formatC(`Cohen D`, digits=digits, format="f"))
    
    # drop calculation columns
    dtable = dtable %>% dplyr::select(-c(ref_Mean, ref_SD, ref_n, SDpooled))
  }
  
  return(dtable)
}



# define function for assigning gifted status based on optimal id
identify_opti <- function(data, assessments, nom, nom_cutoff, test_cutoff,
                          mode = "decisions", weights = NA) {
  
  if (mode %!in% c("decisions", "meanscores")) {
    stop("argument 'mode' must be one of 'decisions' or 'meanscores'")
  }
  
  # calculate nomination cutoff at empirical percentile
  nom_cutoff_val = qnorm(nom_cutoff)
  
  
  # if no weights were given, set them all to 1
  if (is.na(weights[1])) {
    weights <- rep(1, times = length(assessments))
  }
  
  # normalize the weights
  w <- weights / sum(weights)
  
  # shortcut for calculating mean (as weighted sum)
  meanscore <- as.matrix(data[, assessments]) %*% w
 
  # shrinkage-adjusted cutoff
  if (length(assessments) > 1) {
      r = cor(data[assessments], use='complete.obs')
      sd_shrinkage_factor = sqrt(var_mean(r=r, w=w))
  } else {sd_shrinkage_factor = 0}
  
  test_cutoff_val = qnorm(test_cutoff, 0, sd=sd_shrinkage_factor)
  
  # shrinkage-adjusted cutoff
  opti_gifted <- (data[, nom] >= nom_cutoff_val) & (meanscore >= test_cutoff_val)
  
  if (mode == "decisions") {
    return(opti_gifted)
  } else if (mode == "meanscores") {
    return(meanscore)
  }
}

# define function for creating the equity summary table
equity_table <- function(data, id_var, group=NA) {
  
  # if no grouping variable was given, make a virtual one for the group_by
  #   which will be dropped
  if (is.na(group[1])) {
    data$nogroup <- 1
    group = "nogroup"
  }
  
  tbl <- data %>%
    group_by_at(group) %>%
    summarize(total = dplyr::n(), .groups = "keep")
  
  for (i in 1:length(id_var)) {
    this_id_var <- id_var[i]
    
    tbl_numerator <- data %>%
      dplyr::filter(get(this_id_var) == 1) %>%
      group_by_at(group) %>%
      summarize(this_id_var = dplyr::n(), .groups = "keep")
    
    tbl <- merge(tbl, tbl_numerator, by = group, all = TRUE)
    tbl <- tbl %>% mutate(this_id_var = ifelse(is.na(this_id_var), 0, this_id_var))
    
    tbl <- tbl %>% dplyr::rename(!!this_id_var := "this_id_var")
  }
  
  tbl = tbl[, names(tbl) != "nogroup"]
  
  return(tbl)
}


equity_table_to_prop <- function(data, group=NA, target_vars, mode = "RI", total_var = "total") {
  
  # if no grouping variable was given, make a virtual one for the group_by
  #   which will be dropped
  if (is.na(group[1])) {
    data$nogroup <- 1
    group = "nogroup"
  }
  
  # consolidate table over group
  # this allows groups in the equity table to be collapsed together, eg marginal statistics
  data <- data %>%
    group_by_at(group) %>%
    summarize_at(all_of(target_vars), sum, na.rm = TRUE)
  
  # mode "RI" is for calculating representation index
  #  the proportions in the identified/gifted column(s) are the proportion of the row's
  #   group amongst the identified population
  if (mode == "RI") {
    
    # calculate column sums
    col_sums <- apply(data[, target_vars], 2, sum, na.rm = TRUE)
    
    # expand col_sums to the correct shape
    col_sums <- matrix(col_sums, nrow = nrow(data), ncol = length(target_vars), byrow = TRUE)
    
    data[, target_vars] <- data[, target_vars] / col_sums
  }
  
  # mode "group_proportions" is for calculationg the proportion of each row's group
  #  that is identified. The denominator is the total N for the row.
  # The column describing the total (e.g., total_var) is treated differently; its denominator
  #  is the total population size - so users can quickly understand how the population is divided
  #  into groups of interest.
  else if (mode == "group_proportions") {
    
    # calculate n by group and expand into matrix of the proper shape
    n_by_group <- matrix(data[[total_var]], nrow = nrow(data), ncol = length(target_vars), byrow = FALSE)
    total_n <- sum(data[, total_var], na.rm = TRUE)
    
    non_total_target_vars <- target_vars[target_vars %!in% total_var]
    
    data[, non_total_target_vars] <- data[, non_total_target_vars] / n_by_group
    data[, total_var] <- data[, total_var] / total_n
  }
  
  # mode "pop_proportions" is for calculationg the proportion of the population that
  #  is described by the row-column combination
  #  that is identified. The denominator is the total N for table.
  else if (mode == "pop_proportions") {
    total_n <- sum(data[, total_var], na.rm = TRUE)
    
    data[, target_vars] <- data[, target_vars] / total_n
  }
  
  # drop the virtual grouping variable if it exists
  data = data[, names(data) != "nogroup"]
  
  return(data)
}


# define functions for calculating metrics

calc_RI <- function(eq_tbl, group, total_var, target_vars) {
  eq_prop_tbl <- equity_table_to_prop(
    data = eq_tbl, group = group,
    target_vars = c(total_var, target_vars),
    mode = "RI"
  )
  
  RI <- eq_prop_tbl[, target_vars] / matrix(as.numeric(
    eq_prop_tbl[[total_var]]
  ),
  nrow = nrow(eq_prop_tbl), ncol = length(target_vars)
  )
  
  RI <- cbind(eq_prop_tbl[, group], RI)
  
  RI_long <- pivot_longer(data = RI, cols = all_of(target_vars), 
                          names_to = "comparison", values_to = "RI")
  
  RI_long <- reshape::melt(data.frame(RI_long), variable_name = "metric", 
                           id.vars = c(group, "comparison"))
  
  RI_long <- RI_long[, c(group, "comparison", "metric", "value")]
  
  return(RI_long)
}


calc_RI_ratio <- function(eq_tbl, group, total_var, target_vars, reference_grp) {
  
  RI_tbl <- calc_RI(eq_tbl = eq_tbl, group = group, total_var = total_var, 
                    target_vars = target_vars)
  
  
  # grab the row for the reference category
  
  ref_df <- RI_tbl %>% filter(eval(parse(text = reference_grp)))
  ref_df <- ref_df[, c("comparison", "value")]
  
  joined <- merge(RI_tbl, ref_df, by = c("comparison"), all.x = TRUE)
  
  joined$value <- joined$value.x / joined$value.y
  
  joined$metric <- "RI_ratio"
  
  joined <- joined[, c(group, "comparison", "metric", "value")]
  
  return(joined)
}


calc_comparison_metrics <- function(eq_tbl, group, total_var, target_vars, reference_grp) {
  
  # collapse categories if needed
  eq_tbl <- eq_tbl %>%
    group_by_at(group) %>%
    summarize_at(all_of(c(total_var, target_vars)), sum, na.rm = TRUE)
  
  # make contingency table
  ctable <- list()
  
  # for each target variable...
  for (i in 1:length(target_vars)) {
    
    # subset columns
    ctable[[i]] <- eq_tbl[, c(group, total_var, target_vars[i])]
    
    # put the current target variable in the 'comparison' column
    ctable[[i]][, "comparison"] = target_vars[i] 
    
    # compute the count of non-identified by group  
    ctable[[i]][, "non_identified"] <- ctable[[i]][, total_var] - ctable[[i]][, target_vars[i]]
    
    # calc pct identified and odds by group
    ctable[[i]][, "pct_identified"] <- ctable[[i]][, target_vars[i]] / ctable[[i]][, total_var]
    ctable[[i]][, "odds"] <- ctable[[i]][, target_vars[i]] / ctable[[i]][, "non_identified"]
    
    # get the pct_identified and odds for the referene group
    ref_pct = ctable[[i]] %>% 
      dplyr::filter(eval(parse(text = reference_grp))) %>% 
      as.data.frame() %>% 
      dplyr::select(pct_identified) %>% 
      as.numeric()
    
    ref_odds = ctable[[i]] %>% 
      dplyr::filter(eval(parse(text = reference_grp))) %>% 
      as.data.frame() %>% 
      dplyr::select(odds) %>% 
      as.numeric()
    
    # divide to get ratios, then get logit
    ctable[[i]][, "RR"] <- ctable[[i]][, "pct_identified"] / ref_pct
    ctable[[i]][, "OR"] <- ctable[[i]][, "odds"] / ref_odds
    ctable[[i]][, "logit"] <- log(ctable[[i]][, "OR"])
    
    # remove the unnecessary columns
    ctable[[i]] <- ctable[[i]][, names(ctable[[i]]) %in% 
                                 c(group, "comparison", "pct_identified", "odds", "RR", "OR", "logit")]
    
  }
  
  # bind the list together into a dataframe
  result = do.call("rbind", ctable)
  
  # melt to long form
  result_long = reshape::melt(data.frame(result), variable_name = "metric", 
                              id.vars = c(group, "comparison"))
  
  return(result_long)
}



calc_change <- function(eq_tbl, group=NA, baseline, comparison) {
  
  # if no grouping variable was given, make a virtual one for the group_by
  #   which will be dropped
  if (is.na(group[1])) {
    eq_tbl$nogroup <- 1
    group = "nogroup"
  }
  
  target_vars <- c(baseline, comparison)
  
  eq_tbl <- eq_tbl %>%
    group_by_at(group) %>%
    summarize_at(all_of(target_vars), sum, na.rm = TRUE)
  
  pct_change <- ((eq_tbl[[comparison]] - eq_tbl[[baseline]]) / eq_tbl[[baseline]]) * 100
  count_change <- eq_tbl[[comparison]] - eq_tbl[[baseline]]
  
  pct_change[is.infinite(pct_change)] <- NA
  
  new_df <- eq_tbl[, group]
  
  new_df$baseline <- baseline
  new_df$comparison <- comparison
  
  new_df$pct_change <- pct_change
  new_df$count_change <- count_change
  
  new_df <- reshape::melt(data.frame(new_df), variable_name = "metric", id.vars = c(group, "baseline", "comparison"))
  
  # drop the virtual grouping variable if it exists
  new_df = new_df[, names(new_df) != "nogroup"]
  
  return(new_df)
}


calc_CramerV <- function(eq_tbl, group, total_var, target_vars) {
  
  # collapse categories if needed
  eq_tbl <- eq_tbl %>%
    group_by_at(group) %>%
    summarize_at(all_of(c(total_var, target_vars)), sum, na.rm = TRUE)
  
  # make contingency table
  # collapse groups
  # create a new column `x` with the three columns collapsed together
  eq_tbl$new_group <- apply(eq_tbl[, group], 1, paste, collapse = "-")
  
  # remove the unnecessary columns
  eq_tbl <- eq_tbl[, !(names(eq_tbl) %in% group)]
  
  ctable <- list()
  result <- data.frame(comparison = NA, metric=NA, value = NA)
  
  for (i in 1:length(target_vars)) {
    ctable[[i]] <- eq_tbl[, c("new_group", total_var, target_vars[i])]
    
    ctable[[i]]$non_identified <- ctable[[i]][, total_var] - ctable[[i]][, target_vars[i]]
    ctable[[i]] <- ctable[[i]][, names(ctable[[i]]) %in% c("non_identified", target_vars[i])]
    
    result[i, 1:3] <- list(target_vars[i], "CramerV", round(DescTools::CramerV(ctable[[i]]),5))
  }
  
  return(result)
}


equity_table_to_long <- function(eq_tbl, group=NA, total_var, target_vars) {
  
  # if no grouping variable was given, make a virtual one for the group_by
  #   which will be dropped
  if (is.na(group[1])) {
    eq_tbl$nogroup <- 1
    group = "nogroup"
  }
  
  # collapse categories if needed
  eq_tbl <- eq_tbl %>%
    group_by_at(group) %>%
    summarize_at(all_of(c(total_var, target_vars)), sum, na.rm = TRUE)
  
  eq_tbl_long <- reshape::melt(data.frame(eq_tbl), variable_name = "comparison", id.vars = group)
  
  eq_tbl_long$metric <- "count"
  
  eq_tbl_long <- eq_tbl_long[, c(group, "comparison", "metric", "value")]
  
  # drop the virtual grouping variable if it exists
  eq_tbl_long  = eq_tbl_long [, names(eq_tbl_long ) != "nogroup"]
  
  return(eq_tbl_long)
}





### Define master function for calculating equity statistics ###

get_equity <- function(data, group, reference_grp, assessments, nom, nom_cutoff, 
                       test_cutoff, baseline_id_var, weights = NA) {
  
  # create a designator variable for identification under optimal id
  data$opti_gifted <- identify_opti(
    data = data, assessments = assessments, nom = nom, nom_cutoff = nom_cutoff, 
    test_cutoff = test_cutoff, mode="decisions", weights=weights)
  
  # calculate equity table statistics
  eq_tbl <- equity_table(
    data = data, id_var = c(baseline_id_var, "opti_gifted"),
    group = group
  )
  
  # now calculate all the metrics we want to report
  t1 <- calc_change(
    eq_tbl = eq_tbl, group = group, baseline = baseline_id_var,
    comparison = "opti_gifted"
  )
  
  t2 <- equity_table_to_long(eq_tbl,
                             group = group, total_var = "total",
                             target_vars = c(baseline_id_var, "opti_gifted")
  )
  
  t3 <- calc_RI(
    eq_tbl = eq_tbl, group = group, total_var = "total",
    target_vars = c(baseline_id_var, "opti_gifted")
  )
  
  t4 <- calc_RI_ratio(
    eq_tbl = eq_tbl, group = group, total_var = "total",
    target_vars = c(baseline_id_var, "opti_gifted"), reference_grp = reference_grp
  )
  
  t5 <- calc_comparison_metrics(
    eq_tbl = eq_tbl, group = group, total_var = "total",
    target_vars = c(baseline_id_var, "opti_gifted"), reference_grp = reference_grp
  ) 
  
  t6 <- calc_CramerV(
    eq_tbl = eq_tbl, group = group, total_var = "total",
    target_vars = c(baseline_id_var, "opti_gifted")
  )
  
  t7 <- equity_table_to_long(eq_tbl,
                             group = NA, total_var = "total",
                             target_vars = c(baseline_id_var, "opti_gifted")
  )
  
  # stack sub-results
  out = bind_rows(t2, t1, t3, t4, t5, t6, t7)
  
  # reorder cols
  out = out[, c(group, "metric", "baseline", "comparison", "value")]
  
  return(out)
  
}



equity_plot = function(data,
                       group,
                       reference_grp,
                       assessments,
                       nom,
                       nom_cutoff,
                       mean_cutoff,
                       baseline_id_var,
                       plot_metric,
                       weights=NA)  {
  
  summary_tbl = get_equity(data=data,
                           group=group,
                           reference_grp=reference_grp,
                           assessments=assessments,
                           nom=nom,
                           nom_cutoff=nom_cutoff,
                           test_cutoff=mean_cutoff,
                           baseline_id_var=baseline_id_var,
                           weights=weights)
  
  # this line filters out any row from the summary table with an NA for any
  #  of the columns in 'group' -- except for the rows for Cramer's V
  summary_tbl = summary_tbl[!apply(is.na(
    dplyr::select_at(summary_tbl, group)), 
    1, max) | summary_tbl$metric == 'CramerV', ]
  
  terse_metric = dplyr::case_when(
    plot_metric == 'Count' ~ 'count',
    plot_metric == 'Representation Index' ~ 'RI',
    plot_metric == 'Relative Risk' ~ 'RR',
    plot_metric == 'Proportion Identified' ~ 'pct_identified',
    plot_metric == "Cramer's V"~ 'CramerV'
  )
  
  p = ggplot(data=dplyr::filter(summary_tbl, metric==terse_metric), 
             aes(x=comparison, y=value, fill=comparison))+
    geom_bar(stat="identity", alpha=.65)+
    geom_text(aes(label=round(value, 3)), nudge_y=-0.01, size=5)+
    geom_hline(yintercept=0)+
    theme_bw()+
    theme(legend.position="bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_fill_brewer(palette="Set1")+
    ggtitle(paste0("Metric: ", plot_metric))+
    theme(text=element_text(size=16))+
    labs(fill=NULL)
  
  if (length(group)==1) {
    if (terse_metric %in% c('RR', 'logit', 'RI_ratio')) {
      p = p + facet_wrap(vars(get(group)))
    } else if (terse_metric != 'CramerV') {
      p = p + facet_wrap(vars(get(group)), scales="free_y")
    }
  }
  
  if (length(group)==2) {
    if (terse_metric %in% c('RR', 'logit', 'RI_ratio')) {
      p = p + facet_grid(rows=vars(get(group[1])), cols=vars(get(group[2])))
    } else if (terse_metric != 'CramerV') {
      p = p + facet_grid(rows=vars(get(group[1])), cols=vars(get(group[2])), 
                         scales="free_y")
    }
  }
  
  return(list(p=p, summary_tbl=summary_tbl))
}

#test
