get_raw_data_exp2 <- function() {
  data_bet <- read_csv("data/exp2/data-bet.csv")
  data_bet["condition"] <- "bet"
  data_cnf <- read_csv("data/exp2/data-conf.csv")
  data_cnf["condition"] <- "cnf"

  data <- rbind(data_bet, data_cnf) %>%
    filter(display == "Trial") %>%
    filter(`Screen Name` == "Bet or Conf" | `Screen Name` == "Resp_Match_High" | `Screen Name` == "Resp_Match_Low" | `Screen Name` == "Retrospective") %>%
    select(`Participant Private ID`, `Trial Number`, condition, `Screen Name`, metadata, metadata_order, Correct, Response, `Reaction Time`) %>%
    mutate(`Screen Name` = if_else(
      `Screen Name` == "Bet or Conf",
      "bet",
      if_else(
        `Screen Name` == "Retrospective",
        "cnf",
        "response"
      )
    )) %>%
    pivot_wider(names_from = `Screen Name`, values_from = c(Correct, `Reaction Time`, Response)) %>%
    select(-Correct_bet, -Correct_cnf) %>%
    rename(
      id = `Participant Private ID`,
      trial = `Trial Number`,
      correctness = Correct_response,
      bet_rt = `Reaction Time_bet`,
      ans_rt = `Reaction Time_response`,
      cnf_rt = `Reaction Time_cnf`,
      bet = Response_bet,
      response = Response_response,
      cnf = Response_cnf,
      difficulty = metadata,
      diff_order = metadata_order
    ) %>%
    mutate(
      bet = if_else(
        bet == "正解:+2  不正解:-1", "risk",
        if_else(
          bet == "正解:+1 不正解:-0",
          "safe",
          if_else(
            bet == "標的音1をよく覚えている",
            "risk",
            if_else(
              bet == "覚えていない",
              "safe",
              bet
            )
          )
        )
      ),
      difficulty = str_sub(difficulty, start = 1, end = -2),
      trial = as.numeric(trial),
      cnf = as.numeric(cnf),
      response = if_else(
        response == "Same",
        1,
        if_else(
          response == "Ans_Miss",
          NA,
          0
        )
      ),
      stimulus = if_else(
        correctness == 1,
        response,
        response * -1 + 1
      )
    ) %>%
    mutate(
      bet_str = bet,
      bet_dummy = if_else(
        bet == "safe",
        1,
        if_else(
          bet == "Bet_Miss",
          NA,
          2
        )
      ),
      point = if_else(
        correctness == 0,
        if_else(
          bet == "safe",
          0,
          -1
        ),
        if_else(
          bet == "safe",
          1,
          2
        )
      ),
      bet = bet_dummy
    )
  return(data)
}

get_subject_info_exp2 <- function() {
  data_bet <- read_csv("data/exp2/data-bet.csv")
  data_bet["condition"] <- "bet"
  data_cnf <- read_csv("data/exp2/data-conf.csv")
  data_cnf["condition"] <- "cnf"

  data <- rbind(data_bet, data_cnf) %>%
    filter(display == "Agreement") %>%
    filter(
      `Zone Type` == "response_slider_endValue" |   # age
      `Zone Type` == "response_rating_scale_likert" # sex
    ) %>%
    select(`Participant Private ID`, `Zone Type`, Response, metadata_order) %>%
    mutate(`Zone Type` = if_else(
      `Zone Type` == "response_slider_endValue",
      "age",
      "sex"
    )) %>%
    pivot_wider(names_from = `Zone Type`, values_from = c(Response)) %>%
    rename(
      id = `Participant Private ID`,
      diff_order = metadata_order
    ) %>%
    mutate(
      sex = case_when(
        sex == "男性" ~ "male",
        sex == "女性" ~ "female",
        sex == "その他" ~ "other",
        sex == "無回答" ~ "no answer",
        .default = "no answer"
      ),
      age = as.numeric(age)
    )
}

preprocess_data <- function(
    data, data_vars,
    remove_fix_behavior = TRUE, 
    remove_half_accuracy = TRUE,
    remove_few_trial_participants = TRUE, 
    remove_fix_bet_correctness = TRUE, 
    remove_fix_cnf_correctness = TRUE
    ) {
  data %>%
    mutate(
      bet_rt = bet_rt / 1000,
      ans_rt = ans_rt / 1000,
      cnf_rt = cnf_rt / 1000
    ) %>%
    group_by(id, across(all_of(data_vars))) %>%
    filter(
      !is.na(bet),
      !is.na(response),
      !is.na(cnf),
      !is.na(bet_rt),
      !is.na(ans_rt),
      !is.na(cnf_rt),
      bet_rt >= 0.1,
      ans_rt >= 0.1,
      cnf_rt >= 0.1
    ) %>%
    ungroup() %>%
    exclude_outlier_ids(
      data_vars, 
      remove_fix_behavior, 
      remove_half_accuracy, 
      remove_few_trial_participants, 
      remove_fix_bet_correctness, 
      remove_fix_cnf_correctness
    )
}

# Exclude those with less than 10 trials within condition
exclude_outlier_ids <- function(
    data, data_vars, 
    remove_fix_behavior = TRUE, 
    remove_half_accuracy = TRUE,
    remove_few_trial_participants = TRUE, 
    remove_fix_bet_correctness = TRUE, 
    remove_fix_cnf_correctness = TRUE
  ) {
  
  remove_zero_trial_id <- data %>%
    group_by(id, across(all_of(data_vars))) %>%
    summarise(
      n_trial = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = !!sym(data_vars[2]), values_from = n_trial) %>%
    mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
    pivot_longer(cols = -c(id, !!sym(data_vars[1])), names_to = data_vars[2], values_to = "n_trial") %>%
    filter(n_trial == 0)
  
  data <- data %>%
    filter(
      !id %in% remove_zero_trial_id$id
    )
   
  remove_fix_bet_id <- data %>%
    count(id, across(all_of(data_vars)), bet) %>%
    pivot_wider(names_from = bet, values_from = n) %>% 
    mutate(count_na = rowSums(is.na(.))) %>%
    filter(count_na == 1)
  remove_fix_response_id <- data %>%
    count(id, across(all_of(data_vars)), response) %>%
    pivot_wider(names_from = response, values_from = n) %>% 
    mutate(count_na = rowSums(is.na(.))) %>%
    filter(count_na == 1)
  remove_fix_cnf_id <- data %>%
    count(id, across(all_of(data_vars)), cnf) %>%
    pivot_wider(names_from = cnf, values_from = n) %>% 
    mutate(count_na = rowSums(is.na(.))) %>%
    filter(count_na == 4)
  
  if(remove_fix_behavior) {
    data <- data %>%
      filter(
        !id %in% remove_fix_bet_id$id,
        !id %in% remove_fix_response_id$id,
        !id %in% remove_fix_cnf_id$id
      )
  }
  
  remove_few_trial_id <- data %>%
    group_by(id, across(all_of(data_vars))) %>%
    summarise(
      n_trial = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = !!sym(data_vars[2]), values_from = n_trial) %>%
    mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
    pivot_longer(cols = -c(id, !!sym(data_vars[1])), names_to = data_vars[2], values_to = "n_trial") %>%
    filter(n_trial < 18)
  
  if(remove_few_trial_participants) {
    data <- data %>%
      filter(
        !id %in% remove_few_trial_id$id
      )
  }
   
  remove_half_accuracy_id <- data %>%
    group_by(id, across(all_of(data_vars))) %>%
    summarise(
      accuracy = mean(correctness),
      .groups = "drop"
    ) %>%
    mutate(across(where(is.numeric), \(x) replace_na(x, 0))) %>%
    filter(accuracy == 0.5)
  
  if(remove_half_accuracy) {
    data <- data %>%
      filter(
        !id %in% remove_half_accuracy_id$id
      )
  }
  
  remove_fix_bet_correctness_id <- data %>%
    count(id, across(all_of(data_vars)), correctness, bet) %>%
    pivot_wider(names_from = bet, values_from = n) %>% 
    mutate(count_na = rowSums(is.na(.))) %>%
    filter(count_na == 1)
  
  if(remove_fix_bet_correctness) {
    data <- data %>%
      filter(
        !id %in% remove_fix_bet_correctness_id$id
      )
  }
  
  remove_fix_cnf_correctness_id <- data %>%
    count(id, across(all_of(data_vars)), correctness, cnf) %>%
    pivot_wider(names_from = cnf, values_from = n) %>% 
    mutate(count_na = rowSums(is.na(.))) %>%
    filter(count_na == 4)
  
  if(remove_fix_cnf_correctness) {
    data <- data %>%
      filter(
        !id %in% remove_fix_cnf_correctness_id$id
      )
  }
  
  return(data)
}

summarise_data <- function(data, data_vars) {
  data %>%
    group_nest(across(all_of(data_vars)), keep = TRUE) %>%
    mutate(
      accuracy    = map_dbl(data, ~ mean(.$correctness)),
      bet         = map_dbl(data, ~ mean(.$bet)),
      cnf         = map_dbl(data, ~ mean(.$cnf)),
      score       = map_dbl(data, ~ sum(.$point)),
      bet_rt      = map_dbl(data, ~ mean(.$bet_rt)),
      ans_rt      = map_dbl(data, ~ mean(.$ans_rt)),
      cnf_rt      = map_dbl(data, ~ mean(.$cnf_rt)),
      d1          = map_dbl(data, ~ calculate_d1(.)),
      c1          = map_dbl(data, ~ calculate_c1(.)),
      pre_auroc2  = map_dbl(data, ~ calculate_type2roc(., is_pre = T)),
      post_auroc2 = map_dbl(data, ~ calculate_type2roc(., is_pre = F)),
      pre_qsr     = map_dbl(data, ~ calculate_qsr(., is_pre = T)),
      post_qsr    = map_dbl(data, ~ calculate_qsr(., is_pre = F)),
    ) %>%
    select(-data)
}

calculate_d1 <- function(data) {
  cnf_count <- data %>%
    trials2nconf() %>%
    group_by(response) %>%
    mutate(
      ord = if_else(
        response == 0,
        cnf,
        abs(cnf - max(cnf) - 1)
      )
    ) %>%
    ungroup() %>%
    arrange(response, desc(ord))
  nR_S1 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0)
  nR_S2 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1)
  nR_S1 <- list(nR_S1)
  nR_S2 <- list(nR_S2)
  ret <- calculate_type1sdt(nR_S1, nR_S2)
  ret$d1
}

calculate_c1 <- function(data) {
  cnf_count <- data %>%
    trials2nconf() %>%
    group_by(response) %>%
    mutate(
      ord = if_else(
        response == 0,
        cnf,
        abs(cnf - max(cnf) - 1)
      )
    ) %>%
    ungroup() %>%
    arrange(response, desc(ord))
  nR_S1 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0)
  nR_S2 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1)
  nR_S1 <- list(nR_S1)
  nR_S2 <- list(nR_S2)
  ret <- calculate_type1sdt(nR_S1, nR_S2)
  ret$c1
}

calculate_type2roc <- function(data, is_pre = T) {
  # ref: https://sites.google.com/view/marcogandolfo/resources/R/auroc2r-metacognitive-sensitivity
  
  if (is_pre) {
    n_ratings <- 2
    conf <- data$bet
  } else {
    n_ratings <- 5
    conf <- data$cnf
  }
  correct <- data$correctness
  
  i <- n_ratings+1
  H2 <- c()
  FA2 <- c()
  for (c in 1:n_ratings){
    H2[i-1] <- length(which(conf == c & correct == 1)) + 0.5
    FA2[i-1] <- length(which(conf == c & correct == 0)) + 0.5
    i <- i-1
  }
  
  H2 <- H2 / sum(H2)
  FA2 <- FA2 / sum(FA2)
  
  cum_H2 <- c(0, cumsum(H2))
  cum_FA2 <- c(0, cumsum(FA2))
  
  k <- numeric(n_ratings)
  
  for (c in 1:n_ratings) {
    k[c] <- (cum_H2[c + 1] - cum_FA2[c])^2 - (cum_H2[c] - cum_FA2[c + 1])^2
  }
  
  auroc2 <- 0.5 + 0.25 * sum(k)
  
  return(auroc2)
}

calculate_qsr <- function(data, is_pre = T) {
  
  data |> 
    mutate(
      qsr = ifelse(
        is_pre,
        1-((bet-1)-correctness)^2,
        1-((cnf-1)/4-correctness)^2
      )
    ) |> 
    summarise(qsr = mean(qsr)) |> 
    pull(qsr)
}

eda <- function(data) {
  diagnose_web_report(data)
  eda_web_report(data)
  transformation_web_report(data)
}

plot_between_two_condition_for_paper <- function(data, y, x1, x2, groups) {

  pos <- position_dodge(width = 0.1)
  
  p <- data %>%
    ggplot() +
    
    geom_violinhalf(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[1]),
      alpha = 0, linewidth=2, flip = c(1,2), position = position_nudge(-0.1), show.legend = FALSE) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[1]),
      fun.y = mean,
      geom = "point",
      size = 2.5,
      position = pos) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[1]),
      fun.data = mean_cl_boot,
      fun.args = list(conf.int = 0.95),
      geom = 'errorbar',
      width = 0.075,
      lwd = 1,
      position = pos) +
    
    scale_color_manual(values = c(brewer.pal(3, "Blues")[2], brewer.pal(3, "Blues")[3])) +
    new_scale_color() +
    
    geom_violinhalf(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[2]),
      alpha = 0, linewidth=2, position = position_nudge(+0.1), show.legend = FALSE) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[2]),
      fun.y = mean,
      geom = "point",
      size = 2.5,
      position = pos) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      filter(data, !!rlang::sym(x1) == groups[2]),
      fun.data = mean_cl_boot,
      fun.args = list(conf.int = 0.95),
      geom = 'errorbar',
      width = 0.075,
      lwd = 1,
      position = pos) +
    
    scale_color_manual(values = c(brewer.pal(3, "Oranges")[2], brewer.pal(3, "Oranges")[3])) +
    
    theme_pubr(base_size = 24)
  
  return(p)
}

plot_within_two_condition_for_paper <- function(data, y, x1, x2, palette_color) {
  
  pos <- position_dodge(width = 0.1)
  
  p <- data %>%
    ggplot() +
    
    geom_violinhalf(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      alpha = 0, linewidth=2, flip = c(1,3), position = position_nudge_center(x=0.1, direction = "split"), show.legend = FALSE) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      fun.y = mean,
      geom = "point",
      size = 2.5,
      position = pos) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      fun.data = mean_cl_boot,
      fun.args = list(conf.int = 0.95),
      geom = 'errorbar',
      width = 0.075,
      lwd = 1,
      position = pos) +
    
    stat_summary(
      aes(
        x     = .data[[x1]], 
        y     = .data[[y]], 
        color = .data[[x2]]
      ),
      fun.y = mean,
      geom = 'line',
      lwd = 1,
      position = pos) +
    
    scale_color_manual(values = c(brewer.pal(3, palette_color)[2], brewer.pal(3, palette_color)[3])) +
    
    theme_pubr(base_size = 24)
  
  return(p)
}

plot_within_one_condition_for_paper <- function(data, y, x1) {
  
  pos <- position_dodge(width = 0.1)
  
  p <- data %>%
    ggplot() +
    
    geom_violinhalf(
      aes(
        x = .data[[x1]], 
        y = .data[[y]]
      ),
      alpha = 0, linewidth=2, flip = c(1,3), position = position_nudge_center(x=0.1, direction = "split"), show.legend = FALSE) +
    
    stat_summary(
      aes(
        x = .data[[x1]], 
        y = .data[[y]]
      ),
      fun.y = mean,
      geom = "point",
      size = 2.5,
      position = pos) +
    
    stat_summary(
      aes(
        x = .data[[x1]], 
        y = .data[[y]]
      ),
      fun.data = mean_cl_boot,
      fun.args = list(conf.int = 0.95),
      geom = 'errorbar',
      width = 0.075,
      lwd = 1,
      position = pos) +
    
    stat_summary(
      aes(
        x = .data[[x1]], 
        y = .data[[y]]
      ),
      fun.y = mean,
      geom = 'line',
      lwd = 1,
      position = pos) +
    
    theme_pubr(base_size = 24)
  
  return(p)
}

# ref:
# - https://gitlab.com/nfaivre/meta_scz_public/-/blob/master/behav/subscripts/metaperf.R
# - https://github.com/metacoglab/HMeta-d/blob/master/R/trials2counts.R

# for post-M-ratio
preprocess_metad_2way_mixed_anova <- function(data, data_vars) {
  # convert trial to count
  cnf_count <- data %>%
    trials2nconf(data_vars) %>%
    group_by(id, across(all_of(data_vars)), response) %>%
    mutate(
      ord = if_else(
        response == 0,
        cnf,
        abs(cnf - max(cnf) - 1)
      )
    ) %>%
    ungroup() %>%
    arrange(id, response, desc(ord))

  x1 <- data_vars[1]
  x2 <- data_vars[2]
  group_conds <- cnf_count %>%
    select(id, !!sym(x1)) %>%
    unique() %>%
    mutate(!!x1 := as.numeric(factor(!!sym(x1))) - 1) %>%
    select(!!sym(x1)) %>%
    as.vector() %>%
    unlist()
  names(group_conds) <- NULL

  ## for x1
  nR_S1_1 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0, x2, 1)
  nR_S2_1 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1, x2, 1)

  ## for x2
  nR_S1_2 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0, x2, 2)
  nR_S2_2 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1, x2, 2)

  nR_S1 <- list(nR_S1_1, nR_S1_2)
  nR_S2 <- list(nR_S2_1, nR_S2_2)
  # Type 1 parameters
  nratings <- nrow(nR_S1[[1]]) / 2
  nsubj <- ncol(nR_S1[[1]])

  # Adjust to ensure non-zero counts for type 1 d' point estimate
  d1 <- NULL
  c1 <- NULL
  counts <- NULL

  for (n in 1:2) {
    ret <- calculate_type1sdt(nR_S1, nR_S2, n)
    d1 <- abind(d1, ret$d1, along = 2)
    c1 <- abind(c1, ret$c1, along = 2)
    counts <- abind(counts, ret$counts, along = 3)
  }

  Tol <- 1e-05

  data <- list(
    d1 = d1,
    c1 = c1,
    nsubj = nsubj,
    counts = counts,
    nratings = nratings,
    cov = group_conds,
    Tol = Tol,
    Condition = c(1, 0)
  )

  return(data)
}

# for pre-M-ratio
preprocess_metad_bet_2way_mixed_anova <- function(data, data_vars) {
  # convert trial to count
  cnf_count <- data %>%
    trials2nbet(data_vars) %>%
    group_by(id, across(all_of(data_vars)), response) %>%
    mutate(
      ord = if_else(
        response == 0,
        bet,
        abs(bet - max(bet) - 1)
      )
    ) %>%
    ungroup() %>%
    arrange(id, response, desc(ord))

  x1 <- data_vars[1]
  x2 <- data_vars[2]
  group_conds <- cnf_count %>%
    select(id, !!sym(x1)) %>%
    unique() %>%
    mutate(!!x1 := as.numeric(factor(!!sym(x1))) - 1) %>%
    select(!!sym(x1)) %>%
    as.vector() %>%
    unlist()
  names(group_conds) <- NULL

  ## for x1
  nR_S1_1 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0, x2, 1)
  nR_S2_1 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1, x2, 1)

  ## for x2
  nR_S1_2 <- cnf_count %>% # stim0
    arrange_stim_cnf_resp(0, x2, 2)
  nR_S2_2 <- cnf_count %>% # stim1
    arrange_stim_cnf_resp(1, x2, 2)

  nR_S1 <- list(nR_S1_1, nR_S1_2)
  nR_S2 <- list(nR_S2_1, nR_S2_2)
  # Type 1 parameters
  nratings <- nrow(nR_S1[[1]]) / 2
  nsubj <- ncol(nR_S1[[1]])

  # Adjust to ensure non-zero counts for type 1 d' point estimate
  d1 <- NULL
  c1 <- NULL
  counts <- NULL

  for (n in 1:2) {
    ret <- calculate_type1sdt(nR_S1, nR_S2, n)
    d1 <- abind(d1, ret$d1, along = 2)
    c1 <- abind(c1, ret$c1, along = 2)
    counts <- abind(counts, ret$counts, along = 3)
  }

  Tol <- 1e-05

  data <- list(
    d1 = d1,
    c1 = c1,
    nsubj = nsubj,
    counts = counts,
    nratings = nratings,
    cov = group_conds,
    Tol = Tol,
    Condition = c(1, 0)
  )

  return(data)
}

trials2nconf <- function(data, data_vars = NULL) {
  nratings = 5
  if (is.null(data_vars)) {
    data <- data %>%
      group_by(id, stimulus, response, cnf) %>%
      summarise(ntrials = n(), .groups = "keep") %>%
      ungroup() %>%
      complete(id, stimulus=0:1, response=0:1, cnf=1:nratings) %>%
      pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
  } else {
    if (length(data_vars) == 1) {
      x1 <- data_vars[1]
      data <- data %>%
        group_by(id, !!sym(x1), stimulus, response, cnf) %>%
        summarise(ntrials = n(), .groups = "keep") %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x1)), stimulus=0:1, response=0:1, cnf=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    } else if (length(data_vars) == 2) {
      x1 <- data_vars[1]
      x2 <- data_vars[2]
      data <- data %>%
        group_by(id, !!sym(x1), !!sym(x2), stimulus, response, cnf) %>%
        summarise(ntrials = n(), .groups = "keep") %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x1)), stimulus=0:1, response=0:1, !!sym(x2), cnf=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    } else if (length(data_vars) == 3) {
      x1 <- data_vars[1]
      x2 <- data_vars[2]
      x3 <- data_vars[3]
      data <- data %>%
        group_by(id, !!sym(x1), !!sym(x2), !!sym(x3), stimulus, response, cnf) %>%
        summarise(ntrials = n(), .groups = "keep") %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x1), !!sym(x2)), stimulus=0:1, response=0:1, !!sym(x3), cnf=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    }
  }
  data %>%
    arrange(id, desc(cnf), response) %>%
    mutate(
      ## comment out for padding
      # stim0 = replace_na(stim0, 0) + 1 / (2 * max(cnf)),
      # stim1 = replace_na(stim1, 0) + 1 / (2 * max(cnf)),
      stim0 = replace_na(stim0, 0),
      stim1 = replace_na(stim1, 0),
    )
}

trials2nbet <- function(data, data_vars = NULL) {
  nratings = 2
  if (is.null(data_vars)) {
    data <- data %>%
      group_by(id, stimulus, response, bet) %>%
      summarise(ntrials = n()) %>%
      ungroup() %>%
      complete(id, stimulus=0:1, response=0:1, bet=1:nratings) %>%
      pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
  } else {
    if(length(data_vars) == 1) {
      x <- data_vars[1]
      data <- data %>%
        group_by(id, !!sym(x), stimulus, response, bet) %>%
        summarise(ntrials = n()) %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x)), stimulus=0:1, response=0:1, bet=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    } else if (length(data_vars) == 2) {
      x1 <- data_vars[1]
      x2 <- data_vars[2]
      data <- data %>%
        group_by(id, !!sym(x1), !!sym(x2), stimulus, response, bet) %>%
        summarise(ntrials = n()) %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x1)), stimulus=0:1, response=0:1, !!sym(x2), bet=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    } else if (length(data_vars) == 3) {
      x1 <- data_vars[1]
      x2 <- data_vars[2]
      x3 <- data_vars[3]
      data <- data %>%
        group_by(id, !!sym(x1), !!sym(x2), !!sym(x3), stimulus, response, bet) %>%
        summarise(ntrials = n()) %>%
        ungroup() %>%
        complete(nesting(id, !!sym(x1), !!sym(x2)), stimulus=0:1, response=0:1, !!sym(x3), bet=1:nratings) %>%
        pivot_wider(names_from = stimulus, values_from = ntrials, names_prefix = "stim")
    }
  }
  data %>%
    arrange(id, desc(bet), response) %>%
    mutate(
      ## comment out for padding
      # stim0 = replace_na(stim0, 0) + 1 / (2 * max(cnf)),
      # stim1 = replace_na(stim1, 0) + 1 / (2 * max(cnf)),
      stim0 = replace_na(stim0, 0),
      stim1 = replace_na(stim1, 0),
    )
}

# arrange confidence count data for metad manner
arrange_stim_cnf_resp <- function(cnf_count, stim_id, data_vars = NULL, data_var_ids = NULL) {
  if (!is.null(data_vars)) {
    if (length(data_vars) > 1) {
      unique_conditions <- cnf_count %>%
        select(all_of(data_vars)) %>%
        unique()
      x1 <- data_vars[1]
      x2 <- data_vars[2]
      ix1 <- data_var_ids[1]
      ix2 <- data_var_ids[2]
      cnf_count <- cnf_count %>%
        filter(
          !!sym(x1) == unique_conditions[[ix1, 1]],
          !!sym(x2) == unique_conditions[[ix2, 2]]
        )
    } else {
      x <- data_vars[1]
      unique_conditions <- cnf_count %>%
        select(!!sym(x)) %>%
        unique()
      ix <- data_var_ids[1]
      cnf_count <- cnf_count %>%
        filter(!!sym(x) == unique_conditions[[ix, 1]])
    }
  }
  cnf_count %>%
    select(id, !!sym(paste0("stim", stim_id))) %>%
    group_by(id) %>%
    mutate(tmp = 1:n()) %>%
    pivot_wider(names_from = id, values_from = !!sym(paste0("stim", stim_id))) %>%
    mutate_all(~ replace(., is.null(.), 0)) %>%
    mutate_all(~ replace(., is.na(.), 0)) %>%
    select(-tmp) %>%
    ungroup() %>%
    as.data.frame()
}

calculate_type1sdt <- function(nR_S1, nR_S2, data_var_id = 1) {
  # Type 1 parameters
  nratings <- nrow(nR_S1[[1]]) / 2
  nsubj <- ncol(nR_S1[[1]])
  nTask <- length(nR_S1)

  # Adjust to ensure non-zero counts for type 1 d' point estimate
  d1 <- NULL
  c1 <- NULL

  for (n in 1:(nsubj)) {
    adj_f <- 1 / ((nratings) * 2)
    nR_S1_adj <- nR_S1[[data_var_id]][, n] + adj_f
    nR_S2_adj <- nR_S2[[data_var_id]][, n] + adj_f

    ratingHR <- matrix()
    ratingFAR <- matrix()

    for (c in 2:(nratings * 2)) {
      ratingHR[c - 1] <- sum(nR_S2_adj[c:length(nR_S2_adj)]) / sum(nR_S2_adj)
      ratingFAR[c - 1] <- sum(nR_S1_adj[c:length(nR_S1_adj)]) / sum(nR_S1_adj)
    }
    d1 <- d1 |>
      abind(qnorm(ratingHR[(nratings)]) - qnorm(ratingFAR[(nratings)]), along = 1)
    c1 <- c1 |>
      abind(-0.5 * (qnorm(ratingHR[(nratings)]) + qnorm(ratingFAR[(nratings)])), along = 1)
  }
  counts <- t(nR_S1[[data_var_id]]) %>%
    abind(t(nR_S2[[data_var_id]]), along = 2)
  d1[abs(d1) < 0.0001] <- 0.0001

  return(list(d1 = d1, c1 = c1, counts = counts))
}

create_inits <- function(dataframe) {
  function() {
    list(
      dbase = rnorm(dataframe$nsubj, mean = 0, sd = 1),
      Bd_Condition = rnorm(dataframe$nsubj, mean = 0, sd = 1),
      Bd_interaction = rnorm(dataframe$nsubj, mean = 0, sd = 1),
      tau = rgamma(dataframe$nsubj, shape = 1, rate = 1),
      beta1 = rnorm(dataframe$nsubj, mean = 0, sd = 1),
      logMratio = array(rnorm(dataframe$nsubj * 2, mean = 0, sd = 1), dim = c(dataframe$nsubj, 2)),
      mu_c2 = rnorm(1, mean = 0, sd = 1),
      sigma_c2 = abs(rnorm(1, mean = 1, sd = 0.5)),
      muD = rnorm(1, mean = 0, sd = 1),
      sigma_D = abs(rnorm(1, mean = 1, sd = 0.5)),
      mu_beta1 = rnorm(1, mean = 0, sd = 1),
      sigma_beta1 = abs(rnorm(1, mean = 1, sd = 0.5)),
      muBd_Condition = rnorm(1, mean = 0, sd = 1),
      sigma_Condition = abs(rnorm(1, mean = 1, sd = 0.5)),
      muBd_interaction = rnorm(1, mean = 0, sd = 1),
      sigma_interaction = abs(rnorm(1, mean = 1, sd = 0.5))
    )
  }
}

check_mcmc <- function(data.summary, data.samples, parameter_list) {
  p1 <- mcmc_rhat(
    data.summary %>%
      filter(variable %in% parameter_list) %>%
      pull(rhat)
  )
  print(p1)
  p2 <- mcmc_rank_hist(
    data.samples,
    pars = parameter_list
  )
  print(p2)
  p3 <- mcmc_acf_bar(
    data.samples,
    pars = parameter_list
  )
  print(p3)
  p4 <- mcmc_neff(
    data.summary %>%
      filter(variable %in% parameter_list) %>%
      pull(ess_bulk)
  )
  print(p4)
  p5 <- mcmc_areas(
    data.samples,
    pars = parameter_list[startsWith(parameter_list, "mu")],
    prob = 0.95
  )
  print(p5)
}

calculate_pd <- function(df, var, is_positive = T) {
  if (is_positive) sum(df[[var]]>0)/length(df[[var]])
  else sum(df[[var]]<0)/length(df[[var]])
}
