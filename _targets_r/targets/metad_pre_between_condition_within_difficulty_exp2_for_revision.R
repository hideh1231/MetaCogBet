list(
  tar_jags(
    hmetad_pre_between_condition_within_difficulty_exp2_revise,
    jags_files = "jags/Bayes_metad_2way_mixed_ANOVA.txt",
    parameters.to.save = c("muBd_Condition", "lamBd_Condition", "sigD_Condition", "muBd_Condition_prior",
                           "muBd_interaction", "lamBd_interaction", "sigD_interaction", "muBd_interaction_prior",
                           "mu_beta1", "mu_beta1_prior",
                           "Mratio","logMratio","muD","mu_c2","mu_regression",
                           "Bd_Condition","Bd_interaction", "beta1","mu","d1","c1"),
    data = preprocess_metad_pre_between_condition_within_difficulty_exp2_revise,
    n.cluster = 3,
    n.chains = 3,
    n.iter = 10000,
    n.burnin = 1000,
    n.thin = 1,
    refresh = 200,
    error = "continue", 
    progress.bar = "gui"
  )
)
