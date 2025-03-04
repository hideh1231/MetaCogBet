list(
  tar_target(data_exp2, get_raw_data_exp2(), deployment = "main"),
  tar_target(subj_info_exp2, get_subject_info_exp2()),
  tar_target(preprocess_exp2_revise, 
             preprocess_data(
               data_exp2, c("condition","difficulty"),
               remove_fix_behavior = TRUE, 
               remove_few_trial_participants = TRUE, 
               remove_half_accuracy = FALSE,
               remove_fix_bet_correctness = FALSE, 
               remove_fix_cnf_correctness = FALSE
               ))
)
