list(
  tar_target(
    dataframe_condition_difficulty_revise,
    summarise_data(preprocess_exp2_revise |> mutate(bet=bet-1), c("id", "difficulty", "condition"))
  ),
  tar_target(
    dataframe_bet_revise,
    summarise_data(preprocess_exp2_revise |> mutate(bet=bet-1), c("id", "bet"))
  )
)
