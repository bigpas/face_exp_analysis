calculate_sensitivity_score <- function(scores_df, number_of_frames, target){
    target_score <- scores_df[[target]]
    (target_score/number_of_frames) * 100
}

sensitivity_scores <- function(df, target_labels_for_dataset){
    #subsetting
    df_subset <- df[, grepl(
        x = names(df),
        pattern = paste0(target_labels_for_dataset,
                         collapse = '|')
    )]

    #scores
    scores <- lapply(df, function(x) x > 0) %>%
        lapply(function(x) sum(x))

    res <- purrr::map_df(target_labels_for_dataset, function(emotion_label)
        calculate_sensitivity_score(scores_df = scores,
                                    number_of_frames = nrow(df_subset),
                                    target = emotion_label)
        )

    round(res, 2)
}

sensitivity_scores(AFDES_data[[1]], targets_for_AFDES)
