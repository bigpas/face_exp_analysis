calculate_confidence_score <- function(scores_df,
                                       scores_sum,
                                       target){
    target_score <- scores_df[[target]]
    (target_score/scores_sum)*100
}

confidence_score <- function(df, target_labels_for_dataset) {
    #subsetting
    df_subset <- df[, grepl(
        x = names(df),
        pattern = paste0(target_labels_for_dataset,
                         collapse = '|')
    )]

    #calculate the sum of ratings > 0
    scores <- lapply(df_subset, function(x)
        ifelse(x < 0, 0, x)) %>%
        lapply(function(x)
            sum(x))

    sum_of_scores <- Reduce(`+`, scores)

    res <- purrr::map_df(target_labels_for_dataset, function(emotion_label)
        calculate_confidence_score(scores_df = scores,
                                   scores_sum = sum_of_scores,
                                   target = emotion_label)
        )

    round(res, 2)

}
