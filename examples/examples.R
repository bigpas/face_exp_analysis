source('R/libs.R')
source('R/confidence_score.R')
source('R/sensitivity_score.R')

# Emotional labels of interest --------------------------------------------
target_labels <-
    c("Joy", "Anger", "Surprise", "Fear", "Disgust", "Sadness")

targets_for_ADFES <- sapply(target_labels, function(x)
    paste0(x, " Evidence"))

# Input files and Data preparation -----------------------------------------------------
ADFES_raw_files <-
    dir('raw-data/', pattern = '^Dump', full.names = TRUE)
ADFES_raw_files

# problem with readr_tsv(); fread works great instead!
ADFES_data <- lapply(ADFES_raw_files, function(x){
    y <- data.table::fread(x, data.table = FALSE)
    gPattern <- paste0(targets_for_ADFES, collapse = '|')
    y[ , grepl(pattern = gPattern, x = names(y))][is.na(y[ , grepl(pattern = gPattern, x = names(y))])] <- -99
    y
    }) %>%
    set_names(nm = gsub("raw-data//|\\.txt$", "", ADFES_raw_files) %>%
                    gsub('Dump\\d+_', '', .) %>%
                        gsub('\\s+|-', "_", .)
              )


# examples confidence scores -----------------------------------------------
#single data frame
confidence_score(df = ADFES_data[["North_Europ_F01sadnessface_Forward_7"]],
                 target_labels_for_dataset = targets_for_ADFES)

#list of data frames
lapply(ADFES_data, function(dataset)
    confidence_score(df = dataset,
                     target_labels_for_dataset = targets_for_ADFES)) %>%
    plyr::ldply(.id = "file_Id")

# examples sensitivity scores ---------------------------------------------
sensitivity_scores(df = ADFES_data[["North_Europ_F01sadnessface_Forward_7"]],
                   target_labels_for_dataset = targets_for_ADFES)

lapply(ADFES_data, function(dataset)
    sensitivity_scores(df = dataset,
                       target_labels_for_dataset = targets_for_ADFES)) %>%
    plyr::ldply(.id = "file_Id")



# adding features ---------------------------------------------------------
## Confidence Scores with Odds
# single file
confidence_score_odds(df = ADFES_data[["North_Europ_F01sadnessface_Forward_7"]],
                 target_labels_for_dataset = targets_for_ADFES,
                 score_type = 'odds')


##list of data frames
##lapply(ADFES_data, function(dataset)
lapply(ADFES_data, function(dataset)
    confidence_score_odds(df = dataset,
                     target_labels_for_dataset = targets_for_ADFES,
                     score_type = 'odds')
    ) %>%
    plyr::ldply(.id = "file_Id")

#a variation: confidence calculated on probabilty scores
confidence_score_odds(df = ADFES_data[["North_Europ_F01sadnessface_Forward_7"]],
                      target_labels_for_dataset = targets_for_ADFES,
                      score_type = 'probs')
