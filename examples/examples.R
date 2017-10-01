source('R/libs.R')
source('R/confidence_score.R')
source('R/sensitivity_score.R')

# Emotional labels of interest --------------------------------------------
target_labels <-
    c("Joy", "Anger", "Surprise", "Fear", "Disgust", "Sadness")

targets_for_AFDES <- sapply(target_labels, function(x)
    paste0(x, " Evidence"))

# Input files and Data preparation -----------------------------------------------------
AFDES_raw_files <-
    dir('raw-data/', pattern = '^Dump', full.names = TRUE)
AFDES_raw_files

# problem with readr_tsv(); fread works great instead!
AFDES_data <- lapply(AFDES_raw_files, function(x)
    data.table::fread(x, data.table = FALSE))

names(AFDES_data) <- gsub("raw-data//|\\.txt$", "", AFDES_raw_files) %>%
                        gsub('Dump\\d+_', '', .) %>%
                            gsub('\\s+|-', "_", .)


# examples confidence scores -----------------------------------------------
#single data frame
confidence_score(df = AFDES_data[["North_Europ_F01sadnessface_Forward_7"]],
                 target_labels_for_dataset = targets_for_AFDES)

#list of data frames
lapply(AFDES_data, function(dataset)
    confidence_score(df = dataset,
                     target_labels_for_dataset = targets_for_AFDES)) %>%
    plyr::ldply(.id = "file_Id")

# examples sensitivity scores ---------------------------------------------
sensitivity_scores(df = AFDES_data[["North_Europ_F01sadnessface_Forward_7"]],
                   target_labels_for_dataset = targets_for_AFDES)

lapply(AFDES_data, function(dataset)
    sensitivity_scores(df = dataset,
                       target_labels_for_dataset = targets_for_AFDES)) %>%
    plyr::ldply(.id = "file_Id")
