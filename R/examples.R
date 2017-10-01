source('R/libs.R')
source('R/confidence_score.R')

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

# Emotional labels of interest --------------------------------------------
target_labels <-
    c("Joy", "Anger", "Surprise", "Fear", "Disgust", "Sadness")

targets_for_AFDES <- sapply(target_labels, function(x)
    paste0(x, " Evidence"))

# examples confidence scores -----------------------------------------------
#single data frame
confidence_score(df = AFDES_data[[1]],
                 target_labels_for_dataset = targets_for_AFDES)

#list of data frames
lapply(AFDES_data, function(x)
    confidence_score(df = x,
                     target_labels_for_dataset = targets_for_AFDES)) %>%
    plyr::ldply(.id = "file_Id")

# examples sensitivity scores ---------------------------------------------
