# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

source('R/libs.R')

hello <- function() {
  print("Hello, world!")
}

preProcPoES <- function(data, vector){

    #pick the target name
    targetName <- vector %>%
        strsplit(split = "_") %>%
        unlist %>% '['(2)

    #normalize names
    #rectify
    targetName <- ifelse(targetName == "happiness" |
                             targetName == "amusement",
                         "joy", targetName)

    #sum of scores above 0
    scores <- lapply(df, function(x) ifelse(x < 0, 0, x)) %>%
        lapply(function(x) sum(x))

    #tagert score
    targetScores <- scores[[paste(targetName)]]
    #sum of scores
    sumOfScores <- Reduce(`+`, scores)

    confidenceScore <- (targetScores/sumOfScores)*100
    # list(target = targetName, scores = scores, tScore = targetScores, sum = sumOfScores,
    #      cs = confidenceScore)
    confidenceScore

}

confidenceScores <- Map(function(x, y) preProcPoES(x,y),
                        x = names(discreteDfs),
                        y = discreteDfs2) %>%
    data.table::data.table(entry = names(.), confScore = .)
confidenceScores[, c("DB", "target", "gender", "occurrence") := tstrsplit(entry, "_")]

confidenceScores[, confScore := .(round(unlist(confScore), 2))]
confidenceScores[, target := ifelse(target == "happiness" |
                                        target == "amusement",
                                    "joy", target)]

preProcSens <- function(data, vector){
    #(sum of frames > 0/total numeber of frames) * 100

    #pick the target name
    targetName <- vector %>%
        strsplit(split = "_") %>%
        unlist %>% '['(2)

    #normalize names
    #rectify
    targetName <- ifelse(targetName == "happiness" |
                             targetName == "amusement",
                         "joy", targetName)

    #sum of scores above 0
    scores <- lapply(df, function(x) x > 0) %>%
        lapply(function(x) sum(x))

    #tagert score
    targetScores <- scores[[paste(targetName)]]

    #number of total frames
    nFrames <- length(df[[paste(targetName)]])

    senScore <- (targetScores/nFrames)*100

    # list(scores, targetScores, nFrames, senScore)
    senScore
}

# master table ------------------------------------------------------------
sensitivityScores <- Map(function(x, y) preProcSens(x,y),
                         x = names(discreteDfs),
                         y = discreteDfs) %>%
    data.table(entry = names(.), senScore = .)
sensitivityScores[, c("DB", "target", "gender", "occurrence") := tstrsplit(entry, "_")]

sensitivityScores[, senScore := .(round(unlist(senScore), 2))]
sensitivityScores[, target := ifelse(target == "happiness" |
                                         target == "amusement",
                                     "joy", target)]

source("R/libs.R")
load('raw-data/checkpointMachine05.rda')
discreteDfs2 <- lapply(discreteDfs, function(x) x[, contempt:=NULL])



# subset AFDES only for the library ------------------------------------------------------------------
# all the way from the raw score
AFDES <- discreteDfs[grep(pattern = "^AFDES", x = names(discreteDfs), value = TRUE)]


AFDES_raw_files <- dir('raw-data/', pattern = '^Dump', full.names = TRUE)
AFDES_raw_files
# AFDES_raw_data <- map(AFDES_raw_files, function(x) readr::read_tsv(x))
# problems! fread works great instead!
data <- lapply(AFDES_raw_files, data.table::fread)
names(data) <- gsub("raw-data//|\\.txt$", "", AFDES_raw_files)
relevantCols <- function(df){
    df %>%
        dplyr::select(StudyName, Timestamp, ends_with("Evidence"))

}
relevantDfs <- lapply(data, relevantCols)
exampleOfDiscreteEvidenceAll <- names(relevantDfs$`Dump001_Medit F06joyface Forward-1`)[3:14]
