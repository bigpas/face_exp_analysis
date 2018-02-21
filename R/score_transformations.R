to_probabilities <- function(evidence_score, ...){
    1/(1 + (10 ^ -evidence_score))
}

to_odds <- function(probability_score, ...){
    1/((1/probability_score)-1)
}
