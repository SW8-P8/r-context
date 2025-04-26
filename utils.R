library(dplyr)
library(tidyr)
library(rlang)


filter_responses <- function(df) {
  first_check_passed <- !is.na(df$sensitivity2.sensAttentionCheck.) & df$sensitivity2.sensAttentionCheck. == 4
  
  second_check_passed <- (
    (df$GROUP == 0 & !is.na(df$G1drawingClic2.G1AttentionCheck.) & df$G1drawingClic2.G1AttentionCheck. == 1) |
      (df$GROUP == 1 & !is.na(df$G2baselineClic2.G2AttentionCheck.) & df$G2baselineClic2.G2AttentionCheck. == 5) |
      (df$GROUP == 2 & !is.na(df$G3descClic2.G3AttentionCheck.) & df$G3descClic2.G3AttentionCheck. == 2) |
      (df$GROUP == 3 & !is.na(df$G4warnClic2.G4AttentionCheck.) & df$G4warnClic2.G4AttentionCheck. == 4)
  )
  
  completed <- df$lastpage == 121
  
  list(
    valid = df[first_check_passed & second_check_passed & completed,],
    almost_valid = df[first_check_passed & second_check_passed & !completed,],
    semi_valid = df[first_check_passed != second_check_passed,],
    invalid = df[!(first_check_passed | second_check_passed),]
  )
}



merge_columns <- function(df, cols_to_merge, new_col_name = "merged_col") {
  df %>%
    dplyr::mutate(!!new_col_name := dplyr::coalesce(!!!rlang::syms(cols_to_merge))) %>%
    dplyr::select(-dplyr::all_of(cols_to_merge))
}

average_columns <- function(df, cols_to_average, new_col_name = "average_col") {
  df %>%
    dplyr::mutate(!!new_col_name := rowMeans(dplyr::across(dplyr::all_of(cols_to_average)), na.rm = TRUE)) %>%
    dplyr::select(-dplyr::all_of(cols_to_average))
}

sum_columns <- function(df, cols_to_sum, new_col_name = "sum_col") {
  df %>%
    dplyr::mutate(!!new_col_name := rowSums(dplyr::across(dplyr::all_of(cols_to_sum)), na.rm = TRUE)) %>%
    dplyr::select(-dplyr::all_of(cols_to_sum))
}

remove_columns <- function(df, cols_to_remove) {
  df[cols_to_remove] <- NULL
  return(df)
}

clean_responses <- function(df) {
  df <- clean_useless_cols(df)
  df <- clean_sensitivity_cols(df)
  df <- clean_prototype_cols(df)
  
  return(df)
}

clean_useless_cols <- function(df) {
  df <- remove_columns(df, c("startlanguage", "seed", "datestamp", "instagramUser","scsintro", "interactiveIntro", "notInTargetGroup"))
  return(df)
}

clean_sensitivity_cols <- function(df) {
  df <- sum_columns(df, c("sensitivity1.sens1.", "sensitivity1.sens2.", "sensitivity1.sens3.", "sensitivity2.sens4.", "sensitivity2.sens5.", "sensitivity2.sens6.", "sensitivity2.sens7.", "sensitivity3.sens8.", "sensitivity3.sens9.", "sensitivity3.sens10."), "sens")
  df <- sum_columns(df, c("sensitivity1.arou1.", "sensitivity1.arou2.", "sensitivity1.arou3.", "sensitivity3.arou4.", "sensitivity3.arou5.", "sensitivity3.arou6.", "sensitivity3.arou7."), "arou")
  df <- sum_columns(df, c("sensitivity1.pers1.", "sensitivity2.pers2.", "sensitivity2.pers3.", "sensitivity2.pers4."), "pers")
  df$ers <- df$sens + df$arou + df$pers
  return(df)
}

clean_prototype_cols <- function(df) {
  # baseline
  df <- remove_columns(df, c("G1baselineIntro", "G2baselineIntro", "G3baselineIntro", "G4baselineIntro"))
  df <- remove_columns(df, c("G1baseline1", "G2baseline3", "G3baseline4", "G4baseline2"))
  df <- remove_columns(df, c("G2baselineClic2.G2AttentionCheck."))
  df <- merge_columns(df, c("G1baselineClic1.clic1.", "G2baselineClic1.clic1.", "G3baselineClic1.clic1.", "G4baselineClic1.clic1."), "baselineClic1")
  df <- merge_columns(df, c("G1baselineClic1.clic2.", "G2baselineClic1.clic2.", "G3baselineClic1.clic2.", "G4baselineClic1.clic2."), "baselineClic2")
  df <- merge_columns(df, c("G1baselineClic1.clic3.", "G2baselineClic1.clic3.", "G3baselineClic1.clic3.", "G4baselineClic1.clic3."), "baselineClic3")
  df <- merge_columns(df, c("G1baselineClic1.clic4.", "G2baselineClic1.clic4.", "G3baselineClic1.clic4.", "G4baselineClic1.clic4."), "baselineClic4")
  df <- merge_columns(df, c("G1baselineClic1.clic5.", "G2baselineClic1.clic5.", "G3baselineClic1.clic5.", "G4baselineClic1.clic5."), "baselineClic5")
  df <- merge_columns(df, c("G1baselineClic1.clic6.", "G2baselineClic1.clic6.", "G3baselineClic1.clic6.", "G4baselineClic1.clic6."), "baselineClic6")
  df <- merge_columns(df, c("G1baselineClic2.clic7.", "G2baselineClic2.clic7.", "G3baselineClic2.clic7.", "G4baselineClic2.clic7."), "baselineClic7")
  df <- merge_columns(df, c("G1baselineClic2.clic8.", "G2baselineClic2.clic8.", "G3baselineClic2.clic8.", "G4baselineClic2.clic8."), "baselineClic8")
  df <- merge_columns(df, c("G1baselineClic2.clic9.", "G2baselineClic2.clic9.", "G3baselineClic2.clic9.", "G4baselineClic2.clic9."), "baselineClic9")
  df <- merge_columns(df, c("G1baselineClic2.clic10.", "G2baselineClic2.clic10.", "G3baselineClic2.clic10.", "G4baselineClic2.clic10."), "baselineClic10")
  df <- merge_columns(df, c("G1baselineClic2.clic11.", "G2baselineClic2.clic11.", "G3baselineClic2.clic11.", "G4baselineClic2.clic11."), "baselineClic11")
  df <- merge_columns(df, c("G1baselineClic2.clic12.", "G2baselineClic2.clic12.", "G3baselineClic2.clic12.", "G4baselineClic2.clic12."), "baselineClic12")
  df <- merge_columns(df, c("G1baselineThoughts", "G2baselineThoughts", "G3baselineThoughts", "G4baselineThoughts"), "baselineThoughts")
  df <- merge_columns(df, c("G1baselineImprove", "G2baselineImprove", "G3baselineImprove", "G4baselineImprove"), "baselineImprove")
  
  # desc
  df <- remove_columns(df, c("G1descIntro", "G2descIntro", "G3descIntro", "G4descIntro"))
  df <- remove_columns(df, c("G1desc2", "G2desc1", "G3desc3", "G4desc4"))
  df <- remove_columns(df, c("G3descClic2.G3AttentionCheck."))
  df <- merge_columns(df, c("G1descClic1.clic1.", "G2descClic1.clic1.", "G3descClic1.clic1.", "G4descClic1.clic1."), "descClic1")
  df <- merge_columns(df, c("G1descClic1.clic2.", "G2descClic1.clic2.", "G3descClic1.clic2.", "G4descClic1.clic2."), "descClic2")
  df <- merge_columns(df, c("G1descClic1.clic3.", "G2descClic1.clic3.", "G3descClic1.clic3.", "G4descClic1.clic3."), "descClic3")
  df <- merge_columns(df, c("G1descClic1.clic4.", "G2descClic1.clic4.", "G3descClic1.clic4.", "G4descClic1.clic4."), "descClic4")
  df <- merge_columns(df, c("G1descClic1.clic5.", "G2descClic1.clic5.", "G3descClic1.clic5.", "G4descClic1.clic5."), "descClic5")
  df <- merge_columns(df, c("G1descClic1.clic6.", "G2descClic1.clic6.", "G3descClic1.clic6.", "G4descClic1.clic6."), "descClic6")
  df <- merge_columns(df, c("G1descClic2.clic7.", "G2descClic2.clic7.", "G3descClic2.clic7.", "G4descClic2.clic7."), "descClic7")
  df <- merge_columns(df, c("G1descClic2.clic8.", "G2descClic2.clic8.", "G3descClic2.clic8.", "G4descClic2.clic8."), "descClic8")
  df <- merge_columns(df, c("G1descClic2.clic9.", "G2descClic2.clic9.", "G3descClic2.clic9.", "G4descClic2.clic9."), "descClic9")
  df <- merge_columns(df, c("G1descClic2.clic10.", "G2descClic2.clic10.", "G3descClic2.clic10.", "G4descClic2.clic10."), "descClic10")
  df <- merge_columns(df, c("G1descClic2.clic11.", "G2descClic2.clic11.", "G3descClic2.clic11.", "G4descClic2.clic11."), "descClic11")
  df <- merge_columns(df, c("G1descClic2.clic12.", "G2descClic2.clic12.", "G3descClic2.clic12.", "G4descClic2.clic12."), "descClic12")
  df <- merge_columns(df, c("G1descThoughts", "G2descThoughts", "G3descThoughts", "G4descThoughts"), "descThoughts")
  df <- merge_columns(df, c("G1descImprove", "G2descImprove", "G3descImprove", "G4descImprove"), "descImprove")
  
  # warn
  df <- remove_columns(df, c("G1warnIntro", "G2warnIntro", "G3warnIntro", "G4warnIntro"))
  df <- remove_columns(df, c("G1warn4", "G2warn2", "G3warn1", "G4warn3"))
  df <- remove_columns(df, c("G4warnClic2.G4AttentionCheck."))
  df <- merge_columns(df, c("G1warnClic1.clic1.", "G2warnClic1.clic1.", "G3warnClic1.clic1.", "G4warnClic1.clic1."), "warnClic1")
  df <- merge_columns(df, c("G1warnClic1.clic2.", "G2warnClic1.clic2.", "G3warnClic1.clic2.", "G4warnClic1.clic2."), "warnClic2")
  df <- merge_columns(df, c("G1warnClic1.clic3.", "G2warnClic1.clic3.", "G3warnClic1.clic3.", "G4warnClic1.clic3."), "warnClic3")
  df <- merge_columns(df, c("G1warnClic1.clic4.", "G2warnClic1.clic4.", "G3warnClic1.clic4.", "G4warnClic1.clic4."), "warnClic4")
  df <- merge_columns(df, c("G1warnClic1.clic5.", "G2warnClic1.clic5.", "G3warnClic1.clic5.", "G4warnClic1.clic5."), "warnClic5")
  df <- merge_columns(df, c("G1warnClic1.clic6.", "G2warnClic1.clic6.", "G3warnClic1.clic6.", "G4warnClic1.clic6."), "warnClic6")
  df <- merge_columns(df, c("G1warnClic2.clic7.", "G2warnClic2.clic7.", "G3warnClic2.clic7.", "G4warnClic2.clic7."), "warnClic7")
  df <- merge_columns(df, c("G1warnClic2.clic8.", "G2warnClic2.clic8.", "G3warnClic2.clic8.", "G4warnClic2.clic8."), "warnClic8")
  df <- merge_columns(df, c("G1warnClic2.clic9.", "G2warnClic2.clic9.", "G3warnClic2.clic9.", "G4warnClic2.clic9."), "warnClic9")
  df <- merge_columns(df, c("G1warnClic2.clic10.", "G2warnClic2.clic10.", "G3warnClic2.clic10.", "G4warnClic2.clic10."), "warnClic10")
  df <- merge_columns(df, c("G1warnClic2.clic11.", "G2warnClic2.clic11.", "G3warnClic2.clic11.", "G4warnClic2.clic11."), "warnClic11")
  df <- merge_columns(df, c("G1warnClic2.clic12.", "G2warnClic2.clic12.", "G3warnClic2.clic12.", "G4warnClic2.clic12."), "warnClic12")
  df <- merge_columns(df, c("G1warnThoughts", "G2warnThoughts", "G3warnThoughts", "G4warnThoughts"), "warnThoughts")
  df <- merge_columns(df, c("G1warnImprove", "G2warnImprove", "G3warnImprove", "G4warnImprove"), "warnImprove")
  
  # drawing
  df <- remove_columns(df, c("G1drawingIntro", "G2drawingIntro", "G3drawingIntro", "G4drawingIntro"))
  df <- remove_columns(df, c("G1drawing3", "G2drawing4", "G3drawing2", "G4drawing1"))
  df <- remove_columns(df, c("G1drawingClic2.G1AttentionCheck."))
  df <- merge_columns(df, c("G1drawingClic1.clic1.", "G2drawingClic1.clic1.", "G3drawingClic1.clic1.", "G4drawingClic1.clic1."), "drawingClic1")
  df <- merge_columns(df, c("G1drawingClic1.clic2.", "G2drawingClic1.clic2.", "G3drawingClic1.clic2.", "G4drawingClic1.clic2."), "drawingClic2")
  df <- merge_columns(df, c("G1drawingClic1.clic3.", "G2drawingClic1.clic3.", "G3drawingClic1.clic3.", "G4drawingClic1.clic3."), "drawingClic3")
  df <- merge_columns(df, c("G1drawingClic1.clic4.", "G2drawingClic1.clic4.", "G3drawingClic1.clic4.", "G4drawingClic1.clic4."), "drawingClic4")
  df <- merge_columns(df, c("G1drawingClic1.clic5.", "G2drawingClic1.clic5.", "G3drawingClic1.clic5.", "G4drawingClic1.clic5."), "drawingClic5")
  df <- merge_columns(df, c("G1drawingClic1.clic6.", "G2drawingClic1.clic6.", "G3drawingClic1.clic6.", "G4drawingClic1.clic6."), "drawingClic6")
  df <- merge_columns(df, c("G1drawingClic2.clic7.", "G2drawingClic2.clic7.", "G3drawingClic2.clic7.", "G4drawingClic2.clic7."), "drawingClic7")
  df <- merge_columns(df, c("G1drawingClic2.clic8.", "G2drawingClic2.clic8.", "G3drawingClic2.clic8.", "G4drawingClic2.clic8."), "drawingClic8")
  df <- merge_columns(df, c("G1drawingClic2.clic9.", "G2drawingClic2.clic9.", "G3drawingClic2.clic9.", "G4drawingClic2.clic9."), "drawingClic9")
  df <- merge_columns(df, c("G1drawingClic2.clic10.", "G2drawingClic2.clic10.", "G3drawingClic2.clic10.", "G4drawingClic2.clic10."), "drawingClic10")
  df <- merge_columns(df, c("G1drawingClic2.clic11.", "G2drawingClic2.clic11.", "G3drawingClic2.clic11.", "G4drawingClic2.clic11."), "drawingClic11")
  df <- merge_columns(df, c("G1drawingClic2.clic12.", "G2drawingClic2.clic12.", "G3drawingClic2.clic12.", "G4drawingClic2.clic12."), "drawingClic12")
  df <- merge_columns(df, c("G1drawingThoughts", "G2drawingThoughts", "G3drawingThoughts", "G4drawingThoughts"), "drawingThoughts")
  df <- merge_columns(df, c("G1drawingImprove", "G2drawingImprove", "G3drawingImprove", "G4drawingImprove"), "drawingImprove")
  
  
  # compute scores
  
  # baseline
  df <- average_columns(df, c("baselineClic1", "baselineClic2", "baselineClic3"), "baselineClar")
  df <- average_columns(df, c("baselineClic4", "baselineClic5", "baselineClic6"), "baselineLike")
  df <- average_columns(df, c("baselineClic7", "baselineClic8", "baselineClic9"), "baselineInfo")
  df <- average_columns(df, c("baselineClic10", "baselineClic11", "baselineClic12"), "baselineCred")
  df$baselineClic <- (df$baselineClar + df$baselineLike + df$baselineInfo + df$baselineCred) / 4
  
  # desc
  df <- average_columns(df, c("descClic1", "descClic2", "descClic3"), "descClar")
  df <- average_columns(df, c("descClic4", "descClic5", "descClic6"), "descLike")
  df <- average_columns(df, c("descClic7", "descClic8", "descClic9"), "descInfo")
  df <- average_columns(df, c("descClic10", "descClic11", "descClic12"), "descCred")
  df$descClic <- (df$descClar + df$descLike + df$descInfo + df$descCred) / 4
  
  # warn
  df <- average_columns(df, c("warnClic1", "warnClic2", "warnClic3"), "warnClar")
  df <- average_columns(df, c("warnClic4", "warnClic5", "warnClic6"), "warnLike")
  df <- average_columns(df, c("warnClic7", "warnClic8", "warnClic9"), "warnInfo")
  df <- average_columns(df, c("warnClic10", "warnClic11", "warnClic12"), "warnCred")
  df$warnClic <- (df$warnClar + df$warnLike + df$warnInfo + df$warnCred) / 4
  
  # drawing
  df <- average_columns(df, c("drawingClic1", "drawingClic2", "drawingClic3"), "drawingClar")
  df <- average_columns(df, c("drawingClic4", "drawingClic5", "drawingClic6"), "drawingLike")
  df <- average_columns(df, c("drawingClic7", "drawingClic8", "drawingClic9"), "drawingInfo")
  df <- average_columns(df, c("drawingClic10", "drawingClic11", "drawingClic12"), "drawingCred")
  df$drawingClic <- (df$drawingClar + df$drawingLike + df$drawingInfo + df$drawingCred) / 4
  
  return(df)
}