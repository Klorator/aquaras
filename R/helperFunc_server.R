#' Update well info
#'
#' @family Runlist Generator
#'
#' @param df Data frame to update
#' @param well.current Current well plate position (LC_Position)
#' @param date New date value (character string)
#' @param signature New signature value
#' @param compound New compound value
#' @param timepoint New timepoint value
#' @param type New type value
#' @param replicate New replicate value
#'
#' @return Same data frame, but with updated row
#' @export
#'
#' @examples
#' Example_Runlist[1,]
#' Example_Runlist = well.update(df = Example_Runlist,
#'                               well.current = "3:A,1",
#'                               date = "20220730",
#'                               signature = "RH",
#'                               compound = "Warfarin",
#'                               timepoint = "42",
#'                               type = "bead",
#'                               replicate = "6")
#' Example_Runlist[1,]
well.update = function(df, well.current, date, signature,
                       compound, timepoint, type, replicate) {
  # Sanitize input
  date      = sanitizeInput(date)
  signature = sanitizeInput(signature)
  compound  = sanitizeInput(compound)
  timepoint = sanitizeInput(timepoint)
  type      = sanitizeInput(type)
  replicate = sanitizeInput(replicate)
  # New well info
  df[df["LC_Position"] == well.current, "Compound"]   = compound
  df[df["LC_Position"] == well.current, "Timepoint"]  = timepoint
  df[df["LC_Position"] == well.current, "Well_Type"]  = type
  df[df["LC_Position"] == well.current, "Replicate"]  = replicate
  df[df["LC_Position"] == well.current, "Date"]       = date
  df[df["LC_Position"] == well.current, "Signature"]  = signature
  # New composite sample name & text
  index = df[df["LC_Position"] == well.current, "Index"]
  new.sample_name = paste0(date, "_", signature, "_", "Index.", index)
  new.sample_text = paste0(compound, "_", timepoint, "_", type, "_", replicate)
  df[df["LC_Position"] == well.current, "Sample_name"] = new.sample_name
  df[df["LC_Position"] == well.current, "Sample_text"] = new.sample_text
  # Change LC Well Type
  if ( type == "blank" ) {
    df[df["LC_Position"] == well.current, "LC_Well_Type"] = "blank"
  } else {
    df[df["LC_Position"] == well.current, "LC_Well_Type"] = "Analyte"
  }
  return(df)
}

#' Update Date, Signature, & Sample_name
#'
#' Update Date, Signature, & Sample_name for the entire data frame.
#' Sample_name is a composite in the format "date_signature_index".
#'
#' @family Runlist Generator
#'
#' @inheritParams well.update
#'
#' @return Updated data frame
#' @export
#'
#' @examples
#' head(Example_Runlist)
#' Example_Runlist = dateSignAll.update(df = Example_Runlist,
#'                                      date = "20220730",
#'                                      signature = "RH")
#' head(Example_Runlist)
dateSignAll.update = function(df, date, signature) {
  date      = sanitizeInput(date)
  signature = sanitizeInput(signature)
  df["Date"]      = date
  df["Signature"] = signature
  for (i in 1:nrow(df)) {
    df[i, "Sample_name"] = paste0(df[i, "Date"], "_", df[i, "Signature"], "_", "Index.", df[i, "Index"])
  }
  return(df)
}

#' Sanitize input
#'
#' Replaces underscores with periods.
#'
#' @family Runlist Generator
#'
#' @param inputVar Character string to sanitize
#'
#' @return Character string
#' @export
#' @examples
#' sanitizeInput("Spaces etc -- but_no_underscore!")
sanitizeInput = function(inputVar) {
  cleanVar = gsub("_", ".", inputVar) # Replace any underscore
  return(cleanVar)
}
