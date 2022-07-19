#' Update well info
#'
#' @param df Data frame to update row
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
update.well = function(df, well.current, date, signature,
                       compound, timepoint, type, replicate) {
  # New well info
  df[df["LC_Position"] == well.current, "Compound"]   = compound
  df[df["LC_Position"] == well.current, "Timepoint"]  = timepoint
  df[df["LC_Position"] == well.current, "Well_Type"]  = type
  df[df["LC_Position"] == well.current, "Replicate"]  = replicate
  df[df["LC_Position"] == well.current, "Date"]       = date
  df[df["LC_Position"] == well.current, "Signature"]  = signature
  # New composit sample name & text
  index = df[df["LC_Position"] == well.current, "Index"]
  new.sample_name = paste0(date,"_",signature,"_",index)
  new.sample_text = paste0(compound,"_",timepoint,"_",type,"_",replicate)
  df[df["LC_Position"] == well.current, "Sample_name"] = new.sample_name
  df[df["LC_Position"] == well.current, "Sample_text"] = new.sample_text
  return(df)
}
