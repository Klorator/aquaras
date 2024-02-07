# Calculations -----------------------------------------------------------------
#' Calculate F u,Homogenate
#'
#'  Equation: f u,hom = Peak area(buffer sample) /
#'  ( Peak area(cell homogenate sample)*sample dilution factor )
#'
#' @param df_calc Data frame with values
#' @param Buffer Name of column for buffer values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Name of column for dilution factor
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_Fu.hom <- function(df_calc,
                           Buffer = "Buffer_Con._avg",
                           Homogenate = "homogenate",
                           Dilution_factor = "dilution") {
  df_calc <- df_calc %>%
    dplyr::mutate(fuhom = (.data[[Buffer]])/
                    (.data[[Homogenate]] * .data[[Dilution_factor]]) )
  return(df_calc)
}
#' Calculate D protein (dilution factor of protein)
#'
#' Equation: D = 1000 /
#' ("Protein conc.(mg/ml)" * 6.5) )
#'
#' @param df_calc Data frame with values
#' @param Protein_col Name of column for protein values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_D.prot <- function(df_calc,
                           Protein_col = "Protein_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(D = 1000 / (.data[[Protein_col]] * 6.5) )
  return(df_calc)
}
#' Calculate F u,cell
#'
#' Equation: fucell = 1 / (D.prot * (1/Fu.hom-1)+1)
#'
#' @param df_calc Data frame with values
#' @param D.prot Name of column for protein dilution values
#' @param Fu.hom Name of column for F u,Homogenate values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_Fu.cell <- function(df_calc,
                            D.prot = "D",
                            Fu.hom = "fuhom") {
  df_calc <- df_calc %>%
    dplyr::mutate(fucell = 1 / (.data[[D.prot]] * (1/.data[[Fu.hom]]-1)+1) )
  return(df_calc)
}
#' Calculate Stability
#'
#' Equation: Stability = Stab_Conc.avg / Czero_Conc.avg
#'
#' @param df_calc Data frame with values
#' @param Stab Name of column for stability values
#' @param C.zero Name of column for C zero values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_stability <- function(df_calc,
                              Stab = "Stab",
                              C.zero = "Czero") {
  df_calc <- df_calc %>%
    dplyr::mutate(Stability = .data[[Stab]] / .data[[C.zero]])
  return(df_calc)
}
#' Calculate mass balance 10.2.5
#'
#' Calculate mass balance according to section 10.2.5 in the SOP.
#' Equation:Mass_balance_10.2.5 = (Hom_Conc.avg * dilution
#' + Buffer_Conc._avg * 1.75) / Stab_Conc.avg
#'
#' @param df_calc Data frame with values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Name of column for dilution factor
#' @param Buffer Name of column for buffer values
#' @param Stab Name of column for stability values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_mass_balance_10.2.5 <- function(df_calc,
                                        Homogenate = "Hom_Conc._avg",
                                        Dilution_factor = "dilution",
                                        Buffer = "Buffer_Con._avg",
                                        Stab = "Stab_Conc._avg",
                                        mass_factor = 1.75) {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
                    (.data[[Homogenate]]*.data[[Dilution_factor]] +.data[[Buffer]]*mass_factor)
                  / .data[[Stab]] )
  return(df_calc)
}
#' Calculate A cell
#'
#' Equation: Acell = Cell_Conc.avg(nM) * 200(micro l)
#'
#' @param df_calc Data frame with values
#' @param Cell Name of column for cell values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_A.cell <- function(df_calc,
                           Cell = "Cell_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Acell = .data[[Cell]] * 200)
  return(df_calc)
}
#' Calculate Cell volume
#'
#' Equation: Vcell = "Protein amount/well" * 6.5
#'
#' @param df_calc Data frame with values
#' @param Protein_volume Name of column for protein volume values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_V.cell <- function(df_calc,
                           Protein_volume = "V.Prot") {
  df_calc <- df_calc %>%
    dplyr::mutate(Vcell = .data[[Protein_volume]] * 6.5)
  return(df_calc)
}
#' Calculate Kp
#'
#' Equation: Kp = (Acell/Vcell) / (Medium_Conc.avg * 10
#'
#' @param df_calc Data frame with values
#' @param A.cell Name of column for A cell values
#' @param V.cell Name of column for V cell values
#' @param Medium Name of column for medium values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_Kp <- function(df_calc,
                       A.cell = "Acell",
                       V.cell = "Vcell",
                       Medium = "Medium_Conc._avg") {
  df_calc <- df_calc %>%
    dplyr::mutate(Kp = (.data[[A.cell]]/.data[[V.cell]]) / (.data[[Medium]] * 10) )
  return(df_calc)
}
#' Calculate mass balance 10.3.2
#'
#' Calculate mass balance according to section 10.3.2 in the SOP.
#' Equation: Mass_balance_10.3.5 =
#' ((Acell+Medium_Conc.avg*10*Vmedium)/(Vmedium))/Kp_Conc.avg
#'
#' @param df_calc Data frame with values
#' @param A.cell Name of column for A cell values
#' @param Medium Name of column for medium values
#' @param V.medium Name of column for V medium values
#' @param C.zero.Kp Name of column for C zero Kp values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_mass_balance_10.3.4 <- function(df_calc,
                                        A.cell = "Acell",
                                        Medium = "Medium_Conc._avg",
                                        V.medium = 200,
                                        C.zero.Kp = "CzeroKp") {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.3.5 =
                    ((.data[[A.cell]]+.data[[Medium]]*10*{{V.medium}})/({{V.medium}}))
                  /.data[[C.zero.Kp]] )
  return(df_calc)
}
#' Calculate F ic
#'
#' Equation: Fic = fucell * Kp
#'
#' @param df_calc Data frame with values
#' @param Fu.cell Name of column for F u, cell values
#' @param Kp Name of column for Kp values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_Fic <- function(df_calc,
                        Fu.cell = "fucell",
                        Kp = "Kp") {
  df_calc <- df_calc %>%
    dplyr::mutate(Fic = .data[[Fu.cell]] * .data[[Kp]])
  return(df_calc)
}
#' Calculate Fu, feces
#'
#' Equation: Fufeces = 1 / (D.prot * (1/Fu.hom-1)+1)
#'
#' @param df_calc Data frame with values
#' @param D Value for dilution factor
#' @param Fu.hom Name of column for F u,Homogenate values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fic_fu.feces <- function(df_calc,
                             D = 1 / ( (1/4.8) * 0.95),
                             Fu.hom = "fuhom") {
  df_calc <- df_calc %>%
    dplyr::mutate(fufeces = 1 / (D * (1/.data[[Fu.hom]]-1)+1) )
  return(df_calc)
}
#' Calculate mass balance 10.2.5 (Fu feces)
#'
#' Calculate mass balance according to section 10.2.5 in the SOP.
#' Equation:Mass_balance_10.2.5 = (Hom_Conc.avg * dilution
#' + Buffer_Conc._avg * 1.75) / Stab_Conc.avg
#'
#' @param df_calc Data frame with values
#' @param Homogenate Name of column for Homogenate values
#' @param Dilution_factor Dilution factor value
#' @param Buffer Name of column for buffer values
#' @param Stab Name of column for stability values
#'
#' @return Same data frame with added column from equation
#' @noRd
ras.Fu_feces_mass_balance_10.2.5 <- function(df_calc,
                                             Homogenate = "Hom_Conc._avg",
                                             Dilution_factor = 4.8,
                                             Buffer = "Buffer_Con._avg",
                                             Stab = "Stab_Conc._avg",
                                             mass_factor = 1.75) {
  df_calc <- df_calc %>%
    dplyr::mutate(Mass_balance_10.2.5 =
                    (.data[[Homogenate]] * Dilution_factor + .data[[Buffer]] * mass_factor)
                  / .data[[Stab]] )
  return(df_calc)
}

ras.Fu_feces_summary <- function(df,
                                 grouping_col = "Sample_ID",
                                 .summarize = TRUE,
                                 .SD = TRUE) {
  if (.SD) {

  }

  if (.summarize) {

  }

  return(df)
}
