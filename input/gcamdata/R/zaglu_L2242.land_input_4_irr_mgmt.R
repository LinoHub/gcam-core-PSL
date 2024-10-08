# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2242.land_input_4_irr_mgmt
#'
#' Generate logit exponent of the fourth land node that specifies crop commodity and GLU by region,
#' and generate the ghost node share for the bioenergy node.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2242.LN4_Logit}, \code{L2242.LN4_NodeGhostShare}, \code{L2242.LN4_NodeIsGhostShareRel}. The corresponding file in the
#' original data system was \code{L2242.land_input_4_irr_mgmt.R} (aglu level2).
#' @details This chunk generates the logit exponent of the fourth land nest that specifies crop commodity and GLU by region,
#' and the ghost node share for the bioenergy node in future years, and specifies whether the bioenergy ghost node share is relative.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @importFrom tidyr separate
#' @author RC August 2017
module_aglu_L2242.land_input_4_irr_mgmt <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_LandNode_logit_irr",
      FILE = "aglu/A_bio_ghost_share",
      FILE = "aglu/A_LT_Mapping",
      FILE = "aglu/A_LandLeaf3",
      FILE = "aglu/A_biomassSupplyShare_R",
      "L2012.AgYield_bio_ref",
      "L2012.AgProduction_ag_irr_mgmt")

  MODULE_OUTPUTS <-
    c("L2242.LN4_Logit",
      "L2242.LN4_NodeGhostShare",
      "L2242.LN4_NodeIsGhostShareRel")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    # silence package check notes
    GLU_name <- LandLeaf <- LandNode4 <- LandNode1 <- LandNode2 <- LandNode3 <- year <-
      ghost.share <- GCAM_commodity <- Land_Type <- ghost.unnormalized.share <-
      region <- AgSupplySector <- AgSupplySubsector <- NULL

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # L2242.LN4_Logit: Logit exponent of the fourth land nest by region
    # There are no technologies that are disaggregated to irrigated and rainfed but not to lo- and hi-input techs,
    # so here we only write out the logit exponent for the irrigated/rainfed node competition.
    L2012.AgProduction_ag_irr_mgmt %>%
      distinct(region, AgSupplySubsector, AgSupplySector) %>%
      bind_rows(distinct(L2012.AgYield_bio_ref, region, AgSupplySubsector, AgSupplySector)) %>%
      mutate(AgSupplySector = if_else(grepl("biomassTree", AgSupplySubsector), "biomassTree", "biomassGrass")) %>%
      left_join(A_LandLeaf3, by=c("AgSupplySector" = "LandLeaf")) %>%
      separate(AgSupplySubsector, c("LandNode4", "GLU_name")) %>%
      mutate(logit.year.fillout = min(MODEL_BASE_YEARS)) %>%
      # Match in logit exponent values, use left_join instead because the logit.type variable are NAs, drop later
      left_join(A_LandNode_logit_irr, by = c("LandNode4" = "LandNode")) %>%
      mutate(LandAllocatorRoot = "root",
             LandNode1 = paste(LandNode1, GLU_name, sep = "_"),
             LandNode2 = paste(LandNode2, GLU_name, sep = "_"),
             LandNode3 = paste(LandNode3, GLU_name, sep = "_"),
             LandNode4 = paste(LandNode4, GLU_name, sep = "_")) %>%
      select(LEVEL2_DATA_NAMES[["LN4_Logit"]], LOGIT_TYPE_COLNAME) ->
      L2242.LN4_Logit



    # assert that all regions in GCAM_region_names are in A_biomassSupplyShare_R
    if (GCAM_region_names %>% distinct(region) %>% dplyr::setdiff(A_biomassSupplyShare_R %>% distinct(region)) %>% nrow > 0) {
      warning("Regions doesn't exist in A_biomassSupplyShare_R follow USA assumptions (High). Consider adding all GCAM regions to aglu/A_biomassSupplyShare_R to avoid this warning.")
    }

    A_biomassSupplyShare_R %>%
      right_join(GCAM_region_names %>% distinct(region), by = "region") %>%
      # set missing to USA's value so region breakout won't have any issue
      replace_na(list(preference = A_biomassSupplyShare_R$preference[A_biomassSupplyShare_R$region == "USA"])) %>%
      left_join(A_bio_ghost_share, by = "preference") %>%
      select(region, year, ghost.share) ->
      A_bio_ghost_share_R

    # L2242.LN4_NodeGhostShare:
    # Specify ghost node share for bioenergy node in future years (starting with first bio year).
    L2012.AgYield_bio_ref %>%
      distinct(region, AgSupplySubsector) %>%
      mutate(GCAM_commodity = if_else(grepl("^biomassGrass", AgSupplySubsector), "biomassGrass", "biomassTree"),
             GLU_name = if_else(grepl("^biomassGrass", AgSupplySubsector), gsub("biomassGrass_", "", AgSupplySubsector),
                                                                            gsub("biomassTree_", "", AgSupplySubsector))) %>%
      left_join_error_no_match(A_LT_Mapping, by = "GCAM_commodity") %>%
      mutate(LandAllocatorRoot = "root",
             LandNode1 = paste(LandNode1, GLU_name, sep = aglu.LT_GLU_DELIMITER),
             LandNode2 = paste(LandNode2, GLU_name, sep = aglu.LT_GLU_DELIMITER),
             LandNode3 = paste(LandNode3, GLU_name, sep = aglu.LT_GLU_DELIMITER),
             LandNode4 = paste(LandLeaf, GLU_name, sep = aglu.LT_GLU_DELIMITER)) %>%
      repeat_add_columns(tibble::tibble(year = MODEL_FUTURE_YEARS)) %>%
      filter(year >= aglu.BIO_START_YEAR) %>%
      left_join(A_bio_ghost_share_R, by = c("year", "region")) %>%
      group_by(region, AgSupplySubsector, GCAM_commodity, GLU_name, LandNode1, LandNode2, LandNode3, LandNode4,
               Land_Type, LandLeaf, LandAllocatorRoot) %>%
      mutate(ghost.unnormalized.share = approx_fun(year, ghost.share)) %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES[["LN4_NodeGhostShare"]]) ->
      L2242.LN4_NodeGhostShare

    # L2242.LN4_NodeIsGhostShareRel:
    # Specify whether bioenergy ghost node share is relative.
    # These are the same values that would have been set in the leaves in land input 3.
    # Specify whether bioenergy ghost share is relative to the dominant crop
    # Note: this was just set to 1 in the old data system
    L2242.LN4_NodeGhostShare %>%
      select(-year, -ghost.unnormalized.share) %>%
      distinct() %>%
      mutate(is.ghost.share.relative = 1) ->
      L2242.LN4_NodeIsGhostShareRel

    # Produce outputs
    L2242.LN4_Logit %>%
      add_title("Logit exponent of the fourth land nest by region") %>%
      add_units("NA") %>%
      add_comments("Only write out the logit exponent for the irrigated/rainfed node competition") %>%
      add_legacy_name("L2242.LN4_Logit") %>%
      add_precursors("aglu/A_LandNode_logit_irr",
                     "aglu/A_LandLeaf3",
                     "L2012.AgYield_bio_ref",
                     "L2012.AgProduction_ag_irr_mgmt") ->
      L2242.LN4_Logit

    L2242.LN4_NodeGhostShare %>%
      add_title("Ghost node share for bioenergy node in future years, the fourth land nest") %>%
      add_units("NA") %>%
      add_comments("Ghost share values are read in from an assumptions file") %>%
      add_comments("and then mapped to all bioenergy nodes for future years after the bio start year") %>%
      add_legacy_name("L2242.LN4_NodeGhostShare") %>%
      add_precursors("L2012.AgYield_bio_ref",
                     "aglu/A_bio_ghost_share",
                     "aglu/A_biomassSupplyShare_R",
                     "common/GCAM_region_names",
                     "aglu/A_LT_Mapping") ->
      L2242.LN4_NodeGhostShare

    L2242.LN4_NodeIsGhostShareRel %>%
      add_title("Whether bioenergy ghost share is relative to the dominant crop, the forth land nest") %>%
      add_units("NA") %>%
      add_comments("Copy the nesting structure from L2242.LN4_NodeGhostShare") %>%
      add_comments("Set is.ghost.share.relative to 1") %>%
      add_legacy_name("L2242.LN4_NodeIsGhostShareRel") %>%
      same_precursors_as("L2242.LN4_NodeGhostShare") ->
      L2242.LN4_NodeIsGhostShareRel

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
