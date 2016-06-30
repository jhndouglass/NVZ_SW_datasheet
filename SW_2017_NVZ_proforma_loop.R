## OPENING STATEMENTS-----------------------------------------------------------
################################################################################
## Author: John Douglass - ARE
## Date: April 2016
## Version: 0.1
################################################################################
## File Description;
## Purpose: Generate datahseets for SW NVZ designations
##
## Inputs: 'driver table' to input most of the data, source apportion table,
## SEPERATE model tables, pre-gen maps and graphs (folders in parent directory),
## tables with point soucre information
##
## Outputs: PDFs - output to parent directory
##
################################################################################
## Library Statements
library(knitr)
library(markdown)
library(rmarkdown)
library(png)
library(grid)
library(ggplot2)
library(pander)

################################################################################
## FUNCTION DEFINITIONS---------------------------------------------------------
LinesGraph <- function(concs) {
    stopifnot(length(concs) == 5)
    conc.text.plot <- c("High confidence upper bound",
                        "Moderate confidence upper bound",
                        "Land use model 95th percentile TIN as N concentration",
                        "Moderate condfidence lower bound",
                        "High confidence lower bound")
    palette <- c("red", "chartreuse4", "deepskyblue4", "gray45", "chartreuse1",
                 "deepskyblue")

    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_segment(size = 0.5, aes(x = 0.1, y = concs ,
                                                   xend = 0.2,
                                                   yend = concs,
                                                   colour = conc.text.plot,
                                                   linetype = conc.text.plot))
    p <- p + ggplot2::geom_segment(size = 0.5, aes(x = 0.1, y = 11.3,
                                                   xend = 0.2, yend = 11.3,
                                                   colour = "11.3 mg/l TIN",
                                                   linetype = "11.3 mg/l TIN"))
    p <- p + ggplot2::geom_segment(size = 0.1, aes(x = c(0.1, 0.2), y = 0,
                                                   xend = c(0.1, 0.2),
                                                   yend = max(concs) * 1.2))
    p <- p + ggplot2::scale_colour_manual(values = palette,
                                          breaks = c("11.3 mg/l TIN",
                                                     conc.text.plot))
    p <- p + ggplot2::scale_linetype_discrete(breaks = c("11.3 mg/l TIN",
                                                         conc.text.plot))
    p <- p + ggplot2::theme_void()
    p
}

FigCount <- function(ii) {
    ii <- ii + 1
}

TabCount <- function(ii) {
    ii <- ii + 1
}

## EXECUTED STATEMENTS----------------------------------------------------------
#pri <- read.csv("priority _NVZ.csv", stringsAsFactors = FALSE)
#pri <- pri[, 1]

nvzs <- read.csv("DatasheetExport.txt",
                 stringsAsFactors = FALSE)
#nvzs <- nvzs[2, ]
#nvzs <- nvzs[nvzs$NVZ_ID %in% pri, ]

new.text <- read.csv("designation_new_text.csv", row.names = 1,
                     stringsAsFactors = FALSE)
names(new.text) <- c("improved", "remained stable", "deteriorated", "notused")

existing.text <- read.csv("designation_existing_text.csv", row.names = 1,
                          stringsAsFactors = FALSE)
names(existing.text) <- c("improved", "remained stable", "deteriorated", "notused")

modified.text <- read.csv("designation_modified_text.csv", row.names = 1,
                          stringsAsFactors = FALSE)
names(modified.text) <- c("improved", "remained stable", "deteriorated", "notused")

sep_local <- read.csv("seperate_local.csv")
sep_cumulative <- read.csv("seperate_cumulative.csv")
sep_cycle1 <- read.csv("seperate_cycle1_list.csv")
sep_cycle1.us <- read.csv("seperate_cycle1_list_us.csv")

source.app <- read.csv("SourceApp.txt",
                       stringsAsFactors = FALSE)

smpts <- read.csv("sample_points_in_nvz.txt", stringsAsFactors = FALSE)

sources <- read.csv("MassBalance_individual_loads.txt",
                    stringsAsFactors = FALSE)

status <- read.csv("NVZ_status.txt", stringsAsFactors = FALSE)

version <- 1

#date <- as.Date('01/06/2016', format = '%d/%m/%Y')
#date <- format(date, '%d/%m/%Y')
date <- "June 2016"

## knitr loop ------------------------------------------------------------------
for (nvz in unique(nvzs$NVZ_ID)) {
    fig.count <- 0
    tab.count <- 0
    nvz.curr <- nvzs[nvzs$NVZ_ID == nvz, ]
    source.app.curr <- source.app[source.app$NVZ_ID == nvz, ]
    smpts.curr <- smpts[smpts$NVZ_ID == nvz, ]
    status.curr <- status[status$NVZ_ID == nvz, ]
    nvz.name <- nvz.curr$NVZ_NAME
    nvz.name.short <- substr(nvz.name, 0, nchar(nvz.name) - 4)
    nvz.id <- paste0("S", nvz.curr$NVZ_ID)
    nvz.wb <- nvz.curr$MAP_WB
    nvz.area <- round(nvz.curr$POLYAREA, 2)
    nvz.type1 <- status.curr$NVZ_STATUS
    nvz.type2 <- nvz.curr$Accumluated_NVZ_type_2017
    nvz.type3 <- nvz.curr$CYCLE
    nvz.prev.sw <- nvz.curr$PropSW13 * 100
    nvz.prev <- nvz.curr$PropNVZ13 * 100
    additional.info <- nvz.curr$Summary_Text
    workshop.discussion <- nvz.curr$Workshop_Discussion_summary
    overview.map <- paste0("Maps/Overview maps/overview_map_", nvz, ".png")
    monitoring.map <- paste0("Maps/Monitoring maps/monitoring_map_", nvz, ".png")
    source.table <- sources[sources$NVZ_ID == nvz, ]
    names(source.table) <- c("NVZ ID", "Source type", "Source sub-type",
                             "Source name", "Lower load (N kg yr ^-1^)",
                             "Upper load (N kg yr ^-1^)")
    ## assign modelling and monitoring scores from current and previous
    ## designation rounds and get count of failing samplwe points text
    if (nvz.type2 == "C") {
        main.wb.scores.text <- "catchment scores"
        nvz.type2.text <- "catchment"
        smpt.count <- nvz.curr$X2017_main_Smpt_in_WB + nvz.curr$X2017_trib_Smpt_in_WB
        smpt.count.upstream <- nvz.curr$X2017_Upstream_Main_SMPT + nvz.curr$X2017_Upstream_trib_SMPT + smpt.count
        fail.count <- nvz.curr$X2017_Main_Fails_in_WB + nvz.curr$X2017_trib_Fails_in_WB
        fail.count.upstream <- nvz.curr$X2017_Upstream_Main_River_Fails + nvz.curr$X2017_Upstream_trib_River_Fails + fail.count
        if (fail.count == 0) {
            fail.count.text <- "are currently no polluted sample points"
        } else{
            if (fail.count > 1) {
                fail.count.text <- paste0("are currently ", fail.count, " polluted sample points")
            } else{fail.count.text <- "is currently one polluted sample point"}
        }
        if (fail.count.upstream == 0) {
            fail.count.text.upstream <- "no polluted sample points"
        } else{
            if (fail.count.upstream > 1) {
                fail.count.text.upstream <- paste0(fail.count.upstream, " polluted sample points")
            } else{fail.count.text.upstream <- "one polluted sample point"}
        }
        worst <- nvz.curr$X2017_Main_SMPT_Id
        worst.graph <- paste0("Graphs/SW TIN PNG Files/", worst, ".png" )
        neap.graph <- paste0("Graphs/NEAPN_graphs/", nvz.wb,"_cycle1_catchment_neapn.png")
        modelling.map <- paste0("Maps/Modelling maps/modelling_map_catchment_", nvz, ".png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- smpts.curr$SMPT_ID
        all.smpts.names <- smpts.curr$smpt_short_name
        mult.graph <- paste0("Graphs/SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- nvz.curr$Main_Method
        mon.2009 <- nvz.curr$X2009_Main_Mon_Class
        mon.2013 <- nvz.curr$X2013_Main_mon_Class
        mon.2017 <- nvz.curr$X2017_Main_Mon_Class
        mod.2009 <- nvz.curr$X2009_Main_Mod_Class
        mod.2013 <- nvz.curr$X2013_Main_Mod_Class
        mod.2017 <- nvz.curr$X2017_Main_Mod_Class
        mon.conc <- round(nvz.curr$Main_Current_est, 2)
        mon.conc.u95 <- round(nvz.curr$Main_Current_u90, 2)
        mon.conc.l95 <- round(nvz.curr$Main_Current_l90, 2)
        mon.conc.u75 <- round(nvz.curr$Main_Current_u50, 2)
        mon.conc.l75 <- round(nvz.curr$Main_Current_l50, 2)
        trend.2017 <- nvz.curr$Main_Trend_Class
        trend.conc <- round(nvz.curr$Main_Trend_Est, 2)
        trend.conc.u95 <- round(nvz.curr$Main_Trend_u90, 2)
        trend.conc.l95 <- round(nvz.curr$Main_Trend_l90, 2)
        trend.conc.u75 <- round(nvz.curr$Main_Trend_u50, 2)
        trend.conc.l75 <- round(nvz.curr$Main_Tren_l50, 2)
        if (nvz.type3 == 1) {
            mod.conc <- round(nvz.curr$TIN_C_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$TIN_C_u95, 2)
            mod.conc.l95 <- round(nvz.curr$TIN_C_l95, 2)
            mod.conc.u75 <- round(nvz.curr$TIN_C_u75, 2)
            mod.conc.l75 <- round(nvz.curr$TIN_C_l75, 2)
        } else {
            #mod.conc <- round(nvz.curr$Cyc2_TIN_C_Q95, 2)
            #mod.conc.u95 <- round(nvz.curr$Cyc2_TIN_C_u95, 2)
            #mod.conc.l95 <- round(nvz.curr$Cyc2_TIN_C_l95, 2)
            #mod.conc.u75 <- round(nvz.curr$Cyc2_TIN_C_u75, 2)
            #mod.conc.l75 <- round(nvz.curr$Cyc2_TIN_C_l75, 2)
            mod.conc <- round(nvz.curr$TIN_C_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$TIN_C_u95, 2)
            mod.conc.l95 <- round(nvz.curr$TIN_C_l95, 2)
            mod.conc.u75 <- round(nvz.curr$TIN_C_u75, 2)
            mod.conc.l75 <- round(nvz.curr$TIN_C_l75, 2)
        }
        ps.load.min <- round(source.app.curr$Cat_AvgPntSrc_TIN_kgYr, 2)
        ps.load.max <- round(source.app.curr$Cat_MaxPntSrc_TIN_kYr, 2)
        diff.urban.load <- round(source.app.curr$Cat_Urban_N_MethodC_kgYr, 2)
        septic.load <- round(source.app.curr$Cat_SSD_N_to_River_kgYr, 2)
        ag.load <- round(source.app.curr$CAT_SPR_AG_TOTAL_KG, 2)
        arable.load <- round(source.app.curr$CAT_SPR_ARABLE_KG, 2)
        grass.load <- round(source.app.curr$CAT_SPR_GRASS_KG, 2)
        rough.load <- round(source.app.curr$CAT_SPR_ROUGH_KG, 2)
        otherag.load <- round(source.app.curr$CAT_SPR_OT_KG, 2)
        total.load.agmin <- round(source.app.curr$AGMIN_CAT_TOTALN_kgYr, 2)
        total.load.agmax <- round(source.app.curr$AGMAX_Cat_TotalN_kgYr, 2)
        perc.ag.load.min <- round(source.app.curr$Cat_AgMin_PropAg * 100, 1)
        perc.ag.load.max <- round(source.app.curr$Cat_AGMAx_PropAg * 100, 1)
        arable.area <- round(nvz.curr$NVZ_AR_HA, 2)
        grass.area <- round(nvz.curr$NVZ_PG_HA,2)
        rough.area <- round(nvz.curr$NVZ_RG_HA, 2)
        wood.area <- round(nvz.curr$NVZ_WD_HA, 2)
        urban.area <- round(nvz.curr$NVZ_UR_HA, 2)
        count.sheep <- nvz.curr$NVZ_Sheep
        count.cows <- nvz.curr$NVZ_Cows
        count.pigs <- nvz.curr$NVZ_Pigs
        count.poultry <- nvz.curr$NVZ_Poultry
        count.other <- nvz.curr$NVZ_OtherAnimals
        simcat.conc <- round(source.app.curr$SIMCAT_N_C_Conc, 2)
        simcat.prop <- round(source.app.curr$SIMCAT_C_DiffN_Prop, 2)
    } else {
        main.wb.scores.text <- "waterbody scores"
        nvz.type2.text <- "waterbody"
        smpt.count <- nvz.curr$X2017_main_Smpt_in_WB + nvz.curr$X2017_trib_Smpt_in_WB
        smpt.count.upstream <- nvz.curr$X2017_Upstream_Main_SMPT + nvz.curr$X2017_Upstream_trib_SMPT + smpt.count
        fail.count <- nvz.curr$X2017_Main_Fails_in_WB + nvz.curr$X2017_trib_Fails_in_WB
        fail.count.upstream <- nvz.curr$X2017_Upstream_Main_River_Fails + nvz.curr$X2017_Upstream_trib_River_Fails + fail.count
        if (fail.count == 0) {
            fail.count.text <- "are no polluted sample points"
        } else{
            if (fail.count > 1) {
                fail.count.text <- paste0("are ", fail.count, " polluted sample points")
            } else{fail.count.text <- "is one polluted sample point"}
        }
        if (fail.count.upstream == 0) {
            fail.count.text.upstream <- "no polluted sample points"
        } else{
            if (fail.count.upstream > 1) {
                fail.count.text.upstream <- paste0(fail.count.upstream, " polluted sample points")
            } else{fail.count.text.upstream <- "one polluted sample point"}
        }
        worst <- nvz.curr$Trib_SMPT_Id
        worst.graph <- paste0("Graphs/SW TIN PNG Files/", worst, ".png" )
        neap.graph <- paste0("Graphs/NEAPN_graphs/", nvz.wb,"_cycle1_wb_neapn.png")
        modelling.map <- paste0("Maps/Modelling maps/modelling_map_wb_", nvz, ".png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- smpts.curr$SMPT_ID
        all.smpts.names <- smpts.curr$smpt_short_name
        mult.graph <- paste0("Graphs/SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- nvz.curr$Trib_Method
        mon.2009 <- nvz.curr$X2009_WB_Mon_Class
        mon.2013 <- nvz.curr$X2013_WB_Mon_Class
        mon.2017 <- nvz.curr$X2017_WB_Mon_Class
        mod.2009 <- nvz.curr$X2009_WB_Mod_Class
        mod.2013 <- nvz.curr$X2013_WB_Model_Class
        mod.2017 <- nvz.curr$X2017_WB_Model_Class
        mon.conc <- round(nvz.curr$Trib_Current_Est, 2)
        mon.conc.u95 <- round(nvz.curr$Trib_Current_u90, 2)
        mon.conc.l95 <- round(nvz.curr$Trib_Current_l90, 2)
        mon.conc.u75 <- round(nvz.curr$Trib_Current_u50, 2)
        mon.conc.l75 <- round(nvz.curr$Trib_Current_l50, 2)
        trend.2017 <- nvz.curr$Trib_trend_Class
        trend.conc <- round(nvz.curr$Trib_Trend_Est, 2)
        trend.conc.u95 <- round(nvz.curr$Trib_Trend_u90, 2)
        trend.conc.l95 <- round(nvz.curr$Trib_Trend_l90, 2)
        trend.conc.u75 <- round(nvz.curr$Trib_Trend_u50, 2)
        trend.conc.l75 <- round(nvz.curr$Trib_Trend_l50, 2)
        if (nvz.type3 == 1) {
            mod.conc <- round(nvz.curr$TIN_WB_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$TIN_WB_u95, 2)
            mod.conc.l95 <- round(nvz.curr$TIN_WB_l95, 2)
            mod.conc.u75 <- round(nvz.curr$TIN_WB_u75, 2)
            mod.conc.l75 <- round(nvz.curr$TIN_WB_l75, 2)
        } else {
            #mod.conc <- round(nvz.curr$Cyc2_TIN_C_Q95, 2)
            #mod.conc.u95 <- round(nvz.curr$Cyc2_TIN_C_u95, 2)
            #mod.conc.l95 <- round(nvz.curr$Cyc2_TIN_C_l95, 2)
            #mod.conc.u75 <- round(nvz.curr$Cyc2_TIN_C_u75, 2)
            #mod.conc.l75 <- round(nvz.curr$Cyc2_TIN_C_l75, 2)
            mod.conc <- round(nvz.curr$TIN_C_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$TIN_C_u95, 2)
            mod.conc.l95 <- round(nvz.curr$TIN_C_l95, 2)
            mod.conc.u75 <- round(nvz.curr$TIN_C_u75, 2)
            mod.conc.l75 <- round(nvz.curr$TIN_C_l75, 2)
        }
        ps.load.min <- round(source.app.curr$WB_AvgPntSrc_TIN_kgYr, 2)
        ps.load.max <- round(source.app.curr$WB_MaxPntSrc_TIN_kYr, 2)
        diff.urban.load <- round(source.app.curr$WB_Urban_N_Method_C_kgYr, 2)
        septic.load <- round(source.app.curr$WB_SSD_N_toRiver_kgYr, 2)
        ag.load <- round(source.app.curr$WB_SPR_AG_Total_kg, 2)
        arable.load <- round(source.app.curr$WB_SPR_Arable_Kg, 2)
        grass.load <- round(source.app.curr$WB_SPR_Grass_Kg, 2)
        rough.load <- round(source.app.curr$WB_SPR_ROUGH_KG, 2)
        otherag.load <- round(source.app.curr$WB_SPR_OT_KG, 2)
        total.load.agmin <- round(source.app.curr$AGMIN_WB_TOTAL_N_kgYr, 2)
        total.load.agmax <- round(source.app.curr$AGMAX_WB_TOTALN_kgyr, 2)
        perc.ag.load.min <- round(source.app.curr$WB_Min_PropAg * 100, 1)
        perc.ag.load.max <- round(source.app.curr$WB_Max_PropAg * 100, 1)
        arable.area <- round(nvz.curr$NVZ_AR_HA, 2)
        grass.area <- round(nvz.curr$NVZ_PG_HA,2)
        rough.area <- round(nvz.curr$NVZ_RG_HA, 2)
        wood.area <- round(nvz.curr$NVZ_WD_HA, 2)
        urban.area <- round(nvz.curr$NVZ_UR_HA, 2)
        count.sheep <- nvz.curr$NVZ_Sheep
        count.cows <- nvz.curr$NVZ_Cows
        count.pigs <- nvz.curr$NVZ_Pigs
        count.poultry <- nvz.curr$NVZ_Poultry
        count.other <- nvz.curr$NVZ_OtherAnimals
    }

    ## get text to indicate how monitoring score has changed
    if (mon.2017 != 0) {
        if (mon.2017 > mon.2013) {
            mon.change.text <- "deteriorated"
        } else {if (mon.2017 == mon.2013) {
            mon.change.text <- "remained stable"
        } else {mon.change.text <- "improved" }}
    } else {
        mon.change.text <- "notused"
    }

    ## get text to indicate how modelling score has changed
    if (mod.2017 != 0) {
        if (mod.2017 > mod.2013) {
            mod.change.text <- "deteriorated"
        } else {if (mod.2017 == mod.2013) {
            mod.change.text <- "remained stable"
        } else {mod.change.text <- "improved" }}
    } else {
        mod.change.text <- "notused"
    }

    ## get text to explain status of NVZ
    if (tolower(nvz.type1) == "existing") {
        nvz.des.text <- existing.text[which(row.names(existing.text) == mon.change.text),
                                      which(names(existing.text) == mod.change.text)]
        nvz.type1.text <- "an existing"
        nvz.mod.text <- ""
    }

    if (tolower(nvz.type1) == "new") {
        nvz.des.text <- new.text[which(row.names(new.text) == mon.change.text),
                                 which(names(new.text) == mod.change.text)]
        nvz.type1.text <- "a new"
        nvz.mod.text <- ""
    }

    if (tolower(nvz.type1) == "modified") {
        nvz.des.text <- modified.text[which(row.names(modified.text) == mon.change.text),
                                      which(names(modified.text) == mod.change.text)]
        nvz.type1.text <- "a modified"
        nvz.mod.text <- "A modified designation means that at least part of the land covered by the proposed deisgnation is already designated. The designation has been updated to reflect current water quality."
    }

    ## Mod/Mon confidence
    if (mon.2017 == 0) {
        mon.change.text.long <- "We did not use monitoring evidence in proposing this designation."
    } else {
        mon.conf.text <- switch(as.character(mon.2017),
                                "1" = "Based on our assessment of monitoring data we have high confidence that the water is not polluted.",
                                "2" = "Based on our assessment of monitoring data we have moderate confidence that water is not polluted.",
                                "3" = "Based on our assessment of monitoring data we have low confidence that the water is not polluted.",
                                "4" = "Based on our assessment of monitoring data we have low confidence that the water is polluted.",
                                "5" = "Based on our assessment of monitoring data we have moderate confidence that water is polluted.",
                                "6" = "Based on our assessment of monitoring data we have high confidence that the water is polluted.")

        mon.change.text.long <- paste0("Our assessment of monitoring data shows that water quality in this NVZ has ", mon.change.text, " in the 2017 NVZ review period compared to the previous NVZ review. ", mon.conf.text)
    }

    if (mod.2017 == 0) {
        mod.change.text.long <- "We did not use modelling evidence in proposing this designation, because the proposed NVZ is smaller than the minimum size that we can apply the land use model with confidence."
    } else {
        mod.conf.text <- switch(as.character(mod.2017),
                                "1" = "Based on our modelling assessement we have high confidence that the water is not polluted.",
                                "2" = "Based on our modelling assessement we have moderate confidence that water is not polluted.",
                                "3" = "Based on our modelling assessement we have low confidence that the water is not polluted.",
                                "4" = "Based on our modelling assessement we have low confidence that the water is polluted.",
                                "5" = "Based on our modelling assessement we have moderate confidence that water is polluted.",
                                "6" = "Based on our modelling assessement we have high confidence that the water is polluted.")

        mod.change.text.long <- paste0("Our modelling assessment shows that water quality in this NVZ has ", mod.change.text, " in the 2017 NVZ review period compared to the previous NVZ review. ", mod.conf.text)
    }

    ## Extra bit of text to make it clear that dedesignation is not appropriate
    dedes.text <- NULL
    if (mon.2017 <= 3 && mod.2017 <= 3 && (tolower(nvz.type1) == "existing" || tolower(nvz.type1) == "modified")) {
        dedes.text <- "Despite current evidence suggesting the proposed designation is not affected by pollution, we must have high confidence that a designation will  not become affected by pollution in the future before a designation may be removed. In this case the designation has not met the criteria for de-designation."
    }

    ## get text to explain current 95th percentile estiamtes
    if (tolower(mon.type) == "weibull/qr") {
        curr.est.text <- "The green bar shows the current 95^th^ percentile. The green shaded areas show the uncertainty in the current 95^th^ percentile, we have high confidence that the current 95^th^ percentile lies within the light green shaded area and moderate confidence that the current 95^th^ percentile lies within the dark green shaded area"
    } else  {curr.est.text <- "The left most dashed blue line represents mid-2015, where it crosses the dark blue line is the current 95^th^ percentile"
    }

    ## this is done very verbosely...
    mod.conc <- ifelse(mod.conc < 0, 0, mod.conc)
    mod.conc.l75 <- ifelse(mod.conc.l75 < 0, 0, mod.conc.l75)
    mod.conc.l95 <- ifelse(mod.conc.l95 < 0, 0, mod.conc.l95)
    mod.conc.u75 <- ifelse(mod.conc.u75 < 0, 0, mod.conc.u75)
    mod.conc.u95 <- ifelse(mod.conc.u95 < 0, 0, mod.conc.u95)

    mon.conc <- ifelse(mon.conc < 0, 0, mon.conc)
    mon.conc.l75 <- ifelse(mon.conc.l75 < 0, 0, mon.conc.l75)
    mon.conc.l95 <- ifelse(mon.conc.l95 < 0, 0, mon.conc.l95)
    mon.conc.u75 <- ifelse(mon.conc.u75 < 0, 0, mon.conc.u75)
    mon.conc.u95 <- ifelse(mon.conc.u95 < 0, 0, mon.conc.u95)

    trend.conc <- ifelse(trend.conc < 0, 0, trend.conc)
    trend.conc.l75 <- ifelse(trend.conc.l75 < 0, 0, trend.conc.l75)
    trend.conc.l95 <- ifelse(trend.conc.l95 < 0, 0, trend.conc.l95)
    trend.conc.u75 <- ifelse(trend.conc.u75 < 0, 0, trend.conc.u75)
    trend.conc.u95 <- ifelse(trend.conc.u95 < 0, 0, trend.conc.u95)

    ## Markdown render call --------------------------------------------------------
    render(input = paste0(getwd(), "/SW_2017_NVZ_proforma_markup.rmd"),
           output_format = "pdf_document",
           output_file = paste0("Data_Sheet_", nvz.id, ".pdf"),
           output_dir = paste0(getwd(), "/out"))
}
