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
library(markdown)
library(rmarkdown)
library(ggplot2)

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
## EXECUTED STATEMENTS----------------------------------------------------------
nvzs <- read.csv("DatasheetExport.csv",
                 stringsAsFactors = FALSE)
nvzs <- nvzs[2:4, ]

new.text <- read.csv("designation_new_text.csv", row.names = 1,
                     stringsAsFactors = FALSE)
names(new.text) <- c("improved", "remained stable", "deteriorated")

sep_local <- read.csv("seperate_local.csv")
sep_cumulative <- read.csv("seperate_cumulative.csv")
sep_cycle1 <- read.csv("seperate_cycle1_list.csv")

source.app <- read.csv("SourceApp.csv",
                       stringsAsFactors = FALSE)

existing.text <- read.csv("designation_existing_text.csv", row.names = 1,
                          stringsAsFactors = FALSE)
names(existing.text) <- c("improved", "remained stable", "deteriorated")

## knitr loop ------------------------------------------------------------------
for (nvz in unique(nvzs$NVZ_ID)) {
    nvz.curr <- nvzs[nvzs$NVZ_ID == nvz, ]
    source.app.curr <- source.app[source.app$NVZ_ID == nvz, ]
    nvz.name <- nvz.curr$NVZ_NAME
    nvz.id <- nvz.curr$NVZ_ID
    nvz.wb <- nvz.curr$MAP_WB
    nvz.area <- nvz.curr$POLYAREA
    nvz.type1 <- nvz.curr$NVZ_TYPE
    nvz.type2 <- nvz.curr$Accumluated_NVZ_type_2017
    nvz.type3 <- nvz.curr$CYCLE
    nvz.prev.sw <- nvz.curr$PropSW13 * 100
    nvz.prev <- nvz.curr$PropNVZ13 * 100
    additional.info <- nvz.curr$Summary_Text
    fail.count.text.upstream <- "Equivalent graphs for all other upstream
                                polluted sample points are included in
                                appendix 1 of this document."
    overview.map <- paste0("Overview maps/", nvz.wb, "_overviewmap.png")
    monitoring.map <- paste0("Monitoring maps/", nvz.wb, "_monitoringmap.png")
    ## The table of source must be supplied to the program as a table - chat
    ## with chris.
    source.table <- read.csv(paste0("N sources tables/", nvz.wb,
                                    "_source_loads.csv"))
    names(source.table) <- c("Discharge", "Data source", "Average flow",
                             "Average concentration",
                             "Average load")

    ## assign modelling and monitoring scores from current and previous
    ## designation rounds and get count of failing samplwe points text
    if (nvz.type2 == "C") {
        main.wb.scores.text <- "catchment scores"
        nvz.type2.text <- "catchment"
        smpt.count <- nvz.curr$X2017_main_Smpt_in_WB
        smpt.count.upstream <- nvz.curr$X2017_Upstream_Main_SMPT
        fail.count <- nvz.curr$X2017_Main_Fails_in_WB
        fail.count.upstream <- nvz.curr$X2017_Upstream_Main_River_Fails
        if (fail.count == 0) {
            fail.count.text <- "are no polluted sample points"
        } else{
            if (fail.count > 1) {
                fail.count.text <- paste0("are ", fail.count, " polluted sample points")
            } else{fail.count.text <- "is one polluted sample point"}
        }
        if (fail.count.upstream == 0) {
            fail.count.text.upstream1 <- "no polluted sample points"
        } else{
            if (fail.count.upstream > 1) {
                fail.count.text.upstream1 <- paste0(fail.count.upstream, " polluted sample points")
            } else{fail.count.text.upstream1 <- "one polluted sample point"}
        }
        ifelse(fail.count.upstream > 1,
               fail.count.text.us <- fail.count.text.upstream,
               fail.count.text.us <- "")
        worst <- nvz.curr$X2017_Main_SMPT_Id
        worst.graph <- paste0("SW TIN PNG Files/", worst, ".png" )
        neap.graph <- paste0("NEAPN_graphs/", nvz.wb,"_cycle", nvz.type3, "_catchment_neapn.png")
        modelling.map <- paste0("Modelling maps/", nvz.wb,
                                "_modelling_catchment_cycle", nvz.type3, "_map.png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- c("45212350", "45213020", "45214350", "45215500",
                       "45216170", "45217250", "45220500", "45221950") #!!!!
        mult.graph <- paste0("SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- nvz.curr$Main_Method
        mon.2009 <- nvz.curr$X2009_Main_Mon_Class
        mon.2013 <- nvz.curr$X2013_Main_mon_Class
        mon.2017 <- nvz.curr$X2017_Main_Mon_Class
        mon.conf.text <- switch(as.character(mon.2017),
                                "0" = "",
                                "1" = "Based on our assessment of monitoring data we are highly confident that the water is not polluted.",
                                "2" = "Based on our assessment of monitoring data we are moderately confident that water is not polluted.",
                                "3" = "Based on our assessment of monitoring data we are somewhat confident that the water is not polluted.",
                                "4" = "Based on our assessment of monitoring data we are somewhat confident that the water is polluted.",
                                "5" = "Based on our assessment of monitoring data we are moderately confident that water is polluted.",
                                "6" = "Based on our assessment of monitoring data we are highly confident that the water is polluted.")
        mod.2009 <- nvz.curr$X2009_Main_Mod_Class
        mod.2013 <- nvz.curr$X2013_Main_Mod_Class
        mod.2017 <- nvz.curr$X2017_Main_Mod_Class
        mod.conf.text <- switch(as.character(mod.2017),
                                "0" = "We did not use modelling evidence in making this designation",
                                "1" = "Based on our modelling assessement we are highly confident that the water is not polluted.",
                                "2" = "Based on our modelling assessement we are moderately confident that water is not polluted.",
                                "3" = "Based on our modelling assessement we are somewhat confident that the water is not polluted.",
                                "4" = "Based on our modelling assessement we are somewhat confident that the water is polluted.",
                                "5" = "Based on our modelling assessement we are moderately confident that water is polluted.",
                                "6" = "Based on our modelling assessement we are highly confident that the water is polluted.")
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
            mod.conc <- round(nvz.curr$Cyc2_TIN_C_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$Cyc2_TIN_C_u95, 2)
            mod.conc.l95 <- round(nvz.curr$Cyc2_TIN_C_l95, 2)
            mod.conc.u75 <- round(nvz.curr$Cyc2_TIN_C_u75, 2)
            mod.conc.l75 <- round(nvz.curr$Cyc2_TIN_C_l75, 2)
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
        perc.ag.load.min <- round(source.app.curr$Cat_AgMin_PropAg * 100, 2)
        perc.ag.load.max <- round(source.app.curr$Cat_AGMAx_PropAg * 100, 2)
    } else {
        main.wb.scores.text <- "waterbody scores"
        nvz.type2.text <- "waterbody"
        smpt.count <- nvz.curr$X2017_trib_Smpt_in_WB
        smpt.count.upstream <- nvz.curr$X2017_Upstream_trib_SMPT
        fail.count <- nvz.curr$X2017_trib_Fails_in_WB
        fail.count.upstream <- nvz.curr$X2017_Upstream_trib_River_Fails
        if (fail.count == 0) {
            fail.count.text <- "are no polluted sample points"
        } else{
            if (fail.count > 1) {
                fail.count.text <- paste0("are ", fail.count, " polluted sample points")
            } else{fail.count.text <- "is one polluted sample point"}
        }
        if (fail.count.upstream == 0) {
            fail.count.text.upstream1 <- "no polluted sample points"
        } else{
            if (fail.count.upstream > 1) {
                fail.count.text.upstream1 <- paste0(fail.count.upstream, " polluted sample points")
            } else{fail.count.text.upstream1 <- "one polluted sample point"}
        }
        ifelse(fail.count.upstream > 1,
               fail.count.text.us <- fail.count.text.upstream,
               fail.count.text.us <- "")
        worst <- nvz.curr$Trib_SMPT_Id
        worst.graph <- paste0("SW TIN PNG Files/", worst, ".png" )
        neap.graph <- paste0("NEAPN_graphs/", nvz.wb,"_cycle", nvz.type3, "_catchment_neapn.png")
        modelling.map <- paste0("Modelling maps/", nvz.wb,
                                "_modelling_catchment_cycle", nvz.type3, "_map.png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- c("PWRR0007", "PWRR0002") #!!!!!
        mult.graph <- paste0("SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- nvz.curr$Trib_Method
        mon.2009 <- nvz.curr$X2009_WB_Mon_Class
        mon.2013 <- nvz.curr$X2013_WB_Mon_Class
        mon.2017 <- nvz.curr$X2017_WB_Mon_Class
        mon.conf.text <- switch(as.character(mon.2017),
                                "0" = "",
                                "1" = "Based on our assessment of monitoring data we are highly confident that the water is not polluted.",
                                "2" = "Based on our assessment of monitoring data we are moderately confident that water is not polluted.",
                                "3" = "Based on our assessment of monitoring data we are somewhat confident that the water is not polluted.",
                                "4" = "Based on our assessment of monitoring data we are somewhat confident that the water is polluted.",
                                "5" = "Based on our assessment of monitoring data we are moderately confident that water is polluted.",
                                "6" = "Based on our assessment of monitoring data we are highly confident that the water is polluted.")
        mod.2009 <- nvz.curr$X2009_WB_Mod_Class
        mod.2013 <- nvz.curr$X2013_WB_Model_Class
        mod.2017 <- nvz.curr$X2017_WB_Model_Class
        mod.conf.text <- switch(as.character(mod.2017),
                                "0" = "We did not use modelling evidence in making this designation",
                                "1" = "Based on our modelling assessement we are highly confident that the water is not polluted.",
                                "2" = "Based on our modelling assessement we are moderately confident that water is not polluted.",
                                "3" = "Based on our modelling assessement we are somewhat confident that the water is not polluted.",
                                "4" = "Based on our modelling assessement we are somewhat confident that the water is polluted.",
                                "5" = "Based on our modelling assessement we are moderately confident that water is polluted.",
                                "6" = "Based on our modelling assessement we are highly confident that the water is polluted.")
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
            mod.conc <- round(nvz.curr$Cyc2_TIN_WB_Q95, 2)
            mod.conc.u95 <- round(nvz.curr$Cyc2_TIN_WB_u95, 2)
            mod.conc.l95 <- round(nvz.curr$Cyc2_TIN_WB_l95, 2)
            mod.conc.u75 <- round(nvz.curr$Cyc2_TIN_WB_u75, 2)
            mod.conc.l75 <- round(nvz.curr$Cyc2_TIN_WB_l75, 2)
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
        perc.ag.load.min <- round(source.app.curr$WB_Min_PropAg * 100, 2)
        perc.ag.load.max <- round(source.app.curr$WB_Max_PropAg * 100, 2)
    }

    ## get text to indicate how monitoring score has changed
    if (mon.2017 > max(mon.2013, mon.2009)) {
        mon.change.text <- "deteriorated"
    } else {if (mon.2017 == max(mon.2013, mon.2009)) {
        mon.change.text <- "remained stable"
    } else {mon.change.text <- "improved" }}

    ## get text to indicate how modelling score has changed
    if (mod.2017 > max(mod.2013, mod.2009)) {
        mod.change.text <- "deteriorated"
    } else {if (mod.2017 == max(mod.2013, mod.2009)) {
        mod.change.text <- "remained stable"
    } else {mod.change.text <- "improved" }}


    ## get text to explain status of NVZ
    if (tolower(nvz.type1) == "existing") {
        nvz.des.text <- existing.text[which(row.names(existing.text) == mon.change.text),
                                      which(names(existing.text) == mod.change.text)]
        nvz.type1.text <- "an existing"
    } else {
        nvz.des.text <- new.text[which(row.names(new.text) == mon.change.text),
                                 which(names(new.text) == mod.change.text)]
        nvz.type1.text <- "a new"
    }

    ## get text to explain current 95th percentile estiamtes
    if (tolower(mon.type) == "weibull/qr") {
        curr.est.text <- "The green bar represents the confidence intervals
        around the current 95^th^ percentile."
    } else  {curr.est.text <- "The left most dashed blue line represents
                               mid-2015. Where is it crosses the dark blue line is
                               the current 95^th^ percentile."
    }

    ## Markdown render call --------------------------------------------------------
    rmarkdown::render(input = "C:/Users/jdouglass/Desktop/NVZ_proformas/SW_2017_NVZ_proforma_markup.rmd",
                      output_format = "pdf_document",
                      output_file = paste0("Data_Sheet_", nvz.wb, ".pdf"),
                      output_dir = "C:/Users/jdouglass/Desktop/NVZ_proformas")
}
