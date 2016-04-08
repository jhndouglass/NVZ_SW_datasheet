library(knitr)
library(markdown)
library(rmarkdown)
library(ggplot2)

##### Function defs #####
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

## knitr loop

## load table containing all NVZs
nvzs <- read.csv("tbl_Local_Workshop_Spreadsheet_Nov2015.csv",
                 stringsAsFactors = FALSE)
nvzs <- nvzs[1, ]

new.text <- read.csv("designation_new_text.csv", row.names = 1,
                     stringsAsFactors = FALSE)
names(new.text) <- c("improved", "remained stable", "deteriorated")

## load SEPERATE model bits and bobs
sep_local <- read.csv("seperate_local.csv")
sep_cumulative <- read.csv("seperate_cumulative.csv")
sep_cycle1 <- read.csv("seperate_cycle1_list.csv")

## necessary as spaces are removed on import - no  option to prevent this.

existing.text <- read.csv("designation_existing_text.csv", row.names = 1,
                          stringsAsFactors = FALSE)
names(existing.text) <- c("improved", "remained stable", "deteriorated")
## necessary as spaces are removed on import - no  option to prevent this.

for (nvz in unique(nvzs$Designating.Water.body)) {
    nvz.curr <- nvzs[nvzs$Designating.Water.body == nvz, ]
    nvz.name <- nvz.curr$Indicative.name
    nvz.id <- nvz.curr$Current.NVZ.Id
    #nvz.wb <- nvz.curr$Designating.Water.body
    nvz.wb <- "GB104029067520" # DELETE ME
    nvz.area <- 192.87 #!!!!!
    nvz.type1 <- "new" #!!!!!
    nvz.type2 <- "main river" # !!!!!
    nvz.type3 <- 2 # !!!!!
    nvz.prev.sw <- 100 #!!!!!!
    nvz.prev <- 100 #!!!!!!
    additional.info <- "No additional information included" #!!!!!!
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
    if (nvz.type2 == "main river") { #!!!!!!
        main.wb.scores.text <- "catchment scores"
        smpt.count <- 6 #!!!!!
        fail.count <- nvz.curr$X2015.number.of.failing.main.river.points
        fail.count.upstream <- 8 #!!!!!
        ifelse(fail.count > 1,
               fail.count.text <- paste0("are ", fail.count, " polluted sample points"),
               fail.count.text <- "is one polluted sample point")
        ifelse(fail.count.upstream > 1,
               fail.count.text.us <- fail.count.text.upstream,
               fail.count.text.us <- "")
        worst <- nvz.curr$X2015.Worst.main.river.monitoring.Point.Id
        worst.name <- nvz.curr$X2015.worst.main.river.monitoring.sample.point.name
        worst.graph <- paste0("SW TIN PNG Files/", worst, "TH.png" ) #!!!!!!
        neap.graph <- paste0("NEAPN_graphs/", nvz.wb,"_cycle", nvz.type3, "_catchment_neapn.png")
        modelling.map <- paste0("Modelling maps/", nvz.wb,
                                "_modelling_catchment_cycle", nvz.type3, "_map.png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- c("45212350MI", "45213020MI", "45214350MI", "45215500MI",
                       "45216170MI", "45217250MI", "45220500MI", "45221950MI")
        mult.graph <- paste0("SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- "Weibull" #!!!!!!!
        mon.2009 <- 6 #!!!!!
        mon.2013 <- 6 #!!!!!
        mon.2017 <- 6 #!!!!!
        mon.conf.text <- switch(as.character(mon.2017),
                                "1" = "highly confident that the water is not polluted",
                                "2" = "moderately confident that water is not polluted",
                                "3" = "somewhat confident that the water is not polluted",
                                "4" = "somewhat confident that the water is polluted",
                                "5" = "moderately confident that water is polluted",
                                "6" = "highly confident that the water is polluted")
        mod.2009 <- 5 #!!!!!
        mod.2013 <- 5 #!!!!!
        mod.2017 <- 4 #!!!!!
        mod.conf.text <- switch(as.character(mod.2017),
                             "1" = "highly confident that the water is not polluted",
                             "2" = "moderately confident that water is not polluted",
                             "3" = "somewhat confident that the water is not polluted",
                             "4" = "somewhat confident that the water is polluted",
                             "5" = "moderately confident that water is polluted",
                             "6" = "highly confident that the water is polluted")
        mon.conc <- 24.79 #!!!!!!!
        mon.conc.u95 <- 27.5 #!!!!!
        mon.conc.l95 <- 20.5 #!!!!!!
        mon.conc.u75 <- 25.5 #!!!!!
        mon.conc.l75 <- 21.5 #!!!!!!
        trend.2017 <- 6 #!!!!!
        trend.conc <- 23.47 #!!!!!!
        trend.conc.u95 <- 27.5 #!!!!!
        trend.conc.l95 <- 20.5 #!!!!!!
        trend.conc.u75 <- 25.5 #!!!!!
        trend.conc.l75 <- 21.5 #!!!!!!
        mod.conc <- 12.1 #!!!!!
        mod.conc.u95 <- 23.5 #!!!!!
        mod.conc.l95 <- 3.5 #!!!!!!
        mod.conc.u75 <- 17.5 #!!!!!
        mod.conc.l75 <- 8.5 #!!!!!!
        ps.load.min <- 11247458 #!!!!!!
        ps.load.max <- 27451981 #!!!!!!
        diff.urban.load.min <- 486252 #!!!!!!
        diff.urban.load.max <- 486252 #!!!!!!
        septic.load.min <- 64820 #!!!!!!
        septic.load.max <- 64820 #!!!!!!
        ag.load.min <- 9902332 #!!!!!!
        ag.load.max <- 15003533 #!!!!!!
        total.load.min <- 37905384 #!!!!!!
        total.load.max <- 26802063 #!!!!!!
        perc.ag.load.min <- 26 #!!!!!!
        perc.ag.load.max <- 56 #!!!!!!
    } else {
        main.wb.scores.text <- "waterbody scores"
        smpt.count <- 8 #!!!!!
        fail.count <- nvz.curr$X2015.number.of.failing.tributary.points
        fail.count.upstream <- 2 #!!!!!
        ifelse(fail.count > 1,
               fail.count.text <- "are multiple failures",
               fail.count.text <- "is one failure")
        ifelse(fail.count.upstream > 1,
               fail.count.text.us <- fail.count.text.upstream,
               fail.count.text.us <- "")
        worst <- nvz.curr$X2015.worst.tributary.sample.point.id
        worst.name <- nvz.curr$X2015.worst.tributary.sample.point.name
        worst.graph <- paste0("SW TIN PNG Files/", worst, "TH.png")
        neap.graph <- paste0("NEAPN_graphs/", nvz.wb,"_cycle", nvz.type3, "_wb_neapn.png")
        modelling.map <- paste0("Modelling maps/", nvz.wb,
                                "_modelling_wb_cycle", nvz.type3, "_map.png")
        ## A list of sample points needs to be included in main table driving
        ## reports.
        all.smpts <- c("PWRR0007TH", "PWRR0002TH")
        mult.graph <- paste0("SW TIN PNG Files/", all.smpts, ".png")
        mon.type <- "QR" #!!!!!!!
        mon.2009 <- 6 #!!!!!
        mon.2013 <- 6 #!!!!!
        mon.2017 <- 6 #!!!!!
        mon.conf.text <- switch(as.character(mon.2017),
                                "1" = "highly confident that the water is not polluted",
                                "2" = "moderately confident that water is not polluted",
                                "3" = "somewhat confident that the water is not polluted",
                                "4" = "somewhat very confident that the water is polluted",
                                "5" = "moderately confident that water is polluted",
                                "6" = "highly confident that the water is polluted")
        mod.2009 <- 5 #!!!!!
        mod.2013 <- 4 #!!!!!
        mod.2017 <- 4 #!!!!!
        mod.conf.text <- switch(as.character(mod.2017),
                                "1" = "highly confident that the water is not polluted",
                                "2" = "moderately confident that water is not polluted",
                                "3" = "somewhat confident that the water is not polluted",
                                "4" = "somewhat confident that the water is polluted",
                                "5" = "moderately confident that water is polluted",
                                "6" = "highly confident that the water is polluted")
        mon.conc <- 24.79 #!!!!!!!
        mon.conc.u95 <- 27.5 #!!!!!
        mon.conc.l95 <- 20.5 #!!!!!!
        mon.conc.u75 <- 25.5 #!!!!!
        mon.conc.l75 <- 21.5 #!!!!!!
        trend.2017 <- 6 #!!!!!
        trend.conc <- 23.47 #!!!!!!
        trend.conc.u95 <- 27.5 #!!!!!
        trend.conc.l95 <- 20.5 #!!!!!!
        trend.conc.u75 <- 25.5 #!!!!!
        trend.conc.l75 <- 21.5 #!!!!!!
        mod.conc <- 12.1 #!!!!!
        mod.conc.u95 <- 23.5 #!!!!!
        mod.conc.l95 <- 3.5 #!!!!!!
        mod.conc.u75 <- 17.5 #!!!!!
        mod.conc.l75 <- 8.5 #!!!!!!
        ps.load.min <- 11247458 #!!!!!!
        ps.load.max <- 27451981 #!!!!!!
        diff.urban.load.min <- 486252 #!!!!!!
        diff.urban.load.max <- 486252 #!!!!!!
        septic.load.min <- 64820 #!!!!!!
        septic.load.max <- 64820 #!!!!!!
        ag.load.min <- 9902332 #!!!!!!
        ag.load.max <- 15003533 #!!!!!!
        total.load.min <- 37905384 #!!!!!!
        total.load.max <- 26802063 #!!!!!!
        perc.ag.load.min <- 26 #!!!!!!
        perc.ag.load.max <- 56 #!!!!!!
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
    if (nvz.type1 == "existing") {
        nvz.des.text <- existing.text[which(row.names(existing.text) == mon.change.text),
                                      which(names(existing.text) == mod.change.text)]
        nvz.type1.text <- "an existing"
    } else {
        nvz.des.text <- new.text[which(row.names(new.text) == mon.change.text),
                                 which(names(new.text) == mod.change.text)]
        nvz.type1.text <- "a new"
    }

    ## get text to explain current 95th percentile estiamtes
    if (mon.type == "Weibull") {
        curr.est.text <- "The green bar represents the confidence intervals
        around the current 95^th^ percentile."
    } else  {curr.est.text <- "The left most dashed blue line represents
                               mid-2015. Where is it crosses the dark blue line is
                               the current 95^th^ percentile."
    }


    rmarkdown::render(input = "C:/Users/jdouglass/Desktop/NVZ_proformas/SW_2017_NVZ_proforma_markup.rmd",
                      output_format = "pdf_document",
                      output_file = paste0("Data_Sheet_", nvz.wb, ".pdf"),
                      output_dir = "C:/Users/jdouglass/Desktop/NVZ_proformas")
}
