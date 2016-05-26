## OPENING STATEMENTS-----------------------------------------------------------
################################################################################
## Author: John Douglass - ARE
## Date: May 2016
## Version: 0.1
################################################################################
## File Description;
## Purpose: Generate docs with graphs for GW datasheets
##
## Inputs: list of sample points and NVZ IDs
##
## Outputs: .docx documners
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

## EXECUTED STATEMENTS----------------------------------------------------------
smpts <- read.csv("smpts.csv", stringsAsFactors = FALSE)
#smpts <- smpts[smpts$NVZ_ID == 3, ]


## knitr loop ------------------------------------------------------------------
for (nvz in unique(smpts$NVZ_ID)) {
    nvz.curr <- smpts[smpts$NVZ_ID == nvz, ]
    all.smpts <- nvz.curr$siteID
    all.smpts.short <- substr(all.smpts, 0, nchar(all.smpts) - 2)
    #all.smpts.names <- smpts.curr$smpt_short_name
    mult.graph <- paste0("GW TIN 45 PNG Files/", all.smpts, ".png")

    ## Markdown render call --------------------------------------------------------
    render(input = paste0(getwd(), "/GW_gen.rmd"),
           output_format = "word_document",
           output_file = paste0("graphs_", nvz, ".docx"),
           output_dir = paste0(getwd(), "/out"))
}
