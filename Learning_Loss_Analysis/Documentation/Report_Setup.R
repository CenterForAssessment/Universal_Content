#####
###   Set up the Demonstration Learning Loss Analysis Report directory
###   Copy Literasee package assets, custom content templates, update packages
###   and create/customize/complete the required YAML and RMD file config lists
#####

###   Set up your R working directory
setwd("... /Documentation")

###   Install/update (if needed) the latest packages
remotes::install_github("centerforassessment/Literasee")
remotes::install_github('rstudio/rmarkdown')
remotes::install_github('rstudio/bookdown')
remotes::install_github('rstudio/pagedown')

require(Literasee)

###   Set up new report directory
###   An initial setup will include copying the Literasee package assets and
###   any custom RMD templates (from "Universal_Content" or another, similar, state)

template.path <- file.path("..", "..", "..", "Universal_Content", "Learning_Loss_Analysis", "Child_RMD", "Template_Custom_Content")
setupReportDirectory(custom.content.path = template.path)

###   It may be necessary to occasionally update Literasee package assets.
# updateAssets()

###   Alternatively, one can update custom content and/or Literasee assets
# setupReportDirectory(new.report = FALSE, update.assets = TRUE, custom.content.path = template.path, overwrite.custom=FALSE)


###
###   Get Report Config and RMD file lists -- merge custom and universal config lists
###

###  The "custom.config" list with mainly be to supply client/state info.
###  It can also be used to override some of the Universal settings (authors, etc.)

custom.config <- list(
  client.info = list(
    state.name="Demonstration COVID", # required at a minimum
    # state.abv = "DEMO", # Added manually in Report_Configs.R -- need to fix getStateAbbreviation for DEMO/DEMO_COVID!
    city.name = "Washington",
    organization = "Demonstration Department of Education",
    org.head = "Joseph R. Biden, Jr.",
    github.repo = "CenterForAssessment/SGP_Research/tree/master/Demonstration/Learning_Loss_Analysis",
    acknowledgements="the entire staff of the DDoE Assessment and Accountability Office, and particularly Maggie Q. Public,"
  ),
  # Override defaults for author/Affil
  top.level=list(             # Title/subtitle, author.names, author.affil, date
    title = "Example Academic Impact Analysis",
    subtitle = "Student Achievement and Growth during the COVID-19 Pandemic"
  ),
  params = list(
    draft.text = "DRAFT REPORT -- DO NOT CITE", #
    keyword = "academic impact" # should be lower case.  Camel applied as needed in params.Rmd or can be customized as keyword_camel
  )
)

##   The following script will merge the report.config (universal) and custom.config lists and return 'report.config' to be used in next steps
source(file.path("..", "..", "..", "Universal_Content", "Learning_Loss_Analysis", "Meta_Data", "Report_Configs.R"))

##   The following script will merge the rmd.files (universal) and custom.files lists and return 'rmd.files' to be used in next steps
# custom.files <- list(...) # override defaults if desired.  Otherwise a message that universal list will be used.
source(file.path("..", "..", "..", "Universal_Content", "Learning_Loss_Analysis", "Meta_Data", "Report_Content.R"))

###   Save report YAML and file configurations
save(list=c("report.config", "rmd.files"), file = "Report_Configuration_MetaData.rda")


###    Create the .yml and .Rmd "master/parent" documents for the `bookdown` site and `pagedown` report
createReportScripts(report_config=report.config, rmd_file_list=rmd.files)
