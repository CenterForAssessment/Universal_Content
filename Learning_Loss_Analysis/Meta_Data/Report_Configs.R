################################################################################
#####                                                                      #####
###  Report configuration list creation -- merge universal and custom lists  ###
#####                                                                      #####
################################################################################

###   Universal (generic) report configuration elements:

report.config <- list(
  top.level=list(             # Title/subtitle, author.names, author.affil, date
    # title = "Demonstration COVID Learning Loss Analysis", auto configured if NULL based on client state.name
    # subtitle = "Academic Impact During the COVID-19 Pandemic", # auto configured if NULL
    author.names = c("Damian W. Betebenner",  "Adam R. VanIwaarden", "Nathan Dadey"),
    # author.affil = c("Center for Assessment", "Center for Assessment"), # Only needed if different affiliations. - Could also just be single "Center for Assessment"
    # date = "March 2021",  #  auto configured to MONTH YEAR format
    executive.summary = TRUE,
    draft = TRUE,
    project.team = "SGP Team",
    project.email = "anyone@nciea.org"
  ),
  ###   client.info to be customized for each state.
  client.info=c(), # placeholder for list order
  params = list(
    # draft.text = "ALTERNATE DRAFT TEXT", #  auto configured to 'DRAFT REPORT' if report.config$top.level$draft = TRUE
    base.directory = getwd(),
    unvrsl.rmd.path = file.path("..", "..", "..", "Universal_Content", "Learning_Loss_Analysis", "Child_RMD"),
    custom.rmd.path = file.path("assets", "rmd", "Custom_Content")
  )
)

###   Modify/merge together the custom.config and report.config lists

if (!exists("custom.config")) {
  message("\n\tNo 'custom.config' list exists in your current environment.  The universal ('report.config') list will be returned unchanged (without report.config$client.info).\n")
  custom.config <- list()
} else {
  report.config <- modifyList(report.config, custom.config)
}

###   Set prelim params - "state.name" required at a minimum

if (is.null(report.config$params$state.name)) {
  report.config$params$state.name <- report.config$client.info$state.name
}
if (is.null(report.config$client.info$state.name)) {
  report.config$client.info$state.name <- report.config$params$state.name
}
if (is.null(report.config$params$state.name)) {
  stop("Full 'State' name must be provided at a minimum in either 'report.config$params$state.name' or 'report.config$client.info$state.name'.")
}

if (!is.null(report.config$top.level$draft)) {
  if (is.logical(report.config$top.level$draft)) {
    if (report.config$top.level$draft & is.null(report.config$params$draft.text)) {
      report.config$params$draft.text <- "DRAFT REPORT -- DO NOT CITE"
    }
  }
}

###   Add common top-level YAML fields

if (is.null(report.config$top.level$title)) {
  report.config$top.level$title <- paste(report.config$params$state.name, "Learning Loss Analysis")
}
if (is.null(report.config$top.level$subtitle)) {
  report.config$top.level$subtitle <- "Academic Impact During the COVID-19 Pandemic"
}

if (is.null(report.config$top.level$executive.summary)) {
  report.config$top.level$executive.summary <- FALSE
} else {
  if (!is.logical(report.config$top.level$executive.summary)) {
    report.config$top.level$executive.summary <- TRUE
  }
}

if (is.null(report.config$top.level$bibliography)) {
  report.config$top.level$bibliography <- system.file("rmarkdown", "content", "bibliography", "Literasee.bib" , package = "Literasee")
}

##    Client Info
if (is.null(report.config$client.info$state.abv)){
  if (toupper(report.config$client.info$state.name)=="DEMONSTRATION COVID") {
    report.config$client.info$state.abv <- "DEMO_COVID"
  } else {
    report.config$client.info$state.abv <- datasets::state.abb[datasets::state.name == report.config$client.info$state.name]
    # getStateAbbreviation(report.config$client.info$state.name, type="Abbreviation") # Still broken for DEMO and may work in all cases...
  }
}
