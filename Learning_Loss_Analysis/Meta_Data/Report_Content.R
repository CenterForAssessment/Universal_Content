################################################################################
#####                                                                      #####
###    List and order of child .Rmd files to be used in report/appendices    ###
#####                                                                      #####
################################################################################

rmd.files <- list(
  report = list(
    file.order = c(
      "setup.Rmd",
      "params.Rmd",
      "0_Executive_Summary.Rmd",
      "1_Intro__Overview.Rmd",
      "1_Intro_Background.Rmd",
      "1_Intro_Methods.Rmd",
      "1_Intro_Data_Sources.Rmd",
      "2_Participate__Analysis.Rmd",
      "2_Participate__Overview.Rmd",
      "2_Participate_Counts.Rmd",
      "2_Participate_Mode_of_Instruction.Rmd",
      "2_Participate_Attendance.Rmd",
      "2_Participate_School_Closures.Rmd",
      "3_Impact__Overview.Rmd",
      "3_Impact_Achievement_Analysis.Rmd",
      "3_Impact_Achievement_Overview.Rmd",
      "3_Impact_Growth_Analysis.Rmd",
      "3_Impact_Growth_Overview.Rmd",
      "3_Impact_Synthesis.Rmd",
      "4_Discussion__Overview.Rmd",
      "9_Summary.Rmd"
    ),
    references = TRUE
  ),
  appendices = list(
    A = list(
      title = "Goodness of Fit Plots",
      file.order = c(
        "setup_appendix_a.Rmd",   #  Should be appendix specific (counter override, etc.)
        "params.Rmd",  #  Could be appendix specific - params_appendix_a.Rmd
        "Appendix_A_Intro.Rmd" #,
        # "Appendix_A_Grade_Level.Rmd"
      ),
      references = NULL
    )
  ),
  bookdown = list(
    rmd.path = file.path("assets", "rmd", "bookdown"),
    report = list(
      file.order = c()
    )
  ),
  pagedown = list(
    rmd.path = c(),
    report = list(
      file.order = c(1:4, 6, 5, 7:20)
    )
  )
)

###   Modify/merge together the custom.files and report.config lists
if (!exists("custom.files")) {
  message("\n\tNo 'custom.files' list exists in your current environment.  The universal ('rmd.files') list will be returned.\n")
  custom.files <- list()
} else {
  rmd.files <- modifyList(rmd.files, custom.files)
}

rmd.files$report.source$custom <-
  match(list.files(report.config$params$custom.rmd.path), rmd.files$report$file.order)
if (all(is.na(rmd.files$report.source$custom))) {
  rmd.files$report.source$custom <- NULL
} else {
  if (any(is.na(rmd.files$report.source$custom))) {
    message("\n\t", list.files(report.config$params$custom.rmd.path)[which((is.na(rmd.files$report.source$custom)))], " is located in the customized content directory, but does not appear in the report 'file.order'\n")
  }
}

rmd.files$report.source$universal <-
  setdiff((1:length(rmd.files$report$file.order)), rmd.files$report.source$custom)
if (all(is.na(rmd.files$report.source$universal))) {
  rmd.files$report.source$universal <- NULL
}

if (!is.null(rmd.files$appendices)) {
  for (apdx in seq(length(rmd.files$appendices))) {
    rmd.files$appendices[[apdx]]$file.source$custom <-
      match(list.files(report.config$params$custom.rmd.path), rmd.files$appendices[[apdx]]$file.order)
    if (all(is.na(rmd.files$appendices[[apdx]]$file.source$custom))) {
      rmd.files$appendices[[apdx]]$file.source$custom <- NULL
    }
    rmd.files$appendices[[apdx]]$file.source$universal <-
      setdiff((1:length(rmd.files$appendices[[apdx]]$file.order)), rmd.files$appendices[[apdx]]$file.source$custom)
    if (all(is.na(rmd.files$appendices[[apdx]]$file.source$universal))) {
      rmd.files$appendices[[apdx]]$file.source$universal <- NULL
    }
  }
}
