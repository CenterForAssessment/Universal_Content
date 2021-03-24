#####
###   Produce Demonstration Skip Year Analysis Report using `bookdown` and `pagedreport`
#####

###   Set up your R working directory
setwd("/Users/avi/Data/Demonstration/Skip_Year_Analysis/Documentation")

###   Load/Format/Subset Report Data
load("/Users/avi/Dropbox (SGP)/SGP/State_Alt_Analyses/Demonstration/Skip_Year_Analysis/Data/Demonstration_SGP_2018_2019_PART_2c.Rdata")
Report_Data <- data.table::copy(Demonstration_SGP_2018_2019_PART_2c@Data)[VALID_CASE == "VALID_CASE" & YEAR == '2018_2019' & CONTENT_AREA %in% c("READING", "MATHEMATICS") & SCHOOL_ENROLLMENT_STATUS == "Enrolled School: Yes"]

###   Load required packages
require(Literasee)
require(SGP)


#####
###    Render the report using `bookdown`
#####

bookdown::render_book(".", "bookdown::gitbook", config_file = "_bookdown_site.yml")

#  Serve the site directory on a local host to see the results:
servr::httw(dir = "site", watch = "site", port=4224)
servr::daemon_stop()


#####
###    Render the report using `pagedown`
#####

rmarkdown::render(file.path("report", "Demonstration_Skip_Year_SGP_Analyses.Rmd"))
pagedown::chrome_print(file.path("report", "Demonstration_Skip_Year_SGP_Analyses.html"), wait=10, timeout=60)

rmarkdown::render(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.Rmd"))
unlink(file.path("report", "_bookdown.yml")) #  Need to remove - seems to mess up subsequent attempts to re-render the `bookdown` site ...
pagedown::chrome_print(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.html"), wait=10, timeout=60) # , options = list(pageRanges='1-10') # last (blank) page issue fixed in dev pagedown varsion

###  Copy report to the bookdown site for download links
if (!file.exists(file.path("site", "downloads"))) dir.create(file.path("site", "downloads"))
file.copy(file.path("report", "Demonstration_Skip_Year_SGP_Analyses.pdf"), file.path("site", "downloads"), overwrite = TRUE)
file.copy(file.path("report", "Goodness_of_Fit_Plots_APPENDIX_A.pdf"), file.path("site", "downloads"), overwrite = TRUE)
