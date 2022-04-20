# this function overwrites the mig_net_count_age_sex sheet of the input excel file
#
# EXAMPLE
# mig_net_count_age_sex <- data.frame(time_start = rep(1950,131*2),
#                                     time_span = rep(1,131*2),
#                                     sex = c(rep(1,131),rep(2,131)),
#                                     age_start = rep(0:130,2),
#                                     age_span = rep(1,131*2),
#                                     value = rep(1000,131*2))
# write_mig_net_count_age_sex_to_excel(input_file_path = "C:/Users/SARAH/OneDrive - United Nations/WPP2021/MLT/test input excel files/PRT_R21.xlsx",
#                                      mig_net_count_age_sex = mig_net_count_age_sex)

#' @export
write_mig_net_count_age_sex_to_excel  <- function(input_file_path,
                                                  mig_net_count_age_sex) {

  # order the columns in the mig_net_count_age_sex table
  mig_net_count_age_sex <- mig_net_count_age_sex[,c("time_start",	"time_span",	"sex",	"age_start",	"age_span",	"value")]

  # load the input excel file
  wb <- openxlsx::loadWorkbook(file = input_file_path)

  # delete all of the existing data in the mig_net_count_age_sex sheet
  openxlsx::deleteData(wb, sheet = "mig_net_count_age_sex", cols = 1:6, rows = 2:1048576, gridExpand = TRUE)

  # write the new data into the mig_net_count_age_sex sheet
  openxlsx::writeData(wb, sheet = "mig_net_count_age_sex", x = mig_net_count_age_sex, startCol = 1, startRow = 2, colNames = FALSE)

  ## update_status worksheet
  update_status <- openxlsx::readWorkbook(xlsxFile = wb, sheet = "update_status")
  now <- format(Sys.time(), format="%Y-%m-%d %H:%M:%S")
  update_status$last_update[update_status$worksheet == "mig_net_count_age_sex"] <- now
  openxlsx::writeData(wb, sheet = "update_status", update_status$last_update, startCol=3, startRow=2, colNames = FALSE, rowNames=FALSE)

  # save the workbook
  openxlsx::saveWorkbook(wb, input_file_path, overwrite = TRUE)

}



