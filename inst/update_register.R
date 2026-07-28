# Update the AAGI-CU IPPO Register
#

# for macOS only as the R_drive path will not work with Windows in this config
library(rippo)
library(fs)
sp <- "CU" # strategic partner for report
R_drive <- "/Volumes/dmp/A-J/AAGI_CCDM_CBADA-GIBBEM-SE21982/"

tl <- list_ippo_tables(
    dir_path_in = path(R_drive, "Projects"),
    sp = sp
)
create_ippo_report(
    tables_list = tl,
    outfile = "~/tmp/test_ippo.docx",
    sp = sp
)
