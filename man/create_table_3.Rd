% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_table_3.R
\name{create_table_3}
\alias{create_table_3}
\title{Create a Table of Third Party IP (Table 3) of the R Packages and Quarto Template Used for an AAGI IPPO Document}
\usage{
create_table_3(
  project_path = getwd(),
  file_path = "1 Documentation",
  quarto = FALSE,
  digger = FALSE
)
}
\arguments{
\item{project_path}{The path to the directory of the project to generate the
tables for, defaults to the current working directory and recurses into
subfolders.}

\item{file_path}{The path to the directory where the table will be saved,
defaults to the project's "1 Documentation" directory.}

\item{quarto}{Logical, whether to cite the \acronym{AAGI} Quarto template.
Defaults to \code{FALSE}.}
}
\value{
nothing, called for its side effects of writing an Excel file to disk
for inclusion in the IPPO document spreadsheet.
}
\description{
Create a Table of Third Party IP (Table 3) of the R Packages and Quarto Template Used for an AAGI IPPO Document
}
\examples{
create_table_3()

}
\author{
Zhanglong Cao, \email{zhanglong.cao@curtin.edu.au} and Adam H.
Sparks, \email{adam.sparks@curtin.edu.au}
}
