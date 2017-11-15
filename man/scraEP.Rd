\name{scraEP}
\alias{scraEP}

\title{
Extract all (in-house) career information of MEPs from the European parliament's website.
}
\description{
This function downloads all career information from the European parliament's website <http://www.europarl.europa.eu> or from local html files, and extracts it into a data frame.
}
\usage{
scraEP(local.html= NA, save.html= NA, max.try= 20)
}
\arguments{
  \item{local.html}{a character string, indicating the local directory from which the MEP's html files are imported (existing files will be overwritten). If \code{NA} (default), the career information is downloaded from the EP's website.}
  \item{save.html}{a character string, indicating the local directory in which the downloaded html files should be saved. If \code{NA} (default), the downloaded files are not saved to local filesystem.}
  \item{max.try}{numeric: maximum number of tries to download html files from the EP's website.}
}
\value{
A data.frame, where each row is a career step recorded on the EP's website.
}
\details{
Downloading all the pages can take a long time, be sure to store the result in an object.
}
\author{
Julien Boelaert \email{jubo.stats@gmail.com}
}
