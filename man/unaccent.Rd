\name{unaccent}
\alias{unaccent}

\title{
Remove all accents from character strings.
}
\description{
This function removes all diacritic accents from character strings (eg. transforms "àüî" into "aui").
}
\usage{
unaccent(text)
}
\arguments{
  \item{text}{a character vector}
  }
\value{
A character vector, containing the unaccentuated version of the given text.
}
\author{
Julien Boelaert \email{jubo.stats@gmail.com}
}
\examples{
unaccent("âàéèïì")
}
