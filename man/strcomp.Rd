\name{strcomp}
\alias{strcomp}

\title{
Compare the contents of two vectors of character strings.
}
\description{
This function compares the contents of two vectors of character strings: how many elements are present in both vectors, how many are only present in each one, etc., and lists the elements that are only present in one of the vectors.
}
\usage{
strcomp(text1, text2)
}
\arguments{
  \item{text1, text2}{two character vectors to be compared}
  }
\value{
A list containing the following elements: 

\code{matchTable}: a cross table of all the unique elements of the concatenation of text1 and text2, indicating whether they belong to text1 and/or to text2.

\code{matchOneInTwo}: a table of the elements of text1 according to how many times they are present in text2.

\code{matchTwoInOne}: a table of the elements of text2 according to how many times they are present in text1.

\code{tabOneInTwo}: a table of the elements of text1 according to whether they are present in text2.

\code{tabTwoInOne}: a table of the elements of text2 according to whether they are present in text1.

\code{oneNotInTwo}: a vector containing the elements of text1 not present in text2.

\code{twoNotInOne}: a vector containing the elements of text2 not present in text1.
}
\author{
Julien Boelaert \email{jubo.stats@gmail.com}
}
\examples{
str1 <- c("Alice", "Alice", "Bob", "Carol")
str2 <- c("Bob", "Denise", "Emerson", "Foteini")
strcomp(str1, str2)
}
