\name{xscrape}
\alias{xscrape}

\title{
Extract information from webpages to a data.frame, using XPath queries.
}
\description{
This function transforms an html page (or list of pages) into a data.frame, extracting nodes specified by their XPath.
}
\usage{
xscrape(pages, col.xpath, row.xpath= "/html", collapse= " | ", encoding= "UTF-8")
}
\arguments{
  \item{pages}{an object of class \code{XMLDocument} (as returned by function \code{htmlParse}), or list of such objects. Alternatively, a character vector containing the URLs of webpages to be parsed. These are the webpages that information is to be extracted from.}
  \item{col.xpath}{a character vector of XPath queries. Each element of this vector will be a column in the resulting data.frame. If the vector is named, these names are given to the result's.}
  \item{row.xpath}{(optional) a character string, containing an XPath query. This functions as an intermediary node: if specified, each result of this XPath query (on each page) becomes a row in the resulting data.frame. If not specified (default), the intermediary nodes are whole html pages, so that each page becomes a row in the result.}
  \item{collapse}{(optional) a character string, containing the separator that will be used in case a \code{col.xpath} query yields multiple results.}
  \item{encoding}{(optional) a character string, containing the encoding parameter that will be used by \code{htmlParse} if \code{pages} is a vector of URLs.}}
\value{
A data.frame, where each row corresponds to an intermediary node (either a full page or an XML node within a page, specified by \code{row.xpath}), and each column corresponds to the text of an \code{col.xpath} query.
}
\details{
If a \code{col.xpath} query designs a full node, only its text is extracted. If it designs an attribute (eg ends with '/@href' for weblinks), only the attribute's value is extracted.

If a \code{col.xpath} query matches no elements in a page, returned value is \code{NA}. If it matches multiple elements, they are concatenated into a single character string, separated by \code{collapse}.
}
\author{
Julien Boelaert \email{jubo.stats@gmail.com}
}
\examples{
## Extract all external links and their titles from a wikipedia page
data(wiki)
wiki.parse <- XML::htmlParse(wiki)
links <- xscrape(wiki.parse, 
                 row.xpath= "//a[starts-with(./@href, 'http')]", 
                 col.xpath= c(title= ".", link= "./@href"))

\dontrun{
## Convert results from a search for 'R' on duckduckgo.com
## First download the search page
duck <- XML::htmlParse("http://duckduckgo.com/html/?q=R")
## Then run xscrape on the dowloaded and parsed page
results <- xscrape(duck, 
                   row.xpath= "//div[contains(@class, 'result__body')]",
                   col.xpath= c(title= "./h2", 
                                snippet= ".//*[@class='result__snippet']", 
                                url= ".//a[@class='result__url']/@href"))
}

\dontrun{
## Convert results from a search for 'R' and 'Julia' on duckduckgo.com
## Directly provide the URLs to xscrape
results <- xscrape(c("http://duckduckgo.com/html/?q=R", 
                     "http://duckduckgo.com/html/?q=julia"), 
                   row.xpath= "//div[contains(@class, 'result__body')]",
                   col.xpath= c(title= "./h2", 
                                snippet= ".//*[@class='result__snippet']", 
                                url= ".//a[@class='result__url']/@href"))
}

}
