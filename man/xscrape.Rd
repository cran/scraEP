\name{xscrape}
\alias{xscrape}

\title{
Extract information from webpages to a data.frame, using XPath or CSS queries.
}
\description{
This function transforms an html/xml page (or list of pages) into a data.frame, extracting nodes specified by their XPath.
}
\usage{
xscrape(pages, 
        col.xpath = ".", row.xpath = "/html", 
        col.css = NULL, row.css = NULL, 
        collapse = " | ", encoding = NULL, 
        page.name = TRUE, nice.text = TRUE, 
        parallel = 0, 
        engine = c("auto", "XML", "xml2"))
}
\arguments{
  \item{pages}{an object of class \code{XMLInternalDocument} or \code{xml_document} (as returned by functions \code{XML::htmlParse} or \code{xml2::read_html} or \code{rvest::read_html}), or list of such objects. Alternatively, a character vector containing the URLs or local paths of webpages to be parsed. These are the webpages that information is to be extracted from. If the provided list or vector is named, its names will be used to indicate data provenance when \code{page.name} is TRUE.}
  \item{col.xpath}{a character vector of XPath queries used for creating the result columns. If the vector is named, these names are given to the columns. The default "." takes the text from the whole of each page or intermediary node (specified by \code{row.xpath} or \code{row.css}).}
  \item{row.xpath}{a character string, containing an XPath query for creating the result rows. The result of this query (on each page) becomes a row in the resulting data.frame. If not specified (default), the intermediary nodes are whole html pages, so that each page becomes a row in the result.}
  \item{col.css}{same as \code{col.xpath}, but with CSS selectors instead of XPath queries. If \code{col.xpath} was also given, the XPath columns will be placed before the CSS columns in the result.}
  \item{row.css}{same as \code{row.xpath}, but with a CSS selector instead of an XPath query. If given, this will be used instead of \code{row.xpath}.}
  \item{collapse}{a character string, containing the separator that will be used in case a \code{col.xpath} query yields multiple results within a given intermediary node. The default is " | ".}
  \item{encoding}{ a character string (eg. "UTF-8" or "ISO-8859-1"), containing the encoding parameter that will be used by \code{htmlParse} or \code{read_html} if \code{pages} is a vector of URLs or local file names.}
  \item{page.name}{a logical. If TRUE, the result will contain a column indicating the name of the page each row was extracted from. If \code{pages} has no names, they will be numbered from 1 to \code{length(pages)}}
  \item{nice.text}{a logical. If TRUE (only possible with engine xml2), the rvest::html_text2 function is used to extract text into the result, often making the text much cleaner. If FALSE, the function runs faster, but the text might be less clean.}
  \item{parallel}{a numeric, indicating the number of cores to use for parallel computation. The default 0 takes all available cores. The parallelization is done on the pages if their number is greater than the number of provided cores, otherwise it is done on the intermediary nodes. Note that parallelization relies on parallel::mclapply, and is thus not supported on Windows systems.}
  \item{engine}{a character string, indicating the engine to use for data extraction: either "XML", "xml2", or "auto" (default). The default will adapt the engine to the type of \code{pages}, or will use "xml2" if \code{pages} are URLs or file names. Note: CSS selectors and \code{nice.text} are only available for "xml2", but XPath queries and "XML" engine tend to be much faster.}
  }
\value{
A data.frame, where each row corresponds to an intermediary node (either a full page or an XML node within a page, specified by \code{row.xpath} or \code{row.css}), and each column corresponds to the text of a \code{col.xpath} or \code{col.css} query.
}
\details{
If a \code{col.xpath} or \code{col.css} query designs a full node, only its text is extracted. If it designs an attribute (eg. ends with '/@href' for weblinks), only the attribute's value is extracted.

If a \code{col.xpath} or \code{col.css} query matches no elements in a page, returned value is \code{NA}. If it matches multiple elements, they are concatenated into a single character string, separated by \code{collapse}.
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
                 col.xpath= c(title= ".", link= "./@href"), 
                 parallel = 1)

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
