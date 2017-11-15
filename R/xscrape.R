## xscrape : make a data.frame from a list of parsed html/xml pages with XPath
xscrape <- function(pages, col.xpath, row.xpath= "/html", collapse= " | ") {
  if (inherits(pages, "XMLInternalDocument"))
    pages <- list(pages)
  if (!is.list(col.xpath)) 
    col.xpath <- as.list(col.xpath)
  if (is.null(names(col.xpath)))
    names(col.xpath) <- paste0("var", 1:length(col.xpath))
  do.call(rbind, lapply(pages, function(la.page) {
    nodes <- xpathApply(la.page, row.xpath) 
    do.call(rbind, lapply(nodes, function(le.noeud) {
      brut <- lapply(col.xpath, function(le.xpath) 
        xpathSApply(le.noeud, le.xpath, function(x) if ("XMLAttributeValue" %in% class(x)) { x } else xmlValue(x)))
      as.data.frame(lapply(brut, function(vect) { 
        if (!length(vect)) { NA } else if (length(vect) > 1) { paste(vect, collapse= collapse) } else vect
      }))
    }))
  }))
}
