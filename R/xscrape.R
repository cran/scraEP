## xscrape : make a data.frame from a list of parsed html/xml pages with XPath
xscrape <- function(pages, col.xpath, row.xpath= "/html", collapse= " | ", 
                    encoding= "UTF-8") {
  ## Argument `pages` can be : 
  ## 1) a vector of URL addresses, to be passed to XML::htmlParse
  ## 2) a parsed html/xml object, or a list of such objects
  if (! is.list(pages)) { 
    if (length(pages) == 1) {
      if (!inherits(pages, "XMLInternalDocument")) {
        tmp.down <- list(htmlParse(pages, encoding= encoding))
        names(tmp.down) <- pages
        pages <- tmp.down
      } else 
        pages <- list(pages)
    } else {
      if (!inherits(pages[1], "XMLInternalDocument")) {
        tmp.down <- lapply(pages, htmlParse, encoding= encoding)
        names(tmp.down) <- pages
        pages <- tmp.down
      } else 
        pages <- list(pages)
    }
  } else if (!inherits(pages[[1]], "XMLInternalDocument")) 
    stop("If `pages` is a list, it must contain parsed html or xml documents.")

  if (!is.list(col.xpath)) 
    col.xpath <- as.list(col.xpath)
  if (is.null(names(col.xpath)))
    names(col.xpath) <- paste0("var", 1:length(col.xpath))
  
  as.data.frame(rbindlist(lapply(pages, function(la.page) {
    nodes <- xpathApply(la.page, row.xpath) 
    rbindlist(lapply(nodes, function(le.noeud) {
      brut <- lapply(col.xpath, function(le.xpath) 
        xpathSApply(le.noeud, le.xpath, function(x) if ("XMLAttributeValue" %in% class(x)) { x } else xmlValue(x)))
      as.data.frame(lapply(brut, function(vect) { 
        if (!length(vect)) { NA } else if (length(vect) > 1) { paste(vect, collapse= collapse) } else vect
      }))
    }))
  })))
}
