xscrape <- function(pages, col.xpath = ".", row.xpath = "/html", 
                      col.css = NULL, row.css = NULL, 
                      collapse = " | ", encoding = NULL, 
                      page.name = TRUE, nice.text = TRUE, parallel = 0, 
                      engine = c("auto", "XML", "xml2")) {
  ## Select engine : XML or xml2
  engine <- match.arg(engine)
  if (engine == "auto") {
    engine <- "xml2"
    if (is.list(pages)) {
      if (inherits(pages[[1]], "XMLInternalDocument")) engine <- "XML"
    } else if (inherits(pages, "XMLInternalDocument")) engine <- "XML"
  }
  if (engine == "XML" & length(c(col.css, row.css)) > 0) {
    col.css <- NULL
    row.css <- NULL
    warning("Using XML engine, CSS selectors will be ignored. Select xml2 \
    engine to use CSS selectors.")
  }
  if (engine == "xml2") {
    if (isTRUE(nice.text)) {
      textfun <- rvest::html_text2
    } else textfun <- function(x) rvest::html_text(x, trim = TRUE)
  }
  
  if (is.null(encoding)) {
    if (engine == "XML") encoding <- character()
    if (engine == "xml2") encoding <- ""
  }
  
  ## Don't use row.xpath if row.css is set
  if (!is.null(row.css)) row.xpath <- NULL
  
  ## Handle parallel cores
  if (parallel == 0) 
    parallel <- parallel::detectCores()
  
  ## Handle pages : coerce to XML/xml2 doc to list, download if necessary
  if (inherits(pages, "xml_document") | 
      inherits(pages, "XMLInternalDocument")) {
    pages <- list(pages)
  } else {
    if (is.list(pages)) {
      all.xml <- all(sapply(pages, function(x) 
        inherits(x, "xml_document") | inherits(x, "XMLInternalDocument")))
      if (!all.xml) {
        stop("If pages is a list, all its elements must be xml/html documents \
             of class XMLInternalDocument or xml_document, \
             as created eg. by `XML::htmlParse` or `xml2::read_html`.")
      }
    } else if (is.character(pages)) {
      cat("\nImporting ", length(pages), " pages:\n")
      prog <- utils::txtProgressBar(max = length(pages), style = 3)
      tmp <- list()
      for (idoc in 1:length(pages)) {
        if (engine == "XML") {
          tmp[[idoc]] <- XML::htmlParse(pages[idoc], encoding = encoding)
        } else 
          tmp[[idoc]] <- xml2::read_html(pages[idoc], encoding = encoding)
        utils::setTxtProgressBar(prog, idoc)
      }
      names(tmp) <- pages
      pages <- tmp
    } else {
      stop("pages must be an object of class xml_document or \
        XMLInternalDocument (as created eg. by `XML::htmlParse` or\
        `xml2::read_html`), a list of such objects, \
        or a character vector containing URLs (or paths to local files).")
    }
  }
  
  ## Convert pages between XML and xml2 according to engine choice
  if (engine == "XML" & inherits(pages[[1]], "xml_document")) {
    pages <- parallel::mclapply(mc.cores = parallel, pages, function(x) {
      letempfile <- tempfile(fileext = ".xml")
      xml2::write_xml(x, letempfile)
      XML::htmlParse(letempfile)
    })
  } else if (engine == "xml2" & inherits(pages[[1]], "XMLInternalDocument")) {
    pages <- lapply(pages, function(x) xml2::read_html(XML::saveXML(x)))
  }
  
  ## Ensure pages has names
  if (is.null(names(pages)))
    names(pages) <- as.character(1:length(pages))

  ## Ensure col.xpath and col.css have names  
  if (!is.null(col.xpath)) if (is.null(names(col.xpath))) 
    names(col.xpath) <- paste0("var", 1:length(col.xpath))
  if (!is.null(col.css)) if (is.null(names(col.css))) 
    names(col.css) <- paste0("var", length(col.xpath) + 1:length(col.css))
  
  ## Parallelisation scheme
  if (length(pages) >= parallel) {
    para.pages <- parallel
    para.inside <- 1
  } else {
    para.pages <- 1
    para.inside <- parallel
  }
  
  ## Triple loop for clean extraction
  if (engine == "XML") {
    loopres <- parallel::mclapply(mc.cores = para.pages,
      seq_along(pages), function(i.page) {
        la.page <- pages[[i.page]]
        nodes <- try(XML::xpathApply(la.page, row.xpath))
        if (inherits(nodes, "try-error"))
          stop("Error in row xpath extraction, check that xpath is correct.")
        res <- data.table::rbindlist(
          parallel::mclapply(mc.cores = para.inside, nodes, function(le.node) {
            brut <- try(lapply(col.xpath, function(le.xpath) XML::xpathSApply(
              le.node, le.xpath, function(x) 
                if (inherits(x, "XMLAttributeValue")) x else XML::xmlValue(x))))
            as.data.frame(lapply(brut, function(vect) {
              if (!length(vect)) { NA }
              else if (length(vect) > 1) { paste(vect, collapse = collapse) }
              else vect
            }))
          }))
        if (page.name & nrow(res) > 0) 
          res <- data.frame(page = names(pages)[i.page], res)
        res
      })
  } else {
    loopres <- parallel::mclapply(mc.cores = para.pages,
      seq_along(pages), function(i.page) {
        la.page <- pages[[i.page]]
        if (is.null(row.xpath)) {
          nodes <- try(rvest::html_elements(la.page, css = row.css))
          if (inherits(nodes, "try-error"))
            stop("Error in row css extraction, check that css selector is correct.")
        } else {
          nodes <- try(rvest::html_elements(la.page, xpath = row.xpath))
          if (inherits(nodes, "try-error"))
            stop("Error in row xpath extraction, check that xpath is correct.")
        }
        res <- data.table::rbindlist(
          parallel::mclapply(mc.cores = para.inside, nodes, function(le.noeud) {
            brutxpath <- try(lapply(col.xpath, function(le.xpath) 
              textfun(rvest::html_elements(le.noeud, xpath = le.xpath))))
            brutcss <- try(lapply(col.css, function(le.css) 
              textfun(rvest::html_elements(le.noeud, css = le.css))))
            as.data.frame(lapply(c(brutxpath, brutcss), function(vect) {
              if (!length(vect)) { NA }
              else if (length(vect) > 1) { paste(vect, collapse = collapse) }
              else vect
            }))
          }))
        if (page.name & nrow(res) > 0) 
          res <- data.frame(page = names(pages)[i.page], res)
        res
      })
  }
  as.data.frame(data.table::rbindlist(loopres))
}
