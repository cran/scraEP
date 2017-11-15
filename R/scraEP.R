## scraEP : scrape all in-house carreer information from the European 
## Parliament's website

scraEP <- function(local.html= NA, save.html= NA, max.try= 20) {
  if (! is.na(save.html)) {
    save.html <- gsub("/$", "", save.html)
    if (!file.exists(save.html))
      stop("Specified 'save.html' directory does not exist.")
  }
    
  if (is.na(local.html)) {
    ## Download table of contents for individual MEP links
    cat("Downloading table of contents:")
    dl.ok <- F
    for (i.try in 1:max.try) {
      toc <- try(htmlParse("http://www.europarl.europa.eu/meps/en/directory.html?filter=all&leg=0"), silent= T)
      if (! "try-error" %in% class(toc)) {
        dl.ok <- T
        break
      }
    }
    if (!dl.ok)
      stop("Impossible to download the file in 'max.try' tries.")
    
    toc.links <- xpathSApply(toc, "//li[@class='mep_name']/a/@href")
    toc.links <- paste0("http://www.europarl.europa.eu", 
                        gsub("_home.html$", "_history.html", toc.links))
    cat(" done.\n")
    
    ## Download individual career pages
    pages <- lapply(seq_along(toc.links), function(i) {
      cat(paste0("\rDownloading individual career pages: ", i, " / ", length(toc.links), "."))
      dl.ok <- F
      for (i.try in 1:max.try) {
        tmp.page <- try(getURL(toc.links[i]), silent= T)
        if (! "try-error" %in% class(tmp.page)) {
          dl.ok <- T
          break
        }
      }
      if (!dl.ok)
        stop("Impossible to download the file in 'max.try' tries.")
      if (!is.na(save.html)) {
        writeLines(tmp.page, paste0(save.html, "/", gsub(".*/", "", toc.links[i])))
      }
      htmlParse(tmp.page)
    })
    cat("\n")
  } else {
    cat("Importing local html files: ")
    toc.links <- dir(local.html)
    local.html <- gsub("/$", "", local.html)
    if(!file.exists(local.html))
      stop("Specified 'local.html' directory does not exist.")
    pages <- lapply(paste0(local.html, "/", dir(local.html)), htmlParse)
    cat("done.\n")
  }

  ## Extract career info
  cat("Extracting career information:")
  names(pages) <- toc.links
  mep <- xscrape(pages, 
                 row.xpath= "//div[@id='content_left']//li[count(*)=0]",
                 col.xpath= c(name= "//img[@class='photo_mep']/@title",
                              country= "//div[@class='zone_info_mep_transparent']//li[@class='nationality noflag']",
                              current.group= "//div[@class='zone_info_mep_transparent']//li[contains(@class, 'group')]",
                              current.party= "//div[@class='zone_info_mep_transparent']//span[@class='name_pol_group']",
                              moreinfo= "//div[@class='zone_info_mep_transparent']//span[@class='more_info']",
                              header= "./../preceding-sibling::*[1]", 
                              activity= "."))
  mep$path <- gsub("[.]html.*$", ".html", rownames(mep))
  rownames(mep) <- NULL
  cat(" done.\n")
  
  ## Clean up the table
  cat("Cleaning up the table:")
  mep$ID <- as.numeric(gsub("(\\d+)-.*$", "\\1", mep$path))
  mep$current.group <- gsub("\\r|\\n|\\t", "", mep$current.group)
  mep$country <- gsub("\\r|\\n|\\t", "", mep$country)
  mep$current.party <- as.character(mep$current.party)
  for (i_row in 1:nrow(mep)) 
    mep$country[i_row] <- gsub(mep$current.party[i_row], "", mep$country[i_row])
  mep$moreinfo <- gsub("\\r|\\n|\\t", "", mep$moreinfo)
  mep$activity <- gsub("\\r|\\n|\\t", "", mep$activity)
  
  mep$birth.date <- gsub(".*birth: (\\d+ \\w+ \\d{4}).*", "\\1", mep$moreinfo)
  for (i in 1:12) mep$birth.date <- gsub(month.name[i], paste(i), mep$birth.date)
  mep$birth.date <- as.Date(mep$birth.date, "%d %m %Y")
  
  mep$birth.place <- gsub(".*birth: \\d+ \\w+ \\d{4},? ?(.*)", "\\1", mep$moreinfo)
  mep$birth.place <- gsub(" \\| .*", "", mep$birth.place)
  mep$birth.place[mep$birth.place == ""] <- NA
  
  mep$death.date <- gsub(".*death: (\\d+ \\w+ \\d{4}).*", "\\1", mep$moreinfo)
  for (i in 1:12) mep$death.date <- gsub(month.name[i], paste(i), mep$death.date)
  mep$death.date <- as.Date(mep$death.date, "%d %m %Y")
  
  mep$start <- as.Date(gsub("^([0-9.]{10}).*", "\\1", mep$activity), "%d.%m.%Y")
  mep$end <- as.Date(gsub("^[0-9.]{10} / ([0-9.]{10}).*", "\\1", mep$activity), "%d.%m.%Y")
  mep$end[grep("[0-9.]{10} [.]{3}", mep$activity)] <- NA
  mep$detail <- gsub("^[0-9./ ]*:(.*)$", "\\1", mep$activity)
  
  mep$status <- as.character(mep$header)
  mep$status[mep$status == "Political groups"] <- 
    gsub("^.* - (.*)", "\\1", mep$detail[mep$status == "Political groups"])
  mep$status[mep$header == "Political groups" & mep$status == ""] <- "Member"
  mep$status[mep$status == "National parties"] <- NA
  
  mep$context <- as.character(mep$header)
  mep$context[grep("Committee", mep$detail)] <- "Committee"
  mep$context[grep("Subcommittee", mep$detail)] <- "Subcommittee"
  mep$context[grep(" committee", mep$detail)] <- "Temp. Committee"
  mep$context[grep("^Delegation", mep$detail)] <- "Delegation"
  mep$context[grep("ACP", mep$detail)] <- "ACP"
  mep$context[grep("Quaestors", mep$detail)] <- "Quaestors"
  mep$context[mep$detail == "European Parliament"] <- "European Parliament"
  mep$context[mep$detail == "Parliament's Bureau"] <- "Parliament's Bureau"
  mep$context[mep$detail == "Conference of Presidents"] <- "Conference of Presidents"
  mep$context[mep$detail == "Conference of Delegation Chairs"] <- "Conference of Delegation Chairs"
  mep$context[mep$detail == ""] <- "Unknown"
  
  mep$full.info <- paste(mep$header, mep$activity, sep= " | ")
  cat(" done.\n")
  
  mep[, c("ID", "name", "country", "birth.date", "birth.place", "death.date", 
          "current.group", "current.party", "context", "status", 
          "start", "end", "detail", "full.info", "path")]
}
