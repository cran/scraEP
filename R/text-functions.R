## Function to remove all accents from a character string
unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}

## Compare two string vectors for common elements.
strcomp <- function(text1, text2) {
  text1 <- as.character(text1)
  text2 <- as.character(text2)
  matchOneInTwo <- table(sapply(text1, function(x) sum(text2 == x)))
  matchTwoInOne <- table(sapply(text2, function(x) sum(text1 == x)))
  tabOneInTwo <- table(text1 %in% text2)
  tabTwoInOne <- table(text2 %in% text1)
  oneNotInTwo <- text1[! text1 %in% text2]
  twoNotInOne <- text2[! text2 %in% text1]
  concat <- unique(c(text1, text2))
  tabAll <- table(x1= concat %in% text1, x2= concat %in% text2)
  list(matchTable = tabAll, 
       matchOneInTwo = matchOneInTwo, matchTwoInOne = matchTwoInOne, 
       tabOneInTwo = tabOneInTwo, tabTwoInOne = tabTwoInOne, 
       oneNotInTwo = oneNotInTwo, twoNotInOne = twoNotInOne)
}
