
library(XML)
library(httr)

url <- "https://docs.google.com/spreadsheets/d/1ND8hEGn7nGiIA94eTVOvmsrD3erPoOGLo12hmh7j1OM/pubhtml"

readSpreadsheet <- function(url, sheet = 1){
  library(httr)
  r <- GET(url)
  html <- content(r)
  sheets <- readHTMLTable(html, header=F, stringsAsFactors=FALSE)
  df <- sheets[[sheet]]
  dfClean <- function(df){
    nms <- t(df[1,])
    names(df) <- nms
    df <- df[-1,-1] 
    row.names(df) <- seq(1,nrow(df))
    df
  }
  dfClean(df)
}
df <- readSpreadsheet(url,2)
df
str(df)
