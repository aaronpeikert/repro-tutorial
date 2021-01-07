link <- function(link, linktext = link, footnote = link != linktext, file = here::here("links.txt")){
  markdown <- paste0("[", linktext, "](", link, ")")
  if(footnote){
    ref <- gsub("[^a-zA-Z]*", "", linktext)
    markdown <- paste0(markdown, "[^", ref, "]", collapse = "")
    cat(paste0("[^", ref, "]: ", link, "\n"), file = file, append = TRUE)
  }
  return(markdown)
}

link_index <- function(file = here::here("links.txt")){
  if(file.exists("links.txt")){
    links <- readLines(file)
    cat(links, sep = "\n")
    file.remove(file)
    invisible()
  } else return(invisible())
}
