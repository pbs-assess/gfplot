source("R/make-spp-list.R")

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

temp <- lapply(spp, function(x) {
  lab <- gsub("-", " ", firstup(x))
  out <- list()
  out[[1]] <- paste0("\\section*{", lab, "}")
  out[[2]] <- ""
  out[[3]] <- "\\begin{figure}[htbp]"
  out[[4]] <- "\\centering"
  out[[5]] <- paste0("\\includegraphics[height=1.25in]{catches/", x, ".pdf}")
  out[[6]] <- paste0("\\includegraphics[height=2.85in]{sparks/", x, ".pdf}")
  out[[7]] <- paste0("\\includegraphics[height=2.8in]{joy/", x, "-joy.pdf}")
  out[[8]] <- paste0("\\includegraphics[height=2.78in]{cpue/", x, ".pdf}")
  out[[9]] <- paste0("\\includegraphics[height=1.5in]{synop/dat-syn-", x, ".pdf}")
  
  if (file.exists(paste0("spatial-survey/", x, "-spatial-survey.pdf"))) {
    out[[10]] <- paste0("\\includegraphics[height=3.3in]{spatial-survey/", x, "-spatial-survey.pdf}")
  } else {
    out[[10]] <- paste0("% \\includegraphics[height=3.3in]{spatial-survey/", x, "-spatial-survey.pdf}")
  }

  out[[11]] <- paste0("\\caption{", lab, "}")
  out[[12]] <- "\\end{figure}"
  out[[13]] <- "\\clearpage"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")

writeLines(temp, con = "synopsis-draft-body.tex")


