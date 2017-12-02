
source("R/make-spp-list.R")
spp <- get_spp_names()
spp <- spp$spp_w_hyphens

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# spp <- "lingcod"

temp <- lapply(spp, function(x) {
  lab <- gsub("-", " ", firstup(x))
  out <- list()
  out[[1]] <- paste0("\\section*{", lab, "}")
  out[[2]] <- ""
  out[[3]] <- "\\begin{figure}[htbp]"
  out[[4]] <- "\\centering"
  out[[5]] <- paste0("\\includegraphics[height=1.25in]{catches/", x, ".pdf}")
  out[[6]] <- paste0("\\includegraphics[height=2.75in]{sparks/", x, ".pdf}")
  out[[7]] <- paste0("\\includegraphics[height=2.8in]{joy/", x, "-joy.pdf}")
  out[[8]] <- paste0("\\includegraphics[height=2.8in]{cpue/", x, ".pdf}")
  if (file.exists(paste0("spatial-survey/", x, ".pdf"))) {
    out[[9]] <- paste0("\\includegraphics[height=2.78in]{spatial-survey/", x, ".pdf}")
  } else {
    out[[9]] <- paste0("% survey map not rendered")
  }
  out[[10]] <- paste0("\\includegraphics[height=1.5in]{synop/dat-syn-", x, ".pdf}")
  out[[11]] <- paste0("\\caption{", lab, "}")
  out[[12]] <- "\\end{figure}"
  out[[13]] <- "\\clearpage"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")

writeLines(temp, con = "synopsis-draft-body.tex")

system("pdflatex synopsis-plots")
