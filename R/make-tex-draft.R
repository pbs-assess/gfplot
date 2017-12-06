
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
  out[[1]] <- paste0("\\subsection*{", lab, "}")
  out[[length(out) + 1]] <- ""
  out[[length(out) + 1]] <- "\\begin{figure}[htbp]"
  out[[length(out) + 1]] <- "\\centering"
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.5in]{bubbles/", x, ".pdf}")
  
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.45in]{sparks/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.6in]{joy/", x, "-joy.pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.65in]{cpue/", x, ".pdf}")
  if (file.exists(paste0("spatial-survey/", x, ".pdf"))) {
    out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.62in]{spatial-survey/", x, ".pdf}")
  } else {
    out[[length(out) + 1]] <- paste0("% survey map not rendered")
  }
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=1.25in]{catches/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=1.5in]{synop/dat-syn-", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.2in]{vb/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\caption{", lab, "}")
  out[[length(out) + 1]] <- "\\end{figure}"
  out[[length(out) + 1]] <- "\\clearpage"
  if (x == "english-sole") # last of "commercially valuable"
    out[[length(out) + 1]] <- "\\section*{Candidate species for triage assessments}"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")

writeLines(temp, con = "synopsis-draft-body.tex")

system("pdflatex synopsis-plots")
