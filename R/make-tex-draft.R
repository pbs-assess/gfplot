source("R/make-spp-list.R")
spp <- get_spp_names()
spp <- spp$spp_w_hyphens

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# from ?toupper
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
    {s <- substring(s, 2); if(strict) tolower(s) else s},
    sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

files <- list.files(path = "spatial-survey", pattern = "\\.pdf", full.names = TRUE)
files_png <- sub("\\.pdf", ".png", files)
for(i in seq_along(files)) {
  message(files_png[i])
  system(paste("convert -density 180 -quality 100 -trim",
    files[i], files_png[i]))
}

temp <- lapply(spp, function(x) {
  lab <- capwords(gsub("-", " ", x))
  out <- list()
  out[[1]] <- paste0("\\section{", lab, "}")
  out[[length(out) + 1]] <- ""
  out[[length(out) + 1]] <- "\\begin{figure}[htbp]"
  out[[length(out) + 1]] <- "\\centering"
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.4in]{sparks/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.6in]{joy/", x, "-joy.pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.5in]{bubbles/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.63in]{cpue/", x, ".pdf}")
  if (file.exists(paste0("spatial-survey/", x, ".pdf"))) {
    out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.6in]{spatial-survey/", x, ".pdf}")
  } else {
    out[[length(out) + 1]] <- paste0("% survey map not rendered")
  }
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=1.35in]{catches/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=1.45in]{synop/dat-syn-", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\includegraphics[height=2.35in]{vb/", x, ".pdf}")
  out[[length(out) + 1]] <- paste0("\\caption{", lab, "}")
  out[[length(out) + 1]] <- "\\end{figure}"
  out[[length(out) + 1]] <- "\\clearpage"
  # if (x == "english-sole") # last of "commercially valuable"
    # out[[length(out) + 1]] <- "\\section*{Candidate species for triage assessments}"
  out
})

temp <- lapply(temp, function(x) paste(x, collapse = "\n"))
temp <- paste(temp, collapse = "\n")
writeLines(temp, con = "synopsis-draft-body.tex")
system("pdflatex synopsis-plots")
system("pdflatex synopsis-plots")
# system("pdflatex synopsis-plots")
