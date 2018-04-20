cat("\n===Début du clean===\n")
index <- system.file("html/00Index.html", package = "COGugaison")
if (file.exists(index)) {
  cat("--Re encodage du fichier 00Index.html\n")
  content <- readLines(index)
  content_utf8 <- enc2utf8(content)
  out <- file(index, open = "w+", encoding = "native.enc")
  writeLines(content_utf8, out, useBytes = TRUE)
  close(out)
}
cat("\n===Fin du clean===\n")
