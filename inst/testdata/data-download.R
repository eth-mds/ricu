
output <- NULL
tables <- c("PATIENTS.csv", "SERVICES.csv")

dl_file <- function(x) {
  ricu:::download_pysionet_file(
    paste("https://physionet.org/files/mimiciii-demo/1.4", x, sep = "/")
  )
}

trace(
  curl::curl_fetch_memory,
  exit = function() { output <<- returnValue() }
)

sink <- dl_file("SHA256SUMS.txt")

con <- rawConnection(output$content)
chksums <- readLines(con)
close(con)

chksums <- chksums[
  vapply(strsplit(chksums, " "), `[[`, character(1L), 2L) %in% tables
]

con <- rawConnection(raw(0), "r+")
writeLines(chksums, con)

output$content <- rawConnectionValue(con)
close(con)

saveRDS(output, "SHA256SUMS.txt.rds", version = 2L)

sink <- dl_file("PATIENTS.csv")

saveRDS(output, "PATIENTS.csv.rds", version = 2L)

sink <- dl_file("SERVICES.csv")

saveRDS(output, "SERVICES.csv.rds", version = 2L)

untrace(curl::curl_fetch_memory)
