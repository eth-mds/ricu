
output <- NULL

trace(
  curl::curl_fetch_memory,
  exit = function() { output <<- returnValue() }
)

sepsr:::download_pysionet_file(
  "https://physionet.org/files/mimiciii-demo/1.4/SHA256SUMS.txt"
)

con <- rawConnection(output$content)
chksums <- readLines(con)
close(con)
chksums <- chksums[c(1, 22, 26)]

con <- rawConnection(raw(0), "r+")
writeLines(chksums, con)

output$content <- rawConnectionValue(con)
close(con)

saveRDS(output, "SHA256SUMS.txt.rds")

sepsr:::download_pysionet_file(
  "https://physionet.org/files/mimiciii-demo/1.4/patients.csv"
)

saveRDS(output, "patients.csv.rds")

sepsr:::download_pysionet_file(
  "https://physionet.org/files/mimiciii-demo/1.4/services.csv"
)

saveRDS(output, "services.csv.rds")

untrace(curl::curl_fetch_memory)
