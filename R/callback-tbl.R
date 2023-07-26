
sic_data_float_h <- function(dat, ...) {
  hexstring_to_float <- function(x) {
    if (is.na(x)) {
      return(NA_real_)
    }
    hexstring <- substring(x, seq(1, 482, 2), seq(2, 482, 2))
    bytes <- as.raw(strtoi(hexstring[-1], base = 16))
    floats <- readBin(bytes, numeric(), length(bytes) %/% 4, 4, endian = "little")
    ifelse(floats == 0, NA_real_, floats)
  }
  
  setDT(dat)
  dat[, c("rawdata") := lapply(get("rawdata"), hexstring_to_float)] # TODO: remove hard coding of rawdata and derive from JSON config
  dat <- dat[, .(
      Offset = Offset + 60 * (0:(sapply(rawdata, length)-1)),
      Val = Val,
      cnt = cnt,
      rawdata = unlist(rawdata),
      rawdata_present = !is.na(rawdata)
    ),
    by = .(id, CaseID, DataID)
  ]
  dat[rawdata_present == FALSE, rawdata := Val] # Fix measurements that only have one 
  dat[, rawdata_present := NULL]
  dat
}


