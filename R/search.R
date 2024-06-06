
sic_search <- function(names, ...) {
  
  ret <- lapply(names, function(p) {
    rows <- grep(p, sic[["d_references"]][["ReferenceValue"]], ...)
    if(length(rows) == 0) return(NULL)
    res <- lapply(rows, function(i) {
      item <- sic[["d_references"]][["ReferenceGlobalID"]][i]
      name <- sic[["d_references"]][["ReferenceValue"]][i]
      table <- sic[["d_references"]][["ReferenceName"]][i]
      unit <- sic[["d_references"]][["ReferenceUnit"]][i]
      # if (table == "microbiologyevents"){
      #   count <- 0
      # } else count <- nrow(subset(sic[[table]], itemid == item))
      # return(c(item, name, table, count))
      return(data.frame(item, name, table, unit))
    })
    res <- Reduce(rbind, res)
    # res <- data.frame()
    # if(length(names(res)) != 4) browser()
    # browser()
    names(res) <- c("item", "name", "table", "unit")
    # names(res) <- c("item", "name", "table", "count")
    # res$item <- as.integer(as.character(res$item))
    # res$count <- as.integer(as.character(res$count))
    # res <- res[order(-res$count),]
    rownames(res) <- NULL
    return(res)
  })
  names(ret) <- names
  return(ret)
}

eicu_search <- function(names, table, col_name, ...) {
  strn <- eicu[[table]][[col_name]]
  a <- lapply(names, function(nm) {
    rows <- grep(nm, strn, ...)
    if(length(rows) == 0) return(NULL)
    res <- table(strn[rows])
    ret <- data.frame(
      item_name = attr(res, "dimnames")[[1]],
      count = as.integer(res)
    )
    ret <- ret[order(-ret$count),]
    return(ret)
  })
  names(a) <- names
  return(a)
}

mimic_search <- function(names, ...) {
  ret <- lapply(names, function(p) {
    rows <- grep(p, mimic[["d_items"]][["label"]], ...)
    if(length(rows) == 0) return(NULL)
    res <- lapply(rows, function(i) {
      item <- mimic[["d_items"]][["itemid"]][i]
      name <- mimic[["d_items"]][["label"]][i]
      table <- mimic[["d_items"]][["linksto"]][i]
      if (table == "microbiologyevents"){
        count <- 0
      } else count <- nrow(subset(mimic[[table]], itemid == item))
      return(c(item, name, table, count))
    })
    res <- Reduce(rbind, res)
    res <- data.frame(matrix(res, ncol = 4))
    if(length(names(res)) != 4) browser()
    names(res) <- c("item", "name", "table", "count")
    res$item <- as.integer(as.character(res$item))
    res$count <- as.integer(as.character(res$count))
    res <- res[order(-res$count),]
    rownames(res) <- NULL
    return(res)
  })
  names(ret) <- names
  return(ret)
}

miiv_search <- function(names) {
  ret <- lapply(names, function(p) {
    rows <- grep(p, miiv[["d_items"]][["label"]], ignore.case = TRUE)
    if(length(rows) == 0) return(NULL)
    res <- lapply(rows, function(i) {
      item <- miiv[["d_items"]][["itemid"]][i]
      name <- miiv[["d_items"]][["label"]][i]
      table <- miiv[["d_items"]][["linksto"]][i]
      if (table == "microbiologyevents"){
        count <- 0
      } else count <- nrow(subset(miiv[[table]], itemid == item))
      return(c(item, name, table, count))
    })
    res <- Reduce(rbind, res)
    res <- data.frame(matrix(res, ncol = 4))
    if(length(names(res)) != 4) browser()
    names(res) <- c("item", "name", "table", "count")
    res$item <- as.integer(as.character(res$item))
    res$count <- as.integer(as.character(res$count))
    res <- res[order(-res$count),]
    rownames(res) <- NULL
    return(res)
  })
  names(ret) <- names
  return(ret)
}

aumc_search <- function(names, tbl, ...) {
  
  ret <- lapply(
    names, function(nm) {
      idx <- grepl(nm, aumc[[tbl]]$item, ...)
      res <- data.table::as.data.table(aumc[[tbl]][idx, ])
      res <- res[, c("itemid", "item"), with = FALSE]
      res <- res[, .N, by = c("itemid", "item")]
      data.table::setorderv(res, "N", -1L)
      
      unique(res)
    }
  )
  names(ret) <- names
  
  ret
}