
#' @export
stay_windows <- function(source, id_type = "icustay", win_type = "icustay",
                         in_time = "intime", out_time = "outtime",
                         interval = hours(1L)) {

  reclock <- function(x, y) re_time(x - y, interval)

  orig <- map_in_cols(id_type)
  inn  <- map_in_cols(win_type)
  out  <- map_out_cols(win_type)

  cols <- unique(c(id_type, win_type, inn, out, orig))

  map <- get_tbl("id_map", source, "aux")
  map <- map[, cols, with = FALSE]

  map <- map[, c(in_time, out_time) := lapply(.SD, reclock, get(orig)),
             .SDcols = c(inn, out)]
  map <- rm_cols(map, setdiff(colnames(map),
                              c(id_type, win_type, in_time, out_time)))

  as_id_tbl(map, id_type)
}
