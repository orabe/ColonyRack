#' Reads the adjacency list of the reader+cage neighborhood.
#'
#' @param filename the default path to use. If NA, a dialog box will appear.
#'
#' @return a Dataframe with the read adjacency list (call this a "graph").
#'
#' @details
#'   Expected conventions are:
#'     * no names with "," "." ";",
#'     * no stand-alone integers,
#'     * readers are not "direct neighbors" of each other.
#'   The Graph is given as a list of readers (one row per reader) and the
#'   corresponding list of edges the reader is adjacent to.
#'   The edges are labeled as the cages.
#'
#' @md
#' @export
read_adj_list <- function(filename = NA) {
  if(is.na(filename)) {
    message.file <- "Select file containing reader+cage neighborhood data."
    green(message.file, "Dialog might appear behind rstudio.\n") %>% cat()

    filename <- tk_choose.files(
      default = paste0(getwd(), "/RawData"),
      caption = message.file,
      multi = FALSE
    )
    filename <- filename[str_detect(filename, "\\.(csv|xlsx)")]
  }

  read_csv2(filename, show_col_types = FALSE)
}

#' Chooses a specific entry in the APSP matrix.
#'
#' @param APSP the All-Pairs-Shortest-Paths matrix obtained by [get_apsp]
#' @param first string; name of first reader
#' @param second string; name of second reader
#'
#' @return the specific entry in APSP(first, second)
choose <- function(APSP, first, second) {
  if(is.na(first) | is.na(second)) { return(NA) }
  if(!first %in% APSP$Reader | !second %in% APSP$Reader) {
    return(NA)
  }
  APSP %>% filter(Reader == first) %>% .[[second]]
}

#' Computes the All-Pairs-Shortest-Paths (short: APSP) matrix for a given graph.
#'
#' @param G a graph (that is, an adjacency list Dataframe) obtained by
#'   [read_adj_list]
#'
#' @return APSP matrix as a dataframe.
#'   The entries are the paths (vector of cage names), "0" (zero) if the
#'   readers are direct neighbors, or "Inf" if the readers are not connected
#'
#' @details The output APSP matrix has as entries (strings):
#'   * "0" ; same reader,
#'   * "Inf" (infinity) ; The readers are not connected,
#'   * cage name ; The readers are neighbors, using some cage,
#'   * any integer greater than 1, as a string ;
#'     The readers are connected, but the distance is at least 2.
#'
#' @md
#' @export
#' @import stringr
get_apsp <- function(G) {
  G.size <- length(G$Reader)
  G <- G %>% mutate(Neighbors = str_split(Neighbors, pattern = ", "))
  G_copy <- G

  # helping functions ---------------------------------------------------------#

  # Determines what the cage between two readers is, or that there are none.
  #
  # @param first list of strings, Neighbor-list of first reader
  # @param second list of strings, Neighbor-list of second reader
  #
  # @return string, either "Inf" or the cage between the two readers
  rcut <- function(first, second) {
    first <- unlist(first)
    second <- unlist(second)
    res <- (first[first %in% second])[1]
    if(is.na(res)) {"Inf"} else {res}
  }

  # Checks if there exists a path of length k between readers x and y.
  #
  # @param x string, name of first reader
  # @param y string, name of second reader
  # @param k integer, the distance to check for
  #
  # @return boolean, whether there exists a path of length k between x and y
  check_dist <- function(x, y, k) {
    z <- G %>% filter(!!sym(x) == k-1, !!sym(y) == 1) %>% .$Reader %>% .[1]
    !is.na(z)
  }
  # ---------------------------------------------------------------------------#

  # distance 1
  G_cages <- G
  walk2(G_copy$Reader, G_copy$Neighbors, ~{
    G_cages <<- G_cages %>% rowwise() %>% mutate(!!.x := rcut(Neighbors, .y))
  })
  # copy the Inf and 1 instead of Cage
  G <- G_cages %>%
    mutate(
      across(any_of(G$Reader), ~{ if(.x == "Inf") { Inf } else { 1 } })
    )
  G_copy <- G

  #further distances
  2:G.size %>% walk(function(k) {
    G <<- G %>%
      mutate(
        across(
          any_of(G$Reader),
          function(val) {
            if(val < k) {
              val
            } else {
              if(check_dist(Reader, cur_column(), k)) { k } else { val }
            }
          }
        )
      )
  })

  G_cages <- G %>%
    mutate(
      across(
        any_of(G_copy$Reader),
        function(val) {
          if(Reader == cur_column()) {
            "0"
          }
          else if(val == 1) {
            choose(G_cages, Reader, cur_column())
          } else {
            as.character(val)
          }
        }
      )
    ) %>%
    select(-Neighbors)
  G_cages
}
