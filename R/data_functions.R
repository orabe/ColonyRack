inner <- new.env()
inner$phase <- list(none = 0, light = 1, dark = 2, unknown = -1)

#' Reads the chosen data
#'
#' @param filenames will be read if given, otherwise dialog GUI
#' @return the raw data
#' @export
#' @importFrom crayon green
#' @importFrom magrittr %>%
#' @importFrom tcltk tk_choose.files
#' @import purrr
readData <- function(filenames = NA) {
  # this code is was removed for privacy reasons

}

#' cleans the data, creates first new columns
#'
#' @param data the raw data
#' @param phases list of different phases, each consisting of a vector of hours.
#' @param groups list of different groups, each group being a vector of strings.
#' @return cleaned data
#' @export
#' @importFrom stringr str_replace
#' @import lubridate

cleanRawData <- function(
  data,
  phases = list("light" = 0:11, "dark" = setdiff(0:23, 0:11)),
  groups
) {
  # this code is was removed for privacy reasons
}

#' generates dateReaderData
#'
#' @param data data that was cleaned with cleanRawData function
#' @return dateReaderData
get_dateReaderData <- function(data) {
  # this code is was removed for privacy reasons
}


#' generates table with mice-cage information.
#'
#' @param data data that was cleaned with cleanRawData before.
get_locations <- function(
  data,
  # reader_neighbors = is_next,
  reader_cage = get_cage
) {
   # this code is was removed for privacy reasons
}

#' Returns a function that chooses the "right" cage based on what cage the
#' mouse was in before.
same_reader <- function(first_cage){
    # this code is was removed for privacy reasons
}

#' Reads the wanted data, or compute it itself, if needed.
#'              Useful on big data frames.
#'
#' @param data data to use in the toCompute function
#' @param to.compute function to compute new data.frame if file not found
#' @param filename file to use if exists.
#'                 If NA, the user has to choose existent file
#' @param force.compute TRUE if computation is wanted, even if file exists
#' @return either computer or read data.frame
compute_or_read <-
  function(data, to.compute, filename = NA, force.compute = FALSE) {
      # this code is was removed for privacy reasons
  }

#' Generates roaming entropy data.frame
#' @param data data.frame with Date, Cage, Group, IdLabel, Duration information.
#'   the Date, IdLabel, Cage are used for grouping,
#'   Duration for the actual roaming entropy.
#' @return data.frame with roaming entropy per date, per mouse or group.
generate_roaming.entropy <- function(data, cage.values) {
    # this code is was removed for privacy reasons
}

#' gets the id of every date-day in all_times.
#' @param all_times list with dates with regular interval
#' @return a data.fram with one column - the id. (maybe list would be better)
get_date_ids <- function(all_times) {
  # this code is was removed for privacy reasons
}

#' reader neighborhood used for V1-3D-Cage
is_next <- function(now, lead) {
  # in each vector in these two lists, [1] is floor and [2] is "door"
  now.reader.nums <-
    str_extract_all(now, "\\d+") %>% map(~as.numeric(.x)) %>% do.call(rbind, .)
  lead.reader.nums <-
    str_extract_all(lead, "\\d+") %>% map(~as.numeric(.x)) %>% do.call(rbind, .)
  # -1 = went down, +1 = went up, 0 = same floor, else unexpected
  floor.difference <- lead.reader.nums[,1] - now.reader.nums[,1]
  #browser()
  case_when(
    #{browser();FALSE}~FALSE,
    # 1
    now.reader.nums[,2] == 1 ~
      case_when(
        now.reader.nums[,1] == 1 ~ NA, # there is no R1-1
        floor.difference == 0 ~ lead.reader.nums[,2] %in% c(2, 3),
        floor.difference == -1 ~ lead.reader.nums[,2] %in% c(5, 6),
        TRUE ~ FALSE
      ),
    # 2
    now.reader.nums[,2] == 2 ~
      case_when(
        now.reader.nums[,1] == 1 ~
          floor.difference == 0 & lead.reader.nums[,2] %in% c(3, 4),
        floor.difference == 0 ~ lead.reader.nums[,2] %in% c(1, 4),
        floor.difference == -1 ~ lead.reader.nums[,2] %in% c(5, 6),
        TRUE ~ FALSE
      ),
    # 3
    now.reader.nums[,2] == 3 ~
      case_when(
        # number of floors needs to be dynamic? -> change this switch!
        now.reader.nums[,1] == 1 ~ lead.reader.nums[,2] %in% c(2, 5, 7),
        now.reader.nums[,1] == 2 ~ lead.reader.nums[,2] %in% c(1, 5, 7),
        now.reader.nums[,1] == 3 ~ lead.reader.nums[,2] %in% c(1, 6, 7),
        TRUE ~ NA
      ),
    # 4
    now.reader.nums[,2] == 4 ~
      floor.difference == 0 & lead.reader.nums[,2] %in% c(2, 6, 8),
    # 5
    now.reader.nums[,2] == 5 ~
      case_when(
        now.reader.nums[,1] == 3 ~ NA, # there is no R3-5
        floor.difference == 0 ~ lead.reader.nums[,2] %in% c(3, 6, 7),
        floor.difference == 1 ~ lead.reader.nums[,2] %in% c(1, 2),
        TRUE ~ FALSE
      ),
    # 6
    now.reader.nums[,2] == 6 ~
      case_when(
        now.reader.nums[,1] == 3 ~
          floor.difference == 0 & lead.reader.nums[,2] %in% c(3, 4, 7, 8),
        floor.difference == 0 ~ lead.reader.nums[,2] %in% c(4, 5, 8),
        floor.difference == 1 ~ lead.reader.nums[,2] %in% c(1, 2),
        TRUE ~ FALSE
      ),
    # 7
    now.reader.nums[,2] == 7 ~
      if(now.reader.nums[,1] == 3) {
        floor.difference == 0 & lead.reader.nums[,2] %in% c(3, 6, 10)
      } else {
        floor.difference == 0 & lead.reader.nums[,2] %in% c(3, 5, 10)
      },
    # 8
    now.reader.nums[,2] == 8 ~
      floor.difference == 0 & lead.reader.nums[,2] %in% c(4, 6, 10),
    # 10
    now.reader.nums[,2] == 10 ~
      floor.difference == 0 & lead.reader.nums[,2] %in% c(7, 8),
    # else
    TRUE ~ NA
  )
}

#' cage detection used for V1-3D-Cage
#' TODO maybe use function(...){case_when(...,TRUE~NA)}
get_cage <- function(from, to, floor.max = 3) {
  # in each vector in these two lists, [1] is floor and [2] is "door"
  from.reader.nums <-
    str_extract_all(from, "\\d+") %>% map(~as.numeric(.x)) %>% do.call(rbind, .)
  to.reader.nums <-
    str_extract_all(to, "\\d+") %>% map(~as.numeric(.x)) %>% do.call(rbind, .)
  # -1 = went down, +1 = went up, 0 = same floor, else unexpected
  floor.difference <- to.reader.nums[,1] - from.reader.nums[,1]
  #browser()
  case_when(
    #{browser();FALSE}~FALSE,
    # 1
    from.reader.nums[,2] == 1 ~
      case_when(
        from.reader.nums[,1] == 1 ~ NA_character_, # there is no R1-1
        floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 2 ~
              paste0("Betw ", from.reader.nums[,1], "-", "1:2"),
            to.reader.nums[,2] == 3 ~
              paste0("Cage ", from.reader.nums[,1], "-", "1"),
            TRUE ~ NA_character_
          ),
        floor.difference == -1 ~
          ifelse(
            to.reader.nums[,2] %in% c(5, 6),
            paste0("Betw ", from.reader.nums[,1], ":", to.reader.nums[,1]),
            NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 2
    from.reader.nums[,2] == 2 ~
      case_when(
        from.reader.nums[,1] == 1 ~
          case_when(
            floor.difference != 0 ~ NA_character_, # there is no floor below
            to.reader.nums[,2] == 3 ~ "Cage 1-1",
            to.reader.nums[,2] == 4 ~ "Cage 1-2",
            TRUE ~ NA_character_
          ),
        floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 1 ~
              paste0("Betw ", from.reader.nums[,1], "-", "1:2"),
            to.reader.nums[,2] == 4 ~
              paste0("Cage ", from.reader.nums[,1], "-", "2"),
            TRUE ~ NA_character_
          ),
        floor.difference == -1 ~
          ifelse(
            to.reader.nums[,2] %in% c(5, 6),
            paste0("Betw ", from.reader.nums[,1], ":", to.reader.nums[,1]),
            NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 3
    from.reader.nums[,2] == 3 & floor.difference == 0 ~
      case_when(
        # lowest floor doesn't have the down-ladder (no 6-reader)
        from.reader.nums[,1] == 1 ~
          case_when(
            to.reader.nums[,2] == 2 ~ "Cage 1-1",
            to.reader.nums[,2] %in% c(5, 7) ~ "Cage 1-3",
            TRUE ~ NA_character_
          ),
        from.reader.nums[,1] < floor.max & floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 1 ~
              paste0("Cage ", from.reader.nums[,1], "-", "1"),
            to.reader.nums[,2] %in% c(5, 7) ~
              paste0("Cage ", from.reader.nums[,1], "-", "3"),
            TRUE ~ NA_character_
          ),
        # highest floor doesn't have the up-ladder (no 2-reader)
        from.reader.nums[,1] == floor.max & floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 1 ~ paste0("Cage ", floor.max, "-", "1"),
            to.reader.nums[,2] %in% c(6, 7) ~
              paste0("Cage ", floor.max, "-", "3"),
            TRUE ~ NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 4
    from.reader.nums[,2] == 4 & floor.difference == 0 ~
      case_when(
        to.reader.nums[,2] == 2 ~
          paste0("Cage ", from.reader.nums[,1], "-", "2"),
        to.reader.nums[,2] %in% c(6, 8) ~
          paste0("Cage ", from.reader.nums[,1], "-", "4"),
        TRUE ~ NA_character_
      ),
    # 5
    from.reader.nums[,2] == 5 ~
      case_when(
        # there is no R\floor.max\-5
        from.reader.nums[,1] == floor.max ~ NA_character_,
        floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] %in% c(3, 7) ~
              paste0("Cage ", from.reader.nums[,1], "-", "3"),
            to.reader.nums[,2] == 6 ~
              paste0("Betw ", from.reader.nums[,1], "-", "5:6"),
          ),
        floor.difference == 1 ~
          ifelse(
            to.reader.nums[,2] %in% c(1, 2),
            paste0("Betw ", from.reader.nums[,1], ":", to.reader.nums[,1]),
            NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 6
    from.reader.nums[,2] == 6 ~
      case_when(
        from.reader.nums[,1] == floor.max ~
          case_when(
            floor.difference != 0 ~ NA_character_, # there is no upper floor
            to.reader.nums[,2] %in% c(3, 7) ~
              paste0("Cage ", floor.max, "-", "3"),
            to.reader.nums[,2] %in% c(4, 8) ~
              paste0("Cage ", floor.max, "-", "4"),
            TRUE ~ NA_character_
          ),
        floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 5 ~
              paste0("Betw ", from.reader.nums[,1], "-", "5:6"),
            to.reader.nums[,2] %in% c(4, 8) ~
              paste0("Cage ", from.reader.nums[,1], "-", "4"),
            TRUE ~ NA_character_
          ),
        floor.difference == 1 ~
          ifelse(
            to.reader.nums[,2] %in% c(1, 2),
            paste0("Betw ", from.reader.nums[,1], ":", to.reader.nums[,1]),
            NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 7
    from.reader.nums[,2] == 7 ~
      case_when(
        from.reader.nums[,1] < floor.max & floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 10 ~
              paste0("Cage ", from.reader.nums[,1], "-", "5"),
            to.reader.nums[,2] %in% c(3, 5) ~
              paste0("Cage ", from.reader.nums[,1], "-", "3"),
            TRUE ~ NA_character_
          ),
        from.reader.nums[,1] == floor.max & floor.difference == 0 ~
          case_when(
            to.reader.nums[,2] == 10 ~
              paste0("Cage ", floor.max, "-", "5"),
            to.reader.nums[,2] %in% c(3, 6) ~
              paste0("Cage ", floor.max, "-", "3"),
            TRUE ~ NA_character_
          ),
        TRUE ~ NA_character_
      ),
    # 8
    from.reader.nums[,2] == 8 & floor.difference == 0 ~
      case_when(
        to.reader.nums[,2] == 10 ~
          paste0("Cage ", from.reader.nums[,1], "-", "6"),
        to.reader.nums[,2] %in% c(4, 6) ~
          paste0("Cage ", from.reader.nums[,1], "-", "4"),
        TRUE ~ NA_character_
      ),
    # 10
    from.reader.nums[,2] == 10 & floor.difference == 0 ~
      case_when(
        to.reader.nums[,2] == 7 ~
          paste0("Cage ", from.reader.nums[,1], "-", "5"),
        to.reader.nums[,2] == 8 ~
          paste0("Cage ", from.reader.nums[,1], "-", "6"),
        TRUE ~ NA_character_
      ),
    # else
    TRUE ~ NA_character_
  )
}

#' summarizes the cages a mouse was in at the same time interval into one column
#' @param all_times list with the timestamps for the intervals as all_times
#' @param data data.frame with cage visits (one cage per visit)
#' @param time.interval the interval duration
#' @return a data.frame with cages per interval information.
#' TODO: IT'S NOT WORKING THAT WELL YET!
get_cages_perinterval <- function(all_times, data, time.interval) {
  all_times %>%
    map_df(
      ~ data %>%
        mutate(
          DateTime_from =
            format(DateTime_from, "%Y-%m-%d %H:%M") %>%
            as.POSIXct(tz = "UTC"),
          DateTime_to =
            format(DateTime_to, "%Y-%m-%d %H:%M") %>%
            as.POSIXct(tz = "UTC")
        ) %>%
        filter(Duration > 5) %>%
        group_by(IdLabel) %>%
        mutate(
          # TRUE if interval starts before event end
          # and if interval ends after event starts
          overlap = (
            difftime(.x, DateTime_to) <= 0 &
              difftime(.x, DateTime_from) >= 0
          )
        ) %>%
        summarize(
          Cages = paste(unique(Cage[overlap]), collapse = " "),
          DateTime_from = .x,
          DateTime_to = .x + minutes(time.interval)
        )
    ) %>%
    select(IdLabel, DateTime_from, DateTime_to, Cages)
}

###--------------------------------------------------------------------------###
#  Administrative functions                                                    #
###--------------------------------------------------------------------------###

#' lets the user choose a results directory, generates subdirectories and
#' returns an environment containing the directory and the subdirectories.
#' @param dir_name will be read if given, otherwise dialog GUI
#' @return Environment containing the chosen directory and the subdirectories
#'
#' @md
#' @export
#' @importFrom crayon green
#' @importFrom magrittr %>%
#' @importFrom tcltk tk_choose.files tk_choose.dir
get_dirsave <- function(dir_name = NA) {
  dirs <- new.env()

  if(any(is.na(dir_name))){
    message <- "Select a folder to save the analysis results in."
    green(message, "Dialog might appear behind rstudio.\n") %>% cat()


  dirs$base <- tk_choose.dir(paste0(getwd(), "/results/"), message)
  }

  else {

    if (!dir.exists(paste0(getwd(), "/results/"))) {
      dir.create(path = paste0(getwd(), "/results/"))
      }

    dirs$base = dir_name
  }

  # ---------------------------------------------------- jpg:
  # subdirectory for jpg files (mostly the figures) which holds all images
  dirs$jpg <- paste0(dirs$base, "/jpg")
  if (!dir.exists(dirs$jpg)) {
    dir.create(path = dirs$jpg)
  }

  # # subdirectory for jpg files (jpg_locomotion which contains duration, distance and speed sub-directories)
  # dirs$jpg_locomotion <- paste0(dirs$jpg, "/locomotion")
  # if (!dir.exists(dirs$jpg_locomotion)) {
  #   dir.create(path = paste0(dirs$jpg_locomotion))
  # }

  # subdirectory for jpg files (duration)
  dirs$jpg_duration <- paste0(dirs$jpg, "/duration")
  if (!dir.exists(dirs$jpg_duration)) {
    dir.create(path = paste0(dirs$jpg_duration))
  }

  # subdirectory for jpg files (distance)
  dirs$jpg_distance <- paste0(dirs$jpg, "/distance")
  if (!dir.exists(dirs$jpg_distance)) {
    dir.create(path = paste0(dirs$jpg_distance))
  }

  # subdirectory for jpg files (speed)
  dirs$jpg_speed <- paste0(dirs$jpg, "/speed")
  if (!dir.exists(dirs$jpg_speed)) {
    dir.create(path = paste0(dirs$jpg_speed))
  }

  # subdirectory for jpg files (cage_visits)
  dirs$jpg_cage_visits <- paste0(dirs$jpg, "/cage_visits")
  if (!dir.exists(dirs$jpg_cage_visits)) {
    dir.create(path = paste0(dirs$jpg_cage_visits))
  }

  # subdirectory for jpg files (spatial)
  dirs$jpg_spatial <- paste0(dirs$jpg, "/spatial")
  if (!dir.exists(dirs$jpg_spatial)) {
    dir.create(path = paste0(dirs$jpg_spatial))
  }

  # # subdirectory for jpg files (social)
  # dirs$jpg_social <- paste0(dirs$jpg, "/social")
  # if (!dir.exists(dirs$jpg_social)) {
  #   dir.create(path = paste0(dirs$jpg_social))
  # }

  # ---------------------------------------------------- csv:

  # subdirectory for csv files
  dirs$csv <- paste0(dirs$base, "/csv")
  if (!dir.exists(dirs$csv)) {
    dir.create(path = dirs$csv)
  }

  # subdirectory for 24-hour-tour-raw
  dirs$tour_raw <- paste0(dirs$csv, "/tour_raw")
  if (!dir.exists(dirs$tour_raw)) {
    dir.create(path = dirs$tour_raw)
  }

  # subdirectory for 24-hour-tour-processed
  dirs$tour_processed <- paste0(dirs$csv, "/tour_processed")
  if (!dir.exists(dirs$tour_processed)) {
    dir.create(path = dirs$tour_processed)
  }

  # subdirectory for analyzed data
  dirs$csv_analyzed <- paste0(dirs$csv, "/analyzed")
  if (!dir.exists(dirs$csv_analyzed)) {
    dir.create(path = dirs$csv_analyzed)
  }

  # # subdirectory for csv files (locomotion which contains duration, distance and speed sub-directories)
  # dirs$csv_locomotion <- paste0(dirs$csv_analyzed, "/locomotion")
  # if (!dir.exists(dirs$csv_locomotion)) {
  #   dir.create(path = paste0(dirs$csv_locomotion))
  # }

  # subdirectory for csv files (duration)
  dirs$csv_duration <- paste0(dirs$csv_analyzed, "/duration")
  if (!dir.exists(dirs$csv_duration)) {
    dir.create(path = paste0(dirs$csv_duration))
  }

  # subdirectory for csv files (distance)
  dirs$csv_distance <- paste0(dirs$csv_analyzed, "/distance")
  if (!dir.exists(dirs$csv_distance)) {
    dir.create(path = paste0(dirs$csv_distance))
  }

  # subdirectory for csv files (speed)
  dirs$csv_speed <- paste0(dirs$csv_analyzed, "/speed")
  if (!dir.exists(dirs$csv_speed)) {
    dir.create(path = paste0(dirs$csv_speed))
  }

  # subdirectory for csv files (cage_visits)
  dirs$csv_cage_visits <- paste0(dirs$csv_analyzed, "/cage_visits")
  if (!dir.exists(dirs$csv_cage_visits)) {
    dir.create(path = paste0(dirs$csv_cage_visits))
  }

  # subdirectory for csv files (spatial)
  dirs$csv_spatial <- paste0(dirs$csv_analyzed, "/spatial")
  if (!dir.exists(dirs$csv_spatial)) {
    dir.create(path = paste0(dirs$csv_spatial))
  }

  # # subdirectory for csv files (social)
  # dirs$csv_social <- paste0(dirs$csv_analyzed, "/social")
  # if (!dir.exists(dirs$csv_social)) {
  #   dir.create(path = paste0(dirs$csv_social))
  # }


  dirs
}


###--------------------------------------------------------------------------###
#  functions for the new way of analyzing the data (iteratively)               #
###--------------------------------------------------------------------------###

#' Get a list of groups and for each the vector of mice in the group.
#' @param data Dataframe obtained by using \code{\link{cleanRawData}} on
#'   colonyrack-rawdata
#' @param groupnames list or vector with names of the groups to be found in data
#'
#' @return list of vectors. One list-entry per group, and the vector contains
#'   the mice ids that are in the group.
#'
#' @md
#' @export
get_mice_per_group <- function(data, groupnames) {
  groupnames %>%
    map(~ {
      data %>%
        ungroup() %>%
        filter(Group == .x) %>%
        .$IdLabel %>%
        unique()
    }) %>%
    setNames(groupnames)
}

#' small helping function-generator for getting the lag of some value
lag_generator <- function() {
  old <- NA
  function(new) {
    tmp <- old
    old <<- new
    tmp
  }
}

#' Set the day of each entry.
#'
#' @param data Dataframe obtained by using \code{\link{cleanRawData}} on
#'   colonyrack-rawdata
#' @param day_start int between 0 and 23, set the hour when a day should start
#'
#' @return the input dataframe, but with the \code{Day} column
#' @md
#' @export
#' @import lubridate
set_day <- function(data, day_start) {
  firstDay <- min(data$DateTime[1]) %>%
    (function(DateTime) {
      is_before_start <- hour(DateTime) < day_start
      DateTime - days(as.numeric(is_before_start))
    })() %>%
    # reset the day and set it to day_start hours
    floor_date(unit = "day") +
    hours(day_start)

  data %>%
    group_by(IdLabel) %>%
    # alternative would be
    # 1 + floor(as.duration(firstDay %--% DateTime) / ddays(1))
    mutate(Day = 1 + difftime(DateTime, firstDay) %>% as.period() %>% day()) %>%
    ungroup()
}


#' Generate the reader-coords-table.
#'
#' @param data the raw data (obtained using \code{\link{readData}})
#'
#' @return Dataframe with (reader, x, y, z) as columns
#' @export
#' @import dplyr
get_coords <- function(data) {
  data %>%
    filter(DateTime == "#ID-Device") %>%
    distinct(., IdRFID, .keep_all = TRUE) %>%
    #{unique(.$IdRFID)} %>% #Alternative: .$IdRFID %>% unique() %>%
    select(IdRFID, IdLabel, unitLabel, eventDuration) %>%
    transmute(
      Reader = IdRFID,
      x = IdLabel, y = unitLabel, z = eventDuration
    ) %>%
    mutate(across(matches("[xyz]"), ~as.numeric(.x)))
}

#' Get the double reads in the tour data.
#'
#' @param data Dataframe obtained by using \code{\link{cleanRawData}} on
#'   colonyrack-rawdata
#'
#' @return the input dataframe, but with the \code{DoubleRead} column
#'
#' @details
#'   * We look at each separate mouse, call it M.
#'   * If there is a new event with M, while an earlier event with M has not
#'     finished yet, we call it a *double read*.
#'   * The number tells us what consecutive double read the event is.
#'
#' @md
#' @export
get_double_reads <- function(data) {
  data %>%
    group_by(IdLabel) %>%
    mutate(
      DateTime_myend = DateTime + milliseconds(eventDuration),
      after_last = as.period(lag(DateTime) %--% DateTime),
    ) %>%
    mutate(
      DateTime_end = {
        my_lag <- lag_generator()
        map2(DateTime, DateTime_myend, function(DateTime, DateTime_myend) {
          lag_end <- my_lag(DateTime_myend)
          overlap <- DateTime < lag_end
          new_val <- {
            if(is.na(overlap)) {
              DateTime_myend
            } else if(overlap) {
              max(DateTime_myend, lag_end)
            } else {
              DateTime_myend
            }
          }
          my_lag(new_val)
          new_val
        }) %>% unlist() %>% as_datetime(tz = "UTC")
      },
      overlap = DateTime < lag(DateTime_end),
      multiple = overlap & lag(overlap),
      DoubleRead = {
        my_lag <- lag_generator()
        overlap %>%
          map(~ {
            old_rr <- my_lag(NA)
            rr <- {
              if(is.na(.x)) {
                0
              } else if(.x) {
                old_rr + 1
              } else {
                0
              }
            }
            my_lag(rr)
            rr
          }) %>%
          unlist()
      }
    ) %>%
    select(-overlap, -multiple, -after_last, -DateTime_myend) %>%
    ungroup()
}

#' Get the 24hour tour raw data. That is, with Day and double reads information
#' added.
#'
#' @param data Dataframe obtained by using \code{\link{cleanRawData}} on
#'   colonyrack-rawdata
#' @param day_start int between 0 and 23, set the hour when a day should start
#'
#' @return the 24hour tour raw data, that is,
#'   the input dataframe, but with the \code{Day} and \code{DoubleRead} columns
#'
#' @md
#' @export
get_24hour_tour_raw <- function(data, day_start) {
  data %>% set_day(day_start) %>% get_double_reads()
}

#' Get the 24hour tour processed data.
#'
#' Add the segment-data (cage, distance, duration etc.).
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_raw}}
#' @param APSP All-Pairs-Shortest-Path dataframe obtained by using
#'   \code{\link{get_apsp}}
#' @param cages vector of cage-labels
#' @param connections vector of connection-labels
#'
#' @details
#'   * \code{Segment_type} tells how many cages the mouse crossed at least (and
#'     most probably),
#'   * \code{Cage_type} tells whether the Cage is an actual Cage (1) or a level
#'     connection (2),
#'   * One (linear) \code{Crossing} is the traversing of multiple Readers that
#'     are on the same $x or $y level (computed using \code{`%linear%`}).
#'
#' @return the 24hour tour processed data (see other documentation for details)
#' @export
get_24hour_tour_processed <- function(data, APSP, cages, connections, coords) {
  #----------------------------------------------------------------------------#
  #-- helping functions -------------------------------------------------------#
  `%dist%` <- function(from, to) {
    if(any(is.na(c(from, to)))) { return(NA_integer_) }

    a <- coords %>% filter(Reader == from)
    b <- coords %>% filter(Reader == to)

    sqrt((b$x-a$x)^2 + (b$y-a$y)^2 + (b$z-a$z)^2)
  }

  `%height%` <- function(from, to) {
    if(any(is.na(c(from, to)))) { return(NA_integer_) }

    a <- coords %>% filter(Reader == from)
    b <- coords %>% filter(Reader == to)
    b$z - a$z
  }

  `%linear%` <- function(from, to) {
    if(any(is.na(c(from, to)))) { return(FALSE) }
    if(from == to) { return(FALSE) }

    a <- coords %>% filter(Reader == from)
    b <- coords %>% filter(Reader == to)

    a$z == b$z & (a$x == b$x | a$y == b$y)
  }

  get_newID <- (function() {
    id <- 0
    function() {
      id <<- id + 1
      id
    }
  })()
  #----------------------------------------------------------------------------#


  APSP_matrix <- APSP %>% select(-Reader) %>% as.matrix()
  rownames(APSP_matrix) <- APSP$Reader
  data %>%
    filter(DoubleRead == 0) %>%
    group_by(IdLabel) %>%
    arrange(DateTime) %>%
    mutate(
      Week = (Day / 7) %>% ceiling(),
      DateTime_in = DateTime,
      DateTime_out = lead(DateTime),
      Duration = as.duration(DateTime_in %--% DateTime_out) %>% round(3),
      unitLabel_from = unitLabel,
      unitLabel_to = lead(unitLabel),
      Cage = map2_chr(unitLabel_from, unitLabel_to, ~{
        if(is.na(.y)) { NA } else { APSP_matrix[.x, .y] }
      }),
      Segment_type = Cage %>% map_dbl(~{
        if(.x %in% c("0")) {
          0
        } else if(.x %in% as.character(1:length(APSP$Reader))) {
          as.integer(.x)
        } else {
          1
        }
      }),
      Cage_type = Cage %>% map_dbl(~{
        if(.x %in% cages) {
          1
        } else if(.x %in% connections) {
          2
        } else {
          NA
        }
      }),
      Distance = map2_dbl(unitLabel_from, unitLabel_to, ~ .x %dist% .y),
      Height = map2_dbl(unitLabel_from, unitLabel_to, ~ .x %height% .y),
      Crossing_step =
        ifelse(
          is.na(Cage_type),
          0,
          map2_dbl(
            unitLabel_from, unitLabel_to,
            (function() {
              step <- 0
              function(from, to) {
                if(from %linear% to) {
                  step <<- step + 1
                } else {
                  step <<- 0
                }
                step
              }
            })()
          )
        ),
      Crossing = map_dbl(Crossing_step,
        (function() {
          id <- 0
          function(step) {
            if(step %in% c(0, 1)) {
              id <<- get_newID()
            }
            id
          }
        })()
      )
    ) %>%
    group_by(Crossing) %>%
    mutate(Crossing_length = max(Crossing_step)) %>%
    ungroup() %>%
    select(
      Date, Week, Day, Phase, DateTime_in, DateTime_out, Duration, IdLabel, Group,
      everything(),
      -DateTime, -unitLabel, -eventDuration, -senseRFIDrecords, -MsgValue1,
      -DateTime_end, -DoubleRead
    )
}

#' Save the tour data for each 12 hour of each phase (dark and light) of each day
#' (either raw or processed)
#'
#' @details inside the dirsave$csv tour_raw or tour_processed directory, creates
#'   one subdirectory per group, inside that one subdirectory per mouse in the
#'   group, and inside that subdirectory per mouse make a subdirectory for days.
#'   and store all csv file per phase (daek and light).
#'
#' @param data Dataframe obtained by using \code{\link{get_12hour_tour_raw}}
#' @param group_mice list of vectors. One list-entry per group, and the vector
#'   contains the mice ids that are in the group.
#' @param dirsave environment containing all the results-directory-paths
#' @param type either "raw" or "processed", indicating the type of the table
#' @export

save_12hour_tour <- function(data, group_mice, dirsave, type, date) {
  if(type == "raw") {
    dirsave_tour <- dirsave$tour_raw
  } else if(type == "processed") {
    dirsave_tour <- dirsave$tour_processed
  } else {
    stop("wrong type of 24hour tour data\n")
  }
  walk2(group_mice, names(group_mice), function(gr_mice, gr_name) {
    group_folder <- paste0(dirsave_tour, "/", gr_name)
    if (!dir.exists(group_folder)) {
      dir.create(path = group_folder)
    }

    walk(gr_mice, function(ID) {
      ID_folder <- paste0(group_folder, "/", ID)
      if (!dir.exists(ID_folder)) {
        dir.create(path = ID_folder)
      }

        hours_folder <- paste0(ID_folder, "/", "Phases")
        if(!dir.exists(hours_folder)){
          dir.create(path = hours_folder)
        }

        walk(date, function(my_date){
          date_folder <- paste0(hours_folder, "/", my_date)
          if(!dir.exists(date_folder)){
            dir.create(path = date_folder)
          }

          data %>%
            filter(Group == gr_name, IdLabel == ID, Date == my_date) %>%
            group_by(Phase) %>%
            group_walk(~ {
              fwrite(.x, paste0(date_folder, "/", .y, ".csv"))
            }, .keep = TRUE)
    })
  })
})
}


#' Save the 24 hour tour data (either raw or processed)
#'
#' @details inside the dirsave$csv tour_raw or tour_processed directory, creates
#'   one subdirectory per group, inside that one subdirectory per mouse in the
#'   group, and inside that subdirectory per mouse, make a subdirectory for days.
#'   and store all csv file per day.
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_raw}}
#' @param group_mice list of vectors. One list-entry per group, and the vector
#'   contains the mice ids that are in the group.
#' @param dirsave environment containing all the results-directory-paths
#' @param type either "raw" or "processed", indicating the type of the table
#' @export
#' @importFrom data.table fwrite

save_24hour_tour <- function(data, group_mice, dirsave, type) {
  if(type == "raw") {
    dirsave_tour <- dirsave$tour_raw
  } else if(type == "processed") {
    dirsave_tour <- dirsave$tour_processed
  } else {
    stop("wrong type of 24hour tour data\n")
  }
  walk2(group_mice, names(group_mice), function(gr_mice, gr_name) {
    group_folder <- paste0(dirsave_tour, "/", gr_name)
    if (!dir.exists(group_folder)) {
      dir.create(path = group_folder)
    }

    walk(gr_mice, function(ID) {
      mouse_folder <- paste0(group_folder, "/", ID)
      if (!dir.exists(mouse_folder)) {
        dir.create(path = mouse_folder)
      }

      hours_folder <- paste0(mouse_folder, "/", "24hours")
      if(!dir.exists(hours_folder)){
        dir.create(path = hours_folder)
      }

      data %>%
        filter(Group == gr_name, IdLabel == ID) %>%
        group_by(Date) %>%
        group_walk(~ {
          #write_csv2(.x, paste0(hours_folder, "/day", .y, ".csv"))
          fwrite(.x, paste0(hours_folder, "/", .y$Date , ".csv"))
        }, .keep = TRUE)
    })
  })
}

#' Calculate the traveled distance for each mouse and phase.
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @return Data frame contains the total traveled distance, light and dark phase for each mouse.
#' @export
get_locomotion_distance <- function(data) {
  data %>%
    group_by(Group, IdLabel, Age_in_months, Day) %>%
    summarize(
      Distance_light = map2_dbl(Distance, Phase, ~{
          if(.y == "light") { .x } else { 0 }
      }) %>% sum(na.rm = TRUE),
      Distance_dark = map2_dbl(Distance, Phase, ~{
        if(.y == "dark") { .x } else { 0 }
      }) %>% sum(na.rm = TRUE),
      Distance_total = sum(Distance, na.rm = TRUE)
    )
}


#' Calculate the traveled distance in cages per minute
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_dist_1m <- function(data) {
  lgt <- length(data$DateTime_in)

  # mintime and maxtime are set to full minute.
  mintime <- min(data$DateTime_in, na.rm = TRUE) %>% {. - second(.)}
  maxtime <- max(data$DateTime_out, na.rm = TRUE) %>% {. + (60-second(.))}


  #-- times -------------------------------------------------------------------#
  times.1m <- seq(mintime, maxtime, dminutes(1))
  #------------------------------------------------------------------- times --#

  #-- intervals ---------------------------------------------------------------#
  int.1m <- map(times.1m, ~.x %--% ((.x+dminutes(1))-dmicroseconds(1)))
  #--------------------------------------------------------------- intervals --#

  #-- overlap_matrices --------------------------------------------------------#
  # cols are the entries, rows are the interval-bins
  make_overlap_matrix <- function(int_list) {
    (data$DateTime_in %--% data$DateTime_out) %>%
      map(function(my_interval) {
        int_list %>% map(~int_overlaps(my_interval, .x)) %>% unlist()
      }) %>%
      unlist() %>%
      matrix(ncol = lgt, byrow = FALSE)
  }

  overlap_matrix.1m <- make_overlap_matrix(int.1m)
  #-------------------------------------------------------- overlap_matrices --#

  overlaps <- 1:lgt %>% map_int(~ overlap_matrix.1m[,.x] %>% sum(na.rm = TRUE))

  # Get the proportion of segment being in bin.
  #
  # @param bin a time interval (lubridate)
  # @param segment a time interval (lubridate)
  #
  # @return The proportion of segment being in bin.
  #   It is a numeric value between 0 and 1.
  #
  # @details call this function only if you are sure, that there exists an
  #   overlap between bin and segment.
  get_overlap_proportion <- function(bin, segment) {
    if(segment %within% bin) {
      return(1.0)
    }

    if(int_start(segment) < int_start(bin)) {
      overlap.duration <- int_length(int_start(bin) %--% int_end(segment))
      segment.duration <- int_length(segment)
    } else {
      overlap.duration <- int_length(int_start(segment) %--% int_end(bin))
      segment.duration <- int_length(segment)
    }

    overlap.duration / segment.duration
  }

  1:length(times.1m) %>% map_df(~ {
    tibble(id = .x, DateTime_bin = int.1m[[.x]])
  }) %>%
    rowwise() %>%
    mutate(
      Distance = overlap_matrix.1m[id,] %>%
        which() %>%
        reduce(function(acc, nxt) {
          acc <- acc +++
            data$Distance[nxt] *
            get_overlap_proportion(
              DateTime_bin,
              data$DateTime_in[nxt] %--% data$DateTime_out[nxt]
            )
        }, .init = 0)
    ) %>%
    select(-id)
}

#' Calculate the traveled distance in cages every 5 minutes
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_dist_5m <- function(data) {
  lgt <- length(data$DateTime_in)

  # mintime and maxtime are set to full minute.
  mintime <- min(data$DateTime_in, na.rm = TRUE) %>% {. - second(.)}
  maxtime <- max(data$DateTime_out, na.rm = TRUE) %>% {. + (60-second(.))}

  #-- times -------------------------------------------------------------------#
  times.5m <- seq(mintime, maxtime, dminutes(5))
  #------------------------------------------------------------------- times --#

  #-- intervals ---------------------------------------------------------------#
  int.5m <- map(times.5m, ~.x %--% ((.x+dminutes(5))-dmicroseconds(5)))
  #--------------------------------------------------------------- intervals --#

  #-- overlap_matrices --------------------------------------------------------#
  # cols are the entries, rows are the interval-bins
  make_overlap_matrix <- function(int_list) {
    (data$DateTime_in %--% data$DateTime_out) %>%
      map(function(my_interval) {
        int_list %>% map(~int_overlaps(my_interval, .x)) %>% unlist()
      }) %>%
      unlist() %>%
      matrix(ncol = lgt, byrow = FALSE)
  }

  overlap_matrix.5m <- make_overlap_matrix(int.5m)
  #-------------------------------------------------------- overlap_matrices --#

  overlaps <- 1:lgt %>% map_int(~ overlap_matrix.5m[,.x] %>% sum(na.rm = TRUE))

  # Get the proportion of segment being in bin.
  #
  # @param bin a time interval (lubridate)
  # @param segment a time interval (lubridate)
  #
  # @return The proportion of segment being in bin.
  #   It is a numeric value between 0 and 1.
  #
  # @details call this function only if you are sure, that there exists an
  #   overlap between bin and segment.
  get_overlap_proportion <- function(bin, segment) {
    if(segment %within% bin) {
      return(1.0)
    }

    if(int_start(segment) < int_start(bin)) {
      overlap.duration <- int_length(int_start(bin) %--% int_end(segment))
      segment.duration <- int_length(segment)
    } else {
      overlap.duration <- int_length(int_start(segment) %--% int_end(bin))
      segment.duration <- int_length(segment)
    }

    overlap.duration / segment.duration
  }

  1:length(times.5m) %>% map_df(~ {
    tibble(id = .x, DateTime_bin = int.5m[[.x]])
  }) %>%
    rowwise() %>%
    mutate(
      Distance = overlap_matrix.5m[id,] %>%
        which() %>%
        reduce(function(acc, nxt) {
          acc <- acc +++
            data$Distance[nxt] *
            get_overlap_proportion(
              DateTime_bin,
              data$DateTime_in[nxt] %--% data$DateTime_out[nxt]
            )
        }, .init = 0)
    ) %>%
    select(-id)
}

#' Calculate the traveled distance in cages per hour
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_dist_1h <- function(data) {
  lgt <- length(data$DateTime_in)

  # mintime and maxtime are set to full minute.
  mintime <- min(data$DateTime_in, na.rm = TRUE) %>% {. - second(.)}
  maxtime <- max(data$DateTime_out, na.rm = TRUE) %>% {. + (60-second(.))}


  #-- times -------------------------------------------------------------------#
  times.1h <- seq(
    mintime - minute(mintime)*60,
    maxtime + (60-minute(maxtime))*60,
    dhours(1)
  )
  #------------------------------------------------------------------- times --#

  #-- intervals ---------------------------------------------------------------#
  int.1h <- map(times.1h, ~.x %--% ((.x+dhours(1))-dmicroseconds(1)))
  #--------------------------------------------------------------- intervals --#

  #-- overlap_matrices --------------------------------------------------------#
  # cols are the entries, rows are the interval-bins
  make_overlap_matrix <- function(int_list) {
    (data$DateTime_in %--% data$DateTime_out) %>%
      map(function(my_interval) {
        int_list %>% map(~int_overlaps(my_interval, .x)) %>% unlist()
      }) %>%
      unlist() %>%
      matrix(ncol = lgt, byrow = FALSE)
  }

  overlap_matrix.1h <- make_overlap_matrix(int.1h)
  #-------------------------------------------------------- overlap_matrices --#

  overlaps <- 1:lgt %>% map_int(~ overlap_matrix.1h[,.x] %>% sum(na.rm = TRUE))

  # Get the proportion of segment being in bin.
  #
  # @param bin a time interval (lubridate)
  # @param segment a time interval (lubridate)
  #
  # @return The proportion of segment being in bin.
  #   It is a numeric value between 0 and 1.
  #
  # @details call this function only if you are sure, that there exists an
  #   overlap between bin and segment.
  get_overlap_proportion <- function(bin, segment) {
    if(segment %within% bin) {
      return(1.0)
    }

    if(int_start(segment) < int_start(bin)) {
      overlap.duration <- int_length(int_start(bin) %--% int_end(segment))
      segment.duration <- int_length(segment)
    } else {
      overlap.duration <- int_length(int_start(segment) %--% int_end(bin))
      segment.duration <- int_length(segment)
    }

    overlap.duration / segment.duration
  }

  1:length(times.1h) %>% map_df(~ {
    tibble(id = .x, DateTime_bin = int.1h[[.x]])
  }) %>%
    rowwise() %>%
    mutate(
      Distance = overlap_matrix.1h[id,] %>%
        which() %>%
        #keep(~data$Height[.x] > 0) %>%
        reduce(function(acc, nxt) {
          acc <- acc +++
            data$Distance[nxt] *
            get_overlap_proportion(
              DateTime_bin,
              data$DateTime_in[nxt] %--% data$DateTime_out[nxt]
            )
        }, .init = 0)
    ) %>%
    select(-id)
}


#' Calculate the traveled distance in tubes per hour
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_up.1h <- function(data) {
  lgt <- length(data$DateTime_in)

  # mintime and maxtime are set to full minute.
  mintime <- min(data$DateTime_in, na.rm = TRUE) %>% {. - second(.)}
  maxtime <- max(data$DateTime_out, na.rm = TRUE) %>% {. + (60-second(.))}


  #-- times -------------------------------------------------------------------#
  times.1h <- seq(
    mintime - minute(mintime)*60,
    maxtime + (60-minute(maxtime))*60,
    dhours(1)
  )
  #------------------------------------------------------------------- times --#

  #-- intervals ---------------------------------------------------------------#
  int.1h <- map(times.1h, ~.x %--% ((.x+dhours(1))-dmicroseconds(1)))
  #--------------------------------------------------------------- intervals --#

  #-- overlap_matrices --------------------------------------------------------#
  # cols are the entries, rows are the interval-bins
  make_overlap_matrix <- function(int_list) {
    (data$DateTime_in %--% data$DateTime_out) %>%
      map(function(my_interval) {
        int_list %>% map(~int_overlaps(my_interval, .x)) %>% unlist()
      }) %>%
      unlist() %>%
      matrix(ncol = lgt, byrow = FALSE)
  }

  overlap_matrix.1h <- make_overlap_matrix(int.1h)
  #-------------------------------------------------------- overlap_matrices --#

  overlaps <- 1:lgt %>% map_int(~ overlap_matrix.1h[,.x] %>% sum(na.rm = TRUE))

  # Get the proportion of segment being in bin.
  #
  # @param bin a time interval (lubridate)
  # @param segment a time interval (lubridate)
  #
  # @return The proportion of segment being in bin.
  #   It is a numeric value between 0 and 1.
  #
  # @details call this function only if you are sure, that there exists an
  #   overlap between bin and segment.
  get_overlap_proportion <- function(bin, segment) {
    if(segment %within% bin) {
      return(1.0)
    }

    if(int_start(segment) < int_start(bin)) {
      overlap.duration <- int_length(int_start(bin) %--% int_end(segment))
      segment.duration <- int_length(segment)
    } else {
      overlap.duration <- int_length(int_start(segment) %--% int_end(bin))
      segment.duration <- int_length(segment)
    }

    overlap.duration / segment.duration
  }

  1:length(times.1h) %>% map_df(~ {
    tibble(id = .x, DateTime_bin = int.1h[[.x]])
  }) %>%
    rowwise() %>%
    mutate(
      Distance = overlap_matrix.1h[id,] %>%
        which() %>%
        keep(~data$Height[.x] > 0) %>%
        reduce(function(acc, nxt) {
          acc <- acc +++
            data$Height[nxt] *
            get_overlap_proportion(
              DateTime_bin,
              data$DateTime_in[nxt] %--% data$DateTime_out[nxt]
            )
        }, .init = 0)
    ) %>%
    select(-id)
}


#' Create locomotion summary for each crossing length
#'
#' For each crossing, summarize distance, duration and speed.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @export
get_crossing_summary <- function(data) {
  data %>%
    filter(!is.na(Crossing_length)) %>%
    mutate(Speed = Distance / as.numeric(Duration)) %>%
    group_by(Crossing) %>%
    summarize(
      Crossing_length = Crossing_length,
      count = n(),
      across(Distance,
        list(
          sum = ~sum(.x, na.rm = T),
          mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
        .names = "{col}_{fn}"
      ),
      across(Duration,
        list(
          sum = ~sum(.x, na.rm = T),
          min = ~min(.x, na.rm = T), max = ~max(.x, na.rm = T),
          mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
        .names = "{col}_{fn}"
      ),
      across(Speed,
        list(
          min = ~min(.x, na.rm = T), max = ~max(.x, na.rm = T),
          median = ~median(.x, na.rm = T), mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    unique() %>%
    select(
      starts_with(c("Crossing")),
      starts_with(c("Distance", "Duration", "Speed"))
    )
}


#' Create locomotion summary for each crossing length
#'
#' For each crossing length, summarize distance, duration and speed for all mice at once.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @export
get_length_crossing_summary <- function(data) {
  data %>%
    filter(!is.na(Crossing_length)) %>%
    mutate(Speed = Distance / as.numeric(Duration)) %>%
    group_by(Crossing) %>%
    summarize(
      Crossing_length = Crossing_length,
      Distance = sum(Distance),
      Duration = sum(Duration),
      Speed = Distance / as.numeric(Duration)
    ) %>%
    group_by(Crossing_length) %>%
    summarize(
      count = Crossing %>% unique() %>% length(),
      across(Distance,
        list(
          sum = ~sum(.x, na.rm = T),
          mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
        .names = "{col}_{fn}"
      ),
      across(Duration,
        list(
          sum = ~sum(.x, na.rm = T),
          min = ~min(.x, na.rm = T), max = ~max(.x, na.rm = T),
          mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
       .names = "{col}_{fn}"
      ),
      across(Speed,
        list(
          min = ~min(.x, na.rm = T), max = ~max(.x, na.rm = T),
          median = ~median(.x, na.rm = T), mean = ~mean(.x, na.rm = T),
          sd = ~sd(.x, na.rm = T)
        ),
        .names = "{col}_{fn}"
      )
    ) %>%
    unique() %>%
    select(
      starts_with(c("Crossing")), count,
      starts_with(c("Distance", "Duration", "Speed"))
    )
}

#' Calculate the duration in each cage.
#'
#' For each mouse and day, and phase (dark and light) calculate the time spent in each cage.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_total_duration <- function(data) {
  data %>%
    filter(Segment_type == 1) %>% # valid cages
    group_by(Group, IdLabel, Day, Cage) %>%
    summarize(
      Duration_sum = sum(Duration) %>% as.duration(),
      Duration_light = reduce2(Duration, Phase, function(acc, dur, phase) {
        if(phase == "light") { acc + dur } else { acc }
      }, .init = 0) %>% as.duration() %>% round(3),
      Duration_dark = reduce2(Duration, Phase, function(acc, dur, phase) {
        if(phase == "dark") { acc + dur } else { acc }
      }, .init = 0) %>% as.duration() %>% round(3),
    )
}

#' Calculate the distribution of duration in each cage.
#'
#' Calculate the mean, standard deviation, minimum and maximum time spent in each cage for each mouse in day and dark and light phases.
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_distribution_duration <- function(data) {
  data %>%
    filter(Segment_type == 1) %>% # valid cages
    group_by(Group, IdLabel, Day, Cage) %>%
    summarize(
      across(Duration,
        list(
          # total
          min = ~suppressWarnings(min(.x, na.rm = TRUE)) %>% as.duration() %>% round(3),
          max = ~suppressWarnings(max(.x, na.rm = TRUE)) %>% as.duration() %>% round(3),
          mean = ~mean(.x, na.rm = TRUE) %>% as.duration() %>% round(3),
          sd = ~sd(.x, na.rm = TRUE) %>% as.duration() %>% round(3),
          # TODO: this is very generic. Better way of implementation?
          # light
          light_min = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "light") { val } else { NA }
            }) %>% unlist() %>%
              suppressWarnings(min(na.rm = TRUE)) %>%
              as.duration() %>%
              round(3)
          },
          light_max = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "light") { val } else { NA }
            }) %>% unlist() %>%
              suppressWarnings(max(na.rm = TRUE)) %>%
              as.duration() %>%
              round(3)
          },
          light_mean = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "light") { val } else { NA }
            }) %>% unlist() %>%
              mean(na.rm = TRUE) %>%
              as.duration() %>%
              round(3)
          },
          light_sd = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "light") { val } else { NA }
            }) %>% unlist() %>%
              sd(na.rm = TRUE) %>%
              as.duration() %>%
              round(3)
          },
          # dark
          dark_min = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "dark") { val } else { NA }
            }) %>% unlist() %>%
              suppressWarnings(min(na.rm = TRUE)) %>%
              as.duration() %>%
              round(3)
          },
          dark_max = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "dark") { val } else { NA }
            }) %>% unlist() %>%
              suppressWarnings(max(na.rm = TRUE)) %>%
              as.duration() %>%
              round(3)
          },
          dark_mean = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "dark") { val } else { NA }
            }) %>% unlist() %>%
              mean(na.rm = TRUE) %>%
              as.duration() %>%
              round(3)
          },
          dark_sd = ~{
            map2(.x, Phase, function(val, phase) {
              if(phase == "dark") { val } else { NA }
            }) %>% unlist() %>%
              sd(na.rm = TRUE) %>%
              as.duration() %>%
              round(3)
          }
        ),
        .names = "{col}_{fn}"
      )
    )
}


#' Calculate the duration entropy value in each cage.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_entropy_duration <- function(data) {
  entropy <- function(values) {
    (values/sum(values)) %>% # getting the probabilities
      discard(~ .x %in% c(0,1, NA, Inf, -Inf)) %>% # zeros should be ignored
      map_dbl(~ .x*log(.x)) %>%
      sum() %>%
      {-. / log(length(values))} %>% # normalization
      {if(is.nan(.)) 0 else .}
  }
  data %>%
    filter(Segment_type == 1) %>% # valid cages
    group_by(Group, IdLabel, Day, Cage) %>%
    summarize(
      Duration_entropy = entropy(Duration %>% as.numeric()) %>% round(3),
      Duration_entropy_light =
        map2(Duration, Phase, function(val, phase) {
          if(phase == "light") { val } else { NA }
        }) %>% unlist() %>%
        as.numeric() %>%
        entropy() %>%
        round(3),
      Duration_entropy_dark =
        map2(Duration, Phase, function(val, phase) {
          if(phase == "dark") { val } else { NA }
        }) %>% unlist() %>%
        as.numeric() %>%
        entropy() %>%
        round(3)
    )
}



#' read mice ages to merge it with later dailyTourRaw
#'
#' @param filenames will be read if given, otherwise dialog GUI
#'
#' @return some raw data with mice age and ID
#' @export
readAge <- function(filenames = NA) {
  if(any(is.na(filenames))) {
    message.file <- "Select a file containing mice age"
    green(message.file, "Dialog might appear behind rstudio.\n") %>% cat()

    filenames <- tk_choose.files(
      default = paste0(getwd()),
      caption = message.file
    )
    filenames <- filenames[str_detect(filenames, "\\.(csv|xlsx)")]
  }
  # output the raw data
  filenames %>%
    map(
      ~ read_excel(
        .x,
        #as.is = T, row.names = NULL
      )
    ) %>% do.call(what = rbind, args = .)
}



#' clean the age dataframe
#'
#' @param age Data frame obtained by using \code{\link{readAge}}
#'
#' @return mice ID and their ages as a dataframe
#' @export
cleanAge <- function(age) {
  age %>%
  select(`animal ID`, `Age in months (/4.345)`) %>%
  drop_na() %>%
  rename(
    IdLabel = `animal ID`,
    Age_in_months = `Age in months (/4.345)`
  ) %>%
  mutate(
    Age_in_months = Age_in_months %>% as.numeric()
  )

}


#' Count the number of visited cages in time intervals
#'
#' For each day, the length of the time interval can be specified (either per minute or hour).
#' Note that the date on the \code{Time} column is the \code{today()} date. This date is created for plotting purposes.
#' However the time on Time column is correct
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param first_bedding_day The date of the first bedding date as a string (e.g. \code{"2020-08-25"})
#' @param interval_type \code{minute} or \code{hour}. Specify how the time interval should be grouped.
#' @export
count_visited_cages_per_interval <- function(data, first_bedding_day, interval_type="minute"){
  if(!interval_type %in% c("minute", "hour"))
    stop("Wrong value for `interval_type`. Please type `hour` or `minute`.")

  visited_cages <- data %>%
    arrange(Date) %>%
    filter(Segment_type == 1,  # selects the valid cages names
           Date >= first_bedding_day) %>%
    # try this again later
    # mutate(Time = round(DateTime_in, units = "mins")) %>% "mins"))
    # delete seconds and convert string to date
    # NOTE THAT THE DATE ON THE TIME COLUMN ARE NOT VALID : today(). FIX THIS LATER
    mutate(Time = case_when(interval_type=="hour" ~ (paste(today(), format(DateTime_in, format="%H:00:00")) %>% as_datetime(tz = "UTC")),
                            interval_type=="minute" ~ (paste(today(), format(DateTime_in, format="%H:%M:00")) %>% as_datetime(tz = "UTC")),
                            ),
           Bedding_week = (((Date+1) - min(Date)) %>% as.numeric() / 7) %>% ceiling()) %>%

    #select(Date, Day, DateTime_in, Time, IdLabel, Cage) %>%
    group_by(Bedding_week, Date, Time, Age_in_months, IdLabel) %>%
    summarise(total_visited_cages=n()) %>%
    arrange(Date, Time, Age_in_months)
}




#' Calculate the transition distribution in UP/Down tubes
#' For each day Count the number of visited tubes between cage levels (without linear connection on the same level where the height is 0)
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_transition_distr <- function(data){
  # NEED TO BE TESTED!!!!!!
  transition_distr <- data %>%
    # just for tubes. Exclude staying in tubes on same level (type=2 and height=0)
    filter(!is.na(Cage_type),
           Cage_type == 2,
           Height != 0) %>%
    group_by(Week, Day, Phase, Age_in_months, IdLabel, Cage, Height) %>%
    summarise(
      cage_visits_sum = n(),
    )

  # rename cage height to be down and up
  transition_distr$Height <- with(transition_distr, ifelse(Height == 237, "Up", "Down"))

  # reorder the height of the tube so that `UP` comes first
  transition_distr$Height <- factor(transition_distr$Height, levels = c("Up", "Down"))
  transition_distr
}



#' Calculate the mean duration in cage categories
#'
#' For each mouse, calculate the mean time spent in each cage category (food, water, resting cages) in each phase and cage level
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param water_cages Cages that with a water bottle for drinking
#' @param food_cages Cages that have food
#' @param sleeping_cages Cages that are used for resting or sleeping
#' @export
get_mean_duration_cageLevelCategory <- function(data, water_cages, food_cages, sleeping_cages){
  duration_cageLevelCategory <- data %>%
    mutate(
      Cage_level = Cage %>% substr(. ,6 ,6) %>% as.numeric(),
      Cage_category =
        ifelse (Cage %in% water_cages, "water cages",
                ifelse(Cage %in% food_cages, "food cages",
                       ifelse(Cage %in% sleeping_cages, "resting cages", NA
                       )
                )
        )
    ) %>%
    group_by(Week, Day, Phase, Age_in_months, IdLabel, Cage_level, Cage_category) %>%
    # note that the `duration` is still in seconds not converted here to minutes (not divided by 60 yet!)
    summarise(mean_duration = mean(Duration) %>% as.duration() %>% round(3)) %>%
    drop_na()

  # reorder the cage levels to put them in a similar order (to the real cage: 1 -> bottom cages, etc.
  # duration_cageLevelCategory$Cage_level <- factor(data$Cage_level, levels = c(3,2,1))
  duration_cageLevelCategory
}




#' Calculate traveled distance per day and hour
#'
#' Create a data frame that contains total distance values from different phases for each mouse.
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param method Specification of the length of the time interval for summing up the distances. Use either \code{perHour} or \code{perDay}.
#'
#' @export
get_distance <- function(data, phase=NA, method){
    data <- data %>%
      filter(
        if (phase %in% c("dark", "light"))
          Phase == phase
        else
          if (is.na(phase))
            TRUE
        else
          stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
      )%>%
      {if (method == "perHour"){
        # try this again later
        # mutate(Time = round(DateTime_in, units = "mins")) %>% "mins"))
        # delete seconds and convert string to date
        # NOTE THAT THE DATE ON ON THE DATETIME ON COLUMN TIME ARE NOT CORRECT : today(). CHANGE THIS LATER
        #mutate(Time = paste(today(), format(DateTime_in, format="%H:00:00")) %>% as_datetime(tz = "GMT")) %>%
        data %>% mutate(Time = format(DateTime_in, format="%Y/%m/%d %H:00:00") %>% as_datetime(tz = "GMT")) %>%
        # select doesnt work hier (need to ungroup()). Add these variables to summarise!!!
        #select(Day, DateTime_in, Time, IdLabel) %>%
        group_by(Week, Time, Day, Age_in_months, IdLabel) %>%
        summarise(sum_distance_hourly = sum(Distance)) %>%
        arrange(Week, Day, Age_in_months, IdLabel)
      }
    else if (method == "perDay"){
      data %>%
        group_by(Week, Day, Age_in_months, IdLabel) %>%
        summarise(sum_distance_daily = sum(Distance)) %>%
        arrange(Week, Day, IdLabel, Age_in_months)
      }
    else
      stop("Wrong value for `method`. Please type `perHour` or `perDay`.")
    }
}




#' Calculate traveled distance per hour and and manipulate the DateTime_in (Time) to make a stacked bar.
#'
#' Create a data frame that contains total distance values from different phases for each mouse.
#'
#' @param data Dataframe obtained by using \code{\link{get_24hour_tour_processed}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#'
#' @export
#' @importFrom data.table as.ITime
manipulatedDate_distance_hourly <- function(data, phase=NA){
  data %>%
    filter(
      if (phase %in% c("dark", "light"))
        Phase == phase
      else
        if (is.na(phase))
          TRUE
        else
          stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
    )%>%
    # try: . %>% hour()
    mutate(Time = format(DateTime_in, format="%H:00") %>% as.ITime(tz="GMT")) %>%
    # NOTE THAT THE DATE ON ON THE DATETIME ON COLUMN TIME ARE NOT CORRECT : today(). CHANGE THIS LATER
    #mutate(Time = paste(today(), format(DateTime_in, format="%H:00:00")) %>% as_datetime(tz = "GMT")) %>%
    #mutate(Time = format(DateTime_in, format="%Y/%m/%d %H:00:00") %>% as_datetime(tz = "GMT")) %>%
    # select doesnt work hier (need to ungroup()). Add these variables to summarise!!!
    #select(Day, DateTime_in, Time, IdLabel) %>%
    group_by(Week, Time, Day, Age_in_months, IdLabel) %>%
    summarise(sum_distance_hourly = sum(Distance)) %>%
    arrange(Week, Day, Age_in_months, IdLabel)
}




#' Calculate the mean distance per hour for each day
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#'
#' @export
get_mean_hourlyDistance_perDay <- function(data){
  data %>%
    group_by(Week, Day, Age_in_months, IdLabel) %>%
    summarise(mean_hourlyDistance_perDay = mean(sum_distance_hourly/1000, na.rm=TRUE),
              sd_HourlyDistance_perDay = sd(sum_distance_hourly/1000, na.rm = TRUE)) %>%
    arrange(Week, Day, Age_in_months, IdLabel)
}




#' Create total duration with different interval of time length
#'
#' For each mouse calculate the time spent in each cage and level (colony rack level) per day or per hour.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param time_window_type How the duration should be calculated. Use \code{perDay} for total time spent in cages per day and \code{perHour} for duration per hour.
#'
#' @export
get_duration_inCageLevel <- function(data, phase=NA, time_window_type){
  if(time_window_type %in% c("perDay, perHour")){
    stop("Invalid value for `time_window_type`. Plese write `perDay` or `perHour`.")
  }

  cage_distribution <- data %>%
    filter(Cage_type == 1,
           if (phase %in% c("dark", "light"))
              Phase == phase
            else
              if (is.na(phase))
                TRUE
            else
              stop("Wrong input for phase. Please time_window_type `dark`, `light`. Default is all day (24h)")
    )

    if (time_window_type == "perDay"){
      cage_distribution <- cage_distribution %>%
        mutate(Cage_level = Cage %>% substr(. ,6 ,6) %>% as.numeric()) %>%
        group_by(Day, Age_in_months, IdLabel, Cage_level, Cage) %>%
        summarise(
          duration = sum(Duration) %>% as.duration() %>% round(3)) %>%
        drop_na()
    }

    if (time_window_type == "perHour"){
      cage_distribution <- cage_distribution %>%
        mutate(Cage_level = Cage %>% substr(. ,6 ,6) %>% as.numeric(),
             Time = format(DateTime_in, format="%Y/%m/%d %H:00:00") %>% as_datetime(tz = "GMT")) %>%
        group_by(Day, Time, Age_in_months, IdLabel, Cage_level, Cage) %>%
        summarise(duration = sum(Duration) %>% as.duration() %>% round(3)) %>%
        drop_na()
    }

  # reorder the cage levels to put them in a similar order (to the real cage: 1->bottom cage, etc.)
  cage_distribution$Cage_level <-
      factor(cage_distribution$Cage_level, levels = c(3,2,1))

  cage_distribution
}

################### Speed ######################



#' Calculate the speed of each mouse
#'
#' The speed is determined by dividing the traveled distance between two cages/tubs and the duration (time spent in cage).
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @export
get_speed <- function(data) {
  data %>%
    filter(Segment_type == 1  # select valid cages names
    ) %>%
    mutate(Cage_level = Cage %>% substr(. ,6 ,6) %>% as.numeric()) %>%
    group_by(Week, Day, Phase, Cage_level, Cage, Age_in_months, IdLabel) %>%
    summarize(
      Cage_type = Cage_type,
      Height = Height,
      unitLabel_from = unitLabel_from,
      unitLabel_to = unitLabel_to,
      DateTime_in = DateTime_in,
      DateTime_out = DateTime_out,
      Distance = Distance,
      Duration = Duration,
      Speed = (Distance/10)/as.numeric(Duration), # cm/s
      Crossing_length = Crossing_length,
    )
}


#' Calculate the mean speed per day for each mouse
#'
#' @param data Data frame obtained by using \code{\link{get_speed}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @export
get_daily_mean_speed <- function(data, phase=NA){
  data %>%
  filter(
    if (phase %in% c("dark", "light"))
      Phase == phase
    else
      if (is.na(phase))
        TRUE
    else
      stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")) %>%
    group_by(Week, Day, Phase, Age_in_months, IdLabel) %>%
    summarise(mean_speed_daily = mean(Speed, na.rm=TRUE)) %>%
    arrange(Week, Day, Phase, Age_in_months, IdLabel)
}

################################### cage visits ###################################

#' Count the number of visited cages (no tubes) for each mouse per hour or minute
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param time_window_type How the duration should be calculated. Use \code{perHour} for total time spent in cages per hour and \code{perMinute} for duration per minute.
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#'
#' @export
get_count_cage_visits <- function(data, time_window_type, phase= NA){
  count_cage_visits <- data %>%
    arrange(Date) %>%
    filter(Segment_type == 1, Cage_type == 1, # selects the valid cages names
      if (phase %in% c("dark", "light"))
        Phase == phase
      else
        if (is.na(phase))
          TRUE
      else
        stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
    )%>%

    # delete seconds and convert string to date
    # NOTE THAT THE DATE ON ON THE DATE ON COLUMN TIME ARE NOT VALID : today(). FIX THIS LATER
    mutate(Time = if(time_window_type == "perHour") paste(today(), format(DateTime_in, format="%H:%00:00")) %>% as_datetime(tz = "GMT")
                  else if(time_window_type == "perMinute") paste(today(), format(DateTime_in, format="%H:%M:00")) %>% as_datetime(tz = "GMT")
                       else stop("Invalid input fot `time_window_type`. Please insert `perHour` or `perMinute`"))  %>%
    group_by(Date, Day, Time, Phase, Cage, IdLabel, Age_in_months) %>%
    summarise(cage_sum = n()) %>%
    arrange(Date, Time, Age_in_months)
  count_cage_visits
}

################################### spatial ###################################

#' Calculate the niche overlap value for each pair of mice
#'
#' Spatial overlap (Pianka’s index) is a measure of the degree of spatial overlap between
#' the different individuals. To obtain Pianka’s symmetrical index of niche overlap (Pianka 1973)
#' the overlap for each mouse with each of the other mice was calculated.
#' The maximum value of the index is 1 and indicates complete overlap,
#' while the minimum value 0 indicates no overlap.
#'
#' @details Reference: Nachev, Vladislav Nikolaev. "Cognition mediated floral evolution." (2014).
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param method Specify on what basis should the overlap be calculated. Use \code{cage_visits} to calculate the niche overlap value
#' based on the number of visited and \code{duration} for overlap based time spent in cages.
#'
#' @export
get_pairwise_niche_overlap <- function(data, method){

  # create a dataframe with three relevant columns: IdLabel, Cage, duration
  df_overlap <- data %>%
    filter(!is.na(Cage_type)) %>%
    group_by(IdLabel, Cage) %>%
    summarise(pianka = if (method == "duration") sum(Duration)
                       else if (method == "cage_visits") n()
                             else stop("Invalid value for `method`. Please type either `duration` or `cage_visits`")
        )

  cages <- df_overlap$Cage %>% unique()
  IdLabel <- df_overlap$IdLabel %>% unique()


  # add new rows for each mouse that has not visited some cages (duration = 0)
  for (id in IdLabel){
    df_overlap_id <- df_overlap %>% filter(IdLabel == id)
    unvisisted_cages <- setdiff(cages, df_overlap_id %>% filter(IdLabel == id) %>% .$Cage)
    # apply(unvisisted_cages, function(cg){
    for (cg in unvisisted_cages){
      df_overlap[nrow(df_overlap)+1,] <- list(id, cg, 0)
    }
  }

  pairwise <- cbind(t(combn(IdLabel, 2)), 0) # set up pairwise mice list
  pairwise_df_overlap <- pairwise %>% as.data.frame()
  colnames(pairwise_df_overlap)<-c('p1', 'p2', 'pianka_index')
  # pairwise_df_overlap

  pianka_index <- list("numerator" ,"denominator1" ,'denominator2')


  for (pair_idx in 1:nrow(pairwise_df_overlap)) {
    # map2(pairwise_df$p1%>%head(1), pairwise_df$p2%>%head(1), function(p1, p2) {

    for (cage_idx in 1:length(cages)){
      # map2(1:2, cages%>%head(2), function(cg, cage){

      p <- df_overlap %>% filter(Cage == cages[cage_idx], IdLabel %in% c(pairwise_df_overlap[pair_idx, 1], pairwise_df_overlap[pair_idx, 2])) %>% .$pianka

      pianka_index$numerator[cage_idx] <- p[1] * p[2]
      pianka_index$denominator1[cage_idx] <- p[1]**2
      pianka_index$denominator2[cage_idx] <- p[2]**2
    }
    pairwise_df_overlap[pair_idx, 3] <- sum(pianka_index$numerator) / sqrt(sum(pianka_index$denominator1) * sum(pianka_index$denominator2))
  }
  pairwise_df_overlap <- pairwise_df_overlap %>% mutate(pianka_index = pianka_index %>% as.double())
  pairwise_df_overlap


}


#' Calculate the mean pianka index for each mouse
#'
#' Mean spatial overlap (Pianka’s index) is a measure of the degree of spatial overlap between
#' the different individuals. To obtain the mean Pianka’s symmetrical index of niche overlap (Pianka 1973)
#' the overlap for each mouse with each of the other mice was calculated and then a mean value was
#' determined from these data for each individual.
#' The maximum value of the index is 1 and indicates complete overlap,
#' while the minimum value 0 indicates no overlap.
#'
#' @details Reference: Nachev, Vladislav Nikolaev. "Cognition mediated floral evolution." (2014).
#'
#' @param data Data frame obtained by using \code{\link{get_pairwise_niche_overlap}}
#' @param mice_age Data frame contains two columns: The mice ID \code{IdLabel} and their age \code{Age_in_months}
#'
#' @export
get_mean_pianka_index <- function(data, mice_age){

  # make the pianka's index dataframe asymmetric by adding traverse column. Nedded for calculating the mean of each mouse
  combined_pairwise_df_overlap <- rbind(data, data %>% rename(p1=p2, p2=p1)) %>% group_by(p1)

  # sort the dataframe
  combined_pairwise_df_overlap <- combined_pairwise_df_overlap[order(combined_pairwise_df_overlap$p1, combined_pairwise_df_overlap$p2), ]

  # calculate the mean pianka's index for each mouse
  combined_pairwise_df_overlap <- aggregate(combined_pairwise_df_overlap$pianka_index, list(IdLabel = combined_pairwise_df_overlap$p1), FUN=mean) %>% rename(mean_pianka = x)

  # merge mouse age with IdLabel
  combined_pairwise_df_overlap <- merge(combined_pairwise_df_overlap, mice_age)

  combined_pairwise_df_overlap
}


###################################### duration ######################################



#' Calculate total duration with different interval of time length
#'
#' Create a data frame that contains total time spent in cages (no tubes) for each mouse and phase.
#' For each mouse calculate the time spent in each cage and level (colony rack level) per day or per hour.
#' Note that the date on the \code{Time} column is the \code{today()} date. This date is created for plotting purposes.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param time_window_type How the duration should be calculated. Use \code{perDay} for total time spent in cages per day and \code{perHour} for duration per hour.
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#'
#' @export
get_duration_inCages <- function(data, time_window_type, phase= NA){
  duration_inCages <- data %>%
    arrange(Date) %>%
    filter(Segment_type == 1, Cage_type == 1, # selects the valid cages names
           if (phase %in% c("dark", "light"))
             Phase == phase
           else
             if (is.na(phase))
               TRUE
           else
             stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
    )%>%

    # delete seconds and convert string to date
    # NOTE THAT THE DATE ON ON THE DATE ON COLUMN TIME ARE NOT VALID : today(). FIX THIS LATER
    mutate(Time = if(time_window_type == "perHour") paste(today(), format(DateTime_in, format="%H:%00:00")) %>% as_datetime(tz = "GMT")
           else if(time_window_type == "perMinute") paste(today(), format(DateTime_in, format="%H:%M:00")) %>% as_datetime(tz = "GMT")
           else stop("Invalid input fot `type`. Please insert `perHour` or `perMinute`"))  %>%
    group_by(Date, Day, Time, Phase, Cage, IdLabel, Age_in_months) %>%
    summarise(hourly_duration = sum(Duration) %>% as.duration() %>% round(3))
    # arrange(Date, Time, Age_in_months)
    duration_inCages
}




#' Calculate the overlap of timespent in cages between all mice permutations
#'
#' The function sums up all duration overlaps (time spent in same cages) between all mice of different lengths.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#'
#' @export
get_duration_overlap <- function(data){
  DT_data <- data %>%
    filter(!is.na(Cage_type), Segment_type == 1) %>%
    setDT()

  # cols <- c("DateTime_in", "DateTime_out")
  # DT_data[, (cols) := lapply(.SD, as.POSIXct), .SDcols = cols]

  breaks <- DT_data[, {
    tmp <- unique(sort(c(DateTime_in, DateTime_out)))
    .(start = head(tmp, -1L), end = tail(tmp, -1L))
  }, by = Cage]

  result <- DT_data[breaks, on = .(Cage, DateTime_in <= start, DateTime_out >= end), paste(IdLabel, collapse = "+"),
                             by = .EACHI, allow.cartesian = T] %>%
    mutate(lengthinseconds = as.numeric(difftime(DateTime_out, DateTime_in, units = "secs")),
           nr_of_mice = str_count(V1, "\\+") + 1) %>%
    rename(miceID = V1) %>%
    filter(miceID != "NA") # remove "NA" strings


  # not used for now
  sum_duration_overlaping_with_cages <- result %>%
    mutate(miceID = map_chr(strsplit(miceID, "\\+"), ~ toString(sort(.x)))) %>% # sort mice id to avoid duplicate later
    group_by(Cage, nr_of_mice, miceID) %>%
    summarise(sum_overlaps = sum(lengthinseconds) %>% as.duration() %>% round(3))


  sum_duration_overlaping_withOUT_cages <- result %>%
    mutate(miceID = map_chr(strsplit(miceID, "\\+"), ~ toString(sort(.x)))) %>% # sort mice id to avoid duplicate later
    group_by(nr_of_mice, miceID) %>%
    summarise(sum_overlaps = sum(lengthinseconds) %>% as.duration() %>% round(3))

  sum_duration_overlaping_withOUT_cages
  }
