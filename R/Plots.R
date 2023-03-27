# ------------------------------------------------
# Locomotion: Distance
# ------------------------------------------------

#' Plot and save traveled distance as a PNG file.
#'
#' Distance is calculated separately for dark/light/day phases and each day. Data are plotted as a line graph, where each colors indicate the mice age
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_locomotion_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution (Dots per Inch)
#' @export
plot_locomotion_distance_phases_panel_line <- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  p1_day <- ggplot(data, mapping=aes(x=Day, y=Distance_total/1000, color = as.factor(Age_in_months)))+
    geom_line() +
    labs(
      title = "All day",
      x = "Days",
      y = "Distance traveled (m)",
      color = "Age in months") +
    theme_bw()
  # theme(#axis.title = element_blank(),
  #   #plot.margin = unit(c(0,0,0,1), "cm")
  # )

  p2_light <- ggplot(data, mapping=aes(x=Day, y=Distance_light/1000, color = as.factor(Age_in_months))) +
    geom_line() +
    labs(title = "Light phase",
         #  subtitle = "Two mice per age group",
         x = "Days",
         y = "Distance traveled (m)",
         color = "Age in months") + #,
    # color = "Age in months") +
    # theme(axis.title.x=element_blank()) +
    #scale_color_brewer(palette="Dark2") +
    theme_bw() +
    theme(legend.position="none",
          # axis.title = element_blank(),
          # plot.margin = unit(c(0,0.5,0.5,1), "cm")
    )


  p3_dark <- ggplot(data, mapping=aes(x=Day, y=Distance_dark/1000, color = as.factor(Age_in_months))) +
    geom_line() +
    labs(title = "Dark phase",
         # subtitle = "Two mice per age group",
         x = "Days",
         y = "Distance traveled (m)",
         color = "Age in months") +  #scale_color_brewer(palette="Dark2") +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          # plot.margin = unit(c(0,0.5,0,0), "cm")
    )+
    scale_fill_viridis(discrete=T, option = "H")

  locomotion_distance_phases_line <- ggarrange(p2_light, ggarrange(p1_day, p3_dark,
                                                                   ncol=2, nrow=1, common.legend = TRUE, legend="bottom"), ncol = 1, nrow = 2)

  locomotion_distance_phases_line <- annotate_figure(locomotion_distance_phases_line, top = text_grob("Total distance", size = 20))

  ggsave(plot = locomotion_distance_phases_line, filename = file.path(dirsave$jpg_distance, paste0(plot_file_name, ".png")),
         limitsize = FALSE, bg="white", height = plot_height, width = plot_width, dpi = DPI)
  locomotion_distance_phases_line
}




#' Plot and save locomotion_distance_phases and save it as PNG file
#'
#' Each panel contains a comparison between traveled distances in dark and light phase for each mouse
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_locomotion_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_locomotion_distance_phases_line <- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  locomotion_distance_line_phases <-
    data %>%
    ggplot(aes(x=Day)) +
    geom_line(aes(y=Distance_light/1000, color="blue")) + # , color="red", lwd=3
    geom_line(aes(y=Distance_dark/1000, color="red")) +
    #scale_x_discrete(limits = unique(data$Day)) +
    labs(x = "Days", y="Distance traveled (m)",
         title = "Total distance over days (light and dark phase)",
         subtitle = "Mice from different age groups") +
    scale_color_manual(values = c("blue", "red"),
                       labels=c("Light ", "Dark "),
                       name="Phases") +
    # theme(
    #       text = element_text(size = 8),
    #       plot.title = element_text(hjust = 0.5),
    #       legend.key.size = unit(30,"line")
    #       axis.ticks = element_line(size = 4),
    #       axis.ticks.length = unit(0.5, "cm"),
    #       panel.grid = element_line(size = 2)
    #       ) +
    facet_wrap(vars(Age_in_months, IdLabel), labeller = "label_both", ncol=nr_mice_in_ageGroup) +
    # scale_x_continuous(breaks = seq(0,10,1)) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = max(data$Day)))+
    theme_bw()+
  #theme_classic() #+
  geom_rug()

  ggsave(plot = locomotion_distance_line_phases, height = plot_height, width = plot_width,
         filename = file.path(dirsave$jpg_distance, paste0(plot_file_name, ".png")), limitsize = FALSE, dpi = DPI)

  locomotion_distance_line_phases
}




#' Plot and save locomotion-distance as a heatmap and save it as a PNG file.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_locomotion_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param nr_xTicks
#'
#' @export
plot_locomotion_distance_heatmap <- function(data, plot_file_name, plot_height, plot_width, DPI=150, nr_xTicks){
  locomotion_distance_heatmap <- ggplot(data, aes(Day, IdLabel)) +
    geom_raster(aes(fill= Distance_total/1000)) +
    labs(title = "Total Distance",
         y = "Mouse ID",
         fill="Total distance(m)") +
    scale_fill_viridis(discrete=FALSE) +
    scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks))+
    theme_bw()

  ggsave(plot = locomotion_distance_heatmap, filename = file.path(dirsave$jpg_distance, paste0(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, dpi = DPI)
  locomotion_distance_heatmap
  }


#' Plot and save locomotion-distance for dark/light/day as a heatmap and save it as a PNG file.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data locomotion dataframe
#' @param plot_file_name File name to create on disk
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_locomotion_distance_phases_heatmap <- function(data, plot_file_name, plot_height, plot_width, nr_xTicks, DPI=150){
  p1 <- ggplot(data, aes(Day, IdLabel, fill= Distance_dark/1000)) +
    geom_tile() +
    labs(
      tag = " Dark",
      fill = "Traveled distance (m)",
      # title = "Dark phase",
      x = "",
      y = "")  +
    scale_fill_viridis(discrete=FALSE) +
    theme_bw()+
    scale_fill_viridis(discrete=FALSE, option = "H", limits = c(0, max(data$Distance_total)/1000)) +

    # annotate(geom="text", label="Dark", fontface =2, tick=FALSE, print_value = F, size = 4, angle = -90,
    #          x = length(unique(data$Day))+1,
    #          y = length(unique(data$IdLabel))/2) +


    theme(
      plot.tag.position = "right",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

    # scale_x_continuous(breaks = scales::pretty_breaks(n = max(data$Day)))


  p2 <- ggplot(data, aes(Day, IdLabel, fill= Distance_total/1000)) +
    geom_tile() +
    labs(
      # title = "Day",
      tag = " Day",
      x = "",
      y = "Mouse ID",
      fill = "Traveled distance (m)")  +
    theme_bw() +
    scale_fill_viridis(discrete=FALSE, option = "H", limits = c(0, max(data$Distance_total)/1000))+

    # annotate(geom="text", label="Day",fontface =2, tick=FALSE, print_value = F, size = 7, angle = -90,
    #          x = length(unique(data$Day)) + 3,
    #          y = length(unique(data$IdLabel))/2) +

    theme(
      plot.tag.position = "right",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()

    )
    # scale_x_continuous(breaks = scales::pretty_breaks(n = max(data$Day)))
    # scale_x_discrete(breaks = seq(1,max(data$Day),nr_xTicks))



  p3 <- ggplot(data, aes(Day, IdLabel, fill= Distance_light/1000)) +
    geom_tile() +
    labs(
      tag = " Light",
      # title = "Light phase",
      x = "Days",
      y = "",
      fill = "Traveled distance (m)") +
    theme_bw()+
    scale_fill_viridis(discrete=FALSE, option = "H", limits = c(0, max(data$Distance_total)/1000)) +

    # annotate(geom="text", label="Light", fontface =2, tick=FALSE, print_value = F, size = 7, angle = -90,
    #          x = length(unique(data$Day)) + 3,
    #          y = length(unique(data$IdLabel))/2)  +
    theme(
      plot.tag.position = "right"
      # title = element_text(size = 7)
          )+
    # scale_x_continuous(breaks = scales::pretty_breaks(n = max(data$Day)))
     scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks))



  locomotion_distance_phases_heatmap <- ggarrange(p1, p2, p3, ncol=1, nrow=3, common.legend = T, legend="bottom")
  locomotion_distance_phases_heatmap <- annotate_figure(locomotion_distance_phases_heatmap,
                                                        top = text_grob("Total distance", size = 10)) +
  theme_bw()

  ggsave(plot = locomotion_distance_phases_heatmap, filename = file.path(dirsave$jpg_distance, paste0(plot_file_name, ".png")),
         limitsize = FALSE, height = plot_height, width = plot_width, bg = "white", dpi = DPI)

  locomotion_distance_phases_heatmap
}


# ------------------------------------------------
# Locomotion: Duration
# ------------------------------------------------

#' Plot and save total time spent in cages as a heatmap
#'
#' Each panel represents a data for one mouse. Color intensity indicate the time spent in each cage. Days are plotted on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_total_duration}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#'
#' @export
plot_duration_heatmap_total <- function(data, plot_file_name, phase=NA, plot_height, plot_width, DPI=150, nr_xTicks){
  duration_heatmap_total <- data %>%
  ggplot(aes(x=Day,
             y=Cage,
             fill= {if (phase == "dark") Duration_dark/60
                    else if (phase == "light") fill=Duration_light/60
                    else if (is.na(phase)) fill=Duration_sum/60
                    else stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
                    }
             )
         ) +
    geom_tile() +
    labs(
      title = "Total duration",
      x = "Days",
      y = "Cages",
      fill = "Duration (m)") +
    facet_wrap(~ IdLabel, ncol = 2, drop=F) +
    # scale_fill_viridis(option = "H") +
    scale_fill_viridis(discrete=FALSE) +
    # scale_x_continuous(breaks = scales::pretty_breaks(n = max(data$Day)))+
    scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks)) +
    theme_bw()

  ggsave(plot = duration_heatmap_total, filename = file.path(dirsave$jpg_duration, paste0(plot_file_name, ".png")),
         limitsize = FALSE, bg = "white", height = plot_height, width = plot_width, dpi = DPI)
  duration_heatmap_total
}


#' Plot distribution of duration as a heatmap
#'
#' Plot the mean or the standard deviation of time spent in cages for each mouse and day.
#' Each panel represents a data for one mouse. Color intensity indicate (mean duration or sd value) of time spent in each cage.
#' Days are plotted on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_distribution_duration}}
#' @param dist_type Use \code{mean} or \code{sd} for calculating the mean or stadard deviation of the traveled distance
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#'
#' @export
plot_duration_heatmap_distr <- function(data, dist_type=NA, plot_file_name, phase=NA, plot_height, plot_width, DPI=150, nr_xTicks){
  if (dist_type == "mean"){
    duration_heatmap_distr <- data %>%
      ggplot(aes(x=Day,
                 y=Cage,
                 fill= {if (phase == "dark") Duration_dark_mean/60
                   else if (phase == "light") fill=Duration_light_mean/60
                   else if (is.na(phase)) fill=Duration_mean/60
                   else stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
                  })) +
      geom_tile() +
      labs(
        title = "Mean duration",
        x = "Days",
        y = "Cages",
        fill = "Duration (s)") +
      facet_wrap(~ IdLabel, ncol = 2, drop=F) +
      scale_fill_viridis(discrete=FALSE) +
      scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks)) +
      theme_bw()
  }
  else if (dist_type == "sd"){
    duration_heatmap_distr <- data %>%
      ggplot(aes(x=Day,
                 y=Cage,
                 fill= {if (phase == "dark") Duration_dark_sd/60
                   else if (phase == "light") fill=Duration_light_sd/60
                   else if (is.na(phase)) fill=Duration_sd/60
                   else stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
                 })) +
      geom_tile() +
      labs(
        title = "Standard deviation of duration",
        x = "Days",
        y = "Cages",
        fill = "Duration (s)") +
      facet_wrap(~ IdLabel, ncol = 2, drop=F) +
      scale_fill_viridis(discrete=FALSE) +
      scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks)) +
      theme_bw()
  }
  else{
    stop("invalid value for dist_type. Type dist_type='mean' or dist_type='sd'")
  }
    ggsave(plot = duration_heatmap_distr, filename = file.path(dirsave$jpg_duration, paste0(plot_file_name, ".png")),
         bg = "white", height = plot_height, width = plot_width, limitsize = F, dpi = DPI)
    duration_heatmap_distr
}



#' Plot duration entropy as a heatmap
#'
#' Plot the entropy score value for each mouse in each cage.
#' Each panel represents a data for one mouse. Color intensity indicate the entropy value in each cage.
#' Days are plotted on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by the function \code{\link{get_entropy_duration}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#'
#' @export
plot_duration_heatmap_entropy <- function(data, plot_file_name, phase=NA, plot_height, plot_width, DPI=150, nr_xTicks){
  duration_heatmap_entropy_dark <- data %>%
    filter(Cage %in% c(cages, connections)) %>% # cages & connections is defined in the global env
    ggplot(aes(x=Day,
               y=Cage,
               fill= {if (!is.na(phase) & phase == "dark") Duration_entropy_dark/60
                 else if (!is.na(phase) & phase == "light") fill=Duration_entropy_light/60
                 else if (is.na(phase)) fill=Duration_entropy/60
                 else stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
               })
              ) +
    geom_tile() +
    labs(
      title = "Duration entrophy",
      x = "Days",
      y = "Cages",
      fill = "Entropy") +
    facet_wrap(~ IdLabel, ncol = 2, drop=F) +
    scale_x_continuous(breaks = seq(1,max(data$Day),nr_xTicks)) +
    scale_fill_viridis(option = "H") +
    theme_bw()

  ggsave(plot = duration_heatmap_entropy_dark, filename = file.path(dirsave$jpg_duration, paste0(plot_file_name, ".png")),
         limitsize = FALSE, bg = "white", height = plot_height, width = plot_width, dpi = DPI)
  duration_heatmap_entropy_dark
}

# ====================== Duration histograms/freqpoly ======================

#' Plot duration histogram for linear cages
#'
#' Plot the duration with absolute frequency for all possible crossing lengths (number of continuous linear crossing between cages/tubes).
#' Only crossing between cages is considered (no tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis. The function generates two plots for each phase.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param DPI Plot resolution(Dots per Inch)
#' @param plot_width Plot width
#'
#' @export
plot_duration_absoluteFreq_linearCages_hist <- function(data, plot_file_name, log_scale=F, plot_height, plot_width, DPI=150){
  lapply(names(my_phases), function(phase) {
    duration_absoluteFreq_linearCages_hist <- data %>%
      filter(Phase == phase, Crossing_length != 0, Cage_type == 1) %>%
      ggplot(aes(x = Duration/60)) +
      geom_histogram() +
      {if (log_scale == T) {scale_y_log10()}} +
      labs(
        title = "Occurrences of time duration for linear crossing steps in cages",
        subtitle = paste0("Absolute frequency. ", phase, " phase.", max(data$Day), " days. Linear cages"),
        x = "Duration (min)",
        y = "Nr. of occurrences"
      ) +
      facet_grid(Age_in_months ~ Crossing_length, drop = F, scales = "fixed",
                 labeller = labeller (Age_in_months=label_value, Crossing_length=label_both))

    duration_absoluteFreq_linearCages_hist = annotate_figure(duration_absoluteFreq_linearCages_hist,
                                                                       right = text_grob("Age in months", rot = -90))

    ggsave(duration_absoluteFreq_linearCages_hist,
           filename = file.path(dirsave$jpg_duration, phase, "absolute_frequency", paste0(plot_file_name, "_", phase, ".png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)
  })
}




#' Plot and save a customized histogram for duration in linear cages
#'
#' Plot the duration with absolute frequency for a specific number of crossing lengths (number of continuous linear crossing between cages/tubes).
#' Only crossing between cages is considered (no tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param duration_length maximum duration length to be considered on the x axis.
#' @param binwidth The width of the bins. Can be specified as an integer number The width of the bins. Can be specified as an integer number
#' @param max_crossing_length Maximum number of crossing to be considered. Maximum number of crossing to be considered.
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_absoluteFreq_linearCages_hist_customized <- function(data, plot_file_name, duration_length, binwidth, max_crossing_length, log_scale=F, plot_height, plot_width, DPI=150){

  duration_absoluteFreq_linearCages_hist_customized  <- data %>%
    filter(Crossing_length != 0, Crossing_length <= max_crossing_length,
           Cage_type == 1,
           Duration <= duration_length) %>%
    ggplot(aes(x = Duration)) +
    geom_histogram(binwidth = binwidth) +
    {if (log_scale == T) {scale_y_log10()}} +
    labs(
      title = "Occurrences of time duration for linear crossing steps in cages",
      subtitle = paste0("Absolute frequency. 24 hours.", max(data$Day), "days. ", duration_length,"s duration. ", binwidth,"s Binwidth"),
      x = "Duration (sec)",
      y = "Nr. of occurrences"
    ) +
    facet_grid(Age_in_months ~ Crossing_length, drop = F, scales = "fixed",
               labeller = labeller (Age_in_months=label_value, Crossing_length=label_both))

  duration_absoluteFreq_linearCages_hist_customized  = annotate_figure(duration_absoluteFreq_linearCages_hist_customized ,
                                                                         right = text_grob("Age in months", rot = -90))

  ggsave(duration_absoluteFreq_linearCages_hist_customized ,
         filename = file.path(dirsave$jpg_duration, "24hours", "absolute_frequency", paste0(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize= F, bg="white", dpi = DPI)
}


#' Plot and save duration histogram for connection (tubes)
#'
#' Plot absolute frequency for the duration for only crossing in linear, up and down tubes (no cages).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis. The function generates two plots for each phase.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_absoluteFreq_AllTubes_hist <- function(data, plot_file_name, log_scale=F, plot_height, plot_width, DPI=150){
  lapply(c(0, 237, -237), function(height){
    lapply(names(my_phases), function(phase) {
      duration_absoluteFreq_AllTubes_hist <- data %>%
        filter(Phase == phase, Cage_type == 2, Height == height)

      {if (height == 0) height = 'Linear tubes'
        else if (height > 0) height = 'Tube up'
        else if (height < 0) height = 'Tube down'}

      duration_absoluteFreq_AllTubes_hist <- duration_absoluteFreq_AllTubes_hist %>%
        ggplot(aes(x = Duration/60)) +
        geom_histogram() +
        {if (log_scale == T) {scale_y_log10()}} +
        labs(
          title = "Occurrences of time duration for crossing steps in tubes",
          subtitle = paste0("Absolute frequency. ", phase, " phase. ", max(data$Day), " days. ",  height),
          x = "Duration (min)",
          y = "Nr. of occurrences"
        ) +
        facet_grid(Age_in_months ~ Crossing_length, drop = F, scales = "fixed",
                   labeller = labeller (Age_in_months=label_value, Crossing_length=label_both))

      duration_absoluteFreq_AllTubes_hist = annotate_figure(duration_absoluteFreq_AllTubes_hist,
                                                                   right = text_grob("Age in months", rot = -90))

      ggsave(duration_absoluteFreq_AllTubes_hist,
             # filename = paste0("Results/jpg/duration/",
             #                   phase, "/absolute_frequency/duration_absoluteFreq_logY_tubes_hist_",
             #                   phase, "_", height, '.jpg'),
             filename = file.path(dirsave$jpg_duration, phase, "absolute_frequency", paste0(plot_file_name, "_", phase, "_", height, ".png")),
             height = plot_height, width = plot_width, bg="white", dpi = DPI)
    })
  })
}


#' Plot and save duration histogram for connection (tubes)
#'
#' Plot absolute frequency for the duration for only crossing in up and down tubes (no cages or linear tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param duration_length maximum duration length to be considered on the x axis.
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param binwidth The width of the bins. Can be specified as an integer number The width of the bins. Can be specified as an integer number
#'
#' @export
plot_duration_absoluteFreq_upDownTube_hist <- function(data, plot_file_name, duration_length, log_scale=F, plot_height, plot_width, DPI=150, binwidth = binwidth){
  lapply(c(237, -237), function(height){
    duration_absoluteFreq_upDownTube_hist <- data %>%
      filter(Cage_type == 2,
             Height == height,
             Duration <= duration_length)

    {if (height > 0) height = 'Tube up'
      else if (height < 0) height = 'Tube down'}

    duration_absoluteFreq_upDownTube_hist <- duration_absoluteFreq_upDownTube_hist %>%
      ggplot(aes(x = Duration)) +
      geom_histogram(binwidth = binwidth) +
      {if (log_scale == T) {scale_y_log10()}} +
      labs(
        title = "Occurrences of time duration for crossing steps in tubes",
        subtitle = paste0("Absolute frequency. 24 hours. ", max(data$Day), " days. ",  height,". ", duration_length,"s duration"),
        x = "Duration (sec)",
        y = "Nr. of occurrences"
      ) +
      facet_grid(Age_in_months ~ Crossing_length, drop = F, scales = "fixed",
                 labeller = labeller (Age_in_months=label_value, Crossing_length=label_both))

    duration_absoluteFreq_upDownTube_hist = annotate_figure(duration_absoluteFreq_upDownTube_hist,
                                                      right = text_grob("Age in months", rot = -90))

    ggsave(duration_absoluteFreq_upDownTube_hist,
           # filename = paste0("Results/jpg/duration/24hours/absolute_frequency/duration_absoluteFreq_tube_hist_", height, '.jpg'),
           filename = file.path(dirsave$jpg_duration, "24hours", "absolute_frequency", paste0(plot_file_name, "_", height,".png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)

  })
}




#' Plot and save duration histogram for connection (tubes)
#'
#' Plot absolute frequency for the duration in linear tubes (no cages or up/down tubes) where the height is zero, for a specific number of crossing lengths.
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param duration_length maximum duration length to be considered on the x axis. to be considered on the x axis.
#' @param binwidth The width of the bins. Can be specified as an integer number
#' @param max_crossing_length Maximum number of crossing to be considered.
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_absoluteFreq_LinearTube_hist_customized <- function(data, plot_file_name, duration_length, binwidth, max_crossing_length, log_scale=F, plot_height, plot_width, DPI=150){
  lapply(c(0), function(height){
    duration_absoluteFreq_LinearTube_hist_customized <- data %>%
      filter(Cage_type == 2,
             Height == height,
             Duration <= duration_length,
             Crossing_length < max_crossing_length)

    {if (height == 0) height = 'Linear tubes'}

    duration_absoluteFreq_LinearTube_hist_customized <- duration_absoluteFreq_LinearTube_hist_customized %>%
      ggplot(aes(x = Duration)) +
      geom_histogram(binwidth = binwidth) +
      {if (log_scale == T) {scale_y_log10()}} +
      labs(
        title = "Occurrences of time duration for crossing steps in tubes",
        subtitle = paste0("Absolute frequency. 24 hours. ", max(data$Day), "days. ", duration_length,"s duration. ", binwidth,"s Binwidth"),
        x = "Duration (sec)",
        y = "Nr. of occurrences"
      ) +
      facet_grid(Age_in_months ~ Crossing_length, drop = F, scales = "fixed",
                 labeller = labeller (Age_in_months=label_value, Crossing_length=label_both))

    duration_absoluteFreq_LinearTube_hist_customized = annotate_figure(duration_absoluteFreq_LinearTube_hist_customized,
                                                           right = text_grob("Age in months", rot = -90))

    ggsave(duration_absoluteFreq_LinearTube_hist_customized,
           filename = file.path(dirsave$jpg_duration, "24hours", "absolute_frequency", paste0(plot_file_name,"_",binwidth,"bin", ".png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)
  })
}




#' Plot and save the frequency of duration as polygon graph
#'
#' Plot the duration with absolute frequency for specific number of crossing length (number of continuous linear crossing between cages/tubes).
#' Only crossing in linear cages are considered (no tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param duration_length maximum duration length to be considered on the x axis.
#' @param max_crossing_length Maximum number of crossing to be considered.
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_absoluteFreq_freqpoly_linearCages <- function(data, plot_file_name, duration_length, max_crossing_length, log_scale=F, plot_height, plot_width, DPI=150){
  duration_freqpoly_linearCages <- data %>%
    filter(
      Cage_type == 1, # linear cages
      Duration <= duration_length,
      Crossing_length == max_crossing_length
    )  %>%
    ggplot(aes(x=Duration, color=as.factor(Age_in_months))) +
    geom_freqpoly() +
    {if (log_scale == T) {scale_y_log10()}} +
    labs(
      title =  paste0("Occurrences of time duration for linear crossing steps in cages"),
      subtitle = paste0("Absolute frequency. 24 hours. ", max(data$Day), "days. ", duration_length,"s duration. ", "Crossing_length: ", max_crossing_length),
      x= "Duration (sec)",
      y= "Nr. of occurrences",
      color = "Age in months"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  ggsave(plot = duration_freqpoly_linearCages,
         filename = file.path(dirsave$jpg_duration, "24hours", "absolute_frequency", paste0(plot_file_name, "_", max_crossing_length,"crossing_length", ".png")),
         height = plot_height, width = plot_width, bg="white", dpi = DPI)
}




#' Plot and save the frequency of duration as polygon graphs
#'
#' Plot the duration with absolute frequency for specific number of crossing length (number of continuous linear crossing between cages/tubes).
#' Only crossing in tubes is considered (no cages). The function can cut out irrelevant duration values using \code{duration_limit_linear_tubes} and \code{duration_limit_linear_tubes}.
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis. The function generatet wo plots for each phase.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param max_crossing_length Maximum number of crossing to be considered.
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param duration_limit_linear_tubes Specify the limit of the duration value on the x-axis when plotting linear tupes
#' @param duration_limit_UpDown_tubes Specify the limit of the duration value on the x-axis when plotting up and down tupes
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_absoluteFreq_freqpoly_tubes <- function(data, plot_file_name, max_crossing_length, log_scale=F, duration_limit_linear_tubes, duration_limit_UpDown_tubes, plot_height, plot_width, DPI=150){

  for(height in c(0, 237, -237)){
    {if (height == 0) duration = duration_limit_linear_tubes else duration = duration_limit_UpDown_tubes}
    duration_freqpoly_tubes <- data %>%
      filter(
        Cage_type == 2, # tube type
        Duration <= duration,
        Height ==  height
        #Crossing_length == 1
      )

    {if (height == 0) height = 'Linear tubes'
      else if (height > 0) height = 'Tube up'
      else if (height < 0) height = 'Tube down'}

    duration_freqpoly_tubes <- duration_freqpoly_tubes %>%
      ggplot(aes(x=Duration, color=as.factor(Age_in_months))) +
      geom_freqpoly() +
      {if (log_scale == T) {scale_y_log10()}} +
      labs(
        title =  paste0("Occurrences of time duration for crossing steps in tubes"),
        subtitle = paste0("Absolute frequency. 24 hours. ", max(data$Day), "days. ", height, duration,"s duration. ", "Crossing_length: ", max_crossing_length),
        x= "Duration (sec)",
        y= "Nr. of occurrences",
        color = "Age in months"
      ) +
      theme_bw() +
      theme(legend.position = "bottom")


    ggsave(plot = duration_freqpoly_tubes,
           filename = file.path(dirsave$jpg_duration, "24hours", "absolute_frequency", paste0(sep = "_", plot_file_name, "24hours", height, ".png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)
  }
}


# ====================== Duration histograms/freqpoly (relative freq)======================

#' Plot and save a customized histogram for duration in linear cages
#'
#' Plot the duration with relative frequency for a specific number of crossing lengths (number of continuous linear crossing between cages/tubes).
#' Only crossing between cages is considered (no tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param max_duration_length maximum duration length
#' @param cross_length length of crossing between cages
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#'
#' @export
plot_duration_relativeFreq_LinearCages_hist_customized <- function(data, plot_file_name, max_duration_length=NULL, cross_length=NULL, log_scale=F, plot_height, plot_width, DPI=150){
  for (phase in names(my_phases)){
    func_duration_relativeFreq_cages_hist <- lapply(Age_group_mice$Age_in_months, function(age){
      plot_duration_relativeFreq_cages_hist <- data %>%
        filter(
               Age_in_months == age,
               Phase == phase,
               Cage_type == 1, # linear cages
               {if (!is.null(cross_length)) Crossing_length == cross_length else Crossing_length != 0},
               {if (!is.null(max_duration_length)) Duration <= max_duration_length else TRUE}
               # Crossing_length != 0,
               # Duration <= duration_length
        ) %>%
        ggplot(aes(x=Duration, y=stat(..count..)/sum(..count..))) +
        geom_histogram() +
        {if (log_scale == T) {scale_y_log10()}} +

        facet_wrap(Age_in_months~ Crossing_length, scales = "fixed", drop = F, nrow = 1,
                   labeller = labeller(Age_in_months=label_both, Crossing_length=label_both)) +
        theme(axis.title = element_blank())

      plot_duration_relativeFreq_cages_hist
    })

    allplots <- ggarrange(plotlist = func_duration_relativeFreq_cages_hist, ncol = 1)

    allplots <- annotate_figure(allplots,
                                top = paste0("Occurrences of time duration for linear crossing steps in cages\n",
                                             "Relative frequency. ", phase, " phase. ", max(data$Day), " days."),
                                bottom = text_grob("Duration (sec)"),
                                # right = text_grob("Age in months", rot = -90),
                                left = text_grob("Relative frequency", rot = 90)
    )

    ggsave(plot = allplots,filename = file.path(dirsave$jpg_duration, phase, "relative_frequency", paste(sep = "_", plot_file_name, phase, "linearCages.png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)
  }
}






#' Plot and save duration histogram for connection (tubes)
#'
#' Plot relative frequency for the duration tubes (linear, up and down tubes) for a specific number of crossing lengths.
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param max_duration_length maximum duration length
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @export
plot_duration_relativeFreq_tubes_hist_customized <- function(data, plot_file_name, max_duration_length=NULL, log_scale=F, plot_height, plot_width, DPI=150){
  lapply(c(0, 237, -237), function(height){
    lapply(names(my_phases), function(phase) {
      func_duration_relativeFreq_tubes_hist <- lapply(Age_group_mice$Age_in_months, function(age){
        plot_duration_relativeFreq_tubes_hist <- data %>%
          filter(Age_in_months == age,
                 Phase == phase,
                 Cage_type == 2, # tubes cages
                 Height ==  height,
                 {if (!is.null(max_duration_length)) Duration <= max_duration_length else TRUE}
          )
        if(plot_duration_relativeFreq_tubes_hist %>% nrow() != 0){
          plot_duration_relativeFreq_tubes_hist <- plot_duration_relativeFreq_tubes_hist %>%
            ggplot(aes(x=Duration, y=stat(..count..)/sum(..count..))) +
            geom_histogram() +
            facet_grid(Age_in_months ~ Crossing_length, scales = "free_y", drop = F,
                       labeller = labeller(Age_in_months=label_value, Crossing_length=label_both)) +
            #  facet_wrap(~ Age_in_months, scales = "fixed", nrow = 1, strip.position = "right") +
            theme(axis.title = element_blank())
        }
        # plot_duration_relativeFreq_tubes_hist
      })

      {if (height == 0) height = 'Linear tubes'
        else if (height > 0) height = 'Tube up'
        else if (height < 0) height = 'Tube down'}

      allplots <- ggarrange(plotlist = func_duration_relativeFreq_tubes_hist, ncol = 1)
      allplots <- annotate_figure(allplots,
                                  top = paste0("Occurrences of time duration for crossing steps in tubes\n",
                                               "Relative frequency. ", phase, " phase. ", max(data$Day)," days. ", height),
                                  bottom = text_grob("Duration (sec)"),
                                  right = text_grob("Age in months", rot = -90),
                                  left = text_grob("Relative frequency", rot = 90)
      )

      ggsave(plot = allplots,filename = file.path(dirsave$jpg_duration, phase, "relative_frequency", paste(sep="_", plot_file_name, height, ".png")),
             height = plot_height, width = plot_width, bg="white", dpi = DPI)
    })

  })

}


#' Plot and save the frequency of duration as polygon graph
#'
#' Plot the duration with relative frequency for specific number of crossing length (number of continuous linear crossing between cages/tubes).
#' Only crossing in linear cages are considered (no tubes).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param max_duration_length maximum duration length
#' @param cross_length length if crossing
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @export
plot_duration_relativeFreq_freqpoly_linearCages_customized <- function(data, plot_file_name, cross_length, max_duration_length=NULL,log_scale=F, plot_height, plot_width, DPI=150){
  if(class(cross_length) != "numeric")
    stop("Wrong type of `cross_length`. Must be integer value")

  # create an empty data frame
  df_duration_freqpoly_linearCages <- data.frame(Date=as.Date(character()),
                                                 File=character(),
                                                 User=character(),
                                                 stringsAsFactors=FALSE)
  for (phase in names(my_phases)){
    for (age in Age_group_mice$Age_in_months){

      df_to_bind <- data %>%
        #group_by(Age_in_months) %>%
        filter(Age_in_months == age,
               Phase == phase,
               Cage_type == 1, # linear cages
               {if (!is.null(max_duration_length)) Duration <= max_duration_length else TRUE},
               Crossing_length == cross_length
        )

      df_duration_freqpoly_linearCages <- rbind(df_duration_freqpoly_linearCages, df_to_bind)
    }

    plot_duration_freqpoly_linearCages <- df_duration_freqpoly_linearCages %>%
      ggplot(aes(x=Duration, y=stat(..count..)/sum(..count..), color=as.factor(Age_in_months))) +
      geom_freqpoly() +
      labs(
        title =  paste0("Occurrences of time duration for linear crossing steps in cages"),
        subtitle = paste0("Relative frequency. ", phase, " phase.", max(data$Day), " days. Crossing length: ", cross_length),
        x= "Duration (sec)",
        y= "Relative frequency"
      ) +
      {if (log_scale == T) {scale_y_log10()}} +
      theme_bw() +
      theme(legend.position = "bottom")


    ggsave(plot = plot_duration_freqpoly_linearCages,filename = file.path(dirsave$jpg_duration, phase, "relative_frequency", paste(sep="_", plot_file_name, phase, "linearCages.png")),
           height = plot_height, width = plot_width, bg="white", dpi = DPI)
    }
}



#' Plot and save the frequency of duration as polygon graph
#'
#' Plot the duration with relative frequency for specific number of crossing length (number of continuous linear crossing between cages/tubes).
#' Only crossing in tubes (linear, up and down tubes) are considered (no cages).
#' Set the parameter \code{log_scaled} to \code{TRUE} to scale the values on the y-axis.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_24hour_tour_processed}}
#' @param plot_file_name File name to create on disk
#' @param max_duration_length maximum duration length
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @export
plot_duration_relativeFreq_freqpoly_tubes_customized <- function(data, plot_file_name, max_duration_length=NULL,log_scale=F, plot_height, plot_width, DPI=150){
  # create an empty data frame
  df_duration_freqpoly_tubes <- data.frame(Date=as.Date(character()),
                                                   File=character(),
                                                   User=character(),
                                                   stringsAsFactors=FALSE)

  for (phase in names(my_phases)){
    for(height in data$Height%>%unique()){
      for (age in Age_group_mice$Age_in_months){

        df_to_bind <- data %>%
          filter(Age_in_months == age,
                 Phase == phase,
                 Cage_type == 2, # tube type
                 {if (!is.null(max_duration_length)) Duration <= max_duration_length else TRUE},
                 Height ==  height
          )

        df_duration_freqpoly_tubes <- rbind(df_duration_freqpoly_tubes, df_to_bind)
      }

      {if (height == 0) height = 'Linear tubes'
        else if (height > 0) height = 'Tube up'
        else if (height < 0) height = 'Tube down'}

      plot_duration_freqpoly_tubes <- df_duration_freqpoly_tubes %>%
        ggplot(aes(x=Duration, y=stat(..count..)/sum(..count..), color=as.factor(Age_in_months))) +
        geom_freqpoly() +
        labs(
          title =  paste0("Occurrences of time duration for crossing steps in tubes"),
          subtitle = paste0("Relative frequency. ", phase, " phase. ", height, ". ", max(data$Day), " days"),
          x= "Duration (sec)",
          y= "Relative frequency"
        ) +
        theme_bw() +
        theme(legend.position = "bottom")

      ggsave(plot = plot_duration_freqpoly_tubes,
             filename = file.path(dirsave$jpg_duration, phase, "relative_frequency", paste(sep="_", plot_file_name, phase, height,".png")),
             height = plot_height, width = plot_width, bg="white", dpi = DPI)
      }
    # remove all rows from df
    df_duration_freqpoly_tubes <- df_duration_freqpoly_tubes[0,]


  }
}


#' Plot and save the number of cage visits in time intervals
#'
#' For one specific mouse and each day, plot the total number of visited cages as a smoothed line.
#' Each panel represents data for one week. Data points (line/dots) indicate the days of each week. The first day in each panel is the day in which the bedding has been changed.
#' The input data, that include the number of cage visits, can be be both grouped per hour or minute. See \code{\link{help(count_visited_cages_per_interval)}} for more information.
#' Use \code{span} to control the "wiggliness" of the default loess smoother.
#' The time interval of interest can be specified using the parameters \code{from} and \code{to} which are the upper and lower time limits (in hour)
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{count_visited_cages_per_interval}}
#' @param plot_file_name File name to create on disk
#' @param mouseID Choose one mouse from the entire data set. The \code{mouseID} must be a \code{String} value.
#' @param week Use `all` to include data from all weeks or enter the number of the week as an integer number
#' @param from Starting time of the interval (e.g. from="06:00")
#' @param to Ending time of the interval (e.g. to="13:00")
#' @param plot_width Plot width
#' @param plot_height Plot height
#' @param graph_type Defines the plot type. It can be either \code{smooth}, \code{line} or \code{point}
#' @param span Floating number that controls the amount of smoothing for the default loess smoother. Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
#' @param DPI Plot resolution(Dots per Inch)
#' @importFrom cowplot plot_grid
#' @export
plot_cageVisits_after_bedding_change <- function(data, plot_file_name, mouse, week, from, to, graph_type="smooth", span=0.75, plot_width, plot_height, DPI=150){
  palette_OkabeIto_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  n_week <- max(data$Bedding_week)

  # check the type of the time interval to plot it on the x-axis text title
  if(var(format(as.POSIXct(data$Time), format = "%M"))==0)
    interval_type = "per hour"
  else
    interval_type = "per minute"

  if(week == "all"){
    data_interval <- data %>%  filter(
      IdLabel == mouse$IdLabel,
      format(Time, format="%H:%M") >= from & format(Time, format="%H:%M") <= to)

    # make list of plots
    plot_cageVisits_after_changing_bedding <- lapply(split(data_interval, f=data_interval$Bedding_week), function(i) {
      ggplot(i, aes(x=(Time), y=total_visited_cages, group=Date ,color=as.factor(Date))) +

        {if(graph_type == "smooth") geom_smooth(se=F, span=span)
          else if(graph_type == "line") geom_line()
          else if(graph_type == "point") geom_point()
          else stop("Invalid graph_type! Please type: `smooth`, `line` ,`point`")
        } +
        scale_x_datetime(breaks = scales::date_breaks("2 hour"), date_labels = "%H:%M") +
        labs(
          title = "Activity after bedding change",
          subtitle = paste("Mouse ID: ", mouse$IdLabel, ", mouse age: ", mouse$Age_in_months , "months"),
          x = "Time(hour)",
          y = paste0("Nr. of visited cages ", interval_type),
          color = "Days") +
        scale_y_continuous(limit=c(0,NA),oob=squish)+
        scale_color_manual(values = palette_OkabeIto_black)+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme_bw()

    })
    # plot as grid in 1 columns
    plot_cageVisits_after_changing_bedding <- plot_grid(plotlist = plot_cageVisits_after_changing_bedding,
                                                                 ncol = 1, align = 'v',
                                                                 facet_grid = IdLabel~Bedding_week)
  }

  else if((class(week) == "numeric") && (week%%1==0)){
    #activity for one week
    plot_cageVisits_after_changing_bedding <-
      data %>%
      filter(Bedding_week == 1,
             # Date == "2020-08-26", # Date == "2020-09-05"
             IdLabel == mouse$IdLabel,
             format(Time, format="%H:%M") >= from & format(Time, format="%H:%M") <= to) %>%
      ggplot(aes(x=(Time), y=total_visited_cages, group=Date ,color=as.factor(Date))) +

      {if(graph_type == "smooth") geom_smooth(se=F, span=span)
        else if(graph_type == "line") geom_line()
        else if(graph_type == "point") geom_point()
        else stop("Invalid graph_type! Please type: `smooth`, `line` ,`point`")
      } +

      scale_x_datetime(breaks = scales::date_breaks("2 hour"), date_labels = "%H:%M") +
      labs(
        title = "Activity after bedding change",
        subtitle = paste("Mouse ID: ", mouse$IdLabel, ", mouse age: ", mouse$Age_in_months , "months"),
        x = "Time(hour)",
        y = paste0("Nr. of visited cages ", interval_type),
        color = "Days") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      theme_bw()+
      scale_color_manual(values = palette_OkabeIto_black)
  }


  # else stop("wrong type for `week` argument!. Please type 'all' or a valid integer number.")

  ggsave(plot = plot_cageVisits_after_changing_bedding,
         filename = file.path(dirsave$jpg_cage_visits, paste0(plot_file_name,"_", interval_type,"_", graph_type,"_", "Mouse",mouse$IdLabel, ".png")),
         limitsize = FALSE, bg="white", width = plot_width, height = plot_height, dpi = DPI)
  plot_cageVisits_after_changing_bedding
}




#' Plot and save weekly transition distribution in UP/Down tubes as a boxplot
#'
#' For each week and phase plot the number of visits in tubes where mice can go up and down (linear connections on the same level where height is 0 are not considered).
#' Each boxplot indicates data for one mouse, which is the number of transitions in tubes for one week.
#' A box plot is created from 7 days (one week), with each day containing the sum of all visits on that day.
#' Each column-of-panels represents data for one week. The first row-panels include data for mice that use the tubes to go up (UP-tube);
#' the second row-of-panels include tubes that are used for down transitions in the colony rack setup.
#' Colors indicate the tube type number (e.g. \code{Tube 1:2} is the connection between first and second level in both direction, a.k.a. from level 1 to 2 and from 2 to 1).
#' Values on the x-axis refer to the mice ID.
#' The figure is saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_transition_distr}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param weeks Vector of number of weeks to be considered i.g. c(1,5)
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_transition_tube_xAxisID <- function(data, plot_file_name, phase=NA, weeks=NA, plot_height, plot_width, DPI=150){
  # transactions in tubes up and down (nr of visited tubes) - x-axis:IdLabel
  transition_tube_xAxisID <- data %>%
    filter({ifelse (any(is.na(weeks)), TRUE, (Week %in% weeks))},
           {if (phase %in% c("dark", "light"))
             Phase == phase
             else
               if (is.na(phase))
                 TRUE
             else
               stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
           }) %>%
    ggplot(aes(x=IdLabel, y = cage_visits_sum, fill = Cage)) +  #color=as.factor(Day)
    stat_boxplot(geom="errorbar") +
    geom_boxplot(position=position_dodge(0.8), outlier.alpha = 0.2) +
    #geom_jitter(position=position_dodge(0.8), shape=21, size=1) +
    labs(title = "Number of transitions",
         subtitle = paste0("Comparison of UP tube and DOWN tube in the ", phase, " phase"),
         x = "Mouse ID",
         y = "Nr. of visitedted tubes / day") +
    theme_bw() +

    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))+
    facet_grid(Height ~ Week,
               #labeller = as_labeller(c(`237` = "Tube Up", `-237` = "Tube Down"))
               labeller = labeller (Week=label_both, Height=label_value),
               drop = F
    )

  ggsave(plot = transition_tube_xAxisID,
         filename = file.path(dirsave$jpg_cage_visits, paste(sep="_", plot_file_name, phase,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  transition_tube_xAxisID
}



#' Plot and save weekly transition distribution in UP/Down tubes as a boxplot
#'
#' For each week and phase plot the number of visits in tubes where mice can go up and down (linear connections on the same level where height is 0 are not considered).
#' Each boxplot indicates data for one mouse, which is the number of transitions in tubes for one week.
#' A box plot is created from 7 days (one week), with each day containing the sum of all visits on that day.
#' Each column-of-panels represents one mouse, The first row-panels include data for mice that use the tubes to go up (UP-tube);
#' the second row-of-panels include tubes that are used for down transitions in the colony rack setup.
#' Colors indicate the tube type number (e.g. \code{Tube 1:2} is the connection between the level 1 and 2 with no movement direction a.k.a. does not mean the movement from level 1 one to level 2).
#' Values on the x-axis refer to the week number.
#' The figures are saved as PNG files in the corresponding phase-folder in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_transition_distr}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param weeks Vector of number of weeks to be considered i.g. c(1,5)
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_transition_tube_xAxisWeek <- function(data, plot_file_name, phase, weeks=NULL, plot_height, plot_width, DPI=150){
  # transactions in tubes up and down (nr of visited tubes) - x-axis:Week
  transition_tube_xAxisWeek <- data %>%
    filter({ifelse (any(is.na(weeks)), TRUE, (Week %in% weeks))},
           {if (phase %in% c("dark", "light"))
             Phase == phase
             else
               if (is.na(phase))
                 TRUE
             else
               stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
           }) %>%
    ggplot(aes(x=as.factor(Week), y = cage_visits_sum, fill = Cage)) +  #color=as.factor(Day)
    stat_boxplot(geom="errorbar") +
    geom_boxplot(position=position_dodge(0.8), outlier.alpha = 0.2) +
    labs(title = "Number of transitions",
         subtitle = paste0("Comparison of UP tube and DOWN tube in the ", phase, " phase"),
         x = "Week",
         y = "Nr. of visitedted tubes / day") +
    theme_bw() +
    facet_grid(Height ~ IdLabel,
               #labeller = as_labeller(c(`237` = "Tube Up", `-237` = "Tube Down"))
               labeller = labeller (Week=label_both, Height=label_value)
    )

  ggsave(plot = transition_tube_xAxisWeek,
         filename = file.path(dirsave$jpg_cage_visits, paste(plot_file_name, "_", phase,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  transition_tube_xAxisWeek
}


#' Plot and save distribution of duration in cages
#'
#' Plot the mean of time spent in each cage type(food, water, rest cages) for each level in the colony rack setup.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_mean_duration_cageLevelCategory}}
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param weeks Vector of number of weeks to be considered i.g. c(1,5)
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param plot_width Plot width
#' @param plot_height Plot height
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_distr_cageLevelCategory <- function(data, plot_file_name, phase, weeks=NA, log_scale=TRUE, plot_width, plot_height, DPI=150){
  plot_duration_level <- data %>%
    filter({ifelse (any(is.na(weeks)), TRUE, (Week %in% weeks))},
           {if (phase %in% c("dark", "light"))
             Phase == phase
             else
               if (is.na(phase))
                 TRUE
             else
               stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
           }) %>%
    rename(level = Cage_level) %>%
    ggplot(aes(x=Cage_category, y=mean_duration, fill=as.factor(Age_in_months))) +
    geom_boxplot(position=position_dodge(0.8), outlier.alpha=0.2) +
    facet_grid(level ~ Week, drop = F,
               #labeller=as_labeller(c(`1` = "Level 1", `2` = "Level 2", `3` = "Level 3")),
               labeller = labeller(Week = label_both, level = label_both)
    ) +
    {if(log_scale==TRUE) scale_y_log10()} +
    {if(log_scale==TRUE) annotation_logticks(sides="lr")} +


    labs(
      title = "Mean time spent in cages",
      subtitle = {ifelse(log_scale==TRUE,
                         paste0("Week: ", weeks, ". Log scaled values"),
                         paste0("Week: ", weeks))},
      x = "Cage Category",
      y = "Mean duration (second)",
      fill = "Age in months") +
    theme_bw()

  ggsave(plot = plot_duration_level,
         filename = file.path(dirsave$jpg_duration, phase, paste(sep="_", plot_file_name, phase,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  plot_duration_level

}




#' Plot and save traveled distance for each week.
#'
#' Present the distribution of traveled distance per hour for day and week as a boxplot.
#' Each panel includes data from one day. Mice age is plotted on the x-axis.
#' Colors indicate the mice ID. Each row of panels describes the data for one week.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk name of the plot to be saved
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_distance_hourly_xAxisAge<- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_hourly_xAxisAge <- data %>% #filter(Week %in% c(min(distance_hourly$Week), max(distance_hourly$Week))) %>%
    ggplot(aes(x=as.factor(Age_in_months), y=sum_distance_hourly/1000, fill=IdLabel)) +
    stat_boxplot(geom="errorbar") +
    geom_boxplot(aes(y=sum_distance_hourly/1000), outlier.size = 1, outlier.alpha = 0.2) +
    labs(
      title = "Traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Mice age in months",
      y = "Traveled distance per hour (m)",
      fill = "Mice ID"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")

    ) +
    facet_wrap(Week~Day,
               nrow=length(unique(data$Week)),
               ncol=7, # nr. of days in week
               scales = "free_x",
               labeller = labeller(Week = label_both, Day = label_both)
    )

  ggsave(plot = distance_hourly_xAxisAge,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_hourly_xAxisAge

}



#' Plot and save traveled distance for each week.
#'
#' Present the distribution of traveled distance per hour for day and week as a boxplot.
#' Each panel includes data for one mouse. Mice age is plotted on the x-axis.
#' Colors indicate mice age. Each column of panels describes the data for one week. Each row of panels describes data for one mouse.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @export
plot_distance_hourly_xAxisAge_miceTogether<- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_hourly_xAxisAge_miceTogether <- data %>% #filter(Week %in% c(min(distance_hourly$Week), max(distance_hourly$Week))) %>%
    rename(ID = IdLabel) %>%
    ggplot(aes(x=as.factor(Age_in_months), y=sum_distance_hourly/1000, fill=as.factor(Day))) +
    stat_boxplot(geom="errorbar") +
    geom_boxplot(aes(y=sum_distance_hourly/1000), outlier.size = 1, outlier.alpha = 0.2) +
    labs(
      title = "Traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Mice age in months",
      y = "Traveled distance per hour (m)",
      fill = "Days"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")

    ) +
    facet_wrap(ID ~ Week,
               nrow=length(unique(data$IdLabel)),
               ncol=length(unique(data$Week)),
               #ncol = 1,
               scales = "free_x",
               labeller = labeller(Week = label_both, ID = label_both)
    )

  ggsave(plot = distance_hourly_xAxisAge_miceTogether,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_hourly_xAxisAge_miceTogether

}




#' Plot and save traveled distance for each week.
#'
#' Present the distribution of traveled distance per hour for day and week as a boxplot.
#' Each panel includes data for one mouse. Days are plotted on the x-axis.
#' Colors indicate the days of the week. Each column of panels describes the data for one week. Each row of panels describes data for one mouse.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#' @export
plot_distance_hourly_xAxisDay<- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_hourly_xAxisDay <- data %>% #filter(Week %in% c(min(distance_hourly$Week), max(distance_hourly$Week))) %>%
    rename(ID = IdLabel) %>%
    ggplot(aes(x=as.factor(Day), y=sum_distance_hourly/1000, fill=as.factor(Age_in_months))) +
    stat_boxplot(geom="errorbar") +
    geom_boxplot(aes(y=sum_distance_hourly/1000), outlier.size = 1, outlier.alpha = 0.2) +
    labs(
      title = "Traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Days",
      y = "Traveled distance per hour (m)",
      fill = "Mice age in months"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")

    ) +
    facet_wrap(ID~Week,
               nrow=length(unique(data$IdLabel)),
               ncol=length(unique(data$Week)),
               scales = "free_x",
               labeller = labeller(Week = label_both, ID = label_both)
    )

  ggsave(plot = distance_hourly_xAxisDay,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_hourly_xAxisDay

}

#' Plot and save traveled distance for each week.
#'
#' Present the distribution of traveled distance per hour for day and week as a smoothed line
#' Each panel includes data for one mouse. Days are plotted on the x-axis.
#' Colors indicate the days of the week. Each column of panels describes the data for one week.
#' Each row of panels describes data for one mouse.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_distance_hourly_xAxisDay_smooth<- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_hourly_xAxisDay_smooth <- data %>% #filter(Week %in% c(min(distance_hourly$Week), max(distance_hourly$Week))) %>%
    rename(ID = IdLabel) %>%
    ggplot(aes(x=(Day), y=sum_distance_hourly/1000, fill=as.factor(Age_in_months))) +
    #stat_boxplot(geom="errorbar") +
    #geom_boxplot(aes(y=sum_distance_hourly/1000), outlier.size = 1, outlier.alpha = 0.2) +
    geom_point(alpha = 0.2) +
    geom_smooth(aes(x=(Day), y=sum_distance_hourly/1000))+
    labs(
      title = "Traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Days",
      y = "Traveled distance per hour (m)",
      fill = "Mice age in months"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")

    ) +
    facet_wrap(ID~Week,
               nrow=length(unique(data$IdLabel)),
               ncol=length(unique(data$Week)),
               scales = "free_x",
               labeller = labeller(Week = label_both, ID = label_both)
    )

  ggsave(plot = distance_hourly_xAxisDay_smooth,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_hourly_xAxisDay_smooth

}




#' Plot and save traveled distance for each day
#'
#' Present traveled distance per hour for each day as a stacked bar.
#' Each panel includes data for one mouse. Days are plotted on the x-axis.
#' Colors indicate the distance value (in meter) for each hour. Each bar presents one day.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{manipulatedDate_distance_hourly}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_distance_hourly_xAxisDay_stackedBar<- function(data, plot_file_name, plot_height, plot_width, nr_xTicks, DPI=150){
  distance_hourly_xAxisDay_stackedBar <- data %>% #filter(Week %in% c(min(distance_hourly$Week), max(distance_hourly$Week))) %>%
    rename(ID = IdLabel) %>% rename(Age = Age_in_months) %>%
    ggplot(aes(x=as.factor(Day), y=sum_distance_hourly/1000, fill= as.factor(Time%>% hour()))) +
    #stat_boxplot(geom="errorbar") +
    #geom_boxplot(aes(y=sum_distance_hourly/1000), outlier.size = 1, outlier.alpha = 0.2) +
    #geom_point(alpha = 0.2) +
    geom_bar(aes(x=as.factor(Day), y=sum_distance_hourly/1000), stat="identity")+
    labs(
      title = "Stacked Barplot: Traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Days",
      y = "Traveled distance per hour (m)",
      fill = "Time (hour)"
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      strip.background.x = element_rect(fill="white")
    ) +
    #scale_x_discrete(limits = 7) +
    scale_x_discrete(breaks = seq(1,max(data$Day),nr_xTicks)) +

    facet_wrap(Age ~ ID,
               nrow=1,
               #nrow=length(unique(distance_hourly$ID)),
               #ncol=length(unique(distance_hourly$Week)),
               scales = "free_x",
               labeller = labeller(Age = label_both, ID = label_both)
    ) #+
  #scale_fill_viridis(discrete = TRUE)

  ggsave(plot = distance_hourly_xAxisDay_stackedBar,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_hourly_xAxisDay_stackedBar

}



#' Plot and save traveled distance for each week.
#'
#' Present the distribution of daily traveled distance for each week as a boxplot.
#' Each panel include one week. Mice age is plotted on the x-axis. Colors indicate the mice ID.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Dataframe obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_distance_daily_xAxisAge <- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_daily_xAxisAge <- data %>%
    ggplot(aes(x=Age_in_months, y=sum_distance_daily/1000, fill=IdLabel)) +
    stat_boxplot(geom="errorbar") +
    geom_boxplot(aes(y=sum_distance_daily/1000), outlier.size = 1, outlier.alpha = 0.2) +
    geom_jitter(position=position_dodge(0.8), shape= 21, size=2) +
    #geom_text(aes(label=as.factor(Day)), check_overlap = F, position=position_jitter(width=0.15))+
    labs(title = "Traveled distance",
         subtitle = paste(max(data$Day), " days"),
         x = "Age in months",
         y = "Traveled distance per day (m)",
         fill = "Mouse ID") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background =element_rect(fill="white")) +
    guides(col = guide_legend(nrow=1, byrow= T)) +
    facet_wrap(~Week, labeller = labeller(Week = label_both), nrow = 1)

  ggsave(plot = distance_daily_xAxisAge,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_daily_xAxisAge
}




#' Plot and save traveled distance for each week.
#'
#' The figure presents the distribution of daily traveled distance for each week as a boxplot.
#' Each panel include one mouse. Week number is written on the x-axis. Colors indicate the mice age.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_distance}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_distance_daily_xAxisWeek <- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  distance_daily_xAxisWeek <- data %>%
    rename(ID = IdLabel) %>%
    ggplot(aes(x=as.factor(Week), y=sum_distance_daily/1000, fill=as.factor(Age_in_months))) +
    stat_boxplot(geom="errorbar") +
    geom_boxplot(aes(y=sum_distance_daily/1000), outlier.size = 1, outlier.alpha = 0.2) +
    geom_jitter(position=position_dodge(0.8), shape=21) +
    labs(
      title = "Traveled distance",
      subtitle = paste(max(data$Day), " days"),

      x = "Week",
      y = "Traveled distance per day (m)",
      fill = "Mice age in months"
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")

    ) +
    facet_wrap(~ID,
               nrow=1,
               #ncol=length(unique(distance_hourly$Week)),
               scales = "free_x",
               labeller = labeller(ID = label_both)
    )

  ggsave(plot = distance_daily_xAxisWeek,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  distance_daily_xAxisWeek
}



#' Bar plot for mean traveled distance per hour, per day. Day is on X-axis
#'
#' Days are plotted on the x-axis. Colors indicate the mice age.Each column of panels includes data for one week.
#' Each row includes data for one mouse.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_mean_hourlyDistance_perDay}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_mean_hourlyDistance_perDay_xAxisDay_bar<- function(data, plot_file_name, plot_height, plot_width, DPI=150){
  mean_hourlyDistance_perDay_xAxisDay_bar <- data %>%
    rename(ID = IdLabel) %>%
    ggplot(aes(x=as.factor(Day), y=mean_hourlyDistance_perDay , fill=as.factor(Age_in_months))) +
    geom_bar(stat = "identity", position = position_dodge())+
    geom_errorbar(aes(ymin=mean_hourlyDistance_perDay -sd_HourlyDistance_perDay,
                      ymax=mean_hourlyDistance_perDay +sd_HourlyDistance_perDay),
                  width=.2) +
    #geom_point(shape=21) +
    labs(
      title = "Mean traveled distance",
      subtitle = paste(max(data$Day), " days"),
      x = "Days",
      y = "Mean traveled distance per hour (m)",
      fill = "Mice age in months") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.background.x = element_rect(fill="white")) +
    facet_wrap(ID~Week,
               nrow=length(unique(data$IdLabel)),
               ncol=length(unique(data$Week)),
               scales = "free_x",
               labeller = labeller(Week = label_both, ID = label_both))

  ggsave(plot = mean_hourlyDistance_perDay_xAxisDay_bar,
         filename = file.path(dirsave$jpg_distance, paste(plot_file_name,".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)
}




#' Plot distribution of time spent in cages as boxplot
#'
#' Duration values are plotted per hour or per day. If there is a high value of an outlier, set the \code{log_scale} to \code{True}.
#' Each panel includes data for one level in the colony rack setup (from down to top). Colors indicate the mice age.
#' Cages are presented on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_duration_inCageLevel}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param time_window_type How the duration should be calculated. Use \code{perDay} for total time spent in cages per day and \code{perHour} for duration per hour.
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_duration_cage_distribution <- function(data, plot_file_name, plot_height, plot_width, log_scale=F, time_window_type, DPI=150){
  cage_distribution <- data %>%
    #duration_cages_distr_daily_eliminatedOutliers %>% #filter(Cage_level==1) %>%
    ggplot(aes(x=Cage, y=duration/60)) +
    geom_boxplot(aes(fill=as.factor(Age_in_months)), outlier.size = 1, outlier.alpha = 0.2) +
    #coord_cartesian(ylim = c(0, ylim_upper_cut_daily/60))+

    {if (log_scale == TRUE) {scale_y_log10()}} +
    {if(log_scale==TRUE) annotation_logticks(sides="lr")} +

    {if (time_window_type == "perDay")
      labs(
        title="Time spent in cages",
        subtitle = paste(max(data$Day), " days"),
        y="Total time spent in cages per day (in minutes)",
        fill = "Age in months")
      else
        labs(
          title="Time spent in cages",
          subtitle = paste(max(data$Day), " days"),
          y="Total time spent in cages per hour (in minutes)",
          fill = "Age in months")
    } +

    facet_wrap(~Cage_level, nrow=3, drop=F, scales = "free_x",
               labeller=as_labeller(c(`1` = "Level 1", `2` = "Level 2", `3` = "Level 3"))) +
    theme_bw()

  ggsave(plot = cage_distribution,
         filename = file.path(dirsave$jpg_duration, paste(plot_file_name, "_", time_window_type, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)
  cage_distribution
}


#' Plot distribution of time spent in cages as boxplot and cut outliers values
#'
#' Duration values are plotted per hour or per day. Each panel includes data for one level in the colony rack setup (from down to top). Colors indicate the mice age.
#' Cages are presented on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_duration_inCageLevel}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param time_window_type How the duration should be calculated. Use \code{perDay} for total time spent in cages per day and \code{perHour} for duration per hour.
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_cage_distribution_cutOutiers <- function(data, plot_file_name, plot_height, plot_width, time_window_type, DPI=150){
  # extract the first quantile (.75%)
  quantiles <- data %>%
    group_by(IdLabel, Cage) %>%
    summarise(
      quantile75 = quantile(duration, 0.75),
      #max = (max(daily_duration)) %>% as.duration() %>% round(3),
      #min = (min(daily_duration)) %>% as.duration() %>% round(3),
      #quantile25 = quantile(daily_duration, 0.25)
      #Qdiff = IQR(daily_duration)
    )
  ylim_upper_cut <- quantiles$quantile75 %>% max()

  cage_distribution <- data %>%
    #duration_cages_distr_daily_eliminatedOutliers %>% #filter(Cage_level==1) %>%
    ggplot(aes(x=Cage, y=duration/60)) +
    geom_boxplot(aes(fill=as.factor(Age_in_months)), outlier.size = 1, outlier.alpha = 0.2) +
    coord_cartesian(ylim = c(0, ylim_upper_cut/60))+
    {if (time_window_type == "perDay")
      labs(
        title="Time spent in cages",
        subtitle = paste(max(data$Day), " days"),
        y="Total time spent in cages per day (in minutes)",
        fill = "Age in months")
      else
      labs(
        title="Time spent in cages",
        subtitle = paste(max(data$Day), " days"),
        y="Total time spent in cages per hour (in minutes)",
        fill = "Age in months")
    } +

    facet_wrap(~Cage_level, nrow=3, drop=F, scales = "free_x",
               labeller=as_labeller(c(`1` = "Level 1", `2` = "Level 2", `3` = "Level 3"))) +
    theme_bw()
    # theme (strip.background =element_rect(fill="salmon"))


  ggsave(plot = cage_distribution,
         filename = file.path(dirsave$jpg_duration, paste(plot_file_name, "_", time_window_type, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)
  cage_distribution
}



##########################################
################ Speed ###################
##########################################

#' Plot and save speed distribution as a box plot
#'
#' Speed values are presented for each mouse per day .
#' Each panel includes data for one mouse. Colors indicate the mice age.
#' Days are presented on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_speed}}
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_speed_distribution <- function(data, plot_file_name, phase=NA, nr_xTicks, plot_height, plot_width, log_scale = F, DPI=150){
  speed_distribution_all_mice <- data %>%
    filter(Cage_type == 1,
           if (phase %in% c("dark", "light"))
             Phase == phase
           else
             if (is.na(phase))
               TRUE
           else
             stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")) %>%
    ggplot(aes(x=as.factor(Day), y=Speed))+
    geom_boxplot(aes(fill=as.factor(Age_in_months)), outlier.alpha = 0.2) +
    labs(
      title = "Speed distribution",
      subtitle = paste(max(data$Day), " days. All cages"),
      x = "Days",
      y = "Speed (cm/s)",
      fill ="Mice age\n(months)") +
    facet_wrap(~IdLabel, ncol=1) +

    {if (log_scale == TRUE) {scale_y_log10()}} +
    {if(log_scale==TRUE) annotation_logticks(sides="lr")} +

    scale_x_discrete(breaks = seq(1,max(data$Day), nr_xTicks)) +
    theme_bw()

  ggsave(plot = speed_distribution_all_mice,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, "_", phase, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  speed_distribution_all_mice
}



#' Plot and save speed distribution as a box plot
#'
#' For each cage in each day, speed values of one specific mouse are plotted.
#' Each panel includes data for one cage. Colors indicate the cage levels in the colony rack setup.
#' Each row of panels is one level.
#' Days are presented on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_speed}}
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param mouseID Choose one mouse of the entire data set. The \code{mouseID} must be a \code{String} value.
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param log_scale A Boolean parameter for scaling the duration values on the y-axis. Use either \code{TRUE} or \code{FALSE}.
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_speed_cage_distribution <- function(data, plot_file_name, phase=NA, mouseID, nr_xTicks, plot_height, plot_width, log_scale=F, DPI=150){
  # data$Cage_level <- factor(data$Cage_level, levels = c(3,2,1))
  speed_distribution <- data %>%
    filter(IdLabel == mouseID, Cage_type == 1,
          if (phase %in% c("dark", "light"))
            Phase == phase
          else
            if (is.na(phase))
              TRUE
          else
            stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")) %>%
    ggplot(aes(x=as.factor(Day), y=Speed))+
    geom_boxplot(aes(fill=as.factor(Cage_level)), outlier.alpha = 0.2) +
    labs(
      title = "Speed distribution",
      subtitle = paste0("Mouse ID: ", mouseID, ". ", max(data$Day), " days"),
      x = "Days",
      y = "Speed (cm/s)",
      fill = "Cage level") +
    facet_wrap(~Cage, nrow=3, labeller = labeller(Cage_level=label_both, Cage=label_value)) +

    {if (log_scale == TRUE) {scale_y_log10()}} +
    {if(log_scale==TRUE) annotation_logticks(sides="lr")} +

    scale_x_discrete(breaks = seq(1,max(data$Day), nr_xTicks)) +
    theme_bw()

  ggsave(plot = speed_distribution,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, "_", phase, mouseID, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  speed_distribution
}



#' Plot and save mean speed per day as a bar plot.
#'
#' For each week and day, speed values of each mouse are plotted.
#' Each panel includes data for one mouse and one week. Colors indicate the mice age.
#' Each row of panels represents one mouse. Each column of panels represents one week
#' Days are presented on the x-axis.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_daily_mean_speed}}
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_speed_daily_xAxisDay_bar <- function(data, plot_file_name, phase=NA, plot_height, plot_width, DPI=150){
  speed_daily_xAxisDay_bar <- data %>%
    filter(
           if (phase %in% c("dark", "light"))
             Phase == phase
           else
             if (is.na(phase))
               TRUE
           else
             stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")) %>%
  ggplot(aes(x=as.factor(Day), y=mean_speed_daily, fill=as.factor(Age_in_months))) +
  geom_bar(stat = "identity", position = position_dodge()) +

  #geom_point(shape=21) +
  labs(
    title = "Mean speed per day",
    subtitle = paste(max(data$Day), " days"),
    x = "Days",
    y = "Mean speed (cm/s)",
    fill = "Mice age in months"
  ) +
  theme(
    legend.position = "bottom",
    strip.background.x = element_rect(fill="white")

  ) +
  facet_wrap(IdLabel~Week,
             nrow=length(unique(data$IdLabel)),
             ncol=length(unique(data$Week)),
             scales = "free_x",
             labeller = labeller(Week = label_both, IdLabel = label_both)
  ) +
  theme_bw()

  ggsave(plot = speed_daily_xAxisDay_bar,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, "_", phase, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

    speed_daily_xAxisDay_bar
}


#' Plot and save speed frequency histogram
#'
#' Specify the crossing length by adjusting the \code{crossing_length}. Speed in cages and tubes (linear, up and down) can be plotted.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_speed}}
#' @param crossing_length Integer number indicates to all underlying cross values. Default is occurrences with all crossing lengths.
#' @param cage_type Integer number. Use 1 to select cages and 2 for tubes
#' @param height Indicate the tube type. Use a \code{height} value  greater than 0 for UP-tubes, less than zero for DOWN-tubes and 0 for linear tubes
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param include_zero_crossing_length Boolean.
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_speed_frequency_hist <- function(data, plot_file_name, phase=NA, include_zero_crossing_length,
                                            crossing_length=Inf, cage_type, height, plot_height, plot_width, DPI=150){

  {if (cage_type == 1) title = 'linear crossing steps in cages'
   else if (cage_type == 2){
          if (height == 0) title = 'linear crossing steps in horizontal tubes'
          else if (height > 0) title = 'tubes (UP)'
          else if (height < 0) title = 'tubes (DOWN)'
        }
  }

  speed_frequency_hist <- data %>%
    filter(
           Cage_type == cage_type,
           Crossing_length <= crossing_length,

           {if (isTRUE(include_zero_crossing_length))
             TRUE

            else
              Crossing_length != 0
            },

           {if (height > 0)
             Height > 0
           else
               if (height < 0)
                 Height < 0
               else
                 if (height == 0)
                   Height == 0

                 else
                   stop("Wrong input for height Please type a value that is greater, lower or equal zero.")
           },

           # {if (isFALSE(include_zero_crossing_length) & cage_type==2)
           #   stop("Tubes has a crossing length of zero. Please set `include_zero_crossing_length` to TRUE.")
           # },

           {if (phase %in% c("dark", "light"))
             Phase == phase
           else
               if (is.na(phase))
                 TRUE
               else
                 stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
          }
    ) %>%
    ggplot(aes(x = Speed)) +
    geom_histogram() +
    # geom_freqpoly(aes(color=IdLabel))+
    theme_bw() +
    facet_grid(IdLabel ~ Crossing_length, drop = F, scale = "fixed",
               labeller = labeller (IdLabel=label_value, Crossing_length=label_both)) +
    labs(
      title = paste0("Speed frequency for ", title),
      subtitle = paste(max(data$Day), " days"),
      x = "Speed (cm/s)",
      y = "Speed frequency")

  speed_frequency_hist <- annotate_figure(speed_frequency_hist, right = text_grob("Mouse ID", rot = -90))

  ggsave(plot = speed_frequency_hist,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, "_", phase, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  speed_frequency_hist
}



#' Plot and save speed frequency  as polygon graph
#'
#' Specify the crossing length by adjusting the \code{crossing_length}. Speed in cages and tubes (linear, up and down) can be plotted.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_speed}}
#' @param crossing_length integer number indicates to all underlying cross values. Default is all occurrences.
#' @param cage_type Integer number. Use 1 to select cages and 2 for tubes
#' @param height Indicate the tube type. Use a \code{height} value  greater than 0 for UP-tubes, less than zero for DOWN-tubes and 0 for linear tubes
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_speed_frequency_poly <- function(data, plot_file_name, phase=NA, crossing_length, cage_type, height, plot_height, plot_width, DPI=150){

  {if (cage_type == 1) title = 'linear crossing steps in cages'
   else if (cage_type == 2){
      if (height == 0) title = 'linear crossing steps in horizontal tubes'
      else if (height > 0) title = 'tubes (UP)'
      else if (height < 0) title = 'tubes (DOWN)'
      }
  }

  speed_frequency_poly <- data %>%
    filter(
      Cage_type == cage_type,
      Crossing_length == crossing_length,

      {if (height > 0)
        Height > 0
        else
          if (height < 0)
            Height < 0
        else
          if (height == 0)
            Height == 0

        else
          stop("Wrong input for height Please type a value that is greater, lower or equal zero.")
      },


      {if (phase %in% c("dark", "light"))
        Phase == phase
        else
          if (is.na(phase))
            TRUE
        else
          stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
      }
    ) %>%
    ggplot(aes(x = Speed, color=IdLabel)) +
    geom_freqpoly()+
    theme_bw() +
    labs(
      title = paste0("Speed frequency for ", title),
      subtitle = paste(max(data$Day), " days"),
      x = "Speed (cm/s)",
      y = "Speed frequency")+
    theme(legend.position = "bottom")

  ggsave(plot = speed_frequency_poly,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, "_", phase, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  speed_frequency_poly
}


############################################################################
######################## Speed / crossing lengths ##########################
############################################################################

#' Plot and save  locomotion summary for each crossing length
#'
#' For each crossing, summarize distance, duration and speed. All mice are grouped together.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_length_crossing_summary}}
#' @param plot_file_name File name to create on disk
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#' @importFrom cowplot plot_grid
#' @importFrom grid grid.draw
plot_crossing_distance_duration_speed <- function(data, plot_file_name, nr_xTicks, plot_height, plot_width, DPI=150){
  min_crossing_length <- min(data$Crossing_length)
  max_crossing_length <- max(data$Crossing_length)

  g_distance <-
    ggplot(data, aes(x=Crossing_length)) +
    geom_line(aes(y=Distance_mean)) +
    labs(y="Mean Distance", title= "Mean distance, duration and speed for each corssing length") +
    theme(legend.position = "none", axis.title.x=element_blank()) +
    scale_x_continuous(breaks = seq(from = min_crossing_length, to = max_crossing_length, by = nr_xTicks))+
    theme_bw()

  g_duration <-
    ggplot(data, aes(x=Crossing_length)) +
    geom_line(aes(y=Duration_mean)) +
    labs(y="Mean Duration") +
    theme(legend.position = "none", axis.title.x=element_blank())+
    scale_x_continuous(breaks = seq(from = min_crossing_length, to = max_crossing_length, by = nr_xTicks))+
    theme_bw()

  g_speed <-
    ggplot(data, aes(x=Crossing_length)) +
    geom_line(aes(y=Speed_mean)) +
    labs(x ="Crossing length", y="Mean Speed") + # don not forget to center the title
    theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(from = min_crossing_length, to = max_crossing_length, by = nr_xTicks))+
    theme_bw()

  plot_grid(g_distance, g_duration, g_speed, ncol = 1, nrow = 3)


  g1 <- ggplotGrob(g_distance)
  g2 <- ggplotGrob(g_duration)
  g3 <- ggplotGrob(g_speed)

  g <- rbind(g1, g2, g3, size = "first")
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths)
  grid.draw(g)

  ggsave(plot = g,
         filename = file.path(dirsave$jpg_speed, paste(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  # g
}


##################################################################
############################# Spatial ############################
##################################################################

#' Plot and save niche-overlap between pairs.
#'
#' The pianka's index values are plotted as a heat map.
#' The maximum value of the index is 1 and indicates complete overlap,
#' while the minimum value 0 indicates no overlap.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_pairwise_niche_overlap}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param subtitle Text for subtitle the figure. Should indicate the method usded (cage visits or duration)
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_pairwise_niche_overlap <- function(data, plot_file_name, subtitle, plot_height, plot_width, DPI=150){
  pairwise_niche_overlap <- data %>%
    ggplot(aes(x=p1, y=p2, fill=pianka_index)) +
    geom_tile() +
    geom_text(aes(label=pianka_index %>% round(2)))+
    scale_fill_viridis(discrete=FALSE) +
    labs(
      title = "Pairwise spatial niche overlap (Pianka’s Index)",
      subtitle = subtitle,
      x = "Mouse ID",
      y = "Mouse ID",
      fill =  "Pianka's index score"
    ) +
    theme_bw()

  ggsave(plot = pairwise_niche_overlap,
         filename = file.path(dirsave$jpg_spatial, paste(plot_file_name, "_", subtitle, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  pairwise_niche_overlap
}



#' Plot and save the mean pianka's index for each mouse.
#'
#' The pianka's index values are plotted as a dots.
#' The maximum value of the index is 1 and indicates complete overlap,
#' while the minimum value 0 indicates no overlap.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_mean_pianka_index}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param subtitle should indicate the method (cage visits or duration)
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_mean_pianka_index <- function(data, plot_file_name, subtitle, plot_height, plot_width, DPI=150){
  mean_pianka_index <- data %>%
    ggplot(aes(x=IdLabel, y=mean_pianka))+
    geom_point(aes(color=as.factor(Age_in_months)), size=4) +
    labs(
      title = "Mean spatial niche overlap (Pianka’s Index)",
      subtitle =subtitle,
      x = "Mouse ID",
      y = "Mean pianka's index",
      color = "Mice age in months"
    ) +
    theme_bw()

  ggsave(plot = mean_pianka_index,
         filename = file.path(dirsave$jpg_spatial, paste(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  mean_pianka_index
}






#' Plot and save overlap of duration between all mice permutations
#'
#' The function plot the overlap value (time spent in same cages in seconds) as bubbles. The larger the bubble, the higher the overlap value is
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_duration_overlap}}
#' @param plot_file_name File name to create on disk
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param time_lower_limit Minimum number of seconds of overlap to be considered.
#' @param nr_mice_in_group Minimum Number of mice should beD grouped together to calculate the overlapped duration
#' @param DPI Plot resolution(Dots per Inch)
#' @export
plot_duration_overlap <- function(data, plot_file_name, time_lower_limit, nr_mice_in_group, plot_height, plot_width, DPI=150){

  duration_overlap <- data %>%
  filter(sum_overlaps >= as.duration(time_lower_limit),
         nr_of_mice >= nr_mice_in_group)

# Generate the layout. This function return a dataframe with one line per bubble.
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(duration_overlap$sum_overlaps, sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(duration_overlap, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
# plot(data$radius, data$value)

# Go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Make the plot
circles <- dat.gg %>%
  ggplot() +

  # Make the bubbles
  geom_polygon( aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +

  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=sum_overlaps, label = miceID)) +
  scale_size_continuous(range = c(1,4)) +

  # General theme:
  theme_void() +
  theme(legend.position="none") +
  coord_equal()

  ggsave(plot = circles,
         filename = file.path(dirsave$jpg_duration, paste(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)


  circles
}



#' Plot and save the number of visited cages as a heatmap
#'
#' For each mouse and within a specific day plot the number of visited cages per hour
#' Color intensity indicates the number of cage visits. Time is on the x-axis. Each panel represents data one cage.
#' Each row-of-panels refer to one level in the colony rack setup.
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_daily_mean_speed}}
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param day Specify the day of interest as an integer number
#' @param nr_xTicks Time difference between hour-labels to show on the x-axis
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
plot_heatmap_count_cage_visits_hourly <- function(data, plot_file_name, phase=NA, day, nr_xTicks, plot_height, plot_width, DPI=150){
  count_cage_visits_hourly <- data %>%
    filter(Day == day,
           {if (phase %in% c("dark", "light"))
             Phase == phase
             else
               if (is.na(phase))
                 TRUE
             else
               stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
           }) %>%
    ggplot(aes(x=Time, y=IdLabel, fill=(cage_sum))) +
    geom_tile() +
    scale_x_datetime(breaks = scales::date_breaks(paste0(nr_xTicks, " hour")), date_labels = "%H:%M") +
    labs(
      title = "Number of visited cages per hour",
      subtitle = paste("Day: ", day),
      x =  "Time (hour)",
      y = " Mouse ID",
      fill = "Number of\nvisited cages"
    ) +
    facet_wrap(~ Cage, ncol = 6) +
    scale_fill_viridis(discrete=F, option = "H") +
    theme_bw()

  ggsave(plot = count_cage_visits_hourly,
         filename = file.path(dirsave$jpg_cage_visits, paste(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  count_cage_visits_hourly
}



#' Plot and save the duration for one day as a heatmap
#'
#' Select one day and plot the time spent in each cage per hour for each mouse. Color intensity represents the duration. Time (in hour) is set on the x-axis.
#' per hour within a day, for each mouse
#' The figure is saved as a PNG file in the result environment.
#'
#' @param data Data frame obtained by using \code{\link{get_duration_inCages}}
#' @param plot_file_name File name to create on disk
#' @param phase The \code{dark} or \code{light} phase to be considered. If \code{NA} then the values are calculated for 24 hours.
#' @param day Integer number. Specify the day for which the duration per hour should be plotted.
#' @param nr_xTicks Number of ticks and labels to show on the x-axis
#' @param plot_height Plot height
#' @param plot_width Plot width
#' @param DPI Plot resolution(Dots per Inch)
#'
#' @export
#'
plot_heatmap_duration_inCages_hourly <- function(data, plot_file_name, phase=NA, day, nr_xTicks, plot_height, plot_width, DPI=150){
  duration_inCages_hourly <- data %>%
    filter(Day == day,
           {if (phase %in% c("dark", "light"))
             Phase == phase
             else
               if (is.na(phase))
                 TRUE
             else
               stop("Wrong input for phase. Please type `dark`, `light`. Default is all day (24h)")
           }) %>%
    ggplot(aes(x=Time, y=IdLabel, fill=(hourly_duration/60))) +
    geom_tile() +
    scale_x_datetime(breaks = scales::date_breaks(paste0(nr_xTicks, " hour")), date_labels = "%H") +
    labs(
      title = "Time spent in cages per hour",
      subtitle = paste("Day: ", day),
      x =  "Time (hour)",
      y = " Mouse ID",
      fill = "Duration (minute)"
    ) +
    facet_wrap(~ Cage, ncol = 6) +
    scale_fill_viridis(discrete=F, option = "H") +
    theme_bw()

  ggsave(plot = duration_inCages_hourly,
         filename = file.path(dirsave$jpg_duration, paste(plot_file_name, ".png")),
         height = plot_height, width = plot_width, limitsize = FALSE, bg="white", dpi = DPI)

  duration_inCages_hourly
}
