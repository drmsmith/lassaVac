# map: mean cumulative infections for map 
# map: mean per 100k pop across 100 sims 
# map: population size, p_spillover
# map: % sim in which country is present  // spillover incidence 

# mean_cumul_infections
# median_cumul_infections
# mean_cumul_infections_2yrs
# median_cumul_infections_2yrs
# mean_cumul_infections_per100k
# median_cumul_infections_per100k
# mean_cumul_infections_2yrs_per100k
# median_cumul_infections_2yrs_per100k
# pop_size
# p_spillover
# percent_sims

# colors = c("#547dbf", "#fbeaa5", "#db4437"), # can optionally use muted()
# c("white", "#ffa500", "#682860")
# c("#547dbf", "#fbeaa5", "#db4437")

####################
# BAR PLOT HELPERS #
####################

# create col scheme for every panel
make_cepi_base_col_scheme <- function(.data, .base_cols, .region_code) {
    country_counts_per_region <- .data %>%
        count(region_code, country) %>%
        count(region_code)
    nclrs <- country_counts_per_region$n[country_counts_per_region$region_code == .region_code]
    colscheme <- colorRampPalette(cepi_prim_cols)(nclrs) %>%
        unlist() %>%
        unname()
    return(colscheme)
}


gg_metrics_barplot <- function(
    .data,
    .cols_for_scheme,
    .region_code,
    .metric,
    .dest_dir = "figs/barplots_by_region") { #
    # prepare colros and labels for plotting
    v_cols <- make_cepi_base_col_scheme(.data, .cols_for_scheme, .region_code)
    ymin_lab <- str_replace(.metric, "median", "q1")
    ymax_lab <- str_replace(.metric, "median", "q3")
    guide_lab <- ifelse(
        str_detect(.metric, "per100k"),
        "Infections per \n100,000 population",
        "Infections"
    )

    .data %>%
        filter(region_code == .region_code) %>%
        mutate(
            country = factor(country, levels = sort(unique(country)))
        ) %>%
        ggplot(aes(x = country, y = .data[[.metric]])) +
        geom_bar(stat = "identity", aes(fill = country)) +
        geom_errorbar(
            aes(x = country, ymin = .data[[ymin_lab]], ymax = .data[[ymax_lab]]),
            width = 0.4,
            colour = "grey33",
            alpha = 0.9, size = 1.3
        ) +
        scale_fill_manual(values = v_cols) +
        theme_light(base_size = 16) +
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
        guides(fill = "none") +
        labs(x = "", y = guide_lab)

    dest_filename <- paste0(
        .dest_dir, "/", .region_code, "_", .metric, ".png",
        collapse = ""
    )
    ggsave(
        filename = dest_filename,
        width = 4500, height = 3000, units = "px",
        dpi = 400,
        bg = "transparent"
    )
}

# gg_metrics_barplot(df_sum_stats, cepi_prim_cols, 'AFR', 'median_cumul_infections_2yrs_per100k')


gg_metrics_barplot_region <- function(
    .data,
    .metric,
    .dest_dir = "figs/barplot_by_region") { #
    # prepare colros and labels for plotting
    v_cols <- colorRampPalette(cepi_prim_cols)(6) %>%
        unlist() %>%
        unname()
    ymin_lab <- paste0("q1_",.metric)
    ymax_lab <- paste0("q3_",.metric)
    guide_lab <- ifelse(
        str_detect(.metric, "per100k"),
        "Infections per \n100,000 population",
        "Infections"
    )

    .data %>%
        ggplot(aes(x = region_code, y = .data[[.metric]])) +
        geom_bar(stat = "identity", aes(fill = region_code)) +
        geom_errorbar(
            aes(x = region_code, ymin = .data[[ymin_lab]], ymax = .data[[ymax_lab]]),
            width = 0.4,
            colour = "grey33",
            alpha = 0.9, size = 1.3
        ) +
        scale_fill_manual(values = v_cols) +
        theme_light(base_size = 16) +
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
        guides(fill = "none") +
        labs(x = "", y = guide_lab)

    dest_filename <- paste0(
        .dest_dir, "/", "region_", .metric, ".png",
        collapse = ""
    )
    ggsave(
        filename = dest_filename,
        width = 4500, height = 3000, units = "px",
        dpi = 400,
        bg = "transparent"
    )
}




#####################
#### MAP HELPERS ####
#####################

ls_coord_lims <- list(
    AFR = list(xlim = c(-30, 65), ylim = c(-40, 45)),
    AMR = list(xlim = c(-185, -30), ylim = c(-70, 95)),
    EMR = list(xlim = c(-23, 80), ylim = c(-5, 45)),
    EUR = list(xlim = c(-30, 90), ylim = c(27, 75)),
    SEAR = list(xlim = c(60, 145), ylim = c(-15, 45)),
    WPR = list(xlim = c(70, 180), ylim = c(-55, 60))
)

metrics_infects <- c(
    "mean_cumul_infections",
    "median_cumul_infections",
    "mean_cumul_infections_2yrs",
    "median_cumul_infections_2yrs",
    "mean_cumul_infections_per100k",
    "median_cumul_infections_per100k",
    "mean_cumul_infections_2yrs_per100k",
    "median_cumul_infections_2yrs_per100k"
)


gg_metrics_map <- function(
    .wrld_joined_full,
    .region_code,
    .metric,
    .dest_dir = "figs/maps_by_region") {
    if (.metric == "pop_size") {
        cepi_cols <- c("#ffe3b0", "#b0d8e2", "#0080A0")
        guide_lab <- "Population size\n(millions)"
    } else if (.metric == "p_spillover") {
        cepi_cols <- c("#ffe3b0", "#d0bcce", "#682860")
        guide_lab <- "Prob.\nspillover"
    } else if (.metric == "percent_sims") {
        cepi_cols <- c("#ffe3b0", "#e1b5ca", "#9d0f55")
        guide_lab <- "Simulations\n(%)"
    } else if (.metric %in% metrics_infects) {
        cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
        guide_lab <- ifelse(
            str_detect(.metric, "per100k"),
            "Infections\nper 100,000",
            "Infections"
        )
    } else {
        stop("Cannot plot non-existent metric\n")
    }

    # get data set without current region
    wrld_fltrd <- filter(.wrld_joined_full, region_code != .region_code) # & (!is.na(region_code)))

    ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", size = 0.1) +
        geom_sf(
            data = wrld_fltrd, fill = "lightgray", color = NA # , alpha=0.95
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        #     scale_color_manual(values = 'black') + #, na.value='white') +
        # coord_sf()
        coord_sf(
            xlim = ls_coord_lims[[.region_code]]$xlim,
            ylim = ls_coord_lims[[.region_code]]$ylim,
            expand = FALSE
        )

    dest_filename <- paste0(
        .dest_dir, "/", .region_code, "_", .metric, ".png",
        collapse = ""
    )
    ggsave(
        filename = dest_filename,
        width = 6466, height = 4161, units = "px",
        dpi = 500,
        bg = "transparent"
    )
}

# gg_metrics_map(wrld_joined_full, "WPR", "median_cumul_infections_per100k", "figs/cepi_figs_ideas/maps")




gg_metrics_map_global <- function(
    .wrld_joined_full,
    .metric,
    .dest_dir = "figs/global_maps") {
    if (.metric == "pop_size") {
        cepi_cols <- c("#ffe3b0", "#b0d8e2", "#0080A0")
        guide_lab <- "Population size\n(millions)"
    } else if (.metric == "p_spillover") {
        cepi_cols <- c("#ffe3b0", "#d0bcce", "#682860")
        guide_lab <- "Prob.\nspillover"
    } else if (.metric == "percent_sims") {
        cepi_cols <- c("#ffe3b0", "#e1b5ca", "#9d0f55")
        guide_lab <- "Simulations\n(%)"
    } else if (.metric %in% metrics_infects) {
        cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
        guide_lab <- ifelse(
            str_detect(.metric, "per100k"),
            "Infections\nper 100,000",
            "Infections"
        )
    } else {
        stop("Cannot plot non-existent metric\n")
    }

    ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", size = 0.1) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        #     scale_color_manual(values = 'black') + #, na.value='white') +
        coord_sf()

    dest_filename <- paste0(
        .dest_dir, "/", "global_", .metric, ".png",
        collapse = ""
    )
    ggsave(
        filename = dest_filename,
        width = 6466, height = 4161, units = "px",
        dpi = 500,
        bg = "transparent"
    )
}



make_cepi_base_col_scheme_col_out <- function(.base_cols, .out) {
    colscheme <- colorRampPalette(.base_cols)(.out) %>%
        unlist() %>%
        unname()
    return(colscheme)
}






#### single simulation map

gg_map_outbreak_progress <- function(
    .wrld_joined_full,
    .metric,
    .filter_codes,
    .grob,
    .start_day,
    .dest_dir = "figs/outbreak_progression") {

    cepi_cols <- c("#547dbf", "#fbeaa5", "#db4437")
    guide_lab <- ifelse(
            str_detect(.metric, "100k"),
            "Infections\nper 100,000",
            "Infections")

    # get data set without current region
    wrld_fltrd <- filter(.wrld_joined_full, !code %in% .filter_codes)

    ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", size = 0.1) +
        geom_sf(
            data = wrld_fltrd, fill = "gainsboro", color = NA # , alpha=0.95 # fill #D3D3D3, #E8E9EB, azure3
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        theme_light(base_size = 16) +
        guides(color = "none") +
        #     scale_color_manual(values = 'black') + #, na.value='white') +
        coord_sf() + 
        annotation_custom(.grob)

    dest_filename <- paste0(
        .dest_dir, "/", "outbreak_start_", .start_day, ".png",
        collapse = ""
    )
    ggsave(
        filename = dest_filename,
        width = 6e3, height = 25e2, units = "px",
        dpi = 400,
        bg = "transparent"
    )
}
