library("tidyverse")
library("scales")
library("ggthemes")
library("sf")
library("rgdal")
library("rnaturalearth")
library("rnaturalearthdata")
library("countrycode")

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

    .data_fltrd <- .data %>%
        filter(region_code == .region_code) %>%
        mutate(
            country = factor(country, levels = sort(unique(country)))
        )
    .data_fltrd %>%
        ggplot(aes(x = country, y = .data[[.metric]])) +
        geom_bar(stat = "identity", aes(fill = country)) +
        geom_errorbar(
            aes(x = country, ymin = .data[[ymin_lab]], ymax = .data[[ymax_lab]]),
            width = 0.4,
            colour = "grey10",
            alpha = 0.9, linewidth = 1
        ) +
        scale_fill_manual(values = v_cols) +
        scale_y_continuous(
            labels = comma, 
            limits = function(x) {c(0, 1.05*max(.data_fltrd[[ymax_lab]]))}
            ) + 
        theme_light(base_size = 16) +
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
        guides(fill = "none") +
        labs(x = "", y = guide_lab) + 
        coord_cartesian(expand=FALSE)

    dest_filename <- file.path(
        .dest_dir, 
        paste0(.region_code, "_", .metric, ".png",collapse = "")
        )
    ggsave(
        filename = dest_filename,
        width = 4500, height = 3000, units = "px",
        dpi = 400,
        bg = "transparent"
    )
}

# gg_metrics_barplot(df_sum_stats, cepi_prim_cols, 'AFR', 'median_cumul_infections_2yrs_per100k')


gg_metrics_barplot_col_sort <- function(
    .data,
    .cols_for_scheme,
    .region_code,
    .fill_col,
    .metric,
    .format_extension = '.png',
    .dest_dir = "figs/barplots_by_region") { #
    # prepare colros and labels for plotting
    ymin_lab <- str_replace(.metric, "median", "q1")
    ymax_lab <- str_replace(.metric, "median", "q3")
    guide_lab <- ifelse(
        str_detect(.metric, "per100k"),
        "Infections per \n100,000 population",
        "Infections"
    ) 
    .data_fltrd <- .data %>%
        filter(region_code == .region_code) %>%
        arrange(desc(percentage_appearance))
    fctr_levels <- .data_fltrd$country

    .data_fltrd <- .data_fltrd %>%
        mutate(country = factor(country, levels = fctr_levels))
        

    .data_fltrd %>%
        ggplot(aes(x = country, y = .data[[.metric]])) +
        geom_bar(stat = "identity", fill = .fill_col) +
        geom_errorbar(
            aes(x = country, ymin = .data[[ymin_lab]], ymax = .data[[ymax_lab]]),
            width = 0.4,
            colour = "grey10",
            alpha = 0.9, linewidth = 1
        ) +
        # scale_fill_manual(values = v_cols) +
        scale_y_continuous(
            labels = comma, 
            limits = function(x) {c(0, 1.05*max(.data_fltrd[[ymax_lab]]))}
            ) + 
        theme_light(base_size = 16) +
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
        guides(fill = "none") +
        labs(x = "", y = guide_lab) + 
        coord_cartesian(expand=FALSE)

    dest_filename <- file.path(
        .dest_dir, 
        paste0(.region_code, "_", .metric, .format_extension ,collapse = "")
        )
    ggsave(
        filename = dest_filename,
        width = 4500, height = 3000, units = "px",
        dpi = 400,
        bg = "transparent"
    )
}




gg_metrics_barplot_region <- function(
    .data,
    .metric,
    .format_extension = '.png',
    .dest_dir = "figs/barplot_by_region") { #
    # prepare colros and labels for plotting
    # v_cols <- colorRampPalette(cepi_prim_cols)(6) %>%
        # unlist() %>% unname()
    v_cols <- cepi_prim_cols[1:6]
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
            colour = "grey10",
            alpha = 0.9, linewidth = 1
        ) +
        scale_fill_manual(values = v_cols) +
        scale_y_continuous(
            labels = comma, 
            limits = function(x) {c(0, 1.05*max(.data[[ymax_lab]]))}
            ) + 
        theme_light(base_size = 16) +
        theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
        guides(fill = "none") +
        labs(x = "", y = guide_lab) + 
        coord_cartesian(expand=FALSE)

    dest_filename <- file.path(
        .dest_dir, 
        paste0("region_", .metric, .format_extension,collapse = "")
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
    .file_extension='.png',
    .dest_dir = "figs/maps_by_region") {
    if (.metric == "pop_size") {
        cepi_cols <- c("#ffe3b0", "#b0d8e2", "#0080A0")
        guide_lab <- "Population size"
        ##### LOG TRANSFORMAION
        # guide_lab <- "Population size\n(log millions)"
        # .wrld_joined_full[,.metric] <- log(.wrld_joined_full[,.metric]) 
        
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
            # "Infections (log)"
        )
        ##### LOG TRANSFORMAION
        # if (!str_detect(.metric, 'per100k')) {
        #     .wrld_joined_full[,.metric] <- log(.wrld_joined_full[,.metric])  
        #     }
    } else {
        stop("Cannot plot non-existent metric\n")
    }

    # get data set without current region
    wrld_fltrd <- filter(.wrld_joined_full, region_code != .region_code) # & (!is.na(region_code)))
    wrld_na <- .wrld_joined_full %>% filter(!complete.cases(pop_size))

    ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", linewidth = 0.5) +
        geom_sf(
            data = wrld_fltrd, fill = "lightgray", color = NA # , alpha=0.95
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab, labels=comma
        ) +
        geom_sf_pattern(
            data=wrld_na, color = "darkgrey", 
            linewidth = 0.5, 
            pattern = 'stripe', pattern_density = 0.15, 
            pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
            pattern_spacing = 0.01
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

    dest_filename <- file.path(
        .dest_dir, 
        paste0(.region_code, "_", .metric, .file_extension ,collapse = "")
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
    .file_extension = '.png',
    .dest_dir = "figs/global_maps") {
    if (.metric == "pop_size") {
        cepi_cols <- c("#ffe3b0", "#b0d8e2", "#0080A0")
        guide_lab <- "Population size"
        ##### LOG TRANSFORMAION
        # guide_lab <- "Population size\n(log millions)"
        # .wrld_joined_full[,.metric] <- log(.wrld_joined_full[,.metric]) 
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
            # "Infections (log)"
            )
        ##### LOG TRANSFORMAION
        # if (!str_detect(.metric, 'per100k')) {
        #     .wrld_joined_full[,.metric] <- log(.wrld_joined_full[,.metric])  
        #     }
    } else {
        stop("Cannot plot non-existent metric\n")
    }
    wrld_na <- .wrld_joined_full %>% filter(!complete.cases(pop_size))
    ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", linewidth = 0.2) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab, labels=comma
        ) +
        geom_sf_pattern(
            data=wrld_na, color = "darkgrey", 
            linewidth = 0.2, 
            pattern = 'stripe', pattern_density = 0.15, 
            pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
            pattern_spacing = 0.01
            ) + 
        theme_light(base_size = 16) +
        guides(color = "none") +
        coord_sf(expand=FALSE) 

    dest_filename <- file.path(
        .dest_dir, 
        paste0("global_", .metric, .file_extension, collapse = "")
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
    .wrld_joined_nas, 
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
    plt <- ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", linewidth = 0.2) +
        geom_sf(
            data = wrld_fltrd, fill = "gainsboro", color = NA # , alpha=0.95 # fill #D3D3D3, #E8E9EB, azure3
        ) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        geom_sf_pattern(
            data=wrld_joined_nas, color = "darkgrey", 
            linewidth = 0.2, 
            pattern = 'stripe', pattern_density = 0.15, 
            pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
            pattern_spacing = 0.01
        ) + 
        theme_light(base_size = 16) +
        guides(color = "none") +
        #     scale_color_manual(values = 'black') + #, na.value='white') +
        coord_sf(expand=FALSE) + 
        annotation_custom(.grob)

    dest_filename <- file.path(
        .dest_dir, 
        paste0("outbreak_start_", .start_day, ".png",collapse = "")
        )
    # ggsave(
    #     plt, 
    #     filename = dest_filename,
    #     width = 6e3, height = 25e2, units = "px",
    #     dpi = 400,
    #     bg = "transparent"
    # )
    return(plt)
}



get_worldmap <- function() {
    # get world map data for plotting 
    world <- ne_countries(scale = "large", returnclass = "sf") %>% 
        filter(!str_detect(admin, 'arctica')) 
    world <- world %>%
        mutate(code = countrycode(world$name, 'country.name', 'iso3c'))
    # world <- world[, ]
    world$codecode[world$name=='USA'] = 'USA'
    return(world)
}

get_worldmap_nocode <- function() {
    # get world map data for plotting 
    world <- ne_countries(scale = "large", returnclass = "sf") %>% 
        filter(!str_detect(admin, 'arctica')) 
    return(world)
}


gg_dest_map_global <- function(
    .wrld_joined_full,
    .wrld_start,
    .start_point,
    .wrld_joined_nas, 
    .metric,
    .save = T,
    .dest_dir = main_dir,
    .file_name = .metric) {

    # cepi_cols <- c("#ffe3b0", "#e1b5ca", "#9d0f55")
    # cepi_cols <- c("#ffe3b0", "#9d0f55", "#682860")
    # cepi_cols <- c("#ffe3b0", "#9d0f55", "#8c5b00")
    cepi_cols <- c("#ffe3b0",  "#e89600", "#bd5e8d", "#9d0f55")
    guide_lab <- "Simulations\n(%)"

    plt <- ggplot(data = .wrld_joined_full) +
        geom_sf(aes(fill = .data[[.metric]]), color = "darkgrey", linewidth = 0.2) +
        geom_sf(data=.wrld_start, fill = '#54aabf', color = "darkgrey", linewidth = 0.2, stroke=2) +
        scale_fill_gradientn(
            colors = cepi_cols,
            na.value = "gray25", # "lightgray",
            name = guide_lab
        ) +
        geom_sf(data=.start_point, size=1.5, shape=13) + 
        geom_sf_pattern(
            data=wrld_joined_nas, color = "darkgrey", 
            linewidth = 0.2, 
            pattern = 'stripe', pattern_density = 0.15, 
            pattern_fill = 'gray33', pattern_colour = 'darkgrey', 
            pattern_spacing = 0.01
        ) + 
        theme_light(base_size = 16) + # linedraw classic map minimal
        guides(color = "none") + # labs(x='', y='') + 
        #     scale_color_manual(values = 'black') + #, na.value='white') +
        coord_sf(expand=FALSE) 

    if (.save==T) {
        ggsave(
            filename = file.path(.dest_dir, .file_name),
            width = 6466, height = 4161, units = "px",
            dpi = 500,
            bg = "transparent"
        )
    }

    return(plt)
}



# plot a facet wrap of daily new infections 
# for the subsetted data of a given simulation 
plot_sim_daily_infections <- function(.data = outbrk_data) {
    # create a random colour scheme based on CEPI colours 
    cepi_prim_cols <- c(
        "#547dbf", "#ffa500", "#db4437", "#9d0f55", 
        "#682860", "#0080A0", "#F9DF79"
    )
    nclrs <- length(unique(outbrk_data$country))
    v_cols <- sample(colorRampPalette(cepi_prim_cols)(nclrs), nclrs)

    # plot daily cases 
    outbreak_plts <- ggplot(.data, aes(x=time_years, y=daily_infections_sim)) +
        facet_wrap(vars(country), scales = "free_y", ncol = 4) + # country or code
        geom_point(aes(col = country), size = 0.3) +
        geom_line(aes(col = country), linewidth=0.5) +
        geom_vline(
            data=df_timing, aes(xintercept = timing), 
            color="gainsboro", linetype='dashed', lwd=0.8) + 
        scale_color_manual(values = v_cols) +
        theme_light() +
        theme(legend.position = "none") + 
        labs(x='Time (years)', y='Daily infections') + 
        coord_cartesian(expand=FALSE)
    return(outbreak_plts)
}

# correct the facet wrap axes to create a bit of breathing space 
correct_facet_yaxis <- function(.gg_plt = outbreak_plts, .yax_factor = 1.1) {
    # get all facet ylims 
    df_ylims <- map(ggplot_build(.gg_plt)$layout$panel_params, ~data.frame(ymin=0, ymax=.x$y.range[2])) %>% bind_rows
    # create new scales for positions 
    position_scales <- pmap(df_ylims, function(ymin, ymax) {
        return(scale_y_continuous(labels = comma, limits = c(ymin, ymax*.yax_factor)))
    })
    # use the ggh4x package func to adjust all facet wrap scales 
    .gg_plt_return <- .gg_plt + facetted_pos_scales(y = position_scales)
    return(.gg_plt_return)
}

# get ggplot limits for axis adjustment 
get_plot_limits <- function(plot) {
    gb = ggplot_build(plot)
    xmin = gb$layout$panel_params[[1]]$x.range[1]
    xmax = gb$layout$panel_params[[1]]$x.range[2]
    ymin = gb$layout$panel_params[[1]]$y.range[1]
    ymax = gb$layout$panel_params[[1]]$y.range[2]
    list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}
