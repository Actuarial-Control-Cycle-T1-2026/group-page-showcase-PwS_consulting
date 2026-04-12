# -------------------------------------------------------------------------------------------------------------------
# Section 2 - AGGREGATE LOSS DISTRIBUTION - Annotated with premium and tail markers
# -------------------------------------------------------------------------------------------------------------------

plot_loss_distribution <- function(product, loss_vec, gross_vec, year_label) {
  
  q99    <- quantile(loss_vec,  0.99)
  q99g   <- quantile(gross_vec, 0.99)
  
  df <- data.frame(
    loss    = c(loss_vec,  gross_vec),
    type    = rep(c("Net Loss : After Design", "Gross Loss : Before product design"), each = length(loss_vec)))
  
  x_max <- quantile(gross_vec, 1)
  
  ggplot(df, aes(x = loss, fill = type, colour = type)) +
    geom_density(alpha = 0.20, linewidth = 0.8) +
    
    # VaR99 lines
    geom_vline(aes(xintercept = q99, linetype = "99th Percentile VaR"), 
               colour = "#2b8cbe", linewidth = 0.8) +
    geom_vline(aes(xintercept = q99g, linetype = "99th Percentile VaR"), 
               colour = "#d73027", linewidth = 0.8) +
    
    scale_x_continuous(
      labels = label_dollar(prefix = "$ ", scale = 1e-6, suffix = "m"),
      limits = c(0, x_max)
    ) +
    scale_fill_manual(values   = c("Gross Loss : Before product design" = "#d73027", "Net Loss : After Design" = "#2b8cbe")) +
    scale_colour_manual(values = c("Gross Loss : Before product design" = "#d73027",  "Net Loss : After Design" = "#2b8cbe")) +

    scale_linetype_manual(name = "", values = c("99th Percentile VaR" = "dashed")) +
    guides(linetype = guide_legend(override.aes = list(colour = "black"))) +
    
    labs(
      title    = paste0(product, " Aggregate Loss Distribution — ", year_label),
      subtitle = "Comparison of risk profile before and after product design",
      x        = "Annual Aggregate Loss", 
      y        = "Density",
      fill     = "", 
      colour   = ""
    ) +
    
    theme_minimal() + 
    theme( 
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

# -------------------------------------------------------------------------------------------------------------------
# Section 2 - LONG-TERM PROJECTION (either plot side by side or on one panel)
# -------------------------------------------------------------------------------------------------------------------

plot_fan_gross_net <- function(product, gross_profit_mat, profit_mat) {
  
  years <- 10
  
  fan_data <- purrr::map_dfr(1:years, function(yr) {
    net_profit      <- profit_mat[, yr]
    grs_profit    <- gross_profit_mat[, yr]
    
    bind_rows(
      data.frame(
        year  = yr, type = "Net (After Product Design)",
        p5    = quantile(net_profit, 0.05),
        p25   = quantile(net_profit, 0.25),
        p50   = quantile(net_profit, 0.50),
        p75   = quantile(net_profit, 0.75),
        p95   = quantile(net_profit, 0.95)
      ),
      data.frame(
        year  = yr, type = "Gross (Before Product Design)",
        p5    = quantile(grs_profit, 0.05),
        p25   = quantile(grs_profit, 0.25),
        p50   = quantile(grs_profit, 0.50),
        p75   = quantile(grs_profit, 0.75),
        p95   = quantile(grs_profit, 0.95)
      )
    )
  }) %>%
    mutate(type = factor(type, levels = c(
      "Gross (Before Product Design)",
      "Net (After Product Design)"
    )))
  
  col_net   <- "#2b8cbe"
  col_gross <- "#d73027"
  
  make_panel <- function(data, type_label, colour) {
    ggplot(data, aes(x = year)) +
      geom_ribbon(aes(ymin = p5,  ymax = p95),
                  fill = colour, alpha = 0.15) +
      geom_ribbon(aes(ymin = p25, ymax = p75),
                  fill = colour, alpha = 0.30) +
      geom_line(aes(y = p50),
                colour = colour, linewidth = 1.2) +
      geom_point(aes(y = p50),
                 colour = colour, size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 colour = "black", linewidth = 0.7) +
      annotate("text", x = 1, y = 0,
               label = "Break-even", vjust = -0.6, hjust = 0,
               size = 2.8, colour = "black") +
      scale_x_continuous(breaks = 1:years) +
      scale_y_continuous(
        labels = label_dollar(prefix = "Đ ", scale = 1e-6, suffix = "M")
      ) +
      labs(
        title    = type_label,
        x        = "Projection Year",
        y        = "Annual Profit",
        subtitle = "Dark band (P25-P75 profits) | Light band (P5-P95 profits)",
      ) +
      theme_minimal() +
      theme(
        plot.title         = element_text(face = "bold", size = 11,
                                          colour = colour),
        plot.subtitle      = element_text(size = 8, colour = "grey40"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.border       = element_rect(colour = "black", fill = NA,
                                          linewidth = 0.5)
      )
  }
  
  p_gross <- make_panel(
    fan_data %>% filter(type == "Gross (Before Product Design)"),
    "Before Product Design",
    col_gross
  )
  
  p_net <- make_panel(
    fan_data %>% filter(type == "Net (After Product Design)"),
    "After Product Design",
    col_net
  )
  
  # Force same y axis across both panels
  y_min <- min(fan_data$p5)
  y_max <- max(fan_data$p95)
  
  p_gross <- p_gross + coord_cartesian(ylim = c(y_min, y_max))
  p_net   <- p_net   + coord_cartesian(ylim = c(y_min, y_max))
  
  gridExtra::grid.arrange(
    p_gross, p_net, ncol = 2,
    top = grid::textGrob(
      paste0(product, ": Profit Fan — Before vs After Product Design"),
      gp = grid::gpar(fontface = "bold", fontsize = 12)
    )
  )
}

plot_fan_gross_net_single <- function(product, gross_profit_mat, profit_mat) {
  
  years <- 10
  
  # -------------------------
  # Build quantile dataframe
  # -------------------------
  fan_data <- purrr::map_dfr(1:years, function(yr) {
    
    net_profit <- profit_mat[, yr]
    grs_profit <- gross_profit_mat[, yr]
    
    bind_rows(
      data.frame(
        year  = yr, type = "Net (After Product Design)",
        p5    = quantile(net_profit, 0.05),
        p25   = quantile(net_profit, 0.25),
        p50   = quantile(net_profit, 0.50),
        p75   = quantile(net_profit, 0.75),
        p95   = quantile(net_profit, 0.95)
      ),
      data.frame(
        year  = yr, type = "Gross (Before Product Design)",
        p5    = quantile(grs_profit, 0.05),
        p25   = quantile(grs_profit, 0.25),
        p50   = quantile(grs_profit, 0.50),
        p75   = quantile(grs_profit, 0.75),
        p95   = quantile(grs_profit, 0.95)
      )
    )
  }) %>%
    mutate(type = factor(type, levels = c(
      "Gross (Before Product Design)",
      "Net (After Product Design)"
    )))
  
  # -------------------------
  # Colours
  # -------------------------
  col_net   <- "#2b8cbe"
  col_gross <- "#d73027"
  
  # -------------------------
  # Plot: single panel
  # -------------------------
  ggplot(fan_data, aes(x = year)) +
    
    # Gross ribbons
    geom_ribbon(
      data = filter(fan_data, type == "Gross (Before Product Design)"),
      aes(ymin = p5, ymax = p95),
      fill = col_gross, alpha = 0.15
    ) +
    geom_ribbon(
      data = filter(fan_data, type == "Gross (Before Product Design)"),
      aes(ymin = p25, ymax = p75),
      fill = col_gross, alpha = 0.30
    ) +
    
    # Net ribbons
    geom_ribbon(
      data = filter(fan_data, type == "Net (After Product Design)"),
      aes(ymin = p5, ymax = p95),
      fill = col_net, alpha = 0.15
    ) +
    geom_ribbon(
      data = filter(fan_data, type == "Net (After Product Design)"),
      aes(ymin = p25, ymax = p75),
      fill = col_net, alpha = 0.30
    ) +
    
    # Median lines
    geom_line(
      data = filter(fan_data, type == "Gross (Before Product Design)"),
      aes(y = p50, colour = "Gross (Before Product Design)"),
      linewidth = 1.2
    ) +
    geom_line(
      data = filter(fan_data, type == "Net (After Product Design)"),
      aes(y = p50, colour = "Net (After Product Design)"),
      linewidth = 1.2
    ) +
    
    # Points
    geom_point(
      data = filter(fan_data, type == "Gross (Before Product Design)"),
      aes(y = p50, colour = "Gross (Before Product Design)"), size = 2
    ) +
    geom_point(
      data = filter(fan_data, type == "Net (After Product Design)"),
      aes(y = p50, colour = "Net (After Product Design)"), size = 2
    ) +
    
    # Break-even
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.7) +
    annotate("text", x = 1, y = 0, label = "Break-even",
             vjust = -0.6, hjust = 0, size = 2.8, colour = "black") +
    
    # Scales
    scale_colour_manual(values = c(
      "Gross (Before Product Design)" = col_gross,
      "Net (After Product Design)"   = col_net
    )) +
    
    scale_x_continuous(breaks = 1:years) +
    scale_y_continuous(labels = label_dollar(prefix = "Đ ", scale = 1e-6, suffix = "M")) +
    
    # Labels
    labs(
      title = paste0(product, "— Before vs After Product Design"),
      subtitle = "Dark band (P25-P75 profits) | Light band (P5-P95 profits)",
      x = "Projection Year",
      y = "Annual Profit",
      colour = "Series"
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title    = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 8, colour = "grey40"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(colour = "black", fill = NA,
                                        linewidth = 0.5)
    ) 
}


# -------------------------------------------------------------------------------------------------------------------
# Section 3 - Scenarios
# -------------------------------------------------------------------------------------------------------------------

scenarios_plot <- function(product, best_yearly,base_yearly,worst_yearly){ #yearly lists from monte carlo simulation
  
  plot_data <- bind_rows(
    best_yearly %>% mutate(Scenario = "Bull (Optimistic)"),
    base_yearly %>% mutate(Scenario = "Base (Expected)"),
    worst_yearly %>% mutate(Scenario = "Bear (Pessimistic)")
  )
  
  ggplot(plot_data, aes(x = Year, y = EV_profit, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    # Formatting Y axis to Millions for readability
    scale_y_continuous(labels = label_dollar(prefix = "$", scale = 1e-6, suffix = "M")) +
    scale_x_continuous(breaks = 1:10) +
    # Using a professional color palette
    scale_color_manual(values = c(
      "Bull (Optimistic)" = "#2ECC71",  # Green
      "Base (Expected)"   = "#3498DB",  # Blue
      "Bear (Pessimistic)" = "#E74C3C"   # Red
    )) +
    labs(
      title = paste0(product, "— Profitability by scenario (10-year horizon)"),
      subtitle = "Expected Value (EV) of Net Profit across Bull, Base, and Bear cases",
      x = "Projection Year",
      y = "Expected Net Profit"
    ) +
    theme_minimal() + 
    theme( 
      plot.title = element_text(face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}
