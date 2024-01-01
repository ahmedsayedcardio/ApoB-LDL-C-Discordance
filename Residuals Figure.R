#Create downscale to fit into 2 by 2
downscale <- 2.5

if(var_type == "continuous") {
# Continuous Figures ------------------------------------------------------


#Line types
linetypes <- c("solid" = "solid", "dashed" = "dashed", "dotted" = "dotted")

#Set line_color and width
line_color = "maroon"
line_width = 1.5

#Plot
resids_fig <- ggplot(data = resids_fig_df,
              aes(x = indep_var,
                  y = p50)) +
  #Add ribbons
  geom_ribbon(aes(ymin = p25, ymax = p75),
              alpha = 0.4,
              fill = "lightblue") +
  geom_ribbon(aes(ymin = p75, ymax = p97.5),
              alpha = 0.2,
              fill = "lightblue") +
  geom_ribbon(aes(ymin = p2.5, ymax = p25),
              alpha = 0.2,
              fill = "lightblue") +
  #Add horizontal line at zero (zero discordance)
  geom_hline(yintercept = 0, lwd = 1.25) +
  #Add Lines
  geom_smooth(aes(y = p50,
                  x = indep_var,
                  linetype = "solid"),
              color = line_color,
              lwd = line_width) +
  geom_smooth(aes(y = p2.5,
                  x = indep_var,
                  linetype = "dotted"),
              color = line_color,
              lwd = line_width) +
  geom_smooth(aes(y = p97.5,
                  x = indep_var,
                  linetype = "dotted"),
              color = line_color,
              lwd = line_width) +
  geom_smooth(aes(y = p25,
                  x = indep_var,
                  linetype = "dashed"),
              color = line_color,
              lwd = line_width) +
  geom_smooth(aes(y = p75,
                  x = indep_var,
                  linetype = "dashed"),
              color = line_color,
              lwd = line_width) +
  #Add linetype legend
  scale_linetype_manual(values = linetypes,
                        name = "",
                        breaks = c("solid", "dashed", "dotted"),
                        labels = c("solid" = "Median (50th percentile)",
                                   "dashed" = "25th and 75th percentiles",
                                   "dotted" = "2.5th and 97.5th percentiles")) +
  ggtitle(paste0("Discordance between predicted and actual apolipoprotein B across the spectrum of ", resids_reg_title),
          subtitle = "The solid black line represents perfect concordance (actual apoB = predicted apoB).\nThe area below the line indicates negative discordance (actual < predicted) and the area above the line indicates positive discordance (actual > predicted).") +
  #Add title
  #Add scales
  scale_x_continuous(
    name = paste0(resids_reg_title, " (", unit, ")") %>% capitalize,
    limits = c(NA, NA),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Discordance between predicted and actual\n apolipoprotein B (mg/dL)",
    breaks = seq(-100, 100, 10),
  ) +
  theme_pubclean() +
  theme(text = element_text(size = 23/downscale),
        plot.title=element_text(face = "bold",hjust = 0.0, size = 20/downscale),
        plot.subtitle = element_text(face = "bold", size = 12/downscale, hjust = 0.0, color = "grey45"),
        axis.text.x = element_text(size = 20/downscale, face = "bold"),
        axis.text.y = element_text(size = 20/downscale, face = "bold"),
        axis.title.x = element_text(size = 25/downscale, face = "bold"),
        axis.title.y = element_text(size = 25/downscale, face = "bold"),
        axis.line = element_line(colour = "black", size = 2/downscale),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm")/downscale,
        legend.text = element_text(face = "bold"),
        legend.key.width = unit(6, "cm")/downscale,
        legend.key = element_rect('transparent'),
        legend.position = "bottom") +
  #Remove fill in legend
  guides(linetype = guide_legend(override.aes = list(fill = NA)))

}


#Set seed to pick points at random in a fixed manner
set.seed(100)

if(var_type == "categorical") {
# Categorical Figures -----------------------------------------------------

#Set width
width = 0.5
#Plot
resids_fig <- ggplot(data = resids_fig_df,
                     aes(x = indep_var,
                         y = p50,
                         color = indep_var)) +
  #Add Lines & Ribbons
  geom_errorbar(aes(ymin = p2.5, ymax = p97.5), lwd = 1.25, width = width) +
  geom_boxplot(aes(ymin = p2.5, ymax = p97.5, lower = p25, upper = p75, middle = p50), 
               stat = "identity",
               lwd = 1.25, width = width) +
  #Add horizontal line at zero (zero discordance)
  geom_hline(yintercept = 0, lwd = 1.25, linetype = "dashed") +
  #Add beeswarm
  geom_beeswarm(data = x %>% group_by(!!as.symbol(resids_reg_var)) %>% slice_sample(n = 1000, replace = FALSE), 
                aes(y = resids, x = !!as.symbol(resids_reg_var), 
                    fill = !!as.symbol(resids_reg_var),
                    color = !!as.symbol(resids_reg_var)),
             inherit.aes = FALSE, alpha = 0.4, cex = 0.8, size = 2/downscale) +
  #Add color scale
  scale_color_jama() +
  scale_fill_jama() +
  #Add title
  ggtitle(paste0("Discordance between predicted and actual apolipoprotein B according to ", tolower(resids_reg_title)),
          subtitle = "The dashed black line represents perfect concordance (actual apoB = predicted apoB).\nThe area below the line indicates negative discordance (actual < predicted) and the area above the line indicates positive discordance (actual > predicted).") +
  #Add scales
  scale_x_discrete(
    name = paste0(resids_reg_title) %>% capitalize,
  ) +
  scale_y_continuous(
    name = "Discordance\n(actual apoB - predicted apoB; mg/dL)",
    breaks = seq(-100, 100, 10),
    limits = c(min(resids_fig_df$p2.5) * 1.25, max(resids_fig_df$p97.5) * 1.25)
  ) +
  theme_pubclean() +
  theme(text = element_text(size = 23/downscale),
        plot.title=element_text(face = "bold", hjust = 0.0, size = 22/downscale),
        plot.subtitle = element_blank(),
        axis.text.x = element_text(size = 20/downscale, face = "bold"),
        axis.text.y = element_text(size = 18/downscale, face = "bold"),
        axis.title.x = element_text(size = 25/downscale, face = "bold"),
        axis.title.y = element_text(size = 25/downscale, face = "bold"),
        axis.line = element_line(colour = "black", size = 2/downscale),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm")/downscale,
        legend.position = "none") +
  #Remove fill in legend
  guides(linetype = guide_legend(override.aes = list(fill = NA)))

}

