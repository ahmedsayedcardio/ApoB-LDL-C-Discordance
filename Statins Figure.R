downscale = 2.5
#Set width
width = 0.5
#Plot
statins_resids_fig <- ggplot(data = resids_fig_df,
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
  geom_beeswarm(data = x_statins %>% group_by(!!as.symbol(resids_reg_var)) %>% slice_sample(n = 1000, replace = FALSE), 
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
