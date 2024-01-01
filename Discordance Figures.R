#Set titles/breaks etc. accordingly
xtitle <- paste0(lipid_title, " (mg/dL)")
xlimits <- if(lipid_var == "non_hdl") { c(60, 240) } else { c(40, 220) }
first_col_name <- ifelse(lipid_var == "non_hdl", "non-HDL cholesterol", "low-density lipoprotein cholesterol")
xbreaks <- if(lipid_var == "non_hdl") { seq(60, 240, 20) } else { seq(40, 220, 20) }
ytitle <- paste0("Apolipoprotein B (mg/dL)")
ylimits <- c(NA, NA)
ybreaks <- seq(0, 1000, 20)
title <-  paste0("Individual variation in apolipoprotein B levels across the spectrum of ", lipid_title)

#Line types
linetypes <- c("solid" = "solid", "dashed" = "dashed", "dotted" = "dotted")

#Set line_color and width
line_color = "maroon"
line_width = 1.5

#Plot
graph <- ggplot(data = fig_df,
              aes(x = indep_var,
                  y = p50)) +
  #Ribbon
  geom_ribbon(aes(ymin = p25, ymax = p75),
              fill = "lightblue",
              alpha = 0.4) +
  geom_ribbon(aes(ymin = p75, ymax = p97.5),
              fill = "lightblue",
              alpha = 0.2) +
  geom_ribbon(aes(ymin = p2.5, ymax = p25),
              fill = "lightblue",
              alpha = 0.2) +
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
  #Title
  ggtitle(title) +
  #Add linetype legend
  scale_linetype_manual(values = linetypes,
                        breaks = c("solid", "dashed", "dotted"),
                        name = "",
                        labels = c("Median (50th percentile)",
                                   "25th and 75th percentiles",
                                   "2.5th and 97.5th percentiles")) +
  #Add scales
  scale_x_continuous(
    name = capitalize(xtitle),
    breaks = xbreaks,
    expand = c(0.0, 0.0)
  ) +
  scale_y_continuous(
    name = ytitle,
    breaks = ybreaks
  ) +
  theme_pubclean() +
  theme(text = element_text(size = 23),
        plot.title=element_text(face = "bold",hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.x.top = element_text(size = 15, face = "bold"),
        axis.text.x.top = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.title.y.right = element_text(size = 15, face = "bold"),
        axis.text.y.right = element_text(size = 15, face = "bold"),
        axis.line = element_line(colour = "black", linewidth = 1.2),
        panel.border = element_rect(colour = "black", fill=NA, size=1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = c(0.80, 0.25),
        legend.key.width = unit(2, "cm"),
        legend.background = element_rect("transparent")) +
  #Remove fill in legend
  guides(linetype = guide_legend(override.aes = list(fill = NA))) +
  #Add cartesian coords
  coord_cartesian(xlim = xlimits,
                  ylim = fig_df %>% filter(indep_var %between% xlimits) %>% select(p50) %>% pull %>% range)
 

#Adding table below the graph
#First, rearrange table and label it properly
gtable <- table_df %>% relocate(indep_var) %>%
  mutate(ratio =
           paste0((100*(1 - (p50/p97.5))) %>% round(0) , "% lower to ",
         paste0((100*((p50/p2.5) - 1)) %>% round(0) ), "% higher")
           ) %>%
  select(- ratio) %>% #Remove ratio since Dr. Navar believes it might be too confusing.
  rename_all(., ~ c(paste0("Measured ", first_col_name, " (mg/dL)"), 
                    "2.5th percentile", "25th percentile",
                    "50th percentile", "75th percentile",
                    "97.5th percentile")) %>%
  gt %>%
  tab_spanner(label = "Potential range of apolipoprotein B values (mg/dL)",
              columns = 2:6) %>%
  cols_align("center") %>%
  opt_table_font(weight = "bolder") %>% 
  cols_width(contains("Measured") ~ px(1100),
             contains("potential") ~ px(850),
             everything() ~ px(400)) %>%
  tab_options(table.font.size = 40) %>%
  cols_merge(rows = c(1, 2), columns = 1) %>%
  bstfun::as_ggplot(vwidth = 14000)
#Combine graph and table
fig <- wrap_plots(graph, gtable, nrow = 2,
                    heights = c(2, 0.8))

#Add figure formats (MH is going to be among the main figures of the manuscript and should be a PDF. The others should be rendered as PNGs to facilitate embedding them in the word document)
fig_format <- if(lipid_var == "mh") {".pdf"} else {".png"}
#Produce plot
ggsave(fig,
       filename = paste0(output_folder, lipid_title, fig_format),
       dpi = 600,
       width = 16,
       height = 9)

