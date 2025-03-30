# Plot function for Fig.1 and Fig.2

plot_volatiles_Fig1_2 <- function(data, measurevar,
                                 groupvars = c("Treatment", "Iteration")) {
  
  ## Summarize the data
  
  vol <- summarySE(data, measurevar = measurevar, groupvars = groupvars)
  pd <- position_dodge(0.1)
  
  ## Convert measurevar to a symbol for tidy evaluation
  
  measure_sym <- sym(measurevar)
  
  ## Create the plot
  
  volatile_plot <- ggplot (vol, aes(x = Iteration, y = !!measure_sym, colour = Treatment, fill = Treatment)) +
    geom_errorbar (aes (ymin = !!measure_sym - se, ymax = !!measure_sym + se), width = 0.1, position = pd) +
    geom_line (position = pd, size = 0.7) +
    geom_point (position = pd, size = 3, shape = 21) +
    theme_bw () +
    theme(axis.title.y = element_blank()) +
    scale_color_manual (values = c("#01665e", "#5ab4ac", "#8c510a", "#d8b365"), 
                        labels = c("A" = "High R:FR + HIPVs",
                                   "B" = "High R:FR + cVOCs",
                                   "C" = "Low R:FR + HIPVs",
                                   "D" = "Low R:FR + cVOCs")) +
    scale_x_continuous (breaks = scales::pretty_breaks(n = 13)) +
    scale_fill_manual (values = c("#01665e", "#5ab4ac", "#8c510a", "#d8b365"),
                       labels = c("A" = "High R:FR + HIPVs",
                                  "B" = "High R:FR + cVOCs",
                                  "C" = "Low R:FR + HIPVs",
                                  "D" = "Low R:FR + cVOCs")) +
    ggtitle (paste (measurevar))  # Dynamic title for clarity
  
  return(volatile_plot)
}

# Plot function for Fig.3a

plot_volatiles_Fig3a <- function(data, measurevar, groupvars = c("Emitter", "Iteration")) {
  
  ## Summarize the data
  
  vol <- summarySE(data, measurevar = measurevar, groupvars = groupvars)
  pd <- position_dodge(0.1)
  
  ## Convert measurevar to a symbol for tidy evaluation
  
  measure_sym <- sym(measurevar)
  
  ## Create the plot
  
  volatile_plot <- ggplot (vol, aes(x = Iteration, y = !!measure_sym, colour = Emitter, fill = Emitter)) +
    geom_errorbar (aes (ymin = !!measure_sym - se, ymax = !!measure_sym + se), width = 0.1, position = pd) +
    geom_line (position = pd, size = 0.7) +
    geom_point (position = pd, size = 3, shape = 21) +
    theme_bw () +
    theme(axis.title.y = element_blank()) +
    scale_color_manual (values = c("#5ab4ac","#01665e","#d8b365", "#8c510a"), 
                        labels = c("C" = "Low R:FR + cVOCs from High R:FR",
                                   "D" = "Low R:FR + HIPVs from High R:FR",
                                   "G" = "Low R:FR + cVOCs from Low R:FR",
                                   "H" = "Low R:FR + HIPVs from Low R:FR")) +
    scale_x_continuous (breaks = scales::pretty_breaks(n = 13)) +
    scale_fill_manual (values = c("#5ab4ac","#01665e","#d8b365", "#8c510a"),
                       labels = c("C" = "Low R:FR + cVOCs from High R:FR",
                                  "D" = "Low R:FR + HIPVs from High R:FR",
                                  "G" = "Low R:FR + cVOCs from Low R:FR",
                                  "H" = "Low R:FR + HIPVs from Low R:FR")) +
    ggtitle (paste (measurevar))  # Dynamic title for clarity
  
  return(volatile_plot)
}

# Plot function for Fig.3b

plot_volatiles_Fig3b <- function(data, measurevar, groupvars = c("Emitter", "Iteration")) {
  
  ## Summarize the data
  
  vol <- summarySE(data, measurevar = measurevar, groupvars = groupvars)
  pd <- position_dodge(0.1)
  
  ## Convert measurevar to a symbol for tidy evaluation
  
  measure_sym <- sym(measurevar)
  
  ## Create the plot
  
  volatile_plot <- ggplot (vol, aes(x = Iteration, y = !!measure_sym, colour = Emitter, fill = Emitter)) +
    geom_errorbar (aes (ymin = !!measure_sym - se, ymax = !!measure_sym + se), width = 0.1, position = pd) +
    geom_line (position = pd, size = 0.7) +
    geom_point (position = pd, size = 3, shape = 21) +
    theme_bw () +
    theme(axis.title.y = element_blank()) +
    scale_color_manual (values = c("#5ab4ac","#01665e","#d8b365", "#8c510a"), 
                        labels = c("A" = "High R:FR + cVOCs from High R:FR",
                                   "B" = "High R:FR + HIPVs from High R:FR",
                                   "E" = "High R:FR + cVOCs from Low R:FR",
                                   "F" = "High R:FR + HIPVs from Low R:FR")) +
    scale_x_continuous (breaks = scales::pretty_breaks(n = 13)) +
    scale_fill_manual (values = c("#5ab4ac","#01665e","#d8b365", "#8c510a"),
                       labels = c("A" = "High R:FR + cVOCs from High R:FR",
                                  "B" = "High R:FR + HIPVs from High R:FR",
                                  "E" = "High R:FR + cVOCs from Low R:FR",
                                  "F" = "High R:FR + HIPVs from Low R:FR")) +
    ggtitle (paste (measurevar))  # Dynamic title for clarity
  
  return(volatile_plot)
}

# Bar plot for Fig. S5 & S6

plot_Fig_S5_6 <- function(data, measurevar, y_label, colors, variable_name) {
  
  
  
  # Create the plot
  p <- ggbarplot(data, x = "Treatment", y = measurevar, 
                 add = c("mean_se"), fill = "Treatment",
                 position = position_dodge(0.8)) +
    theme(axis.title = element_text(colour = "black", size = 10), 
          axis.text = element_text(colour = "black", size = 8), 
          legend.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key.size = unit(1, "cm"), 
          legend.key.width = unit(1, "cm"),
          axis.text.x = element_blank()) +
    geom_jitter(mapping = aes(x = Treatment, y = .data[[measurevar]], fill = Treatment), 
                position = position_jitterdodge(0.0), size = 2, alpha = 0.2) +
    scale_fill_manual(values = colors) + 
    ylab(y_label) +
    ggtitle(paste(variable_name))
  
  return(p)
}

