# Ans1. Load required R-packages
library(here)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)

# Ans2. element_blank() example: removing panel.grid lines
el_blank_example <- iris %>% ggplot(aes(Petal.Length, Petal.Width)) +
                    geom_jitter() +
                    theme_bw() +
                    theme(panel.grid = element_blank())
png(here::here("Lecture-3", "PlotsLec3", "el_blank_example.png"))
el_blank_example
dev.off()

# Ans3. Plot all theme text elements.
text_el_plt <- iris %>% ggplot(aes(Petal.Length, 
                                   Petal.Width, 
                                   color=Species)) +
                geom_point(position = position_jitter(seed=123)) +
                theme_bw() +
                labs(color = "legend.title",
                     title = "plot.title",
                     subtitle = "plot.subtitle",
                     caption = "plot.caption",
                     tag = "plot.tag",
                     x = "axis.title.x",
                     y = "axis.title.y") +
       theme(text = element_text(size = 15, color = "#2A4DFA"))
png(here::here("Lecture-3", "PlotsLec3", "text_el_plt.png"), height=320,
    width = 480)
text_el_plt
dev.off()

# Ans4. Display strip.text.x and strip.text.y in your plot
data(mpg)
str(mpg)
mpg_subset <- mpg %>% filter(class %in% c("compact", "suv")) %>%
              mutate(x_strip_fac = factor(drv, 
                      labels = c("strip.x.1", "strip.x.2", "strip.x.3")),
                     y_strip_fac = factor(class, 
                      labels = c("strip.y.1", "strip.y.2")))


strip_text_plt <- mpg_subset %>% ggplot(aes(cty, hwy)) +
  geom_point() +
  facet_grid(y_strip_fac ~ x_strip_fac) +
  theme_gray() +
  theme(strip.text.x = element_text(size = 15, color = "#FA435F"),
        strip.text.y = element_text(size = 15, color = "#2A4DFA"),
        strip.background = element_rect(fill = "green"))

png(here::here("Lecture-3", "PlotsLec3", "strip_text_plt.png"),
    height = 300)
strip_text_plt
dev.off()

# Ans5. Enhance the display of strip elements by 
#       changing color, font family and size of the text
iris_enhanced_strip <- iris %>%
              ggplot(aes(Petal.Length, Petal.Width)) +
              geom_point(position = position_jitter(seed = 123),
                         color = "orange") +
              facet_wrap(~ Species) +
              theme_bw() +
              theme(strip.background = element_rect(fill = "white"),
                   strip.text = element_text(family = "serif",
                                             size = 20,
                                             color = "#8b0000"),
              axis.ticks = element_blank(),
              plot.title = element_text(size=24)) +
        ggtitle("Modified strip.text")


png(here::here("Lecture-3", "PlotsLec3", "iris_enhanced_strip.png"), 
    height = 250, width=480)
iris_enhanced_strip
dev.off()

# Ans6. Modify legend.title and legend.text
mod_leg_title_text_plt <- iris %>%
                          ggplot(aes(Petal.Length, 
                                     Petal.Width, 
                                     color = Species)) +
                          geom_point(position = position_jitter(seed = 123),
                                     alpha = 0.30) +
                          theme_classic() +
                          theme(axis.title = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(size=18, 
                                                    face="bold", 
                                                    family="serif"),
legend.title = element_text(family = "serif", size = 16, color = "#8F2421"),
legend.text = element_text(family = "serif", face = "italic",
                           size = 16, color = "#DB2C27")) +
  
    scale_color_manual(labels = c("Setosa", "Versicolor", "Virginica"),
                       values = c("#72DB48","#5359DB", "#DB27CB")) +
                          ggtitle("Modified legend.text and legend.title")

png(here::here("Lecture-3", "PlotsLec3", "mod_leg_title_text_plt.png"), 
    height = 250, width=480)
mod_leg_title_text_plt
dev.off()

# Ans7. Improve the legend guide of the above plot
enhanced_leg_guide <- mod_leg_title_text_plt +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
    ggtitle("Enhanced legend guide")

png(here::here("Lecture-3", "PlotsLec3", "enhanced_leg_guide.png"), 
   height = 250, width=480)
enhanced_leg_guide
dev.off()

# Ans8. Place the legend on the top
leg_on_top <- enhanced_leg_guide +
              theme(legend.position = "top") +
              ggtitle("Legend on top")

png(here::here("Lecture-3", "PlotsLec3", "leg_on_top.png"), 
    height = 250, width=350)
leg_on_top
dev.off()

# Ans9. Place the legend inside the plot
leg_in_plt <- enhanced_leg_guide +
              theme(legend.position = c(0.15, 0.85),
                    legend.margin = margin(0,0,0,0,  "mm")) +
              ggtitle("Legend inside panel area")

leg_in_plt0 <- enhanced_leg_guide +
  theme_bw()+
  theme(legend.position = c(0.15, 0.85),
        legend.margin = margin(2,2,2,2,  "cm")) +
  ggtitle("Legend inside panel area")

png(here::here("Lecture-3", "PlotsLec3", "leg_in_plt.png"), 
    height = 250, width=350)
leg_in_plt
dev.off()

# Ans10. Create the Curtin theme
custom_theme <- theme(plot.background = element_rect(color = "#E09B34"),
                      panel.background = element_rect(fill = "#E0BE34"),
                      panel.grid.major = element_line(color = "gray50"),
                      panel.grid.minor = element_blank(),
                      axis.text = element_text(face = "bold",
                                               family = "serif"),
                      axis.title = element_text(face = "bold",
                                                family = "serif"))

iris_custom_theme <- iris %>% ggplot(aes(Petal.Length,Petal.Width)) +
  geom_point(position = position_jitter(seed = 123), color = "white",
             size = 1.3) +
  geom_smooth(method = "lm", se=FALSE, color = "black") +
  custom_theme +
  ggtitle("Customised theme inspired by Curtin logo")

png(here::here("Lecture-3", "PlotsLec3", "iris_custom_theme.png"), 
    height = 300, width=480)
iris_custom_theme
dev.off()

# Ans11. Use the wall street journal theme
data(mtcars)
data(mpg)

mtcars_default_plt <- mtcars %>%
  ggplot(aes(disp, mpg, color = factor(cyl))) +
  geom_point() +
  labs(color = "Cylinder",
       x = "Engine displacement (cu.in)",
       y = "Miles per gallon",
       title = "Default: theme_grey()") +
  theme(legend.position = c(0.9, 0.8),
        legend.margin = margin(0,0,0,0, "mm"))

mtcars_wsj_plt <- mtcars_default_plt +
  theme_wsj() +
  theme(legend.position = c(0.65, 0.9)) +
  theme(axis.title = element_text(size=12),
        plot.title = element_text(size=20),
        legend.title = element_text(size=10)) +
  ggtitle("theme_wsj()")

png(here::here("Lecture-3", "PlotsLec3", "mtcars_wsj_plt.png"), 
    height = 300, width=480)
mtcars_wsj_plt
dev.off()

# Ans12. Use the five thirty eight theme
mtcars_538_plt <- mtcars_default_plt +
                  theme_fivethirtyeight() +
                  theme(legend.position = c(0.65, 0.9)) +
                  theme(axis.title = element_text()) +
                  ggtitle("theme_fivethirtyeight()")
png(here::here("Lecture-3", "PlotsLec3", "mtcars_538_plt.png"), 
    height = 300, width=480)
mtcars_538_plt
dev.off()

# Ans13. Use the Economist theme
mtcars_econ_plt <- mtcars_default_plt +
                   theme_economist() +
                   theme(legend.position = c(0.90, 0.80)) +
                   theme(axis.title = element_text()) +
                   ggtitle("theme_economist()")
png(here::here("Lecture-3", "PlotsLec3", "mtcars_econ_plt.png"), 
    height = 300, width=480)
mtcars_econ_plt
dev.off()


# 13 scale_x_continuous() example
data(diamonds)

diamonds_plt <- diamonds %>%
  ggplot(aes(carat, price)) +
  geom_point(size = 0.1, alpha=0.10, color = "#D55E00") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(
    breaks = c(0, 5000, 10000, 15000),
    labels = c("0$", "5,000$", "10,000$", "15,000$")
  ) +
  theme_wsj() +
  theme(axis.title = element_text()) +
  labs(x = "log10(carat)")

png(here::here("Lecture-3", "PlotsLec3", "diamonds_plt.png"), 
    height = 300, width=480)
diamonds_plt
dev.off()

# 14. scale_color_brewer example
mtcars_seq_brewer <- ggplot(mtcars, aes(disp, mpg, color = factor(cyl))) +
  geom_point(position = position_jitter(seed = 123), size=2, shape=19) +
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  scale_color_brewer("Cylinder", type = "seq", palette = "YlOrRd") +
  ggtitle("Sequential color palette = 'YlOrRd'")
png(here::here("Lecture-3", "PlotsLec3", "mtcars_seq_brewer.png"), 
    height = 300, width=350)
mtcars_seq_brewer
dev.off()


# 15. scale_color_manual() example
mtcars_base_plt <- ggplot(mtcars, aes(disp, mpg, color = factor(cyl))) +
  geom_point(position = position_jitter(seed = 123), size=2, shape=19) +
  theme_classic() +
  theme(text = element_text(family = "serif"))

col_vec <- c("4" = "blue", "6" = "green", "8" = "red")
mtcars_col_manual <- mtcars_base_plt +
  scale_color_manual("Cylinders",
                     values = col_vec)

png(here::here("Lecture-3", "PlotsLec3", "mtcars_col_manual.png"), 
    height = 300, width=350)
mtcars_col_manual
dev.off()





