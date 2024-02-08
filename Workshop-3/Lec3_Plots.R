library(here)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(RColorBrewer)

data(iris)
# 1. element_blank() example: removin panel.grid.major lines
iris %>% ggplot(aes(Petal.Length, Petal.Width)) +
         geom_jitter() +
         theme_bw() +
         theme(panel.grid = element_blank())

# 2. Themes text elements
iris %>% ggplot(aes(Petal.Length, Petal.Width, color=Species)) +
  geom_jitter() +
  theme_bw() +
  labs(color = "legend.title",
       title = "plot.title",
       subtitle = "plot.subtitle",
       caption = "plot.caption",
       tag = "plot.tag",
       x = "axis.title.x",
       y = "axis.title.y") +
  theme(text = element_text(size = 30, color = "#2A4DFA"))

# 3. strip.text.x and strip.text.y
data(mpg)
str(mpg)
mpg_subset <- mpg %>% filter(class %in% c("compact", "suv")) %>%
              mutate(x_strip_fac = factor(drv, labels = c("strip.x.1", "strip.x.2", "strip.x.3")),
                     y_strip_fac = factor(class, labels = c("strip.y.1", "strip.y.2")))


strip_text_plt <- mpg_subset %>% ggplot(aes(cty, hwy)) +
               facet_grid(y_strip_fac ~ x_strip_fac) +
               theme_gray() +
               theme(strip.text.x = element_text(size = 30, color = "#FA435F"),
                     strip.text.y = element_text(size = 30, color = "#2A4DFA"),
                     strip.background = element_rect(fill = "white"),
                     axis.title = element_blank(),
                     axis.text = element_blank())
pdf(here::here("Lecture-3", "PlotsLec3", "StripText.pdf"), 
    height = 4)
strip_text_plt
dev.off()

# 4. Modify strip.text
iris_sub <- iris %>% filter(Species %in% c("setosa", "virginica")) %>%
               mutate(Species = as.factor(Species))
iris_plt1 <- iris_sub %>%
            ggplot(aes(Petal.Length, Petal.Width)) +
            geom_point(position = position_jitter(seed = 123), color="orange") +
            facet_wrap(~ Species) +
            theme_bw() +
            theme(axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  plot.title = element_text(size=24)) +
            ggtitle("Default strip.text")

iris_plt2 <- iris_sub %>%
  ggplot(aes(Petal.Length, Petal.Width)) +
  geom_point(position = position_jitter(seed = 123),
             color = "orange") +
  facet_wrap(~ Species) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(family = "serif",
                                  size = 20,
                                  color = "#8b0000"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=24)) +
  ggtitle("Modified strip.text")

stripTextPlt2 <- ggarrange(iris_plt1, iris_plt2,
                           ncol = 2)
pdf(here::here("Lecture-3", "PlotsLec3", "StripText2.pdf"), 
    height = 3.2, width=8)
stripTextPlt2
dev.off()

# 5. Modify legend.title and legend.text
def_leg_plt <- iris %>%
  ggplot(aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(position = position_jitter(seed = 123),
             alpha = 0.30) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=18, face="bold", family="serif")) +
  scale_color_manual(labels = c("Setosa", "Versicolor", "Virginica"),
                     values = c("#72DB48", "#5359DB", "#DB27CB")) +
  ggtitle("Default legend.text and legend.title")

mod_leg_plt <- iris %>%
  ggplot(aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(position = position_jitter(seed = 123),
             alpha = 0.30) +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=18, face="bold", family="serif"),
        legend.title = element_text(family = "serif",
                                    size = 20,
                                    color = "#8F2421"),
        legend.text = element_text(family = "serif",
                                   face = "italic",
                                   size = 16,
                                   color = "#DB2C27")) +
  scale_color_manual(labels = c("Setosa", "Versicolor", "Virginica"),
                     values = c("#72DB48", "#5359DB", "#DB27CB")) +
  ggtitle("Modified legend.text and legend.title")

legTextPlt <- ggarrange(def_leg_plt, mod_leg_plt,
                           ncol = 2)
pdf(here::here("Lecture-3", "PlotsLec3", "LegText.pdf"), 
    height = 3.2, width=8)
legTextPlt
dev.off()

# 6. Fixing legend guides
mod_leg_plt2 <- mod_leg_plt +
                ggtitle("Default legend guide")
mod_leg_guide <- mod_leg_plt +
                 guides(color = guide_legend(override.aes = list(alpha = 1,
                                                                 size = 3))) +
                 ggtitle("Enhanced legend guide")
legGuidePlt <- ggarrange(mod_leg_plt2, mod_leg_guide,
                        ncol = 2)
pdf(here::here("Lecture-3", "PlotsLec3", "LegGuide.pdf"), 
    height = 3.2, width=8)
legGuidePlt
dev.off()

# 7. Different legend positions
legpos_left <- mod_leg_guide +
               theme(legend.position = "left") +
               ggtitle('legend.position = "left"')

legpos_right <- mod_leg_guide +
  theme(legend.position = "right") +
  ggtitle('legend.position = "right"')

legpos_top <- mod_leg_guide +
  theme(legend.position = "top") +
  ggtitle('legend.position = "top"')

legpos_bottom <- mod_leg_guide +
  theme(legend.position = "bottom") +
  ggtitle('legend.position = "bottom"')

legPosPlt <- ggarrange(legpos_top, legpos_right,
                       legpos_left, legpos_bottom,
                        ncol = 2, nrow=2)
pdf(here::here("Lecture-3", "PlotsLec3", "LegPos.pdf"), 
    height = 7, width=10)
legPosPlt
dev.off()

# 7. Put legend inside the plot panel area
leg_inside <- mod_leg_guide + theme_bw() +
  theme(legend.position = c(0.9, 0.2),
        legend.margin = margin(0,0,0,0,  "mm")) +
  ggtitle("Legend inside panel area")

pdf(here::here("Lecture-3", "PlotsLec3", "LegPosInside.pdf"), 
    height = 4, width=6)
leg_inside
dev.off()

# 8. Panel grid lines
pan_grid1 <- iris %>%
             ggplot(aes(Petal.Length, Petal.Width)) +
             theme_bw() +
             theme(axis.title = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank()) +
             ggtitle("Default gridlines")

pan_grid2 <- pan_grid1 +
             theme(panel.grid = element_line(color = "blue",
                                             linetype = "dashed",
                                             size = 1)) +
             ggtitle("Blue dashed grid lines")

GrdLinePlt <- ggarrange(pan_grid1, pan_grid2,
                         ncol = 2)
pdf(here::here("Lecture-3", "PlotsLec3", "GridLinePlt.pdf"), 
    height = 3.2, width=8)
GrdLinePlt
dev.off()

# 9. Axis ticks
axis_ticks1 <- pan_grid1 +
               theme(axis.ticks = element_line(color = "red")) +
               ggtitle("Default size of axis ticks")

axis_ticks2 <- pan_grid1 +
  theme(axis.ticks = element_line(color = "red"),
        axis.ticks.length = unit(1, "cm")) +
  ggtitle('Increased length of unit(1, "cm") for axis ticks')

AxisTicksPlt <- ggarrange(axis_ticks1, axis_ticks2,
                          ncol = 2)
pdf(here::here("Lecture-3", "PlotsLec3", "AxisTicksPlt.pdf"), 
    height = 3.2, width=8.5)
AxisTicksPlt
dev.off()

# 10 Create customised theme
curtin_theme <- theme(plot.background = element_rect(color = "#E09B34"),
                      panel.background = element_rect(fill = "#E0BE34"),
                      panel.grid.major = element_line(color = "gray50"),
                      panel.grid.minor = element_blank(),
                      axis.text = element_text(face = "bold",
                                               family = "serif"),
                      axis.title = element_text(face = "bold",
                                                family = "serif"))

curtinTheme <- iris %>% ggplot(aes(Petal.Length,Petal.Width)) +
  geom_point(position = position_jitter(seed = 123), color = "white",
             size = 1.3) +
  geom_smooth(method = "lm", se=FALSE, color = "black") +
  curtin_theme +
  ggtitle("Customised theme inspired by Curtin logo")

pdf(here::here("Lecture-3", "PlotsLec3", "CurtinThemePlt.pdf"), 
    height = 3.2, width=6)
curtinTheme
dev.off()

# 11. In-built themes
data(mtcars)
data(mpg)

mtcars_plt <- mtcars %>%
             ggplot(aes(disp, mpg, color = factor(cyl))) +
             geom_point() +
             labs(color = "Cylinder",
                  x = "Engine displacement (cu.in)",
                  y = "Miles per gallon",
                  title = "Default: theme_grey()") +
             theme(legend.position = c(0.9, 0.8),
                   legend.margin = margin(0,0,0,0, "mm"))
mtcars_bw <- mtcars_plt +
             theme_bw() +
  theme(legend.position = c(0.9, 0.8),
        legend.margin = margin(0,0,0,0, "mm")) +
  ggtitle("theme_bw()")

mtcars_classic <- mtcars_plt +
  theme_classic() +
  theme(legend.position = c(0.9, 0.8),
        legend.margin = margin(0,0,0,0, "mm")) +
  ggtitle("theme_classic()")

mtcars_dark <- mtcars_plt +
  theme_dark() +
  theme(legend.position = c(0.9, 0.8)) +
  ggtitle("theme_dark()")

InBuiltTheme <- ggarrange(mtcars_plt,
                          mtcars_bw,
                          mtcars_classic,
                          mtcars_dark, nrow = 2, ncol=2)
pdf(here::here("Lecture-3", "PlotsLec3", "InBuiltTheme.pdf"), 
    height = 6, width=8)
InBuiltTheme
dev.off()

# 12. Use ggthemes
mtcars_538 <- mtcars_plt +
  theme_fivethirtyeight() +
  theme(legend.position = c(0.65, 0.9)) +
  theme(axis.title = element_text()) +
  ggtitle("theme_fivethirtyeight()")

mtcars_wsj <- mtcars_plt +
  theme_wsj() +
  theme(legend.position = c(0.65, 0.9)) +
  theme(axis.title = element_text(size=12),
        plot.title = element_text(size=20),
        legend.title = element_text(size=10)) +
  ggtitle("theme_wsj()")

mtcars_solariszd <- mtcars_plt +
  theme_solarized() +
  theme(legend.position = c(0.90, 0.80)) +
  theme(axis.title = element_text()) +
  ggtitle("theme_solarized()")

mtcars_economist <- mtcars_plt +
  theme_economist() +
  theme(legend.position = c(0.90, 0.80)) +
  theme(axis.title = element_text()) +
  ggtitle("theme_economist()")

InBuiltTheme2 <- ggarrange(mtcars_538,
                           mtcars_wsj,
                          mtcars_solariszd,
                          mtcars_economist, nrow = 2, ncol=2)
pdf(here::here("Lecture-3", "PlotsLec3", "InBuiltTheme2.pdf"), 
    height = 6, width=8)
InBuiltTheme2
dev.off()


# 13 scale_x_continuous() example
data(diamonds)

diam_plt <- diamonds %>%
            ggplot(aes(carat, price)) +
            geom_point(size = 0.1, alpha=0.10, color = "#D55E00") +
            scale_x_continuous(
              "log10(carat)",
              trans = "log10") +
            scale_y_continuous(
              breaks = c(0, 5000, 10000, 15000),
              labels = c("0$", "5,000$", "10,000$", "15,000$")
            ) +
            theme_wsj()

diam_plt0 <- diamonds %>%
  ggplot(aes(carat, price)) +
  geom_point(size = 0.1, alpha=0.10, color = "#D55E00") +
  scale_x_continuous(
    "log10(carat)",
    trans = "log10") +
    theme_wsj()
  
diam_plts <- ggarrange(diam_plt0,
                       diam_plt)

pdf(here::here("Lecture-3", "PlotsLec3", "ScalePlt.pdf"), 
    height = 3, width=6)
diam_plts
dev.off()


# 14. scale_color_brewer
mtcars_seq_brew <- ggplot(mtcars, aes(disp, mpg, color = factor(cyl))) +
  geom_point(position = position_jitter(seed = 123), size=2, shape=19) +
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  scale_color_brewer("Cylinder", type = "seq", palette = "YlOrRd") +
  ggtitle("Sequential color palette = 'YlOrRd'")

mtcars_qual_brew <- ggplot(mtcars, aes(disp, mpg, color = factor(cyl))) +
  geom_point(position = position_jitter(seed = 123), size=2, shape=19) +
  theme_classic() +
  theme(text = element_text(family = "serif")) +
  scale_color_brewer("Cylinder", type = "qual", palette = "Accent") +
  ggtitle("Qualitative color palette = 'Accent'")

color_brewer_plts <- ggarrange(mtcars_seq_brew,
                               mtcars_qual_brew)

pdf(here::here("Lecture-3", "PlotsLec3", "ColorBrewer.pdf"), 
    height = 3, width=6)
color_brewer_plts
dev.off()

# 15. scale_color_manual() example
mtcars_base <- ggplot(mtcars, aes(disp, mpg, color = factor(cyl))) +
  geom_point(position = position_jitter(seed = 123), size=2, shape=19) +
  theme_classic() +
  theme(text = element_text(family = "serif"))

col_vec <- c("4" = "blue", "6" = "green", "8" = "red")
mtcars_color_manual <- mtcars_base +
                       scale_color_manual("Cylinders",
                                          values = col_vec)

pdf(here::here("Lecture-3", "PlotsLec3", "ColorManual.pdf"), 
    height = 3, width=3.5)
mtcars_color_manual
dev.off()                       

