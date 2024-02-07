# Load required R packages
library(here) # For navigating through files and folders
library(tidyverse) # For data wrangling and ggplot2
library(ggpubr)

# Load the iris data
data(iris)
# Assign the data to iris object
iris <- iris
# Check the variable types of the dataset
str(iris)
# Explore the relationship between Sepal.Length and Sepal.Width
# Construct a scatter plot of Sepal.Length against Sepal.Width
scatter1 <- ggplot(data = iris, 
                   mapping = aes(x = Sepal.Width, y = Sepal.Length)) +
            geom_point()

scatter3 <- ggplot(data = iris, 
                   mapping = aes(x = Sepal.Width, y = Sepal.Length)) +
           geom_point() +
           geom_smooth(color = "blue", method="lm")

pdf(here::here("Lecture-2", "PlotsLec2", "Scatter1.pdf"), 
    height = 4)
scatter1
dev.off()

png(here::here("Lecture-2", "PlotsLec2", "Scatter1.png"), 
    height = 360)
scatter1
dev.off()
# Shorter version of the same code
iris %>%
  ggplot(aes(Sepal.Width, Sepal.Length)) +
  geom_point()

scatter2 <- ggplot(data = iris, 
       mapping = aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE) +
  coord_fixed() +
  theme_classic()
png(here::here("Lecture-2", "PlotsLec2", "Scatter2.png"))
scatter2
dev.off()

png(here::here("Lecture-2", "PlotsLec2", "Scatter3.png"))
scatter3
dev.off()
#######################################################################
# Let's see another example                                           #
#######################################################################
# Load in mtcars dataset
data("mtcars")
# Assign the dataset to a variable named mtcars
mtcars <- mtcars
# Inspect the structure of the dataset
str(mtcars)
# mpg vs cyl scatter plot
mpg_vs_cyl <- mtcars %>%
              ggplot(aes(cyl, mpg)) +
              geom_point()
pdf(here::here("Lecture-2", "PlotsLec2", "mpg_vs_cyl.pdf"), 
    height = 4)
mpg_vs_cyl
dev.off()

mtcars_plt1 <- mtcars %>%
              ggplot(aes(fct_reorder(factor(cyl), mpg), mpg)) +
              geom_boxplot(fill = "red", alpha = 0.10,
                           outlier.colour = "blue",
                           outlier.alpha = 1,
                           outlier.size = 3) +
              theme_classic() +
              labs(x = "Cylinder number",
                   y = "Miles/gallon")

png(here::here("Lecture-2", "PlotsLec2", "mtcars_plt1.png"))
mtcars_plt1
dev.off()


mtcars_plt2 <- mtcars %>%
  ggplot(aes(fct_reorder(factor(cyl), mpg), mpg, color = factor(cyl))) +
  geom_boxplot(outlier.size = 2,
               outlier.shape = 3,
               outlier.color = "black") +
  geom_point(color = "black", size=2, shape = 3,
              position = position_jitter(seed = 123, width = 0.1)) +
  theme_classic() +
  labs(x = "Cylinder number",
       y = "Miles/gallon",
       color = "Cylinder")

png(here::here("Lecture-2", "PlotsLec2", "mtcars_plt2.png"))
mtcars_plt2
dev.off()

mtcars_plt3 <- mtcars %>%
  ggplot(aes(x=mpg, fill = factor(cyl))) +
  geom_density(alpha = 0.3) +
  geom_rug(color = "red") +
  theme_classic() +
  labs(y = "Density",
       x = "Miles/gallon",
       fill = "Cylinder")

png(here::here("Lecture-2", "PlotsLec2", "mtcars_plt3.png"),
    height = 280, width=480)
mtcars_plt3
dev.off()

# Change cyl to a character vector
mpg_vs_cyl2 <- mtcars %>%
               ggplot(aes(as.character(cyl), mpg)) +
               geom_point() +
               theme(axis.title = element_text(size=20),
               axis.text = element_text(size=12, face="bold"))
pdf(here::here("Lecture-2", "PlotsLec2", "mpg_vs_cyl2.pdf"), 
    height = 4)
mpg_vs_cyl2
dev.off()

# Change cyl to a factor vector
mpg_vs_cyl3 <- mtcars %>%
               ggplot(aes(factor(cyl), mpg)) +
               geom_point() +
               theme(axis.title = element_text(size=24),
                     axis.text = element_text(size=12, face="bold"))
pdf(here::here("Lecture-2", "PlotsLec2", "mpg_vs_cyl3.pdf"), 
    height = 4)
mpg_vs_cyl3
dev.off()

# Change the default ordering of cyl levels
mpg_vs_cyl4 <- mtcars %>%
  ggplot(aes(factor(cyl, levels = c(8, 6, 4)), mpg)) +
  geom_point() +
  theme(axis.title = element_text(size=18),
        axis.text.x = element_text(size=22, face="bold"),
        axis.text.y = element_text(size=12, face="bold"))

pdf(here::here("Lecture-2", "PlotsLec2", "mpg_vs_cyl4.pdf"), 
    height = 4)
mpg_vs_cyl4
dev.off()

# Map factor(cyl) onto color aesthetics
cyl_to_color <- mtcars %>%
                ggplot(aes(disp, mpg, color = factor(cyl))) +
                geom_point(size=2) +
              theme(axis.title = element_text(size=18),
                    axis.text.x = element_text(size=12, face="bold"),
                    axis.text.y = element_text(size=12, face="bold"),
                    legend.title = element_text(size = 18))
pdf(here::here("Lecture-2", "PlotsLec2", "cyl_to_color.pdf"), 
    height = 4)
cyl_to_color
dev.off()

# Map disp onto color aesthetics
disp_to_color <- mtcars %>%
  ggplot(aes(factor(cyl), mpg, color = disp)) +
  geom_point(size=5) +
  theme(axis.title = element_text(size=18),
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        legend.title = element_text(size = 18))
pdf(here::here("Lecture-2", "PlotsLec2", "disp_to_color.pdf"), 
    height = 4)
disp_to_color
dev.off()

# Load ggplot2 mpg dataset
data(mpg)
mpg_data <- mpg
help(mpg)
str(mpg)
# Investigate the relationship between hwy (highway miles per gallon)
# as a function of cty (city miles per gallon) with size of the each point
# representing the engine displacement (displ) value
displ_to_size <- mpg_data %>%
                 ggplot(aes(x = cty, y=hwy, size = displ)) +
                 geom_jitter(alpha=0.5) +
                 theme_classic() +
  theme(axis.title = element_text(size=18),
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        legend.title = element_text(size = 18, face="bold"),
        legend.position = "right")

pdf(here::here("Lecture-2", "PlotsLec2", "displ_to_size.pdf"), height = 5)
displ_to_size
dev.off()

# Avoid mapping factors to size aesthetics
str(mpg)
mpg_data <- mpg_data %>%
            mutate_if(is.character, as.factor)
str(mpg_data)

  mpg %>%
  ggplot(aes(x = cty, y=hwy, size = factor(drv))) +
  geom_jitter()

# Map factor(drv) onto shape
  drv_to_shape  <- mpg %>%
                ggplot(aes(x = cty, y=hwy, shape = factor(drv))) +
                 geom_jitter(size=3) +
                 theme_classic() +
  theme(axis.title = element_text(size=18),
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        legend.title = element_text(size = 18, face="bold")) +
  guides(shape = guide_legend(override.aes = list(size = 5)))
  
  pdf(here::here("Lecture-2", "PlotsLec2", "drv_to_shape.pdf"), height = 5)
  drv_to_shape
  dev.off()
  
# Faceting
facet1  <- mpg %>%
    ggplot(aes(x = cty, y=hwy)) +
    geom_jitter(size=3) +
    theme_classic() +
    facet_wrap(~ factor(drv)) +
    theme(strip.text.x = element_text(size=22),
          axis.title = element_text(size=18),
          axis.text.x = element_text(size=12, face="bold"),
          axis.text.y = element_text(size=12, face="bold"))
pdf(here::here("Lecture-2", "PlotsLec2", "facet1.pdf"), height = 4)
facet1
dev.off()
##########################################
# hwy vs cty vs drv vs class             #
###########################################
str(mpg_data) 
table(mpg_data$class, mpg_data$drv)
mpg_suv_compact <- mpg %>%
                   filter(class %in% c("suv", "compact"))
mpg_suv_compact <- mpg_suv_compact %>%
                   mutate_if(is.character, as.factor)

facet_grid_example <- mpg_suv_compact %>% ggplot(aes(x = cty, y=hwy)) +
  geom_jitter(alpha=0.3) +
  theme_bw() +
  facet_grid(class ~ drv) +
  theme(strip.text = element_text(size=15),
        axis.title = element_text(size=18),
        axis.text.x = element_text(size=12, face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        panel.grid.minor = element_blank())
pdf(here::here("Lecture-2", "PlotsLec2", "facet_grid_example.pdf"), 
    height = 5)
facet_grid_example
dev.off()

facet_grid_example2 <- mpg_suv_compact %>% ggplot(aes(x = cty, y=hwy)) +
  geom_point(position = position_jitter(seed=123), alpha=0.3,
             color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +
  theme_classic() +
  facet_grid(class ~ drv) 

## Color vs Fill aesthetics: boxplot
head(iris, n=1)
color_aes_bxplt <- iris %>% ggplot(aes(x = Species, y=Petal.Length, color=Species)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "color = Species")

fill_aes_bxplt <- iris %>% ggplot(aes(x = Species, y=Petal.Length, fill=Species)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "fill = Species")
  
bxplt_ex1 <- ggarrange(color_aes_bxplt,
                       fill_aes_bxplt, nrow = 1)

pdf(here::here("Lecture-2", "PlotsLec2", "bxplt_ex1.pdf"), 
    height = 4, width=8)
bxplt_ex1
dev.off()

## Color vs Fill aesthetics: density
color_aes_denplt <- iris %>% ggplot(aes(x = Petal.Length, color=Species)) +
  geom_density() +
  theme_classic() +
  labs(title = "color = Species")

fill_aes_denplt <- iris %>% ggplot(aes(x = Petal.Length, fill=Species)) +
  geom_density(alpha=0.5) +
  theme_classic() +
  labs(title = "fill = Species")

denplt_ex1 <- ggarrange(color_aes_denplt,
                       fill_aes_denplt, nrow = 1)

pdf(here::here("Lecture-2", "PlotsLec2", "denplt_ex1.pdf"), 
    height = 4, width=8)
denplt_ex1
dev.off()

# color vs fill for geom_point()
base_scatter <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) +
                theme_classic() + coord_fixed() + theme(legend.position = "right")
color_scat <- base_scatter + geom_point(aes(color = Species), shape = 21, size=2) +
               ggtitle("color = Species")
fill_scat <- base_scatter + geom_point(aes(fill = Species), shape = 21, size=2) +
  ggtitle("fill = Species")

scatplt_ex1 <- ggarrange(color_scat,
                         fill_scat, nrow = 1)

pdf(here::here("Lecture-2", "PlotsLec2", "scatplt_ex1.pdf"), 
    height = 4, width=8)
scatplt_ex1
dev.off()
#######################################
# Checking alpha transparency         #
#######################################
data(diamonds)
str(diamonds)
scat_opaque <- diamonds %>% ggplot(aes(carat, price)) +
               geom_point(alpha = 1) +
               theme_classic() +
               ggtitle("alpha = 1")
scat_trans <-  diamonds %>% ggplot(aes(carat, price)) +
               geom_point(alpha = 0.10) +
               theme_classic() +
               ggtitle("alpha = 0.10")               
diam_alpha <- ggarrange(scat_opaque, scat_trans, nrow = 1)

pdf(here::here("Lecture-2", "PlotsLec2", "diam_alpha.pdf"), 
    height = 4, width=8)
diam_alpha
dev.off()

pdf(here::here("Lecture-2", "PlotsLec2", "diamonds.pdf"), 
    height = 4, width=6)
diamonds %>% ggplot(aes(carat, price)) +
  geom_point(alpha = 0.10, size=0.25, shape=1, color = "blue") +
  theme_classic() +
  ggtitle("alpha = 0.10, size = 0.25, shape = 1, color = 'blue'")               
dev.off()

pdf(here::here("Lecture-2", "PlotsLec2", "geompointsmooth.pdf"), 
    height = 4, width=6)
diamonds %>% ggplot(aes(carat, price)) +
  geom_point(alpha = 0.10, size=0.25, shape=1, color = "blue") +
  geom_smooth(color = "red") +
  theme_classic() +
  xlim(c(0, 2.0)) +
  ggtitle("Diamond price as a function of carat")
dev.off()


png(here::here("Lecture-2", "PlotsLec2", "geompointsmooth.png"), 
    height = 300, width=480)
diamonds %>% ggplot(aes(carat, price)) +
  geom_point(alpha = 0.10, size=0.25, shape=1, color = "blue") +
  geom_smooth(color = "red") +
  theme_classic() +
  xlim(c(0, 2.0)) +
  ggtitle("Diamond price as a function of carat")
dev.off()
#################################################
# geom_jitter() and geom_line() with same aes() #
#################################################
same_aes <- iris %>%
  ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  ggtitle("Common color aesthetics in two geoms")

######################################################
# geom_jitter() and geom_line() with different aes() #
######################################################
diff_aes <- iris %>%
  ggplot(aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_smooth(method = "lm", se = FALSE, color="black") +
  theme_classic() +
  ggtitle("Different color aesthetics in two geoms")
###################################################
scatter_aes_examples <- ggarrange(same_aes,
                                  diff_aes, nrow = 1)

pdf(here::here("Lecture-2", "PlotsLec2", "scatter_aes_examples.pdf"), 
    height = 4, width=8)
scatter_aes_examples
dev.off()

#########################################################
GeomBoxJitter <- ggplot(iris, aes(Species, Petal.Width)) +
                 geom_boxplot(aes(color = Species)) +
                 geom_jitter(size=0.10, width = 0.1) +
                 theme_classic() +
                 ggtitle("Boxplots: Petal.Width vs Species")
pdf(here::here("Lecture-2", "PlotsLec2", "GeomBoxJitter.pdf"), 
    height = 4, width=6)
GeomBoxJitter
dev.off()


GeomBoxJitter2 <- ggplot(iris, aes(Species, Petal.Width)) +
  geom_boxplot(aes(color = Species), 
               outlier.color = "black",
               outlier.size = 0.20) +
  geom_jitter(size=0.20, width = 0.10) +
  theme_classic() +
  ggtitle("Boxplots: Petal.Width vs Species")

pdf(here::here("Lecture-2", "PlotsLec2", "GeomBoxJitter2.pdf"), 
    height = 4, width=6)
GeomBoxJitter2
dev.off()

###############################################
# Combined geom with faceting                 #
###############################################
hwy_cty_drv0 <- mpg %>% ggplot(aes(x = cty, y=hwy)) +
  geom_point(aes(color=factor(drv, levels = c("4", "r", "f"))),
             position = position_jitter(seed = 123),
             shape = 3) +
  facet_wrap(~ factor(drv, levels = c("4", "r", "f"))) +
  theme_classic() +
  labs(color = "Driving train",
       x = "City miles/gallon",
       y = "Highway miles/gallon")
png(here::here("Lecture-2", "PlotsLec2", "hwy_cty_drv0.png"),
    height = 300, width = 480)
hwy_cty_drv0
dev.off()

hwy_cty_drv <- mpg %>% ggplot(aes(x = cty, y=hwy)) +
        geom_point(aes(color=factor(drv)),
                   position = position_jitter(seed = 123),
                   shape = 1) +
        geom_smooth(method = "lm", se = FALSE, color="black",
                    linetype = "dashed") +
        facet_wrap(~ factor(drv)) +
        theme_classic() +
        labs(color = "Driving train",
             x = "City miles/gallon",
             y = "Highway miles/gallon")
pdf(here::here("Lecture-2", "PlotsLec2", "hwy_cty_drv.pdf"), 
    height = 4, width=6)
hwy_cty_drv
dev.off()

png(here::here("Lecture-2", "PlotsLec2", "hwy_cty_drv.png"),
    height = 300, width = 480)
hwy_cty_drv
dev.off()

png(here::here("Lecture-2", "PlotsLec2", "facet_grid_example2.png"),
    height = 380, width = 480)
facet_grid_example2
dev.off()

