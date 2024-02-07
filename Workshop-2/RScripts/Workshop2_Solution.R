# Ans-1 
library(tidyverse)

# Ans-2
irisSepalLenVsWid <- iris %>%
                     ggplot(aes(Sepal.Width,
                                Sepal.Length)) +
                     geom_point()
irisSepalLenVsWid

# Ans-3
irisSepalLenVsWid2 <- irisSepalLenVsWid +
                      geom_smooth(method = "lm")
irisSepalLenVsWid2

# Ans-4
irisSepalLenVsWid3 <- iris %>%
                      ggplot(aes(Sepal.Width,
                                 Sepal.Length,
                                 color = Species)) +
                      geom_point() +
                      geom_smooth(method = "lm", se = FALSE) +
                      coord_fixed() +
                      theme_classic()
irisSepalLenVsWid3

# Ans-5
# Yes, it's an example of Simpson's paradox, as the relationship between 
# Sepal.Length and Sepal.Width changes direction once the 
# Species variable is accounted for in the model.

# Ans-6 
mtcarsBoxplt <- mtcars %>%
                mutate(cyl_fac = factor(cyl, 
                                        levels = c(8, 6, 4))) %>%
                ggplot(aes(cyl_fac, mpg)) +
                geom_boxplot(fill = "red", alpha = 0.20,
                             outlier.color = "blue",
                             outlier.size = 2,
                             outlier.alpha = 1) +
                theme_classic() +
                labs(x = "Cylinder number",
                     y = "Miles/gallon")

# Ans-7
mtcarsBoxplt2 <- mtcars %>%
                 mutate(cyl_fac = factor(cyl, 
                        levels = c(8, 6, 4))) %>%
                ggplot(aes(cyl_fac, mpg, color = factor(cyl))) +
                geom_boxplot(outlier.shape = 3,
                             outlier.color = "black") +
                geom_point(position = position_jitter(seed = 123,
                                                      width = 0.1),
                           shape = 3,
                           color = "black") +
                theme_classic() +
                labs(x = "Cylinder number",
                     y = "Miles/Gallon",
                     color = "Cylinder")

# Ans-8
mtcarsDensity <- mtcars %>%
                 mutate(cyl_fac = factor(cyl)) %>%
                 ggplot(aes(x = mpg, fill = cyl_fac)) +
                 geom_density(alpha = 0.25) +
                 geom_rug(color = "red") +
                 theme_classic() +
                 labs(x = "Miles/gallon",
                      y = "Density",
                      fill = "Cylinder")

# Ans-9
data(mpg)
mpg_data <- mpg
names(mpg_data)
glimpse(mpg_data)

# Ans-10
hwyVsCty <- mpg_data %>%
            mutate(drv_fac = factor(drv, levels = c("4", "r", "f")),
                   class_fac = as.factor(class)) %>%
            ggplot(aes(cty, hwy, color = drv_fac)) +
            geom_point(position = position_jitter(seed=123),
                       shape = 3) +
            facet_wrap(~ drv_fac) +
            theme_classic() +
  labs(x = "City miles/gallon",
       y = "Highway miles/gallon",
       color = "Driving train")

# Ans-11
hwyVsCty2 <- mpg_data %>%
             mutate(drv_fac = as.factor(drv),
             class_fac = as.factor(class)) %>%
             ggplot(aes(cty, hwy)) +
             geom_point(aes(color = drv_fac),
                        position = position_jitter(seed=123),
                        shape = 1) +
             geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                         color = "black") +
             facet_wrap(~ drv_fac) +
             theme_classic() +
             labs(x = "City miles/gallon",
                  y = "Highway miles/gallon",
                  color = "Driving train")
# Ans-12
data("diamonds")

# Ans-13
priceVsCarat <- diamonds %>% ggplot(aes(carat, price)) +
  geom_point(alpha = 0.10, size=0.25, shape=1, color = "blue") +
  geom_smooth(color = "red") +
  theme_classic() +
  xlim(c(0, 2.0)) +
  ggtitle("Diamond price as a function of carat")



# Ans-14
mpgLmodelPlt <- mpg %>%
                filter(class %in% c("compact", "suv")) %>%
                mutate(class_fac = as.factor(class),
                       drv_fac = as.factor(drv)) %>%
                ggplot(aes(x = cty, y=hwy)) +
                geom_point(position = position_jitter(seed=123),
                           color = "orange",
                           shape = 1) +
               geom_smooth(method = "lm", se  = FALSE, size=0.5) +
               facet_grid(class_fac ~ drv_fac) +
               theme_classic() +
               labs(x = "City miles/gallon",
                    y = "Highway miles/gallon")









