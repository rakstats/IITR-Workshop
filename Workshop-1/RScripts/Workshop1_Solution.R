# Data Visualisation Workshop-1 Solution
# Load required R libraries
library(dslabs) # for murders dataset
library(tidyverse) # for ggplot2 and other data wrangling packages
library(waffle) # to create waffle chart
library(RColorBrewer) # for selecting attractive color palette
library(ggthemes) # for attractive visual themes
library(ggridges) # for ridge density plot
library(viridis) # for some interesting colour scheme


# Load the murders data
data("murders")
# Assign murders data to a variable called murders
murders <- murders
# Get a glimpse of the data
dplyr::glimpse(murders)
# Get the summary of the data
summary(murders)

### Q1. Create a **Pie chart** showing the regional murder percentages
# group by region and compute the total number of cases for each region
murders_regional_summary <- murders %>%
  dplyr::group_by(region) %>%
  summarise(total = sum(total), .groups = "drop") %>%
  mutate(percentage = round((total/(sum(total))) * 100))

# print murders_regional_summary
murders_regional_summary

# Check that sum of all percentages is 100
sum(murders_regional_summary$percentage)

################################################################################
# Create your first pie chart. Note that we are using `geom_col()` in the      #
# reference to the polar coordinates to create the pie chart.                  #
# The function `geom_col()` is used for creating stacked-bar plots,            #
# so the pie-chart can be thought of a transformed stacked-bar chart,          #
# transformed using the `coord_polar()` function.                              #
################################################################################

# Create the barebone Pie chart using murders_regional_summary
murders_regional_summary %>% 
  ggplot(aes(x="", y=percentage, fill=region)) +
  geom_col() +
  coord_polar("y", start=0, direction=-1)


################################################################################
# Let's polish our Pie chart a bit and create a more presentable visualisation.#
# Let's change the theme first.                                                #
################################################################################

# Add theme_void()
murders_regional_summary %>% 
  ggplot(aes(x="", y=percentage, fill=region)) +
  geom_col() +
  coord_polar("y", start=0, direction=-1) +
  theme_void()

################################################################################
# Now we have a much cleaner visualisation, but we're missing key data in our  #
# visualisation. We are not sure what percentage does each pie represent.      #
# Let's add some annotations inside the color wheel to show the corresponding  #
# percentage values.                                                           #
################################################################################

# Add geom_text() --- arguments are given
murders_regional_summary %>% 
  ggplot(aes(x="", y=percentage, fill=region)) +
  geom_col() +
  coord_polar("y", start=0, direction=-1) +
  theme_void() +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size=10) 

################################################################################
# Produce the final Pie chart                                                  #
# 1. Order the regions in the decreasing order of percentages                  #
# Change the fill mapping from region to fct_reorder(region, -percentage)      #
# 2. Create a proper legend title: "Region"                                    #
# Use labs(fill = "Region")                                                    #  
# 3. Choose an appropriate plot title                                          #
# ggtitle("Regional breakdown of 2010 gun murders")                            #
# 4. Choose a better color palette for Regions                                 #
# Use scale_fill_brewer(palette = "Accent")                                    #
# 5. The theme() elements will discussed in the next two lectures              #
################################################################################
murders_regional_summary %>% 
  ggplot(aes(x="", y=percentage, fill=fct_reorder(region, -percentage))) +
  geom_col() +
  coord_polar("y", start=0, direction=-1) +
  theme_void() +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5), size=10) +
  labs(fill = "Region") +
  ggtitle("Regional breakdown of 2010 gun murders") +
  scale_fill_brewer(palette = "Accent") +
  theme(legend.title = element_text(size=18, hjust = 0.5),
        legend.text = element_text(size=15),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5, size=15, face = "bold"))


################################################################################
### Q2. Create a **Waffle chart** showing the regional murder percentages      #
################################################################################

# Sort percentage values in the murders_regional_summary 
murders_regional_summary2 <- murders_regional_summary %>% 
                             arrange(desc(percentage))

# We need the vector of percentage values to create the waffle chart
# Assign the vector to percent_by_region
percent_by_region <- murders_regional_summary2$percentage

# percent_by_region should be a named vector --- assign the regions to names 
names(percent_by_region) <- murders_regional_summary2$region

# Create the waffle chart using waffle() function
# Assign percent_by_region to the first argument
# Assign number of rows
# Assign brewer.pal(n=4, name="Accent") to colors

waffle::waffle(percent_by_region, rows= 5, 
               colors = brewer.pal(n=4, name = "Accent")) +
        theme(legend.title = element_text(size=15, hjust = 0.5),
              legend.text = element_text(size=12),
              plot.title = element_text(hjust = 0.5, size=15, face = "bold")) +
  ggtitle("Waffle chart: Regional breakdown of 2010 gun murders")


################################################################################
### Q3. Create a **Donut chart** showing the regional murder percentages       #

# Add xlim(0.5, 2.5) to the code of Pie Chart                                  #
# Use murders_regional_summary as the data                                     #  
# Put x = 2 and y = percentage into mapping aesthetics                          #
# Choose appropriate fill aesthetics                                           #
# assign color="black"                                                         #
# Choose appropriate values for the arguments of the coord_polar() function    #  
# Choose an appropriate theme                                                  #
# Assign appropriate legend title fill = "Region"                              #
# Assign palette = "Accent" in scale_fill_brewer()                             #
################################################################################

murders_regional_summary %>% 
  ggplot(aes(x=2, y=percentage, fill=fct_reorder(region, -percentage))) +
  geom_col() +
  coord_polar("y", start=0, direction=-1)+
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5),
            size=8) +
  theme_void() +
  labs(fill = "Region") +
  theme(legend.title = element_text(size=15, hjust = 0.5),
        legend.text = element_text(size=12),
        legend.key.size = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5, size=15,
                                  face = "bold")) +
  scale_fill_brewer(palette = "Accent") +
  ggtitle("Donut chart: Regional breakdown of 2010 gun murders") +
  xlim(0.5, 2.5)


# Let's now create a stacked bar chart using the WHO's disease dataset. 
# First we need to read the data into our R session.

# Read in the csv dataset: world_health_org_disease_data.csv
disease_data <- read.csv(here::here("DATA", "world_health_org_disease_data.csv"))

# Summary of the WHO Disease data
summary(disease_data)

# We can see that some of the variables in the `disease_data` are stored as
# `character` vectors --- we should transform these variables into `factor` 
# variables.

# Use is.character and as.factor function in mutate_if() to
# transform all character vectors into factor variables.
disease_data <- disease_data %>%
                mutate_if(is.factor, as.factor)

# Take a glimpse to check if the transformations happened
glimpse(disease_data)

# Summary of the data
summary(disease_data)

# Create disease_data2016 by selecting data corresponding to year = 2016
disease_data2016 <- disease_data %>%
                    dplyr::filter(year == 2016)

# Summary of the disease_data2016
summary(disease_data2016)


# We have six regions. Let's look at the distribution of these diseases in the
# six regions and compare them. 
# Group by region and disease type and compute the total number of cases
disease2016_summary <- disease_data2016 %>%
                       group_by(region, disease) %>%
                       summarise(cases = sum(cases), .groups = "drop")

################################################################################
# Q4. Create a stacked bar plot comparing the disease distributions of six     # 
#     regions.                                                                 #
################################################################################

# Hint: use geom_col() to create a stacked bar plot
# Use    position = "fill" in geom_col()
# Assign correct x, y, and fill aesthetics

disease2016_summary %>% 
  ggplot(aes(x=region, y=cases, fill=disease)) + 
  geom_col(position = "fill")
                               
                               
################################################################################                               
# Let's clean up the plot a bit.                                               #
# Create 'other' category for non-prevalent disease types.                     #
# Assign the result to disease2016_summary2                                    #
# Four disease types should be "mumps", "pertussis", "measles", and "other"    #
################################################################################
disease2016_summary2  <- disease2016_summary %>%
      mutate(disease = if_else(disease %in% c("measles", "pertussis", "mumps"), 
                               disease, "other"))

# Order disease types and regions for better visualisation
disease2016_summary2 <- disease2016_summary2 %>%
                        group_by(region, disease) %>%
                        summarise(cases = sum(cases), .groups = "drop") %>%
                        mutate(disease = factor(disease, 
                                                 levels = c("other","measles", 
                                                            "pertussis", "mumps")),
                                region = factor(region,
                                                levels = c("EUR", "SEAR",
                                                           "AFR", "AMR",
                                                           "WPR", "EMR")))
# Create a publication quality stacked barplot

# Assign correct x, y, and fill aesthetics
# Assign correct geom with appropriate position argument
# Use theme_economist()
disease2016_summary2 %>% ggplot(aes(x=region, y=cases, fill=disease)) +
  geom_col() +
  theme_economist_white() +
  theme(legend.title = element_text(size=20, face="bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=15, face="bold"),
        axis.text.y = element_text(face="bold.italic"),
        axis.title.y = element_text(size=15, face = "bold"),
        legend.text = element_text(size=15),
        legend.key.size = unit(1, 'cm')) +
  scale_fill_manual(values = rev(economist_pal()(4))) +
  labs(fill = "Disease", y = "Proportion of cases") +
  labs(title = "Disease distribution for six regions")


################################################################################
# Bar Plots are great to compare many categories at once.                      #                
# Also, Barplot should be used if the data follow some notion                  #          
# of stacking --- it simply means that you can add more stuff                  #          
# on top of the stack to increase it's height. Dollar value                    #           
# or Number of cases are great examples of scenarios where                     #
# such stacking principle holds true. Ratios or Percentiles                    #            
# should not be displayed using barplots. Any non-linear transformation        #
# as well, e.g., we often take `log10()` or `sqrt()` transformations           #      
# and they should not be displayed as bars.                                    #
################################################################################
                               
################################################################################                              
### Q5. Gun Murder cases in 10 States                                          #
################################################################################
                               
# Get the top-10 states with most number of cases
murders_top10 <- murders %>% slice_max(total, n=10)
                               
# Construct a bare bone barplot
# Assign correct x and y aesthetics
murders_top10 %>% ggplot(aes(x = state, y=total)) +
                  geom_col()
                               
# Now construct an enhanced barplot 
# Assign correct x and y aesthetics
# Use a colour of liking to fill the bars
# Flip the coordinates to make a horizontal bar plot --- use coord_flip()
# Use the minimal theme
murders_top10 %>% ggplot(aes(x = fct_reorder(state, total), y=total)) +
geom_col(fill = "goldenrod") +
coord_flip() +
theme_minimal() +
theme(panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(face = "bold.italic", size=12),
      axis.text.x = element_text(face="bold", size = 12, color="gray50"),
      plot.title = element_text(hjust=0.5, face="bold", color="red4", size=15),
      plot.subtitle =  element_text(hjust=0.5, face="bold.italic", color="red4", size=10)) +
      labs(title = "Top 10 US States in 2010", 
           subtitle = "According to gun related deaths")
                               
                               
# Now let's look at some examples of **Point Chart**
# First obtain the disease data for **WPR** region 
# for the years **2006** and **2016**.
# Get the AFR data for the years 2006 and 2016 

disease_wpr <- disease_data %>%
               dplyr::filter(region == "AFR",
                             year %in% c(2006, 2016))

# You need to recreate country factor
disease_wpr$country <- factor(as.character(disease_wpr$country))

# Create country and year specific total cases
disease_wpr <- disease_wpr %>%
               group_by(country, year) %>%
               summarise(cases = sum(cases), .groups = "drop")

# Create two columns Cases2006 and Cases2016
# You can use pivot_wider() to achieve this
# Use year as name keys and cases as values
disease_wpr2 <- disease_wpr %>%
                pivot_wider(names_from = year,
                            values_from = cases,
                            names_prefix = "Cases")
# Print disease_wpr2 to have a look at the data
disease_wpr2


# Filter out Countries with zero number of cases 
disease_wpr2 <- disease_wpr2 %>%
                filter(Cases2006 > 0, Cases2016 > 0)

################################################################################
### Q6. Plot log10(cases) for 2016 data.                                       #
################################################################################

# Assign correct x, y aesthetic mappings
# Experiment with the colour of the points
# Create a horizontal barplot
# You can transform the y aesthetic to log10 scale using the scale_y_*()
# Use any theme you like --- I have put theme_bw() as a default

disease_wpr2 %>% ggplot(aes(x=fct_reorder(country, Cases2016),
                            y=Cases2016)) +
                 geom_point(color= "blue") +
                 coord_flip() +
                 scale_y_log10(labels = scales::comma) +
                 theme_bw() +
                 theme(axis.title.y = element_blank(),
                       panel.grid.major = element_line(color="white"),
                       panel.grid.minor = element_blank(),
                       axis.text.y = element_text(face = "bold.italic", size=10, color="blueviolet"),
                       axis.text.x = element_text(face = "bold", size=12, color="navy")) +
                 labs(y = "Number of disease cases (in log10 scale)",
                    title = "Ranking of African countries based on most to least number of cases in 2016")


##################################################################################
### Q7. Create a point chart for log2-fold change using the WPR disease data for #
#       the years 2006 and 2016.                                                 #
##################################################################################
# Create a column log2FoldChange using appropriate formula
disease_wpr3 <- disease_wpr2 %>% 
                mutate(log2FoldChange = log2(Cases2016/Cases2006))


# Create the Point Chart showing log2-fold-change values of African countries
# Assign appropriate x and y aesthetics
# Assign Correct geom_*
# Create a horizontal point chart --- use an appropriate coord_* function
# Experiment with themes --- as the default theme_bw() is given
# Add another geom_* layer to include a dashed-line as a visual anchor at 0 log2-fold-change value --- use geom_hline()


disease_wpr3 %>% ggplot(aes(x=fct_reorder(country, log2FoldChange),
                            y=log2FoldChange)) +
  geom_point(color = "blue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  theme_bw() +
  geom_hline(yintercept=0, linetype= "dashed", size=0.8) +
  theme(axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(face = "bold.italic", size=12),
        axis.text.x = element_text(face = "bold", size=12)) +
  labs(y = "Log2-fold change: log2(Case2016/Case2006)",
       title = "Ranking of African countries based on log2-fold changes")


# Now let's look at some examples of distributional data. 
# We are going to use the `iris` dataset for this last section.
# Load iris data using data() function
data(iris)
                               
# Assign to a variable called iris
iris <- iris 
# Inspect head of the data
head(iris)
                               
# Get the summary of the data
summary(iris)
                               
# Obtain the data only for the Setosa species 
setosa_df <- iris %>% filter(Species == "setosa")
                               
# Create a Histogram using geom_histogram() of Sepal.Length
setosa_df %>% ggplot(aes(x=Sepal.Length)) +
              geom_histogram()
                               
# Create a Boxplot of sepal length data using geom_boxplot()
# Note geom_jitter() in the code creating the jittering of the points
setosa_df %>% ggplot(aes(x=Species, y=Sepal.Length, fill=Species)) +
              geom_boxplot() +
              scale_fill_viridis(discrete=TRUE, alpha=0.2) +
              geom_jitter(width=0.1, height=0, color="orange") +
              theme_bw() +
                                 theme(
                                   legend.position="none",
                                   plot.title = element_text(size=15),
                                   axis.title.y = element_text(size=15),
                                   axis.text.y = element_text(face="bold.italic", size=12),
                                   axis.title.x = element_blank(),
                                   axis.text.x = element_blank(),
                                   axis.ticks.length.x = unit(0, 'cm'),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   
                                 ) +
                                 ggtitle("Sepal length distribution of Setosa") +
                                 labs(y="Sepal length (cm)")
                               

#######################################################################                               
### Q8. Create Kernel density estimates of the Petal.Length densities #
#       for the three iris species.                                   #
#######################################################################
                               
# Use the correct x and fill aesthetics --- note we do not need y-aesthethics 
# for density plot
# Use geom_density with alpha = 0.3
iris %>% ggplot(aes(x = Petal.Length, fill = Species)) +
         geom_density()
                               
################################################################################                               
### Q8. Create Kernel density estimates of the Petal.Length using three        # 
#       different bandwidth sizes: 0.58, (0.58/2), and (0.58/4)                #
################################################################################
                               
# Construct the density using default bandwidth of 0.58
iris %>% ggplot(aes(x = Petal.Length)) +
         geom_density(fill = "maroon") +
         ggtitle("Bandwidth = 0.58")
                               
# Construct the density using bandwidth 0.58/2 --- use adjust = 0.5
iris %>% ggplot(aes(x = Petal.Length)) +
         geom_density(fill = "maroon", adjust = 0.5) +
         ggtitle("Bandwidth = (0.58)/2")
                               
# Construct the density using bandwidth 0.58/4 --- use adjust = 0.25
iris %>% ggplot(aes(x = Petal.Length)) +
         geom_density(fill = "maroon", adjust = 0.25) +
         ggtitle("Bandwidth = (0.58)/4")
                               

################################################################################                               
### Q9. Create Kernel density estimates of the Petal.Length for three species  # 
#       in the same plot.                                                      #
################################################################################
                               
# Create species labels to use as annotations withing the plot
species_label <- data.frame(x=c(2.0, 4.5, 6.0), 
                            y=c(2.5, 1.1, 0.8), 
                            Species = c("Setosa", "Versicolor", "Virginica"))
                               
# Create the Kernel Density Plot
iris %>% ggplot(aes(x = Petal.Length, fill = Species)) +
         geom_density(alpha = 0.3, color="black") +
         theme_bw() +
         scale_fill_viridis(discrete = TRUE) +
         theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               legend.position = "none") +
    geom_text(data = species_label, aes(x=x, y=y, label=Species), size = 4.5)
                               
                               
################################################################################                               
### Q10. Create Boxplots of Petal.Widths comparing three species.              #
################################################################################
                               
# Assign correct x, y, and fill aesthetics
# Use appropriate geom_* 
# Add jittered points
iris %>% ggplot(aes(x = Species, y=Petal.Width, fill = Species)) +
 geom_boxplot(color="white", outlier.size = 1) +
 geom_jitter(color="white", size=1, width=0.1, height=0) +
 scale_fill_viridis(discrete = TRUE) +
 theme_dark() +
 theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       legend.position = "none",
       axis.title.x = element_blank(),
       axis.text.x = element_text(face="bold", size=15),
       axis.title.y = element_text(face = "bold", size=15),
       axis.text.y = element_text(face="bold.italic", size=12,
                                  color = "gray50")) +
 ggtitle("Petal Width distributions of Iris species")


#################################################################################
# Q11. Create **ridgeline** density plots for three iris species based on their #
#################################################################################
# Sepal.Length densities.
# Assign correct x, y, and fill aesthetics.
# Use geom_density_ridges() to create the ridgeline plot

iris %>% ggplot(aes(y= Species, x = Sepal.Length, fill = Species)) +
 geom_density_ridges() +
 scale_fill_viridis(discrete = TRUE) +
 theme_ridges() +
 labs(x = "Petal width (cm)") +
 theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       axis.title.y = element_blank(),
       axis.title.x = element_text(color = "gray50"),
       axis.text.y = element_text(size=15, face = "bold.italic", color="gray50"),
       legend.position = "none")
                               
                               
                               
                               
                               
