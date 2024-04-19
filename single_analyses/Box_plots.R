## Making Box Plots

# Setting up and reading in data

# load packages needed
library(ggplot2)
library(dplyr)
library(Rmisc)
library(scales)

# read in dataset using full path to file

egg_counts_main <- read.csv("~/Desktop/Github/Unckless_Lab_Resources/Mentoring_Resources/R_scripts_Learning_R_and_Stats/innubila_egg_counts_main_sheet.csv")
# look at it 
head(egg_counts_main)


# Make a simple box plot of the average egg count separated by the two ages of the groups of flies, either "2" or "3"

age <- ggplot(egg_counts_main, aes(x=fly_age, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
age


# Make a simple box plot of the average egg count separated out by whether the plate had yeast or not

yeast <- ggplot(egg_counts_main, aes(x=yeast, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
yeast

# Make a simple box plot of the average egg count separated out by plate type 

plate <- ggplot(egg_counts_main, aes(x=plate_type_ab, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
plate

# Separating plots out by age

# First, subset the dataframe by the ages 

egg_counts_two <- egg_counts_main[1:24,]
egg_counts_three <- egg_counts_main[25:48,]

# Then use these separated dataframes to make the plots 

# Effect of yeast on age 2 flies 
yeast_2 <- ggplot(egg_counts_two, aes(x=yeast, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
yeast_2 

# Effect of yeast on age 3 flies 
yeast_3 <- ggplot(egg_counts_three, aes(x=yeast, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
yeast_3

# No real difference by age, the pattern is the same 

# Effect of plate type on age 2 flies
plate_2 <- ggplot(egg_counts_two, aes(x=plate_type_ab, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
plate_2

# Effect of plate type on age 3 flies
plate_3 <- ggplot(egg_counts_three, aes(x=plate_type_ab, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
plate_3

# Looks like there is an effect of age on which plate type is better, apple juice plate becomes better in age 3 flies 


# Separate out the plate types to look at yeast in just the plates that produce the most eggs 
# Also going to keep it separate by age 

# Separate egg_counts_two by plate type 
# Do this by twice telling R that you want to keep rows where the thing in the plate_type_ab column is not either MNS or MS 
# Thus keeping only rows where the plate_type_ab is AJS or MAS 
egg_counts_two_good_plates <- egg_counts_two[egg_counts_two$plate_type_ab != "MNS",]
egg_counts_two_good_plates <- egg_counts_two[egg_counts_two$plate_type_ab != "MS",]

# Do the same for the egg_counts_three data 
egg_counts_three_good_plates <- egg_counts_three[egg_counts_three$plate_type_ab != "MNS",]
egg_counts_three_good_plates <- egg_counts_three[egg_counts_three$plate_type_ab != "MS",]

# Now look at effect of yeast on just plates AJS and MAS for age 2 flies 

yeast_2_good <- ggplot(egg_counts_two_good_plates, aes(x=yeast, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
yeast_2_good

# Effect of yeast on age 3 flies 
yeast_3_good <- ggplot(egg_counts_three_good_plates, aes(x=yeast, y=average_egg_count)) + 
  geom_boxplot()
# plot the figure
yeast_3_good



# What if we wanted to look at both yeast and plate type? 
# This can be done with a grouped box plot 
# For this, we will still keep the ages separate 

# Grouped box plot for age 2 flies 

age_2_grouped <- ggplot(egg_counts_two, aes(x=plate_type_ab, y=average_egg_count, fill = yeast)) + 
  geom_boxplot()

age_2_grouped 

# Grouped box plot for age 3 flies 

age_3_grouped <- ggplot(egg_counts_three, aes(x=plate_type_ab, y=average_egg_count, fill = yeast)) + 
  geom_boxplot()

age_3_grouped 


# We can also look at plate type and age, without separating the yeast type 

plate_and_age <- ggplot(egg_counts_main, aes(x=plate_type_ab, y=average_egg_count, fill = fly_age)) + 
  geom_boxplot()

plate_and_age 



# Making the plots prettier and clearer 
# Redo the age graph with clearer labels 

# Order the boxes by creating "levels" so that the boxes appear in order on the graph 
egg_counts_main$fly_age <- factor(egg_counts_main$fly_age , levels=c("two", "three"))

# Use ylab for the y axis label and xlab for the x axis label 
# geom_jitter() adds the individual data points to your graph 
# Theme_minimal() makes the background of the plot minimal 
# And the theme(legend.position = "none") removes the legend from the graph 

# change the order of the boxes so age "two" comes first 
egg_counts_main$fly_age <- factor(egg_counts_main$fly_age , levels=c("two", "three"))

# compute summary statistics 
stats_egg <- summarySE(egg_counts_main, measurevar="average_egg_count", groupvars=c("fly_age"))
stats_egg


x_title <- expression(paste(" Age of", italic(" D. innubila")))

fig_title <- expression(paste("Comparing ammount of eggs laid by ", italic("D. innubila"), " at different ages"))

# add in SE error bars 
# change it to points not boxes
# change font sizes 
# change colors to be color blind friendly 
ggplot(egg_counts_main, aes(x=fly_age, y=average_egg_count, color = fly_age)) + 
  theme_light() + geom_jitter(position = position_jitter(0.1), size =3) + 
  theme(legend.position = "none") + scale_color_manual(values=c( "#C994C7", "#CE1256")) + 
  geom_errorbar( aes(ymin = average_egg_count-se, ymax = average_egg_count+se), data = stats_egg, width = 0.5) +
  theme(axis.text=element_text(size=14, face="bold"), axis.title.x = element_text(size=18), axis.title.y = element_text(size=18)) +
  scale_x_discrete(labels= c("two" = "4 to 7 days\n post emergence", "three" = "8 to 11 days\n post emergence")) +
  labs(title = fig_title ,y = "Average eggs laid in 24 hours", x = x_title)



                                                            
# plot the figure
age
# save this figure 
ggsave("average_egg_counts_by_age_error_point.png")

# Redo the plate type and yeast graph with clearer labels for just age 3 
# Create a value that will be the title of the legend
legend_title <- "Yeast condition"

age_3_grouped <- ggplot(egg_counts_three, aes(x=plate_type_ab, y=average_egg_count, fill = yeast)) + 
  geom_boxplot() + ylab("Average egg counts") + xlab("Plate Type") + theme_minimal() + geom_jitter() + 
  scale_fill_manual(legend_title,values=c("darksalmon","navajowhite"))

age_3_grouped 
# save this figure 
ggsave("average_egg_counts_age_3_grouped.png")

# Redo the plate type and yeast graph with clearer labels for both ages 

All_grouped <- ggplot(egg_counts_main, aes(x=plate_type_ab, y=average_egg_count, fill = yeast)) + 
  geom_boxplot() + ylab("Average egg counts") + xlab("Plate Type") + theme_minimal() + geom_jitter() + 
  scale_fill_manual(legend_title,values=c("burlywood1","sienna"))

All_grouped 
# save this figure 
ggsave("average_egg_counts_all_grouped.png")








