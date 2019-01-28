

#==========================================#
# ==== make graph of college readiness ====
#==========================================#

#========================#
# ==== load packages ====
#========================#
library(data.table)
library(ggplot2)

rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")
options(scipen = 999)
cat("\f")

# set attributes for plot to default ea theme 
plot_attributes <- theme( plot.background = element_rect(fill = "lightgrey"),
                          panel.grid.major.x = element_line(color = "gray90"), 
                          panel.grid.minor  = element_blank(),
                          panel.background = element_rect(fill = "white", colour = "black") , 
                          panel.grid.major.y = element_line(color = "gray90"),
                          text = element_text(size= 50),
                          plot.margin=unit(c(1.25,1.25,2,1.25),"cm"),
                          axis.title.y = element_text( vjust = 8),
                          axis.title.x = element_text( vjust = -8),
                          plot.title = element_text(vjust= 4, hjust = 0.5, colour = "#0B6357",face = "bold", size = 50))


#====================#
# ==== load data ====
#====================#


# list files to load in
file_list <- list.files("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/Reasearch_milestone_1/mischooldata/college_readiness")

# load in files 
file_name <- file_list[[1]]

read_function <- function(file_name){
  
  dt <- fread(paste0("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/Reasearch_milestone_1/mischooldata/college_readiness/", file_name ))

  # put in year 
  year <- strsplit(file_name, "_")[[1]][[3]]
  year <- gsub(".csv", "", year)
  
  dt[, year := year]
  
  return(dt)
  }

# laod data 
dt_list <- lapply(file_list, read_function )


# stack data 
dt1 <- rbindlist(dt_list)

# subset down to all districts 
dt1 <- dt1[DistrictName == "All Districts"]
dt1 <- dt1[ISDName == "Statewide"]

# checout gourps 
dt1[, .N, Subgroup]

# convert things to numeric 
dt1[, year := as.numeric(year)]
dt1[, MathPercentReady := as.numeric(MathPercentReady) ]
dt1[, AllSubjectPercentReady := as.numeric(AllSubjectPercentReady) ]
dt1[, ReadingPercentReady := as.numeric(ReadingPercentReady) ]
dt1[Subgroup == "Economically Disadvantaged", Subgroup := "Econ Dis"]
dt1[Subgroup == "Not Economically Disadvantaged", Subgroup := "Not Econ Dis"]


# subset to gender for a gender graph 
gender_dt <- dt1[ Subgroup %chin% c("Male", "Female")]

# subset to econ disadvantage 
econ_dis_dt <- dt1[ Subgroup %chin% c("Econ Dis", "Not Econ Dis")]

# get race data 
race_dt <- dt1[Subgroup %chin% c("American Indian or Alaska Native", "Asian or Pacific Islander", "Black, not of Hispanic origin",
                                "Hispanic", "Two or More Races", "White, not of Hispanic origin", "Asian", "Native Hawaiian or Other Pacific Islander" )]

# fix NA data 
dt1[dt1 == "N/A"]<- NA 

#=======================#
# ==== make graphs ====
#=======================#

in_data = gender_dt
in_plot_tag = "Gender"
in_data = econ_dis_dt
in_data = race_dt


plot_funciton <- function(in_data, in_plot_tag, in_w = 1200){
  

# graph all subject percent ready by gender 
plot1 <- ggplot(data = in_data, aes(x = year, y = AllSubjectPercentReady, group=Subgroup)) + geom_line(size = 4, aes(color = Subgroup)) + geom_point(aes(color = Subgroup), size = 6)
plot1 <- plot1 + geom_vline(xintercept = 2016) + annotate("text", x = 2014.5, y = 70, label = "Switch to SAT", size = 12)
plot1 <- plot1 + scale_x_continuous( breaks= seq(from = 2007, to = 2017, by =1)) + xlab("Year") +ylab("All Subject Percent Ready")
plot1 <- plot1 + ggtitle(paste0("College Readiness by ", in_plot_tag)) + scale_y_continuous(breaks = seq(from = 0, to = 70, by = 5), limits = c(0,70)) +  theme(axis.text.x = element_text(angle = -70, hjust = 1))
plot1
plot1 <- plot1 + plot_attributes


plot2 <- ggplot(data = in_data, aes(x = year, y = MathPercentReady, group=Subgroup)) + geom_line(size = 4, aes(color = Subgroup)) + geom_point(aes(color = Subgroup), size = 6)
plot2 <- plot2 + geom_vline(xintercept = 2016) + annotate("text", x = 2014.5, y = 70, label = "Switch to SAT", size = 12)
plot2 <- plot2 + scale_x_continuous( breaks= seq(from = 2007, to = 2017, by =1)) + xlab("Year") +ylab("Math Percent Ready")
plot2 <- plot2 + ggtitle(paste0("College Readiness in Math by ", in_plot_tag)) + scale_y_continuous(breaks = seq(from = 0, to = 70, by = 5), limits = c(0,70)) +  theme(axis.text.x = element_text(angle = -70, hjust = 1))
plot2
plot2 <- plot2 + plot_attributes

plot3 <- ggplot(data = in_data[!is.na(ReadingPercentReady)], aes(x = year, y = ReadingPercentReady, group=Subgroup)) + 
  geom_line(size = 4, aes(color = Subgroup)) + geom_point(aes(color = Subgroup), size = 6)

plot3 <- plot3 + scale_x_continuous( breaks= seq(from = 2007, to = 2015, by =1)) + xlab("Year") +ylab("Reading Percent Ready")
plot3 <- plot3 + ggtitle(paste0("College Readiness in Reading by ", in_plot_tag)) + scale_y_continuous(breaks = seq(from = 0, to = 70, by = 5), limits = c(0,70)) +  theme(axis.text.x = element_text(angle = -70, hjust = 1))

plot3
plot3 <- plot3 + plot_attributes

# save plots 

# now save stuff 
png(paste0("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/Reasearch_milestone_1/plots/College_readiness_all_", in_plot_tag, ".png"), height = 1200, width = in_w, type = "cairo")
print(plot1)
dev.off()

png(paste0("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/Reasearch_milestone_1/plots/College_readiness_math_", in_plot_tag, ".png"), height = 1200, width = in_w, type = "cairo")
print(plot2)
dev.off()

png(paste0("C:/Users/Nmath_000/Documents/MI_school/Second Year/PP 713/Reasearch_milestone_1/plots/College_readiness_reading_", in_plot_tag, ".png"), height = 1200, width = in_w, type = "cairo")
print(plot3)
dev.off()

}

# run function 
plot_funciton(gender_dt, "Gender")
plot_funciton(econ_dis_dt, "Income")
plot_funciton(race_dt, "Race", in_w = 1600)

