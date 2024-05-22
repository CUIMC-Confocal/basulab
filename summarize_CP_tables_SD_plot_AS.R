# summarize_CP_tables_SD_plot.R
# R script to select mean and edge intensity data from CellProfiler object results files
# and summarize by experiment and well (designed for Cytation imager data)
# To use: Run the script.
# Will be prompted for a file

# ---- Setup ----
require(tidyverse)
require(readr)
require(stringr)


# ---- Prompt for an object file ----
# no message will be displayed
objectFile <- file.choose()

# Read the data from the file
objectData <- read_csv(objectFile,
                    locale = locale())


mean_counts <- objectData %>% 
  group_by(FileName_orig) %>% 
  summarise(nCells = n(),
            Mean_PLAMaxima = mean(Children_PLA_Maxima_Objects_Count),
            Min_PLAMaxima = min(Children_PLA_Maxima_Objects_Count),
            Max_PLAMaxima = max(Children_PLA_Maxima_Objects_Count),
            SD_PLAMaxima = sd(Children_PLA_Maxima_Objects_Count),
            MeanPLAObjects = mean(Children_PLA_Objects_Count),
            MinPLAObjects = min(Children_PLA_Objects_Count),
            MaxPLAObjects = max(Children_PLA_Objects_Count),
            SDPLAObjects = sd(Children_PLA_Objects_Count))

# ---- Save new file ----
objectName <- str_sub(basename(objectFile), 1, -5) # name of the file without higher levels or extension
parentDir <- dirname(objectFile) # parent of the file
outputFile = paste(objectName, "_summary.csv") # spaces will be inserted
write_csv(mean_counts,file.path(parentDir, outputFile))

# ---- Plot ----
p_maxima <- ggplot(objectData,
                 aes(x=FileName_orig,
                     y=Children_PLA_Maxima_Objects_Count)) + 
  geom_violin(trim=FALSE)  +
  stat_summary(fun.data=mean_sdl,
               geom="pointrange", color="green")

outputPlot = paste(objectName, "maxima plot.pdf")

# plot size can be changed according to the number of panels
ggsave(file.path(parentDir, outputPlot))


p_obj <- ggplot(objectData,
                   aes(x=FileName_orig,
                       y=Children_PLA_Objects_Count)) + 
  geom_violin(trim=FALSE)  +
  stat_summary(fun.data=mean_sdl,
               geom="pointrange", color="red")

outputPlot = paste(objectName, "objects plot.pdf")

# plot size can be changed according to the number of panels
ggsave(file.path(parentDir, outputPlot))