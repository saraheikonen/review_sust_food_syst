# SETUP-------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
library(fmsb)
library(radarchart)
library(qdapTools)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(tmap)

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

# folder where figures will be saved: (will be created automatically)
fig_folder = 'figures_cleaned_data/'

# relative filepath to the screening data
data_path = "data_list_papers.xlsx"

# NO NEED TO MODIFY CODE FROM HERE ON

# FUNCTIONS---------------------------------------------------------------------

# bar faceted bar plot (fig 1)
bar_plot <- function(df_plot, save_path) {
  
  plt_all_bar <- ggplot(data = df_plot, aes(x = label, y = All_perc)) +
    geom_bar(stat = 'identity', fill = 'grey70') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    ylab("Share of papers in category (%)") +
    xlab("Category") +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    # create facets for values in the "Variable" column: discipline, scale of study and time scale
    facet_wrap(vars(Variable), nrow = 1, ncol = 3, scales = "free_x") +
    theme(strip.background = element_blank(), strip.text.x = element_text(face = 'bold'),
          strip.clip = 'off')
  
  plt_all_bar
  
  
  save_filename <- paste(save_path, 'discip_time_scale_all.pdf', sep = '')
  ggsave(save_filename, plt_all_bar, units = "cm", width = 20, height = 10)
  
}


# radar plot (fig 2)
radar_plot <- function(df_plot, palette, legend_labs, axis_labs, save_filename,
                       legend_title) {
  
  # initialize pdf document to write the image
  pdf(save_filename, width = 30/2.54)#, height =21/2.54)
  
  # see radarchart dicumentation at https://www.rdocumentation.org/packages/fmsb/versions/0.7.6/topics/radarchart
  radarchart(df_plot, pcol = palette, plty = 1, plwd = 2, vlcex = 0.9,
             cglcol="grey", cglty=1, axislabcol="grey", axistype = 4, cglwd=1,
             calcex = 1, palcex = 2, caxislabels = axis_labs, 
             pty = c(8,9,10,15,16,17,18)) # the numbers n pty define the marker symbols
  
  legend(x=1.5, y=1, legend = legend_labs, bty = "n", pch= c(8,9,10,15,16,17,18) , # the numbers in pch define the marker symbols
         col=palette, pt.bg = palette, cex=0.75, pt.cex=1,
         y.intersp = 1, x.intersp = 1, ncol = 1,
         title = legend_title,
         title.adj = 0, title.cex = 1 )
  
  # stop writing in the pdf
  dev.off()
}

# map parts (fig 2)
map_parts_plot <- function(save_path) {
  
  # download country boundaries
  df_countries <- ne_download(scale = 110, type = "countries",
                        category = "cultural", returnclass = "sf") %>%
    dplyr::select(SOVEREIGNT, SUBREGION)
  
  # this avoids issues when dissolving invalid geometries
  sf_use_s2(FALSE)
  
  # add column for custom geographical regions
  df_regions <- df_countries %>%
    mutate(SDG_GROUP = case_when(SUBREGION %in% c("Melanesia")
                                 ~ "Australia and Oceania",
                                 SUBREGION %in% c("Eastern Africa", "Middle Africa",
                                                  "Southern Africa", "Western Africa")
                                 ~ "Sub-Saharan Africa",
                                 SUBREGION %in% c("Northern Africa", "Western Asia")
                                 ~ "Northern Africa and Western Asia",
                                 SUBREGION %in% c("Central Asia", "Southern Asia")
                                 ~ "Central and Southern Asia",
                                 SUBREGION %in% c("South-Eastern Asia", "Eastern Asia")
                                 ~ "Eastern and South-Eastern Asia",
                                 SUBREGION %in% c("Caribbean", "South America",
                                                  "Central America") 
                                 ~ "Latin America and the Caribbean",
                                 SUBREGION %in% c("Australia and New Zealand") 
                                 ~ "Australia and Oceania",
                                 SUBREGION %in% c("Northern America", 
                                                  "Eastern Europe", "Northern Europe",
                                                  "Western Europe", "Southern Europe" ) 
                                 ~ "Europe and Northern America",
                                 SUBREGION %in% c("Antarctica",  "Seven seas (open ocean)")
                                 ~ "Other")) %>% 
    filter(SDG_GROUP != "Other") %>%
    group_by(SDG_GROUP) %>% 
    st_make_valid() %>%
    summarize()
  
  # make separate maps of all regions and save as pdf
  
  regions <- df_regions %>%
    sf::st_drop_geometry() %>%
    pull()
  
  for (region in regions) {
    
    df_region_map <- df_regions %>%
      filter(SDG_GROUP == region)
    
    plt_region <- tm_shape(df_region_map) +
      tm_polygons(fill = 'grey') +
      tm_layout(bg = FALSE, frame = FALSE) 
    
    (plt_region)
    
    fig_path <- paste(save_path, region, '.pdf', sep="")
    tmap_save(plt_region, fig_path)
    
  }
  
  
  
}

# heat map (fig 3)
heatmap_plot <- function(df_plot, save_path, region) {
  
  plt_heatmap <- ggplot(data = df_plot, aes(x = Transformation, y = Driver, fill = Number_of_papers)) +
    geom_tile(height = 1, width = 1) + # this does not produce perfect squares, not sure why
    scale_fill_gradient(low = "white", high = 'grey40') +
    ylab("Socioeconomic driver") +
    xlab("Food system transformation") +
    geom_text(aes(label = Number_of_papers)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_blank()) +
    coord_flip()
  
  plt_heatmap
  
  save_filename <- paste(save_path, 'driver_effect_heatmap_', region, '.pdf', sep="") 
  ggsave(save_filename, plt_heatmap, height = 11.5, width = 18.5, units = 'cm')
  
}

# VISUALIZATION CODE BEGINS-----------------------------------------------------

# create folder for figures if it doesn't exist yet

if (!dir.exists(fig_folder)) {
  dir.create(fig_folder)
}

# Read and clean the data-------------------------------------------------------

# read excel sheet
df_raw <- read_xlsx(data_path,
                    col_names = TRUE,
                    range = 'J2:U594',
                    trim_ws = TRUE, 
                    .name_repair = 'unique')


## select only relevant columns
df <- df_raw %>% 
  dplyr::select(-c(starts_with("Other "), starts_with("Magnitude of ef"),
                   starts_with("Relevance"), starts_with('Disciplines:'))) %>% # drop the discipline
  # column that does not mention nutrition and health separately
  # make col names shorter
  setNames(substr(names(.), 1, 8))


# Rename columns 
# this approach is possibly robust also if the name of columns in excel change..
df <- df %>% 
  rename(Included = Included,
         Discipline = Discipli,
         Geographical_range = Geograph,
         Scale_of_study = `Scale of`,
         Research_topic_effects = Research,
         Socioeconomic_attributes_drivers = Socioeco,
         Time_scale = `Time sca`)

# Fix formatting issues
df <-  data.frame(lapply(df, function(x) {gsub(",", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(" ", "", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(":", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(";;", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub("^\\;|\\;$", "", x)}))
df <-  data.frame(lapply(df, function(x) {gsub("\\.0", "", x)}))

# included and excluded papers
df_all <- df

# Select only included papers
df <- df %>% 
  filter(as.numeric(Included) == 1) # must convert this col to numeric for filtering

# Start creating plots ---------------------------------------------------------

# First, create a dataframe where each row corresponds to a paper and the columns
# represent regions. When value in a column = 1, the paper covers the region and 
# when column value = 0, the paper does not cover the region.

df_regions_enc <- mtabulate(strsplit(df$Geographical_range, ";")) %>%
  dplyr::select(-c(`NA`, NR))

# Rename columns using region names. The order here corresponds to the coding in 
# the excel sheet, i.e. Global = 1, Sub-Saharan Africa = 2 etc. The letter prefixes 
# are for ordering the regions differently in plots.
regions <- c("h_Global", "f_Sub-Saharan Africa", "e_Northern Africa & Western Asia",
             "d_Central & Southern Asia","c_Eastern & South-Eastern Asia", 
             "g_Latin America & the Caribbean", "Australia & New Zealand", "b_Oceania",
             "a_Europe & Northern America")

colnames(df_regions_enc) <- regions

# combine Australia, NZ and Oceania under the name 'Australia & Oceania'
df_regions_enc <- df_regions_enc %>%
  mutate(Oceania_comb = ifelse(`Australia & New Zealand` == 1 | `b_Oceania` == 1, 1, 0)) %>%
  dplyr::select(-c(`Australia & New Zealand`,`b_Oceania`)) %>%
  rename(`b_Australia & Oceania` = Oceania_comb) %>%
  dplyr::select(order(desc(colnames(.))))

# extract region names to be able to loop through regions later
regions <- colnames(df_regions_enc) 

# Bar plots of time scale, discipline and geographical extent (fig 1)-----------

# Timescale

# Labels for time scale, the order corresponds to the codes in the excel sheet:
# less than a year = 1, one to five years = 2 etc.
time_labels = c("a_less than a year",
                "b_one to five years",
                "c_five to twenty years",
                "d_twenty to fifty years",
                "e_more than fifty years")

# To df_regions_enc, add more columns to represent timescales. When value in
# a column = 1, the paper covers the time scale and when column value = 0, 
# the paper does not cover the timescale.
df_reg_time <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Time_scale, ";"))) %>%
  dplyr::select(-c(`NA`, NR)) # drop NA and NR = not applicable and not relevant

# Loop through regions to calculate the number of papers in each region that 
# covers each time scale

# dataframe for collecting results from loop
df_reg_time_sums <- data.frame("Time_scale" = paste("", seq(1:5), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_time %>%
    filter(.[[region]] == 1) %>% # select only the rows that cover the region
    dplyr::select(-(all_of(regions))) %>% # drop all region columns
    colSums() %>% # summarize the number of papers covering each time scale in the region
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add results of the region to the results collecting dataframeS
  colnames(reg_sums) <- c(region, "Time_scale")
  df_reg_time_sums <- df_reg_time_sums %>%
    left_join(reg_sums, by = "Time_scale")  
}

# For fig 1, we do not need regional results, so summarize the numbers of papers 
# from all regions together and transform into percentage
df_all_time_plot <- df_reg_time_sums %>%
  dplyr::select(-Time_scale)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>% # sum number of papers from all regions, transfom to percentage
  dplyr::select(All) %>% # only keep the percentage result
  mutate(Variable = "Time_scale", # add a variable label column for creating facets in the bar plot
         label = time_labels) # add labels for time scales

# Discipline

# To df_regions_enc, add more columns to represent discipline. When value in
# a column = 1, the paper covers the discipline and when column value = 0, 
# the paper does not cover the discipline.
df_reg_discip <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Discipline, ";")))

# Manual inspection of data: 
# find the codes of the most common six disciplines, others disciplines will be combined under "others"
sums_discip <- colSums(df_reg_discip[,9:19]) %>%
  sort(decreasing = TRUE) %>%
  head(6)

# Manually combine smallest categories based on sums_discip. Within the ifelse statement,
# the codes correspond to codes in the excel sheet. New code 9 will represent "others",
# i.e. the combination category, and new code 8 will represent old code 12 (nutrition and health)
df_reg_discip <- df_reg_discip %>%
  mutate(`_9` = ifelse(`4` == 1 | `6` == 1 | `8` == 1 | `9` == 1 | `10` == 1 | `11` == 1, 1, 0)) %>%
  dplyr::select(-c(`4`, `6`, `8`, `9`, `10`, `11`)) %>%
  rename("9" = "_9",
         "8" = `12`)

# Labels of the disciplines in numerical order by code, corresponds to codes in 
# the excel sheet except for 8 (= nutrition and health) and 9 (= others):
# economics = 1, biology = 2, agriculture = 3, sociology = 5, environmental science = 7
discip_labels <- c("economics", "biology", "agriculture", "sociology", 
                   "environmental science", "nutrition and health", "x_others")

# Loop through regions to calculate the number of papers in each region that 
# covers each discipline

# dataframe for collecting results from the loop
df_reg_discip_sums <- data.frame("Discipline" = c("1", "2", "3", "5", "7", "8", "9"))


for (region in regions) {
  reg_sums <- df_reg_discip %>%
    filter(.[[region]] == 1) %>% # select only the rows that cover the region
    dplyr::select(-(all_of(regions))) %>% # drop all region columns
    colSums() %>% # summarize the number of papers covering each discipline in the region
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add regional results to the results collecting dataframe
  colnames(reg_sums) <- c(region, "Discipline")
  df_reg_discip_sums <- df_reg_discip_sums %>%
    left_join(reg_sums, by = "Discipline")  
}

# For fig 1, we do not need regional results. Similarly as with time scale, 
# summarize the numbers of papers from all regions together and transform into percentage 
df_all_discip_plot <- df_reg_discip_sums %>%
  dplyr::select(-Discipline)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>%
  dplyr::select(All) %>%
  mutate(Variable = "Discipline", 
         label = discip_labels)

# Scale of study

# To df_regions_enc, add more columns to represent the scale of study. When value in
# a column = 1, the paper covers the scale of study and when column value = 0, 
# the paper does not cover the scale of study.
df_reg_scale <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Scale_of_study, ";"))) %>%
  dplyr::select(-c(`NA`, NR, `7`, `8`, `9`)) # drop not applicable, not relevant, their number code versions and "others"

# Labels, order corresponds to codes in the excel sheet: 1 = global,
# 2 = regional, etc.
scale_labels = c("f_global","e_regional", "d_national","c_regency/district",
                 "b_village","a_household/individual")

# Loop through regions to calculate the number of papers in each region that 
# covers each scale of study

# dataframe for collecting results from the loop
df_reg_scale_sums <- data.frame("Scale_of_study" = paste("", seq(1:6), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_scale %>%
    filter(.[[region]] == 1) %>% # select only the rows that cover the region
    dplyr::select(-(all_of(regions))) %>% # drop all region columns
    colSums() %>% # summarize the number of papers covering each scale of study in the region
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add regional results to the results collecting dataframe
  colnames(reg_sums) <- c(region, "Scale_of_study")
  df_reg_scale_sums <- df_reg_scale_sums %>%
    left_join(reg_sums, by = "Scale_of_study")  
}

# For fig 1, we do not need regional results. Similarly as with time scale and discipline, 
# summarize the numbers of papers from all regions together and transform into percentage
df_all_scale_plot <- df_reg_scale_sums %>%
  dplyr::select(-Scale_of_study)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>%
  dplyr::select(All) %>%
  mutate(Variable = "Scale_of_study", 
         label = scale_labels)


# Create faceted plot combining percentage of papers for time scale, discipline
# and scale of study
df_all_plot <- rbind(df_all_time_plot, df_all_discip_plot, df_all_scale_plot) %>%
  rename(All_perc = All)

bar_plot(df_all_plot, fig_folder)

# save figure source data, edit one column name for clarity
df_all_plot_table <- df_all_plot %>%
  rename(Perc_of_papers = All_perc)

write_xlsx(df_all_plot, paste0(fig_folder, '/source_data_fig1.xlsx'))


# Radar / spiderweb plots of sos-ec drivers and food syst effects by region (fig 2)-----

# Plot region map elements 
map_parts_plot(fig_folder)

# Sos-ec drivers

# To df_regions_enc, add more columns to represent sos-ec drivers. When value in
# a column = 1, the paper covers the driver and when column value = 0, 
# the paper does not cover the driver. 
df_reg_drivers <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Socioeconomic_attributes_drivers, ";"))) %>%
  dplyr::select(-`13`) %>%
  mutate(`_1` = ifelse(`1` == 1 | `4` == 1, 1, 0), # reclassify drivers for the figures. The numbers/codes
                                                   # within ifelse statements correspond to the excel sheet codes
                                                   # new codes 1-6 do not.
         `_2` = ifelse(`2` == 1 | `14` == 1 | `15` == 1, 1, 0),
         `_3` = ifelse(`3` == 1 | `11` == 1, 1, 0),
         `_4` = ifelse(`5` == 1 | `12` == 1 | `18` == 1, 1, 0),
         `_5` = ifelse(`9` == 1 | `16` == 1 | `6` == 1 | `10` == 1 | `8` == 1, 1, 0),
         `_6` = ifelse(`7` == 1 | `17` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "_1","_2", "_3", "_4", "_5", "_6"))) %>%
  rename("1" = "_1", "2" = "_2", "3" = "_3", "4" = "_4", "5" = "_5", "6" = "_6")


# Loop through regions to calculate the number of papers in each region that 
# covers each sos-ec driver

# dataframe for collecting results from the loop
df_reg_driver_sums <- data.frame("Driver" = paste("", seq(1:6), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_drivers %>%
    filter(.[[region]] == 1) %>% # select only the rows that cover the region
    dplyr::select(-(all_of(regions))) %>% # drop all region columns
    colSums() %>% # summarize the number of papers covering each driver in the region
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add regional results to the results collecting dataframe
  colnames(reg_sums) <- c(region, "Driver")
  df_reg_driver_sums <- df_reg_driver_sums %>%
    left_join(reg_sums, by = "Driver")  
}

# labels for the new driver categories, new code 1 = network & values, 2 = gender etc.
# letter prefixes for ordering the plot.
df_reg_driver_sums <- df_reg_driver_sums %>%
  mutate(Driver = c("f_network & values", "e_gender, age, & family",
                    "d_education & information", "c_income & prices",
                    "b_politics, policy & institutions",
                    "a_infrastructure")) %>%
  rename(Category = Driver)

# order the rows alphabetically based on driver labels
df_reg_driver_plot <- df_reg_driver_sums %>%
  arrange(Category) %>%
  dplyr::select(-Category) %>%
  # transform to study counts to percentage of studies per region
  mutate_all(~ ./sum(.))

# get min and max of the percentage of studies per region
max_all <- max(df_reg_driver_plot)
min_all <- min(df_reg_driver_plot)

# for the radarchart, add max as the first row and min as the second row for all
# columns. Now rows 3-8 include the actual data to be plotted.
df_reg_driver_plot <- rbind(rep(max_all,ncol(df_reg_driver_plot)) , 
                            rep(min_all,ncol(df_reg_driver_plot)) , 
                            df_reg_driver_plot)

# labels and palette for plot, both in the same order
driver_labels <- c("a_infrastructure", "b_politics, policy & institutions",
                   "c_income & prices", "d_education & information", 
                   "e_gender, age, & family","f_network & values")

drivers_palette <- c('#332288', '#332288', '#882255','#44AA99', '#44AA99', '#44AA99')

# axis labels defined manually based on the min and max percentages found above
axis_labs <- c("0.0", "0.07", "0.15", "0.22", "0.29")
legend_title <- "Socioeconomic driver"


# save figure source data. Add a column to indicate what data each row includes
df_reg_driver_plot_source_data <- df_reg_driver_plot %>%
  mutate(row_description = c('max_perc_all_regions', 'min_perc_all_regions', driver_labels))

write_xlsx(df_reg_driver_plot_source_data, paste0(fig_folder, '/source_data_fig2_drivers.xlsx'))

# make radar chart
radar_plot(df_reg_driver_plot, drivers_palette, driver_labels, axis_labs, 
           paste(fig_folder,'region_drivers.pdf',sep = ''),
           legend_title)

# Food system effects

# To df_regions_enc, add more columns to represent effects. When value in
# a column = 1, the paper covers the effect and when column value = 0, 
# the paper does not cover the effect. 
df_reg_effects <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Research_topic_effects, ";"))) %>%
  dplyr::select(-`12`) %>%
  mutate(`_1` = ifelse(`1` == 1 | `2` == 1, 1, 0), # reclassify effects for the figures. The numbers/codes
                                                   # within ifelse statements correspond to the excel sheet codes
                                                   # new codes 1-7 do not.
         `_2` = ifelse(`3` == 1, 1, 0),
         `_3` = ifelse(`4` == 1, 1, 0),
         `_4` = ifelse(`5` == 1 | `11` == 1 | `13` == 1, 1, 0),
         `_5` = ifelse(`6` == 1 | `7` == 1, 1, 0),
         `_6` = ifelse(`8` == 1, 1, 0),
         `_7` = ifelse(`9` == 1 | `10` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "_1","_2", "_3", "_4", "_5", "_6", "_7"))) %>%
  rename("1" = "_1", "2" = "_2", "3" = "_3", "4" = "_4", "5" = "_5", "6" = "_6", "7" = "_7")

# Loop through regions to calculate the number of papers in each region that 
# covers each effect

# dataframe for collecting results from the loop
df_reg_effect_sums <- data.frame("Effect" = paste("", seq(1:7), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_effects %>%
    filter(.[[region]] == 1) %>% # select only the rows that cover the region
    dplyr::select(-(all_of(regions))) %>% # drop all region columns
    colSums() %>% # summarize the number of papers covering each effect in the region
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add regional results to the results collecting dataframe
  colnames(reg_sums) <- c(region, "Effect")
  df_reg_effect_sums <- df_reg_effect_sums %>%
    left_join(reg_sums, by = "Effect")  
}

# labels for the new effect categories, new code 1 = diet change & novel foods, 2 = food loss & waste etc.
# letter prefixes for ordering the plot.
df_reg_effect_sums <- df_reg_effect_sums %>%
  mutate(Effect = c("g_diet change & novel foods", "b_food loss & waste",
                    "f_nutrition & health","e_precision agriculture", "c_climate change & biodiversity",
                    "a_freshwater & marine resources", "d_land resources & soil health")) %>%
  rename(Category = Effect)

# order the rows alphabetically based on effect labels
df_reg_effect_plot <- df_reg_effect_sums %>%
  arrange(Category) %>%
  dplyr::select(-Category) %>%
  # transform to study counts to percentage of studies per region
  mutate_all(~ ./sum(.))

# get min and max of the percentage of studies per region
max_all <- max(df_reg_effect_plot)
min_all <- min(df_reg_effect_plot)

# for the radarchart, add max as the first row and min as the second row for all
# columns. Now rows 3-8 include the actual data to be plotted.
df_reg_effect_plot <- rbind(rep(max_all,ncol(df_reg_effect_plot)) , 
                            rep(min_all,ncol(df_reg_effect_plot)) , 
                            df_reg_effect_plot)

# create palette and labels for the plot, both are in the same order.
effects_palette <- c('#AA4499','#AA4499','#AA4499','#88CCEE','#88CCEE','#6D6103','#6D6103')

effect_labels <- c("a_freshwater & marine resources", "b_food loss & waste",
                   "c_climate change & biodiversity", "d_land resources & soil health" ,
                   "e_precision agriculture", "f_nutrition & health", 
                   "g_diet change & novel foods")

# axis labels defined manually based on the min and max percentages found above
axis_labs <- c("0", "0.09", "0.18", "0.27", "0.36")
legend_title <- "Food system transformation" 

# save figure source data. Add a column to indicate what data each row includes
df_reg_effect_plot_source_data <- df_reg_effect_plot %>%
  mutate(row_description = c('max_perc_all_regions', 'min_perc_all_regions', effect_labels))

write_xlsx(df_reg_effect_plot_source_data, paste0(fig_folder, '/source_data_fig2_effects.xlsx'))

# make radar chart
radar_plot(df_reg_effect_plot, effects_palette, effect_labels, axis_labs, 
           paste(fig_folder,'region_effects.pdf',sep = ''),
           legend_title)

# Heatmap of drivers and effects, including all papers (fig 3)------------------

# Create a dataframe where each row corresponds to a paper and the columns
# represent sos-ec drivers. When value in a column = 1, the paper covers the driver and 
# when column value = 0, the paper does not cover the driver.
df_drivers_enc <- mtabulate(strsplit(df$Socioeconomic_attributes_drivers, ";")) %>%
  dplyr::select(-c(`13`)) %>%
  mutate(d1 = ifelse(`1` == 1 | `4` == 1, 1, 0), # again, reclassify drivers like we did for the radar charts
         d2 = ifelse(`2` == 1 | `14` == 1 | `15` == 1, 1, 0),
         d3 = ifelse(`3` == 1 | `11` == 1, 1, 0),
         d4 = ifelse(`5` == 1 | `12` == 1 | `18` == 1, 1, 0),
         d5 = ifelse(`9` == 1 | `16` == 1 | `6` == 1 | `10` == 1 | `8` == 1, 1, 0),
         d6 = ifelse(`7` == 1 | `17` == 1, 1, 0))%>%
  dplyr::select(any_of(c(regions, "d1","d2", "d3", "d4", "d5", "d6")))

# Create a dataframe where each row corresponds to a paper and the columns
# represent food system effects. When value in a column = 1, the paper covers the effect and 
# when column value = 0, the paper does not cover the effect.
df_effects_enc <- (mtabulate(strsplit(df$Research_topic_effects, ";"))) %>%
  dplyr::select(-`12`) %>%
  mutate(e1 = ifelse(`1` == 1 | `2` == 1, 1, 0), # again, reclassify effects like we did for the radar charts
         e2 = ifelse(`3` == 1, 1, 0),
         e3 = ifelse(`4` == 1, 1, 0),
         e4 = ifelse(`5` == 1 | `11` == 1 | `13` == 1, 1, 0),
         e5 = ifelse(`6` == 1 | `7` == 1, 1, 0),
         e6 = ifelse(`8` == 1, 1, 0),
         e7 = ifelse(`9` == 1 | `10` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "e1","e2", "e3", "e4", "e5", "e6", "e7")))

# combine driver and effect dataframes
df_drivers_effects_enc <- cbind(df_drivers_enc, df_effects_enc)

# Loop through effects to calculate the number of papers for each driver that 
# covers each effect

# effect colnames for looping
effects_list <- colnames(df_effects_enc)

# dataframe for collecting results
df_driver_effect_sums <- data.frame("Driver" = paste("d", seq(1:6), sep = ""))

for (effect in effects_list) {
  eff_sums <- df_drivers_effects_enc %>%
    filter(.[[effect]] == 1) %>% # select only the rows that cover the effect
    dplyr::select(-(all_of(effects_list))) %>% # drop all effect columns
    colSums() %>% # summarize the number of papers covering each driver for the effect
    as.data.frame() %>%
    cbind(rownames(.))
  
  # add data to results collecting dataframe
  colnames(eff_sums) <- c(effect, "Driver")
  df_driver_effect_sums <- df_driver_effect_sums %>%
    left_join(eff_sums, by = "Driver")  
}

# rename drivers and effects, order corresponds to the order of columns and rows 
# in the dataframe at this stage. Letter prefixes are for ordering the data in
# the plot
effect_labels <- c("e_diet change & novel foods", "b_food loss & waste",
                   "d_nutrition & health","g_precision agriculture", "c_climate change & biodiversity",
                   "a_freshwater & marine resources", "f_land resources & soil health")

driver_labels <- c("a_network & values", "b_gender, age, & family", 
                   "c_education & information", "d_income & prices", 
                   "e_politics, policy & institutions",
                   "f_infrastructure")

# column names are drivers
colnames(df_driver_effect_sums) <- c('Driver', effect_labels)

# row names are effects
df_driver_effect_sums <- df_driver_effect_sums %>%
  mutate(Driver = driver_labels) %>%
  dplyr::select(order(colnames(.)))

# transform dataframe to long format for plotting
df_drivers_effects_long <- df_driver_effect_sums %>%
  pivot_longer(cols = -Driver,
               names_to = 'Transformation',
               values_to = 'Number_of_papers')

# save figure source data
write_xlsx(df_drivers_effects_long, paste0(fig_folder, '/source_data_fig3.xlsx'))

# make heatmap
heatmap_plot(df_drivers_effects_long, fig_folder, 'all_regions')





