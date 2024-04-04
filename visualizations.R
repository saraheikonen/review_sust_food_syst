# SETUP-------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(fmsb)
library(radarchart)
library(qdapTools)
library(ggplot2)

# set working directory to the folder of this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
working_dir <- getwd()

# folder where figures will be saved: (will be created automatically)
fig_folder = 'figures_test/'

# relative filepath to the screening data
data_path = "List of papers 4.07.xlsx"

# NO NEED TO MODIFY CODE FROM HERE ON

# FUNCTIONS---------------------------------------------------------------------

# bar faceted bar plot (fig 1)
bar_plot <- function(df_plot, save_path) {
  
  plt_all_bar <- ggplot(data = df_plot, aes(x = label, y = All)) +
    geom_bar(stat = 'identity', fill = 'grey70') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    ylab("Share of papers in category (%)") +
    xlab("Category") +
    theme(plot.background = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    facet_wrap(vars(Variable), nrow = 1, ncol = 3, scales = "free_x") +
    theme(strip.background = element_blank(), strip.text.x = element_text(face = 'bold'),
          strip.clip = 'off')
  
  plt_all_bar
  
  
  save_filename <- paste(save_path, 'disicp_time_scale_all.pdf', sep = '')
  ggsave(save_filename, plt_all_bar, units = "cm", width = 20, height = 10)
  
}


# radar plot (fig 2)
radar_plot <- function(df_plot, palette, legend_labs, axis_labs, save_filename,
                       legend_title) {
  
  pdf(save_filename, width = 30/2.54)#, height =21/2.54)
  
  radarchart(df_plot, pcol = palette, plty = 1, plwd = 2, vlcex = 0.9,
             cglcol="grey", cglty=1, axislabcol="grey", axistype = 4, cglwd=1,
             calcex = 1, palcex = 2, caxislabels = axis_labs, pty = c(8,9,10,15,16,17,18))
  
  legend(x=1.5, y=1, legend = legend_labs, bty = "n", pch= c(8,9,10,15,16,17,18) ,
         col=palette, pt.bg = palette, cex=0.75, pt.cex=1,
         y.intersp = 1, x.intersp = 1, ncol = 1,
         title = legend_title,
         title.adj = 0, title.cex = 1 )
  
  dev.off()
}

# heat map (fig 3)
heatmap_plot <- function(df_plot, save_path, region) {
  
  plt_heatmap <- ggplot(data = df_plot, aes(x = Effect, y = Driver, fill = Number_of_papers)) +
    geom_tile(height = 1, width = 1) + 
    scale_fill_gradient(low = "white", high = 'grey40') +
    ylab("Socioeconomic attribute") +
    xlab("Food system effect") +
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

df_raw <- read_xlsx(data_path,
                    col_names = TRUE,
                    range = 'AB2:AQ594',
                    trim_ws = TRUE, 
                    .name_repair = 'unique')


## select only relevant columns
df <- df_raw %>% 
  dplyr::select(-c(starts_with("Other "), starts_with("Magnitude of ef"),
                   starts_with("Relevance"))) %>% 
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
         Time_scale = `Time sca`,
         Direction_of_effects = Directio,
         Reviewer = `Full pap`)

# Fix formatting issues
df <-  data.frame(lapply(df, function(x) {gsub(",", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(" ", "", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(":", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub(";;", ";", x)}))
df <-  data.frame(lapply(df, function(x) {gsub("^\\;|\\;$", "", x)}))

# included and excluded papers
df_all <- df

# Select only included papers
df <- df %>% 
  filter(as.numeric(Included) == 1)%>% # must convert this col to numeric for filtering
  dplyr::select(-Reviewer)

# regional coding

regions <- c("h_Global", "f_Sub-Saharan Africa", "e_Northern Africa & Western Asia",
             "d_Central & Southern Asia","c_Eastern & South-Eastern Asia", 
             "g_Latin America & the Caribbean", "Australia & New Zealand", "b_Oceania",
             "a_Europe & Northern America")

df_regions_enc <- mtabulate(strsplit(df$Geographical_range, ";")) %>%
  dplyr::select(-c(`NA`, NR))

colnames(df_regions_enc) <- regions

# combine Australia, NZ and Oceania under the name 'Australia & Oceania'
df_regions_enc <- df_regions_enc %>%
  mutate(Oceania_comb = ifelse(`Australia & New Zealand` == 1 | `b_Oceania` == 1, 1, 0)) %>%
  dplyr::select(-c(`Australia & New Zealand`,`b_Oceania`)) %>%
  rename(`b_Australia & Oceania` = Oceania_comb) %>%
  dplyr::select(order(desc(colnames(.))))

regions <- colnames(df_regions_enc) 

# Bar plots of time scale, discipline and geographical extent (fig 1)-----------

# Timescale

time_labels = c("a_less than a year",
                "b_one to five years",
                "c_five to twenty years",
                "d_twenty to fifty years",
                "e_more than fifty years")

# Time scales separated and remapped into new codes
df_reg_time <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Time_scale, ";"))) %>%
  dplyr::select(-c(`NA`, NR)) # drop NA and NR = not applicable and not relevant

df_reg_time_sums <- data.frame("Time_scale" = paste("", seq(1:5), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_time %>%
    filter(.[[region]] == 1) %>%
    dplyr::select(-(all_of(regions))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(reg_sums) <- c(region, "Time_scale")
  df_reg_time_sums <- df_reg_time_sums %>%
    left_join(reg_sums, by = "Time_scale")  
}

df_all_time_plot <- df_reg_time_sums %>%
  dplyr::select(-Time_scale)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>%
  dplyr::select(All) %>%
  mutate(Variable = "Time_scale", 
         label = time_labels)

# Discipline

# Disciplines separated and remapped into new codes
df_reg_discip <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Discipline, ";")))

# find the largest six disciplines, others disciplines will be combined under "others"
sums_discip <- colSums(df_reg_discip[,9:19]) %>%
  sort(decreasing = TRUE) %>%
  head(6)

# manually combining smallest categories based on sums_disicp
df_reg_discip <- df_reg_discip %>%
  mutate(`_8` = ifelse(`10` == 1 | `4` == 1 | `8` == 1 | `9` == 1 | `11` == 1, 1, 0)) %>%
  dplyr::select(-c(`10`, `4`, `8`, `9`, `11`)) %>%
  rename("8" = "_8")

df_reg_discip_sums <- data.frame("Discipline" = c("1", "2", "3", "5", "6", "7", "8"))

discip_labels <- c("economics", 
                   "biology", "agriculture", 
                   "sociology", "political science", 
                   "environmental science", "x_others")

for (region in regions) {
  reg_sums <- df_reg_discip %>%
    filter(.[[region]] == 1) %>%
    dplyr::select(-(all_of(regions))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(reg_sums) <- c(region, "Discipline")
  df_reg_discip_sums <- df_reg_discip_sums %>%
    left_join(reg_sums, by = "Discipline")  
}

df_all_discip_plot <- df_reg_discip_sums %>%
  dplyr::select(-Discipline)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>%
  dplyr::select(All) %>%
  mutate(Variable = "Discipline", 
         label = discip_labels)

# Geographical extent

scale_labels = c("f_global","e_regional", "d_national","c_regency/district",
                 "b_village","a_household/individual")

# Scales of study separated and remapped into new codes
df_reg_scale <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Scale_of_study, ";"))) %>%
  dplyr::select(-c(`NA`, NR, `7`, `8`, `9`)) # drop not applicable, not relevant, their number code versions and "others"

df_reg_scale_sums <- data.frame("Scale_of_study" = paste("", seq(1:6), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_scale %>%
    filter(.[[region]] == 1) %>%
    dplyr::select(-(all_of(regions))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(reg_sums) <- c(region, "Scale_of_study")
  df_reg_scale_sums <- df_reg_scale_sums %>%
    left_join(reg_sums, by = "Scale_of_study")  
}

df_all_scale_plot <- df_reg_scale_sums %>%
  dplyr::select(-Scale_of_study)  %>%
  mutate(All = rowSums(as.matrix(.))/sum(as.matrix(.))*100)  %>%
  dplyr::select(All) %>%
  mutate(Variable = "Scale_of_study", 
         label = scale_labels)


# Create faceted plot combining statistics

df_all_plot <- rbind(df_all_time_plot, df_all_discip_plot, df_all_scale_plot)
bar_plot(df_all_plot, fig_folder)

# Radar / spiderweb plots of sos-ec drivers and food syst effects by region (fig 2)-----

# drivers separated and remapped into new codes
df_reg_drivers <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Socioeconomic_attributes_drivers, ";"))) %>%
  dplyr::select(-`13`) %>%
  mutate(`_1` = ifelse(`1` == 1 | `4` == 1, 1, 0),
         `_2` = ifelse(`2` == 1 | `14` == 1 | `15` == 1, 1, 0),
         `_3` = ifelse(`3` == 1 | `11` == 1, 1, 0),
         `_4` = ifelse(`5` == 1 | `12` == 1 | `18` == 1, 1, 0),
         `_5` = ifelse(`7` == 1 | `8` == 1 | `10` == 1 | `17` == 1, 1, 0),
         `_6` = ifelse(`9` == 1 | `16` == 1 | `6` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "_1","_2", "_3", "_4", "_5", "_6"))) %>%
  rename("1" = "_1", "2" = "_2", "3" = "_3", "4" = "_4", "5" = "_5", "6" = "_6")

df_reg_driver_sums <- data.frame("Driver" = paste("", seq(1:6), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_drivers %>%
    filter(.[[region]] == 1) %>%
    dplyr::select(-(all_of(regions))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(reg_sums) <- c(region, "Driver")
  df_reg_driver_sums <- df_reg_driver_sums %>%
    left_join(reg_sums, by = "Driver")  
}

drivers_palette <- c('#332288', '#332288', '#882255','#44AA99', '#44AA99', '#44AA99')

df_reg_driver_sums <- df_reg_driver_sums %>%
  mutate(Driver = c("f_network & values", "e_gender, age, & family",
                    "d_education & information", "c_income & prices",
                    "b_infrastructure & institutions",
                    "a_politics & policy")) %>%
  rename(Category = Driver)

df_reg_driver_plot <- df_reg_driver_sums %>%
  arrange(Category) %>%
  dplyr::select(-Category) %>%
  # transform to study counts to percentage of studies per region
  mutate_all(~ ./sum(.))

max_all <- max(df_reg_driver_plot)
min_all <- min(df_reg_driver_plot)
df_reg_driver_plot <- rbind(rep(max_all,ncol(df_reg_driver_plot)) , 
                            rep(min_all,ncol(df_reg_driver_plot)) , 
                            df_reg_driver_plot)

driver_labels <- c("a_politics & policy", "b_infrastructure & institutions",
                   "c_income & prices", "d_education & information", 
                   "e_gender, age, & family","f_network & values")

axis_labs <- c("0.0", "0.07", "0.15", "0.22", "0.29")
legend_title <- "Socioeconomic attribute" 

radar_plot(df_reg_driver_plot, drivers_palette, driver_labels, axis_labs, 
           paste(fig_folder,'region_drivers.pdf',sep = ''),
           legend_title)


# effects separated and remapped into new codes
df_reg_effects <- df_regions_enc %>%
  cbind(mtabulate(strsplit(df$Research_topic_effects, ";"))) %>%
  dplyr::select(-`12`) %>%
  mutate(`_1` = ifelse(`1` == 1 | `2` == 1, 1, 0),
         `_2` = ifelse(`3` == 1, 1, 0),
         `_3` = ifelse(`4` == 1, 1, 0),
         `_4` = ifelse(`5` == 1 | `11` == 1 | `13` == 1, 1, 0),
         `_5` = ifelse(`6` == 1 | `7` == 1, 1, 0),
         `_6` = ifelse(`8` == 1, 1, 0),
         `_7` = ifelse(`9` == 1 | `10` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "_1","_2", "_3", "_4", "_5", "_6", "_7"))) %>%
  rename("1" = "_1", "2" = "_2", "3" = "_3", "4" = "_4", "5" = "_5", "6" = "_6", "7" = "_7")

df_reg_effect_sums <- data.frame("Effect" = paste("", seq(1:7), sep = ""))

for (region in regions) {
  reg_sums <- df_reg_effects %>%
    filter(.[[region]] == 1) %>%
    dplyr::select(-(all_of(regions))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(reg_sums) <- c(region, "Effect")
  df_reg_effect_sums <- df_reg_effect_sums %>%
    left_join(reg_sums, by = "Effect")  
}

effects_palette <- c('#AA4499','#AA4499','#AA4499','#88CCEE','#88CCEE','#6D6103','#6D6103')

df_reg_effect_sums <- df_reg_effect_sums %>%
  mutate(Effect = c("g_diet change & novel foods", "b_food loss & waste",
                    "f_nutrition & health","e_precision agriculture", "c_climate change & biodiversity",
                    "a_freshwater & marine resources", "d_land resources & soil health")) %>%
  rename(Category = Effect)

df_reg_effect_plot <- df_reg_effect_sums %>%
  arrange(Category) %>%
  dplyr::select(-Category) %>%
  # transform to study counts to percentage of studies per region
  mutate_all(~ ./sum(.))

max_all <- max(df_reg_effect_plot)
min_all <- min(df_reg_effect_plot)
df_reg_effect_plot <- rbind(rep(max_all,ncol(df_reg_effect_plot)) , 
                            rep(min_all,ncol(df_reg_effect_plot)) , 
                            df_reg_effect_plot)


effect_labels <- c("a_freshwater & marine resources", "b_food loss & waste",
                   "c_climate change & biodiversity", "d_land resources & soil health" ,
                   "e_precision agriculture", "f_nutrition & health", 
                   "g_diet change & novel foods")

axis_labs <- c("0", "0.09", "0.18", "0.27", "0.36")
legend_title <- "Food system effect" 

radar_plot(df_reg_effect_plot, effects_palette, effect_labels, axis_labs, 
           paste(fig_folder,'region_effects.pdf',sep = ''),
           legend_title)

# Heatmap of drivers and effects, including all papers (fig 3)------------------

df_drivers_enc <- mtabulate(strsplit(df$Socioeconomic_attributes_drivers, ";")) %>%
  dplyr::select(-c(`13`)) %>%
  mutate(d1 = ifelse(`1` == 1 | `4` == 1, 1, 0),
         d2 = ifelse(`2` == 1 | `14` == 1 | `15` == 1, 1, 0),
         d3 = ifelse(`3` == 1 | `11` == 1, 1, 0),
         d4 = ifelse(`5` == 1 | `12` == 1 | `18` == 1, 1, 0),
         d5 = ifelse(`7` == 1 | `8` == 1 | `10` == 1 | `17` == 1, 1, 0),
         d6 = ifelse(`9` == 1 | `16` == 1 | `6` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "d1","d2", "d3", "d4", "d5", "d6")))# %>%
#rename("1" = "_1", "2" = "_2", "3" = "_3", "4" = "_4", "5" = "_5", "6" = "_6") 
#rename_with( ~ paste0("d", .x))

df_effects_enc <- (mtabulate(strsplit(df$Research_topic_effects, ";"))) %>%
  dplyr::select(-`12`) %>%
  mutate(e1 = ifelse(`1` == 1 | `2` == 1, 1, 0),
         e2 = ifelse(`3` == 1, 1, 0),
         e3 = ifelse(`4` == 1, 1, 0),
         e4 = ifelse(`5` == 1 | `11` == 1 | `13` == 1, 1, 0),
         e5 = ifelse(`6` == 1 | `7` == 1, 1, 0),
         e6 = ifelse(`8` == 1, 1, 0),
         e7 = ifelse(`9` == 1 | `10` == 1, 1, 0)) %>%
  dplyr::select(any_of(c(regions, "e1","e2", "e3", "e4", "e5", "e6", "e7")))


df_drivers_effects_enc <- cbind(df_drivers_enc, df_effects_enc)

df_driver_effect_sums <- data.frame("Driver" = paste("d", seq(1:6), sep = ""))

effects_list <- colnames(df_effects_enc)

for (effect in effects_list) {
  eff_sums <- df_drivers_effects_enc %>%
    filter(.[[effect]] == 1) %>%
    dplyr::select(-(all_of(effects_list))) %>%
    colSums() %>%
    as.data.frame() %>%
    cbind(rownames(.))
  
  colnames(eff_sums) <- c(effect, "Driver")
  df_driver_effect_sums <- df_driver_effect_sums %>%
    left_join(eff_sums, by = "Driver")  
}

# rename drivers and effects

effect_labels <- c("e_diet change & novel foods", "b_food loss & waste",
                   "d_nutrition & health","g_precision agriculture", "c_climate change & biodiversity",
                   "a_freshwater & marine resources", "f_land resources & soil health")

driver_labels <- c("a_network & values", "b_gender, age, & family", 
                   "c_education & information", "d_income & prices", 
                   "e_infrastructure & institutions",
                   "f_politics & policy")

colnames(df_driver_effect_sums) <- c('Driver', effect_labels)

df_driver_effect_sums <- df_driver_effect_sums %>%
  mutate(Driver = driver_labels) %>%
  dplyr::select(order(colnames(.)))

df_drivers_effects_long <- df_driver_effect_sums %>%
  pivot_longer(cols = -Driver,
               names_to = 'Effect',
               values_to = 'Number_of_papers')

heatmap_plot(df_drivers_effects_long, fig_folder, 'all_regions')





