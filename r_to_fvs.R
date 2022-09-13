# R to FVS
# Takes tree lists from R and creates FVS TreeInit and StandInit files for the Forest Vegetation Simulator

# LOAD packages
library(tidyverse)
library(openxlsx)

# Example is a spruce-fir stand in Maine
# Data were collected using 1/20th acre fixed-radius plots

# ENTER common variables about your forest

VARIANT = "ne" # See https://www.fs.usda.gov/fvs/software/variantkey.shtml	
LOCATION = 922 # See local FVS variant guide
BASAL_AREA_FACTOR	= -20 # A positive value = basal area factor. Negative value = inverse of a large-tree fixed area plot.
SITE_SPECIES = "RS" # FVS species code for site index species

# CREATE an example data set with 20 tree observations from two plots in one stand 

tree <- tribble(
  ~STAND, ~PLOT, ~TREE, ~YEAR, ~SPP, ~DBH,
  "M1", 1, 1, 2022, "BF", 6.2,
  "M1", 1, 2, 2022, "RS", 7.5,
  "M1", 1, 3, 2022, "BF", 5.6,
  "M1", 1, 4, 2022, "RM", 6.4,
  "M1", 1, 5, 2022, "BF", 7.9,
  "M1", 1, 6, 2022, "BF", 5.1,
  "M1", 1, 7, 2022, "RS", 6.1,
  "M1", 1, 8, 2022, "BF", 8.1,
  "M1", 2, 1, 2022, "WP", 5.0,
  "M1", 2, 2, 2022, "BF", 5.2,
  "M1", 2, 3, 2022, "RS", 8.1, 
  "M1", 2, 4, 2022, "RS", 7.0,
  "M1", 2, 5, 2022, "BF", 6.4,
  "M1", 2, 6, 2022, "RM", 8.1,
  "M1", 2, 7, 2022, "RS", 6.5,
  "M1", 2, 8, 2022, "RM", 5.5,
  "M1", 2, 9, 2022, "BF", 6.5,
  "M1", 2, 10, 2022, "RM", 5.3,
  "M1", 2, 11, 2022, "RM", 5.9,
  "M1", 2, 12, 2022, "RS", 6.1
)

# CREATE make_FVS_TreeInit() function

make_FVS_TreeInit <- function(){
  tibble(
    STAND_ID = NA,
    PLOT_ID = NA,
    STANDPLOT_ID = NA,
    TREE_ID = NA,
    YEAR = NA,
    TAG_ID = NA,
    TREE_COUNT = NA,
    HISTORY = NA,
    SPECIES = NA,
    DIAMETER = NA,
    DIAMETER_HT = NA,
    DG = NA,
    HT = NA,
    HTG = NA,
    HTTOPK = NA,
    HT_TO_LIVE_CROWN = NA,
    CRRATIO = NA,
    DAMAGE1 = NA,
    SEVERITY1 = NA,
    DAMAGE2 = NA,
    SEVERITY2 = NA,
    DAMAGE3 = NA,
    SEVERITY3 = NA,
    DEFECT_CUBIC = NA,
    DEFECT_BOARD = NA,
    TREEVALUE = NA,
    PRESCRIPTION = NA,
    AGE = NA,
    SLOPE = NA,
    ASPECT = NA,
    PV_CODE = NA,
    PV_REF_CODE = NA,
    TOPOCODE = NA,
    SITEPREP = NA) 
}

# CREATE blank template for TreeInit sheet

FVS_TreeInit_blank <- make_FVS_TreeInit()

# APPLY example tree records FVS_TreeInit_blank

TreeInit <- FVS_TreeInit_blank %>% 
  add_row(
    # Gotta have these
         STAND_ID = tree$STAND, 
         PLOT_ID = tree$PLOT, 
         TREE_ID = tree$TREE, 
         SPECIES = tree$SPP, 
         DIAMETER = tree$DBH,
         YEAR = tree$YEAR,
    # Good to have these
        HT = NA,
        HT_TO_LIVE_CROWN = NA,
        CRRATIO = NA,
        AGE = NA,
    # Optional
        STANDPLOT_ID = NA,
        TREE_COUNT = NA,
        HISTORY = NA,
        DIAMETER_HT = NA,
        DG = NA,
        HTG = NA,
        DAMAGE1 = NA,
        SEVERITY1 = NA,
        DAMAGE2 = NA,
        SEVERITY2 = NA,
        DAMAGE3 = NA,
        SEVERITY3 = NA,
        DEFECT_CUBIC = NA,
        DEFECT_BOARD = NA,
        TREEVALUE = NA,
        PRESCRIPTION = NA,
        SLOPE = NA,
        ASPECT = NA,
        PV_CODE = NA,
        PV_REF_CODE = NA,
        TOPOCODE = NA,
        SITEPREP = NA
    ) %>% 
  drop_na(TREE_ID)

# SUMMARIZE TreeInit to create PlotInit (list of plots)

plot <- TreeInit %>% 
  group_by(STAND_ID, PLOT_ID, YEAR) %>% 
  summarize(num_trees = n()) %>% 
  select(STAND_ID, PLOT_ID, YEAR)
plot

num_plot <- plot %>% 
  group_by(STAND_ID, YEAR) %>% 
  summarize(num_plots = n()) %>% 
  select(STAND_ID, YEAR, num_plots)
num_plot

# CREATE make_FVS_PlotInit() function

make_FVS_PlotInit <- function(){
  tibble(
    STAND_ID = NA,	
    PLOT_ID	= NA,
    STANDPLOT_ID = NA,	
    VARIANT	= NA,
    INV_YEAR = NA,	
    GROUPS = NA,
    ADDFILES = NA,
    FVSKEYWORDS = NA,
    GIS_LINK = NA,
    PROJECT_NAME = NA,
    LATITUDE = NA,
    LONGITUDE = NA,
    REGION = NA,
    FOREST = NA,
    DISTRICT = NA,
    COMPARTMENT = NA,
    LOCATION = NA,
    ECOREGION = NA,
    PV_CODE = NA,
    PV_REF_CODE = NA,
    AGE = NA,
    ASPECT = NA,
    SLOPE = NA,
    ELEVATION = NA,
    ELEVFT = NA,
    BASAL_AREA_FACTOR = NA,
    INV_PLOT_SIZE = NA,
    BRK_DBH = NA,
    NUM_PLOTS = NA,
    NONSTK_PLOTS = NA,
    SAM_WT = NA,
    STK_PCNT = NA,
    DG_TRANS = NA,
    DG_MEASURE = NA,
    HTG_TRANS = NA,
    HTG_MEASURE = NA,
    MORT_MEASURE = NA,
    MAX_BA = NA,
    MAX_SDI = NA,
    SITE_SPECIES = NA,
    SITE_INDEX = NA,
    MODEL_TYPE = NA,
    PHYSIO_REGION = NA,
    FOREST_TYPE = NA,
    STATE	= NA,
    COUNTY = NA,
    FUEL_MODEL = NA,
    FUEL_0_25 = NA,
    FUEL_25_1 = NA,
    FUEL_1_3 = NA,
    FUEL_3_6_H = NA,
    FUEL_6_12_H = NA,
    FUEL_12_20_H = NA,
    FUEL_20_35_H = NA,
    FUEL_35_50_H = NA,
    FUEL_GT_50_H = NA,
    FUEL_3_6_S = NA,
    FUEL_6_12_S = NA,
    FUEL_12_20_S = NA,
    FUEL_20_35_S = NA,
    FUEL_35_50_S = NA,
    FUEL_GT_50_S = NA,
    FUEL_LITTER = NA,
    FUEL_DUFF = NA,
    PHOTO_REF = NA,
    PHOTO_CODE = NA)
}

# CREATE blank template for TreeInit sheet

FVS_PlotInit_blank <- make_FVS_PlotInit()


# APPLY example plot records FVS_PlotInit_blank

PlotInit <- FVS_PlotInit_blank %>% 
  add_row(
    # Gotta have these
    STAND_ID = plot$STAND_ID,	
    PLOT_ID	= plot$PLOT_ID,
    STANDPLOT_ID = paste0(STAND_ID, "_", PLOT_ID),	
    VARIANT	= VARIANT,
    INV_YEAR = plot$YEAR,	
    GROUPS = NA,
    ADDFILES = NA,
    FVSKEYWORDS = NA,
    GIS_LINK = NA,
    PROJECT_NAME = NA,
    LATITUDE = NA,
    LONGITUDE = NA,
    REGION = NA,
    FOREST = NA,
    DISTRICT = NA,
    COMPARTMENT = NA,
    LOCATION = LOCATION,
    ECOREGION = NA,
    PV_CODE = NA,
    PV_REF_CODE = NA,
    AGE = NA,
    ASPECT = NA,
    SLOPE = NA,
    ELEVATION = NA,
    ELEVFT = NA,
    BASAL_AREA_FACTOR = BASAL_AREA_FACTOR,
    INV_PLOT_SIZE = NA,
    BRK_DBH = NA,
    NUM_PLOTS = as.numeric(nrow(plot)),
    NONSTK_PLOTS = NA,
    SAM_WT = NA,
    STK_PCNT = NA,
    DG_TRANS = NA,
    DG_MEASURE = NA,
    HTG_TRANS = NA,
    HTG_MEASURE = NA,
    MORT_MEASURE = NA,
    MAX_BA = NA,
    MAX_SDI = NA,
    SITE_SPECIES = NA,
    SITE_INDEX = NA,
    MODEL_TYPE = NA,
    PHYSIO_REGION = NA,
    FOREST_TYPE = NA,
    STATE	= NA,
    COUNTY = NA,
    FUEL_MODEL = NA,
    FUEL_0_25 = NA,
    FUEL_25_1 = NA,
    FUEL_1_3 = NA,
    FUEL_3_6_H = NA,
    FUEL_6_12_H = NA,
    FUEL_12_20_H = NA,
    FUEL_20_35_H = NA,
    FUEL_35_50_H = NA,
    FUEL_GT_50_H = NA,
    FUEL_3_6_S = NA,
    FUEL_6_12_S = NA,
    FUEL_12_20_S = NA,
    FUEL_20_35_S = NA,
    FUEL_35_50_S = NA,
    FUEL_GT_50_S = NA,
    FUEL_LITTER = NA,
    FUEL_DUFF = NA,
    PHOTO_REF = NA,
    PHOTO_CODE = NA) %>% 
  drop_na(PLOT_ID)

PlotInit <- inner_join(PlotInit, num_plot)

PlotInit <- PlotInit %>% 
  select(-c(NUM_PLOTS))

PlotInit <- PlotInit %>%
  rename(NUM_PLOTS = num_plots) %>% 
  relocate(NUM_PLOTS, .after = BRK_DBH)

# Summarize PlotInit to create StandInit (list of stands)

stand <- plot %>% 
  group_by(STAND_ID, YEAR) %>% 
  summarize(num_trees = n()) %>% 
  select(STAND_ID, YEAR)
stand

# CREATE make_FVS_StandInit() function

make_FVS_StandInit <- function(){
  tibble(
    STAND_ID = NA,	
    VARIANT = NA,	
    INV_YEAR = NA,	
    GROUPS = NA,	
    ADDFILES = NA,	
    FVSKEYWORDS = NA,	
    GIS_LINK = NA,	
    PROJECT_NAME = NA,
    LATITUDE = NA,	
    LONGITUDE = NA,	
    REGION = NA,
    FOREST = NA,
    DISTRICT = NA,	
    COMPARTMENT = NA,
    LOCATION	= NA,
    ECOREGION = NA,
    PV_CODE = NA,	
    PV_REF_CODE = NA,
    AGE = NA,
    ASPECT = NA,	
    SLOPE	= NA,
    ELEVATION	= NA,
    ELEVFT = NA,	
    BASAL_AREA_FACTOR = NA,
    INV_PLOT_SIZE = NA,
    BRK_DBH = NA,
    NUM_PLOTS = NA,
    NONSTK_PLOTS = NA,	
    SAM_WT = NA,	
    STK_PCNT = NA,
    DG_TRANS = NA,
    DG_MEASURE = NA,
    HTG_TRANS = NA,
    HTG_MEASURE = NA,	
    MORT_MEASURE = NA,	
    MAX_BA = NA,
    MAX_SDI	= NA,
    SITE_SPECIES = NA,	
    SITE_INDEX = NA,
    MODEL_TYPE = NA,
    PHYSIO_REGION = NA,
    FOREST_TYPE = NA,	
    STATE = NA,
    COUNTY = NA,	
    FUEL_MODEL = NA,
    FUEL_0_25 = NA,	
    FUEL_25_1	= NA,
    FUEL_1_3 = NA,
    FUEL_3_6_H= NA,
    FUEL_6_12_H	= NA,
    FUEL_12_20_H= NA,
    FUEL_20_35_H= NA,
    FUEL_35_50_H= NA,
    FUEL_GT_50_H= NA,
    FUEL_3_6_S= NA,
    FUEL_6_12_S	= NA,
    FUEL_12_20_S = NA,
    FUEL_20_35_S= NA,
    FUEL_35_50_S= NA,
    FUEL_GT_50_S= NA,
    FUEL_LITTER	= NA,
    FUEL_DUFF	= NA,
    PHOTO_REF	= NA,
    PHOTO_CODE = NA)
}

# CREATE blank template for StandInit sheet

FVS_StandInit_blank <- make_FVS_StandInit()

# APPLY example stand records FVS_StandInit_blank

StandInit <- FVS_StandInit_blank %>% 
  add_row(
    STAND_ID = stand$STAND_ID,	
    VARIANT = VARIANT,	
    INV_YEAR = stand$YEAR,	
    GROUPS = NA,	
    ADDFILES = NA,	
    FVSKEYWORDS = NA,	
    GIS_LINK = NA,	
    PROJECT_NAME = NA,
    LATITUDE = NA,	
    LONGITUDE = NA,	
    REGION = NA,
    FOREST = NA,
    DISTRICT = NA,	
    COMPARTMENT = NA,
    LOCATION	= LOCATION,
    ECOREGION = NA,
    PV_CODE = NA,	
    PV_REF_CODE = NA,
    AGE = NA,
    ASPECT = NA,	
    SLOPE	= NA,
    ELEVATION	= NA,
    ELEVFT = NA,	
    BASAL_AREA_FACTOR = BASAL_AREA_FACTOR,
    INV_PLOT_SIZE = NA,
    BRK_DBH = NA,
    NUM_PLOTS = as.numeric(nrow(plot)),
    NONSTK_PLOTS = NA,	
    SAM_WT = NA,	
    STK_PCNT = NA,
    DG_TRANS = NA,
    DG_MEASURE = NA,
    HTG_TRANS = NA,
    HTG_MEASURE = NA,	
    MORT_MEASURE = NA,	
    MAX_BA = NA,
    MAX_SDI	= NA,
    SITE_SPECIES = NA,	
    SITE_INDEX = NA,
    MODEL_TYPE = NA,
    PHYSIO_REGION = NA,
    FOREST_TYPE = NA,	
    STATE = NA,
    COUNTY = NA,	
    FUEL_MODEL = NA,
    FUEL_0_25 = NA,	
    FUEL_25_1	= NA,
    FUEL_1_3 = NA,
    FUEL_3_6_H= NA,
    FUEL_6_12_H	= NA,
    FUEL_12_20_H= NA,
    FUEL_20_35_H= NA,
    FUEL_35_50_H= NA,
    FUEL_GT_50_H= NA,
    FUEL_3_6_S= NA,
    FUEL_6_12_S	= NA,
    FUEL_12_20_S = NA,
    FUEL_20_35_S= NA,
    FUEL_35_50_S= NA,
    FUEL_GT_50_S= NA,
    FUEL_LITTER	= NA,
    FUEL_DUFF	= NA,
    PHOTO_REF	= NA,
    PHOTO_CODE = NA) %>% 
  drop_na(STAND_ID)

StandInit <- inner_join(StandInit, num_plot)

StandInit <- StandInit %>% 
  select(-c(NUM_PLOTS))

StandInit <- StandInit %>%
  rename(NUM_PLOTS = num_plots) %>% 
  relocate(NUM_PLOTS, .after = BRK_DBH)

# Omit YEAR from TreeInit (not needed in FVS file)

TreeInit <- TreeInit %>% 
  select(-YEAR)

# Export FVS files to a single Excel workbook in the FVS-ready format

wb <- createWorkbook() 
addWorksheet(wb = wb, sheetName = "FVS_StandInit") 
writeData(wb = wb, sheet = 1, x = StandInit)

addWorksheet(wb = wb, sheetName = "FVS_PlotInit") 
writeData(wb = wb, sheet = 2, x = PlotInit) 

addWorksheet(wb = wb, sheetName = "FVS_TreeInit") 
writeDataTable(wb = wb, sheet = 3, x = TreeInit) 

saveWorkbook(wb, "my_FVS_ready_file.xlsx", overwrite = TRUE)


