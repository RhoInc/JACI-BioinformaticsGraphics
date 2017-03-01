# packages
pacman::p_load(tidyverse, rio, scales)
pacman::p_load(magrittr)
pacman::p_load(tibble)
pacman::p_load(flowCore, flowQ, flowViz, flowStats)

# import meta data
md <- import("c:/egg/SDY218-DR20_Subject_2_Flow_cytometry_result.txt", fread=F) %>%
  janitor::clean_names() %>% 
  dplyr::filter(subject_accession %in% c('SUB122181','SUB122223',
                                         'SUB122194','SUB122208')) %>% 
  dplyr::filter(planned_visit_name %in% c("Visit 00 Screening Baseline","Visit 03 ~10 mon (Initial OFC)")) %>% 
  dplyr::filter(file_detail == 'Flow cytometry result in fcs format') %>% 
  dplyr::filter(expsample_treatement %in% c(
    #'Basophil medium w/ 0.001 ug EW',
    'Basophil medium w/ 0.01 ug EW',
    'Basophil medium w/ 0.1 ug EW')) %>% 
  select(subject_accession, arm_name, biosample_accession, study_time_collected,
         planned_visit_accession, planned_visit_name, expsample_treatement,
         file_info_id, file_name, original_file_name) %>% 
  as_tibble

# Import FCS data
setwd("C:/egg/ResultFiles/Flow_cytometry_result")
files <- dir(pattern = "*.fcs")
files <- files

dd <- data_frame(file_name = files) %>% 
  mutate(file_content = map(file_name, ~ read.FCS(.)),
         data  = map(file_content, ~.@exprs %>% data.frame))

# merge both files
df <- left_join(md, dd) %>%
  select(file_name, subject_accession, arm_name, planned_visit_name, expsample_treatement, data) %>% 
  mutate(planned_visit_name = factor(planned_visit_name, labels=c("Baseline","Month 10")))

# Select OIT Subject @ Baseline + 10 mos Egg IT
df1 <- df  %>% 
  slice(c(2,4,10,12))

# Export
export(df1, "C:/egg/anly/egg_flow_data.rds")
