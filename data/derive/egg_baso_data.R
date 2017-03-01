# packages
pacman::p_load(tidyverse, rio)
pacman::p_load(magrittr)
pacman::p_load(tibble)

# Import enroll
er <- import("./data/immport/clinical/CF3_ENROLL.txt") %>% 
  janitor::clean_names() %>% 
  select(subject_accession, trttrue)

# oral food challange
ed <- import("./data/immport/clinical/CF3_OFC.txt") %>%
  janitor::clean_names() %>%
  select(subject_accession, ofc_date, ofcf3vis, chalresu) %>% 
  filter(ofcf3vis %in% c('5g M10 (Desens.)')) %>%
  right_join(er) %>% 
  mutate(group = factor(interaction(trttrue, chalresu), labels=c("OIT Response","OIT No-Response","Placebo")),
         chalresu = factor(chalresu, labels=c("Negative","Positive")))

# basophils
d1 <- import("./data/immport/clinical/CF3_BAS.txt") %>% 
  janitor::clean_names() %>%
  filter(visno %in% c(0,3)) %>%
  left_join(ed) %>% 
  select(subject_accession, trttrue, chalresu, group, visno,contains("CD63POS")) %>% 
  mutate_at(-c(1:5),funs(as.numeric(.))) %>% 
  mutate(trttrue = factor(trttrue),
            visno = factor(visno, label = c("Baseline","Month 10")))

# impute
pacman::p_load(missForest)

d2 <- d1 %>% select(-c(1:5)) %>% data.matrix %>% 
  missForest( ntree = 500, maxiter=100) %>% 
  extract2("ximp") %>% 
  as.data.frame

d2 <- bind_cols(d1 %>% select(c(1:5)),
                d2) %>% 
  gather(variable,value,-c(1:5))

# labels
label <- tribble(
  ~variable,  ~label, 
  'CD63NEG1','Antigen1 CD63neg' ,
  'C63N63M1','Antigen1 CD63neg_CD63_MFI',
  'C63N63C1','Antigen1 CD63neg_CD63_CV', 
  'C63N63S1','Antigen1 CD63neg_CD63_SD', 
  'C63N20M1','Antign1 CD63neg_CD203_MFI',
  'C63N20S1','Antigen1 CD63neg_CD203_SD',
  'CD63NNT1','Antigen1 CD63neg_COUNT', 
  'CD63NEG2','Antigen2 CD63neg' ,
  'C63N63M2','Antigen2 CD63neg_CD63_MFI',
  'C63N63C2','Antigen2 CD63neg_CD63_CV', 
  'C63N63S2','Antigen2 CD63neg_CD63_SD', 
  'C63N20M2','Antign2 CD63neg_CD203_MFI',
  'C63N20S2','Antigen2 CD63neg_CD203_SD',
  'CD63NNT2','Antigen2 CD63neg_COUNT', 
  'CD63NEG3','Antigen3 CD63neg', 
  'C63N63M3','Antigen3 CD63neg_CD63_MFI',
  'C63N63C3','Antigen3 CD63neg_CD63_CV', 
  'C63N63S3','Antigen3 CD63neg_CD63_SD', 
  'C63N20M3','Antign3  CD63neg_CD203_MFI',
  'C63N20S3','Antigen3 CD63neg_CD203_SD',
  'CD63NNT3','Antigen3 CD63neg_COUNT', 
  'CD63NEG4','Antigen4 CD63neg', 
  'C63N63M4','Antigen4 CD63neg_CD63_MFI',
  'C63N63C4','Antigen4 CD63neg_CD63_CV', 
  'C63N63S4','Antigen4 CD63neg_CD63_SD', 
  'C63N20M4','Antign4 CD63neg_CD203_MFI',
  'C63N20S4','Antigen4 CD63neg_CD203_SD',
  'CD63NNT4','Antigen4 CD63neg_COUNT', 
  'CD63NEG5','IL-3 CD63neg', 
  'C63N63M5','IL-3 CD63neg_CD63_MFI', 
  'C63N63C5','IL-3 CD63neg_CD63_CV', 
  'C63N63S5','IL-3 CD63neg_CD63_SD', 
  'C63N20M5','IL-3 CD63neg_CD203_MFI', 
  'C63N20S5','IL-3 CD63neg_CD203_SD', 
  'CD63NNT5','IL-3 CD63neg_COUNT', 
  'CD63NEG6','Medium CD63neg', 
  'C63N63M6','Medium CD63neg_CD63_MFI', 
  'C63N63C6','Medium CD63neg_CD63_CV', 
  'C63N63S6','Medium CD63neg_CD63_SD', 
  'C63N20M6','Medium CD63neg_CD203_MFI', 
  'C63N20S6','Medium CD63neg_CD203_SD', 
  'CD63NNT6','Medium CD63neg_COUNT', 
  'CD63NEG7','Unstain CD63neg', 
  'C63N63M7','Unstain CD63neg_CD63_MFI', 
  'C63N63C7','Unstain CD63neg_CD63_CV', 
  'C63N63S7','Unstain CD63neg_CD63_SD', 
  'C63N20M7','Unstain CD63neg_CD203_MFI',
  'C63N20S7','Unstain CD63neg_CD203_SD', 
  'CD63NNT7','Unstain CD63neg_COUNT', 
  'CD63NEG8','antiIGE CD63neg', 
  'C63N63M8','antiIGE CD63neg_CD63_MFI', 
  'C63N63C8','antiIGE CD63neg_CD63_CV', 
  'C63N63S8','antiIGE CD63neg_CD63_SD', 
  'C63N20M8','antiIGE CD63neg_CD203_MFI',
  'C63N20S8','antiIGE CD63neg_CD203_SD', 
  'CD63NNT8','antiIGE CD63neg_COUNT', 
  'CD63NEG9','fMLP CD63neg', 
  'C63N63M9','fMLP CD63neg_CD63_MFI', 
  'C63N63C9','fMLP CD63neg_CD63_CV', 
  'C63N63S9','fMLP CD63neg_CD63_SD', 
  'C63N20M9','fMLP CD63neg_CD203_MFI', 
  'C63N20S9','fMLP CD63neg_CD203_SD', 
  'CD63NNT9','fMLP CD63neg_COUNT', 
  'CD63POS1','Antigen1 CD63pos', 
  'C63P63M1','Antigen1 CD63pos_CD63_MFI',
  'C63P63C1','Antigen1 CD63pos_CD63_CV', 
  'C63P63S1','Antigen1 CD63pos_CD63_SD', 
  'C63P20M1','Antign1 CD63pos_CD203_MFI',
  'C63P20S1','Antigen1 CD63pos_CD203_SD',
  'CD63PNT1','Antigen1 CD63pos_COUNT', 
  'CD63POS2','Antigen2 CD63pos', 
  'C63P63M2','Antigen2 CD63pos_CD63_MFI',
  'C63P63C2','Antigen2 CD63pos_CD63_CV', 
  'C63P63S2','Antigen2 CD63pos_CD63_SD', 
  'C63P20M2','Antign2 CD63pos_CD203_MFI',
  'C63P20S2','Antigen2 CD63pos_CD203_SD',
  'CD63PNT2','Antigen2 CD63pos_COUNT', 
  'CD63POS3','Antigen3 CD63pos', 
  'C63P63M3','Antigen3 CD63pos_CD63_MFI',
  'C63P63C3','Antigen3 CD63pos_CD63_CV', 
  'C63P63S3','Antigen3 CD63pos_CD63_SD', 
  'C63P20M3','Antign3 CD63pos_CD203_MFI',
  'C63P20S3','Antigen3 CD63pos_CD203_SD',
  'CD63PNT3','Antigen3 CD63pos_COUNT', 
  'CD63POS4','Antigen4 CD63pos', 
  'C63P63M4','Antigen4 CD63pos_CD63_MFI',
  'C63P63C4','Antigen4 CD63pos_CD63_CV', 
  'C63P63S4','Antigen4 CD63pos_CD63_SD', 
  'C63P20M4','Antign4 CD63pos_CD203_MFI',
  'C63P20S4','Antigen4 CD63pos_CD203_SD',
  'CD63PNT4','Antigen4 CD63pos_COUNT', 
  'CD63POS5','IL-3 CD63pos', 
  'C63P63M5','IL-3 CD63pos_CD63_MFI', 
  'C63P63C5','IL-3 CD63pos_CD63_CV', 
  'C63P63S5','IL-3 CD63pos_CD63_SD', 
  'C63P20M5','IL-3 CD63pos_CD203_MFI', 
  'C63P20S5','IL-3 CD63pos_CD203_SD', 
  'CD63PNT5','IL-3 CD63pos_COUNT', 
  'CD63POS6','Medium CD63pos', 
  'C63P63M6','Medium CD63pos_CD63_MFI', 
  'C63P63C6','Medium CD63pos_CD63_CV', 
  'C63P63S6','Medium CD63pos_CD63_SD', 
  'C63P20M6','Medium CD63pos_CD203_MFI', 
  'C63P20S6','Medium CD63pos_CD203_SD', 
  'CD63PNT6','Medium CD63pos_COUNT', 
  'CD63POS7','Unstain CD63pos', 
  'C63P63M7','Unstain CD63pos_CD63_MFI', 
  'C63P63C7','Unstain CD63pos_CD63_CV', 
  'C63P63S7','Unstain CD63pos_CD63_SD', 
  'C63P20M7','Unstain CD63pos_CD203_MFI',
  'C63P20S7','Unstain CD63pos_CD203_SD', 
  'CD63PNT7','Unstain CD63pos_COUNT', 
  'CD63POS8','antiIGE CD63pos', 
  'C63P63M8','antiIGE CD63pos_CD63_MFI', 
  'C63P63C8','antiIGE CD63pos_CD63_CV', 
  'C63P63S8','antiIGE CD63pos_CD63_SD', 
  'C63P20M8','antiIGE CD63pos_CD203_MFI',
  'C63P20S8','antiIGE CD63pos_CD203_SD' ,
  'CD63PNT8','antiIGE CD63pos_COUNT' ,
  'CD63POS9','fMLP CD63pos' ,
  'C63P63M9','fMLP CD63pos_CD63_MFI' ,
  'C63P63C9','fMLP CD63pos_CD63_CV' ,
  'C63P63S9','fMLP CD63pos_CD63_SD' ,
  'C63P20M9','fMLP CD63pos_CD203_MFI' ,
  'C63P20S9','fMLP CD63pos_CD203_SD',
  'CD63PNT9','fMLP CD63pos_COUNT')

label$variable <- tolower(label$variable)

# merge with labels
d2 <- d2 %>% left_join(label)%>% 
  mutate(label = factor(label,label=c("1 mcg/mL Egg CD63pos",
                                      "0.1  mcg/mL Egg CD63pos",
                                      "0.01  mcg/mL Egg CD63pos",
                                      "0.001  mcg/mL Egg CD63pos",
                                      "antiIGE CD63pos",
                                      "fMLP CD63pos",
                                      "IL-3 CD63pos",
                                      "Medium CD63pos", 
                                      "Unstain CD63pos")))

# export
export(d2, "./data/derive/egg_baso_data.csv")
export(d2, "./data/derive/egg_baso_data.rds")