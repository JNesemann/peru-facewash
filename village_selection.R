# village selection

# loading packages
library(tidyverse)
library(here)

# loading data
data <- read_csv(here("data","Loreto w photo grades.csv"),
                 guess_max = 3000)
  
# generating village level object with TF prev
villages <- data %>% 
  # grouping by community and district
  group_by(community, district) %>%
  # filtering so we only have kids age 1 to 9
  filter(age<10 & age>=1) %>%
  # filtering to get only balsapuerto district
  filter(district == "Balsapuerto") %>%
  # creating a variable for tf/ti in either eye using field and photo grades
  mutate(od_ti= if_else(od_ti==-999999999, NA_real_,od_ti),
         os_ti= if_else(os_ti==-999999999, NA_real_,os_ti),
         tf=case_when(od_tf==1 | os_tf==1 ~ 1,
                      od_tf==0 & os_tf==0 ~ 0),
         # xtabs(data=villages, ~os_tf+tf, addNA=T)
         ti=case_when(od_ti==1 | os_ti==1 ~ 1,
                      is.na(od_ti) & os_ti==1 ~ 1,
                      is.na(od_ti) & os_ti==0 ~ 0,
                      is.na(os_ti) & od_ti==1 ~ 1,
                      is.na(os_ti) & od_ti==0 ~ 0,
                      od_ti==0 & os_ti==0 ~ 0),
         # xtabs(data=villages, ~od_ti+ti, addNA=T)
         tf.photo=case_when(tf_photo_od==1 & tf_photo_os==1 ~ 1,
                            tf_photo_od==1 & tf_photo_os==0 ~ 1,
                            tf_photo_od==0 & tf_photo_os==1 ~ 1,
                            is.na(tf_photo_od) & tf_photo_os==1 ~ 1,
                            is.na(tf_photo_od) & tf_photo_os==0 ~ 0,
                            is.na(tf_photo_os) & tf_photo_od==1 ~ 1,
                            is.na(tf_photo_os) & tf_photo_od==0 ~ 0,
                            tf_photo_od==0 & tf_photo_os==0 ~ 0,
                            is.na(tf_photo_od) & is.na(tf_photo_os) ~ NA_real_),
         # xtabs(data=villages, ~tf_photo_os+tf.photo, addNA=T)
         ti.photo=case_when(ti_photo_od==1 | ti_photo_os==1 ~ 1,
                            is.na(ti_photo_od) & ti_photo_os==0 ~ 0,
                            is.na(ti_photo_os) & ti_photo_od==0 ~ 0,
                            ti_photo_od==0 & ti_photo_os==0 ~ 0)) %>%
  # xtabs(data=villages, ~ti_photo_od+ti.photo, addNA=T)
  summarise(n=sum(!is.na(unique.id)),
            n_tf=sum(tf==1, na.rm=T),
            p_tf=n_tf/n,
            n_ti=sum(ti==1, na.rm=T),
            p_ti=n_ti/n,
            n_photo_tf=sum(tf.photo==1, na.rm=T),
            p_photo_tf=n_photo_tf/n,
            n_photo_ti=sum(ti.photo==1, na.rm=T),
            p_photo_ti=n_photo_ti/n)

villages

villages %>% 
  ungroup() %>% 
  summarise(min=min(p_tf),
            max=max(p_tf),
            min.photo=min(p_photo_tf),
            max.photo=max(p_photo_tf))

# only one village with tf prevalence <10%
# I think I will just randomly select 6 villages in this case

# setting seed
set.seed(3292247) # date and range of wave heights at Playa Barranquito (4-7ft)

# randomly selecting 6 villages without replacement
selected.villages <- sample(villages$community, # elements from which to choose
                            size = 6, # size of sample
                            replace = FALSE) # sample with replacement?

selected.villages
# [1] "Loma Linda"              
# "San Antonio"              
# "Maranatha"                
# "Bellavista (Balsapuerto)" 
# "Centro America"          
# [6] "Panam" 


