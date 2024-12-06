library(magrittr)
nlsy_jlcpa %>% param

# Reorder the RHO parameters as ascending order
reordered1 <- nlsy_jlcpa %>%
   reorder(SMK_98 = c(1, 3, 2),
           DRK_98 = c(3, 2, 1),
           MRJ_98 = c(3, 1, 2))
reordered1 %>% param
# Label class1: nonuse
#       class2: lifetime use
#       class3: current use

# Reorder the TAU parameters for joint classes as ascending order
reordered2 <- reordered1 %>%
   reorder(SUB_98 = c(3, 4, 5, 1, 2))
reordered2 %>% param
# Label class1: nonuse
#       class2: heavy drinking only
#       class3: not heavy use
#       class4: heavy drinking & smoking
#       class5: heavy use

# Reorder the TAU paramters for profiles as ascending order
reordered3 <- reordered2 %>%
   reorder(PROF = c(4, 1, 3, 2))
reordered3 %>% param
# Label class1: nonuse stayer
#       class2: heavy drinking advancer
#       class3: heavy drk & smk advancer
#       class4: heavy use advancer
