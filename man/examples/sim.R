library(magrittr)
model <- slca(lc1[3] ~ x1 + x2 + x3 + x4 + x5)
sim_data <- model %>% simulate(nlevel = c(3, 3, 3, 3, 3))
y <- sim_data$response
sapply(y, table)

sim_data <- model %>%
   simulate(nlevel = c(x1 = 2, x3 = 3, x4 = 4, x5 = 5))
y <- sim_data$response
sapply(y, table)

model <- slca(lc1[3] ~ x1 + x2 + x3 + x4 + x5,
              lc2[4] ~ y1 + y2 + y3 + y4 + y5)
sim_data <- model %>% simulate(1000)
sapply(sim_data$class, table)
