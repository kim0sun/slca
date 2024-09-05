library(magrittr)
f5_var <- slca(
   DLQ1(5) ~ LIEw1 + LOUDw1 + DAMAGEw1 + SHOPLIFTw1 + STEALw1 + FIGHTw1,
   DLQ2(5) ~ LIEw2 + LOUDw2 + DAMAGEw2 + SHOPLIFTw2 + STEALw2 + FIGHTw2,
   DLQ1 ~ DLQ2
) %>% estimate(addhealth[1:300,])

f5_inv <- slca(
   DLQ1(5) ~ LIEw1 + LOUDw1 + DAMAGEw1 + SHOPLIFTw1 + STEALw1 + FIGHTw1,
   DLQ2(5) ~ LIEw2 + LOUDw2 + DAMAGEw2 + SHOPLIFTw2 + STEALw2 + FIGHTw2,
   DLQ1 ~ DLQ2,
   constraints = c("DLQ1", "DLQ2")
) %>% estimate(addhealth[1:300,])

compare(f5_var, f5_inv, test = "chisq")
param(f5_inv)
