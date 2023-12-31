library(tidyverse)

# Load
all_syphilis <- read_csv("data/all stages syphilis.csv") %>%
  mutate(disease = "Syphilis")%>%
  filter(Category == "Total",
         Label == "Total")

chlamydia <- read_csv("data/chlamydia.csv")%>%
  mutate(disease = "Chlamydia")%>%
  filter(Category == "Total",
         Label == "Total")

gonorrhea <- read_csv("data/gonorrhea.csv")%>%
  mutate(disease = "Gonorrhea")%>%
  filter(Category == "Total",
         Label == "Total")

# Smush

all_sti_annual <- all_syphilis %>%
  bind_rows(chlamydia) %>%
  bind_rows(gonorrhea)

# write

write_csv(all_sti_annual,file = "data/all_sti_annual.csv")
