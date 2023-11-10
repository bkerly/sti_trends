library(tidyverse)
library(ggthemes)
library(lubridate)

# Grarph annual data-------------
all_sti_annual <- read_csv("data/all_sti.csv")

ggplot(all_sti_annual) +
  geom_line(aes(x=Year,y=Diagnoses))+
  facet_wrap(~disease,3,scales="free_y")

# Graph quarterly data--------------
all_sti_quart <- readxl::read_excel("data/BERequest110723.xlsx") %>%
  filter(!((Year == 2023) & (Quarter == 4)))

# redo dates
all_sti_quart <- all_sti_quart %>%
  mutate(month = Quarter * 3
  ) %>%
  mutate(date = ymd(
    paste0(
      Year,
      "/",
      month,
      "/",
      "1"
    )
  )) 


# pivot long
all_sti_quart_long <- all_sti_quart %>%
  select(-Year,-Quarter,-month) %>%
  pivot_longer(-date,
               names_to = "Disease",
               values_to = "Diagnoses")%>%
  mutate(Year = year(date)) %>%
  # Diagnoses are no longer cumulative, within years which is nice!
  # group_by(Year,Disease) %>%
  # mutate(q.Diagnoses = Diagnoses -lag(Diagnoses,1,default = 0)) %>%
   ungroup() 

ggplot(all_sti_quart_long) +
  geom_line(aes(x=date,y=Diagnoses,color = Disease))

# Cool, now let's do a lm

all_sti_quart_wide <- all_sti_quart_long %>%
  #select(-Diagnoses) %>%
  pivot_wider(names_from=Disease,
              values_from=Diagnoses) %>%
  mutate(Year = year(date))

model <- lm(Gonorrhea ~ Chlamydia + `Syphilis (Primary)` + `Syphilis (Secondary)` + HIV,
            all_sti_quart_wide %>%
     filter(!(Year %in% c(2022,2023)))
)

gon_pred <- all_sti_quart_wide %>%
  mutate(pred_gonorrhea = predict(model,all_sti_quart_wide)) 

ggplot(gon_pred,aes (x = date))+
  geom_line(aes(y=Gonorrhea,color="Actual Gonorrhea Cases"),linetype=1)+
  geom_line(aes(y=pred_gonorrhea,color="Predicted Cases"),linetype=1) + 
  geom_segment(
    aes(x = max(date),xend=max(date),
           y = pred_gonorrhea[date==max(date)],
           yend = Gonorrhea[date==max(date)], color = "Gap"),
    linetype = 3,
    linewidth = .75
  ) +
  theme_fivethirtyeight() + 
  theme(legend.title = element_blank()) + 
  labs(title = "Actual and Predicated Gonorrhea Rates by Quarter",
       subtitle = "LM Based on 2018-2021 Quarterly CT, Syphilis, and HIV Rates")


gon_pred %>%
  ggplot(aes(x=Gonorrhea,y=pred_gonorrhea))+
  geom_point()+
  geom_abline()+
  theme(legend.title = element_blank()) + 
  labs(title = "Predicted vs Actual Gonorrhea Cases",
       subtitle = "LM Based on 2018-2021 Quarterly CT, Syphilis, and HIV Rates")+
  ylab("Predicted Cases")+
  xlab("Actual Cases") +
  ggforce::geom_mark_ellipse(aes(fill = as.factor(Year),
                                 filter = 
                                   Year %in% c(2022,2023),
                                 label = Year,
                                 x0 = 2000)
                                   
                             )+
  theme(legend.position = "none")
   
                             