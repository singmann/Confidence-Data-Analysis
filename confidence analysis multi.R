library(tidyverse)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data_path <- "confidence data"

all_dat <- list.files(data_path, full.names = TRUE, pattern = "^data")
all_names <- list.files(data_path, full.names = FALSE, pattern = "^data") |>
  str_remove("^data_") |>
  str_remove(".csv$")

##----------------------------------------------------------------
##                        Adler_2018_Expt1 - Cognitive             -
##----------------------------------------------------------------
adler_2018_ex1<- 1
adler_2018_ex1 <- read_csv(all_dat[adler_2018_ex1]) |> 
  mutate(dataset = all_names[adler_2018_ex1])

#wide format
adler_2018_ex1<- adler_2018_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Task == "A"~"1",
    Task == "B"~"2"))

adler_2018_ex1_wide <- adler_2018_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Task A #######
adler_2018_ex1_A<-adler_2018_ex1 |> 
  filter(Task == "A")

#cumulative probs
adler_2018_ex1_A_plot<- adler_2018_ex1_A |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "A") 

###### Task B #######
adler_2018_ex1_B<-adler_2018_ex1 |> 
  filter(Task == "B")

#cumulative probs
adler_2018_ex1_B_plot<- adler_2018_ex1_B |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "B")

adler_2018_ex1_plots<- bind_rows(adler_2018_ex1_A_plot, adler_2018_ex1_B_plot)

ggplot(adler_2018_ex1_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Adler_2018_Expt2 - Cognitive             -
##----------------------------------------------------------------
adler_2018_ex2<- 2
adler_2018_ex2 <- read_csv(all_dat[adler_2018_ex2]) |> 
  mutate(dataset = all_names[adler_2018_ex2])

#4-point scale + wide format
adler_2018_ex2<- adler_2018_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Task == "A"~"1",
    Task == "B"~"2"))

adler_2018_ex2_wide <- adler_2018_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Task A #######
adler_2018_ex2_A<-adler_2018_ex2 |> 
  filter(Task == "A")

#cumulative probs
adler_2018_ex2_A_plot<- adler_2018_ex2_A |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "A") 

###### Task B #######
adler_2018_ex2_B<-adler_2018_ex2 |> 
  filter(Task == "B")

#cumulative probs
adler_2018_ex2_B_plot<- adler_2018_ex2_B |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "B")

adler_2018_ex2_plots<- bind_rows(adler_2018_ex2_A_plot, adler_2018_ex2_B_plot)

ggplot(adler_2018_ex2_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Bang_2019_Exp1 - Perception             -
##----------------------------------------------------------------
bang_2019_exp1<- 13
bang_2019_exp1 <- read_csv(all_dat[bang_2019_exp1]) |> 
  mutate(dataset = all_names[bang_2019_exp1])

#4-point scale + wide format
bang_2019_exp1<- bang_2019_exp1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

bang_2019_exp1_wide <- bang_2019_exp1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Condition 1 #######
bang_2019_exp1_cond1<-bang_2019_exp1 |> 
  filter(Condition == 1)

#cumulative probs
bang_2019_exp1_cond1_plot<- bang_2019_exp1_cond1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1") 

###### Condition 2 #######
bang_2019_exp1_cond2<-bang_2019_exp1 |> 
  filter(Condition == 2)

#cumulative probs
bang_2019_exp1_cond2_plot<- bang_2019_exp1_cond2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "2")

###### Condition 3 #######
bang_2019_exp1_cond3<-bang_2019_exp1 |> 
  filter(Condition == 3)

#cumulative probs
bang_2019_exp1_cond3_plot<- bang_2019_exp1_cond3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "3")

bang_2019_ex1_plots<- bind_rows(bang_2019_exp1_cond1_plot, bang_2019_exp1_cond2_plot,
                                bang_2019_exp1_cond3_plot)

ggplot(bang_2019_ex1_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Bang_2019_Exp2 - Perception             -
##----------------------------------------------------------------
bang_2019_exp2<- 14
bang_2019_exp2 <- read_csv(all_dat[bang_2019_exp2]) |> 
  mutate(dataset = all_names[bang_2019_exp2])

#4-point scale + wide format
bang_2019_exp2<- bang_2019_exp2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Task == "1"~"1",
    Task == "2"~"2"))
  

bang_2019_exp2_wide <- bang_2019_exp2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Task 1 (coarse discrimination) #######
bang_2019_exp2_task1<-bang_2019_exp2 |> 
  filter(Task == 1)

#cumulative probs
bang_2019_exp2_task1_plot<- bang_2019_exp2_task1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1") 

###### Task 2 (fine discrimination) #######
bang_2019_exp2_task2<-bang_2019_exp2 |> 
  filter(Task == 2)

#cumulative probs
bang_2019_exp2_task2_plot<- bang_2019_exp2_task2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "2")

bang_2019_ex2_plots<- bind_rows(bang_2019_exp2_task1_plot, bang_2019_exp2_task2_plot)

ggplot(bang_2019_ex2_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Clark_2018 - Perception             -
##----------------------------------------------------------------
clark_2018<- 20
clark_2018 <- read_csv(all_dat[clark_2018]) |> 
  mutate(dataset = all_names[clark_2018]) |>
  filter(!is.na(Confidence))

#2-point scale + wide format
clark_2018<- clark_2018 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 3 - Confidence,
    resp ==  "signal" ~ 2 + Confidence
  )) |>
  mutate(Condition = case_when(
    Task == "2-stage binary/uncertainty"~"1",
    Task == "2-stage trinary/binary"~"2",
    Task == "1-stage binary/uncertainty"~"3"))

clark_2018_wide <- clark_2018 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Task 1 #######
clark_2018_task1<-clark_2018 |> 
  filter(Task == "2-stage binary/uncertainty")

#cumulative probs
clark_2018_task1_plot<- clark_2018_task1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1") 

###### Task 2 #######
clark_2018_task2<-clark_2018 |> 
  filter(Task == "2-stage trinary/binary")

#cumulative probs
clark_2018_task2_plot<- clark_2018_task2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "2")

###### Task 3 #######
clark_2018_task3<-clark_2018 |> 
  filter(Task == "1-stage binary/uncertainty")

#cumulative probs
clark_2018_task3_plot<- clark_2018_task3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "3")

clark_2018_plots<- bind_rows(clark_2018_task1_plot, clark_2018_task2_plot,
                                clark_2018_task3_plot)

ggplot(clark_2018_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Denison_2018 - Perception & Cognitive            -
##----------------------------------------------------------------
denison_2018<- 23
denison_2018 <- read_csv(all_dat[denison_2018]) |> 
  mutate(dataset = all_names[denison_2018])

#4-point scale + wide format
denison_2018<- denison_2018 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Difficulty == "1"~"1",
    Difficulty == "2"~"2",
    Difficulty == "3"~"3"))

denison_2018_wide <- denison_2018 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Valid condition #######
denison_2018_1<-denison_2018 |> 
  filter(Difficulty == 1)

#cumulative probs
denison_2018_1_plot<- denison_2018_1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "1") 

###### Neutral condition #######
denison_2018_2<-denison_2018 |> 
  filter(Difficulty == 2)

#cumulative probs
denison_2018_2_plot<- denison_2018_2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "2")

###### Invalid condition #######
denison_2018_3<-denison_2018 |> 
  filter(Difficulty == 3)

#cumulative probs
denison_2018_3_plot<- denison_2018_3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "3")

denison_2018_plots<- bind_rows(denison_2018_1_plot, denison_2018_2_plot,
                             denison_2018_3_plot)

ggplot(denison_2018_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Desender_2022_Ex1 - Perception             -
##----------------------------------------------------------------
desender_2022_ex1<- 30
desender_2022_ex1 <- read_csv(all_dat[desender_2022_ex1]) |> 
  mutate(dataset = all_names[desender_2022_ex1]) |>
  filter(!is.na(Confidence))

#6-point scale + wide format
desender_2022_ex1<- desender_2022_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  )) 

desender_2022_ex1_wide <- desender_2022_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Speed condition #######
desender_2022_ex1_speed<-desender_2022_ex1 |> 
  filter(Condition == "speed")

#cumulative probs
desender_2022_ex1_speed_plot<- desender_2022_ex1_speed |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "speed") 

###### Accuracy condition #######
desender_2022_ex1_accuracy<-desender_2022_ex1 |> 
  filter(Condition == "accuracy")

#cumulative probs
desender_2022_ex1_accuracy_plot<- desender_2022_ex1_accuracy |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "accuracy")

desender_2022_ex1_plots<- bind_rows(desender_2022_ex1_accuracy_plot,
                                    desender_2022_ex1_speed_plot)

ggplot(desender_2022_ex1_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Haddara_2022_Ex1 - Perception             -
##----------------------------------------------------------------
haddara_2022_ex1<- 51
haddara_2022_ex1 <- read_csv(all_dat[haddara_2022_ex1]) |> 
  mutate(dataset = all_names[haddara_2022_ex1])

#4-point scale + wide format
haddara_2022_ex1<- haddara_2022_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Task == "1"~"1",
    Task == "2"~"2"))

haddara_2022_ex1_wide <- haddara_2022_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### Task 1 #######
haddara_2022_ex1_task1<-haddara_2022_ex1 |> 
  filter(Task == 1)

#cumulative probs
haddara_2022_ex1_task1_plot<- haddara_2022_ex1_task1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1") 

###### Task 2 #######
haddara_2022_ex1_task2<-haddara_2022_ex1 |> 
  filter(Task == 2)

#cumulative probs
haddara_2022_ex1_task2_plot<- haddara_2022_ex1_task2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "2") 

haddara_2022_ex1_plots<- bind_rows(haddara_2022_ex1_task1_plot,
                                    haddara_2022_ex1_task2_plot)

ggplot(haddara_2022_ex1_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Haddara_2022_Ex2 - Perception             -
##----------------------------------------------------------------
haddara_2022_ex2<- 52
haddara_2022_ex2 <- read_csv(all_dat[haddara_2022_ex2]) |> 
  mutate(dataset = all_names[haddara_2022_ex2])

#4-point scale + wide format
haddara_2022_ex2<- haddara_2022_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    Feedback == "1"~"1",
    Feedback == "0"~"2"))

haddara_2022_ex2_wide <- haddara_2022_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### feedback condition #######
haddara_2022_ex2_f<-haddara_2022_ex2 |> 
  filter(Feedback == 1)

#cumulative probs
haddara_2022_ex2_f_plot<- haddara_2022_ex2_f |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1") 

###### no feedback condition #######
haddara_2022_ex2_nf<-haddara_2022_ex2 |> 
  filter(Feedback == 0)

#cumulative probs
haddara_2022_ex2_nf_plot<- haddara_2022_ex2_nf |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "0") 

haddara_2022_ex2_plots<- bind_rows(haddara_2022_ex2_f_plot,
                                   haddara_2022_ex2_nf_plot)

ggplot(haddara_2022_ex2_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Mazor_2020 - Perception             -
##----------------------------------------------------------------
mazor_2020<- 95
mazor_2020 <- read_csv(all_dat[mazor_2020]) |> 
  mutate(dataset = all_names[mazor_2020]) |>
  filter(!is.na(Confidence))

#6-point scale + wide format
mazor_2020<- mazor_2020 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  ))

mazor_2020_wide <- mazor_2020 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### discrimination condition #######
mazor_2020_disc<-mazor_2020 |> 
  filter(Condition == "Discrimination")

#cumulative probs
mazor_2020_disc_plot<- mazor_2020_disc |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "discrimination") 

###### detection condition #######
mazor_2020_detect<-mazor_2020 |> 
  filter(Condition == "Detection")

#cumulative probs
mazor_2020_detect_plot<- mazor_2020_detect |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "detection")

mazor_2020_plots<- bind_rows(mazor_2020_disc_plot,
                                   mazor_2020_detect_plot)

ggplot(mazor_2020_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Rausch_2020 - Perception             -
##----------------------------------------------------------------
rausch_2020<- 121
rausch_2020 <- read_csv(all_dat[rausch_2020]) |> 
  mutate(dataset = all_names[rausch_2020]) 

#4-point scale + wide format
rausch_2020<- rausch_2020 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    SOA == "16.7"~"1",
    SOA == "33.3"~"2",
    SOA == "66.7"~"3"))

rausch_2020_wide <- rausch_2020 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  ) |>
  filter(!is.na(Condition))

###### SOA 16.7 #######
rausch_2020_soa1<-rausch_2020 |> 
  filter(SOA == 16.7)

#cumulative probs
rausch_2020_soa1_plot<- rausch_2020_soa1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "16.7")

###### SOA 33.3 #######
rausch_2020_soa2<-rausch_2020 |> 
  filter(SOA == 33.3)

#cumulative probs
rausch_2020_soa2_plot<- rausch_2020_soa2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "33.3")

###### SOA 66.7 #######
rausch_2020_soa3<-rausch_2020 |> 
  filter(SOA == 66.7)

#cumulative probs
rausch_2020_soa3_plot<- rausch_2020_soa3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "66.7")


rausch_2020_plots<- bind_rows(rausch_2020_soa1_plot,
                             rausch_2020_soa2_plot,
                             rausch_2020_soa3_plot)

ggplot(rausch_2020_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Sadeghi_2017_Memory              -
##----------------------------------------------------------------
sadeghi_2017_memory<- 129
sadeghi_2017_memory <- read_csv(all_dat[sadeghi_2017_memory]) |> 
  mutate(dataset = all_names[sadeghi_2017_memory])

#6-point scale + wide format
sadeghi_2017_memory<- sadeghi_2017_memory |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  )) |>
  mutate(Condition = case_when(
    group == "patient"~"1",
    group == "control"~"2"))

sadeghi_2017_memory_wide <- sadeghi_2017_memory |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### patient group #######
sadeghi_2017_memory_p<-sadeghi_2017_memory |> 
  filter(group == "patient")

#cumulative probs
sadeghi_2017_memory_p_plot<- sadeghi_2017_memory_p |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "patient")

###### control group #######
sadeghi_2017_memory_c<-sadeghi_2017_memory |> 
  filter(group == "control")

#cumulative probs
sadeghi_2017_memory_c_plot<- sadeghi_2017_memory_c |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "control")

sadeghi_2017_memory_plots<- bind_rows(sadeghi_2017_memory_p_plot,
                              sadeghi_2017_memory_c_plot)

ggplot(sadeghi_2017_memory_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Sadeghi_2017_Perception              -
##----------------------------------------------------------------
sadeghi_2017_perception<- 130
sadeghi_2017_perception <- read_csv(all_dat[sadeghi_2017_perception]) |> 
  mutate(dataset = all_names[sadeghi_2017_perception])

#6-point scale + wide format
sadeghi_2017_perception<- sadeghi_2017_perception |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  )) |>
  mutate(Condition = case_when(
    group == "patient"~"1",
    group == "control"~"2"))

sadeghi_2017_perception_wide <- sadeghi_2017_perception |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### patient group #######
sadeghi_2017_perception_p<-sadeghi_2017_perception |> 
  filter(group == "patient")

#cumulative probs
sadeghi_2017_perception_p_plot<- sadeghi_2017_perception_p |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "patient")

###### control group #######
sadeghi_2017_perception_c<-sadeghi_2017_perception |> 
  filter(group == "control")

#cumulative probs
sadeghi_2017_perception_c_plot<- sadeghi_2017_perception_c |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "control")

sadeghi_2017_perception_plots<- bind_rows(sadeghi_2017_perception_p_plot,
                                      sadeghi_2017_perception_c_plot)

ggplot(sadeghi_2017_perception_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Shekhar_2018 - Perception             -
##----------------------------------------------------------------
shekhar_2018<- 139
shekhar_2018 <- read_csv(all_dat[shekhar_2018]) |> 
  mutate(dataset = all_names[shekhar_2018]) 

#4-point scale + wide format
shekhar_2018<- shekhar_2018 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  )) |>
  mutate(Condition = case_when(
    TMSsite == "1"~"1",
    TMSsite == "2"~"2",
    TMSsite == "3"~"3"))

shekhar_2018_wide <- shekhar_2018 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### S1 #######
shekhar_2018_s1<-shekhar_2018 |> 
  filter(TMSsite == 1)

#cumulative probs
shekhar_2018_s1_plot<- shekhar_2018_s1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "S1")

###### DLPFC #######
shekhar_2018_dlpfc<-shekhar_2018 |> 
  filter(TMSsite == 2)

#cumulative probs
shekhar_2018_dlpfc_plot<- shekhar_2018_dlpfc |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "DLPFC")

###### aPFC #######
shekhar_2018_apfc<-shekhar_2018 |> 
  filter(TMSsite == 3)

#cumulative probs
shekhar_2018_apfc_plot<- shekhar_2018_apfc |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "aPFC")

shekhar_2018_plots<- bind_rows(shekhar_2018_s1_plot,
                              shekhar_2018_dlpfc_plot,
                              shekhar_2018_apfc_plot)

ggplot(shekhar_2018_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Sherman_2016_JOCN - Perception             -
##----------------------------------------------------------------
sherman_2016_jocn<- 142
sherman_2016_jocn <- read_csv(all_dat[sherman_2016_jocn]) |> 
  mutate(dataset = all_names[sherman_2016_jocn]) 

#4-point scale + wide format
sherman_2016_jocn<- sherman_2016_jocn |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

sherman_2016_jocn_wide <- sherman_2016_jocn |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### condition 1: full attention, expect target absent #######
sherman_2016_jocn_1<-sherman_2016_jocn |> 
  filter(Condition == 1)

#cumulative probs
sherman_2016_jocn_1_plot<- sherman_2016_jocn_1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "1")

###### condition 2: full attention, expect target present #######
sherman_2016_jocn_2<-sherman_2016_jocn |> 
  filter(Condition == 2)

#cumulative probs
sherman_2016_jocn_2_plot<- sherman_2016_jocn_2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "2")

###### condition 3: diverted attention, expect target absent #######
sherman_2016_jocn_3<-sherman_2016_jocn |> 
  filter(Condition == 3)

#cumulative probs
sherman_2016_jocn_3_plot<- sherman_2016_jocn_3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "3")

###### condition 4: diverted attention, expect target present #######
sherman_2016_jocn_4<-sherman_2016_jocn |> 
  filter(Condition == 4)

#cumulative probs
sherman_2016_jocn_4_plot<- sherman_2016_jocn_4 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "4")

sherman_2016_jocn_plots<- bind_rows(sherman_2016_jocn_1_plot,
                               sherman_2016_jocn_2_plot,
                               sherman_2016_jocn_3_plot,
                               sherman_2016_jocn_4_plot)

ggplot(sherman_2016_jocn_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Siedlecka_2016 - Cognitive             -
##----------------------------------------------------------------
siedlecka_2016<- 145
siedlecka_2016 <- read_csv(all_dat[siedlecka_2016]) |> 
  mutate(dataset = all_names[siedlecka_2016]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale + wide format
siedlecka_2016<- siedlecka_2016 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

siedlecka_2016_wide <- siedlecka_2016 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### condition 1: MtD #######
siedlecka_2016_1<-siedlecka_2016 |> 
  filter(Condition == 1)

#cumulative probs
siedlecka_2016_1_plot<- siedlecka_2016_1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "1")

###### condition 2: tDM #######
siedlecka_2016_2<-siedlecka_2016 |> 
  filter(Condition == 2)

#cumulative probs
siedlecka_2016_2_plot<- siedlecka_2016_2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "2")

###### condition 3: tMD #######
siedlecka_2016_3<-siedlecka_2016 |> 
  filter(Condition == 3)

#cumulative probs
siedlecka_2016_3_plot<- siedlecka_2016_3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "3")

siedlecka_2016_plots<- bind_rows(siedlecka_2016_1_plot,
                                    siedlecka_2016_2_plot,
                                    siedlecka_2016_3_plot)

ggplot(siedlecka_2016_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Siedlecka_2019_Ex1 - Memory             -
##----------------------------------------------------------------
siedlecka_2019_1<- 148
siedlecka_2019_1 <- read_csv(all_dat[siedlecka_2019_1]) |> 
  mutate(dataset = all_names[siedlecka_2019_1]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale + wide format
siedlecka_2019_1<- siedlecka_2019_1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

siedlecka_2019_1_wide <- siedlecka_2019_1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### condition 0: response first #######
siedlecka_2019_1_resp<-siedlecka_2019_1 |> 
  filter(Condition == 0)

#cumulative probs
siedlecka_2019_1_resp_plot<- siedlecka_2019_1_resp |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "0")

###### condition 1: rating first #######
siedlecka_2019_1_conf<-siedlecka_2019_1 |> 
  filter(Condition == 1)

#cumulative probs
siedlecka_2019_1_conf_plot<- siedlecka_2019_1_conf |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "1")

siedlecka_2019_1_plots<- bind_rows(siedlecka_2019_1_resp_plot,
                                 siedlecka_2019_1_conf_plot)

ggplot(siedlecka_2019_1_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Siedlecka_2019_Ex2 - Memory             -
##----------------------------------------------------------------
siedlecka_2019_2<- 149
siedlecka_2019_2 <- read_csv(all_dat[siedlecka_2019_2]) |> 
  mutate(dataset = all_names[siedlecka_2019_2]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale + wide format
siedlecka_2019_2<- siedlecka_2019_2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

siedlecka_2019_2_wide <- siedlecka_2019_2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### condition 0: decision first #######
siedlecka_2019_2_dec<-siedlecka_2019_2 |> 
  filter(Condition == 0)

#cumulative probs
siedlecka_2019_2_dec_plot<- siedlecka_2019_2_dec |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "0")

###### condition 1: rating first #######
siedlecka_2019_2_conf<-siedlecka_2019_2 |> 
  filter(Condition == 1)

#cumulative probs
siedlecka_2019_2_conf_plot<- siedlecka_2019_2_conf |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "1")

siedlecka_2019_2_plots<- bind_rows(siedlecka_2019_2_dec_plot,
                                   siedlecka_2019_2_conf_plot)

ggplot(siedlecka_2019_2_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Skora_2016 - Memory             -
##----------------------------------------------------------------
skora_2016<- 153
skora_2016 <- read_csv(all_dat[skora_2016]) |> 
  mutate(dataset = all_names[skora_2016]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale + wide format
skora_2016<- skora_2016 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

skora_2016_wide <- skora_2016 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

###### condition FM: fragile memory (1000ms) #######
skora_2016_fm<-skora_2016 |> 
  filter(Condition == "FM")

#cumulative probs
skora_2016_fm_plot<- skora_2016_fm |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "FM")

###### condition IM: iconic memory (50ms) #######
skora_2016_im<-skora_2016 |> 
  filter(Condition == "IM")

#cumulative probs
skora_2016_im_plot<- skora_2016_im |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "IM")

###### condition WM: working memory (1000ms after) #######
skora_2016_wm<-skora_2016 |> 
  filter(Condition == "WM")

#cumulative probs
skora_2016_wm_plot<- skora_2016_wm |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "WM")

skora_2016_plots<- bind_rows(skora_2016_fm_plot,
                                   skora_2016_im_plot,
                             skora_2016_wm_plot)

ggplot(skora_2016_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        **Xu_2019_Ex1 - Cognitive & Memory          -
##----------------------------------------------------------------
xu_2019_ex1<- 167
xu_2019_ex1<- read_csv(all_dat[xu_2019_ex1]) |> 
  mutate(dataset = all_names[xu_2019_ex1]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale
xu_2019_ex1<- xu_2019_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

##### cognitive task ####
xu_2019_ex1_c<- xu_2019_ex1 |> filter(Task=="C")

xu_2019_ex1_c_wide <- xu_2019_ex1_c |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#congruent condition
xu_2019_ex1_c_congruent<-xu_2019_ex1_c |> 
  filter(Condition == "Congruent")

#cumulative probs
xu_2019_ex1_c_congruent_plot<- xu_2019_ex1_c_congruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "congruent")

#incongruent condition
xu_2019_ex1_c_incongruent<-xu_2019_ex1_c |> 
  filter(Condition == "Incongruent")

#cumulative probs
xu_2019_ex1_c_incongruent_plot<- xu_2019_ex1_c_incongruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "incongruent")

xu_2019_ex1_c_plots<- bind_rows(xu_2019_ex1_c_congruent_plot,
                                xu_2019_ex1_c_incongruent_plot)

ggplot(xu_2019_ex1_c_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


#### memory task ####
xu_2019_ex1_m<- xu_2019_ex1 |> filter(Task=="N")

xu_2019_ex1_m_wide <- xu_2019_ex1_m |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#congruent condition
xu_2019_ex1_m_congruent<-xu_2019_ex1_m |> 
  filter(Condition == "Congruent")

#cumulative probs
xu_2019_ex1_m_congruent_plot<- xu_2019_ex1_m_congruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "congruent")

#incongruent condition
xu_2019_ex1_m_incongruent<-xu_2019_ex1_m |> 
  filter(Condition == "Incongruent")

#cumulative probs
xu_2019_ex1_m_incongruent_plot<- xu_2019_ex1_m_incongruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "incongruent")

xu_2019_ex1_m_plots<- bind_rows(xu_2019_ex1_m_congruent_plot,
                                xu_2019_ex1_m_incongruent_plot)

ggplot(xu_2019_ex1_m_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        **Xu_2019_Ex2 - Cognitive & Memory          -
##----------------------------------------------------------------
xu_2019_ex2<- 168
xu_2019_ex2<- read_csv(all_dat[xu_2019_ex2]) |> 
  mutate(dataset = all_names[xu_2019_ex2]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale
xu_2019_ex2<- xu_2019_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

##### cognitive task ####
xu_2019_ex2_c<- xu_2019_ex2 |> filter(Task=="C")

xu_2019_ex2_c_wide <- xu_2019_ex2_c |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#congruent condition
xu_2019_ex2_c_congruent<-xu_2019_ex2_c |> 
  filter(Condition == "Congruent")

#cumulative probs
xu_2019_ex2_c_congruent_plot<- xu_2019_ex2_c_congruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "congruent")

#incongruent condition
xu_2019_ex2_c_incongruent<-xu_2019_ex2_c |> 
  filter(Condition == "Incongruent")

#cumulative probs
xu_2019_ex2_c_incongruent_plot<- xu_2019_ex2_c_incongruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive",
                               condition = "incongruent")

xu_2019_ex2_c_plots<- bind_rows(xu_2019_ex2_c_congruent_plot,
                                xu_2019_ex2_c_incongruent_plot)

ggplot(xu_2019_ex2_c_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


#### memory task ####
xu_2019_ex2_m<- xu_2019_ex2 |> filter(Task=="N")

xu_2019_ex2_m_wide <- xu_2019_ex2_m |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#congruent condition
xu_2019_ex2_m_congruent<-xu_2019_ex2_m |> 
  filter(Condition == "Congruent")

#cumulative probs
xu_2019_ex2_m_congruent_plot<- xu_2019_ex2_m_congruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "congruent")

#incongruent condition
xu_2019_ex2_m_incongruent<-xu_2019_ex2_m |> 
  filter(Condition == "Incongruent")

#cumulative probs
xu_2019_ex2_m_incongruent_plot<- xu_2019_ex2_m_incongruent |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "incongruent")

xu_2019_ex2_m_plots<- bind_rows(xu_2019_ex2_m_congruent_plot,
                                xu_2019_ex2_m_incongruent_plot)

ggplot(xu_2019_ex2_m_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        **Ye_2018 - Perception & Memory          -
##----------------------------------------------------------------
ye_2018<- 169
ye_2018<- read_csv(all_dat[ye_2018]) |> 
  mutate(dataset = all_names[ye_2018]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point scale
ye_2018<- ye_2018 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

### perception task ###
ye_2018_p<- ye_2018 |> filter(Task=="perception")

ye_2018_p_wide <- ye_2018_p |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#precuneus TMS condition
ye_2018_p_precuneus<-ye_2018_p |> 
  filter(Condition == 1)

#cumulative probs
ye_2018_p_precuneus_plot<- ye_2018_p_precuneus |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "precuneus")

#vertex condition
ye_2018_p_vertex<-ye_2018_p |> 
  filter(Condition == 2)

#cumulative probs
ye_2018_p_vertex_plot<- ye_2018_p_vertex |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception",
                               condition = "vertex")

ye_2018_p_plots<- bind_rows(ye_2018_p_precuneus_plot,
                                ye_2018_p_vertex_plot)

ggplot(ye_2018_p_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

### memory task ###
ye_2018_m<- ye_2018 |> filter(Task=="memory")

ye_2018_m_wide <- ye_2018_m |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, Condition, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#precuneus TMS condition
ye_2018_m_precuneus<-ye_2018_m |> 
  filter(Condition == 1)

#cumulative probs
ye_2018_m_precuneus_plot<- ye_2018_m_precuneus |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "precuneus")

#vertex condition
ye_2018_m_vertex<-ye_2018_m |> 
  filter(Condition == 2)

#cumulative probs
ye_2018_m_vertex_plot<- ye_2018_m_vertex |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory",
                               condition = "vertex")

ye_2018_m_plots<- bind_rows(ye_2018_m_precuneus_plot,
                            ye_2018_m_vertex_plot)

ggplot(ye_2018_m_plots, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

######

plot_multidata<-bind_rows(adler_2018_ex2_plots,
                          bang_2019_ex1_plots,
                          bang_2019_ex2_plots,
                          clark_2018_plots,
                          denison_2018_plots,
                          desender_2022_ex1_plots,
                          haddara_2022_ex1_plots,
                          haddara_2022_ex2_plots,
                          mazor_2020_plots,
                          rausch_2020_plots,
                          sadeghi_2017_memory_plots,
                          sadeghi_2017_perception_plots,
                          shekhar_2018_plots,
                          sherman_2016_jocn_plots,
                          siedlecka_2016_plots,
                          siedlecka_2019_1_plots,
                          siedlecka_2019_2_plots,
                          skora_2016_plots,
                          xu_2019_ex1_c_plots,
                          xu_2019_ex1_m_plots,
                          xu_2019_ex2_c_plots,
                          xu_2019_ex2_m_plots,
                          ye_2018_m_plots,
                          ye_2018_p_plots)

#cognitive experiment graphs
plot_multidata %>% filter(category=="Cognitive") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

#perception experiment graphs
plot_multidata %>% filter(category=="Perception") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

#memory experiment graphs
plot_multidata %>% filter(category=="Memory") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = condition), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

