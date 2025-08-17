library(tidyverse)
library(stringr)

data_path <- "C:/Users/kiara/OneDrive/Desktop/Confidence Data Analysis/confidence data"

all_dat <- list.files(data_path, full.names = TRUE, pattern = "^data")
all_names <- list.files(data_path, full.names = FALSE, pattern = "^data") |>
  str_remove("^data_") |>
  str_remove(".csv$")


### select data sets that have:
#### - presentation of single item (yes-no) or 2 items (2-AFC)
#### - binary response (correct vs. incorrect)
#### - start with 1 condition only  
#### - confidence rating
#### - published
#### - no continuous confidence rating (for now)
#### - for categorisation tasks, stimulus 1 is signal


### bannerCommenter::boxup("")
##----------------------------------------------------------------
##                        Adler_2018_Expt3 - Cognitive           -
##----------------------------------------------------------------
#reading the data
adler_2018_ex3 <- 3
adler_2018_ex3 <- read_csv(all_dat[adler_2018_ex3]) |> 
  mutate(dataset = all_names[adler_2018_ex3])

#recoding response and confidence
adler_2018_ex3 <- adler_2018_ex3 |> 
  mutate(status = if_else(Stimulus == 2, "signal", "noise"), 
         resp = if_else(Response == 2, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

with(adler_2018_ex3, table(status, conf))

## for each data set, we need a wide data set as below
adler_2018_ex3_wide <- adler_2018_ex3 |> 
  mutate(status = factor(status, levels = c("signal", "noise"))) %>% 
  count(dataset, Subj_idx, status, conf) |> 
  pivot_wider(names_from = c(status, conf), values_from = n, values_fill = 0)

adler_2018_ex3_plot <- adler_2018_ex3 |> 
  count(dataset, status, conf) |> 
  group_by(dataset, status) |> 
  mutate(prob = n/sum(n)) |> 
  ungroup() |> 
  select(-n) |> 
  pivot_wider(names_from = status, values_from = prob) |> 
  arrange(dataset, desc(conf)) |> 
  mutate(across(c(noise, signal), cumsum)) |>  
  filter(conf != "1") |> mutate(category = "Cognitive")

adler_2018_ex3_plot |> 
  ggplot(aes(x =  noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Chandravadia_2020 - Memory                      -
##----------------------------------------------------------------

#aggregating across 3 variants
chandravadia_2020<- 16
chandravadia_2020 <- read_csv(all_dat[chandravadia_2020]) |> 
  mutate(dataset = all_names[chandravadia_2020])


#3 point confidence scale
chandravadia_2020<-chandravadia_2020 |>
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), #0 = new (noise), 1 = old (signal)
         resp = if_else(Response == 1, "signal", "noise"),
         conf = case_when(
           resp == "noise" ~ 4- Confidence,
           resp == "signal" ~ 3 + Confidence
         ))

#wide format
chandravadia_2020_wide <- chandravadia_2020 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(patientNumber, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
chandravadia_2020_plot<- chandravadia_2020 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(chandravadia_2020_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

#########variant 1###########
chandravadia_2020_var1<-chandravadia_2020 |> 
  filter(Variant == 1)

#3 point confidence scale
chandravadia_2020_var1<-chandravadia_2020_var1 |>
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), #0 = new (noise), 1 = old (signal)
         resp = if_else(Response == 1, "signal", "noise"),
         conf = case_when(
           resp == "noise" ~ 4- Confidence,
           resp == "signal" ~ 3 + Confidence
         ))

#cumulative probs
chandravadia_2020_plot_1<- chandravadia_2020_var1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") 

#######variant 2#######
chandravadia_2020_var2<-chandravadia_2020 |> 
  filter(Variant == 2)

#3 point confidence scale
chandravadia_2020_var2<-chandravadia_2020_var2 |>
  mutate(status = if_else(Stimulus == 1, "signal", "noise"),
         resp = if_else(Response == 1, "signal", "noise"),
         conf = case_when(
           resp == "noise" ~ 4- Confidence,
           resp == "signal" ~ 3 + Confidence
         ))

#cumulative probs
chandravadia_2020_plot_2<- chandravadia_2020_var2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") 

#######variant 3#######
chandravadia_2020_var3<-chandravadia_2020 |> 
  filter(Variant == 3)

#3 point confidence scale
chandravadia_2020_var3<-chandravadia_2020_var3 |>
  mutate(status = if_else(Stimulus == 1, "signal", "noise"),
         resp = if_else(Response == 1, "signal", "noise"),
         conf = case_when(
           resp == "noise" ~ 4- Confidence,
           resp == "signal" ~ 3 + Confidence
         ))

#cumulative probs
chandravadia_2020_plot_3<- chandravadia_2020_var3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") 

#overlay all 3 ROC
chandravadia_2020_plot_1<- chandravadia_2020_plot_1 |> mutate(variant = "1")
chandravadia_2020_plot_2<- chandravadia_2020_plot_2 |> mutate(variant = "2")
chandravadia_2020_plot_3<- chandravadia_2020_plot_3 |> mutate(variant = "3")
chandravadia_2020_allplot<- bind_rows(
  chandravadia_2020_plot_1,
  chandravadia_2020_plot_2,
  chandravadia_2020_plot_3
)
ggplot(chandravadia_2020_allplot, aes(x = noise, y = signal, colour = variant)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = variant), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Desender_2022 - Perception             -
##----------------------------------------------------------------
desender_2022_p2B<- 32
desender_2022_p2B <- read_csv(all_dat[desender_2022_p2B]) |> 
  mutate(dataset = all_names[desender_2022_p2B])

#6-point confidence scale
desender_2022_p2B<- desender_2022_p2B |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  ))

#wide format
desender_2022_p2B_wide <- desender_2022_p2B |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
desender_2022_p2B_plot<- desender_2022_p2B |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception") 

ggplot(desender_2022_p2B_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Gajdos_2019 - Perception                       -
##----------------------------------------------------------------
gajdos_2019<- 47
gajdos_2019 <- read_csv(all_dat[gajdos_2019]) |> 
  mutate(dataset = all_names[gajdos_2019]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#4-point confidence scale
gajdos_2019<- gajdos_2019 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

#wide format
gajdos_2019_wide <- gajdos_2019 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
gajdos_2019_plot<- gajdos_2019 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(gajdos_2019_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Gherman_2018 - Perception                       -
##----------------------------------------------------------------
gherman_2018<- 50
gherman_2018 <- read_csv(all_dat[gherman_2018]) |> 
  mutate(dataset = all_names[gherman_2018])

#9-point confidence scale
gherman_2018<- gherman_2018 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) %>% 
  mutate(conf = case_when(
    resp ==  "noise" ~ 10 - Confidence,
    resp ==  "signal" ~ 9 + Confidence
  ))

#wide format
gherman_2018_wide <- gherman_2018 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
gherman_2018_plot<- gherman_2018 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(gherman_2018_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Hu_2017 - Memory                      -
##----------------------------------------------------------------
hu_2017<- 58
hu_2017 <- read_csv(all_dat[hu_2017]) |> 
  mutate(dataset = all_names[hu_2017])

#6-point confidence scale
hu_2017<- hu_2017 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  ))

#wide format
hu_2017_wide <- hu_2017 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
hu_2017_plot<- hu_2017 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(hu_2017_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Jachs_2015_ex1 - Perception                      -
##----------------------------------------------------------------
jachs_2015_ex1<- 59
jachs_2015_ex1 <- read_csv(all_dat[jachs_2015_ex1]) |> 
  mutate(dataset = all_names[jachs_2015_ex1])

#3-point confidence scale
jachs_2015_ex1<- jachs_2015_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |>
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
jachs_2015_ex1_wide <- jachs_2015_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
jachs_2015_ex1_plot<- jachs_2015_ex1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(jachs_2015_ex1_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Jachs_2015_ex2 - Perception                      -
##----------------------------------------------------------------
jachs_2015_ex2<- 60
jachs_2015_ex2 <- read_csv(all_dat[jachs_2015_ex2]) |> 
  mutate(dataset = all_names[jachs_2015_ex2])

#3-point confidence scale
jachs_2015_ex2<- jachs_2015_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |>
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
jachs_2015_ex2_wide <- jachs_2015_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
jachs_2015_ex2_plot<- jachs_2015_ex2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(jachs_2015_ex2_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Jachs_2015_ex3 - Perception                      -
##----------------------------------------------------------------
jachs_2015_ex3<- 61
jachs_2015_ex3 <- read_csv(all_dat[jachs_2015_ex3]) |> 
  mutate(dataset = all_names[jachs_2015_ex3])

#2-point confidence scale
jachs_2015_ex3<- jachs_2015_ex3 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 3 - Confidence,
    resp ==  "signal" ~ 2 + Confidence
  ))

#wide format
jachs_2015_ex3_wide <- jachs_2015_ex3 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
jachs_2015_ex3_plot<- jachs_2015_ex3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(jachs_2015_ex3_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Kantner_2012_ex1  - memory                     -
##----------------------------------------------------------------
kantner_2012_ex1<- 64
kantner_2012_ex1 <- read_csv(all_dat[kantner_2012_ex1]) |> 
  mutate(dataset = all_names[kantner_2012_ex1])

#3-point confidence scale
kantner_2012_ex1<- kantner_2012_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
kantner_2012_ex1_wide <- kantner_2012_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
kantner_2012_ex1_plot<- kantner_2012_ex1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(kantner_2012_ex1_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Kantner_2012_ex2 - memory                      -
##----------------------------------------------------------------
kantner_2012_ex2<- 65
kantner_2012_ex2 <- read_csv(all_dat[kantner_2012_ex2]) |> 
  mutate(dataset = all_names[kantner_2012_ex2])

#3-point confidence scale
kantner_2012_ex2<- kantner_2012_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
kantner_2012_ex2_wide <- kantner_2012_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
kantner_2012_ex2_plot<- kantner_2012_ex2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(kantner_2012_ex2_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Kantner_2012_ex4 - memory                      -
##----------------------------------------------------------------
kantner_2012_ex4<- 67
kantner_2012_ex4 <- read_csv(all_dat[kantner_2012_ex4]) |> 
  mutate(dataset = all_names[kantner_2012_ex4])

#3-point confidence scale
kantner_2012_ex4<- kantner_2012_ex4 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
kantner_2012_ex4_wide <- kantner_2012_ex4 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
kantner_2012_ex4_plot<- kantner_2012_ex4 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(kantner_2012_ex4_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Kantner_2014_ex1 - memory                      -
##----------------------------------------------------------------
kantner_2014_ex1<- 68
kantner_2014_ex1 <- read_csv(all_dat[kantner_2014_ex1]) |> 
  mutate(dataset = all_names[kantner_2014_ex1])

#3-point confidence scale
kantner_2014_ex1<- kantner_2014_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
kantner_2014_ex1_wide <- kantner_2014_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
kantner_2014_ex1_plot<- kantner_2014_ex1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(kantner_2014_ex1_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))


##----------------------------------------------------------------
##                        Kantner_2014_ex2  - memory                     -
##----------------------------------------------------------------
kantner_2014_ex2<- 69
kantner_2014_ex2 <- read_csv(all_dat[kantner_2014_ex2]) |> 
  mutate(dataset = all_names[kantner_2014_ex2])

#3-point confidence scale
kantner_2014_ex2<- kantner_2014_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
kantner_2014_ex2_wide <- kantner_2014_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
kantner_2014_ex2_plot<- kantner_2014_ex2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(kantner_2014_ex2_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Lindsay_2014 - Memory                      -
##----------------------------------------------------------------
lindsay_2014<- 80
lindsay_2014 <- read_csv(all_dat[lindsay_2014]) |> 
  mutate(dataset = all_names[lindsay_2014])

#3 point confidence scale
lindsay_2014<- lindsay_2014 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 4 - Confidence,
    resp ==  "signal" ~ 3 + Confidence
  ))

#wide format
lindsay_2014_wide <- lindsay_2014 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
lindsay_2014_plot<- lindsay_2014 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Memory")

ggplot(lindsay_2014_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Maniscalco_2017_ex1 - Perception                      -
##----------------------------------------------------------------
maniscalco_2017_ex1<- 83
maniscalco_2017_ex1 <- read_csv(all_dat[maniscalco_2017_ex1]) |> 
  mutate(dataset = all_names[maniscalco_2017_ex1]) |>
  filter(!is.na(Response), !is.na(Confidence))

#4-point confidence scale
maniscalco_2017_ex1<- maniscalco_2017_ex1 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

#wide format
maniscalco_2017_ex1_wide <- maniscalco_2017_ex1 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
maniscalco_2017_ex1_plot<- maniscalco_2017_ex1 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(maniscalco_2017_ex1_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Maniscalco_2017_ex3 - Perception                      -
##----------------------------------------------------------------
maniscalco_2017_ex3<- 85
maniscalco_2017_ex3 <- read_csv(all_dat[maniscalco_2017_ex3]) |> 
  mutate(dataset = all_names[maniscalco_2017_ex3]) |>
  filter(ConfCollected == 1,
         !is.na(Confidence))

#4-point confidence scale
maniscalco_2017_ex3<- maniscalco_2017_ex3 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 5 - Confidence,
    resp ==  "signal" ~ 4 + Confidence
  ))

#wide format
maniscalco_2017_ex3_wide <- maniscalco_2017_ex3 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
maniscalco_2017_ex3_plot<- maniscalco_2017_ex3 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(maniscalco_2017_ex3_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Rouault_2018_ex2 - Perception                      -
##----------------------------------------------------------------
rouault_2018_ex2<- 126
rouault_2018_ex2 <- read_csv(all_dat[rouault_2018_ex2]) |> 
  mutate(dataset = all_names[rouault_2018_ex2])

#6-point confidence scale
rouault_2018_ex2<- rouault_2018_ex2 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  ))

#wide format
rouault_2018_ex2_wide <- rouault_2018_ex2 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
rouault_2018_ex2_plot<- rouault_2018_ex2 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(rouault_2018_ex2_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Sinanaj_2015 - Cognitive             -
##----------------------------------------------------------------
sinanaj_2015<- 152
sinanaj_2015 <- read_csv(all_dat[sinanaj_2015]) |> 
  mutate(dataset = all_names[sinanaj_2015])

#5-point scale + wide format
sinanaj_2015<- sinanaj_2015 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 6 - Confidence,
    resp ==  "signal" ~ 5 + Confidence
  ))

sinanaj_2015_wide <- sinanaj_2015 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
sinanaj_2015_plot<- sinanaj_2015 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Cognitive")

ggplot(sinanaj_2015_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

##----------------------------------------------------------------
##                        Song_2011 - Perception             -
##----------------------------------------------------------------
song_2011<- 155
song_2011 <- read_csv(all_dat[song_2011]) |> 
  mutate(dataset = all_names[song_2011]) |>
  filter(!is.na(Confidence),
         !is.na(Response))

#6-point scale + wide format
song_2011<- song_2011 |> 
  mutate(status = if_else(Stimulus == 1, "signal", "noise"), 
         resp = if_else(Response == 1, "signal", "noise")) |> 
  mutate(conf = case_when(
    resp ==  "noise" ~ 7 - Confidence,
    resp ==  "signal" ~ 6 + Confidence
  ))

song_2011_wide <- song_2011 |>
  mutate(status = factor(status, levels = c("signal", "noise"))) %>%
  count(Subj_idx, status, conf) |>
  pivot_wider(
    names_from = c(status, conf),
    values_from = n,
    values_fill = 0
  )

#cumulative probs
song_2011_plot<- song_2011 |>
  count(status, conf, dataset) |>
  group_by(dataset, status) |>
  mutate(prob = n/sum(n)) |>
  ungroup() |>
  select(-n) |>
  pivot_wider(names_from = status, values_from = prob, values_fill = 0)|>
  arrange(desc(conf))|>
  mutate(across(c(noise, signal), cumsum)) |>
  filter(conf !="1") |> mutate(category = "Perception")

ggplot(song_2011_plot, aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))

######

plot_data<-bind_rows(adler_2018_ex3_plot, rouault_2018_ex2_plot,
             chandravadia_2020_plot, desender_2022_p2B_plot,
             gajdos_2019_plot, gherman_2018_plot,
             hainguerlot_2018_plot, hu_2017_plot,
             jachs_2022_ex1_plot, jachs_2022_ex2_plot,
             jachs_2022_ex3_plot, kantner_2012_ex1_plot,
             kantner_2012_ex2_plot, kantner_2012_ex4_plot,
             kantner_2014_ex1_plot, kantner_2014_ex2_plot, lindsay_2014_plot,
             maniscalco_2017_ex1_plot, maniscalco_2017_ex3_plot,
             rouault_2018_ex2_plot, sinanaj_2015_plot,
             song_2011_plot
             )

#cognitive experiment graphs
plot_data %>% filter(category=="Cognitive") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

#perception experiment graphs
plot_data %>% filter(category=="Perception") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

#memory experiment graphs
plot_data %>% filter(category=="Memory") %>% 
  ggplot(aes(x = noise, y = signal)) +
  geom_abline(slope = -1, intercept = 1, linetype = 2) +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = "white") +
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), y = c(-Inf, Inf, -Inf), fill = rgb(0.7, 0.7, 0.7, alpha = 0.4)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_line(aes(group = 1), linewidth = 1) +
  geom_point(size = 2.5) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"))+
  facet_wrap(vars(dataset))

