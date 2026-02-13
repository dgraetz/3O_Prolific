library(tidyverse)
library(ggrepel)
library(psych)

#pull data from pavlovia
system(paste0("git -C gazebos_prolific pull https://dgraetz:", "glpat-zFGAU_MLRrJuDLxbrCDx", "@gitlab.pavlovia.org/dgraetz/gazebos_prolific.git"))
system(paste0("git -C beaches_prolific pull"))

#csv <- list.files("beaches/data_saved_RAs/", full.names = TRUE, pattern = ".csv")
csv1 <- list.files("beaches_prolific//data/", full.names = TRUE, pattern = "log.gz") %>% gsub("log.gz", "csv", .)
csv2 <- list.files("gazebos_prolific//data/", full.names = TRUE, pattern = "log.gz") %>% gsub("log.gz", "csv", .)

csv <- c(csv1, csv2)

data <- list()
for(i in 1:length(csv)){
  if (file.size(csv[i]) > 0){
    data[[i]] <- read.csv(csv[i]) 
  }
}

data <- bind_rows(data) %>%
  rename(participant = PROLIFIC_PID.1)

datax <- bind_rows(data) %>% 
  filter(!is.na(img0_x)) %>%
  select(participant, expName, date, randomization_version, img0:current_is_ac, mouse.clicked_name, mouse.time, left:right) %>%
  mutate_at(c("img0", "img1", "img2", "left", "middle", "right"), ~gsub("stimuli/", "", .)) %>%
  mutate(date = as.POSIXct(gsub("_", " ", gsub("h|\\.", ":", date)), format = "%Y-%m-%d %H:%M:%OS"),
         clicked = parse_number(mouse.clicked_name) %>% as.numeric() + 1,
         #selected = c(img0, img1, img2)[clicked]),
         selected = case_when(clicked == 1 ~ img0,
                              clicked == 2 ~ img1,
                              clicked == 3 ~ img2),
         mouse.time = parse_number(mouse.time) %>% as.numeric(),
         ac_passed = NA,
         ac_passed = ifelse(current_is_ac == 1, case_when(img0 == img1 ~ img2,
                                                          img1 == img2 ~ img0,
                                                          img0 == img2 ~ img1), ac_passed),
         ac_passed = case_when(current_is_ac == 1 & ac_passed == selected ~ 1,
                               current_is_ac == 1 & ac_passed != selected ~ 0,
                               current_is_ac == 0                         ~ NA))%>%
  group_by(expName, participant)

datax %>%
  group_by(expName) %>%
  summarize(N = length(unique(participant)))

all_beach_participants <- unique(datax[datax$expName == "beaches",]$participant)
all_gazebo_participants <- unique(datax[datax$expName == "gazebos",]$participant)

datax <- datax %>% # we are excluding participants who had participated in an outdated randomization version, and we only want to now include participants that have both participations with the new version
  group_by(expName, participant, date, randomization_version) %>%
  filter(n() >= 100) %>% #min of 100 trials
  ungroup()


timing <- bind_rows(data) %>% 
  group_by(participant, date)%>%
  summarize(end = mean(gbye.stopped, na.rm = TRUE),
            end = end/60) 

sum(timing$end <= 30, na.rm = TRUE)/length(timing$end)

participant_summary <- datax %>%
  group_by(expName, participant, date, randomization_version) %>%
  summarize(M_AC_passed = mean(ac_passed, na.rm = TRUE),
            RT = mean(mouse.time, na.rm = TRUE),
            N_Trials = n(),
            N_Trials_excl_ac = sum(current_is_ac == 0)) %>%
  arrange(expName, participant, date) %>% #sort by everything, including date
  group_by(expName, participant, date) %>% 
  slice(1) %>%
  group_by(expName, participant) %>%
  mutate(min_AC = min(M_AC_passed))

participant_summary %>%
  print(n = nrow(.))

datax <- left_join(participant_summary, datax)


datax %>%
  filter(min_AC >= 0.8) %>%
  group_by(expName, participant) %>%
  summarize(RT = mean(mouse.time[current_is_ac == 0]),
            Acc = mean(ac_passed[current_is_ac == 1])) %>%
  group_by(expName) %>%
  summarize(RT_M = mean(RT),
            RT_SD = sd(RT),
            Acc_M = mean(Acc),
            Acc_SD = sd(Acc))

datax %>%
  filter(min_AC >= 0.8) %>%
  group_by(expName, participant) %>%
  summarize(NTrials = n()) %>%
  group_by(expName, participant) %>%
  summarize(NTrials = mean(NTrials)) %>%
  group_by(expName) %>%
  summarize(NTrials_M = mean(NTrials),
            NTrials_SD = sd(NTrials))
# ggplot(participant_summary, aes(x = RT, y = M_AC_passed))+
#   geom_point()+
#   geom_smooth()+
#   theme_classic()

participant_summary %>%
  group_by(expName) %>%
  summarize(N_goodAC_participations = length(participant[min_AC >= 0.8]),
            N_participations = n(),
            N_goodAC_participants = length(unique(participant[min_AC >= 0.8])),
            N_participants = length(unique(participant)),
            N_goodAC_Trials = sum(N_Trials[min_AC >= 0.8], na.rm = TRUE),
            N_goodAC_Trials_excl_AC = sum(N_Trials_excl_ac[min_AC >= 0.8], na.rm = TRUE),
            N_Trials = sum(N_Trials),
            N_Trials_excl_AC = sum(N_Trials_excl_ac),
            AC_all = mean(min_AC, na.rm = TRUE),
            AC_goodAC = mean(min_AC[min_AC >= 0.8], na.rm = TRUE))



# #######################################################
# #demographics
# 
# library(rio)
# demo <- import("demographics.csv") %>%
#   group_by(expName, participant) %>%
#   slice(1)
# 
# 
# demo$Age2 <- (as.numeric(difftime(as.Date(demo$responseDate, format="%y-%m-%d"), 
#                     as.Date(demo$DOB, format="%y-%m-%d"),
#                     unit="weeks")) / 52.25) %>% floor()
# 
# demo <- demo %>%
#   mutate(is_valid_age = ifelse(Age >= 18 & Age < 80, 1, 0),
#          is_valid_age2 = ifelse(Age2 >= 18 & Age2 < 80, 1, 0),
#          final_age = ifelse(is_valid_age == 1, Age, ifelse(is_valid_age2 == 1, Age2, Age)),
#          is_valid_age_final = ifelse(final_age >= 18 & Age < 80, 1, 0),
#          sex = ifelse(sex == "Item 2", "Male", sex)) 
# 
# demo_unique_participants <- demo %>%
#   filter(participant %in% c(all_beach_participants, all_gazebo_participants)) %>%
#   group_by(participant) %>%
#   slice(1) %>% 
#   ungroup() %>%
#   summarize(N = n(),
#             Age_valid = length(final_age[is_valid_age_final == 1]),
#             Age_M = mean(final_age[is_valid_age_final == 1]), 
#             Age_SD = sd(final_age[is_valid_age_final == 1]),
#             Age_min = min(final_age[is_valid_age_final == 1]),
#             Age_max = max(final_age[is_valid_age_final == 1]),
#             Male = sum(sex == "Male"),,
#             Male_p = Male/N,
#             Female = sum(sex == "Female"),
#             Female_p = Female/N)
# 
# demo_beach_participants <- demo %>%
#   filter(participant %in% all_beach_participants) %>%
#   group_by(participant) %>%
#   slice(1) %>%
#   ungroup() %>%
#   summarize(N = n(),
#             Age_valid = length(final_age[is_valid_age_final == 1]),
#             Age_M = mean(final_age[is_valid_age_final == 1]), 
#             Age_SD = sd(final_age[is_valid_age_final == 1]),
#             Age_min = min(final_age[is_valid_age_final == 1]),
#             Age_max = max(final_age[is_valid_age_final == 1]),
#             Male = sum(sex == "Male"),,
#             Male_p = Male/N,
#             Female = sum(sex == "Female"),
#             Female_p = Female/N)
# 
# 
# demo_gazebo_participants <- demo %>%
#   filter(participant %in% all_gazebo_participants) %>%
#   group_by(participant) %>%
#   slice(1) %>%
#   ungroup() %>%
#   summarize(N = n(),
#             Age_valid = length(final_age[is_valid_age_final == 1]),
#             Age_M = mean(final_age[is_valid_age_final == 1]), 
#             Age_SD = sd(final_age[is_valid_age_final == 1]),
#             Age_min = min(final_age[is_valid_age_final == 1]),
#             Age_max = max(final_age[is_valid_age_final == 1]),
#             Male = sum(sex == "Male"),,
#             Male_p = Male/N,
#             Female = sum(sex == "Female"),
#             Female_p = Female/N)
# 
#   
final_participants <- datax %>% 
  filter(min_AC >= 0.8) %>%
  group_by(expName) %>%
  summarize(N_participations = length(unique(participant)))

# demo_unique_participants
# demo_beach_participants
# demo_gazebo_participants
# final_participants
# 
# paste0("In total, N = ", 
#        demo_unique_participants$N, 
#        " human subjects participated in the perceptual similarity study (Age: M = ", 
#        round(demo_unique_participants$Age_M, 2),
#        ", SD = ",
#        round(demo_unique_participants$Age_SD, 2),
#        ", min = ",
#        demo_unique_participants$Age_min,
#        ", max = ",
#        demo_unique_participants$Age_max,
#        "; N female = ",
#        demo_unique_participants$Female,
#        "). ",
#        demo_beach_participants$N,
#        " participated in the beach similarity experiment (Age: M = ", 
#        round(demo_beach_participants$Age_M, 2),
#        ", SD = ",
#        round(demo_beach_participants$Age_SD, 2),
#        ", min = ",
#        demo_beach_participants$Age_min,
#        ", max = ",
#        demo_beach_participants$Age_max,
#        "; N female = ",
#        demo_beach_participants$Female,
#        "), and ",
#        demo_gazebo_participants$N,
#        " participated in the gazebo similarity experiment (Age: M = ", 
#        round(demo_gazebo_participants$Age_M, 2),
#        ", SD = ",
#        round(demo_gazebo_participants$Age_SD, 2),
#        ", min = ",
#        demo_gazebo_participants$Age_min,
#        ", max = ",
#        demo_gazebo_participants$Age_max,
#        "; N female = ",
#        demo_gazebo_participants$Female,
#        "). N = ",
#        final_participants[final_participants$expName == "beaches",]$N_participations,
#        " and N = ",
#        final_participants[final_participants$expName == "gazebos",]$N_participations,
#        " were included in the final analyses for the beach and gazebo similarity experiments, respectively."
# )


#######################################################


pairs <- expand.grid(img1 = unique(datax$img0),
                         img2 = unique(datax$img0)) %>%
  mutate(img1_cat = gsub("[[:digit:]]+.jpg", "", .$img1),
         img2_cat = gsub("[[:digit:]]+.jpg", "", .$img2)) %>%
  filter(img1_cat == img2_cat) %>%
  #make_unique(column1 = "img1", column2 = "img2") %>%
  filter(img1 != img2) %>% 
  mutate(similarity = NA,
         n_participants_seen = NA,
         n_pair_exposures = NA,
         n_contexts = NA,
         M_AC_passed = NA)

# all_data <- datax
# 
# 
# ###############
# datax <- all_data

# datax <- filter(datax) %>%
#   filter(participant == 2282320)


get_similarity <- function(pairs, data){

  for(i in 1:nrow(pairs)){
    current_pair <- c(pairs$img1[i], pairs$img2[i]) %>% as.vector()
    tmp <-   data %>%
      ungroup() %>%
      filter((img0 %in% current_pair & img1 %in% current_pair)|
               (img0 %in% current_pair & img2 %in% current_pair)|
               (img1 %in% current_pair & img2 %in% current_pair),
             current_is_ac == 0) %>%
      mutate(context = case_when((img0 %in% current_pair & img1 %in% current_pair) ~ img2,
                                 (img0 %in% current_pair & img2 %in% current_pair) ~ img1,
                                 (img1 %in% current_pair & img2 %in% current_pair) ~ img0),
             n_participants_seen = length(unique(participant)))    %>%
      group_by(context) %>%
      summarize(M_AC_passed = M_AC_passed[1],
                min_AC = min_AC[1],
                n_participants_seen = n_participants_seen[1],
                pair_exposures = n(),
                similarity = mean(selected == context)) %>% #if selected is the context, i. e., not one of the current images we are interested in, return TRUE (or 1) --> this will be the similarity value for this image pair for this trial; otherwise 0. We then average across these images
      ungroup() %>%
      summarize(M_AC_passed = M_AC_passed[1],
                min_AC = min_AC[1],
                n_participants_seen = n_participants_seen[1],
                n_pair_exposures = sum(pair_exposures),
                n_contexts = n(),
                similarity = mean(similarity))
    
    pairs$n_participants_seen[i] <- tmp$n_participants_seen
    pairs$similarity[i] <- tmp$similarity
    pairs$n_contexts [i] <- tmp$n_contexts 
    pairs$n_pair_exposures[i] <- tmp$n_pair_exposures 
    pairs$M_AC_passed[i] <- tmp$M_AC_passed 
    pairs$min_AC[i] <- tmp$min_AC 
    
  }
  return(pairs)
}

time1 <- proc.time()
all_pair_similarity <- get_similarity(pairs, datax)

#Output for Soroush:
#all_pair_similarity08 <- get_similarity(pairs, datax %>% filter(M_AC_passed >= 0.8))
data_final <- datax %>% 
  filter(min_AC >= 0.8)

all_pair_similarity08 <- get_similarity(pairs, data_final)
final_all_pair_similarity08 <- all_pair_similarity08 %>%
  select(img1, img2, similarity) %>%
  arrange(img1, img2)

data_final %>% 
  group_by(expName) %>%
  summarize(N_participations = length(unique(participant)))
  
write.csv(final_all_pair_similarity08, paste0("results/3O_similarity08_", gsub(":", "", Sys.time()), ".csv"), row.names = FALSE)

prior <- rio::import("/Users/dominik/Google Drive/My Drive/Studium/University of Oregon/SAP/3O/results/3O_similarity08_2025-03-03 154159.343498.csv")

xx <- left_join(final_all_pair_similarity08, prior, by = c("img1", "img2"))
cor.test(xx$similarity.x, xx$similarity.y, use = "pairwise.complete.obs")
xx <- xx %>%
  mutate(cat1 = gsub("[[:digit:]]+.jpg", "", img1),
         cat2 = gsub("[[:digit:]]+.jpg", "", img2))

beaches_cor <- xx %>%
  filter(cat1 == cat2) %>%
  filter(cat1 == "beach")

cor.test(beaches_cor$similarity.x, beaches_cor$similarity.y, use = "pairwise.complete.obs")

gazebo_cor <- xx %>%
  filter(cat1 == cat2) %>%
  filter(cat1 == "gazebo")

cor.test(gazebo_cor$similarity.x, gazebo_cor$similarity.y, use = "pairwise.complete.obs")


ggplot(prior, aes(x = img1, y = img2, fill = similarity))+
  geom_tile()

ggplot(final_all_pair_similarity08, aes(x = img1, y = img2, fill = similarity))+
  geom_tile()



#######################################################################################################################
#RELIABILITY ANALYSIS
#####################

#let's get the similarity matrices for individual participants
library(foreach)
library(doParallel)
cl <- makeCluster(15)
registerDoParallel(cl)

gazebo_participants <- unique(datax[datax$expName == "gazebos",]$participant)
gazpairs <- pairs %>% filter(img1_cat == "gazebo")

gazebo_sims <- foreach (i = gazebo_participants, .packages = c("tidyverse")) %dopar% { 

  return(
    get_similarity(gazpairs, datax %>% filter(participant == i & expName == "gazebos")) %>% mutate(participant = i)
  )
}

beach_participants <- unique(datax[datax$expName == "beaches",]$participant)
beachpairs <- pairs %>% filter(img1_cat == "beach")
beach_sims <- foreach (i = beach_participants, .packages = c("tidyverse")) %dopar% { 
  
  return(
    get_similarity(beachpairs, datax %>% filter(participant == i & expName == "beaches")) %>% mutate(participant = i)
  )
}

saveRDS(gazebo_sims, "old_gazebo_similarity.RDS")
saveRDS(beach_sims, "old_beach_similarity.RDS")

#general settings:
reps <- 50 #how many times do we want to sample per number of subejcts?
performance <- c(0, seq(0.80, 0.95, by = 0.05)) #which performance criteria do we want to look at?

#######################
#GAZEBOS
#######################
gazebo_sims <- bind_rows(gazebo_sims)

all_gazebo_rels <- foreach(p = performance, .packages = c("tidyverse")) %dopar%{  
#for (p in performance){

  gazebo_participants <- gazebo_sims %>%
    #filter(M_AC_passed >= p)
    filter(min_AC >= p)
  
  gazebo_participants <- unique(gazebo_participants$participant)
  
  gazebo_n <- length(gazebo_participants)
  gazebo_n <- ifelse(gazebo_n %% 2 == 0, gazebo_n, gazebo_n - 1)
  
  gazebo_rel <- expand.grid(performance = p,
                            rep = 1:reps,
                            N_participants = seq(2, gazebo_n, by = 2),
                            cor = NA)
  
  for(i in 1:nrow(gazebo_rel)){
    set.seed(i)
    random_participants <- sample(gazebo_participants, gazebo_rel$N_participants[i], replace = FALSE)
    set1_participants <- head(random_participants, length(random_participants)/2)#idx[1:(length(idx)/2)]
    set2_participants <- tail(random_participants, length(random_participants)/2)#idx[((length(idx)/2)+1):length(idx)]
    
    set1 <- gazebo_sims %>%
      filter(participant %in% set1_participants) %>%
      group_by(img1, img2) %>%
      summarize(similarity = mean(similarity, na.rm = TRUE))
    
    set2 <- gazebo_sims %>%
      filter(participant %in% set2_participants) %>%
      group_by(img1, img2) %>%
      summarize(similarity = mean(similarity, na.rm = TRUE))
    
    gazebo_rel$cor[i] <- cor(set1$similarity, set2$similarity, use = "pairwise.complete.obs")
    #print(i)
    
  }
  
  return(gazebo_rel)
}

all_gazebo_rels_agg <- all_gazebo_rels %>%
  bind_rows() %>%
  group_by(performance, N_participants) %>%
  summarize(SD = sd(cor),
            cor = mean(fisherz(cor)),
            cor = fisherz2r(cor)) %>%
  ungroup()

all_gazebo_rels_agg %>% 
  filter(performance == 0.8) %>%
  filter(N_participants == max(N_participants))

all_gazebo_rels_agg_plot <- all_gazebo_rels_agg %>%
  filter(N_participants %% 4 == 0) %>%
  mutate(N_participants_dodged = case_when(near(performance, 0.8)  ~ N_participants + 1.2,
                                           near(performance, 0.85) ~ N_participants + 0.6,
                                           near(performance, 0.9)  ~ N_participants - 0.6,
                                           near(performance, 0.95) ~ N_participants - 1.2,
                                           near(performance, 0)    ~ N_participants))

ggplot(mapping = aes(x = N_participants_dodged, y = cor, color = factor(performance), group = performance))+
  geom_point(data = all_gazebo_rels_agg_plot %>% filter(performance > 0.7))+
  geom_line(data = all_gazebo_rels_agg_plot %>% filter(performance > 0.7))+
  geom_errorbar(data = all_gazebo_rels_agg_plot %>% filter(performance > 0.7), aes(ymin = cor - SD, ymax = cor + SD), width = 0)+
  
  geom_point(data = all_gazebo_rels_agg_plot %>% filter(performance < 0.7), color = "red")+
  geom_line(data = all_gazebo_rels_agg_plot %>% filter(performance < 0.7), color = "red")+
  geom_errorbar(data = all_gazebo_rels_agg_plot %>% filter(performance < 0.7), aes(ymin = cor - SD, ymax = cor + SD), width = 0, color = "red")+
  
  #geom_label_repel(data = all_gazebo_rels_agg_plot %>% filter(cor == max(cor)), aes(label = paste("max cor:\n", round(cor, 3))), color = "black", nudge_x = -2, nudge_y = -0.2, show.legend = FALSE)+
  geom_label_repel(data = all_gazebo_rels_agg_plot %>% filter(cor == max(cor)) %>% slice(1), 
                   aes(label = paste("max cor:\n", round(cor, 3))), 
                   color = "black",
                   box.padding = 5,
                   show.legend = FALSE)+
  #scale_y_continuous(limits = c(0.15, .95))+
  scale_color_viridis_d(name = "Min Performance in \nAttention Check", begin = 0, end = 0.9)+
  labs(x = "Total Number of Participants\n(divide by 2 for sample size per set)",
       y = "Average Correlation",
       title = "Reliability: Gazebo Study",
       subtitle = "Red: All data; Errorbars: +/- 1 SD across iterations")+
  theme_classic()+
  theme(legend.position = c(0.7, 0.3))



#######################
#BEACHES
#######################

beach_sims <- bind_rows(beach_sims)

all_beach_rels <- foreach(p = performance, .packages = c("tidyverse")) %dopar%{  
  #for (p in performance){
  
  beach_participants <- beach_sims %>%
    #filter(M_AC_passed >= p)
    filter(min_AC >= p)
  
  beach_participants <- unique(beach_participants$participant)
  
  beach_n <- length(beach_participants)
  beach_n <- ifelse(beach_n %% 2 == 0, beach_n, beach_n - 1)
  
  beach_rel <- expand.grid(performance = p,
                            rep = 1:reps,
                            N_participants = seq(2, beach_n, by = 2),
                            cor = NA)
  
  for(i in 1:nrow(beach_rel)){
    set.seed(i)
    random_participants <- sample(beach_participants, beach_rel$N_participants[i], replace = FALSE)
    set1_participants <- head(random_participants, length(random_participants)/2)#idx[1:(length(idx)/2)]
    set2_participants <- tail(random_participants, length(random_participants)/2)#idx[((length(idx)/2)+1):length(idx)]
    
    set1 <- beach_sims %>%
      filter(participant %in% set1_participants) %>%
      group_by(img1, img2) %>%
      summarize(similarity = mean(similarity, na.rm = TRUE))
    
    set2 <- beach_sims %>%
      filter(participant %in% set2_participants) %>%
      group_by(img1, img2) %>%
      summarize(similarity = mean(similarity, na.rm = TRUE))
    
    beach_rel$cor[i] <- cor(set1$similarity, set2$similarity, use = "pairwise.complete.obs")
    #print(i)
    
  }
  
  return(beach_rel)
}
stopCluster(cl)

all_beach_rels_agg <- all_beach_rels %>%
  bind_rows() %>%
  group_by(performance, N_participants) %>%
  summarize(SD = sd(cor),
            cor = mean(fisherz(cor)),
            cor = fisherz2r(cor)) %>% 
  ungroup()

all_beach_rels_agg %>% 
  filter(performance == 0.8) %>%
  filter(N_participants == max(N_participants))


all_beach_rels_agg_plot <- all_beach_rels_agg %>%
  filter(N_participants %% 4 == 0) %>%
  mutate(N_participants_dodged = case_when(near(performance, 0.8)  ~ N_participants + 1.2,
                                           near(performance, 0.85) ~ N_participants + 0.6,
                                           near(performance, 0.9)  ~ N_participants - 0.6,
                                           near(performance, 0.95) ~ N_participants - 1.2,
                                           near(performance, 0)    ~ N_participants))

ggplot(mapping = aes(x = N_participants_dodged, y = cor, color = factor(performance), group = performance))+
  geom_point(data = all_beach_rels_agg_plot %>% filter(performance > 0.7))+
  geom_line(data = all_beach_rels_agg_plot %>% filter(performance > 0.7))+
  geom_errorbar(data = all_beach_rels_agg_plot %>% filter(performance > 0.7), aes(ymin = cor - SD, ymax = cor + SD), width = 0)+
  
  geom_point(data = all_beach_rels_agg_plot %>% filter(performance < 0.7), color = "red")+
  geom_line(data = all_beach_rels_agg_plot %>% filter(performance < 0.7), color = "red")+
  geom_errorbar(data = all_beach_rels_agg_plot %>% filter(performance < 0.7), aes(ymin = cor - SD, ymax = cor + SD), width = 0, color = "red")+
  
  #geom_label_repel(data = all_beach_rels_agg_plot %>% filter(cor == max(cor)), aes(label = paste("max cor:\n", round(cor, 3))), color = "black", nudge_x = -2, nudge_y = -0.2, show.legend = FALSE)+
  geom_label_repel(data = all_beach_rels_agg_plot %>% filter(cor == max(cor)) %>% slice(1), 
                   aes(label = paste("max cor:\n", round(cor, 3))), 
                   color = "black", 
                   box.padding = 5,
                   show.legend = FALSE)+#scale_y_continuous(limits = c(0.15, .95))+
  scale_color_viridis_d(name = "Min Performance in \nAttention Check", begin = 0, end = 0.9)+
  labs(x = "Total Number of Participants\n(divide by 2 for sample size per set)",
       y = "Average Correlation",
       title = "Reliability: Beach Study",
       subtitle = "Red: All data; Errorbars: +/- 1 SD across iterations")+
  theme_classic()+
  theme(legend.position = c(0.7, 0.3))

#############################################################################################

######################
#little investigation#
######################

inv <- datax %>%
  filter(expName == "gazebos") %>%
  group_by(left, middle, right) %>%
  summarize(N = n())

triplets <- rio::import("gazebos/gazebo_triplets.csv") %>%
  mutate(left = basename(left),
         middle = basename(middle),
         right = basename(right))

inv <- left_join(triplets, inv) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         idx = 1:n(),
         even = idx%%2 == 0)

hist(inv$N)
hist(inv[inv$even == TRUE,]$N)
hist(inv[inv$even == FALSE,]$N)


####################

inv <- datax %>%
  filter(expName == "beaches") %>%
  group_by(left, middle, right) %>%
  summarize(N = n())

triplets <- rio::import("beaches/beach_triplets.csv") %>%
  mutate(left = basename(left),
         middle = basename(middle),
         right = basename(right))

inv <- left_join(triplets, inv) %>%
  mutate(N = ifelse(is.na(N), 0, N),
         idx = 1:n(),
         even = idx%%2 == 0)

hist(inv$N)
hist(inv[inv$even == TRUE,]$N)
hist(inv[inv$even == FALSE,]$N)

####################


datax %>%
  group_by(expName, participant) %>%
  summarize(N = n()) %>%
  group_by(expName) %>%
  summarize(NTrials = mean(N),
            Nparticipants = length(unique(participant)))

#simulation
sim <- data.frame(combo = 1:2024,
                  N = 0)

for(i in 1:14){
  idx <- sample(1:2024, size = 644, replace = FALSE)
  sim$N[idx] <- sim$N[idx]+1
}

hist(sim$N)

datax %>% 
  filter(expName == "gazebos")%>%
  # group_by(participant) %>%
  # mutate(has_session1 = ifelse(sum(session == 1) > 0, 1, 0)) %>%
  # filter(has_session1 == 1) %>%
  mutate(is_even = ifelse(participant %% 2 == 0, "even", "odd"),
         trials_exposed = case_when(is_even == "even" & session == 1 ~ "even",
                                    is_even == "even" & session == 2 ~ "odd",
                                    is_even == "odd" & session == 1 ~ "odd",
                                    is_even == "odd" & session == 2 ~ "even")) %>%
  group_by(trials_exposed) %>%
  summarize(NTrials = n(),
            N_participants = length(unique(participant)))
  
datax %>%
  filter(expName == "gazebos") %>%
  group_by(participant, session, date) %>%
  slice(1:5) %>%
  #filter(participant == 56967) %>% 
  View()

datax %>%
  filter(participant == 60818) %>%
  filter(expName == "gazebos") %>%
  filter(session == 1) %>% View()

####################session#############################################################################################

sim <- readRDS("G:/My Drive/Studium/University of Oregon/SAP/similarity_analysis/cors.RDS") %>% 
  filter(pic1 != pic2, pic1_isLure == 0 & pic2_isLure == 0) %>%
  filter(cat1 == cat2)
  #filter(type == "beach")

new <- left_join(sim, all_pair_similarity, by = c("pic1" = "img1", "pic2" = "img2"))
new08 <- left_join(sim, all_pair_similarity08, by = c("pic1" = "img1", "pic2" = "img2"))

get_cor <- function(data, models, category = NA, filtered = TRUE){
  
  tmp <- data %>%
    filter(model == models)
  
  if (!is.na(category)){
    tmp <- tmp %>% 
      filter(type == category)
  }
  
  if(filtered){
    tmp <- tmp %>%
      filter(n_contexts >= 10)
  }
  
  return(cor(tmp$cor, tmp$similarity))
}

get_cor(new, models = "vgg", category = "beach", filtered = TRUE)
get_cor(new, models = "densenet", category = "beach", filtered = TRUE)
get_cor(new, models = "alexnet", category = "beach", filtered = TRUE)
get_cor(new, models = "resnet-18", category = "beach", filtered = TRUE)

get_cor(new, models = "vgg", category = "beach", filtered = FALSE)
get_cor(new, models = "densenet", category = "beach", filtered = FALSE)
get_cor(new, models = "alexnet", category = "beach", filtered = FALSE)
get_cor(new, models = "resnet-18", category = "beach", filtered = FALSE)

get_cor(new, models = "vgg", category = "gazebo", filtered = TRUE)
get_cor(new, models = "densenet", category = "gazebo", filtered = TRUE)
get_cor(new, models = "alexnet", category = "gazebo", filtered = TRUE)
get_cor(new, models = "resnet-18", category = "gazebo", filtered = TRUE)

get_cor(new, models = "vgg", category = "gazebo", filtered = FALSE)
get_cor(new, models = "densenet", category = "gazebo", filtered = FALSE)
get_cor(new, models = "alexnet", category = "gazebo", filtered = FALSE)
get_cor(new, models = "resnet-18", category = "gazebo", filtered = FALSE)

get_cor(new08, models = "vgg", category = "beach", filtered = TRUE)
get_cor(new08, models = "densenet", category = "beach", filtered = TRUE)
get_cor(new08, models = "alexnet", category = "beach", filtered = TRUE)
get_cor(new08, models = "resnet-18", category = "beach", filtered = TRUE)

get_cor(new08, models = "vgg", category = "beach", filtered = FALSE)
get_cor(new08, models = "densenet", category = "beach", filtered = FALSE)
get_cor(new08, models = "alexnet", category = "beach", filtered = FALSE)
get_cor(new08, models = "resnet-18", category = "beach", filtered = FALSE)

get_cor(new08, models = "vgg", category = "gazebo", filtered = TRUE)
get_cor(new08, models = "densenet", category = "gazebo", filtered = TRUE)
get_cor(new08, models = "alexnet", category = "gazebo", filtered = TRUE)
get_cor(new08, models = "resnet-18", category = "gazebo", filtered = TRUE)

get_cor(new08, models = "vgg", category = "gazebo", filtered = FALSE)
get_cor(new08, models = "densenet", category = "gazebo", filtered = FALSE)
get_cor(new08, models = "alexnet", category = "gazebo", filtered = FALSE)
get_cor(new08, models = "resnet-18", category = "gazebo", filtered = FALSE)


ggplot(new, aes(x = pic1, y = pic2, fill = similarity))+
  geom_tile()+
  scale_fill_viridis_c()+
  theme_classic()

ggplot(new, aes(x = pic1, y = pic2, fill = cor))+
  geom_tile()+
  scale_fill_viridis_c()+
  theme_classic()


ggplot(new, aes(x = pic1, y = pic2, fill = cor-similarity))+
  geom_tile()+
  scale_fill_viridis_c()+
  facet_wrap(~model)+
  theme_classic()


###########################################################


make_unique <- function(data, column1, column2){
  #this function reduces a dataframe such that only unique pairs will be left.
  #so, if we have two rows (beach1, beach2; and beach2, beach1), one of them will be removed
  #this assumes that the rest of the rows contains redundant information
  
  mod <- data %>%
    rename(tmp_column1 = column1,
           tmp_column2 = column2) %>%
    mutate(val = 1)
  
  cor_matrix <- dcast(mod, tmp_column1 ~ tmp_column2, value.var = "val") 
  
  cor_matrix_first_col <- cor_matrix[,1]
  cor_matrix <- cor_matrix[,-1]
  
  cor_matrix[lower.tri(cor_matrix)] <- NA
  cor_matrix <- cbind(cor_matrix_first_col, cor_matrix)
  
  out <- cor_matrix %>% 
    pivot_longer(cols = -1, values_to = "remove") %>%
    mutate(remove = ifelse(is.na(remove), 1, 0))
  colnames(out)[1:2] <- c(column1, column2)
  
  data <- 
    left_join(data, out, by = c(column1, column2)) %>%
    filter(remove == 0) %>%
    select(-remove)
  return(data)
  
}


###########################################################
library(reshape2)

N_pairs <- 5

cors <- new %>%
  filter(model == "vgg") %>% #doesn't matter what we choose here
  make_unique(column1 = "pic1", column2 = "pic2")%>%
  group_by(model) %>%
  arrange(similarity)

library(jpeg)
library(grid)
library(ggpubr)


get_plots <- function(data){
  out <- list()
  for(i in 1:nrow(data)){
    left_img <- readJPEG(paste0("beaches/stimuli/", data$pic1[i]))
    left_img <- rasterGrob(left_img, interpolate=TRUE)
    left <- ggplot()+
      annotation_custom(left_img)+
      xlim(0, 1)+
      ylim(0, 1)+
      coord_fixed()+
      theme_void()+
      theme(plot.margin = margin(0,0,0,0))+
      theme(panel.background = element_rect(fill = 'white', colour = 'white'))
    
    center <- ggplot(data)+
      geom_text(aes(x = 0.5, y = 0.5, label = round(similarity[i], 3)))+
      theme_void()+
      theme(panel.background = element_rect(fill = 'white', colour = 'white'))
    
    right_img <- readJPEG(paste0("beaches/stimuli/", data$pic2[i]))
    right_img <- rasterGrob(right_img, interpolate=TRUE)
    right <- ggplot()+
      annotation_custom(right_img)+
      xlim(0, 1)+
      ylim(0, 1)+
      coord_fixed()+
      theme_void()+
      theme(plot.margin = margin(0,0,0,0))+
      theme(panel.background = element_rect(fill = 'white', colour = 'white'))
    
    
    out[[i]] <- ggarrange(plotlist = list(left, center, right), nrow = 3, ncol = 1, heights = c(1, 0.2, 1))
  }
  return(out)
}

# plts <- get_plots(beach_cors_unique %>% filter(model == "vgg"))
# gg <- ggarrange(plotlist = plts, nrow = 1)
# ggsave(plot = gg, "gg.png", width = 10, height = 3, units = "in")

beach_plots_filtered <- get_plots(cors %>% filter(type == "beach" & n_pair_exposures >= 20 & !is.na(similarity)) %>% slice(1:5 , (n()-4):n()))
beach_plots_filtered <- ggarrange(plotlist = beach_plots_filtered, nrow = 1, ncol = 10, vjust = 3)
ggsave("results/beach_plots_filtered.png", width = 20, height = 3, units = "in")

beach_plots_all <- get_plots(cors %>% filter(type == "beach"                               & !is.na(similarity)) %>% slice(1:5 , (n()-4):n()))
beach_plots_all <- ggarrange(plotlist = beach_plots_all, nrow = 1, ncol = 10, vjust = 3)
ggsave("results/beach_plots_all.png", width = 20, height = 3, units = "in")


gazebo_plots_filtered <- get_plots(cors %>% filter(type == "gazebo" & n_pair_exposures >= 20 & !is.na(similarity)) %>% slice(1:5 , (n()-4):n()))
gazebo_plots_filtered <- ggarrange(plotlist = gazebo_plots_filtered, nrow = 1, ncol = 10, vjust = 3)
ggsave("results/gazebo_plots_filtered.png", width = 20, height = 3, units = "in")

gazebo_plots_all <- get_plots(cors %>% filter(type == "gazebo"                               & !is.na(similarity)) %>% slice(1:5 , (n()-4):n()))
gazebo_plots_all <- ggarrange(plotlist = gazebo_plots_all, nrow = 1, ncol = 10, vjust = 3)
ggsave("results/gazebo_plots_all.png", width = 20, height = 3, units = "in")

hist(cors$n_contexts, breaks = 30)
hist(cors$n_participants_seen, breaks = 30)
hist(cors$n_pair_exposures, breaks = 30)

range(cors$n_contexts)
range(cors$n_participants_seen)
range(cors$n_pair_exposures)

############################################################################

library(network)
library(reshape2)
library(GGally)
library(ggnetwork)
library(ggimage)

net <- network(dcast(new %>% 
                       filter(model == "vgg" & type == "beach") %>% 
                       mutate(similarity_new = ifelse(similarity < 0.50, 0, similarity)),
                     pic1 ~ pic2,
                     value.var = "similarity_new")%>% 
                 select(-pic1) %>% 
                 as.matrix(),
               directed = FALSE,
               matrix.type = "adjacency", multiple = TRUE)



network_data <- ggnetwork(net) %>%
  mutate(img2 = NA,
         similarity = NA,
         dist = NA) %>% 
  filter(x != xend &y != yend)

for(i in 1:nrow(network_data)){
  # end_img <- 
  #   network_data %>%
  #   filter(x == network_data$xend[i], y == network_data$yend[i]) %>%
  #   select(vertex.names) %>% 
  #   slice(1) %>% 
  #   unlist()
  
  
  end_img <- network_data %>%
    mutate(dist = sqrt((x-network_data$xend[i])^2 + (y-network_data$yend[i])^2)) %>%
    filter(dist == min(dist)) %>%
    select(vertex.names, dist) %>% 
    slice(1) 
  
  if(length(end_img) > 0){
    network_data$img2[i] <- end_img$vertex.names
    network_data$dist[i] <- end_img$dist
  }
  
  similarity <- new %>%
    filter(pic1 == network_data$vertex.names[i] & 
             pic2 == network_data$img2[i]) %>% 
    select(similarity) %>%
    slice(1) %>%
    unlist()
  
  if(length(similarity) > 0){
    network_data$similarity[i] <- similarity
  }
}

network_data <- network_data %>%
  mutate(distance = sqrt((x-xend)^2 + (y-yend)^2))

(plt <- ggplot(network_data %>% filter(similarity > 0.7), aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = similarity, linewidth = similarity), curvature = 0.1) +
    scale_color_viridis_c(option = "inferno")+
    scale_linewidth_continuous(range = c(0.5, 3))+
    geom_image(aes(image = paste0("beaches/stimuli/", vertex.names)))+
    theme_blank())

(plt <- ggplot(network_data, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(aes(color = similarity, linewidth = similarity), curvature = 0.1) +
    scale_color_viridis_c(option = "inferno")+
    scale_linewidth_continuous(range = c(0.5, 3))+
    geom_text(aes(label = img2))+
    #geom_image(aes(image = paste0("beaches/stimuli/", vertex.names)))+
    theme_blank())







ggsave(plt, file = "try.png")

ggplot(ggnetwork(net), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0.1) +
  #geom_image(aes(image = paste0("beaches/stimuli/", vertex.names)))+
  theme_blank()


ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(curvature = 0.1) +
  #geom_image(aes(image = paste0("beaches/stimuli/", vertex.names)))+
  theme_blank()

#########################################################################
ggnet(net, edge.color = c("color", "grey50"))

plt <- ggnet(net,
             mode = "fruchtermanreingold",)
plt

plt+
  geom_image(aes(image = paste0("beaches/stimuli/", label)))

plt_data <- plt$data

ggplot(plt_data, aes(x = x, y = y))+
  geom_point()+
  theme_classic()

library(ggimage)


############################################################################
igraph::graph_from_adjacency_matrix(dcast(new %>% 
                                            filter(model == "vgg") %>%
                                            filter(type == "beach") %>%
                                            mutate(similarity = ifelse(similarity < 0.60, 0, similarity)),
                                          pic1 ~ pic2, value.var = "similarity") %>% 
                                      select(-pic1) %>% 
                                      as.matrix() , weighted=TRUE, mode="undirected", diag=FALSE) %>% 
  plot()

# Basic chart
plot(network)
