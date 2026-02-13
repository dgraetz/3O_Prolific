library(tidyverse)

set.seed(02102026)

young_beach <- readRDS("young_beach_similarity.RDS") %>% 
  bind_rows() %>%
  filter(min_AC >= .8) %>%
  mutate(participant = factor(participant),
         is_old = 0)

young_gazebo <- readRDS("young_gazebo_similarity.RDS") %>% 
  bind_rows() %>%
  filter(min_AC >= .8)%>%
  mutate(participant = factor(participant),
         is_old = 0)


old_beach <- readRDS("old_beach_similarity.RDS") %>% 
  bind_rows() %>%
  filter(min_AC >= .8)%>%
  mutate(participant = factor(participant),
         is_old = 1)


old_gazebo <- readRDS("old_gazebo_similarity.RDS") %>% 
  bind_rows() %>%
  filter(min_AC >= .8)%>%
  mutate(participant = factor(participant),
         is_old = 1)



young_beach %>%
  group_by(participant) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  summarize(N = mean(N))


young_gazebo %>%
  group_by(participant) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  summarize(N = mean(N))

old_beach %>%
  group_by(participant) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  summarize(N = mean(N))

old_gazebo %>%
  group_by(participant) %>%
  summarize(N = n()) %>%
  ungroup() %>%
  summarize(N = mean(N))


df <- expand.grid(iteration = 1:1000,
                  N_subjects = 40,
                  sample_size = 7500, 
                  scene = c("beaches", "gazebos")) %>%
  mutate(r_younger_younger = NA,
         r_younger_older = NA,
         r_older_older = NA)


create_sim_mat <- function(x){
  
  x %>%
    group_by(img1, img2) %>%
    summarize(similarity = mean(similarity, na.rm  = TRUE))
  
}

for(i in 1:nrow(df)){
  
  
  if (df$scene[i] == "beaches"){
    
    ratings <- list(old = old_beach,
                    young = young_beach)
  } else {
    
    ratings <- list(old = old_gazebo,
                    young = young_gazebo)
  }
  
  
  #let's make sure our data is coming from the same number of subjects to avoid biases (older adults respond slower, therefore fewer trials, also older adult sample is smaller, therefore greater reliability with set number of trials)
  
  young_subjects <- unique(ratings$young$participant)
  young_subjects <- sample(young_subjects, df$N_subjects[i], replace = FALSE)
  old_subjects <- unique(ratings$old$participant)
  old_subjects <- sample(old_subjects, df$N_subjects[i], replace = FALSE)

  ratings$young1 <- ratings$young %>% filter(participant %in% head(young_subjects, df$N_subjects[i]/2))
  ratings$young2 <- ratings$young %>% filter(participant %in% tail(young_subjects, df$N_subjects[i]/2))
  ratings$old1 <- ratings$old %>% filter(participant %in% head(old_subjects, df$N_subjects[i]/2))
  ratings$old2 <- ratings$old %>% filter(participant %in% tail(old_subjects, df$N_subjects[i]/2))
  
  young1_rows <- 1:nrow(ratings$young1)
  young2_rows <- 1:nrow(ratings$young2)
  old1_rows <- 1:nrow(ratings$old1)
  old2_rows <- 1:nrow(ratings$old2)
  
  #uncomment this if you want to control for number of trials per subject
  young1_rows <- sample(1:nrow(ratings$young1), size = df$sample_size[i], replace = FALSE)
  young2_rows <- sample(1:nrow(ratings$young2), size = df$sample_size[i], replace = FALSE)
  old1_rows <- sample(1:nrow(ratings$old1), size = df$sample_size[i], replace = FALSE)
  old2_rows <- sample(1:nrow(ratings$old2), size = df$sample_size[i], replace = FALSE)
  
  
  young1 <- ratings$young1[young1_rows,] %>% create_sim_mat() %>% rename(young1 = similarity)
  young2 <- ratings$young2[young2_rows,] %>% create_sim_mat()  %>% rename(young2 = similarity)
  old1 <- ratings$old1[old1_rows,] %>% create_sim_mat() %>% rename(old1 = similarity)
  old2 <- ratings$old2[old2_rows,] %>% create_sim_mat() %>% rename(old2 = similarity)
  
  all <- young1 %>%
    full_join(young2, by = c("img1", "img2")) %>%
    full_join(old1, by = c("img1", "img2")) %>%
    full_join(old2, by = c("img1", "img2"))
  
  
  df$r_younger_younger[i] <- cor(all$young1, all$young2, use = "pairwise.complete.obs")
  df$r_older_older[i] <- cor(all$old1, all$old2, use = "pairwise.complete.obs")
  
  df$r_younger_older[i] <- cor(all$young1, all$old1, use = "pairwise.complete.obs")
  
  
}

df_long <- df %>%
  pivot_longer(cols = c(r_younger_younger, r_older_older, r_younger_older), names_to = "type", values_to = "cor", names_prefix = "r_") 

df_long_agg <- df_long %>%
  group_by(type, scene) %>%
  summarize(cor = mean(cor))

ggplot(df_long, aes(x = type, y = cor, color = scene, fill = scene))+
  geom_jitter(alpha = 0.2)+
  geom_point(data = df_long_agg, pch = 23, color = "black", size = 3)+
  scale_x_discrete(limits = c("younger_younger", "older_older", "younger_older"))+
  #labs(x = "correlation type")+
  theme_classic()

ggsave("results/correlation_differences.png", width = 7, height = 3, units = "in")

#df_controlled_subjects <- df

#Let's see where the differences are:
older_agg <- read.csv("results/3O_similarity08_2026-02-10 212218.76203.csv") %>%
  rename(older_sim = similarity)

younger_agg <- rio::import("/Users/dominik/Google Drive/My Drive/Studium/University of Oregon/SAP/3O/results/3O_similarity08_2025-03-03 154159.343498.csv") %>%
  rename(younger_sim = similarity)


all_sim_agg <- full_join(older_agg, younger_agg) %>%
  mutate(diff = older_sim-younger_sim)

ggplot(all_sim_agg, aes(x = img1, y = img2, fill = diff))+
  geom_tile()+
  theme_classic()

all_sim_agg <- all_sim_agg %>%
  mutate(img1_path = paste0("beaches_prolific/stimuli/", img1),
         img2_path = paste0("beaches_prolific/stimuli/", img2))




# CLEANING STEPS:
# 1. Fix Symmetry: Filter so we only keep one version of each pair (A-B, not B-A)
# 2. Identify Scene: Guess scene type from filename (assuming 'beach' or 'gazebo' is in the name)
# 3. Create Paths: specific to the scene folder
all_unique <- all_sim_agg %>%
  filter(img1 < img2) %>%                 # Removes duplicates and self-pairs
  mutate(
    scene_type = case_when(
      str_detect(img1, "beach") ~ "beaches",
      str_detect(img1, "gazebo") ~ "gazebos",
      TRUE ~ "unknown"
    ),
  )

# --- 2. PLOTTING FUNCTION ---

get_comparison_plots <- function(data){
  out <- list()
  
  for(i in 1:nrow(data)){
    
    # Image 1
    img1_grob <- tryCatch({
      rasterGrob(readJPEG(data$img1_path[i]), interpolate=TRUE)
    }, error = function(e) { grid::textGrob("Img Missing") })
    
    p1 <- ggplot() + annotation_custom(img1_grob) + theme_void()
    
    # Stats Text
    # Red = Older rated higher; Blue = Younger rated higher
    text_col <- ifelse(data$diff[i] > 0, "darkred", "navyblue")
    stats_text <- paste0(
      "Y: ", sprintf("%.2f", data$younger_sim[i]), "\n",
      "O: ", sprintf("%.2f", data$older_sim[i]), "\n",
      "Diff: ", sprintf("%+.2f", data$diff[i])
    )
    
    p2 <- ggplot() +
      geom_text(aes(x = 0.5, y = 0.5, label = stats_text), 
                size = 3, color = text_col, fontface = "bold") +
      theme_void()
    
    # Image 2
    img2_grob <- tryCatch({
      rasterGrob(readJPEG(data$img2_path[i]), interpolate=TRUE)
    }, error = function(e) { grid::textGrob("Img Missing") })
    
    p3 <- ggplot() + annotation_custom(img2_grob) + theme_void()
    
    # Stack: Image - Stats - Image
    out[[i]] <- ggarrange(p1, p2, p3, ncol = 1, heights = c(1, 0.4, 1))
  }
  return(out)
}

# --- 3. GENERATE PLOTS FOR BEACHES ---

beaches_extreme <- all_unique %>%
  filter(scene_type == "beaches") %>%
  arrange(diff) %>%
  slice(1:5, (n()-4):n()) # Top 5 negative (Younger) and Top 5 positive (Older)

if(nrow(beaches_extreme) > 0){
  beach_plots <- get_comparison_plots(beaches_extreme)
  final_beach <- ggarrange(plotlist = beach_plots, nrow = 1, ncol = 10)
  
  ggsave("results/beach_extremes.png", final_beach, width = 16, height = 4, bg = "white")
  print("Beach plot saved.")
} else {
  print("No beach data found.")
}


# --- 4. GENERATE PLOTS FOR GAZEBOS ---

gazebos_extreme <- all_unique %>%
  filter(scene_type == "gazebos") %>%
  arrange(diff) %>%
  slice(1:5, (n()-4):n()) 

if(nrow(gazebos_extreme) > 0){
  gazebo_plots <- get_comparison_plots(gazebos_extreme)
  final_gazebo <- ggarrange(plotlist = gazebo_plots, nrow = 1, ncol = 10)
  
  ggsave("results/gazebo_extremes.png", final_gazebo, width = 16, height = 4, bg = "white")
  print("Gazebo plot saved.")
} else {
  print("No gazebo data found.")
}





sim <- readRDS("~/Google Drive/My Drive/Studium/University of Oregon/SAP/brain_analysis/helper_files/sim.RDS") 

sim <- sim %>%
  mutate(cat1 = gsub("[[:digit:]]+", "", scene1),
         cat2 = gsub("[[:digit:]]+", "", scene2)) %>%
  filter(cat1 == cat2) %>%
  group_by(cat1) %>%
  mutate(across(
    .cols = c(clip, sem, vgg_mp1, vgg_mp3, vgg_mp5, gist, ssim, alex),             # Select columns (or use where(is.numeric))
    .fns = ~ .x - mean(.x, na.rm = TRUE), # Subtract mean (handling NAs)
    .names = "{.col}_centered"         # Naming pattern for NEW variables
  ))



trials <- bind_rows(young_beach, young_gazebo, old_beach, old_gazebo) %>%
  mutate(scene1 = gsub(".jpg", "", img1),
         scene2 = gsub(".jpg", "", img2)) %>%
  left_join(sim)




#this is to later reorder all the images so that they show up in a consistent order
scene_levels <- c(paste0("beach", 0:23), paste0("gazebo", 0:23))

#here we are defining the matrix that we always want to show
#Basically, this will help us later, when working with full matrices, to select only the pairs that correspond to the lower triangle

df <- expand.grid(scene1 = scene_levels,
                  scene2 = scene_levels) %>%
  mutate(present_before = 0)

for(i in 1:nrow(df)){
  
  idx <- which(df$scene2 == df$scene1[i] & df$scene1 == df$scene2[i])
  if (idx > i){
    df$present_before[idx] <- 1
  }
  
}

#full pairs
unique_pairs <- df %>%
  filter(present_before == 0) %>%
  mutate(cat1 = gsub("[[:digit:]]+", "", scene1),
         cat2 = gsub("[[:digit:]]+", "", scene2),
         scene1 = factor(scene1, levels = scene_levels),
         scene2 = factor(scene2, levels = scene_levels))

#within category pairs
unique_pairs_same_cat <- unique_pairs %>%
  filter(cat1 == cat2)

rm(df)


lower_tr <- function(unique_pairs, df){
  left_join(unique_pairs, df) %>%
    mutate(scene1 = factor(scene1, levels = scene_levels),
           scene2 = factor(scene2, levels = scene_levels)) %>%
    return()
}


trials2 <- lower_tr(unique_pairs_same_cat, trials)


library(lmerTest)


m <- lmer(similarity ~ (sem_centered + gist_centered + vgg_mp1_centered + vgg_mp3_centered + vgg_mp5_centered + alex_centered + clip_centered + ssim_centered) * is_old + (1|participant), trials2)

summary(m)

sjPlot::plot_model(m, terms = c("vgg_mp1_centered", "is_old"), type = "pred")
sjPlot::plot_model(m, terms = c("vgg_mp3_centered", "is_old"), type = "pred")
sjPlot::plot_model(m, terms = c("clip_centered", "is_old"), type = "pred")



all_sim_agg2 <- all_sim_agg %>%
  mutate(scene1 = gsub(".jpg", "", img1),
         scene2 = gsub(".jpg", "", img2)) %>%
  left_join(sim) %>% lower_tr(unique_pairs_same_cat, .)

lm(pc1 ~ older_sim + younger_sim, all_sim_agg2) %>% summary()
lm(pc2 ~ older_sim + younger_sim, all_sim_agg2) %>% summary()

all_sim_agg2_long <- all_sim_agg2 %>%
  pivot_longer(cols = c(older_sim, younger_sim), values_to = "vizsim", names_to = "is_old") %>%
  mutate(is_old = case_when(is_old == "older_sim" ~ 1,
                            is_old == "younger_sim" ~ 0 ))

lm(pc1 ~ is_old * vizsim, all_sim_agg2_long) %>% summary()
lm(pc2 ~ is_old * vizsim, all_sim_agg2_long) %>% summary()


cor(all_sim_agg2_long$pc1[all_sim_agg2_long$is_old == 0], all_sim_agg2_long$vizsim[all_sim_agg2_long$is_old == 0], use = "pairwise.complete.obs")
cor(all_sim_agg2_long$pc1[all_sim_agg2_long$is_old == 1], all_sim_agg2_long$vizsim[all_sim_agg2_long$is_old == 1], use = "pairwise.complete.obs")


cor(all_sim_agg2_long$pc2[all_sim_agg2_long$is_old == 0], all_sim_agg2_long$vizsim[all_sim_agg2_long$is_old == 0], use = "pairwise.complete.obs")
cor(all_sim_agg2_long$pc2[all_sim_agg2_long$is_old == 1], all_sim_agg2_long$vizsim[all_sim_agg2_long$is_old == 1], use = "pairwise.complete.obs")


ggplot(all_sim_agg2_long, aes(x = vizsim, y = pc1, color = factor(is_old)))+
  geom_smooth(method = "lm")

ggplot(all_sim_agg2_long, aes(x = vizsim, y = pc2, color = factor(is_old)))+
  geom_smooth(method = "lm")+
  theme_classic()

ggplot(all_sim_agg2, aes(x = scene1, y = scene2, fill = older_sim))+
  geom_tile()



