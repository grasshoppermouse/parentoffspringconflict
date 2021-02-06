#+ message=F, warning=F, fig.width=10, fig.height=8

library(chuuk2019public)
library(readxl)
library(tidyverse)
library(ggfortify)
library(ggforce)
library(ggmosaic)
library(NMF)
library(hagenutils)
library(visreg)
library(MASS)
library(glmnet)
library(glmnetUtils)
library(lme4)
library(viridis)

set.seed(4321)


# Create Figure directory if it doesn't exist -----------------------------

if (!dir.exists('Figures')) dir.create('Figures')

# Recode vars -------------------------------------------------------------

d <- chuuk2019

# grip_z is standardized *within* sex
d$grip_z <- 0
d$grip_z[d$sex == 'm'] <- as.numeric(scale(d$mean_handgrip[d$sex == 'm']))
d$grip_z[d$sex == 'f'] <- as.numeric(scale(d$mean_handgrip[d$sex == 'f']))

d$severity <- ifelse(d$high_severity, 'High', 'No evidence')
d$severity <- factor(d$severity, levels = c('No evidence', 'High'))

d <-
  d %>%
  mutate(
    withhold_cooperation3 = case_when(
      withhold_cooperation == 1 & high_cost_strategy == 1 ~ 'High cost',
      withhold_cooperation == 1 & high_cost_strategy == 0 ~ 'Low cost',
      withhold_cooperation == 0 ~ 'None',
    ),
    withhold_cooperation3 = factor(withhold_cooperation3, levels = c('None', 'Low cost', 'High cost'))
  )

childstrategy <- c(
  "aggression", 
  "withhold_cooperation", 
  "submission", 
  "seek_assistance", 
  "negotiate_persuade_explain",
  "deception/withhold_information",
  "partner_choice"
)

childstrategy_dict <- c(
  "aggression" = "Aggression", 
  "withhold_cooperation" = "Withhold cooperation", 
  "submission" = "Submission", 
  "seek_assistance" = "Seek assistance", 
  "negotiate_persuade_explain" = "Negotiate/persuade/explain",
  "deception/withhold_information" = "Deception/withhold information",
  "partner_choice" = "Partner choice"
)

# Create matrix of causes

cc <- causematrix
cc <- str_split(d$conflict_causes, ",")

# Aggregate 

for (i in 1:length(cc)){
  for (j in cc[[i]]){
    if (j %in% c('resource allocation(goods)', 'resource allocation(money)', 'resource allocation(tv)', 'resource allocation(bed)')){
      cc[[i]] <- 'resource allocation'
    }
    if (j == 'mateships'){
      cc[[i]] <- 'mateship'
    }
  }
}

cc2 <- str_trim(unlist(cc))
cc3 <- unique(cc2)

plot_causes <- ggdotchart(table(cc2))
plot_causes

agg_conflicts <- agg_dict[cc2]
plot_conflicts <- ggdotchart(table(agg_conflicts))
plot_conflicts

mat_cause <- causematrix

# Filter out low frequency causes

mat_cause2 <- as.data.frame(mat_cause)
mat_cause2$sum <- rowSums(mat_cause2)
mat_cause2 <- mat_cause2[mat_cause2$sum>2,]
mat_cause2$sum <- NULL
mat_cause2 <- as.matrix(mat_cause2)


# Emotions ----------------------------------------------------------------

et <- str_split(d$English_emotion_terms, ",")
et2 <- str_trim(unlist(et))
et3 <- unique(et2)

emotions <-
  c(
    "mad" = "anger",
    "feel bad" = "upset",
    "disappointed" = "sad",
    "stopped caring" = "apathetic",
    "confused" = "confused",
    "upset" = "upset",
    "spite but not mad" = "upset",
    "sad" = "sad",
    "shock" = "surprise",
    "shocked" = "surprise",
    "nervous" = "anxiety",
    "worried" = "anxiety",
    "afraid" = "fear",
    "brave" = "brave",
    "don't feel good" = "upset",
    "anger" = "anger",
    "sadness" = "sad",
    "fear" = "fear",
    "resentment" = "anger",
    "0" = NA,
    "ambivalent" = "ambivalent",
    "pissed" = "anger",
    "pissed off" = "anger",
    "frustrating anger" = "anger",
    "frustration" = "anger",
    "missing out" = "sad",
    "frustrated" = "anger",
    "curiosity" = "curious",
    "feel left out" = "sad",
    "excited" = "excited",
    "disturbed" = "disturbed",
    "stressed" = "anxiety",
    "aggravated" = "anger",
    "unknown" = "unknown",
    "don't know how I felt" = 'unknown',
    "stress" = "anxiety",
    "anxiety" = "anxiety",
    "mad at myself" = "shame",
    "feeling sorry for myself" = "sad",
    "resentful" = "anger",
    "ashamed" = "shame",
    "embarrassed" = "shame",
    "guilt" = "shame",
    "feel sorry" = "shame",
    "hurt" = "upset",
    "scared" = "fear",
    "annoyed" = "anger",
    "surprised" = "surprise",
    "didn't care" = "apathetic",
    "disappointment" = "sad",
    "negative" = "upset",
    "bothers me" = "upset",
    "inbetween sad and mad" = "upset",
    "left out" = "sad",
    "content" = "happy",
    "don't care" = "apathetic",
    "felt stupid" = "shame",
    "feel like missing out" = "sad",
    "missed mom" = "sad",
    "felt unloved" = "sad",
    "feel like being ashamed of myself for making them disappointed" = "shame",
    "uncomfortable" = "shame",
    "feel bad for her" = "pity",
    "feel bad for what I did" = "shame",
    "not feeling good" = "upset",
    "worry" = "anxiety",
    "felt like I had no family" = "sad",
    "felt stupid and selfish" = "shame",
    "thinking why" = "curious",
    "feel like it's unfair" = "upset",
    "bored" = "bored",
    "half and half" = "upset",
    "happy" = "happy",
    "feel not worth it to my family" = "sad",
    "mad with crying" = "upset",
    "hate" = "anger",
    "felt unsafe" = "anxiety",
    "felt betrayed" = "upset",
    "alienation ('felt like he doesn't know me')" = "sad",
    "depressed" = "sad",
    "wanting to cry out" = "upset",
    "having fun" = "happy",
    "feel bad that mom was sad" = "shame",
    "feel bad to make my mom mad" = "shame",
    "feel guilty" = "shame",
    "crazy in love" = "happy"
  )

# Chuuk emotions

emotions2 <-
  c(
    "amwunwumun" = "upset",
    "netipengnaw" = "sad",
    "saew" = "shame",
    "osukosuk" = "upset",
    "teriterino chok faan" = "upset",
    "asaw" = "shame",
    "oput" = "upset",
    "tipew" = "upset",
    "ssong" = "anger",
    "metek" = "upset",
    "amam" = "upset",
    "weires" = "upset",
    "nemennemeno" = "shame",
    "omusano tipis" = "shame",
    "pwos" = "sad",
    "kechiew" = "sad",
    "kote pwan afani" = "apathetic",
    "omusano tipis" = "shame",
    "riafou" = "upset"
  )

# Combine english and chuuk emotions

combined_emotion_dict <- c(emotions, emotions2)

d$emotions_combined <- paste(d$English_emotion_terms, d$Chuukese_emotion_terms, sep = ',')
d$emotions_combined[d$emotions_combined == "0,0"] <- "unknown"

et <- str_split(d$emotions_combined, ",")
et2 <- str_trim(unlist(et))
et2b <- combined_emotion_dict[et2]
et3 <- na.omit(unique(et2b))

plot_emotions <- ggdotchart(table(et2b))
plot_emotions

mat_emotions <- matrix(0, ncol = nrow(d), nrow = length(et3), dimnames = list(et3, NULL))

for (i in 1:nrow(d)){
  for(j in combined_emotion_dict[str_trim(et[[i]])]){
    if(is.na(j)) next
    mat_emotions[j, i] <- 1
  }
}

d$yearssince_conflict <- d$age - d$age_conflict


# Heatmaps ----------------------------------------------------------------

# Heatmap of causes

aheatmap(
  mat_cause2,
  hclustfun = 'ward',
  scale = 'none',
  annCol = list(
    age = d$age, 
    ageconflict = d$age_conflict,
    sex = d$sex,
    location = d$location_conflict2,
    grip_z = d$grip_z, 
    high_severity = d$high_severity
    ),
  width = 10,
  filename = 'Figures/heatmap_conflicts.png',
  cexRow = 2
  )

# Heatmap of emotions

# Add in withhold_cooperation
tmp <- as.data.frame(t(mat_emotions))
tmp$withhold_cooperation <- d$withhold_cooperation

aheatmap(
  t(tmp),
  hclustfun = 'ward',
  annCol = list(
    age = d$age, 
    ageconflict = d$age_conflict,
    sex = d$sex,
    location = d$location_conflict2,
    grip_z = d$grip_z, 
    high_severity = d$high_severity
  ),
  filename = 'Figures/heatmap_emotions.png'
)

tmp2 <- tmp[colSums(tmp)>2]

cv.emotions_withhold_a0 <- cv.glmnet(
  as.matrix(tmp2[-11]), 
  tmp$withhold_cooperation, 
  family="binomial", 
  alpha = 0
  )
plot(cv.emotions_withhold_a0)
coef(cv.emotions_withhold_a0, s="lambda.min")

coefs <- coef(cv.emotions_withhold_a0, s="lambda.min")[-1,]
ggdotchart(coefs)

cv.emotions_withhold_a1 <- cv.glmnet(
  as.matrix(tmp2[-11]), 
  tmp$withhold_cooperation, 
  family="binomial", 
  alpha = 1
)
plot(cv.emotions_withhold_a1)
coef(cv.emotions_withhold_a1, s="lambda.min")

coefs <- coef(cv.emotions_withhold_a1, s="lambda.min")[-1,]
ggdotchart(coefs)


# Heatmap of behaviors

parent_behaviors <-
  c(
    'parent_aggression', 
    'parent_withhold_cooperation',
    'parent_partner_choice',
    'parent_cry',
    'parent_explain_lecture'
  )

d3 <- d[parent_behaviors]

child_behaviors <-
  c(
    'aggression', 
    'withhold_cooperation', 
    'submission', 
    'deception/withhold_information', 
    'partner_choice', 
    'negotiate_persuade_explain',
    'distract',
    'think_about',
    'pray',
    'seek_assistance',
    'child_cry'
    )

df_behaviors <- d[child_behaviors]

df_behaviors$child_apologizes <- ifelse(str_detect(d$apologize, 'child'), 1, 0)

cstrat <- c(
  'aggression' = 'Aggress',
  'seek_assistance' = 'Seek assistence',
  'submission' = 'Acquiese',
  'withhold_cooperation' = 'Withhold cooperation',
  'negotiate_persuade_explain' = 'Persuade',
  'deception/withhold_information' = 'Deceive',
  "partner_choice" = 'Partner choice',
  "distract" = 'Distract',  
  "think_about" = 'Reflect',
  "pray" = 'Pray', 
  "child_cry" = 'Cry',
  "child_apologizes" = 'Apologize'
)

names(df_behaviors) <- cstrat[names(df_behaviors)]

outcome_dict <- c(
  both = "Mixed",
  child = "Child",
  divided = "Mixed",
  neither = "Mixed",
  parent = "Parent",
  unknown = "Mixed"
)

aheatmap(
  t(df_behaviors),
  hclustfun = 'ward',
  scale = 'none',
  color = viridis(2),
  annCol = list(
    outcome = outcome_dict[d$outcome_favor],
    high_severity = factor(d$high_severity)
  ),
  annColors = list(
    outcome = c("orange", "lightblue", "darkgreen"),
    high_severity = c("white", "red")
  ),
  width = 20,
  cexRow = 2,
  legend = F,
  fontsize = 20,
  border_color = 'white',
  filename = 'Figures/heatmap_child_behaviors.png'
)

# elasticnet of outcome vs. strategy --------------------------------------

df_outcome <-
  d %>% 
  mutate(
    outcome = outcome_dict[outcome_favor]
  ) %>% 
  dplyr::select(
    outcome,
    all_of(childstrategy)
  )

# Ridge, alpha = 0
cv_outcome_a0 <- cv.glmnet(as.matrix(df_outcome[-1]), df_outcome$outcome, family="multinomial", alpha = 0)
plot(cv_outcome_a0)
coef(cv_outcome_a0, s="lambda.min")

plot_outcome_child <- ggdotchart(sort(coef(cv_outcome_a0, s="lambda.min")$Child[-1,])) + ggtitle("Outcome favors child")
plot_outcome_mixed <- ggdotchart(sort(coef(cv_outcome_a0, s="lambda.min")$Mixed[-1,])) + ggtitle("Outcome mixed")
plot_outcome_parent <- ggdotchart(sort(coef(cv_outcome_a0, s="lambda.min")$Parent[-1,])) + ggtitle("Outcome favors parent")

# Lasso, alpha = 1
cv_outcome_a1 <- cv.glmnet(as.matrix(df_outcome[-1]), df_outcome$outcome, family="multinomial", alpha = 1)
plot(cv_outcome_a1)
coef(cv_outcome_a1, s="lambda.min")

plot_outcome_child2 <- ggdotchart(sort(coef(cv_outcome_a1, s="lambda.min")$Child[-1,])) + ggtitle("Outcome favors child")
plot_outcome_mixed2 <- ggdotchart(sort(coef(cv_outcome_a1, s="lambda.min")$Mixed[-1,])) + ggtitle("Outcome mixed")
plot_outcome_parent2 <- ggdotchart(sort(coef(cv_outcome_a1, s="lambda.min")$Parent[-1,])) + ggtitle("Outcome favors parent")

# elasticnet of child strategies vs severity ------------------------------

df_severity_strategy <-
  d %>% 
  dplyr::select(
    severity,
    all_of(childstrategy)
  )

cv_severity_strategy_a1 <- cv.glmnet(as.matrix(df_severity_strategy[-1]), df_severity_strategy$severity, family="binomial", alpha = 1)
plot(cv_severity_strategy_a1)
cv_severity_strategy_a1 <- exp(coef(cv_severity_strategy_a1, s="lambda.min")[-1,])

names(cv_severity_strategy_a1) <- childstrategy_dict[names(cv_severity_strategy_a1)]

plot_severity_strategy_a1 <- 
  ggdotchart(sort(cv_severity_strategy_a1)) + 
  geom_vline(xintercept = 1, linetype = 'dotted') +
  scale_x_log10() +
  labs(x = "\nOdds ratio")
plot_severity_strategy_a1

df_severe_strategies <- 
  d %>% 
  dplyr::filter(severity == 'High') %>%  
  dplyr::select(all_of(childstrategy)) %>% 
  gather() %>% 
  dplyr::filter(value == 1)

tbl_severe_strat <- table(df_severe_strategies$key)

# Logistic of child vs. parent --------------------------------------------

df_outcome2 <-
  d %>% 
  mutate(
    outcome = outcome_dict[outcome_favor]
  ) %>% 
  dplyr::select(
    outcome,
    all_of(childstrategy),
    -submission
  ) %>% 
  dplyr::filter(outcome != 'Mixed') %>% 
  mutate(
    outcome = factor(outcome, levels = c('Parent', 'Child'))
  )

# Ridge, alpha = 0
cv_outcome_bin_a0 <- cv.glmnet(as.matrix(df_outcome2[-1]), df_outcome2$outcome, family="binomial", alpha = 0)
plot(cv_outcome_bin_a0)
outcome_coefs <- exp(coef(cv_outcome_bin_a0, s="lambda.min")[-1,])
names(outcome_coefs) <- childstrategy_dict[names(outcome_coefs)]

plot_outcome_child_bin <- 
  ggdotchart(outcome_coefs) + 
  geom_vline(xintercept = 1, linetype = 'dotted') +
  scale_x_log10(breaks=seq(0.7, 1.7, by = 0.1)) +
  labs(title = "Outcome favors child over parent", x = '\nOdds ratio')
plot_outcome_child_bin

# outcome child vs. parent or mixed

df_outcome3 <-
  d %>% 
  mutate(
    outcome = outcome_dict[outcome_favor]
  ) %>% 
  dplyr::select(
    outcome,
    # severity,
    all_of(childstrategy)
  ) %>% 
  mutate(
    # severity = as.numeric(severity) - 1,
    outcome = ifelse(outcome == 'Child', 'Child', 'Parent or mixed'),
    outcome = factor(outcome, levels = c('Parent or mixed', 'Child'))
  )

# Ridge, alpha = 0
cv_outcome_bin2_a0 <- cv.glmnet(as.matrix(df_outcome3[-1]), df_outcome3$outcome, family="binomial", alpha = 0)
plot(cv_outcome_bin2_a0)
coef(cv_outcome_bin2_a0, s="lambda.min")

plot_outcome_child_bin2 <- 
  ggdotchart(sort(exp(coef(cv_outcome_bin2_a0, s="lambda.min")[-1,]))) + 
  geom_vline(xintercept = 1, linetype = 'dotted') +
  scale_x_log10() +
  ggtitle("Outcome favors child")
plot_outcome_child_bin2

# Outcomes vs. severity

plot_outcome_severity <-
  d %>% 
  mutate(
    outcome = outcome_dict[outcome_favor],
    outcome = factor(outcome, levels = c('Parent', 'Mixed', 'Child'))
  ) %>% 
  dplyr::select(
    outcome,
    severity,
    all_of(childstrategy)
  ) %>% 
ggplot() +
  geom_mosaic(aes(x=product(outcome, severity), fill = outcome)) +
  scale_fill_manual('Outcome favors', values = c('dodgerblue3', 'darkgreen', 'gold')) +
  # guides(fill=guide_legend(title="Severity")) +
  labs(x="\nSeverity", y = "") +
  theme_minimal(15)
  # theme(legend.position="none", axis.text.y=element_blank())
plot_outcome_severity

# PCA ---------------------------------------------------------------------

combo <- cbind(df_behaviors, t(mat_cause), t(mat_emotions))
m <- prcomp(combo, scale. = F)
summary(m)
plot(m)
pca_loadings_plot(m)


# Tables
xtabs(~sex+aggression, d)
xtabs(~sex+withhold_cooperation,d)
xtabs(~sex+withhold_cooperation_type,d)

# Age at interview
ggplot(d, aes(factor(aggression), age)) + geom_violin() + geom_point()
ggplot(d, aes(factor(withhold_cooperation), age)) + geom_violin() + geom_point()

# Age at conflict
ggplot(d, aes(factor(aggression), age_conflict)) + geom_violin() + geom_point()
ggplot(d, aes(factor(withhold_cooperation), age_conflict)) + geom_violin() + geom_point()

# Grip strength
ggplot(d, aes(age, mean_handgrip, colour = sex)) + geom_point() + geom_smooth(method='lm')

ggplot(d, aes(factor(sex), age)) + geom_violin() + geom_point()
ggplot(d, aes(factor(sex), age)) + geom_violin() + geom_point()


combo$outcome_favor2 <- ifelse(d$outcome_favor %in% c('child', 'parent'), d$outcome_favor, 'both/neither/NA')
autoplot(m, data = combo, colour = 'outcome_favor2', loadings = T, loadings.label = T, frame = TRUE, frame.type = 'norm')

# Just behaviors and emotions

combo2 <- cbind(df_behaviors, t(mat_emotions))
m <- prcomp(combo2, scale. = F)
summary(m)
plot(m)
pca_loadings_plot(m)
combo2$outcome_favor2 <- ifelse(d$outcome_favor %in% c('child', 'parent'), d$outcome_favor, 'both/neither/NA')
combo2$severity <- d$high_severity
autoplot(m, data = combo2, colour = 'severity', loadings = T, loadings.label = T)

# Parent and child behaviors
combo3 <- cbind(df_behaviors, d3)
m <- prcomp(combo3, scale. = F)
summary(m)
plot(m)
pca_loadings_plot(m)
combo3$outcome_favor2 <- ifelse(d$outcome_favor %in% c('child', 'parent'), d$outcome_favor, 'both/neither/NA')
autoplot(m, data = combo2, colour = 'outcome_favor2', loadings = T, loadings.label = T, frame = TRUE, frame.type = 'norm')


# Use of aggression -------------------------------------------------------

plot(xtabs(~aggression+parent_aggression, d))

m <- glm(aggression ~ age_conflict + parent_aggression, family = binomial, d)
summary(m)

plot(xtabs(~withhold_cooperation+parent_withhold_cooperation, d))
plot(xtabs(~withhold_cooperation+parent_aggression, d))
plot(xtabs(~aggression+parent_withhold_cooperation, d))

vars <- c("aggression", "withhold_cooperation", "parent_aggression", "parent_withhold_cooperation")
d2 <- d[vars]
heatmap(t(as.matrix(d2)), scale='none')

aheatmap(
  mat_cause2,
  hclustfun = 'ward',
  scale = 'none',
  annCol = list(
    age = d$age, 
    ageconflict = d$age_conflict,
    sex = d$sex,
    location = d$location_conflict2,
    grip_z = d$grip_z, 
    high_severity = d$high_severity
  ),
  width = 10,
  filename = 'Figures/heatmap_conflicts.png',
  cexRow = 2
)

aheatmap(
  t(agg_matrix),
  hclustfun = 'ward',
  scale = 'none',
  color = viridis(2),
  annCol = list(
    high_severity = d$high_severity
  ),
  annColors = list(
    high_severity = c("white", "red")
  ),
  width = 20,
  filename = 'Figures/heatmap_agg_conflicts.png',
  cexRow = 2,
  legend = F,
  annLegend = F,
  border_color = 'white'
)

# ggheatmap(
#   t(agg_matrix),
#   hclustmethod = 'ward.D'
#   )

d2 <- d[c('Id_conflict', 'high_severity')]
d2$high_severity <- ifelse(d2$high_severity, 'High', 'Low')
hagenheat(t(agg_matrix), ann_col = d2) + 
  scale_fill_manual(values=c('red', 'darkgreen')) +
  theme_minimal(15) +
  theme(axis.text.x = element_blank())

# Manually sort rows
rowvec <- c(
  "social or cultural norms",
  "fighting or aggression outside the home",
  "religion",
  "resource allocation",
  "work",
  "safety",
  "activities with friends",
  "education",
  "family responsibilities",
  "time away from home or family",
  "mateship",
  "family or home tension"
)

mat <- t(agg_matrix)
mat <- mat[rowvec,]
mat2 <- mat[,order(mat[12,]==0, mat[11,]==0, mat[10,]==0, mat[9,]==0, mat[8,]==0, mat[7,]==0)]
hagenheat(mat2, seriation_method = 'Identity', ann_col = d2) + 
  scale_fill_manual(values=c('red', 'darkgreen')) +
  theme_minimal(15) +
  theme(axis.text.x = element_blank())

mat3 <- mat[,order(d2$high_severity=='Low', mat[12,]==0, mat[11,]==0, mat[10,]==0, mat[9,]==0, mat[8,]==0, mat[7,]==0)]
hagenheat(mat3, seriation_method = 'Identity', ann_col = d2) + 
  scale_fill_manual(values=c('red', 'darkgreen')) +
  theme_minimal(15) +
  theme(axis.text.x = element_blank())

# Sankey/alluvial plot ----------------------------------------------------

parentstrategy <- c("parent_aggression", "parent_withhold_cooperation")
outcomes <- c("outcome_favor")

dfriver <- d %>% 
  dplyr::select(Id_participant, all_of(c(childstrategy, parentstrategy, outcomes))) %>% 
  dplyr::filter(outcome_favor %in% c('child', 'parent')) %>% 
  gather(key = childstrategy, value = childused, all_of(childstrategy)) %>% 
  dplyr::filter(childused == 1) %>% 
  gather(key = parentstrategy, value = parentused, all_of(parentstrategy)) %>% 
  dplyr::filter(parentused == 1) %>% 
  dplyr::select(-childused, -parentused) %>% 
  group_by(childstrategy, parentstrategy, outcome_favor) %>% 
  summarise(Freq = n())

# plot_alluvial <-
#   ggplot(dfriver,
#          aes(y = Freq, axis1 = parentstrategy, axis2 = childstrategy, axis3 = outcome_favor)) +
#   geom_alluvium(aes(fill = childstrategy)) +
#   geom_stratum(width = 1/12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", label.strata = TRUE) +
#   # scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   ggtitle("Chuuk parent-child conflicts") +
#   theme_bw()
# plot_alluvial

# Now with ggforce

# Recode stuff for nicer plot

pstrat <- c(
  'parent_aggression' = 'Aggression',
  'parent_withhold_cooperation' = 'Withhold cooperation'
)

dfriver2 <-
  dfriver %>%
  dplyr::ungroup() %>% 
  mutate(
    parentstrategy = pstrat[parentstrategy],
    childstrategy = cstrat[childstrategy],
    outcome_favor = str_to_sentence(outcome_favor)
  ) %>% 
  rename(
    `Parent strategy` = parentstrategy,
    `Child strategy` = childstrategy,
    `Outcome favors` = outcome_favor,
    value = Freq
    ) %>%
  gather_set_data(1:3) %>% 
  mutate(
    x = factor(x, levels = c('Parent strategy', 'Child strategy', 'Outcome favors'))
  )

plot_parallel_sets <-
  ggplot(dfriver2, aes(x, id = id, split = y, value = value)) + 
    geom_parallel_sets(aes(fill = `Child strategy`), alpha = 0.3, axis.width = 0.1) +
    geom_parallel_sets_axes(axis.width = 0.1) +
    geom_parallel_sets_labels(colour = 'white') +
    scale_x_discrete(expand = expand_scale(mult = c(.05, .05))) +
    labs(x = '', y = 'Number of conflicts\n') +
    theme_minimal(15)
plot_parallel_sets

# Use different categorical vars

dfriver3 <- d %>% 
  dplyr::select(Id_participant, Severity = severity, all_of(c(childstrategy, outcomes))) %>% 
  dplyr::filter(outcome_favor %in% c('child', 'parent')) %>% 
  gather(key = childstrategy, value = childused, all_of(childstrategy)) %>% 
  dplyr::filter(childused == 1) %>% 
  dplyr::select(-childused) %>% 
  group_by(Severity, childstrategy, outcome_favor) %>% 
  summarise(value = n())

dfriver4 <-
  dfriver3 %>%
  dplyr::ungroup() %>% 
  mutate(
    childstrategy = cstrat[childstrategy],
    outcome_favor = str_to_sentence(outcome_favor)
  ) %>% 
  rename(
    `Child strategy` = childstrategy,
    `Outcome favors` = outcome_favor
  ) %>%
  gather_set_data(1:3) %>% 
  mutate(
    x = factor(x, levels = c('Severity', 'Child strategy', 'Outcome favors'))
  )

plot_parallel_sets2 <-
  ggplot(dfriver4, aes(x, id = id, split = y, value = value)) + 
  geom_parallel_sets(aes(fill = Severity), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white') +
  scale_fill_manual(values = c('darkgreen', 'red')) +
  scale_x_discrete(expand = expand_scale(mult = c(.05, .05))) +
  labs(x = '', y = 'Number of conflicts\n') +
  theme_minimal(15)
plot_parallel_sets2

# Strategy and outcome predictors ---------------------------------------------------

msub <- glm(submission ~ age_conflict + I(location_conflict2 == "US mainland"), family = binomial, d)
summary(msub)

magress <- glm(aggression ~ I(location_conflict2 == "US mainland"), family = binomial, d)
summary(magress)

mfavor <-
  d %>% 
  dplyr::filter(outcome_favor %in% c('child', 'parent', 'divided')) %>%
  mutate(
    outcome_favor = factor(outcome_favor),
    sex = factor(sex)
  ) %>% 
  polr(outcome_favor ~ age_conflict, Hess = T, .)
summary(mfavor)
# plot(allEffects(mfavor))


# Conflicts vs strategies -------------------------------------------------

d$child_apologizes <- ifelse(str_detect(d$apologize, 'child'), 1, 0)

con_strat <- 
  as.data.frame(agg_matrix) %>% 
  mutate(
    Id_conflict = rownames(agg_matrix)
  ) %>% 
  left_join(d[c('Id_conflict', childstrategy, 'child_apologizes')]) %>% 
  dplyr::select(-Id_conflict) %>% 
  as.matrix

heatmap(t(con_strat), scale='none', hclustfun = function(x) hclust(x, method = 'ward.D'))

aheatmap(
  t(con_strat),
  hclustfun = 'ward',
  distfun = 'euclidean',
  scale = 'none',
  width = 10,
  filename = 'Figures/conflicts_strategies.png',
  cexRow = 2
)


# Withhold cooperation ----------------------------------------------------

withhold_dict <- c(
  "0" = NA,
  "avoidance" = "Withhold cooperation",
  "avoidance,broke rules" = "Withhold cooperation",
  "broke rules" = "Withhold cooperation",
  "don't want to do work" = "Withdrawal",
  "ignore" = "Withhold cooperation",
  "move away" = "Withhold cooperation",
  "move away,avoidance" = "Withhold cooperation",
  "run away" = "Withhold cooperation",
  "run away,  stay with family, move away" = "Withhold cooperation",
  "run away,stay with family" = "Withhold cooperation",
  "school_droppedout" = "Withhold cooperation",
  "stay with family,avoidance" = "Withhold cooperation",
  "stay with family,avoidance,run away" = "Withhold cooperation",
  "withdraw" = "Withdrawal",
  "withdraw,keep playing anyway" = "Withhold cooperation",
  "withdraw(but not withholding cooperation per se)" = "Withdrawal"
)

d$withhold_cooperation_type2 <- factor(withhold_dict[d$withhold_cooperation_type])
table(d$withhold_cooperation_type2)

d$withhold_cooperation2 <- d$withhold_cooperation_type2 == 'Withhold cooperation'

# No sex differences
summary(xtabs(~withhold_cooperation_type2 + sex, d))

# Severity

plot(xtabs(~severity + withhold_cooperation, d))
summary(xtabs(~severity + withhold_cooperation, d))

m_severity_withhold <- glmer(
  withhold_cooperation ~ 
    severity + 
    (1|familyid),
    family = binomial,
    data = d,
  glmerControl(optimizer = 'bobyqa')
  )
summary(m_severity_withhold)

plot_severity_withhold <-
  visreg(m_severity_withhold, xvar="severity", scale = 'response', band = T, jitter = T, gg=T) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = '\nConflict severity', y = 'Withhold cooperation\n') +
  theme_bw()
plot_severity_withhold

plot(xtabs(~high_severity + withhold_cooperation2, d))
summary(xtabs(~high_severity + withhold_cooperation2, d))

# glm fits better than glmer
m_severity_age <-
  glm(
    severity ~
      age +
      age_conflict +
      # conflict_duration +
      BIOtotalsiblings,
    family = binomial,
    data = d
  )
summary(m_severity_age)

plot_severity_age <-
  visreg(m_severity_age, xvar="age_conflict", scale = 'response', band = T, jitter = T, gg=T) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = '\nAge at time of conflict', y = 'Conflict severity\n') +
  theme_bw(15)
plot_severity_age

# 2D plot

plot_severity_age2 <-
  visreg2d(
    m_severity_age, 
    xvar="age_conflict", 
    yvar='BIOtotalsiblings', 
    scale = 'response', 
    plot.type='gg'
  ) + 
    geom_jitter(data=d, aes(age_conflict, BIOtotalsiblings, colour=severity, shape = severity), size = 3) + 
    scale_fill_viridis("Probability", alpha=.75) +
    scale_colour_manual(values = c('black', 'red')) +
    scale_x_continuous(limits = c(9,26), expand = expand_scale(mult = c(0, 0))) + 
    scale_y_continuous(limits=c(-1, 23)) +
    labs(x = '\nAge at time of conflict (years)', y = 'Number of biological siblings\n', color="Severity", shape="Severity") +
    theme_minimal(15)
plot_severity_age2

# scaled

m_severity_age_z <- 
  d %>% 
  dplyr::select(
    severity,
    age,
    age_conflict,
    BIOtotalsiblings
  ) %>% 
  mutate_at(vars(-severity), function(x) as.vector(scale(x))) %>%
  glm(
    severity ~
      age +
      age_conflict +
      BIOtotalsiblings,
    family = binomial,
    data = .
  ) 
summary(m_severity_age_z)

m_severity_OR <- exp(coef(m_severity_age_z)[-1])

table_severity_age_z <-
  m_severity_age_z %>% 
  logistic_forestplot(
    odds.ratio = T,
    intercept = F,
    varnames = c('age' = 'Age at interview (Z)', 'age_conflict' = 'Age at conflict (Z)', 'BIOtotalsiblings' = 'Number of siblings (Z)')
    )

names(table_severity_age_z$table) <- c("", "")

# glmnet

df_severity <- 
  d %>% 
  dplyr::select(
    severity,
    sex,
    age_conflict,
    age,
    mean_handgrip,
    BIOtotalsiblings,
    parent_conflict2
  ) %>% 
  mutate(
    sex = ifelse(sex == 'f', 0, 1),
    parent_conflict2 = ifelse(parent_conflict2 == 'Biological', 0, 1)
  ) %>% 
  na.omit

cv_severity_a0 <- cv.glmnet(as.matrix(df_severity[-1]), df_severity$severity, family="binomial", alpha = 0)
plot(cv_severity_a0)
coef(cv_severity_a0, s="lambda.min")
ggdotchart(sort(coef(cv_severity_a0, s="lambda.min")[-1,]))

cv_severity_a1 <- cv.glmnet(as.matrix(df_severity[-1]), df_severity$severity, family="binomial", alpha = 1)
plot(cv_severity_a1)
coef(cv_severity_a1, s="lambda.min")
ggdotchart(sort(coef(cv_severity_a1, s="lambda.min")[-1,]))

# Which causes predict severity

df_agg <- as.data.frame(agg_matrix)
df_agg$severity <- ifelse(d$severity == 'No evidence', 'No evidence', 'High severity conflict')
df_agg$severity <- factor(df_agg$severity, levels = c('No evidence', 'High severity conflict'))

cv_causes_a0 <- cv.glmnet(as.matrix(df_agg[-13]), df_agg$severity, family="binomial", alpha = 0)
plot(cv_causes_a0)
coef(cv_causes_a0, s="lambda.min")
ggdotchart(sort(coef(cv_causes_a0, s="lambda.min")[-1,]))

cv_causes_a1 <- cv.glmnet(as.matrix(df_agg[-13]), df_agg$severity, family="binomial", alpha = 1)
plot(cv_causes_a1)
coef(cv_causes_a1, s="lambda.min")
ggdotchart(sort(coef(cv_causes_a1, s="lambda.min")[-1,]))

# Remove family or home tension because confounded with severity

df_agg2 <- df_agg
df_agg2$`family or home tension` <- NULL
cv_causes_a0_2 <- cv.glmnet(as.matrix(df_agg2[-12]), df_agg2$severity, family="binomial", alpha = 1)
plot(cv_causes_a0_2)
coef(cv_causes_a0_2, s="lambda.min")

plot_severity_lasso <- 
  ggdotchart(sort(exp(coef(cv_causes_a0_2, s="lambda.min")[-1,]))) + 
  geom_vline(xintercept = 1, linetype='dotted') +
  scale_x_log10()
plot_severity_lasso
ggsave("Figures/plot_severity.pdf", plot_severity_lasso, width=9, height=5)

# Cross validate alpha

cv_causes <- cva.glmnet(as.matrix(df_agg2[-12]), df_agg$severity, family="binomial")
plot(cv_causes)



# Sample size is too small
# m_withdrawal <- polr(withhold_cooperation_type2 ~ age_conflict, Hess = T, d)
# plot(allEffects(m_withdrawal))

#Modeling withhold cooperation

plot_severity_withhold2 <-
  d %>%
  mutate(
    withhold_cooperation = factor(withhold_cooperation, levels = c('0', '1'), labels = c('No', 'Yes')),

  ) %>%
  ggplot() +
  geom_mosaic(aes(x=product(severity, withhold_cooperation3), fill = severity)) +
    scale_fill_manual(values = c('darkgreen', 'red')) +
    # guides(fill=guide_legend(title="Severity")) +
    labs(x="\nWithhold cooperation", y = "") +
    theme_minimal(15)
    # theme(legend.position="none", axis.text.y=element_blank())
plot_severity_withhold2

clrs <- c('darkgreen','navy')

plot_parent_conflict <-
  d %>% 
  mutate(
    parent_conflict2 = factor(parent_conflict2, levels = c('Biological', 'Adoptive'))
  ) %>% 
  ggplot() + 
  geom_mosaic(aes(x=product(parent_conflict2, parent_conflict3), fill = parent_conflict2)) +
  scale_fill_manual(values = clrs) +
  guides(fill=guide_legend(title="Parent type")) +
  labs(x="", y="") +
  # facet_wrap(~severity) +
  theme_minimal(15) +
  theme(legend.position="top")
plot_parent_conflict

plot_severity_tension <-
  df_agg %>% 
  mutate(
    tension = ifelse(`family or home tension` == 0, 'Low family tension', 'High family tension'),
    tension = factor(tension, levels = c('Low family tension', 'High family tension')),
    ) %>% 
  ggplot() +
    geom_mosaic(aes(x=product(severity, tension), fill = severity)) +
    scale_fill_manual(values = c('darkgreen', 'red')) +
    # guides(fill=guide_legend(title="Severity")) +
    labs(x="", y="") +
    theme_minimal(15) +
    theme(legend.position="none")
plot_severity_tension

# The conflicts that are family or home tension:
names(agg_dict[agg_dict=='family or home tension'])

df_withhold <- d %>% 
  dplyr::select(
    withhold_cooperation2,
    sex,
    age_conflict,
    age,
    mean_handgrip,
    BIOtotalsiblings,
    parent_conflict2
  ) %>% 
  mutate(
    sex = ifelse(sex == 'f', 0, 1),
    parent_conflict2 = ifelse(parent_conflict2 == 'Biological', 0, 1)
  ) %>% 
  na.omit

cv_withhold_a0 <- cv.glmnet(as.matrix(df_withhold[-1]), df_withhold$withhold_cooperation2, family="binomial", alpha = 0)
plot(cv_withhold_a0)
coef(cv_withhold_a0, s="lambda.min")

cv_withhold_a1 <- cv.glmnet(as.matrix(df_withhold[-1]), df_withhold$withhold_cooperation2, family="binomial", alpha = 1)
plot(cv_withhold_a1)
coef(cv_withhold_a1, s="lambda.min")

# Mixed effect model of withholding cooperation

m_withhold <- glmer(withhold_cooperation2 ~ BIOtotalsiblings + (1|Id_participant), family = binomial, d)
summary(m_withhold)

df_outcome4 <- 
  d %>% 
  dplyr::filter(
    outcome_favor %in% c('child', 'parent', 'divided'),
    BIOtotalsiblings < 20, # outlier
    ) %>% 
  mutate(
    outcome_favor3 = case_when(
      outcome_favor == 'child' ~ -1,
      outcome_favor == 'divided' ~ 0,
      outcome_favor == 'parent' ~ 1
    )
  )

m_outcome <- lmer(outcome_favor3 ~ age_conflict + sex + BIOtotalsiblings + (1|Id_participant), df_outcome4)
summary(m_outcome)
visreg(m_outcome)


# Seek assistance ---------------------------------------------------------

seek_dict <- c(
  "0" = NA,
  "aunt" = "Aunts and uncles",
  "aunts and uncles" = "Aunts and uncles",
  "biological father" = "Parents",
  "brother,maternal uncles,paternal uncle" = "Misc. kin",
  "father" = "Parents",
  "friend" = "Friends",
  "friend,godmother" = "Friends",
  "friends" = "Friends",
  "male acquaintance" = "Friends",
  "maternal aunt" = "Aunts and uncles",
  "maternal grandmother" = "Grandparent",
  "maternal grandmother,uncle" = "Misc. kin",
  "maternal grandparents,sister,maternal cousin,friend" = "Misc. kin",
  "maternal uncles" = "Aunts and uncles",
  "mother" = "Parents",
  "mother,friends" = "Kin and non-kin",
  "paternal aunt,cousin,friend" = "Kin and non-kin",
  "paternal uncle" = "Aunts and uncles",
  "siblings" = "Siblings",
  "sister" = "Siblings",
  "sister,same sex cousins" = "Misc. kin",
  "sisters" = "Siblings"
)

d$seek_assistance_who2 <- seek_dict[d$seek_assistance_who]
table(d$seek_assistance_who2)
xtabs(~seek_assistance_who2 + sex, d)
dotchart(sort(table(d$seek_assistance_who2)))


# Sex differences in numbers of conflicts ---------------------------------

df_conflicts <- d %>% 
  group_by(sex, Id_participant) %>% 
  summarise(num_conflicts = n()) 

xtabs(~num_conflicts + sex, df_conflicts)

mean_conficts <- df_conflicts %>% 
  group_by(sex) %>% 
  summarise(mean_num_conficts = mean(num_conflicts))


# Summary stats -----------------------------------------------------

sum_vars = c(
  age = "Age at interview",
  mean_age_conflict = "Mean age at time of conflict",
  number_conflicts = "Total number of conflicts",
  severe_conflicts = "Number of severe conflicts",
  mean_conflict_duration = "Duration of conflict (years)"
)

participant_summary_tbl <- 
  d %>% 
  dplyr::select(
    Id_participant,
    sex,
    age,
    age_conflict,
    conflict_duration,
    high_severity
    # location_conflict,
    # parent_conflict2
  ) %>% 
  group_by(Id_participant) %>% 
  summarise(sex = sex[1], age = age[1], number_conflicts = n(), severe_conflicts = mean(high_severity), mean_age_conflict = mean(age_conflict), mean_conflict_duration = mean(conflict_duration, na.rm=T)) %>%
  custom.summarize(vars = sum_vars, facvar = 'sex', statscol = F)

plot_conflict_summary <- 
  d %>% 
  mutate(
    year_conflict = birthyear + age_conflict
  ) %>% 
  ggplot() + 
    geom_point(
      aes(x=year_conflict, y = location_conflict2),
      size = 3) +
    labs(x = "\nYear of conflict", y = "") +
    # guides(colour = "none") +
    theme_minimal(15) +
    theme(plot.margin = margin(t = 0, r = 1, b = 0, l = 0, unit = "cm"))
plot_conflict_summary
  
tbl_numcon <- table(table(d$Id_participant))

tmp <- unique(d[c('Id_participant', 'sex')])
tbl_sex <- table(tmp$sex)
tbl_loc <- table(d$location_conflict2)
