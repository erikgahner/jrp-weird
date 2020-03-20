###
## Article:   Just as WEIRD? 
##            Personality Traits and Political Attitudes Among Immigrant Minorities
## 
##            Journal of Research in Personality
##
##            Joseph A. Vitriol      Erik Gahner Larsen        Steven G. Ludeke
##
##        
## Data:      LISS-I panel: https://www.dataarchive.lissdata.nl/study_units/view/162
##
###

library("tidyverse")
library("rstatix") 
library("magrittr")
library("stargazer")

theme_set(
  theme_grey(base_size = 11.5) %+replace% 
    theme(
      plot.margin = unit(rep(0.5, 4), "cm"), plot.background = element_blank(), panel.background = element_blank(),
      panel.border = element_blank(), legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA), legend.title = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_line(linetype = "dotted", colour = "#757575", size = 0.3), panel.grid.minor = element_blank(),
      axis.ticks = element_blank(), axis.line = element_line(color = "#FFFFFF", size = 0.3),
      plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
      plot.caption = element_text(size = 10, colour = "#212121", margin = margin(t = 15)),
      axis.title = element_text(size = 11, face = "plain"), axis.text = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 10), strip.text = element_text(size = 12, face = "plain")
    )
)

imm <- read_csv("immWEIRD.csv")

get_correlation <- function(x, y, what, who) {
  imm %>% filter(origin == who) %>% select(x, y) %>% cor_test(.) %>% select(what) %>% as.numeric() 
}

imm_correlations <- crossing(origin = c(0, 101, 102, 201, 202), 
                             trait = c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism"),
                             outcome = c("ideology", "stfdem", "poltr", "involvement", "polintr", "media", "poleff", "antiimm", "eu", "polpar"),
                             est = NA,
                             est_low = NA,
                             est_high = NA
)


for (i in unique(imm_correlations$origin)) {
  for(j in unique(imm_correlations$trait)) {
    for(k in unique(imm_correlations$outcome)) {
      imm_correlations$est[imm_correlations$origin == i & imm_correlations$trait == j & imm_correlations$outcome == k] <- get_correlation(k, j, "cor", i)
      imm_correlations$est_low[imm_correlations$origin == i & imm_correlations$trait == j & imm_correlations$outcome == k] <- get_correlation(k, j, "conf.low", i)
      imm_correlations$est_high[imm_correlations$origin == i & imm_correlations$trait == j & imm_correlations$outcome == k] <- get_correlation(k, j, "conf.high", i)
    }
  }
}

imm_correlations <- imm_correlations %>%
  mutate(groupName = case_when(
    origin == 0 ~ "Dutch",
    origin == 101 ~ "1st gen.,\nWestern",
    origin == 102 ~ "1st gen.,\nnon-Western",
    origin == 201 ~ "2nd gen.,\nWestern",
    origin == 202 ~ "2nd gen.,\nnon-Western"
  ),
  traitName = case_when(
    trait == "openness" ~ "Openness",
    trait == "conscientiousness" ~ "Conscientiousness",
    trait == "extraversion" ~ "Extraversion",
    trait == "agreeableness" ~ "Agreeableness",
    trait == "neuroticism" ~ "Neuroticism"
  ),
  outcomeName = case_when(
    outcome == "ideology" ~ "Ideology",
    outcome == "stfdem" ~ "Sat. democracy",
    outcome == "polintr" ~ "Interest",
    outcome == "poleff" ~ "Efficacy",
    outcome == "involvement" ~ "Involvement",
    outcome == "polpar" ~ "Participation",
    outcome == "media" ~ "Media use",
    outcome == "poltr" ~ "Political trust",
    outcome == "antiimm" ~ "Anti-immigration",
    outcome == "eu" ~ "EU integration"
  ))

imm %>% 
  filter(origin == 0) %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive-dutch.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, Dutch",
            digits = 2, type="text")

imm %>% 
  filter(origin == 101) %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive-fgW.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, 1st gen, Western",
            digits = 2, type="text")

imm %>% 
  filter(origin == 102) %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive-fgNW.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, 1st gen, non-Western",
            digits = 2, type="text")

imm %>% 
  filter(origin == 201) %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive-sgW.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, 2nd gen. Western sample",
            digits = 2, type="text")

imm %>% 
  filter(origin == 202) %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive-sgNW.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics, 2nd gen. non-Western sample",
            digits = 2, type="text")


imm_cor_fig <- imm_correlations %>%
  ggplot(., aes(x = traitName, y = est, ymin = est_low, ymax = est_high, group = groupName, colour = groupName, shape = groupName)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0, position = position_dodge(0.5)) +
  facet_wrap(~ outcomeName, ncol = 2) +
  scale_shape_manual(values = c(16,1,18,2,17)) +
  labs(x = "", y = "") +
  scale_colour_brewer(palette="Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom") 


imm_cor_fig %T>%
  ggsave("fig2.pdf", plot = ., height=9, width=8) %T>%
  ggsave("fig2.png", plot = ., height=9, width=8)


imm %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  data.frame() %>% 
  stargazer(out="tab-descriptive.htm",
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            median = TRUE, iqr = TRUE,
            title = "Summary statistics",
            digits = 2, type="text")

imm %>% 
  select(dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>%
  cor(., use="pairwise.complete.obs") %>%
  stargazer(., 
            covariate.labels = c("Dutch", "First generation", "Non-Western", "Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism", 
                                 "Ideology", "Interest", "Efficacy", "Involvement", "Participation", "Media use", "Political trust", "Satisfaction democracy", "Anti-immigration", "EU integration"),
            type="text", digits = 2, out="tab-correlations.htm")


for (i in c("stfdem")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Satisfaction with democracy",
          out = "tab-reg-stfdem.htm")

for (i in c("poltr")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Political trust",
          out = "tab-reg-poltr.htm")

for (i in c("ideology")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Ideology",
          out = "tab-reg-ideology.htm")

for (i in c("involvement")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Political involvement",
          out = "tab-reg-involvement.htm")

for (i in c("polintr")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Political interest",
          out = "tab-reg-polintr.htm")

for (i in c("media")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Media consumption",
          out = "tab-reg-media.htm")


for (i in c("poleff")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Political efficacy",
          out = "tab-reg-poleff.htm")

for (i in c("polpar")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Political participation",
          out = "tab-reg-polpar.htm")

for (i in c("antiimm")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: Anti-immigration",
          out = "tab-reg-antiimm.htm")

for (i in c("eu")) {
  model1 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*openness")), data = imm)
  model2 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*conscientiousness")), data = imm)
  model3 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*extraversion")), data = imm)
  model4 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*agreeableness")), data = imm)
  model5 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + nw*neuroticism")), data = imm)
  model6 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*openness")), data = imm)
  model7 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*conscientiousness")), data = imm)
  model8 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*extraversion")), data = imm)
  model9 <- lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*agreeableness")), data = imm)
  model10 <-lm(as.formula(paste(i,"~ factor(herkomstgroep) + fg*neuroticism")), data = imm)
}

stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10, 
          covariate.labels = c("First generation foreign, Western background", "First generation foreign, non-Western background",
                               "Second generation foreign, Western background", "Second generation foreign, non-Western background",
                               "nw", "fg", 
                               "Openness", "Openness × Non-Western",
                               "Conscientiousness", "Conscientiousness × Non-Western",
                               "Extraversion", "Extraversion × Non-Western",
                               "Agreeableness", "Agreeableness × Non-Western",
                               "Neuroticism", "Neuroticism × Non-Western",
                               "Openness × First generation",
                               "Conscientiousness × First generation",
                               "Extraversion × First generation",
                               "Agreeableness × First generation",
                               "Neuroticism × First generation"),
          type = "text", dep.var.labels.include = FALSE, 
          column.sep.width = "0pt",
          no.space = TRUE,
          digits = 2,
          title = "Results: EU integration",
          out = "tab-reg-eu.htm")

get_correlation_all <- function(x, y, what) {
  imm %>% select(x, y) %>% cor_test(.) %>% select(what) %>% as.numeric() 
}

imm_correlations_all <- crossing(trait = c("openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism"),
                                 outcome = c("ideology", "stfdem", "poltr", "involvement", "polintr", "media", "poleff", "antiimm", "eu", "polpar"),
                                 est = NA,
                                 est_low = NA,
                                 est_high = NA
)

for(j in unique(imm_correlations_all$trait)) {
  for(k in unique(imm_correlations_all$outcome)) {
    imm_correlations_all$est[imm_correlations_all$trait == j & imm_correlations_all$outcome == k] <- get_correlation_all(k, j, "cor")
    imm_correlations_all$est_low[imm_correlations_all$trait == j & imm_correlations_all$outcome == k] <- get_correlation_all(k, j, "conf.low")
    imm_correlations_all$est_high[imm_correlations_all$trait == j & imm_correlations_all$outcome == k] <- get_correlation_all(k, j, "conf.high")
  }
}

imm_correlations_all <- imm_correlations_all %>%
  mutate(traitName = case_when(
    trait == "openness" ~ "Openness",
    trait == "conscientiousness" ~ "Conscientiousness",
    trait == "extraversion" ~ "Extraversion",
    trait == "agreeableness" ~ "Agreeableness",
    trait == "neuroticism" ~ "Neuroticism"
  ),
  outcomeName = case_when(
    outcome == "ideology" ~ "Ideology",
    outcome == "stfdem" ~ "Sat. democracy",
    outcome == "polintr" ~ "Interest",
    outcome == "poleff" ~ "Efficacy",
    outcome == "involvement" ~ "Involvement",
    outcome == "polpar" ~ "Participation",
    outcome == "media" ~ "Media use",
    outcome == "poltr" ~ "Political trust",
    outcome == "antiimm" ~ "Anti-immigration",
    outcome == "eu" ~ "EU integration"
  ))


imm_cor_fig_all <- imm_correlations_all %>%
  ggplot(., aes(x = traitName, y = est, ymin = est_low, ymax = est_high)) +
  geom_hline(yintercept = 0, colour = "gray") +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(width = 0, position = position_dodge(0.5)) +
  facet_wrap(~ outcomeName, ncol = 5) +
  labs(x = "", y = "") +
  scale_colour_brewer(palette="Set1") +
  theme(legend.position = "bottom")  +
  coord_flip()


imm_cor_fig_all %T>%
  ggsave("fig1.pdf", plot = ., height=4, width=9) %T>%
  ggsave("fig1.png", plot = ., height=4, width=9)


# Create and save sessionInfo.txt
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
