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
library("conflicted")
library("haven")
library("psych")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Create function to standardize variables
two_sd <- function(x) {
  return((x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE)))
}

imm.b <- read_dta("~/Google Drev/data/liss/immigrant/avars_201312_EN_1.0p.dta")
imm.pe <- read_dta("~/Google Drev/data/liss/immigrant/ep14b_EN_1.0p.dta")
imm.po <- read_dta("~/Google Drev/data/liss/immigrant/ka13a_EN_1.0p.dta")
imm <- merge(imm.b, imm.po, by="nomem_encr")
imm <- merge(imm, imm.pe, by="nomem_encr")

imm <- imm %>%
  mutate(
    origin = herkomstgroep,
    dutch = ifelse(herkomstgroep == 0, 1, 0),
    # non-Western
    nw = case_when(herkomstgroep == 0 ~ 0,
                        herkomstgroep == 101 ~ 0,
                        herkomstgroep == 102 ~ 1,
                        herkomstgroep == 201 ~ 0,
                        herkomstgroep == 202 ~ 1,
                        TRUE ~ NA_real_),
    # First generation
    fg = case_when(herkomstgroep == 0 ~ 0,
                        herkomstgroep == 101 ~ 1,
                        herkomstgroep == 102 ~ 1,
                        herkomstgroep == 201 ~ 0,
                        herkomstgroep == 202 ~ 0,
                        TRUE ~ NA_real_),
    
    male = ifelse(geslacht == 1, 1, 0),
    age = leeftijd,
    
    stfdem = two_sd(ifelse(ka13a047 < 999, ka13a047, NA)),
    
    ideology = two_sd(ifelse(ka13a104 < 999, ka13a104, NA)),
    
    poltr = two_sd(ifelse(ka13a016 < 999, ka13a016, NA)),
    
    involvement = two_sd(ka13a061 + ka13a062 + ka13a063 + ka13a064 + ka13a065 + ka13a066 + ka13a067 + ka13a068),
    
    polintr = two_sd(3-ka13a015),
    
    media = two_sd(ka13a003 + ka13a004 + ka13a005 + ka13a006),
    
    poleff.1 = ka13a050 - 1,
    poleff.2 = ka13a051 - 1,
    poleff.3 = ka13a052 - 1,
    poleff.4 = ifelse(ka13a053 == 2, 0, ka13a053),
    poleff.5 = ifelse(ka13a054 == 2, 0, ka13a054),
    poleff.6 = ka13a055 - 1,
    poleff = two_sd(poleff.1 + poleff.2 + poleff.3 + poleff.4 + poleff.5 + poleff.6), 
    
    
    culture1 = two_sd(6-ka13a119),
    culture2 = two_sd(6-ka13a122),
    culture3 = two_sd(ka13a123),
    culture4 = two_sd(ka13a126),
    
    antiimm = two_sd(culture1 + culture2 + culture3 + culture4),
    
    eu = two_sd(ifelse(ka13a108 == 99, NA, 6 - ka13a108)),
    polpar = two_sd(case_when(ka13a056 == 1 ~ 1,
                       ka13a056 == 2 ~ 0,
                       TRUE ~ NA_real_)),
    
    # Personality
    ep14b029rc = 6 - ep14b029,
    ep14b039rc = 6 - ep14b039,
    ep14b049rc = 6 - ep14b049,
    
    ep14b027rc = 6 - ep14b027,
    ep14b037rc = 6 - ep14b037,
    ep14b047rc = 6 - ep14b047,
    ep14b057rc = 6 - ep14b057,
    
    ep14b025rc = 6 - ep14b025,
    ep14b035rc = 6 - ep14b035,
    ep14b055rc = 6 - ep14b055,
    ep14b065rc = 6 - ep14b065,
    ep14b045rc = 6 - ep14b045,
    
    ep14b021rc = 6 - ep14b021,
    ep14b031rc = 6 - ep14b031,
    ep14b051rc = 6 - ep14b051,
    ep14b041rc = 6 - ep14b041,
    
    ep14b038rc = 6 - ep14b038,
    ep14b028rc = 6 - ep14b028,
    
    openness = ep14b024 + ep14b034 + ep14b044 + ep14b054 + ep14b059 + ep14b064 + ep14b069 + ep14b029rc + ep14b039rc + ep14b049rc,
    conscientiousness = ep14b022 + ep14b027rc + ep14b032 + ep14b037rc + ep14b042 + ep14b047rc + ep14b052 + ep14b057rc + ep14b062 + ep14b067,
    extraversion = ep14b020 + ep14b025rc + ep14b030 + ep14b035rc + ep14b050 + ep14b055rc + ep14b060 + ep14b065rc + ep14b040 + ep14b045rc,
    agreeableness = ep14b021rc + ep14b026 + ep14b066 + ep14b031rc + ep14b036 + ep14b061 + ep14b046 + ep14b051rc + ep14b041rc + ep14b056,
    neuroticism = ep14b063 + ep14b038rc + ep14b043 + ep14b048 + ep14b023 + ep14b068 + ep14b028rc + ep14b033 + ep14b053 + ep14b058
  ) %>%
  drop_na(herkomstland, openness, conscientiousness, extraversion, agreeableness, neuroticism)

imm <- within(imm, openness <- (openness - min(openness, na.rm=T)) / (max(openness, na.rm=T) - min(openness, na.rm=T)))
imm <- within(imm, conscientiousness <- (conscientiousness - min(conscientiousness, na.rm=T)) / (max(conscientiousness, na.rm=T) - min(conscientiousness, na.rm=T)))
imm <- within(imm, extraversion <- (extraversion - min(extraversion, na.rm=T)) / (max(extraversion, na.rm=T) - min(extraversion, na.rm=T)))
imm <- within(imm, agreeableness <- (agreeableness - min(agreeableness, na.rm=T)) / (max(agreeableness, na.rm=T) - min(agreeableness, na.rm=T)))
imm <- within(imm, neuroticism <- (neuroticism - min(neuroticism, na.rm=T)) / (max(neuroticism, na.rm=T) - min(neuroticism, na.rm=T)))


# Cronbach's reliability coefficient alpha
# All groups
imm %>% select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha

# Openness
imm %>% filter(herkomstgroep == 0) %>%
  select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 101) %>%
  select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 102) %>%
  select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 201) %>%
  select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 202) %>%
  select(ep14b024, ep14b034, ep14b044, ep14b054, ep14b059, ep14b064, ep14b069, ep14b029rc, ep14b039rc, ep14b049rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha

# Conscientiousness
imm %>% filter(herkomstgroep == 0) %>%
  select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 101) %>%
  select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 102) %>%
  select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 201) %>%
  select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 202) %>%
  select(ep14b022, ep14b027rc, ep14b032, ep14b037rc, ep14b042, ep14b047rc, ep14b052, ep14b057rc, ep14b062, ep14b067) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
  
# E
imm %>% filter(herkomstgroep == 0) %>%
  select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 101) %>%
  select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 102) %>%
  select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 201) %>%
  select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 202) %>%
  select(ep14b020, ep14b025rc, ep14b030, ep14b035rc, ep14b050, ep14b055rc, ep14b060, ep14b065rc, ep14b040, ep14b045rc) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha

# A
imm %>% filter(herkomstgroep == 0) %>%
  select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 101) %>%
  select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 102) %>%
  select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 201) %>%
  select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 202) %>%
  select(ep14b021rc, ep14b026, ep14b066, ep14b031rc, ep14b036, ep14b061, ep14b046, ep14b051rc, ep14b041rc, ep14b056) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha

# N
imm %>% filter(herkomstgroep == 0) %>%
  select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 101) %>%
  select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 102) %>%
  select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 201) %>%
  select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha
imm %>% filter(herkomstgroep == 202) %>%
  select(ep14b063, ep14b038rc, ep14b043, ep14b048, ep14b023, ep14b068, ep14b028rc, ep14b033, ep14b053, ep14b058) %>% alpha(.) %>% .[1] %>% .$total %>% .$raw_alpha

imm %>% 
  select(origin, dutch, fg, nw, openness, conscientiousness, extraversion, agreeableness, neuroticism, 
         ideology, polintr, poleff, involvement, polpar, media, poltr, stfdem, antiimm, eu) %>% 
  write_csv("immWEIRD.csv")

