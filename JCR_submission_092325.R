# Resumes for Resolution: Rebel Leader Pre-War Job Experience and Civil War Negotiations
# Alex Bruens, Ph.D., Mackenzie Harms, Ph.D., and Gina Ligon, Ph.D.

require(nnet)
require(readxl)
require(stargazer)
require(marginaleffects)
require(ggplot2)

df <- readxl::read_excel("JCR_submission_092325.xlsx")
h2modeldata <- model.frame(Negotiation2 ~ Mediation  + negotiationskill_avg + occupationcateogry + military + nsmilitary + dynamicage + dynamicage_sq +
                             founder + elected + ed_numeric + rebstr + rebelsupport_num + islamist +
                             intensity_level + incompatibility + isethnicwar + log_rgdpe_pc + 
                             log_pop + polity2 + year +
                             DyadId, data = df)

# occupationcateogry = case_when(occupation == "Business/entrepeneurship" ~ 11,
#                               occupation == "Law" ~ 23,
#                               occupation == "Career politician" ~ 21,
#                               occupation == "Military" ~ 55,
#                               occupation == "Education" ~ 25,
#                               occupation == "Journalism" ~ 27,
#                               occupation == "Writer" ~ 27,
#                               occupation == "Engineering" ~ 17,
#                               occupation == "Medicine" ~ 29,
#                               occupation == "Sciences" ~ 19,
#                               occupation == "Agriculture" ~ 45,
#                               occupation == "Religion" ~ 21,
#                               occupation == "Laborer" ~ 47,
#                               occupation == "Activist" ~ 21,
#                               occupation == "Film/music" ~ 27,
#                               occupation == "Economics" ~ 19,
#                               occupation == "Aristocrat/landowner" ~ NA,
#                               occupation == "Police" ~ 33,
#                               occupation == "Other" ~ NA,
#                               occupation == "" ~ NA)

table(h2modeldata$occupationcateogry, h2modeldata$Negotiation2)

# H1 Test - Table 2
h1 <- multinom(outcome_tri_w0 ~ negotiationskill_avg + military + nsmilitary + dynamicage + dynamicage_sq +
                   founder + elected + ed_numeric + rebstr + rebelsupport_num + islamist +
                   intensity_level + incompatibility + isethnicwar + log_rgdpe_pc + 
                   log_pop + polity2 + DyadId + year, data = df)
stargazer::stargazer(h1, type="text", star.cutoffs = c(0.05, 0.01, 0.001))
nrow(residuals(h1))

# Figure 2
ame <- avg_slopes(h1_2, newdata = "mean", variables = c("negotiationskill_avg"))
# library(dplyr)
ggplot(ame, aes(x = estimate, y = factor(group))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "AME on Pr(outcome)", y = "", 
       title = "")

# H2 Test - Table 3

h2_1 <- glm(Negotiation2 ~ negotiationskill_avg + military + nsmilitary + dynamicage + dynamicage_sq +
              founder + elected + ed_numeric + rebstr + rebelsupport_num + islamist +
              intensity_level + incompatibility + isethnicwar + log_rgdpe_pc + 
              log_pop + polity2 + DyadId + poly(year, 3), data = h2modeldata, family = binomial("logit"))
h2_2 <- glm(Mediation ~ negotiationskill_avg + military + nsmilitary + dynamicage + dynamicage_sq +
              founder + elected + ed_numeric + rebstr + rebelsupport_num + islamist +
              intensity_level + incompatibility + isethnicwar + log_rgdpe_pc + 
              log_pop + polity2 + DyadId + poly(year, 3), data = h2modeldata, family = binomial("logit"))
nobs(h2_1)
nobs(h2_2)
stargazer::stargazer(list(h2_1, h2_2), type="text", star.cutoffs = c(0.05, 0.01, 0.001))

