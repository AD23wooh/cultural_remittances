install.packages("haven")
library(haven)

install.packages("readr")
library(readr)

install.packages("stargazer")
library(stargazer)

install.packages("plm")
library(plm)

install.packages("Formula")
library(Formula)

install.packages("/Users/alexander.devera25/Downloads/lfe_3.1.1.tar.gz", dependencies = TRUE)
install.packages("/Users/alexander.devera25/Downlodas/car_3.1-3.tgz", dependencies = TRUE)
install.packages("/Users/alexander.devera25/Downloads/AER_1.2-14.tar.gz", dependencies = TRUE) #for AER_package
install.packages("car", dependencies = TRUE)
source("/Users/alexander.devera25/Documents/pdata.frame.Rd") #for pdata.frame()
source("/Users/alexander.devera25/Downloads/felm.R")


ls()
install.packages("/Users/alexander.devera25/Download", repos = NULL, type = "source")
#interpolation for all Bangladesh values (adjust bangladesh_years by statistic)

cultural_remittances_sheet <- read.csv("/Users/alexander.devera25/Downloads/cr_interpolated.csv")
cultural_remittances_updated_sheet <- read.csv("/Users/alexander.devera25/Downloads/cr_analysis_final.csv")

bangladesh_years <- c(1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                      2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)

#bangladesh_years <- c(1995, 1996, , 1997, 1998, 1999, 2000, 2001, 2002, 2003, 
#2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)
#for easy alterations between interpolated dataset

#bangladesh_eci 

bangladesh_eci_trade_values <- c(-1.67946507, -1.615861157, -1.467747105, -1.459351973, -1.591365314, 
                                 -1.609358793, -1.548573508, -1.446023287, -1.366443395, -1.099896443, -1.04786174, 
                                 -1.081447278, -1.259619849, -1.298680954, -1.314295838, -1.298812438, -1.138227231,
                                 -1.32243943)

bangladesh_eci_model <- lm(bangladesh_eci_trade_values~ bangladesh_years)

predict_bangladesh_eci <- predict(bangladesh_eci_model, data.frame(bangladesh_years = 1995:1997))
print(predict_bangladesh_eci)

#bangladesh_hhi

bangladesh_hhi_values <- c(0.152760115, 0.1477646321, 0.1638015019, 0.1749075757, 0.1707306925,
                           0.1598487474, 0.150113304, 0.1161569529, 0.1087037401, 0.1210349091,
                           0.1145533696, 0.1087297234, 0.09785308238, 0.09548098136, 0.09126973851,
                           0.08277442497, 0.0754521413, 0.0744485946, 0.07226064843)

bangladesh_hhi_model <- lm(bangladesh_hhi_values ~ bangladesh_years)

predict_bangladesh_hhi <- predict(bangladesh_hhi_model, data.frame(bangladesh_years = 1999))
print(predict_bangladesh_hhi)

predict_bangladesh_hhi_2 <- predict(bangladesh_hhi_model, data.frame(bangladesh_years = 2014))
print(predict_bangladesh_hhi_2)

#bangladesh_edi

bangladesh_edi_values <- c(4.2468886, 4.2315159, 4.3678856, 4.4504237, 4.5068464, 4.5367198,
                           4.5618601, 4.5729461, 4.6201849, 4.6322732, 4.6421151, 4.6803441,
                           4.6558933, 4.7234402, 4.8244038, 4.7623401, 4.8026147, 4.8944931,
                           4.9444904, 4.9510236)

bangladesh_edi_model <- lm(bangladesh_edi_values ~ bangladesh_years)

predict_bangladesh_edi <- predict(bangladesh_edi_model, data.frame(bangladesh_years = 2015))
print(predict_bangladesh_edi)

#bangladesh_obr

bangladesh_obr_values <- c(1.01082, 1.08814, 1.07173, 1.35323, 1.75298, 1.76371, 1.58183,
                           1.37859, 1.3757, 1.28301, 1.22278, 1.12576, 1.11383)

bangladesh_obr_model <- lm(bangladesh_obr_values ~ bangladesh_years)

predict_bangladesh_obr <- predict(bangladesh_obr_model, data.frame(bangladesh_years = 1995:1998))
predict_bangladesh_obr_2 <- predict(bangladesh_obr_model, data.frame(bangladesh_years = 2010))
predict_bangladesh_obr_3 <- predict(bangladesh_obr_model, data.frame(bangladesh_years = 2013:2015))

print(predict_bangladesh_obr)
print(predict_bangladesh_obr_2)
print(predict_bangladesh_obr_3)

#bangladesh_tertiary_education

bangladesh_tertiary_education_values <- c(5.479310036, 5.46036005, 6.424399853, 6.135369778, 6.196889877,
                                          5.728839874, 6.295829773, 7.241710186, 7.867129803, 8.842450142,
                                          10.81634998, 13.57241726, 13.66753769, 13.65109444,13.65344143, 
                                          15.59071636)

bangladesh_tertiary_education_model <- lm(bangladesh_tertiary_education_values ~ bangladesh_years)

predict_bangladesh_tertiary_education <- predict(bangladesh_tertiary_education_model, data.frame(bangladesh_years = 1995:1998))
predict_bangladesh_tertiary_education_2 <- predict(bangladesh_tertiary_education_model, data.frame(bangladesh_years = 2010))

print(predict_bangladesh_tertiary_education)
print(predict_bangladesh_tertiary_education_2)

#bangladesh_cpi

bangladesh_cpi_values <- c(22.9, 4, 12, 13, 15, 17, 20, 20, 21, 24, 27, 26, 27, 25, 25)

bangladesh_cpi_model <- lm(bangladesh_cpi_values ~ bangladesh_years)

predict_bangladesh_cpi <- predict(bangladesh_cpi_model, data.frame(bangladesh_years = 1995))
predict_bangladesh_cpi_2 <- predict(bangladesh_cpi_model, data.frame(bangladesh_years = 1997:2000))
predict_bangladesh_cpi_3 <- predict(bangladesh_cpi_model, data.frame(bangladesh_years = 2010))

print(predict_bangladesh_cpi)
print(predict_bangladesh_cpi_2)
print(predict_bangladesh_cpi_3)

#bangladesh_political_stability 

bangladesh_political_stability <- c(-0.5488152504, -0.3707467616, -0.7262612581, -1.032590389, -1.115581155, -1.360476375,
                                    -1.863839388, -1.507097363, -1.543071628, -1.508331299, -1.546771407, -1.425789475,
                                    -1.404736876, -1.379088998, -1.627858877, -0.8955631852, -1.209773779)

bangladesh_political_stability_model <- lm(bangladesh_political_stability ~ bangladesh_years)

predict_bangladesh_political_stability <- predict(bangladesh_political_stability_model, data.frame(bangladesh_years = 1995))
predict_bangladesh_political_stability_2 <- predict(bangladesh_political_stability_model, data.frame(bangladesh_years = 1997))
predict_bangladesh_political_stability_3 <- predict(bangladesh_political_stability_model, data.frame(bangladesh_years = 1999))
predict_bangladesh_political_stability_4 <- predict(bangladesh_political_stability_model, data.frame(bangladesh_years = 2001))

print(predict_bangladesh_political_stability)
print(predict_bangladesh_political_stability_2)
print(predict_bangladesh_political_stability_3)
print(predict_bangladesh_political_stability_4)

#bangladesh_rule_of_law

bangladesh_rule_of_law <- c(-0.9340237379, -0.9358645082, -0.9911299944, -0.9916012287, -1.151943922, -1.122482061, -0.9785518646, 
                            -0.907564044, -0.8271559477, -0.7523735762, -0.7845714688, -0.8010132909, -0.7333355546, -0.9361673594, 
                            -0.8715798855, -0.7823567986, -0.7696053386) 
 
bangladesh_rule_of_law_model <- lm(bangladesh_rule_of_law ~ bangladesh_years)

predict_bangladesh_rule_of_law<- predict(bangladesh_rule_of_law_model, data.frame(bangladesh_years = 1995))
predict_bangladesh_rule_of_law_2 <- predict(bangladesh_rule_of_law_model, data.frame(bangladesh_years = 1997))
predict_bangladesh_rule_of_law_3 <- predict(bangladesh_rule_of_law_model, data.frame(bangladesh_years = 1999))
predict_bangladesh_rule_of_law_4 <- predict(bangladesh_rule_of_law_model, data.frame(bangladesh_years = 2001))

print(predict_bangladesh_rule_of_law)
print(predict_bangladesh_rule_of_law_2)
print(predict_bangladesh_rule_of_law_3)
print(predict_bangladesh_rule_of_law_4)

#bangladesh_journal_article

bangladesh_journal_article_values <- c(368.49, 396.28, 389.76, 407.26, 444.19, 434.28, 420.78, 493.64, 531.3, 593.35,
                                       740.2, 871.17, 1056.49, 1159.19, 1426.96, 1391.46, 1921.35, 1787.42, 2408.26, 2182.74)

bangladesh_journal_article_model <- lm(bangladesh_journal_article_values ~ bangladesh_years)

predict_bangladesh_journal_article <- predict(bangladesh_journal_article_model, data.frame(bangladesh_years = 1995))

print(predict_bangladesh_journal_article)

international_students_years <- c(1994, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 
                                  2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)

#bangladesH_international_student

bangladesh_international_student_values <- c(3371, 3845, 4114, 3935, 3596, 3198, 2758, 2581, 2410, 2305, 2706, 2619,
                                             2873, 3314, 3828, 4802, 5455, 6513)

bangladesh_international_student_model <- lm(bangladesh_international_student_values ~ international_students_years)

predict_bangladesh_international_students <- predict(bangladesh_international_student_model, data.frame(international_students_years = 1995:1998))
                                                     
print(predict_bangladesh_international_students)  

brazil_international_student_values <- c(5017, 8600, 8846, 8972, 8388, 7799, 7244, 7009, 7126, 7578, 
                                         8767, 8786, 8777, 9029, 10868, 13286, 23675, 19370)

brazil_international_student_model <- lm(brazil_international_student_values ~ international_students_years)

predict_brazil_international_students <- predict(brazil_international_student_model, data.frame(international_students_years = 1995:1998))

print(predict_brazil_international_students)

cultural_data <- plm::pdata.frame(cultural_remittances_updated_sheet, index = c("Country.Name", "Years"))

cultural_remittances_model <- plm::plm(
EDI ~  cultural_data$graduate_students 
  + cultural_data$undergraduate_students + cultural_data$researchers_per_million + 
    cultural_data$tertiary_education
  + I((cultural_data$graduate_students + cultural_data$undergraduate_students 
      + cultural_data$researchers_per_million + cultural_data$tertiary_education
  )^2) + EF + IQ | 
    graduate_students + undergraduate_students + #proxy variables
    researchers_per_million + service_.of_gdp + 
    gdp_per_capita + political_stability_wgi + education_expenditure + net_migration,
  data = cultural_data,
  model = "within", 
  effect = "twoways"
)

EDI <- cultural_data$export_diversification_index + -1*cultural_data$hhi
CR <- cultural_data$graduate_students + cultural_data$undergraduate_students 
+ cultural_data$researchers_per_million + cultural_data$tertiary_education
EF <- cultural_data$trade_gdp_ratio + cultural_data$fdi + cultural_data$service_.of_gdp +
  cultural_data$eci_trade
IQ <- cultural_data$political_stability_wgi + cultural_data$rule_of_law_wgi + cultural_data$cpi +
  cultural_data$cpi + cultural_data$journal_article 

cultural_remittances_iv_model_2 <- AER::ivreg(
  EDI ~  cultural_data$graduate_students 
  + cultural_data$undergraduate_students + cultural_data$researchers_per_million + 
    cultural_data$tertiary_education
  + I((cultural_data$graduate_students + cultural_data$undergraduate_students 
       + cultural_data$researchers_per_million + cultural_data$tertiary_education
  )^2) + EF + IQ | 
    graduate_students + undergraduate_students + #proxy variables
    researchers_per_million + service_.of_gdp + 
    gdp_per_capita + political_stability_wgi + education_expenditure + net_migration,
  data = cultural_data
)


summary(lmtest::coeftest(cultural_remittances_iv_model_2))



iv_model_4 <- lfe::felm(EDI ~  cultural_data$graduate_students 
                        + cultural_data$undergraduate_students + cultural_data$researchers_per_million + 
                          cultural_data$tertiary_education
                        + I((cultural_data$graduate_students + cultural_data$undergraduate_students 
                             + cultural_data$researchers_per_million + cultural_data$tertiary_education
                        )^2) + EF + IQ | 
                          graduate_students + undergraduate_students + #proxy variables
                          researchers_per_million + service_.of_gdp + 
                          gdp_per_capita + political_stability_wgi + education_expenditure + net_migration,
                        data = cultural_data)

summary(iv_model_4)
  
  numeric_cor <- cultural_data[, sapply(cultural_data, is.numeric)]
  cor_matrix <- cor((numeric_cor), use = "pairwise.complete.obs")
  print(cor_matrix)
 
   cultural_remittances_model_plm_1 <- plm::plm(
    EDI ~  cultural_data$graduate_students  
    + cultural_data$undergraduate_students 
    + cultural_data$researchers_per_million  
    + cultural_data$tertiary_education  
    + I((cultural_data$graduate_students + cultural_data$undergraduate_students  
         + cultural_data$researchers_per_million + cultural_data$tertiary_education  
    )^2) + EF + IQ |  
      graduate_students + undergraduate_students +  
      researchers_per_million + tertiary_education + 
      service_.of_gdp + gdp_per_capita + political_stability_wgi +  
      education_expenditure + net_migration,  
    data = cultural_data,  
    model = "within",  
    effect = "twoways"  
  )
  
summary(lmtest::coeftest(cultural_remittances_model_plm_1))
  
cultural_remittances_model_plm_2 <- plm::plm(
    EDI ~ graduate_students  
    + undergraduate_students 
    + researchers_per_million  
    + tertiary_education  
    + EF + IQ |  
      graduate_students + undergraduate_students +  
      researchers_per_million + tertiary_education +  
      service_.of_gdp + gdp_per_capita + political_stability_wgi +  
      education_expenditure + net_migration,  
    data = cultural_data,  
    model = "within",  
    effect = "twoways"  
  )
  

summary(cultural_remittances_model_plm_2)
  
cultural_remittances_model_plm_3 <- plm::plm(
  EDI ~ graduate_students  
  + undergraduate_students 
  + researchers_per_million  
  + tertiary_education  
  + I((graduate_students + undergraduate_students +  
         researchers_per_million + tertiary_education)^2)  
  + EF + IQ |  
    graduate_students + undergraduate_students +  
    researchers_per_million + tertiary_education +  
    service_.of_gdp + gdp_per_capita + education_expenditure + net_migration,  
  data = cultural_data,  
  model = "within",  
  effect = "twoways"  
  )
  

print(lmtest::coeftest(cultural_remittances_model_plm_3))

3

summary(cultural_remittances_model_plm_4)
#


cultural_remittances_model_iv <- AER::ivreg(
  EDI ~ graduate_students + undergraduate_students + 
    I(graduate_students^2) + I(undergraduate_students^2) + 
    researchers_per_million + tertiary_education + 
    EF + IQ | 
    journal_article + rule_of_law_wgi + political_stability_wgi + education_expenditure + po, 
  data = cultural_data
)

iv_model_final <- AER::ivreg(
  EDI ~ undergraduate_students + graduate_students + 
    researchers_per_million + tertiary_education + EF + IQ |
    journal_article + cpi + political_stability_wgi + rule_of_law_wgi + gdp_per_capita + service_.of_gdp,
  data = cultural_data
)

summary(iv_model_final)
summary(cultural_remittances_model_iv)

model <- lm(EDI ~ service_.of_gdp + manufactuaring_.of_gdp + outbound_mobility_ratio + remittances_received_us_dollars + gdp_per_capita + 
     researchers_per_million + tertiary_education + cpi + trade_gdp_ratio + fdi + fixed_telephone_subscriptions + political_stability_wgi +
     rule_of_law_wgi + journal_article + graduate_students + undergraduate_students + education_expenditure + net_migration, 
   data = cultural_data)

summary(model)

iv_model <- AER::ivreg(
  EDI ~ undergraduate_students + graduate_students +  
    researchers_per_million + tertiary_education + EF + IQ + 
    I((cultural_data$graduate_students + cultural_data$undergraduate_students + 
         cultural_data$researchers_per_million + cultural_data$tertiary_education)^2) |  
    journal_article + cpi + political_stability_wgi + rule_of_law_wgi + EF + IQ + 
    I((cultural_data$graduate_students + cultural_data$undergraduate_students 
    + cultural_data$researchers_per_million + cultural_data$tertiary_education)^2),  data = cultural_data
)

summary(iv_model)

iv_model_plm <- plm::plm(
  EDI ~ undergraduate_students + graduate_students +  
    researchers_per_million + tertiary_education + EF + IQ +
    I((cultural_data$graduate_students + cultural_data$undergraduate_students 
                                                             + cultural_data$researchers_per_million + cultural_data$tertiary_education
    )^2) |  
    journal_article + cpi + political_stability_wgi + rule_of_law_wgi + EF + IQ + 
    I((cultural_data$graduate_students + cultural_data$undergraduate_students 
    + cultural_data$researchers_per_million + cultural_data$tertiary_education)^2),  
  data = cultural_data, effects = "twoways", model = "within"
)


summary(iv_model_plm)
