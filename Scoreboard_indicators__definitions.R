
inside(SCOREBOARD_INDICATORS, indicator_number = "09500_ex-50") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64, men)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="MEN"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09510_ex-49") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64, women)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="WOMEN"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09520_ex-48") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-34)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="Y25-34"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09530_ex-47") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 35-44)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="Y35-44"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09540_ex-46") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 45-54)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="Y45-54"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09550_ex-45") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 55-64)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="Y55-64"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09560_ex-44") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64, educational attainment: less than primary, primary and lower secondary)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="ISCED_0-2"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09570_ex-43") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64, educational attainment: upper secondary and post-secondary non-tertiary)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="ISCED_3-4"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "09580_ex-42") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64, educational attainment: tertiary)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="ISCED_5-8"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10000_ex0") = 
specification(
name = "Adult participation in learning (during the last 12 months, excl. guided on the job training, % of the population aged 25-64)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "H",
url = "https://circabc.europa.eu/ui/group/d14c857a-601d-438a-b878-4b4cebd0e10f/library/c5a8b987-1e37-44d7-a20e-2c50d6101d27/details",
high_is_good = TRUE,
value = fromSpecialCalculation("Participation_in_education_and_training", 
    with_filters(group="TOTAL"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10010_ex1") = 
specification(
name = "Early leavers from education and training (% of population aged 18-24)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/e48bf39d-5176-410d-b32f-f4a5d22fbfdd?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("sdg_04_10", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10020_ex2") = 
specification(
name = "Early leavers from education and training (% of population aged 18-24) - men",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("sdg_04_10", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10030_ex3") = 
specification(
name = "Early leavers from education and training (% of population aged 18-24) - women",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("sdg_04_10", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10040_ex4") = 
specification(
name = "Share of individuals who have basic or above basic overall digital skills (% of population aged 16-74)",
chapter = "Digital access",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/40ecd88d-1514-4d6b-8260-3c4f5b8f4d0a?lang=en",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp410", 
    with_filters(ind_type="IND_TOTAL"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10050_ex5") = 
specification(
name = "Share of individuals who have basic or above basic overall digital skills (% of population aged 16-74) - men",
chapter = "Digital access",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp410", 
    with_filters(ind_type="M_Y16_74"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10060_ex6") = 
specification(
name = "Share of individuals who have basic or above basic overall digital skills (% of population aged 16-74) - women",
chapter = "Digital access",
group = "Equal opportunities",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp410", 
    with_filters(ind_type="F_Y16_74"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10070_ex7") = 
specification(
name = "Young people not in employment, education or training (% of total population aged 15-29)",
chapter = "Youth",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/view/edat_lfse_20__custom_13196781/default/table?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("lfsi_neet_a", 
    with_filters(sex="T", age="Y15-29", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10080_ex8") = 
specification(
name = "Young people not in employment, education or training (% of total population aged 15-29) - men",
chapter = "Youth",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("lfsi_neet_a", 
    with_filters(sex="M", age="Y15-29", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10090_ex9") = 
specification(
name = "Young people not in employment, education or training (% of total population aged 15-29) - women",
chapter = "Youth",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("lfsi_neet_a", 
    with_filters(sex="F", age="Y15-29", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10130_ex13") = 
specification(
name = "Underachievement in education (PISA - Math)",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("educ_outc_pisa", 
    with_filters(field="EF461", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10140_ex14") = 
specification(
name = "Tertiary educational attainment",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("sdg_04_20", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10150_ex15") = 
specification(
name = "Tertiary educational attainment - men",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("sdg_04_20", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10160_ex16") = 
specification(
name = "Tertiary educational attainment - women",
chapter = "Education, skills and life long learning",
group = "Equal opportunities",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("sdg_04_20", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10170_ex17") = 
specification(
name = "Gender employment gap (percentage points, population aged 20-64)",
chapter = "Gender equality in the labour market",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/view/tesem060/default/table?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("tesem060", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10180_ex18") = 
specification(
name = "Gender gap in part-time employment",
chapter = "Gender equality in the labour market",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm210", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10190_ex19") = 
specification(
name = "Gender pay gap in unadjusted form",
chapter = "Gender equality in the labour market",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("sdg_05_20", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10200_ex20") = 
specification(
name = "Income quintile ratio (S80/S20)",
chapter = "Inequality and upward mobility",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/0841b683-2a56-40b9-a57b-3a8acffa97d2?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("tessi180", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10210_ex21") = 
specification(
name = "Variation in performance explained by socio-economic status (PISA)",
chapter = "Inequality and upward mobility",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromSpecialCalculation("OECD_Pisa2", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10220_ex22") = 
specification(
name = "Employment rate (% of population aged 20-64)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/b063fae8-48c3-4443-a404-59e86a115d77?lang=en",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(sex="T", age="Y20-64", indic_em="EMP_LFS", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10230_ex23") = 
specification(
name = "Employment rate (% of population aged 20-64) - men",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(sex="M", age="Y20-64", indic_em="EMP_LFS", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10240_ex24") = 
specification(
name = "Employment rate (% of population aged 20-64) - women",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(sex="F", age="Y20-64", indic_em="EMP_LFS", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10250_ex25") = 
specification(
name = "Employment rate (% of population aged 15-24)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(age="Y15-24", indic_em="EMP_LFS", sex="T", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10260_ex26") = 
specification(
name = "Employment rate (% of population aged 25-54)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(age="Y25-54", indic_em="EMP_LFS", sex="T", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10270_ex27") = 
specification(
name = "Employment rate (% of population aged 55-64)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsi_emp_a", 
    with_filters(age="Y55-64", indic_em="EMP_LFS", sex="T", unit="PC_POP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10280_ex28") = 
specification(
name = "Employment rate (% of population aged 20-64) - ED0-2",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsa_ergaed", 
    with_filters(age="Y20-64", isced11="ED0-2", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10290_ex29") = 
specification(
name = "Employment rate (% of population aged 20-64) - ED3-4",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsa_ergaed", 
    with_filters(age="Y20-64", isced11="ED3_4", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10300_ex30") = 
specification(
name = "Employment rate (% of population aged 20-64) - ED5-8",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("lfsa_ergaed", 
    with_filters(age="Y20-64", isced11="ED5-8", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10310_ex31") = 
specification(
name = "Unemployment rate (% of active population aged 15-74)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/bdef400b-6d01-473e-be00-72bfe820a826?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="T", age="Y15-74", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10320_ex32") = 
specification(
name = "Unemployment rate (% of active population aged 15-74) - men",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="M", age="Y15-74", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10330_ex33") = 
specification(
name = "Unemployment rate (% of active population aged 15-74) - women",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="F", age="Y15-74", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10340_ex34") = 
specification(
name = "Unemployment rate (% of active population aged 15-24)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="T", age="Y15-24", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10350_ex35") = 
specification(
name = "Unemployment rate (% of active population aged 25-74)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="T", age="Y25-74", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10360_ex36") = 
specification(
name = "Unemployment rate (% of active population aged 15-74) - ED0-2",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("lfsa_urgaed", 
    with_filters(sex="T", age="Y15-74", isced11="ED0-2", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10370_ex37") = 
specification(
name = "Unemployment rate (% of active population aged 15-74) - ED3-4",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("lfsa_urgaed", 
    with_filters(sex="T", age="Y15-74", isced11="ED3_4", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10380_ex38") = 
specification(
name = "Unemployment rate (% of active population aged 15-74) - ED5-8",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("lfsa_urgaed", 
    with_filters(sex="T", age="Y15-74", isced11="ED5-8", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10390_ex39") = 
specification(
name = "Activity rate (age 15-64)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc130", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10400_ex40") = 
specification(
name = "Activity rate (age 15-64) - men",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc130", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10410_ex41") = 
specification(
name = "Activity rate (age 15-64) - women",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc130", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10420_ex42") = 
specification(
name = "Activity rate (age 15-24)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc160", 
    with_filters(age="Y15-24"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10430_ex43") = 
specification(
name = "Activity rate (age 25-54)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc160", 
    with_filters(age="Y25-54"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10440_ex44") = 
specification(
name = "Activity rate (age 55-64)",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc160", 
    with_filters(age="Y55-64"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10450_ex45") = 
specification(
name = "Youth unemployment rate",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="T", age="Y15-24", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10460_ex46") = 
specification(
name = "Youth unemployment rate - men",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="M", age="Y15-24", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10470_ex47") = 
specification(
name = "Youth unemployment rate - women",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("une_rt_a", 
    with_filters(sex="F", age="Y15-24", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10480_ex48") = 
specification(
name = "Long-term unemployment rate - men",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("une_ltu_a", 
    with_filters(sex="M", age="Y15-74", indic_em="LTU", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10490_ex49") = 
specification(
name = "Long-term unemployment rate - women",
chapter = "Labour force structure",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("une_ltu_a", 
    with_filters(sex="F", age="Y15-74", indic_em="LTU", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10500_ex50") = 
specification(
name = "Long-term unemployment rate (% active population aged 15-74)",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/d94922a2-1fb4-4897-992e-384f2f942215?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("une_ltu_a", 
    with_filters(sex="T", age="Y15-74", indic_em="LTU", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10510_ex51") = 
specification(
name = "Long-term unemployment rate (% active population aged 15-74) - men",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_ltu_a", 
    with_filters(sex="M", age="Y15-74", indic_em="LTU", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10520_ex52") = 
specification(
name = "Long-term unemployment rate (% active population aged 15-74) - women",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("une_ltu_a", 
    with_filters(sex="F", age="Y15-74", indic_em="LTU", unit="PC_ACT"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10530_ex53") = 
specification(
name = "Employment in current job by duration",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc220", 
    with_filters(duration="TOTAL"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10540_ex54") = 
specification(
name = "Employment in current job by duration - less 1 year",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc220", 
    with_filters(duration="M0-11"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10550_ex55") = 
specification(
name = "Employment in current job by duration - 1- 2 years",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc220", 
    with_filters(duration="M12-23"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10560_ex56") = 
specification(
name = "Employment in current job by duration - 2 - 5 years",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc220", 
    with_filters(duration="M24-59"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10570_ex57") = 
specification(
name = "Employment in current job by duration - over 5 years",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc220", 
    with_filters(duration="M_GE60"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10580_ex58") = 
specification(
name = "Labour transitions from temporary to permanent contracts",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc230", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10590_ex59") = 
specification(
name = "Labour transitions from temporary to permanent contracts - men",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc230", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10600_ex60") = 
specification(
name = "Labour transitions from temporary to permanent contracts - women",
chapter = "Labour market dynamics",
group = "Fair working conditions",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc230", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10610_ex61") = 
specification(
name = "Gross disposable household income (GDHI) per capita growth (index, 2008=100)",
chapter = "Income, including employment related",
group = "Fair working conditions",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/view/tepsr_wc310/default/table?lang=en",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_wc310", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10620_ex62") = 
specification(
name = "In-work at-risk-of-poverty rate",
chapter = "Income, including employment related",
group = "Fair working conditions",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("tesov110", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10630_ex63") = 
specification(
name = "In-work at-risk-of-poverty rate - men",
chapter = "Income, including employment related",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("tesov110", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10640_ex64") = 
specification(
name = "In-work at-risk-of-poverty rate - women",
chapter = "Income, including employment related",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("tesov110", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10650_ex65") = 
specification(
name = "In-work at-risk-of-poverty rate for people with disabilities (18+)",
chapter = "Income, including employment related",
group = "Fair working conditions",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dpe050", 
    with_filters(age="Y_GE18", lev_limit="SM_SEV", sex="T", unit="PC", wstatus="EMP"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10660_ex66") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate (% of total population)",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/369ed6a3-ffeb-4703-9023-dd786dc1c3b3?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_peps01n", 
    with_filters(unit="PC", age="TOTAL", sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10670_ex67") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate (% of total population) - men",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm410", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10680_ex68") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate (% of total population) - women",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm410", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10690_ex69") = 
specification(
name = "At-risk-of-poverty rate",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tessi010", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10700_ex70") = 
specification(
name = "At-risk-of-poverty rate - men",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tessi010", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10710_ex71") = 
specification(
name = "At-risk-of-poverty rate - women",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tessi010", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10720_ex72") = 
specification(
name = "Severe material and social deprivation rate",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm420", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10730_ex73") = 
specification(
name = "Severe material and social deprivation rate - men",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm420", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10740_ex74") = 
specification(
name = "Severe material and social deprivation rate - women",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm420", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10750_ex75") = 
specification(
name = "People living in households with very low work intensity",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm430", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10760_ex76") = 
specification(
name = "People living in households with very low work intensity - men",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm430", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10770_ex77") = 
specification(
name = "People living in households with very low work intensity - women",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm430", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10780_ex78") = 
specification(
name = "Severe housing deprivation rate by tenure status - owner with mortgage or loan",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm440", 
    with_filters(tenure="OWN_L", deg_urb="TOTAL", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10790_ex79") = 
specification(
name = "Severe housing deprivation rate by tenure status - tenant rent at market price",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_lm440", 
    with_filters(tenure="RENT_MKT", deg_urb="TOTAL", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10800_ex80") = 
specification(
name = "Severe housing deprivation rate - men",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dhc030", 
    with_filters(age="Y_GE16", lev_limit="NONE", sex="M", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10810_ex81") = 
specification(
name = "Severe housing deprivation rate - women",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dhc030", 
    with_filters(age="Y_GE16", lev_limit="NONE", sex="F", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10820_ex82") = 
specification(
name = "Severe housing deprivation rate - persons with disabilities",
chapter = "Living conditions and poverty",
group = "Equal opportunities",
type = "SB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dhc030", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10830_ex83") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate for children (% of population aged 0-17)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/813fbd19-0273-469b-9f37-6fa51bfabe8c?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_peps01n", 
    with_filters(unit="PC", age="Y_LT18", sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10840_ex84") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate for children (% of population aged 0-17) - boys",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_peps01n", 
    with_filters(sex="M", unit="PC", age="Y_LT18"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10850_ex85") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate for children (% of population aged 0-17) - girls",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_peps01n", 
    with_filters(sex="F", unit="PC", age="Y_LT18"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10860_ex86") = 
specification(
name = "At risk of poverty or social exclusion (AROPE) rate for persons with disabilities (% of people with disabilities above 16 years)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dpe010", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10870_ex87") = 
specification(
name = "At-risk-of-poverty rate for children (0-17)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi110", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10880_ex88") = 
specification(
name = "At-risk-of-poverty rate for children (0-17) - boys",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi110", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10890_ex89") = 
specification(
name = "At-risk-of-poverty rate for children (0-17) - girls",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi110", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10900_ex90") = 
specification(
name = "At-risk-of-poverty rate for persons with disabilities (% of people with disabilities above 16 years)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dpe020", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10910_ex91") = 
specification(
name = "Severe material and social deprivation rate for children (0-17)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi120", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10920_ex92") = 
specification(
name = "Severe material and social deprivation rate for children (0-17) - men",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi120", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10930_ex93") = 
specification(
name = "Severe material and social deprivation rate for children (0-17) - women",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi120", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10940_ex94") = 
specification(
name = "Severe material deprivation rate for persons with disabilities (16+)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dm010", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10950_ex95") = 
specification(
name = "Children (0-17) living in households with very low work intensity",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "C",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi130", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10960_ex96") = 
specification(
name = "Children (0-17) living in households with very low work intensity - men",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi130", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10970_ex97") = 
specification(
name = "Children (0-17) living in households with very low work intensity - women",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_spi130", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10980_ex98") = 
specification(
name = "People with disabilities (0-59) living in households with very low work intensity",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "CB",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dpe040", 
    with_filters(age="Y_LT60", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "10990_ex99") = 
specification(
name = "Impact of social transfers (other than pensions) on poverty reduction (% reduction of AROP)",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/b001ae62-ce34-4b49-9741-28a3ef99477f?lang=en",
high_is_good = TRUE,
value = fromEurostatDataset("tespm050", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11000_ex100") = 
specification(
name = "Impact of social transfers (other than pensions) on poverty reduction (% reduction of AROP) - men",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("tespm050", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11010_ex101") = 
specification(
name = "Impact of social transfers (other than pensions) on poverty reduction (% reduction of AROP) - women",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = TRUE,
value = fromEurostatDataset("tespm050", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11020_ex102") = 
specification(
name = "General government expenditure - social protection",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp110", 
    with_filters(cofog99="GF10"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11030_ex103") = 
specification(
name = "General government expenditure - health",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp110", 
    with_filters(cofog99="GF07"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11040_ex104") = 
specification(
name = "General government expenditure - education",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp110", 
    with_filters(cofog99="GF09"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11050_ex105") = 
specification(
name = "Aggregate replacement ratio for pensions",
chapter = "Impact of public policies on reducing poverty",
group = "Social protection and inclusion",
type = "S",
high_is_good = TRUE,
value = fromEurostatDataset("tespn070", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11060_ex106") = 
specification(
name = "Disability employment gap (percentage points, population aged 20-64)",
chapter = "",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/view/tepsr_sp200/default/table?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_sp200", 
    with_filters(sex="T", lev_limit="SM_SEV"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11070_ex107") = 
specification(
name = "Disability employment gap (percentage points) - men",
chapter = "",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_sp200", 
    with_filters(sex="M", lev_limit="SM_SEV"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11080_ex108") = 
specification(
name = "Disability employment gap (percentage points) - women",
chapter = "",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_sp200", 
    with_filters(sex="F", lev_limit="SM_SEV"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11090_ex109") = 
specification(
name = "Housing cost overburden (% of total population)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/560bfb6b-870b-419f-9858-49288f31374d?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("tespm140", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11100_ex110") = 
specification(
name = "Housing cost overburden rate (% of total population) - men",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_lvho07a", 
    with_filters(age="TOTAL", sex="M", unit="PC", incgrp="TOTAL"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11110_ex111") = 
specification(
name = "Housing cost overburden rate (% of total population) - women",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("ilc_lvho07a", 
    with_filters(age="TOTAL", sex="F", unit="PC", incgrp="TOTAL"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11120_ex112") = 
specification(
name = "Housing cost overburden rate for persons with disabilities (% of people with disabilities above 16 years)",
chapter = "Living conditions and poverty",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dhc060", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11130_ex113") = 
specification(
name = "Children aged less than 3 years in formal childcare (% of the under 3-years-old population)",
chapter = "Early childhood care",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/bookmark/8ab72433-710a-4248-a9cc-d89400cc59a6?lang=en",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp210", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11140_ex114") = 
specification(
name = "Self-reported unmet need for medical care (% of population 16+)",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "H",
url = "https://ec.europa.eu/eurostat/databrowser/view/tespm110/default/table?lang=en",
high_is_good = FALSE,
value = fromEurostatDataset("tespm110", 
    with_filters(sex="T"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11150_ex115") = 
specification(
name = "Self-reported unmet need for medical care (% of population 16+) - men",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tespm110", 
    with_filters(sex="M"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11160_ex116") = 
specification(
name = "Self-reported unmet need for medical care (% of population 16+) - women",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("tespm110", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11170_ex117") = 
specification(
name = "Self-reported unmet need for medical care (% of population 16+) - persons with disabilities",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "B",
high_is_good = FALSE,
value = fromEurostatDataset("hlth_dh030", 
    with_filters(age="Y_GE16", lev_limit="SM_SEV", sex="T", unit="PC", reason="TOOEFW"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11180_ex118") = 
specification(
name = "Connectivity dimension of the Digital Economy and Society Index",
chapter = "Digital access",
group = "Social protection and inclusion",
type = "S",
high_is_good = TRUE,
value = fromSpecialCalculation("desi", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11190_ex119") = 
specification(
name = "Out-of-pocket expenditure on healthcare",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "S",
high_is_good = FALSE,
value = fromEurostatDataset("tepsr_sp310", 
    with_filters(NA))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11200_ex120") = 
specification(
name = "Healthy life years (at the age of 65) - women",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp320", 
    with_filters(sex="F"))
)
       
inside(SCOREBOARD_INDICATORS, indicator_number = "11210_ex121") = 
specification(
name = "Healthy life years (at the age of 65) - men",
chapter = "Healthcare",
group = "Social protection and inclusion",
type = "SB",
high_is_good = TRUE,
value = fromEurostatDataset("tepsr_sp320", 
    with_filters(sex="M"))
)
      