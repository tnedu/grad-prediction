## Build ready grad prediction set for 2011/12 cohorts
library(haven)
library(zeallot)
library(janitor)
library(tidyverse)

# Population here is graduates
ready_grad_2012 <- read_dta("N:/ORP_accountability/data/2016_ACT/2017_ACT_student_level_actcohorthighest.dta") %>%
    transmute(student_key = as.integer(studentkey), act_composite, ready_grad = as.integer(act_composite >= 21))

cohort_2012 <- read_dta("N:/ORP_accountability/data/2016_graduation_rate/grad_rate_student_level.dta") %>%
    filter(includedincohort == "Y") %>%
    transmute(student_key = as.integer(studentkey), withdrawalreason = withdrawalreasonsum,
        on_time_grad = as.integer(completiontype == 1),
        on_time_grad = if_else(is.na(on_time_grad), 0L, on_time_grad),
    # Early grads
        on_time_grad = if_else(withdrawalreason == 12 | is.na(withdrawalreason), 1L, on_time_grad)) %>%
    left_join(ready_grad_2012, by = "student_key") %>%
    mutate(ready_grad = if_else(on_time_grad == 0, 0L, ready_grad))

ready_grad_2011 <- read_dta("N:/ORP_accountability/data/2015_ACT/2016_ACT_student_level.dta") %>%
    transmute(student_key = as.integer(studentkey), act_composite, ready_grad = as.integer(act_composite >= 21))

cohort_2011 <- read_dta("N:/ORP_accountability/data/2015_graduation_rate/grad_student_level2016.dta") %>%
    filter(includedincohort == "Y") %>%
    transmute(student_key = as.integer(studentkey), withdrawalreason,
        on_time_grad = as.integer(completiontype == 1),
        on_time_grad = if_else(is.na(on_time_grad), 0L, on_time_grad),
    # Early grads
        on_time_grad = if_else(withdrawalreason == 12 | is.na(withdrawalreason), 1L, on_time_grad)) %>%
    left_join(ready_grad_2011, by = "student_key") %>%
    mutate(ready_grad = if_else(on_time_grad == 0, 0L, ready_grad))

school_assignment <- function(file, gr) {

    read_delim(file, delim = "\t") %>%
        clean_names() %>%
        filter(grade == gr) %>%
        transmute(system = district_no, school = school_no, grade,
            student_key = as.integer(student_key), isp_days) %>%
        group_by(grade, student_key) %>%
    # For multiple enrollments, take school with most enrollment days
        mutate(temp = max(isp_days, na.rm = TRUE)) %>%
        ungroup() %>%
        filter(isp_days == temp) %>%
    # Drop duplicates on all variables
        distinct() %>% 
    # For student enrolled equal days in multiple schools, arbitrarily choose one
        group_by(grade, student_key) %>%
        mutate(count = 1, temp = cumsum(count)) %>%
        filter(temp == 1) %>%
        ungroup() %>% 
        select(system, school, grade, student_key)
        
}

c(sch_8_2012, sch_7_2012, sch_6_2012, sch_8_2011, sch_7_2011, sch_6_2011) %<-% map2(
    .x = c("data/Absenteeism/Cohortyear_2012/SY2011 and Grade 8.txt", "data/Absenteeism/Cohortyear_2012/SY2010 and Grade 7_8.txt", "data/Absenteeism/Cohortyear_2012/SY2009 and Grade 6_7_8.txt",
           "data/Absenteeism/Cohortyear_2011/SY2010 and Grade 8.txt", "data/Absenteeism/Cohortyear_2011/SY2009 and Grade 7_8.txt", "data/Absenteeism/Cohortyear_2011/SY2008 and Grade 6_7_8.txt"),
    .y = c("08", "07", "06",
           "08", "07", "06"),
    .f = school_assignment)

student_absences <- function(file, gr) {
    
    read_delim(file, delim = "\t") %>%
        clean_names() %>%
        filter(grade == gr) %>%
        transmute(system = district_no, school = school_no, grade,
            student_key = as.integer(student_key),
            begin_date, end_date, isp_days,
            n_excused = if_else(is.na(cnt_excused), 0L, as.integer(cnt_excused)),
            n_unexcused = if_else(is.na(cnt_unexcused), 0L, as.integer(cnt_unexcused)),
            n_absences = if_else(is.na(cnt_total), 0L, as.integer(cnt_total))) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
        group_by(system, school, student_key, grade, begin_date, end_date) %>%
        mutate(count = n(), temp = max(isp_days)) %>%
        filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences
        group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
        mutate(count = n(), temp = max(n_absences)) %>%
        filter(count == 1 | n_absences == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
        group_by(system, school, student_key, grade, begin_date, end_date, isp_days, n_absences) %>%
        mutate(count = 1, temp = cumsum(count)) %>%
        filter(temp == 1) %>%
    # Collapse multiple enrollments
        group_by(student_key, grade) %>%
        summarise_at(c("n_absences", "n_excused", "n_unexcused", "isp_days"), sum, na.rm = TRUE)

}

c(abs_8_2012, abs_7_2012, abs_6_2012, abs_8_2011, abs_7_2011, abs_6_2011) %<-% map2(
    .x = c("data/Absenteeism/Cohortyear_2012/SY2011 and Grade 8.txt", "data/Absenteeism/Cohortyear_2012/SY2010 and Grade 7_8.txt", "data/Absenteeism/Cohortyear_2012/SY2009 and Grade 6_7_8.txt",
           "data/Absenteeism/Cohortyear_2011/SY2010 and Grade 8.txt", "data/Absenteeism/Cohortyear_2011/SY2009 and Grade 7_8.txt", "data/Absenteeism/Cohortyear_2011/SY2008 and Grade 6_7_8.txt"),
    .y = c("08", "07", "06",
           "08", "07", "06"),
    .f = student_absences)

student_discipline <- function(file, gr) {
    
    discipline <- read_delim(file, delim = "\t") %>% 
        clean_names() %>%
        filter(grade == gr) %>%
        mutate(assault = reason_name %in% c("Assault of student", "Assault of teacher or staff", "Aggravated assault of student", "Aggravated assault of teacher or staff"),
            weapons = reason_name %in% c("Bomb threat", "Non-lethal firearm", "Possession of explosive, incendiary device", "Possession of Handgun", "Possession of weapon other than firearm"),
            theft_vandalism = reason_name %in% c("Theft of Property", "Vandalism/Damage"),
            sexual_assault_harassment = reason_name %in% c("Sexual Assault", "Sexual Harassment"),
            drugs_alcohol = reason_name %in% c("Possession, use or distribution of alcohol", "Possession, Use, or distribution of illegal drugs"),
            threat = reason_name == "Other type of threat",
            school_rules = reason_name == "Violation of School Rules",
            bullying = reason_name == "Bullying",
            fighting = reason_name == "Fighting")
    
    incidents <- discipline %>%
        group_by(student_key) %>%
        count(disciplinary_type) %>%
        spread(disciplinary_type, n)
    
    reasons <- discipline %>%
        group_by(student_key, grade) %>%
        summarise_at(c("assault", "weapons", "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting"), max)
    
    left_join(incidents, reasons, by = "student_key")

}

c(discipline_8_2012, discipline_7_2012, discipline_6_2012, discipline_8_2011, discipline_7_2011, discipline_6_2011) %<-% map2(
    .x = c("data/Discipline/Cohort year 2012/SY2011 and Grade 8.txt", "data/Discipline/Cohort year 2012/SY2010 and Grade 7_8.txt", "data/Discipline/Cohort year 2012/SY2009 and Grade 6_7_8.txt",
           "data/Discipline/Cohort year 2011/SY2010 and Grade 8.txt", "data/Discipline/Cohort year 2011/SY2009 and Grade 7_8.txt", "data/Discipline/Cohort year 2011/SY2008 and Grade 6_7_8.txt"),
    .y = c("08", "07", "06",
           "08", "07", "06"),
    .f = student_discipline
)

student_tests <- function(file, gr) {
    
    read_dta(file) %>%
        filter(!is.na(stud_id),
            MAAS_test_achiev == 0,
            !is.na(ssrd) | !is.na(ssmt) | !is.na(sssc)) %>%
        # Drop duplicates
        add_count(stud_id) %>%
        filter(n == 1) %>%
        # Standardized scale scores by subject
        group_by(grade_tested_achiev_rd) %>%
        mutate(scale_score_rd = scale(ssrd)) %>%
        group_by(grade_tested_achiev_mt) %>% 
        mutate(scale_score_mt = scale(ssmt)) %>%
        group_by(grade_tested_achiev_sc) %>%  
        mutate(scale_score_sc = scale(sssc)) %>%
        ungroup() %>%
        filter(grade_tested_achiev_rd == gr | grade_tested_achiev_mt == gr | grade_tested_achiev_sc == gr) %>%
        transmute(student_key = as.integer(stud_id),
            scale_score_rd, scale_score_mt, scale_score_sc)
    
}

c(tests_8_2012, tests_7_2012, tests_6_2012, tests_8_2011, tests_7_2011) %<-% map2(
    .x = c("N:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta", "N:/Research and Policy/data/TNCRED/TCAP/TCAP_2010-11.dta", "N:/Research and Policy/data/TNCRED/TCAP/TCAP_2009-10.dta",
           "N:/Research and Policy/data/TNCRED/TCAP/TCAP_2010-11.dta", "N:/Research and Policy/data/TNCRED/TCAP/TCAP_2009-10.dta"),
    .y = c(8, 7, 6,
           8, 7),
    .f = student_tests
)

tests_6_2011 <- read_dta("N:/Research and Policy/data/TNCRED/TCAP/TCAP_2008-09.dta") %>%
    filter(!is.na(stud_id),
        !is.na(ssrd) | !is.na(ssmt) | !is.na(sssc)) %>%
    # Drop duplicates
    add_count(stud_id) %>%
    filter(n == 1) %>%
    # Standardized scale scores by subject
    group_by(grade_tested_achiev_rd) %>%
    mutate(scale_score_rd = scale(ssrd)) %>%
    group_by(grade_tested_achiev_mt) %>% 
    mutate(scale_score_mt = scale(ssmt)) %>%
    group_by(grade_tested_achiev_sc) %>%  
    mutate(scale_score_sc = scale(sssc)) %>%
    ungroup() %>%
    filter(grade_tested_achiev_rd == 6 | grade_tested_achiev_mt == 6 | grade_tested_achiev_sc == 6) %>%
    transmute(student_key = as.integer(stud_id),
        scale_score_rd, scale_score_mt, scale_score_sc)

tests_alg_8_2012 <- read_dta("N:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta") %>%
    filter(algI == 1) %>%
    group_by(EOC_sem) %>%
    mutate(ss_EOC = scale(ss_EOC)) %>%
    ungroup() %>%
    transmute(student_key = as.integer(stud_id), ss_EOC)

tests_alg_8_2011 <- read_dta("N:/Research and Policy/data/TNCRED/TCAP/TCAP_2010-11.dta") %>%
    filter(algI == 1) %>%
    group_by(EOC_sem) %>%
    mutate(ss_EOC = scale(ss_EOC)) %>%
    ungroup() %>%
    transmute(student_key = as.integer(stud_id), ss_EOC)

tests_8_2012 <- tests_8_2012 %>%
    left_join(tests_alg_8_2012, by = "student_key") %>%
    mutate(scale_score_mt = if_else(is.na(scale_score_mt) & !is.na(ss_EOC), ss_EOC, scale_score_mt)) %>%
    select(-ss_EOC)

tests_8_2011 <- tests_8_2011 %>%
    left_join(tests_alg_8_2011, by = "student_key") %>%
    mutate(scale_score_mt = if_else(is.na(scale_score_mt) & !is.na(ss_EOC), ss_EOC, scale_score_mt)) %>%
    select(-ss_EOC)

student_demo <- function(file, gr) {

    read_dta(file) %>%
        filter(grepl(gr, allgrades)) %>%
        transmute(student_key = stud_id,
            gender = s_gender,
            race = s_rehierarchy,
            econ_dis = frpl,
            el = as.integer(elb %in% c("ESL", "ONE", "TWO")),
            swd = as.integer(sped == 1 & dis3 != 1)
    )

}

c(demo_8_2012, demo_7_2012, demo_6_2012, demo_8_2011, demo_7_2011, demo_6_2011) %<-% map2(
    .x = c("N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2011-12_BASE.dta", "N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2011-12_BASE.dta", "N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2011-12_BASE.dta",
           "N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2010-11_BASE.dta", "N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2010-11_BASE.dta", "N:/Research and Policy/data/TNCRED/Student Demographic/TN_EIS_STUDENT_DEMOGRAPHIC_2010-11_BASE.dta"),
    .y = c("08", "07", "06",
           "08", "07", "06"),
    .f = student_demo
)

prediction_data_8 <- bind_rows(mutate(abs_8_2012, cohort = 2012), mutate(abs_8_2011, cohort = 2011)) %>%
    left_join(bind_rows(sch_8_2012, sch_8_2011), by = c("student_key", "grade")) %>%
    left_join(bind_rows(discipline_8_2012, discipline_8_2011), by = c("student_key", "grade")) %>%
    left_join(bind_rows(tests_8_2012, tests_8_2011), by = "student_key") %>%
    left_join(bind_rows(demo_8_2012, demo_8_2011), by = "student_key") %>%
    left_join(bind_rows(cohort_2012, cohort_2011), by = "student_key") %>%
# Drop students who tranfer out
    filter(!withdrawalreason %in% c(2, 5, 6, 8, 10, 17)) %>%
    mutate_at(c("E", "I", "R", "S",
            "assault", "weapons", "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting"),
        funs(if_else(is.na(.), 0L, .))) %>%
    mutate(ready_grad = if_else(on_time_grad == 1L & is.na(act_composite), 0L, ready_grad)) %>%
    select(cohort, everything()) %>%
# Calculate cohort (school) aggregate metrics
    group_by(system, school) %>%
    mutate(scale_score_mt_sq = scale_score_mt^2,
        scale_score_rd_sq = scale_score_rd^2,
        scale_score_sc_sq = scale_score_sc^2,
        school_scale_score_mt = mean(scale_score_mt, na.rm = TRUE),
        school_scale_score_rd = mean(scale_score_rd, na.rm = TRUE),
        school_scale_score_sc = mean(scale_score_sc, na.rm = TRUE),
        school_chronic_abs = 100 * mean(n_absences/isp_days > 0.1, na.rm = TRUE)
    )

write_csv(prediction_data_8, path = "data/prediction_data_8.csv")

prediction_data_7 <- bind_rows(mutate(abs_7_2012, cohort = 2012), mutate(abs_7_2011, cohort = 2011)) %>%
    left_join(bind_rows(sch_7_2012, sch_7_2011), by = "student_key") %>%
    left_join(bind_rows(discipline_7_2012, discipline_7_2011), by = "student_key") %>%
    left_join(bind_rows(tests_7_2012, tests_7_2011), by = "student_key") %>%
    left_join(bind_rows(demo_7_2012, demo_7_2011), by = "student_key") %>%
    left_join(bind_rows(cohort_2012, cohort_2011), by = "student_key") %>%
# Drop students who tranfer out
    filter(!withdrawalreason %in% c(2, 5, 6, 8, 10, 17)) %>%
    mutate_at(c("E", "I", "R", "S",
            "assault", "weapons", "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting"),
        funs(if_else(is.na(.), 0L, .))) %>%
    mutate(ready_grad = if_else(on_time_grad == 1L & is.na(act_composite), 0L, ready_grad)) %>%
    select(cohort, everything()) %>%
# Calculate cohort (school) aggregate metrics
    group_by(system, school) %>%
    mutate(scale_score_mt_sq = scale_score_mt^2,
        scale_score_rd_sq = scale_score_rd^2,
        scale_score_sc_sq = scale_score_sc^2,
        school_scale_score_mt = mean(scale_score_mt, na.rm = TRUE),
        school_scale_score_rd = mean(scale_score_rd, na.rm = TRUE),
        school_scale_score_sc = mean(scale_score_sc, na.rm = TRUE),
        school_chronic_abs = 100 * mean(n_absences/isp_days > 0.1, na.rm = TRUE)
    )

write_csv(prediction_data_7, path = "data/prediction_data_7.csv")

prediction_data_6 <- bind_rows(mutate(abs_6_2012, cohort = 2012), mutate(abs_6_2011, cohort = 2011)) %>%
    left_join(bind_rows(sch_6_2012, sch_6_2011), by = "student_key") %>%
    left_join(bind_rows(discipline_6_2012, discipline_6_2011), by = "student_key") %>%
    left_join(bind_rows(tests_6_2012, tests_6_2011), by = "student_key") %>%
    left_join(bind_rows(demo_6_2012, demo_6_2011), by = "student_key") %>%
    left_join(bind_rows(cohort_2012, cohort_2011), by = "student_key") %>%
# Drop students who tranfer out
    filter(!withdrawalreason %in% c(2, 5, 6, 8, 10, 17)) %>%
    mutate_at(c("E", "I", "R", "S",
            "assault", "weapons", "theft_vandalism", "sexual_assault_harassment", "drugs_alcohol", "threat", "school_rules", "bullying", "fighting"),
        funs(if_else(is.na(.), 0L, .))) %>%
    mutate(ready_grad = if_else(on_time_grad == 1L & is.na(act_composite), 0L, ready_grad)) %>%
    select(cohort, everything()) %>%
# Calculate cohort (school) aggregate metrics
    group_by(system, school) %>%
    mutate(scale_score_mt_sq = scale_score_mt^2,
        scale_score_rd_sq = scale_score_rd^2,
        scale_score_sc_sq = scale_score_sc^2,
        school_scale_score_mt = mean(scale_score_mt, na.rm = TRUE),
        school_scale_score_rd = mean(scale_score_rd, na.rm = TRUE),
        school_scale_score_sc = mean(scale_score_sc, na.rm = TRUE),
        school_chronic_abs = 100 * mean(n_absences/isp_days > 0.1, na.rm = TRUE)
    )

write_csv(prediction_data_6, path = "data/prediction_data_6.csv")
