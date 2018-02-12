## Build ready grad prediction set for 2012 cohort
library(haven)
library(zeallot)
library(janitor)
library(tidyverse)

# Population here is graduates
ready_grad <- read_dta("K:/ORP_accountability/data/2016_ACT/2017_ACT_student_level_actcohorthighest.dta") %>%
    transmute(student_key = as.integer(studentkey), act_composite, ready_grad = as.integer(act_composite >= 21))

cohort <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/grad_rate_student_level.dta") %>%
    filter(includedincohort == "Y") %>%
    transmute(student_key = as.integer(studentkey), withdrawalreasonsum,
        on_time_grad = as.integer(completiontype == 1),
        on_time_grad = if_else(is.na(on_time_grad), 0L, on_time_grad),
    # Early grads
        on_time_grad = if_else(withdrawalreasonsum == 12 | is.na(withdrawalreasonsum), 1L, on_time_grad)) %>%
    left_join(ready_grad, by = "student_key") %>%
    mutate(ready_grad = if_else(on_time_grad == 0, 0L, ready_grad))

student_absences <- function(file, gr, delim = "\t") {
    
    read_delim(file, delim = delim) %>%
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
        group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
        mutate(count = n(), temp = max(n_absences)) %>%
        filter(count == 1 | n_absences == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
        group_by(system, school, student_key, grade, begin_date, end_date, isp_days, n_absences) %>%
        mutate(count = 1, temp = cumsum(count)) %>%
        filter(temp == 1) %>%
        # Collapse multiple enrollments
        group_by(student_key) %>%
        summarise_at(c("n_absences", "n_excused", "n_unexcused"), sum, na.rm = TRUE)

}

c(abs_8, abs_7, abs_6) %<-% map2(
    .x = c("data/Absenteeism_SY2011_Grade8.txt", "data/Absenteeism_SY2010_Grade7_8.txt", "data/Absenteeism_SY2009_Grade6_7_8.txt"),
    .y = c("08", "07", "06"),
    .f = student_absences)

student_discipline <- function(file, gr, delim = "\t") {
    
    read_delim(file, delim = delim) %>%
        clean_names() %>%
        filter(grade == gr) %>%
        group_by(student_key) %>%
        count(disciplinary_type) %>%
        spread(disciplinary_type, n)

}

c(discipline_8, discipline_7, discipline_6) %<-% map2(
    .x = c("data/Diciplinary_SY2011_Grade8_revised.txt", "data/Diciplinary_SY2010_Grade7_8_revised.txt", "data/Diciplinary_SY2009_Grade6_7_8.txt"),
    .y = c("08", "07", "06"),
    .f = student_discipline
)

student_tests <- function(file, gr) {
    
    read_dta(file) %>%
        filter(!is.na(stud_id),
            MAAS_test_achiev == 0,
            !is.na(ssrd) | !is.na(ssmt) | !is.na(sssc) | !is.na(ssss)) %>%
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
        group_by(grade_tested_achiev_ss) %>% 
        mutate(scale_score_ss = scale(ssss)) %>%
        ungroup() %>%
        filter(grade_tested_achiev_rd == gr | grade_tested_achiev_mt == gr | grade_tested_achiev_sc == gr | grade_tested_achiev_ss == gr) %>%
        transmute(student_key = as.integer(stud_id),
            scale_score_rd, scale_score_mt, scale_score_sc, scale_score_ss)
    
}

c(tests_8, tests_7, tests_6) %<-% map2(
    .x = c("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta", "K:/Research and Policy/data/TNCRED/TCAP/TCAP_2010-11.dta", "K:/Research and Policy/data/TNCRED/TCAP/TCAP_2009-10.dta"),
    .y = c(8, 7, 6),
    .f = student_tests
)

tests_alg_8 <- read_dta("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta") %>%
    filter(algI == 1) %>%
    group_by(EOC_sem) %>%
    mutate(ss_EOC = scale(ss_EOC)) %>%
    ungroup() %>%
    transmute(student_key = as.integer(stud_id), ss_EOC)

tests_8 <- tests_8 %>%
    left_join(tests_alg_8, by = "student_key") %>%
    mutate(scale_score_mt = if_else(is.na(scale_score_mt) & !is.na(ss_EOC), ss_EOC, scale_score_mt)) %>%
    select(-ss_EOC)

prediction_data_8 <- abs_8 %>%
    left_join(discipline_8, by = "student_key") %>%
    left_join(tests_8, by = "student_key") %>%
    left_join(cohort, by = "student_key") %>%
# Drop students who tranfer out, deceased
    filter(!withdrawalreasonsum %in% c(2, 5, 6, 8, 10, 17))

write_csv(prediction_data_8, path = "data/prediction_data_8.csv")

prediction_data_7 <- abs_7 %>%
    left_join(discipline_7, by = "student_key") %>%
    left_join(tests_7, by = "student_key") %>%
    left_join(cohort, by = "student_key") %>%
# Drop students who tranfer out, deceased
    filter(!withdrawalreasonsum %in% c(2, 5, 6, 8, 10, 17))

write_csv(prediction_data_7, path = "data/prediction_data_7.csv")

prediction_data_6 <- abs_6 %>%
    left_join(discipline_6, by = "student_key") %>%
    left_join(tests_6, by = "student_key") %>%
    left_join(cohort, by = "student_key") %>%
# Drop students who tranfer out, deceased
    filter(!withdrawalreasonsum %in% c(2, 5, 6, 8, 10, 17))

write_csv(prediction_data_6, path = "data/prediction_data_6.csv")
