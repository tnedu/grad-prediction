## Build ready grad prediction set for 2012 cohort
library(haven)
library(janitor)
library(tidyverse)

cohort <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/grad_rate_student_level.dta") %>%
    transmute(student_key = as.integer(studentkey), grad_cohort = as.integer(includedincohort == "Y"),
        on_time_grad = as.integer(completiontype == 1),
        on_time_grad = if_else(grad_cohort == 1 & is.na(on_time_grad), 0L, on_time_grad)
    )

# Population here is graduates
ready_grad <- read_dta("K:/ORP_accountability/data/2016_ACT/2017_ACT_student_level_actcohorthighest.dta") %>%
    transmute(student_key = as.integer(studentkey), act_composite, ready_grad = as.integer(act_composite >= 21))

student_absenteeism <- function(file, delim = "\t") {
    
    read_delim(file, delim = delim) %>%
        clean_names() %>%
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
        summarise_at(c("n_absences", "n_excused", "n_unexcused"), sum, na.rm = TRUE) %>%
        ungroup()

}

abs_9 <- student_absenteeism("data/Absenteeism_SY2012_Grade9.txt") %>%
    rename(n_absences_9 = n_absences, n_excused_9 = n_excused, n_unexcused_9 = n_unexcused)

abs_8 <- student_absenteeism("data/Absenteeism_SY2011_Grade8.txt") %>%
    rename(n_absences_8 = n_absences, n_excused_8 = n_excused, n_unexcused_8 = n_unexcused)

abs_7 <- student_absenteeism("data/Absenteeism_SY2010_Grade7_8.txt") %>%
    rename(n_absences_7 = n_absences, n_excused_7 = n_excused, n_unexcused_7 = n_unexcused)

abs_6 <- student_absenteeism("data/Absenteeism_SY2009_Grade6_7_8.txt") %>%
    rename(n_absences_6 = n_absences, n_excused_6 = n_excused, n_unexcused_6 = n_unexcused)

student_discipline <- function(file, delim = "\t") {
    
    read_delim(file, delim = delim) %>%
        clean_names() %>%
        group_by(student_key) %>%
        count(disciplinary_type) %>%
        spread(disciplinary_type, n)
}

discipline_8 <- student_discipline("data/Diciplinary_SY2011_Grade8.txt") %>%
    rename(expulsion_8 = E, susp_in_8 = I, susp_out_8 = S, change_placement_8 = R)

discipline_7 <- student_discipline("data/Diciplinary_SY2010_Grade7_8.txt") %>%
    rename(expulsion_7 = E, susp_in_7 = I, susp_out_7 = S, change_placement_7 = R)

discipline_6 <- student_discipline("data/Diciplinary_SY2009_Grade6_7_8.txt") %>%
    rename(expulsion_6 = E, susp_in_6 = I, susp_out_6 = S, change_placement_6 = R)

student_tests <- function(file) {

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
        transmute(student_key = as.integer(stud_id),
            scale_score_rd, scale_score_mt, scale_score_sc, scale_score_ss)

}

tests_alg_8 <- read_dta("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta") %>%
    filter(algI == 1) %>%
    group_by(EOC_sem) %>%
    mutate(ss_EOC = scale(ss_EOC)) %>%
    ungroup() %>%
    transmute(student_key = as.integer(stud_id), ss_EOC)

tests_8 <- student_tests("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2011-12.dta") %>%
    rename(scale_score_rd_8 = scale_score_rd, scale_score_mt_8 = scale_score_mt,
        scale_score_sc_8 = scale_score_sc, scale_score_ss_8 = scale_score_ss) %>%
    left_join(tests_alg_8, by = "student_key") %>%
    mutate(scale_score_mt_8 = if_else(is.na(scale_score_mt_8) & !is.na(ss_EOC), ss_EOC, scale_score_mt_8)) %>%
    select(-ss_EOC)

tests_7 <- student_tests("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2010-11.dta") %>%
    rename(scale_score_rd_7 = scale_score_rd, scale_score_mt_7 = scale_score_mt,
        scale_score_sc_7 = scale_score_sc, scale_score_ss_7 = scale_score_ss)

tests_6 <- student_tests("K:/Research and Policy/data/TNCRED/TCAP/TCAP_2009-10.dta") %>%
    rename(scale_score_rd_6 = scale_score_rd, scale_score_mt_6 = scale_score_mt,
        scale_score_sc_6 = scale_score_sc, scale_score_ss_6 = scale_score_ss)

grades_8 <- read_delim("data/Course enrollment and Grades_2011.txt", delim = "\t") %>%
    clean_names() %>%
    filter(grade_assignment == "08")

prediction_data <- cohort %>%
    left_join(ready_grad, by = "student_key") %>%
    left_join(abs_8, by = "student_key") %>%
    left_join(discipline_8, by = "student_key") %>%
    left_join(tests_8, by = "student_key") %>%
    left_join(abs_7, by = "student_key") %>%
    left_join(discipline_7, by = "student_key") %>%
    left_join(tests_7, by = "student_key") %>%
    left_join(abs_6, by = "student_key") %>%
    left_join(discipline_6, by = "student_key") %>%
    left_join(tests_6, by = "student_key")

write_csv(prediction_data, path = "data/prediction_data.csv")
