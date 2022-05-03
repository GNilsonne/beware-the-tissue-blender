library(tidyverse)

#### read data ####

data_age_swe_2021 = read_tsv("BE0101A6_20220426-192456.csv") # from SCB, age distribution in sweden 2021

data = read_csv2("journal.pgen.1002696.s005.csv") %>% 
      rename(lymphocytes = Lympho, granulocytes = Granulo) %>% 
        mutate(gender = case_when(gender == "M" ~ "male", gender == "F" ~ "female")) %>%  
        mutate(dataset="aubert") %>%
  bind_rows(read_csv2("Werner_2013.csv") %>% 
      rename(age =  Alter, lymphocytes = Ly_TEL, granulocytes = Gran_TEL) %>% 
        mutate(dataset="werner")) %>%
  bind_rows(read_csv("data_Alder.csv") %>% 
        rename(lymphocytes = tl_lymphoc, granulocytes = tl_granuloc) %>%
        mutate(dataset="alder")) %>%
  select(age, gender, lymphocytes, granulocytes, dataset)

data_clean = data %>% filter(!is.na(lymphocytes) & !is.na(granulocytes)) %>%
    mutate(age = round(age)) %>% mutate(age = case_when(age > 100 ~ 100, age < 101 ~ age)) %>%
    left_join(data_age_swe_2021 %>% group_by(age) %>% summarise(n=sum(n)), by="age") %>%
    group_by(age) %>% mutate(weight=n/n()) %>% ungroup()

#### functions ####

long = function(d) { # reshape dataset to long
  d %>% pivot_longer(cols = lymphocytes:granulocytes, names_to = "cell", values_to = "telomere")
}

long2 = function(d) { # reshape simulation results to long
  d %>% pivot_longer(cols = t0:t10, names_to = "percent_lymphocytes", names_prefix="t", values_to = "telomere") %>%
    mutate_if(is.character, as.numeric) %>% mutate(percent_lymphocytes = percent_lymphocytes * 10)
}

sample_data = function(data, s=10, k=10) {
  d = tibble(t0 = rep(NA, k), t1=NA, t2=NA, t3=NA, t4=NA, t5=NA, t6=NA, t7=NA, t8=NA, t9=NA, t10=NA)
  for (i in 1:k) {
    d[i, ] = sample_n(data, s, TRUE, weight) %>%
      mutate(r=runif(s), 
           t0 = granulocytes,
           t1 = case_when(r <= .1 ~lymphocytes,r >.1 ~ granulocytes),
           t2 = case_when(r <= .2 ~lymphocytes,r >.2 ~ granulocytes),
           t3 = case_when(r <= .3 ~lymphocytes,r >.3 ~ granulocytes),
           t4 = case_when(r <= .4 ~lymphocytes,r >.4 ~ granulocytes),
           t5 = case_when(r <= .5 ~lymphocytes,r >.5 ~ granulocytes),
           t6 = case_when(r <= .6 ~lymphocytes,r >.6 ~ granulocytes),
           t7 = case_when(r <= .7 ~lymphocytes,r >.7 ~ granulocytes),
           t8 = case_when(r <= .8 ~lymphocytes,r >.8 ~ granulocytes),
           t9 = case_when(r <= .9 ~lymphocytes,r >.9 ~ granulocytes),
           t10 = lymphocytes) %>% select(t0:t10) %>% summarize_all(mean)
  }
  d
}

#### descriptive stats #####

age_swe_2021_summary_stat = data_age_swe_2021 %>% group_by(age) %>% summarise(n=sum(n)) %>% 
  uncount(round(n/100)) %>% summarise(mean=mean(age), sd=sd(age))

#### draw one large sample to check distribution ####

d=data_clean
                    
s=sample_n(d, 10e4, TRUE, weight)

#### descriptive plots ####

p = data %>% long %>% filter(!is.na(telomere)) %>% ggplot(aes(x=age, fill=dataset, color=dataset)) +
  geom_histogram(binwidth=1) +
  facet_wrap(~cell) +
  theme_minimal() +
  ggtitle("Age distribution - Datasets")
plot(p)

p = data_age_swe_2021 %>% ggplot(aes(x=age, y=n, fill=gender, color=gender)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  ggtitle("Age distribution - Sweden 2021")
plot(p)

p = s %>% ggplot(aes(x=age)) +
  geom_histogram(binwidth = 1) +
  theme_minimal() +
  ggtitle("Age distribution - large simulated sample")
plot(p)

p = data_clean %>% long() %>% ggplot(aes(x=age, y=telomere, color=cell)) +
  geom_point(size=2, shape=23) +
  theme_minimal() +
  theme(legend.position=c(.9,.85)) +
  ggtitle("Telomere length and age")
plot(p + facet_wrap(~dataset))
plot(p)

p = s %>% long() %>% ggplot(aes(x=age, y=telomere, color=cell)) +
  geom_point(size=2, shape=23) +
  theme_minimal() +
  theme(legend.position=c(.9,.85)) +
  ggtitle("Telomere length and age - large simulated sample")
plot(p)


#### simulate samples ####

d=data_clean

sample_data(d, 1, 10)

n=1000
k=10
min_age = 20
max_age = 30

s=sample_data(d %>% filter(age >= min_age & age <= max_age), n, k)
p = s %>% long2() %>% ggplot(aes(x=percent_lymphocytes, y=telomere)) +
  geom_point(size=2, shape=23) +
  theme_minimal() +
  #scale_colour_brewer(palette = "Set3") +
  theme(legend.position=c(.9,.85)) +
  ggtitle(paste0("Mean telomere length and percent of lymphocytes (vs granulocytes)\n", k, 
                 " simulated studies with n=", n, ", age range: ", min_age," - ", max_age, " years"))
plot(p)




