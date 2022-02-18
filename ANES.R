library(haven)
library(tidyverse)

#The study interviewed respondents in a pre-election survey that was conducted between August 18, 2020 and November 3, 2020. Election day was November 3, 2020. The study re-interviewed as many as possible of the same respondents in a post-election survey that was conducted between November 8, 2020 and January 4, 2021. 


ANES<-read_dta("~/Dropbox/LinearModels/Homeworks/Homework4/ANES2008_excpt.dta")
ANES20<-read_csv("~/Dropbox/LinearModels/Homeworks/Homework5/anes_timeseries_2020_csv_20210719/anes_timeseries_2020_csv_20210719.csv")


ANES20coded<-ANES20%>%transmute(
libcon = case_when(
# 1 Liberal 7 Conservative
V201200%in%1:7~V201200,
V201201 == 1 ~ 3,
V201201 == 2 ~ 5,
V201201 == 3 ~ 4,
TRUE~ NA_real_),

nationecon = case_when(
# 1 Good 5 Bad
V201324%in%1:5 ~ V201324,
TRUE~ NA_real_),

hydroxychloroquinesafe = case_when(
V202559 == 1~ 1,
V202559 == 2~ 0,
TRUE~NA_real_),

covidlabgrown = case_when(
V202557 == 1~ 1,
V202557 == 2~ 0,
TRUE~NA_real_),

socionat = case_when(
#1 much better 3 same 5 much worse
V201327x%in%1:5 ~ V201327x,
TRUE~ NA_real_),

expectsocionat = case_when(
#1 much better 3 same 5 much worse
V201330x%in%1:5 ~ V201330x,
TRUE~ NA_real_),


worrynat= case_when(
#1 not worry 5 extremely worried
V201335%in%1:5 ~ V201335,
TRUE~ NA_real_),


worryfam = case_when(
#1 extremely worried 5 not worried!
V201594%in%1:5 ~ 6-V201594,
TRUE~ NA_real_),


sex = case_when(

# 1 men 2 woman
V201600==1~0,
V201600==2~1,
TRUE~ NA_real_),


violencejust = case_when(
# 1 not at all 5 great deal
V201602%in%1:5 ~ V201602,
TRUE~ NA_real_),


ownstocks = case_when(
# 1 yes
V201606==1~1,
TRUE~ 0),

partyID = case_when(
#7 Republican
V201231x%in%1:7~V201231x,
TRUE~ NA_real_ ),

famincomek= case_when(
# 1 <9,999 22 >250+
V201617x==1 ~ 5, 
V201617x==2 ~ 10,
V201617x==3 ~ 15,
V201617x==4 ~ 20,
V201617x==5 ~ 25,
V201617x==6 ~ 30,
V201617x==7 ~ 35,
V201617x==8 ~ 40,
V201617x==9 ~ 45,
V201617x==10 ~ 50,
V201617x==11 ~ 60,
V201617x==12 ~ 65,
V201617x==13 ~ 70,
V201617x==14 ~ 75,
V201617x==15 ~ 80,
V201617x==16 ~ 90,
V201617x==17 ~ 100,
V201617x==18 ~ 110,
V201617x==19 ~ 125,
V201617x==20 ~ 150,
V201617x==21 ~ 175,
V201617x==22 ~ 250,
TRUE~NA_real_),


insured = case_when(
V201620==1~ 1,
V201620==2~ 0,
TRUE~NA_real_),
# 1 yes

dontasstblk =  case_when(
# 1 gov help, 7 blacks help selves
V201258%in%1:7 ~ V201258,
TRUE~ NA_real_),


envregbad= case_when(
# 1 tougher good for environment 7 environ reg bad
V201262%in%1:7 ~ V201262,
TRUE~ NA_real_),


covidtest= case_when(
#1 yes 2 no
V201624==1 ~ 1,
V201624==2 ~0 , 
TRUE~ NA_real_),

covidsuspect= case_when(
#1 yes 2 no
V201625==1 ~ 1,
V201625==2 ~0 , 
TRUE~ NA_real_),

age= case_when(
V201507x>17~ V201507x,
TRUE~ NA_real_),

education = case_when(
#1. Less than high school credential 2. High school credential 3. Some post-high school, no bachelor’s degree4. Bachelor’s degree 5. Graduate degree 
V201511x%in%1:5 ~ V201511x,
TRUE~ NA_real_),

nguns= case_when(
V201628>=0~V201628,
TRUE~ NA_real_),

# Guncount

race = case_when(
V201549x==1~ "White",
V201549x==2~ "Black",
V201549x==3~ "Hispanic",
V201549x==4~ "Asian",
V201549x==5~ "Native American",
V201549x==6~ "Multiple",
TRUE~ NA_character_),

nativeparent = case_when(
# 1 both born US, 2 One born in US, 3 both not.
V201553==3~0,
V201553==2~1,
V201553==1~2,
TRUE~ NA_real_),



populist = case_when(
# ‘Most politicians care only about the interests of the rich and powerful.’ # 1 agree 5 disagree
V202415==5~ -2,
V202415==4~ -1,
V202415==3~ 0,
V202415==2~ 1,
V202415==1~ 2,
TRUE~ NA_real_),

antiabortion = case_when(
# 1 never, 2 rape incest 3 proof 4 choice
V201336 ==4~ 1,
V201336 ==3~ 2,
V201336 ==2~ 3,
V201336 ==1~ 4,
TRUE~ NA_real_),

voted= case_when(
#1 yes 2 no 
V202072==1~1,
V202072==2~0,
TRUE~ NA_real_),

voteJoe= case_when(
#1 yes 2 no 
V202073==1~1,
V202073>1~0,
TRUE~ NA_real_),

TrumpFT = case_when(
#Trump
V201152>=0 & V201152<=100~  V201152,
TRUE~ NA_real_),
 
BidenFT = case_when(
#Biden
V201151>=0 & V201151<=100~  V201151,
TRUE~ NA_real_),

FemFT = case_when(
#Feminists
V202160>=0 & V202160<=100~ V202160,
TRUE~ NA_real_),

LiberalsFT = case_when(
#Liberals
V202161>=0 & V202161<=100~  V202161,
TRUE~ NA_real_),


BigBFT = case_when(
#Big Business
V202163>=0 & V202163<=100~  V202163,
TRUE~ NA_real_),


FauciFT = case_when(
#Fauci
V202158>=0 & V202158<=100~  V202158,
TRUE~ NA_real_),

redditor= case_when(
V202541d==1~1,
V202541d==0~0,
TRUE~ NA_real_),

youtuber= case_when(
V202541e==1~1,
V202541e==0~0,
TRUE~ NA_real_),

facebooker= case_when(
V202541a==1~1,
V202541a==0~0,
TRUE~ NA_real_),

facebookposter = case_when(
V202543==5~1,
V202543==4~2,
V202543==3~3,
V202543==2~4,
V202543==1~5,
TRUE~ NA_real_),

sharkbit=case_when(
V202569==1~1,
V202569==2~0,
TRUE~ NA_real_
),

religimp = case_when(
# 5 religion not important.
V201433==5~1,
V201433==4~2,
V201433==3~3,
V201433==2~4,
V201433==1~5,
TRUE~ NA_real_
)
)

write.csv(ANES20coded,"~/Dropbox/LinearModels/Homeworks/Homework5/ANES2020coded.csv")
