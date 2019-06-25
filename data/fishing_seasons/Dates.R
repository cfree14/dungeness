library(tidyverse)
library(lubridate)
library(readr)

seasonsfile <- here::here("data/fishing_seasons/seasons.csv")
seaskeyfile <- here::here("data/fishing_seasons/seasons_key.csv")
seasregfile <- here::here("data/fishing_seasons/seasons_reg.csv")

seasreg <- read_csv(seasregfile)

seaskey <- read_csv(seaskeyfile, col_types=list(col_character(),col_date(format="%m/%d/%y"),col_date(format="%m/%d/%y"))) %>%
                    transmute(Season,Interval=interval(Start,End))

dates2plot <- interval("2011-10-01","2019-09-30")

getseas <- function(x) seaskey[x%within%seaskey$Interval,]$Season


rawdat <- read_csv(seasonsfile, col_types=list(col_date(format="%m/%d/%y"),col_character(),col_character(),
                                         col_number(),col_number(),col_character())) %>%
                replace_na(list(Reason="Regular"))

newdat <- rawdat %>%
                filter(Date %within% dates2plot) %>%
                mutate(Season=map_chr(Date,getseas)) %>%
                separate(Season, c("Reg_Open", "Reg_Close"), sep="-") %>%
                unite(Date_Reason,Date,Reason,sep="_") %>%
                spread(key=Open_Close,Date_Reason) %>%
                separate(Close, c("Close_Date","Close_Reason"), sep="_") %>%
                separate(Open, c("Open_Date","Open_Reason"), sep="_") %>%
                left_join(seasreg) %>%
                unite(Reg_Open,Open,Reg_Open,sep="/") %>%
                unite(Reg_Close,Close,Reg_Close,sep="/") %>%
                mutate(Reg_Open=dmy(Reg_Open),Reg_Close=dmy(Reg_Close),
                	   Close_Date=ymd(Close_Date),Open_Date=ymd(Open_Date),
                	   Open_Reason=na_if(Open_Reason,"Regular"),Close_Reason=na_if(Close_Reason,"Regular")) %>%
				mutate(Close_Date=coalesce(Close_Date,Reg_Close),Open_Date=coalesce(Open_Date,Reg_Open))


seasreg_rect <- seaskey %>%
					filter(Interval %within% dates2plot) %>%
					mutate(Area="Northern") %>%
					full_join(seaskey %>%
						filter(Interval %within% dates2plot) %>%
						mutate(Area="Central")) %>%
					select(Season, Area) %>%
					separate(Season, c("Open_Year","Close_Year"), sep="-") %>%
					full_join(seasreg, by="Area") %>%
					unite(Reg_Open,Open,Open_Year,sep="/") %>%
					unite(Reg_Close,Close,Close_Year,sep="/") %>%
					mutate(Reg_Open=dmy(Reg_Open), Reg_Close=dmy(Reg_Close))

ggplot() +
	geom_rect(data=seasreg_rect,
				mapping=aes(xmin=Reg_Open, xmax=Reg_Close, ymax=Reg_North, ymin=Reg_South),colour="grey50",fill="grey75") +
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Open, xmax=Open_Date, ymin=Southern, ymax=Northern, fill=Open_Reason), alpha=0.7) +
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Close, xmax=Close_Date, ymin=Southern, ymax=Northern, fill=Close_Reason), alpha=0.7) +
	geom_text(data=newdat, aes(x=Reg_Open+(Open_Date-Reg_Open)/2, y=Southern+(Northern-Southern)/2, label=na_if(Reg_Open-Open_Date,0)), size=2) + 
	geom_text(data=newdat, aes(x=Reg_Close+(Close_Date-Reg_Close)/2, y=Southern+(Northern-Southern)/2, label=na_if(Close_Date-Reg_Close,0)), size=2) + 
#	geom_text(data=newdat, aes(x=Open_Date+(Close_Date-Open_Date)/2, y=Southern+(Northern-Southern)/2, label=Close_Date-Open_Date+1), size=3) + 
	ylab("Latitude") +
	xlab("Date") +
	labs(title="California Dungeness Crab Seasons",
        x ="Date", y = "Latitude (Degree Decimals)", fill="Reason") +
	ggthemes::theme_tufte()


