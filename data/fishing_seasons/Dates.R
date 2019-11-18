library(tidyverse)
library(lubridate)
library(readr)

seasonsfile <- here::here("data/fishing_seasons/seasons.csv")
seaskeyfile <- here::here("data/fishing_seasons/seasons_key.csv")
seasregfile <- here::here("data/fishing_seasons/seasons_reg.csv")

dates2plot <- interval("2011-10-01","2019-09-30")

mollusc_samples <- readRDS(here::here("data/da_sampling/processed/CDPH_mollusc_viscera_da_data.Rds")) %>%
					as_tibble() %>%
					group_by(lat_dd,date) %>%
					summarise(Nsamples=n())

crab_samples <- read_csv(here::here("data/da_sampling/processed/CDPH_crab_viscera_da_data.csv"),
						 col_types=list(
						  sampleid = col_character(),
						  year = col_double(),
						  date = col_date(format = ""),
						  port = col_character(),
						  area = col_character(),
						  block = col_double(),
						  block_long_dd = col_double(),
						  block_lat_dd = col_double(),
						  latlong_orig = col_character(),
						  long_dd = col_double(),
						  lat_dd = col_double(),
						  depth_m = col_double(),
						  depth_fthm = col_double(),
						  species = col_character(),
						  sex = col_character(),
						  da_ppm_prefix = col_character(),
						  da_ppm = col_double()
							)) %>%
					mutate(lat_dd=coalesce(lat_dd,block_lat_dd)) %>%
					group_by(lat_dd,date) %>%
					summarise(Nsamples=n()) %>%
					filter(date %within% dates2plot)

seasreg <- read_csv(seasregfile)

seaskey <- read_csv(seaskeyfile, col_types=list(col_character(),col_date(format="%m/%d/%y"),col_date(format="%m/%d/%y"))) %>%
                    transmute(Season,Interval=interval(Start,End))

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
				mapping=aes(xmin=Reg_Open, xmax=Reg_Close, ymax=Reg_North, ymin=Reg_South),colour="grey50",fill="grey90") +
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Open, xmax=Open_Date, ymin=Southern, ymax=Northern, fill=Open_Reason), alpha=0.7) +
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Close, xmax=Close_Date, ymin=Southern, ymax=Northern, fill=Close_Reason), alpha=0.7) +
	geom_text(data=newdat, aes(x=Reg_Open+(Open_Date-Reg_Open)/2, y=Southern+(Northern-Southern)/2, label=na_if(Reg_Open-Open_Date,0)), size=2) + 
	geom_text(data=newdat, aes(x=Reg_Close+(Close_Date-Reg_Close)/2, y=Southern+(Northern-Southern)/2, label=na_if(Close_Date-Reg_Close,0)), size=2) + 
	labs(title="California Dungeness Crab Seasons",
        x ="Date", y = "Latitude (Degree Decimals)", fill="Reason", size="Sample Size") +
	geom_point(data=mollusc_samples, aes(x=date, y=lat_dd), colour="yellow", alpha=0.6, shape="x") +
	geom_point(data=crab_samples, aes(x=date, y=lat_dd, size=Nsamples), alpha=0.1)  + 
	scale_size_continuous(range = c(.5, 3)) +
	ggthemes::theme_tufte()


