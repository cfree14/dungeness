
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(lubridate)
library(readr)

# Directories
plotdir <- "figures"
datadir <- "data/fishing_seasons"

# Read data
seasonsfile <- file.path(datadir, "seasons.csv")
seaskeyfile <- file.path(datadir, "seasons_key.csv")
seasregfile <- file.path(datadir, "seasons_reg.csv")

# Read region key (open/close dates + lats)
seasreg <- read_csv(seasregfile)

# Read season key
seaskey <- read_csv(seaskeyfile, col_types=list(col_character(),
                                                col_date(format="%m/%d/%y"),
                                                col_date(format="%m/%d/%y"))) %>%
                    transmute(Season, Interval=interval(Start,End)) %>% 
  mutate(season1=as.numeric(substr(Season, 1,4)))


dates2plot <- interval("2011-10-01","2019-09-30")

getseas <- function(x) seaskey[x%within%seaskey$Interval,]$Season


rawdat <- read_csv(seasonsfile, col_types=list(col_date(format="%m/%d/%y"),col_character(),col_character(),
                                         col_number(),col_number(),col_character())) %>%
                replace_na(list(Reason="Regular"))


# Build data
################################################################################

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
  select(-Interval) %>% 
  filter(season1 >= 2011) %>% 
					# filter(Interval %within% dates2plot) %>%
					mutate(Area="Northern") %>%
					full_join(seaskey %>%
					  filter(season1 >=2011) %>% 
						# filter(Interval %within% dates2plot) %>%
						mutate(Area="Central")) %>%
					select(Season, Area) %>%
					separate(Season, c("Open_Year","Close_Year"), sep="-") %>%
					full_join(seasreg, by="Area") %>%
					unite(Reg_Open,Open,Open_Year,sep="/") %>%
					unite(Reg_Close,Close,Close_Year,sep="/") %>%
					mutate(Reg_Open=dmy(Reg_Open), Reg_Close=dmy(Reg_Close))


# Plot data
################################################################################

# X-axis labels
labels <- paste(2011:2018, 2012:2019-2000, sep="-")
label_dates <- seq(ymd('2012-04-15'), ymd('2019-04-15'), by="1 year")
length(labels)==length(label_dates)

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  legend.position = "bottom",
                  legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(-5,-5,-5,-5),
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot() +
  # Plot theoretical seasons
	geom_rect(data=seasreg_rect,
				mapping=aes(xmin=Reg_Open, xmax=Reg_Close, ymax=Reg_North, ymin=Reg_South), fill="grey70") +
  # Plot closures
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Open, xmax=Open_Date, ymin=Southern, ymax=Northern, fill=Open_Reason)) +
	geom_rect(data=newdat,
				mapping=aes(xmin=Reg_Close, xmax=Close_Date, ymin=Southern, ymax=Northern, fill=Close_Reason)) +
  # Add days lost labels
	# geom_text(data=newdat, aes(x=Reg_Open+(Open_Date-Reg_Open)/2, y=Southern+(Northern-Southern)/2, label=na_if(Reg_Open-Open_Date,0)), size=2) +
	# geom_text(data=newdat, aes(x=Reg_Close+(Close_Date-Reg_Close)/2, y=Southern+(Northern-Southern)/2, label=na_if(Close_Date-Reg_Close,0)), size=2) +
  # Labels
  labs(x="Season", y="Latitude", main="") +
  # X-axis label
  scale_x_date(breaks=label_dates, labels = labels) +
  # Legend
  scale_fill_discrete(name="") +
  # Themes
	theme_bw() + my_theme
g

# Export figure
ggsave(g, filename=file.path(plotdir, "FigX_closure_timeline.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


