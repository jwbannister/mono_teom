library(tidyverse)
library(circular)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(stringr)
airsci_loc <- Sys.getenv("R_AIRSCI")
load_all(airsci_loc)

files <- list('met'=list.files(path="./data", pattern="*MET*", full.names=T),
              'teom'=list.files(path="./data", pattern="*TEOM*", full.names=T),
              'flux'=list.files(path="./data", pattern="*Flux*", full.names=T))

met_df <- data.frame('deployment'=c(), 'ws'=c(), 'wd'=c(), 'datetime'=c())
deploys <- c('LV', 'MS', 'SI')
for (i in c(1:3)){
    print(files[['met']][i])
    tmp <- read.csv(files[['met']][i]) 
    if (ncol(tmp)==4){
        tmp$hour <- sapply(tmp$hour, function(x) substr(x, 1, nchar(x)-3))
        tmp$datetime <- paste(tmp$date, tmp$hour)
        tmp <- select(tmp, -date, -hour)
    }
    tmp$deployment <- deploys[i]
    tmp$datetime <- as.POSIXct(tmp$datetime, format="%m/%d/%y %H:%M")
    tmp <- tmp[complete.cases(tmp), ]
    met_df <- rbind(met_df, tmp)
}
met_df$date <- date(met_df$datetime - 1)
met_df$year <- year(met_df$datetime - 1)
met_df$month <- month(met_df$datetime - 1)
met_df <- filter(met_df, year>1998)

teom_df <- data.frame('deployment'=c(), 'pm10'=c(), 'datetime'=c())
for (fl in files[['teom']]){
    print(fl)
    tmp <- read.csv(fl) %>%
        filter(SiteName=='Mono Shore TEOM') %>%
        select('pm10'='HR_CON', 'TimeStamp') 
    if (str_sub(tmp$TimeStamp[1], -5) == '00:00'){
        frmt = "%m/%d/%Y %H:%M:%S"
    } else{
        frmt = "%m/%d/%Y %H:%M"
    }
    tmp$datetime <- as.POSIXct(tmp$TimeStamp, format=frmt) 
    tmp$deployment <- rep('MS', nrow(tmp))
    tmp <- select(tmp, -TimeStamp)
    tmp <- tmp[complete.cases(tmp), ]
    teom_df <- rbind(teom_df, tmp)
}
teom_df$date <- date(teom_df$datetime - 1)
teom_df$year <- year(teom_df$datetime - 1)
teom_df$month <- month(teom_df$datetime - 1)
teom_df <- met_df %>% filter(deployment=='MS') %>%
    select(datetime, ws, wd) %>%
    right_join(teom_df, on="datetime") %>%
    filter(pm10>-15)

fl <- files[['flux']][1]
print(fl)
tmp <- read.csv(fl) %>%
    gather(key='site', value='flux', -datetime, -ws, -wd)
tmp$datetime <- as.POSIXct(tmp$datetime, format="%m/%d/%y %H:%M")
tmp$flux <- sapply(tmp$flux, function(x) ifelse(is.na(x), 0, 
                                                ifelse(x==-9999, NA, x)))
tmp$deployment <- rep('MS', nrow(tmp))
flux_df <- tmp
flux_df$date <- date(flux_df$datetime - 1)
flux_df$year <- year(flux_df$datetime - 1)
flux_df$month <- month(flux_df$datetime - 1)

daily_pm10 <- teom_df %>% group_by(date) %>%
    do(pm10_24=round(sum(.$pm10, na.rm=T)/sum(!is.na(.$pm10)), 0), 
              n=sum(!is.na(.$pm10))) %>%
    mutate(pm10_24=unlist(pm10_24), n=unlist(n), year=year(date)) %>%
    ungroup()

pm10_exceeds <- daily_pm10 %>% filter(pm10_24>150 & between(year, 2011, 2015))
write.csv(pm10_exceeds, "/Users/john/code/mono_teom/output/MS_pm10_exceedances.csv",
          row.names=F)
exceed_summary <- pm10_exceeds %>% group_by(year) %>%
    summarize(exceed_days=length(n))

daily_flux <- flux_df %>% group_by(site, date) %>%
    filter(between(year, 2011, 2015)) %>%
    summarize(flux=sum(flux))
high_flux <- daily_flux %>% filter(flux>10)
write.csv(high_flux, "/Users/john/code/mono_teom/output/high_daily_flux.csv",
          row.names=F)

v <- 'ws'
data_continuity_plot <- met_df %>% ggplot(aes_string(x='datetime', y=v)) +
    geom_point() +
    facet_grid(rows=vars(deployment))

dust_plot_df <- filter(teom_df, between(year, 2011, 2015))
dust_plots <- vector(mode='list', length=length(unique(dust_plot_df$year))+1)
names(dust_plots) <- c(unique(dust_plot_df$year), "legend")
valueseq <- c(200, 500, 1000, 1500)
valuemin <- 100
for (yr in as.character(unique(dust_plot_df$year))){
    print(yr)
    dust_plots[[yr]] <- dust_plot_df %>% filter(year==yr & pm10>valuemin) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq, 
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
                  reverse.bars=T, valuemin=valuemin, 
                  plot.title=paste0("Mono Shore PM10 Rose, ", yr))
    if (!exists("legnd")){
        dust_plots[['legend']] <- g_legend(dust_plots[[yr]])
    }
    dust_plots[[yr]] <- dust_plots[[yr]] + theme(legend.position='None')
}
fl <- "/Users/john/code/mono_teom/output/dust_roses.pdf"
pdf(file=fl, height=10.5, width=7.5)
dust_grid <- gridExtra::grid.arrange(grobs=dust_plots, ncol=2)
dev.off()

valueseq <- c(11, 14, 17, 20)
valuemin <- 8
for (deploy in unique(met_df$deployment)){
    print(deploy)
    met_plot_df <- filter(met_df, deployment==deploy) %>%
        filter(between(year, min(.$year)+1, max(.$year)-1))
    wind_plot <- met_plot_df %>% filter(ws>valuemin) %>%
        plot_rose(., value='ws', dir='wd', valueseq=valueseq, 
                  legend.title="Wind Speed (m/s)", 
                  reverse.bars=T, valuemin=valuemin, 
                  plot.title=paste0(deploy, " Wind Rose, ", min(.$year)+1, 
                                    " - ", max(.$year)-1))
    fl <- paste0("/Users/john/code/mono_teom/output/", deploy, "_wind_rose.pdf")
    pdf(file=fl, height=7.5, width=7.5)
    print(wind_plot)
    dev.off()
}

valueseq <- c(20, 50, 100, 150)
valuemin <- 1
flux_df <- filter(flux_df, !(site %in% c('X26', 'X27', 'X28', 'X29', 'X30', 
                                         'X31', 'X32')))
for (st in unique(flux_df$site)){
    print(st)
    flux_plot_df <- filter(flux_df, between(year, 2011, 2015) & site==st)
    flux_plots <- vector(mode='list', length=length(unique(flux_plot_df$year))+1)
    names(flux_plots) <- c(unique(flux_plot_df$year), "legend")
    for (yr in as.character(unique(flux_plot_df$year))){
        print(yr)
        flux_plots[[yr]] <- flux_plot_df %>% filter(year==yr & flux>valuemin) %>%
            plot_rose(., value='flux', dir='wd', valueseq=valueseq, 
                      legend.title=bquote('Sand Flux'~'(g/c'*m^2*')'), 
                      reverse.bars=T, valuemin=valuemin, 
                      plot.title=paste0('Site ', substring(st, 2), ', ', yr))
        if (!exists("legnd")){
            flux_plots[['legend']] <- g_legend(flux_plots[[yr]])
        }
        flux_plots[[yr]] <- flux_plots[[yr]] + theme(legend.position='None')
    }
    fl <- paste0("/Users/john/code/mono_teom/output/", st, "_flux_roses.pdf")
    pdf(file=fl, height=10.5, width=7.5)
    flux_grid <- gridExtra::grid.arrange(grobs=flux_plots, ncol=2)
    dev.off()
}


        

