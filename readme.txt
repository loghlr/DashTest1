
# abb 2/10/2023: 11:07-11:45, 1:15-1:35
diagnose monthly sepdays oscillation

R
d1=read.csv('https://fosteringcourtimprovement.org/ga/County/incare_plcdaysmon_NonFamily.csv')
head(d1,3)
# do monthly totals show it? need to adjust to 30-day months first.
lubridate::days_in_month(as.Date('2011-02-15'))
as.Date('2011|02|15', format='%Y|%m|%d')
table(d1$daysinmonth <- lubridate::days_in_month(as.Date( paste0(d1[,1],'|15'), format='%Y|%m|%d' )))
summary( d1$tot30 <- round(d1[,'Statewide']*30/d1$daysinmonth) )

length( i1 <- grep('^2000.01',d1[,1]):grep('^2021.12',d1[,1]) )
(x1=xtabs( d1[i1,'tot30'] ~ substr(d1[i1,1],6,7) ))
#     01      02      03      04      05      06      07      08      09      10      11      12 
#5211314 4826907 5419639 5276410 5409881 5139054 5264735 5276087 5224506 5426848 5191032 5267888
plot(x1, type='b')
# yearly lines?
(x2=xtabs( d1[i1,'tot30'] ~ substr(d1[i1,1],6,7)+substr(d1[i1,1],1,4) ))
plot(x1/ncol(x2), type='b', ylim=range(x2), lwd=4)
for( j in 1:ncol(x2) ) lines( x2[,j], col=j+1, type='b' )

# compare to xsection?:
d2=read.csv('https://fosteringcourtimprovement.org/ga/County/incare_monthly.csv')
length( i2 <- grep('^0?1[^0-9].*2000$',d2[,1]):grep('^12[^0-9].*2021$',d2[,1]) )
stopifnot( length(i1) == length(i2) )
(x1b=xtabs( d2[i2,'Statewide'] ~ substr(d1[i2,1],6,7) ))
(x2b=xtabs( d2[i2,'Statewide'] ~ substr(d1[i2,1],6,7)+substr(d1[i2,1],1,4) ))

par( mfrow=c(1,2) )
plot(x1/ncol(x2), type='b', ylim=range(x2), lwd=4, main='GA Days Separated by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2) ) lines( x2[,j], col=j+1, type='b' )
plot(x1b/ncol(x2b), type='b', ylim=range(x2b), lwd=4, main='GA In Care on Last Day of Month by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2b) ) lines( x2b[,j], col=j+1, type='b' )

# compare to all days?:
d3=read.csv('https://fosteringcourtimprovement.org/ga/County/incare_plcdaysmon.csv')
table(d3$daysinmonth <- lubridate::days_in_month(as.Date( paste0(d3[,1],'|15'), format='%Y|%m|%d' )))
summary( d3$tot30 <- round(d3[,'Statewide']*30/d3$daysinmonth) )
length( i3 <- grep('^2000.01',d3[,1]):grep('^2021.12',d3[,1]) )
stopifnot( length(i1) == length(i3) )
(x1c=xtabs( d3[i3,'tot30'] ~ substr(d3[i3,1],6,7) ))
(x2c=xtabs( d3[i3,'tot30'] ~ substr(d3[i3,1],6,7)+substr(d3[i3,1],1,4) ))

par( mfrow=c(1,2) )
plot(x1/ncol(x2), type='b', ylim=range(x2), lwd=4, main='GA Days Separated by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2) ) lines( x2[,j], col=j+1, type='b' )
plot(x1c/ncol(x2c), type='b', ylim=range(x2c), lwd=4, main='GA Days In Care by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2c) ) lines( x2c[,j], col=j+1, type='b' )

# compare all days to last day:
#pdf( 'PlcDaysVInCare1a.pdf', width=10, height=8 )
par( mfrow=c(1,2) )
plot(x1c/ncol(x2c), type='b', ylim=range(x2c), lwd=4, main='GA Days In Care by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2c) ) lines( x2c[,j], col=j+1, type='b' )
plot(x1b/ncol(x2b), type='b', ylim=range(x2b), lwd=4, main='GA In Care on Last Day of Month by CY, 2000-2021', xlab='Month')
for( j in 1:ncol(x2b) ) lines( x2b[,j], col=j+1, type='b' )
dev.off()

# abb 2/15/2023: 2:40-3:20
diagnose 6-month ramp up in monthly sepdays

d1=read.csv('/fci/reports/us/perf3/State/incare_plcdaysmon.csv')
#d1=read.csv('/fci/reports/us/perf3/State/incare_plcdaysmon_NonFamily.csv')
#d1=read.csv('/fci/reports/la/perf3/County/incare_plcdaysmon.csv')
#d1=read.csv('/fci/reports/vt/perf3/County/incare_plcdaysmon.csv')
#d1=read.csv('/fci/reports/ga/perf3/County/removals_monthly.csv')
#d1=read.csv('/fci/reports/us/perf3/State/removals_monthly.csv')
head(d1,3)
head( d1[,c(1,ncol(d1))], 20 )
# adjust to 30-day months:
summary(d1$midmonth <- as.Date( paste0(d1[,1],'|15'), format='%Y|%m|%d' ))
table(d1$daysinmonth <- lubridate::days_in_month(d1$midmonth))
summary( d1$tot30 <- round(d1[,grep('Statewide|National',colnames(d1))]*30/d1$daysinmonth) )

par( mfrow=c(1,1) )
plot( d1$midmonth, d1$tot30, type='b', las=3 )
# it's real in US plcdays, 6 mos, 2009|10-2010|03; not in LA, but in (all?) other states? LA decreasing rapidly in 2009? not in US removal rate?

d2=read.csv('/fci/reports/us/perf3/State/incare_monthly.csv')
head(d2,3)
summary(d2$endmonth <- as.Date( d2[,1], format='%m/%d/%Y' ))
plot( d2$endmonth, d2[,grep('Statewide|National',colnames(d2))], type='b', las=3 ) # no in US incare

