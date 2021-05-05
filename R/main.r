#' -----------------------------------------------
#' Andrew Kurzweil
#' MSCE Thesis work
#' -----------------------------------------------


base.dir = "K:/PennDOT DATA TEMP/DATA"
setwd(base.dir)
load('SMPA_EVENT_STATS_ALL.Rdata')
load('SMPA_ALL_DATA.rdata')


library('PerformanceAnalytics')
time = c(1, 2, 3, 4, 38)
chart.Correlation(stats[-time])

volume = grep('Volume', names(stats))
chart.Correlation(stats[volume])

intensity = grep('Intensity', names(stats))
chart.Correlation(stats[intensity])

chart.Correlation(stats[c(-volume,-intensity,-time)])


lm(B2_pond_max_depth_mm ~ ., stats[,-(1:4)])
sum(complete.cases(stats))
sum(complete.cases(dat))


library(zoo)
zdf = read.zoo(dat)
zoo.monthly = as.zooreg(aggregate(zdf, as.yearmon, mean), freq = 12)
as.ts(zoo.monthly)
as.ts(zdf)


plot_data = melt(as.data.frame(stats[between(month(stats$Start_Time), 4, 9) &
																		 	stats$Total_Rainfall_mm > 25.4,
																		 c(
																		 	'Time_Since_Previous_Event_hr',
																		 	'B1_pond_avg_recession_mm.hr',
																		 	'B2_pond_avg_recession_mm.hr'
																		 )]),
								 id.vars = 'Time_Since_Previous_Event_hr')

(
	event_vs_recession = ggplot(
		plot_data,
		aes(x = Time_Since_Previous_Event_hr, y = value, color = variable)
	) +
		geom_point() +
		theme(legend.position = "bottom",
					legend.title = element_blank()) +
		labs(
			x = "Time Since Previous Event (hr)",
			y = "Estimated Recession Rate (mm/hr)",
			title = "Comparison of Recession Rates and Inter-Event Duration",
			subtitle = 'for events greater than 1" total rainfall occuring between April and September'
		)
)


(
	intesity_vs_ponding_delay = ggplot(
		stats[between(month(stats$Start_Time), 4, 9) &
						stats$Total_Rainfall_mm > 12.5,],
		aes(x = Peak_Intensity_mm.hr, y = Ponding_Delay_hrs)
	) +
		geom_point() +
		theme(legend.position = "bottom",
					legend.title = element_blank()) +
		labs(
			x = "Peak Storm Intensity (mm/hr)",
			y = "Time Before Ponding Observed (hrs)",
			title = "Comparison of Storm Intensity & Ponding Delay",
			subtitle = "for events greater than 1/2in."
		)
)

(
	depth_vs_volume_cap = ggplot(
		stats[between(month(stats$Start_Time), 4, 9) &
						stats$Total_Rainfall_mm > 0,],
		aes(x = Total_Rainfall_mm, y = Total_Volume_captured_m3)
	) +
		geom_point() +
		theme(legend.position = "bottom",
					legend.title = element_blank()) +
		labs(
			x = "Total Rainfall Depth (mm)",
			y = bquote("Total Volume Captured "~(m^3)),
			title = "Comparison of Storm Depth & Volume Capture"
			#subtitle = "for events greater than 1/2in."
		)
)



dat %>% 
	filter(GR2a_B1PondDepth > 5, GR2a_B1PondDepth < 400) %>% 
	ggplot() + 
	geom_point(aes(x = GR2a_B1PondDepth[-1], y = diff(GR2a_B1PondDepth)))

ggplot(data = dat, aes(x = GR2a_B1PondDepth[-1], y = diff(GR2a_B1PondDepth))) + 
	geom_point() + 
	labs(x = "B1 Ponding Depth", y = "Change in Ponding Depth")




output = dat %>% select("TIMESTAMP","N8_sumvel","N9_sumvel","N10_sumvel","cumulative_rainfall","CS700Rain_in_TOT","GR2a_N8InletSpeed","GR2a_N9InletSpeed","GR2a_N10InletSpeed")
output$TIMESTAMP = round(output$TIMESTAMP, units = 'hours')
output = output %>% 
	group_by(TIMESTAMP) %>% 
	summarize(
		'N8_cumsumvel' = max(N8_sumvel),
		'N9_cumsumvel' = max(N9_sumvel),
		'N10_cumsumvel' = max(N10_sumvel),
		'Rainfall_cumsum' = max(cumulative_rainfall),
		'N8_sumvel' = sum(GR2a_N8InletSpeed),
		'N9_sumvel' = sum(GR2a_N9InletSpeed),
		'N10_sumvel' = sum(GR2a_N10InletSpeed),
		'Rainfall_sum' = sum(CS700Rain_in_TOT))
 write.csv(output,"HourlyRainVelocity.csv")

ggplot(output) + geom_point(aes(x = Rainfall_cumsum, y = N8_cumsumvel))
ggplot(output %>% filter(Rainfall_sum > 0), aes(x = Rainfall_sum, y = N8_sumvel)) + 
	geom_point() + 
	stat_smooth(method = "lm", col = "red")

ggplot(stats, aes(Total_Rainfall_mm,Inlet_Total_Volume_m3)) +
	geom_point()

attach(dat)
outmod = lm(data = dat %>% filter(!is.na(stormID)), formula = GR2a_B1OutletDepth ~ GR2a_N8InletSpeed + GR2a_N9InletSpeed + GR2a_N10InletSpeed + cumulative_rainfall)
summary(outmod)

library(astsa)
acf2(outmod$residuals^2)

ggplot(dat %>% filter(!is.na(stormID)), aes(x = cumulative_rainfall, y = GR2a_B1OutletDepth)) + 
	geom_point()
