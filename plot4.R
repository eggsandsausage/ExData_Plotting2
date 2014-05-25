createPlot4 <- function(original.NEI, SCC) {

	NEI <- original.NEI[year==2008 | year==1999,]
	i <- grep(".*(comb).*(coal).*", ignore.case=T, x =SCC$Short.Name)
	NEI <- merge(SCC[i, list(SCC)], NEI, by="SCC")
	data("county.fips")
	county <- map_data("county")
	
	#county.fips$polyname <- as.character(county.fips$polyname)
	#SCC[,SCC:=as.character(SCC)]
	NEI[,fips:=as.integer(fips)]
	NEI <- merge(NEI, county.fips, by="fips")
	NEI[,region:=sub("(.*),.*","\\1",polyname)]
	NEI[,subregion:=sub(".*,(.?)","\\1",polyname)]
	
	sub1 <- merge(
	NEI[year==1999, list("emissions99" = sum(Emissions)), by=list(region,subregion)],
	NEI[year==2008, list("emissions08" = sum(Emissions)), by=list(region,subregion)],
	by=c("region", "subregion")
	)

	sub2 <- merge(sub1, county, by=c("region", "subregion"))

	qplot(long, lat, data = sub2, group = group, fill=emissions08-emissions99,
	  geom = "polygon")
	ggsave("plot4.png")
	return("Graph saved.")



}