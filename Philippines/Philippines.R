# References:
#    https://www.michaeljgrogan.com/creating-maps-ggplot2-libraries/
#    https://stackoverflow.com/questions/40675778/center-plot-title-in-ggplot2
#    latitude.to

# User library: ggplot2, ggrepel, mapdata, maps
# System library: base, datasets, graphics, grDevices, methods, stats, utils

global <- map_data("world")

gg1 <- ggplot() + 
  geom_polygon(data=global, aes(x=long, y=lat, group=group), fill="green",
               color="blue") + coord_fixed(1)

coors <- data.frame(long = c(120.9842), lat = c(14.5995), city = c("Manila"))

coors2 <- data.frame(
  long2 = c(120.3873, 121.7270, 122.9550, 122.5621, 123.8854, 124.8403, 124.6319,
            125.4553, 122.0790, 125.1716, 117.7666, 124.6920, 116.2155),
  lat2 = c(17.5704, 17.6131, 14.0995, 10.7202, 10.3157, 11.8240, 8.4542, 7.1907,
           6.9214, 6.1163, 15.1833, 16.8728, 10.4480),
  city2 = c("Vigan", "Tuguegarao", "Daet", "Iloilo", "Cebu", "Catbalogan",
            "Cagayan de Oro", "Davao", "Zamboanga", "General Santos",
            "Bajo de Masinloc", "Philippine Rise", "Kalayaan Islands Group"))
gg1 +
  geom_point(data=coors, aes(long, lat), color="red", size=3) + 
  geom_point(data=coors2, aes(long2, lat2), color = "brown", size= 1.5) +
  ggtitle("Map of the Republic of the Philippines",
          subtitle = "ANG PILIPINAS AY PARA SA MGA PILIPINO") +
  labs(x="",y="") +
  theme(plot.title = element_text(color="black", face="bold", size=16, hjust = 0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  geom_text_repel(data=coors, aes(long, lat, label=city)) + 
  geom_text_repel(data=coors2, aes(long2, lat2, label=city2)) +
  xlim(111,131) + ylim(5,20)