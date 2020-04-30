# Reference: https://www.citypopulation.de/en/philippines/metromanila/admin/

# System library: base, datasets, graphics, grDevices, methods, stats, utils
# library(plotrix)

slices <- c(1780148, 386276, 450741, 755300, 2936116, 122180, 1583978, 365525,
            249463, 620422, 588894, 582602, 504509, 665822, 416522, 63840, 804915)
lbls <- c("Manila","Mandaluyong","Marikina","Pasig","Quezon City","San Juan",
          "Caloocan","Malabon","Navotas","Valenzuela","Las Piñas","Makati",
          "Muntinlupa","Parañaque","Pasay","Pateros","Taguig")

pct <- round(slices/sum(slices) * 100, digits = 2)
lbls <- paste(lbls, "\n", pct, "%", sep="")

pie3D(slices,labels=lbls,height=0.1, radius=0.95,explode=0.15, theta=pi/4.3,
      main="Population: NCR, Philippines (2015)", labelcex = 0.75)