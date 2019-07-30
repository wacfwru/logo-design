
library(maps)
library(rphylopic)
library(png)
library(RCurl)

## define fonts
quartzFonts(open_sans = c("OpenSans", "OpenSans-Italic", "OpenSans-Bold", "OpenSans-BoldItalic"))
quartzFonts(uni_sans = c("UniSansBook", "UniSansBookItalic", "UniSansLightItalic", "UniSansRegularItalic"))
quartzFonts(roboto = c("RobotoCondensed-Regular", "RobotoCondensed-Italic", "RobotoCondensed-Bold", "RobotoCondensed-BoldItalic"))

## color palette
colors <- list(gray = "#e0e0e0",
               purple = "#4b2e83",
               green = "#add0a0",
               black = "#000000")

## function to add png's with proper aspect ratio
## See: https://github.com/sckott/rphylopic/issues/14
logoing_func<-function(logo, x, y, size){
   dims<-dim(logo)[1:2] #number of x-y pixels for the logo
   IAR<-dims[1]/dims[2] #image aspect ratio
   limits <- par("usr")
   AAR<-abs(limits[3]-limits[4])/abs(limits[1]-limits[2]) #axes aspect ratio
   PAR<-par("pin")[2]/par("pin")[1]
   polygon(c(0,0),c(0,0), col=rgb(0,0,0,alpha = 1))
   rasterImage(logo, x-(size/2), y-(IAR*AAR/PAR*size/2), x+(size/2), y+(IAR*AAR/PAR*size/2), interpolate = FALSE)
}

## URL's for images
salmon_url <- "http://phylopic.org/assets/images/submissions/9150be88-6910-4374-aa54-a7f8f3d79fb6.1024.png"
wolf_url <- "http://phylopic.org/assets/images/submissions/e4e306cd-73b6-4ca3-a08c-753a856f7f12.1024.png"
loon_url <- "http://phylopic.org/assets/images/submissions/ae2506e3-b97d-45d7-a3f9-1bfb1567e1b1.1024.png"
cougar_url <- "http://phylopic.org/assets/images/submissions/87c44856-307d-4d1a-84fd-ec54f8591f1a.1024.png"

## convert png to raster
salmon_logo <-  readPNG(getURLContent(salmon_url))
wolf_logo <-  readPNG(getURLContent(wolf_url))
loon_logo <-  readPNG(getURLContent(loon_url))
cougar_logo <-  readPNG(getURLContent(cougar_url))




flag_out <- TRUE



if(flag_out) {
#   tiff(file = "wacfwru_logo.tiff", height = 5.5, width = 5.5, units = "in", res = 300)
   png(file = "wacfwru_logo.png", height = 5.5, width = 5.5, units = "in", res = 300,
       bg = "transparent")
} else {
	dev.new(height = 6, width = 6)
}

par(mai = rep(0,4), omi = rep(0,4), family = "uni_sans")

map("state", region = "washington", fill = TRUE, col = colors$green,
    mar = c(1,1,0,0))

mtext("WASHINGTON", side = 3, line = -3.5,
      cex = 3, adj = 0.87, col = colors$black)

# par(family = "open_sans")
# mtext("C o o p e r a t i v e", side = 3, line = -5.5,
#       cex = 2, adj = 0.81)
# mtext("Fish and Wildlife", side = 3, line = -7.6,
#       cex = 1.8, adj = 0.76)
# mtext("Research Unit", side = 3, line = -9.6,
#       cex = 1.8, adj = 0.74)

par(family = "roboto")
mtext("C o o p e r a t i v e", side = 3, line = -5.5,
      cex = 2, adj = 0.79, col = colors$black)
mtext("Fish and Wildlife", side = 3, line = -7.6,
      cex = 1.8, adj = 0.76, col = colors$black)
mtext("Research Unit", side = 3, line = -9.6,
      cex = 1.8, adj = 0.74, col = colors$black)


add_phylopic_base(salmon_logo, 0.5, 0.5, 0)

logoing_func(salmon_logo, x=0.35, y=0.3, size=0.2)
logoing_func(loon_logo, x=0.6, y=0.3, size=0.2)
logoing_func(cougar_logo, x=0.85, y=0.3, size=0.15)

if(flag_out) {
	dev.off()
}

