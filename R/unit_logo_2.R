
library(maps)
library(rasterImage)
library(rphylopic)
library(png)
library(RCurl)

## output logo to file
flag_out <- TRUE

## define fonts
quartzFonts(open_sans = c("OpenSans", "OpenSans-Italic", "OpenSans-Bold", "OpenSans-BoldItalic"))
quartzFonts(uni_sans = c("UniSansBook", "UniSansBookItalic", "UniSansLightItalic", "UniSansRegularItalic"))
quartzFonts(roboto = c("RobotoCondensed-Regular", "RobotoCondensed-Italic", "RobotoCondensed-Bold", "RobotoCondensed-BoldItalic"))

## color palette
colors <- list(gray = "#ededed",
               darkgray = "#737373",
               purple = "#4b2e83",
               green = "#336633",
               black = "#000000",
               brown = "#6f522b")

## function to add png's with proper aspect ratio
## See: https://github.com/sckott/rphylopic/issues/14
add_image<-function(logo, x, y, size){
   dims <- dim(logo)[1:2] #number of x-y pixels for the logo
   IAR <- dims[1]/dims[2] #image aspect ratio
   limits <- par("usr")
   AAR <- abs(limits[3]-limits[4])/abs(limits[1]-limits[2]) #axes aspect ratio
   PAR <- par("pin")[2]/par("pin")[1]
   polygon(c(0,0),c(0,0), col=rgb(0,0,0,alpha = 1))
   rasterImage(logo, x-(size/2), y-(IAR*AAR/PAR*size/2), x+(size/2), y+(IAR*AAR/PAR*size/2), interpolate = FALSE)
}

## URL's for images
urls <- list(
   salmon = "http://phylopic.org/assets/images/submissions/9150be88-6910-4374-aa54-a7f8f3d79fb6.1024.png",
   wolf = "http://phylopic.org/assets/images/submissions/e4e306cd-73b6-4ca3-a08c-753a856f7f12.1024.png",
   loon = "http://phylopic.org/assets/images/submissions/ae2506e3-b97d-45d7-a3f9-1bfb1567e1b1.1024.png",
   cougar = "http://phylopic.org/assets/images/submissions/87c44856-307d-4d1a-84fd-ec54f8591f1a.1024.png",
   polar = "http://phylopic.org/assets/images/submissions/c11b4873-aa21-4394-9f5e-6996033c379f.1024.png",
   bear = "http://phylopic.org/assets/images/submissions/05f87521-20d4-4a05-8ac6-aa0bab7f1394.1024.png"
)

## read images
logos <- lapply(urls,
                function(x) {
                   readPNG(getURLContent(x))
                   }
                )

## convert png to raster & recolor
rasters <- lapply(logos,
                  function(ll, clr = colors$darkgray) {
                     img <- as.raster(ll, interpolate = F)
                     img <- gsub("#000000", clr, img)
                     return(img)
                     }
                  )

## draw logo
if(flag_out) {
#   tiff(file = "wacfwru_logo.tiff", height = 5.5, width = 5.5, units = "in", res = 300)
   png(file = "wacfwru_logo_2.png", height = 5.5, width = 5.5, units = "in", res = 300,
       bg = "transparent")
}

par(mai = rep(0,4), omi = rep(0,4), family = "uni_sans")

## draw map
map("state", region = "washington", fill = TRUE, col = colors$gray,
    mar = c(1,1,0,0), lwd = 2)

## add text
mtext("WASHINGTON", side = 3, line = -3.6,
      cex = 3, adj = 0.87, col = colors$green)
par(family = "roboto")
mtext("C O O P E R A T I V E", side = 3, line = -5.6,
      cex = 2.33, adj = 0.86, col = colors$brown)
mtext("Fish and Wildlife Research Unit", side = 3, line = -7.1,
      cex = 1.53, adj = 0.86, col = colors$brown)

## dummy add to clear space
add_phylopic_base(logos$salmon, 0.5, 0.5, 0)

## add animal outlines
cc <- 0
bb <- 0.1
add_image(rasters$salmon, x=0.42 - cc, y=0.27 + bb, size=0.13)
add_image(rasters$bear, x=0.63 - cc, y=0.27 + bb, size=0.17)
add_image(rasters$loon, x=0.85 - cc, y=0.27 + bb, size=0.12)

## turn off device
if(flag_out) {
	dev.off()
}

