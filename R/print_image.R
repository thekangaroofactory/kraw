

library(jpeg)

xx <- readJPEG(image_0)

width <- 3648
heigth <- 5472

plot(1 , 1,
     xlim = c(1, width), ylim = c(1, heigth),
     asp = width/heigth, type = 'n', xaxs = 'i', yaxs ='i', xaxt = 'n', yaxt = 'n',
     xlab = '', ylab = '', bty = 'n')

rasterImage(xx, xleft = 0, ybottom = 0, xright = width, ytop = heigth, angle = 90)

