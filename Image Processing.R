install.packages("tesseract")
install.packages("magick")
install.packages("magrittr")

library(tesseract)
library(magick)
library(magrittr)



imageData <- image_read("Denny.png")

image_scale(imageData,"382x509!")
image_trim(imageData)

image_blur(imageData)

image_convert(colors(="gray"))

text <- ocr(imageData)
cat(text)


text <- ocr("http://jeroenooms.github.io/images/testocr.png")
cat(text)
getwd()

text <- ocr("Denny.png")
#image_resize("2000")
#image_convert(colorspace = 'gray') %>%
#image_trim() %>%

image_ocr()

