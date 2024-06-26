# colours_of
**tl;dr:** Shot skript to visually break down images into a colours based PCA and obtain k-means cluster based average image colours. 

![](example/Example.png?raw=true)

## Discription
The colours_of_stuff project started sometime in early 2020, when all of us where doomed to a relatively secluded life by the onset of the COVID19 pandemic and I thought I should try out Instagram... which I managed to suffer through for about 3 whole months before more or less giving up again. I had dabbled with some image analysis in R before and since I always struggle with finding the 'right' colour set for visualizations, I wanted to take real works colour pallets into my some of my workflows and tried to find a statistical way to 'decompose' images into their principal colours. This idea is far from original, and the internet is full of mood boards, colour pallets and even R-packages supplying a wide variety of pallets. Some honourable mentions would be [fishualize](https://github.com/nschiett/fishualize) (colours from tropic fish species), [ggsci](https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html)  (palette from TV shows like the Simpsons, Futurama and Rick and Morty) or [wesanderson](https://github.com/karthik/wesanderson) (Wes Anderson movie pallets).
I spend a few evenings and tried out some different approaches but in the end I ended up with downscaling the images, and using the RGB values of each pixel to calculate Euclidean distances between them. Imagine a two-dimensional plane where each pixel of an image represents a point of data. According to their RGB values, these datapoints will group according to their similarity, i.e. reds (255, 0, 0) are more similar and therefore closer to oranges (255, 165, 0) than to blues (0, 0, 255). Example of unclustered, clustered and kmeans centroid below (left to right).

![](example/kmeans.png?raw=true)

In a last effort to step to show of a decomposed image, a principal component analysis (PCA) is calculated and plotted one with original pixel colours and once with the average colour or the k-means centoids. 
