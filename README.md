# graphics
contains code to create graphics I like, plus a couple  monte carlo functions

------------

# Plotting functions:
strip -- show the average as a point with option SE / CI, shows the data  
bar -- barplot, still needs some work  
scatter -- scatterplot  
mod -- modified box plot  
histogram -- self-explanatory  
simple -- draws a line for the mean of each group and plots the data points   
beeStrip -- like simple, but plotted using the beeswarm package so no alpha is needed  
addAlpha -- add transparancy to any color. pass the color and alpha value in [0,1]   


# Stats functions:
monte_unpaired -- monte carlo permutation test, two unpaired samples  
monte_paired -- monte carlo permutation test, two paired samples

source-ing the code will download any needed packages not installed on your machine

----------

Make pretty graphs like this:

`beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),line_color="black",IQR=T,lab=c("setosa","versicolor","virginica"),xlab="species",ylab="sepal length",main="beeStrip() example")`
![beestrip](https://github.com/lukereding/graphics/raw/master/examplePlots/beeStrip.png)

`scatter(iris %>% filter(Species=="setosa") %>% .$Petal.Length %>% jitter(.25), iris %>% filter(Species=="setosa") %>% .$Petal.Width %>% jitter(0.25),xlab="petal length",ylab="petal width",color="#00000050",main="scatter() example")`
![scatter](https://github.com/lukereding/graphics/raw/master/examplePlots/scatter.png)

`bar(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),median=T,CI=T,lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="bar() example")`
![bar](https://github.com/lukereding/graphics/raw/master/examplePlots/bar.png)
