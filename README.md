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

##Make pretty graphs like this:

### quantitative x categorical data

Plot the data for each group as a histogram; include plain vanilla boxplot alongside. Like a typical scatterplot, this display is nice because it shows the actual data in a nice form alongside a statistical summary of the data (a boxplot here, a regression line in a boxplot).

`data(iris); beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")`

![beeStripBox](https://raw.githubusercontent.com/lukereding/graphics/master/examplePlots/beeStripBox.png)

Plot the data for each group as a histogram; include modified boxplot from Tufte alongside each histogram

`beeStripMod(iris$Sepal.Width,iris$Species,xlab="species",ylab="sepal length",main="beeStripMod() example")`

![beeStripMod](https://github.com/lukereding/graphics/raw/master/examplePlots/beeStripMod.png)

Plot the data jittered, draw line at the mean

`simple(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="simple()",xlab="species")`
![simple](https://github.com/lukereding/graphics/raw/master/examplePlots/simple.png)

Very similar, but use beeswarm() to plot the data to avoid overplotting

`beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species")`
![beeStrip](https://github.com/lukereding/graphics/raw/master/examplePlots/beeStrip.png)

Similar, but show the mean as a dot. Option standard error / confidence interval:

`strip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="strip()",xlab="species",mean_col="black",point_size=1.4,type="ci")`
![strip](https://github.com/lukereding/graphics/raw/master/examplePlots/strip.png)



Bar plot, but show the data jittered

`bar(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),median=T,CI=T,lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="bar() example")`   

![bar](https://github.com/lukereding/graphics/raw/master/examplePlots/bar.png)

Modified box plot alone

`mod(iris$Sepal.Length,iris$Species,ylab="sepal length",xlab="species",bty="l",main="mod() example")`

![mod](https://github.com/lukereding/graphics/raw/master/examplePlots/mod.png)

### quantitative x quantitative data

`scatter(iris %>% filter(Species=="setosa") %>% .$Petal.Length %>% jitter(.25), iris %>% filter(Species=="setosa") %>% .$Petal.Width %>% jitter(0.25),xlab="petal length",ylab="petal width",color="#00000050",main="scatter() example")`    

![scatter](https://github.com/lukereding/graphics/raw/master/examplePlots/scatter.png)

