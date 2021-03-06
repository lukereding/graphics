# graphics
This repo contains code to create graphics I like. Most of the code isn't as flexible as your plain vanilla base R plotting functions (e.g. many of the functions below only accept the data as a list), but you'll find some better ways to plot things not easily implemented in R. These plotting functions work for me but your milage may vary. This script is constantly being updated.

Best way to use this code for the time being: download the `plotting_functions.R` script and source it using `source("/path/to/script/plotting_functions.R")`. It'll download any required packages you don't have.

The code relies heavily on the [magrittr](https://github.com/smbache/magrittr#introduction), [beeswarm](https://github.com/aroneklund/beeswarm), and [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html) packages. 

-------------

#### These plots are designed with the following philosophy in mind:
* every part of the graph should have a clear purpose
* show the data, some summary of the data or parameter estimate of interest, and some measure of variability about that estimate
* clearly label sample sizes
* state some measure of effect size
* state the the test used to assess statistical significance and the resulting p-value
* don't extrapolate
* use color thoughtfully. Or just use [viridis](https://bids.github.io/colormap/). Why? (a) It's perceptually uniform. (b) It looks good. (c) It works just as well printed in black and white. (d) It's accessible to people with the most common forms of colorblindness. (e) It works for representing categories or for representing a quantitative variable. (f) You'll never need to justify your color scheme ever again. Note that this is also true of the other colormaps provided in the viridis package (plasma, magma, and inferno), but they don't look as nice.


Some of these things are not implemented (yet) in the graphics below


------------

### quantitative x categorical data

Chances are you've never plotted the relationship between two quantitative variables without actually showing the data. Imagine giving a talk and showing a plot of a regression line without any data--your audience would become immediately skeptical. Why should standards be any different when plotting the relationship between a quantitative and a categorical variable? Boxplots and bar charts are great, but they rarely actually show the data in a clear and compelling way. This is the goal of the functions below.

----------------

Plot the data for each group as a histogram; include plain vanilla boxplot alongside. Like a typical scatterplot, this display is nice because it shows the actual data in a nice form alongside a statistical summary of the data.

`data(iris); beeStripBox(iris$Sepal.Length,iris$Species,xlab="species",ylab="sepal length",main="beeStripBox() example")`

![beeStripBox](https://raw.githubusercontent.com/lukereding/graphics/master/examplePlots/beeStripBox.png)

Plot the data for each group as a histogram; include modified boxplot from Tufte alongside each histogram

`beeStripMod(iris$Sepal.Width,iris$Species,xlab="species",ylab="sepal length",main="beeStripMod() example")`

![beeStripMod](https://github.com/lukereding/graphics/raw/master/examplePlots/beeStripMod.png)

Plot the data jittered, draw line at the mean. Note that you could use the addAlpha function included in the script to add transparency to each of the colors pretty easily, e.g. `viridis(3) %>% addAlpha(0.6)`

`simple(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="simple()",xlab="species")`
![simple](https://github.com/lukereding/graphics/raw/master/examplePlots/simple.png)

Very similar, but use beeswarm() to plot the data to avoid overplotting

`beeStrip(list(iris %>% filter(Species=="setosa") %>% .$Sepal.Length, iris %>% filter(Species=="versicolor") %>% .$Sepal.Length, iris %>% filter(Species=="virginica") %>% .$Sepal.Length),lab=c("setosa","versicolor","virginica"),ylab="sepal length",main="beeStrip()",xlab="species")`
![beeStrip](https://github.com/lukereding/graphics/raw/master/examplePlots/beeeStrip.png)

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

Note here that the regression line does not extend beyond the range of the data, the p-value, sample size, and r squared values are given clearly, and there is some measure of uncertainty about the slope (the confidence bands).

`scatter(trees[,1],trees[,2],xlab="tree girth (in.)",ylab="tree height (ft.)",main="scatter() example")`    

![scatter](https://github.com/lukereding/graphics/raw/master/examplePlots/scatter.png)

