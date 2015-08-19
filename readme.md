[![Build Status](https://travis-ci.org/johnbaums/trees.svg?branch=master)](https://travis-ci.org/johnbaums/trees)

With `trees`, you can:

- generate a random tree design (a _random seed_, so to speak) with `seed()`;
- sow the seed to give rise to a magnificent tree with `germinate()`;
- add randomly-located leaves (or squirrels) with `foliate()`; 
- add squirrels (for example) to specified locations with `squirrels()`; and
- `prune()` the tree.


----------
    # Install the package and set the RNG state
    devtools::install_github('johnbaums/trees')
    set.seed(1)

**Let's fertilise a seed and grow a tree**

    # Create a tree seed    
    s <- seed(70, 10, min.branch.length=0, max.branch.length=4,
              min.trunk.height=5, max.trunk.height=8)
    
    head(s, 10)
    
    #       branch    length
    # 1          0 6.3039785
    # 2          L 2.8500587
    # 3         LL 1.5999775
    # 4        LLL 1.3014086
    # 5       LLLL 3.0283486
    # 6      LLLLL 0.8107690
    # 7     LLLLLR 2.8444849
    # 8    LLLLLRL 0.4867677
    # 9   LLLLLRLR 0.9819541
    # 10 LLLLLRLRR 0.5732175

    # Germinate the seed
    g <- germinate(s, col='peachpuff4')
![enter image description here][1]


**And add some leaves**

    leafygreens <- colorRampPalette(paste0('darkolivegreen', c('', 1:4)))(100)
    foliate(g, 5000, 4, pch=24:25, col=NA, cex=1.5, bg=paste0(leafygreens, '30'))

![enter image description here][2]

**Or some squirrels**

    plot(g, col='peachpuff4')
    squirrels(g, 
              branches=c("LLLLRRRL", "LRLRR", "LRRLRLLL", "LRRRLL", "RLLLLLR", 
                         "RLLRL", "RLLRRLRR", "RRRLLRL", "RRRLLRR", "RRRRLR"),
              pos=c(0.22, 0.77, 0.16, 0.12, 0.71, 0.23, 0.18, 0.61, 0.8, 2.71),
              pch=20, cex=2.5)

![enter image description here][3]

**Plotting @Remi.b's tree and squirrels**

    g <- germinate(list(trunk.height=32, 
                       branches=c(1, 2, 11, 12, 121, 122),
                       lengths=c(21, 19, 5, 12, 6, 2)), 
                  left='1', right='2', angle=40)
    
    xy <- squirrels(g, c(0, 1, 121, 1, 11), pos=c(23, 12, 4, 2, 1), 
                   left='1', right='2', pch=21, bg='white', cex=3, lwd=2)
    text(xy$x, xy$y, labels=seq_len(nrow(xy)), font=2)
    legend('bottomleft', bty='n',
          legend=paste(seq_len(nrow(xy)), 
                       c('FluffyTail', 'Ginger', 'NutCracker', 'SuperSquirrel', 
                         'ChipnDale'), sep='. '))

![enter image description here][4]


  [1]: http://i.stack.imgur.com/96p9v.png
  [2]: http://i.stack.imgur.com/w7cZ1.png
  [3]: http://i.stack.imgur.com/LlGks.png
  [4]: http://i.stack.imgur.com/6H9cj.png