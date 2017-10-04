# Bayesian Change Point Analysis of Trump's Approval

This app automatically downloads data from the [Huffpost poll aggregator](http://elections.huffingtonpost.com/pollster/trump-job-approval). It then performs a Bayesian change-point analysis of that data to identify key periods when public opinion shifted

## How it works
It is based in the [R computing language](https://www.r-project.org/), using the [bcp package](https://cran.r-project.org/web/packages/bcp/index.html) developed by Xiaofei Wang, Chandra Erdman, and John W. Emerson<sup>1</sup>. The program is based on the Barry-Hartigan method<sup>2</sup>.

It uses a very conservative prior (.001) to minimize noise in the data. Error is aggregated from all polls. There are 2,000 burn-ins and 10,000 markov-chain Monte Carlo resimulations of the data to calculate the posterior means & posterior probabilities.

You, the user, can choose which ones and which populations to focus on. Make your selections then click "Go"

## How to use the app

First, you will need to download a copy of R appropriate for your computer (Mac, Windows, or Linux): 

>https://www.r-project.org/

Next, you will need to install a package called 'shiny' to run it locally. You can do so by pasting this line into the R consul when you launch it:

>install.packages("shiny")


It will ask you to choose a download mirror (you can choose anyone, the result is the same). Then, to run the software:

>shiny::runGitHub("leedrake5/trumpChangePoint")

The first time it runs, it may take some time to download the supporting software. After that, you should be good to go. If you'd like to download a copy and run it offline, you can instead download it from GitHub (https://github.com/leedrake5/CloudCal) and then run it locally:

>shiny::runApp("your/computer/directory/trumpChangePoint")


## How to cite this software
Beats me - this is the first time I've tried to do anything like this. Drop me an email at b.lee.drake@gmail.com if you have questions or need to address this step. 


## References
1. Erdman, C, Emerson, J.W. 2007. bcp: An R package for performing a bayesian analysis of change point problems. Journal of Statistical Software 23(3): 1 - 13

2. Barry, D, Hartigan, J.A. 1993. A Bayesian analysis for change point problems. Journal of the American Statistical Association 35(3), 309 - 319



