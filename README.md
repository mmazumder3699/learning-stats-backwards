# Learning Statistics Backwards

## Introduction and setup

For details on how to install software and download the tutorials, [click here](https://jaeyoungson.com/tutorials.html). You'll be following the instructions for the sister tutorial series `into-the-tidyverse`, but most of the instructions apply just as well for `learning-stats-backwards`.


## File and folder structure

- Files in the parent folder: Whenever you want to engage with these workshops, remember to first double-click on `learning-stats-backwards.Rproj` first. This opens up a new R session (so that work you're doing for this project doesn't disturb whatever work you might be doing for other R projects).

- Tutorials: This is where you can find the HTML files containing all tutorials. Each one features a self-contained lesson on a particular topic.

- Sandbox: This is the only folder you should be modifying. You should never, ever, ever modify anything that's in any other folder. Seriously, under most circumstances, there should be no reason for you to ever navigate into these directories or open these files. If you're wanting to save scripts, plots, whatever, you should do it in the Sandbox. Make sure to follow these three simple rules to make sure everything goes smoothly:

	1. Basically, don't touch anything in the Code and Data folders, and you'll be alright. For those of you following along in realtime (i.e., I haven't finished writing/developing all of the materials yet), this lets you avoid merge conflicts. In short, a merge conflict emerges when my version of the file looks different from your version of the same file. So if you take my code and you use it to play around by adding/deleting code, your file is now different from my file. When you try to "sync" new materials from my repository to your forked clone, this creates a merge conflict that you're forced to resolve before you're allowed to download my new materials.
	
	2. Any time you write new code, do it in this Sandbox folder. The point of a sandbox is that you can do anything you want, and the (potentially destructive) consequences of your actions will never leave the sandbox. This is important because I'm going to encourage you very, very, very strongly never to simply run the existing scripts in the Code folder (or equivalently, simply copy/pasting existing code). So when you're typing code from scratch and need a place to save your scripts, save them in the Sandbox.
	
	3. On that note, you should be typing code from scratch. This sandbox should be full of code, whether it works or not. You learn a lot from typing things from scratch, especially when you're a beginner. This is because you'll inevitably type things incorrectly when you do it from scratch, and then you'll have to self-diagnose why your code doesn't work. That is invaluable experience for learning how to code. And it will also help with your muscle memory. It takes virtually zero time for me to write a pipe (%>%) because I have typed thousands and thousands and thousands of pipes. You cannot get good at writing code quickly without building muscle memory. If you simply run existing code, or copy/paste existing code, you are hurting your own learning. There are no shortcuts to learning code. You must do it the hard way.

- Code: I'm providing all of the `R Markdown` code in the Code folder. These scripts are provided primarily for folks with visual impairments, who would find it useful to have the text for accessibility reasons. By and large, I do **NOT** want any other group of people looking at this code. Why? Because most of your learning occurs when you make (and correct) mistakes, and you can only make mistakes if you're trying things out from scratch. You can't make mistakes if you're simply running someone else's code, or just copy/pasting code.

- Data: We will be using a diverse range of datasets from a variety of sources. There are times when it will make sense for you to directly download the data from the source (such as timeseries data on covid19, which is updated on a daily basis), but otherwise, the datasets will be provided in the Data folder. When applicable/available, the URL to the original dataset will be documented in the relevant scripts.

- Output: Sometimes, there'll be some kind of output I want to save to your hard drive (like a plot or table), so that you have a sense for what your output ought to look like. These will be saved in the Output folder. Anytime you have some output, you should save that to the Sandbox instead.


## Session descriptions

1. An introduction to statistical modeling.
	- The paradox of learning from first principles.
	- My approach: learning statistics backwards.
2. Simple regression.
	- What does it mean to test a hypothesis?
	- Hypothesis test: change depends on quantities.
	- Hypothesis test: change depends on categories.
	- What about correlations? The t-test?
3. Multiple regression I: continuous variables
	- Hypothesis test: change depends on many quantities.
	- The problem of shared variance, and the beauty of semi-partial estimates.
	- Interpreting semi-partial model estimates.
4. Multiple regression II: categorical variables
	- Hypothesis test: change depends on many categories.
	- Why it's commonly said that categorical variables make analysis more complicated.
	- Why that's not really true.
	- Using regression to test the questions you actually care about: smart use of contrasts.
5. Multiple regression III: interactions
	- Hypothesis test: change depends on change...?
	- Mixing continuous and categorical predictors.
6. Parametric models.
	- What is an expectation?
	- Distributional thinking.
	- (Re)introducing the Gaussian distribution.
7. Generalized linear models.
	- Non-Gaussian distributions.
	- Logistic and Poisson regression.
8. Multilevel modeling.
	- The problem of repeated measures.
	- The logic of mixed-effects regression.
	- Introduction to `lme4`.


## Useful resources
1. [R for Data Science](https://r4ds.had.co.nz/)
	- Oriented around data science workflows, and less around statistics
	- Extremely accessible to people who know nothing about programming
	- Makes extensive use of the tidyverse, which is a collection of R packages that streamlines the process of cleaning/analyzing data
	- Written by one of the lead programmers of many tidyverse packages
2. [Learning Statistics with R](https://learningstatisticswithr.com/)
	- Oriented around statistics
	- Extremely accessible to people who know very little about statistics or programming
	- Teaches statistics from first principles, and focuses on conceptual underpinnings
	- Written by a psychologist whose speciality is in computational modeling
3. [Deep dive into statistics using R](http://users.stat.umn.edu/~helwig/teaching.html)
	- A wealth of resources for people who want more statistical grounding and translations of mathematical formalisms into R code
	- Covers everything from simple linear regression to multivariate/nonparametric statistics
	- Great resource for brushing up on advanced statistical concepts
4. [Advanced R](https://adv-r.hadley.nz/)
	- For people who are specifically interested in doing a deep-dive into R *as a programming language*
	- Not really necessary for the average user, but may be a good resource if you're getting a weird error message and want to figure out exactly what it means
5. [An introduction to data analysis](https://michael-franke.github.io/intro-data-analysis/index.html)
	- A really detailed introduction to Bayesian and frequentist data analysis, with a nice introductory section on data wrangling and visualization (in case my materials just don't work for you!)
6. [Doing Bayesian data analysis (in brms and the tidyverse)](https://bookdown.org/content/3686/)
	- A herculean effort by Solomon Kurz to translate John Kruschke's excellent Bayesian data analysis book into use with the `brms` library as well as the tidyverse.
