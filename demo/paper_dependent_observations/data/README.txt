=====================================================================

ScienceRockstars / Webpower b.v. Data disclosure ReadMe

The data included here is provided for research purposes only by Science Rockstars / Webpower b.v. The data can be used for (academic) research purposes free of charge, however any public usage of the data (e.g., usage in academic publications, blog posts, etc. should acknowledge the source of the data.

The data can be downloaded freely from http://www.nth-iteration.com/downloads

=====================================================================

Description:

The dataset (single file, “srs-rand-54acd5db0cf2568d17c6ff68.gz”) contains data of 3103598  (rows) displays of “persuasive strategies” to users of an online store. The data look like this:

"550aa64c0cf253d5f6173e5a",1,"89ce959b-11a4-49f9-a671-0c2f0c0bf883",0,1426761292
"550aa64c0cf253d5f6173e5b",1,"89ce959b-11a4-49f9-a671-0c2f0c0bf883",0,1426761292
"550aa64c0cf253d5f6173e5c",2,"89ce959b-11a4-49f9-a671-0c2f0c0bf883",0,1426761292


The columns are comma separated and contain the following information (columns):

- aid: the id of the display instance (also called “adviceid”. These are unique.)
- sid: strategy id, the type of persuasive strategy displayed
	values:	0: (no strategy)
		1: Authority (e.g., “recommended product”)
		2: Social proof (e.g., “bestseller”)
		3: Scarcity (e.g., “almost out of stock”)
		
- uid: the id of the current user. The dataset often contains multiple observations per user. 
- scs: the success of the display (1 for a click, 0 for no response)
- stamp: the logged time in seconds of the display

The strategies are chosen uniformly at random. Thus, this dataset allows one to run offline evaluations of a contextual bandit algorithm (see http://dx.doi.org/10.1145/1935826.1935878). In this case, the sid (0-4) are the possible actions, the scs (0,1) are the rewards, and the uid or the time can be used as context.

=====================================================================

Complexities in data collection:

Each display of a persuasive strategy accompanied a product displayed in an online store. The store has conceptually two types of pages:

- Product overview pages: On these pages multiple (up to 20) products are presented. Randomly, some of these (in principle 5, but this can differ if there is a small number of product on the page) products displays are accompanied by a persuasive message. A success is a click on the product taking the user to the product detail page.

- Product detail pages: On these pages a single product is displayed (users might arrive here directly through search results), a single strategy is displayed, and adding the product to a shopping basket is counted as a success.

The two are (regretfully) hard to distinguish. One could choose to include only those instances in which 5 strategies are shown (product overview pages). Otherwise, the timestamps could be used to determine the most logical path of the user.


=====================================================================

Usage example:

Here is a very brief example of the possible use of the dataset using [R]:

> library(readr)
> d <- read_delim("srs-rand-54acd5db0cf2568d17c6ff68.gz", ",", col_names=FALSE)
|================================================================================| 100%  239 MB
B
> names(d) <- c("aid", "sid", "uid", "scs", "stamp")
> dim(d)
[1] 3103598       5
> 
> glm(scs ~ as.factor(sid), data=d, family="binomial")

Call:  glm(formula = scs ~ as.factor(sid), family = "binomial", data = d)

Coefficients:
    (Intercept)  as.factor(sid)1  as.factor(sid)2  as.factor(sid)3  
        -4.6115           0.1158           0.1244           0.0646  

Degrees of Freedom: 3103597 Total (i.e. Null);  3103594 Residual
Null Deviance:	    365200 
Residual Deviance: 365200 	AIC: 365200


This brief analysis shows that, over all complexities of the dataset, the products that are not accompanied by any labels are the least clicked, followed by scarcity, authority, and social proof, in that order.
