=====================================================================

ScienceRockstars / Webpower b.v. Data disclosure ReadMe

The data included here is provided for research purposes only by Science Rockstars / Webpower b.v. The data can be used for (academic) research purposes free of charge, however any public usage of the data (e.g., usage in academic publications, blog posts, etc. should acknowledge the source of the data.

The data is derived from the more extensive and bigger persuation data file that can be downloaded freely from http://www.nth-iteration.com/downloads

=====================================================================

Description:

The dataset contains data of 10000  (rows) displays of “persuasive strategies” to users of an online store. The data look like this:

The columns are comma separated and contain the following information (columns):

- choice: strategy id, the type of persuasive strategy displayed
	  values:	1: (no strategy)
			2: Authority (e.g., “recommended product”)
			3: Social proof (e.g., “bestseller”)
			4: Scarcity (e.g., “almost out of stock”)
		
- user: the id of the current user. The dataset often contains multiple observations per user. 
- reward: the success of the display (1 for a click, 0 for no response)

The strategies are chosen uniformly at random. Thus, this dataset allows one to run offline evaluations of a contextual bandit algorithm (see http://dx.doi.org/10.1145/1935826.1935878). In this case, the sid (0-4) are the possible actions, the scs (0,1) are the rewards, and the uid or the time can be used as context.

=====================================================================

Complexities in data collection:

Each display of a persuasive strategy accompanied a product displayed in an online store. The store has conceptually two types of pages:

- Product overview pages: On these pages multiple (up to 20) products are presented. Randomly, some of these (in principle 5, but this can differ if there is a small number of product on the page) products displays are accompanied by a persuasive message. A success is a click on the product taking the user to the product detail page.

- Product detail pages: On these pages a single product is displayed (users might arrive here directly through search results), a single strategy is displayed, and adding the product to a shopping basket is counted as a success.

The two are (regretfully) hard to distinguish. One could choose to include only those instances in which 5 strategies are shown (product overview pages). Otherwise, the timestamps could be used to determine the most logical path of the user.


====================================================================