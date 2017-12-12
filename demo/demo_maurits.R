library(contextual)

bandit <- SyntheticBandit$new(
  k = 2L,
  d = 1L,
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)


## Remarks by Maurits:
## You need to be able to make use of a custom data generating function ..
## Which may imply an AbstractBandit class, from which user developed Bandit classes inherit.
## Which is what Robin already was planning to do, together with an OfflineBandit
## or somesuch class that uses preexisting data to generate

## But maybe its better to change this altogether, context from a context class?
