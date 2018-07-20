library(doAzureParallel)

# 1. Generate your credential and cluster configuration files.
doAzureParallel::generateClusterConfig("cluster.json")
doAzureParallel::generateCredentialsConfig("credentials.json")

# 2. Fill out your credential config and cluster config files.
#    Enter your Azure Batch Account & Azure Storage keys/account-info into your
#    credential config ("credentials.json") and configure your cluster in your
#    cluster config ("cluster.json")

# 3. Set your credentials - you need to give the R session your credentials to
#    interact with Azure
doAzureParallel::setCredentials("credentials.json")

# 4. Register the pool. This will create a new pool if your pool hasn't already
#    been provisioned.
cl <- doAzureParallel::makeCluster("cluster.json")

# 5. Register the pool as your parallel backend
doAzureParallel::registerDoAzureParallel(cl)

# 6. Check that your parallel backend has been registered
workers = foreach::getDoParWorkers()
message(paste0("Workers: ",workers))

clusters <- doAzureParallel::listClusters()
message(paste0("List of clusters: ",clusters))

doAzureParallel::stopCluster(cl)
