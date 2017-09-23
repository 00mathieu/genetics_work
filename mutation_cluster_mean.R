library(data.table)
cluster_means <- function(mutation_mat,p_table,n_clusters=30){
    
    # mutation_mat is dt with each row is a run, each col is a mutation.
    # elements of mutation_mat are cluster assignment of each mutation, run.
    # p_table is a dt with the pvalue of each cluster
    # this function returns the mean p_value of each cluster for each run.
    
    mutation_cols <- colnames(mutation_mat)[grepl("M",colnames(mutation_mat))] 
    
    p_table_t <- data.frame(as.list(unlist(p_table)))
    p_table_dt <- data.table(p_table_t)[rep(1,nrow(mutation_mat)),]
    
    # for each run, get mean p_value for each cluster
    cluster_mat <- data.table(RUNS=1:nrow(mutation_mat))
    for(clust_i in 1:n_clusters){
        print(paste("Running",clust_i,"of",n_clusters))
        col_name <- paste("C",clust_i,sep="")
        indx_i <- mutation_mat[,(eval(mutation_cols)),with=F] == clust_i
        cluster_mat[,eval(col_name) := rowSums(p_table_dt*indx_i)/rowSums(indx_i)]
    }
    
    return(cluster_mat)
    
    
}



n_clusters <- 30



# make data test
n_runs <- 10
n_muts <- 5
n_clusters <- 10
mutation_mat <- data.table(RUNS=1:n_runs)
for(col_i in 1:n_muts){
    mutation_mat[,eval(paste("M",col_i,sep="")):=
                    sample(1:n_clusters,n_runs,replace = T)]
}
p_table <- data.table(P_VAL=runif(n_muts))
p_table <- data.table(P_VAL=1:n_muts)

c_mat <- cluster_means(mutation_mat,p_table,n_clusters=n_clusters)

# make data speed test
n_runs <- 10^6
n_muts <- 50
n_clusters <- 30
mutation_mat <- data.table(RUNS=1:n_runs)
for(col_i in 1:n_muts){
    mutation_mat[,eval(paste("M",col_i,sep="")):=
                     sample(1:n_clusters,n_runs,replace = T)]
}
p_table <- data.table(P_VAL=runif(n_muts))
c_mat <- cluster_means(mutation_mat,p_table,n_clusters=n_clusters)
