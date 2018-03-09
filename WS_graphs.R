#work-study code

#==== load in .csv files ====
connections <- read.csv("connecting_outbreaks.csv")
num_outbreaks <- read.csv("number_of_connecting_outbreaks.csv")
dict <- read.csv("dict_connections.csv")

#==== categorize the interactions ====
# this step is done to merge the two IDs and demonstrate a categorical ID for 
# each interaction

id1 <- c(connections$id_1)
id2 <- c(connections$id_2)
id_interaction <-  vector("list", 1235)
i <- 1
while (i < 1235){
  id_interaction[[i]] <- paste(as.character(id1[i]),as.character(id2[i]))
  id_interaction[[i]] <- gsub(" ", "->", id_interaction[i])
  i <- i+1
}
# bind the merged IDs into the dataframe
connections$id_interaction <- cbind(id_interaction)

#==== clean up the dictionary dataframe ====
# n stands for the other nodes the current node connects to (neighbors)
newcols <- c("node", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9", "n10")
colnames(dict) <- newcols
rownames(dict) <- dict$node
#dict dataframe is now cleaned up (easier to access and understand)

#==== begin checking each of the interactions on the yearly basis==== 
clean <- connections[,colnames(connections)[3:17]]
str(connections)

rowSums(clean[1,] == 1)
clean$two_counts <- rowSums(clean == 2)
clean$zero_counts <- rowSums(clean == 0)
# test_row <- connections[1,]
# j <- 4
# num_changes <- 0
# 
# while(j < 17){
#   if(test_row[j] != test_row[j-1]){
#     num_changes <- num_changes + 1
#   }
#   j <- j+1
# }
# 
# df <- data.frame(newcols)
