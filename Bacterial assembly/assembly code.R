library(iCAMP)
library(picante)

asv <- read.csv('Abundant.csv', row.names = 1)
comm <- data.frame(t(asv))

tree <- read.tree('tree.nwk')

tree$tip.label = gsub("'", "", tree$tip.label)

is.rooted(tree)

tree = root(tree, 1, r = TRUE)
is.rooted(tree)

comm_species <- colnames(comm)
tree_species <- tree$tip.label

missing_in_tree <- setdiff(comm_species, tree_species)

missing_in_comm <- setdiff(tree_species, comm_species)

# 打印缺失的物种信息
if(length(missing_in_tree) > 0) {
  cat("以下物种在comm中存在，但在tree中缺失:\n", paste(missing_in_tree, collapse=", "), "\n")
} else {
  cat("comm中的所有物种都在tree中存在。\n")
}

if(length(missing_in_comm) > 0) {
  cat("以下物种在tree中存在，但在comm中缺失:\n", paste(missing_in_comm, collapse=", "), "\n")
} else {
  cat("tree中的所有物种都在comm中存在。\n")
}

tree <- prune.sample(comm, tree)

comm <- match.phylo.comm(tree,comm) 

comm <- comm$comm

pd <- cophenetic(tree) 

set.seed(123)
qpen.out <- qpen(comm = comm, pd = pd, sig.bNTI = 2, sig.rc = 0.95, rand.time = 1000, nworker = 8)

qpen.out$ratio

head(qpen.out$result) 

write.csv(qpen.out, 'process.csv', row.names = FALSE)
