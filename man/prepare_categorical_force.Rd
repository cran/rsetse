% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepare_categorical_force.R
\name{prepare_categorical_force}
\alias{prepare_categorical_force}
\title{Prepare categorical features for embedding}
\usage{
prepare_categorical_force(g, node_names, force_var, sum_to_one = TRUE)
}
\arguments{
\item{g}{an igraph object}

\item{node_names}{a character string. A vertex attribute which contains the node names.}

\item{force_var}{A vector of force attributes. This describes all the categorical force attributes of the network. 
All named attributes must be either character or factor attributes.}

\item{sum_to_one}{Logical. whether the total positive force sums to 1, if FALSE the total is the sum of the positive cases}
}
\value{
A network with the correct node attributes for the embeddings process.
}
\description{
This function prepares a binary network for SETSe projection.
}
\details{
The network takes in an igraph object and produces an undirected igraph object that can be used with the embedding functions.

The purpose of the function is to easily be able to project categorical features using SETSe. The function creates new variables
 where each variable represents one level of the categorical variables. For embedding only n-1 of the levels are needed.
 
 The function creates several variables of the format "force_". Vertex attributes representing the force produced by each node 
 for each categorical value, there will be n of these variables representing each level of the categorical values. The variable names 
 will be the the name of the variable and the name of the level seperated by and underscore. For example, with a variable group and levels A and B, the created force variables will be
 "group_A" and "group_B" The sum of these variables will be 0.
}
\examples{
set.seed(234) #set the random see for generating the network
g <- generate_peels_network(type = "E")
embeddings <- g \%>\%
prepare_edges(k = 500, distance = 1) \%>\%
#prepare the network for a binary embedding
prepare_categorical_force(., node_names = "name",
                     force_var = "class") \%>\%
#embed the network using auto_setse
  setse_auto(., force = "class_A")

}
\seealso{
\link{setse}, \link{setse_auto}, \link{setse_bicomp}, \link{setse_auto_hd}

Other prepare_setse: 
\code{\link{prepare_continuous_force}()},
\code{\link{prepare_edges}()}
}
\concept{prepare_setse}
