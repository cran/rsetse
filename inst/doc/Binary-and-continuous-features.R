## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, results = 'hide', message=FALSE-----------------------------------
library(rsetse)
library(igraph)
library(dplyr)
library(ggraph)

## ----message=FALSE------------------------------------------------------------
biconnected_network %>%
  ggraph() + 
  geom_edge_link() +
  geom_node_point(aes(colour = group), size = 3) 

## ----message=FALSE------------------------------------------------------------
embeddings_cont <- biconnected_network %>%
  prepare_SETSe_continuous(., node_names = "name", force_var = "force") %>%
  SETSe_auto(., k = "weight")


embeddings_data <- biconnected_network %>%
  prepare_SETSe_continuous(., node_names = "name", force_var = "force") %>%
  SETSe_auto(., k = "weight")

out <- create_node_edge_df(embeddings_data, function_names = c("mean", "mode", "sum"))


## ----message=FALSE------------------------------------------------------------

embeddings_cont_fixed <- biconnected_network %>%
  prepare_SETSe_continuous(., node_names = "name", force_var = "force", k = 500) %>%
  SETSe_auto(., k = "k")


## ----message=FALSE------------------------------------------------------------

continuous_results <- bind_rows(create_node_edge_df(embeddings_cont) %>% mutate(type = "variable k"),
          create_node_edge_df(embeddings_cont_fixed) %>% mutate(type = "fixed k")
 ) 

continuous_results %>% 
  ggplot(aes(x = tension_mean, y = elevation, colour = node)) + geom_jitter() +
  facet_wrap(~type)  +
  facet_wrap(~type) +
  labs(title = "Continuous embeddings",
       x = "mean tension")


## ----message=FALSE------------------------------------------------------------

embeddings_binary <- biconnected_network %>%
  prepare_SETSe_binary(., node_names = "name", force_var = "group", positive_value = "A") %>%
  SETSe_auto(., k = "weight")


embeddings_binary_fixed <- biconnected_network %>%
  prepare_SETSe_binary(., node_names = "name", force_var = "group", positive_value = "A", k = 500) %>%
  SETSe_auto(., k = "k")


binary_results <- bind_rows(create_node_edge_df(embeddings_binary) %>% mutate(type = "variable k"),
          create_node_edge_df(embeddings_binary_fixed) %>% mutate(type = "fixed k")
 ) 

binary_results %>% 
  ggplot(aes(x = tension_mean, y = elevation, colour = node)) + geom_jitter() +
  facet_wrap(~type) +
  labs(title = "Binary embeddings",
       x = "mean tension")


## ----message=FALSE------------------------------------------------------------

left_join(continuous_results, binary_results, by = c("node", "type")) %>%
  ggplot(aes(x = elevation.x, y = elevation.y, colour = node)) + geom_jitter() +
  facet_wrap(~type) +
  labs(title = "Node elevation for two different features",
       x = "elevation with continuous embedding",
       y = "elevation with binary embedding")



