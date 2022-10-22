# ---
# Correspondence Analysis ============================================== 

# Preliminaries ============ 
setwd("~/OneDrive - University of New Haven/Spring 2022/BANL 6420-Unsupervised Machine Learning/Week 5 2.21.2022")
 remove(list = ls())
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(gplots))
suppressPackageStartupMessages(library(corrplot))
pacman::p_load(ca)

smoker_text = ("
Staff None Light Medium Heavy Total
SM 4 2 3 2 11
JM 4 3 7 4 18
SE 25 10 12 4 51
JE 18 24 33 13 88
SC 10 6 7 2 25")

smoke = read.table(textConnection(smoker_text), sep = "",header = TRUE, stringsAsFactor = FALSE)
#default is a string in R
smoke = smoke %>% dplyr::select(-Total)
smoke
str(smoke)

#' 
#' The Contingency Table with Row and Colum Sums is show below
## ---------------------------------------------------------------------------- -
(smoke_df = smoke %>% tibble::column_to_rownames("Staff"))
(  dt = as.table(as.matrix(smoke_df))  )

#'  Row margins
rowSums(dt)
    (row.sum <- apply(dt, 1, sum)) # equivalent, using apply()

addmargins(dt)
#' 
#' Column margins
colSums(dt)
(col.sum <- apply(dt, 2, sum))

#' grand total

(n <- sum(dt))

#' 
#' Visualize the Contingency Table

balloonplot(dt, main = "Smoker & Rank", xlab = "", ylab = "",
            label = T, 
            show.margins = TRUE, 
            colsrt = c(45))
#look at this and perceptual map and make your decision 
#longer arrows of the perceptual map of 25,18 values compared to other values
#'#' 
#' 
(chi_smoke = chisq.test(dt))


smoke.ca = CA(dt, graph = TRUE)
#look at the map, JM and heavy smoke more     

smoke.ca = ca(dt, graph = TRUE) #do not run this when you run the line 143.(for ellipses***see below-circles in your facto map)
    smoke.ca
    plot(smoke.ca)
#CA and ca are two different packages but we have same results 
(eig.val = get_eigenvalue(smoke.ca))
#' 
#' 
fviz_contrib(smoke.ca, choice = "row", axes = 1)

fviz_contrib(smoke.ca, choice = "row", axes = 2)
#little to explain compare to 1 

#' It can be seen that the row items SE & JE are the most important in the
#' definition of the first dimension
#' 
#' The row items JM & SM contribute the most to dimension 2

#the bigger the line and the close the line, the bigger the relationship between the two lines.
fviz_contrib(smoke.ca, choice = "col", axes = 1)

fviz_contrib(smoke.ca, choice = "col", axes = 2)

fviz_ca_biplot(smoke.ca, map = "rowprincipal" , arrow = c(TRUE,TRUE), repel = TRUE)
fviz_ca_biplot(smoke.ca,  arrow = c(TRUE,TRUE), repel = TRUE)
fviz_ca_biplot(smoke.ca, map = "colprincipal" , arrow = c(TRUE,TRUE),repel = TRUE)
fviz_ca_biplot(smoke.ca, arrow = c(TRUE,TRUE), repel = TRUE)



## Ellipses around some columns only
ellipseCA(smoke.ca,ellipse="col",col.col.ell=c(rep("blue",2),rep("transparent",3)),
          invisible=c("row.sup","col.sup"))

## Ellipses around some rows only
ellipseCA(smoke.ca,ellipse="row",col.row.ell=c(rep("blue",2),rep("transparent",2)),
          invisible=c("row.sup","col.sup"))

#' 
#' ===================
#' 
#' Example No. 2
#' 
#' A sample of 100 housewives were asked which of the 14 statements listed below they
#' associated with any of 8 breakfast foods.  Note that multiple responses were allowed.
#' 
#' The key to the statements and foods is presented below. and the frequency of responses is
#' provided as text below. Data is from Bendixen (2003) 
#' 
#'           STATEMENT
#'           A Healthy 
#'           B Nutritious
#'           C Good in summer
#'           D Good in winter
#'           E Expensive
#'           F Quick and easy
#'           G Tasty Stewed
#'           H Economical
#'           I For a treat
#'           J For weekdays
#'           K For weekends
#'           L Tasteless
#'           M Takes too long to prepare
#'           N Family's favorite
#' 
#'                 BREAKFAST FOODS
#'                 Cereals CER
#'                 Muesli MUE
#'                 Porridge POR
#'                 Bacon and eggs B&E
#'                 Toast and tea T&T
#'                 Fresh fruit FRF
#'                 Stewed fruit STF
#'                 Yogurt YOG
#' 
food_text = ("
RESP CER MUE POR BE TT FRF STF YOG TOTAL
A 14 38 25 18 8 31 28 34 196
B 14 28 25 25 7 32 26 31 188
C 42 22 11 13 7 37 16 35 183
D 10 10 32 26 6 11 19 8 122
E 6 33 5 27 3 9 18 10 111
F 54 33 8 2 15 26 8 20 166
G 24 21 16 34 11 33 26 26 191
H 24 3 20 3 16 7 3 7 83
I 5 3 3 31 4 4 16 17 83
J 47 24 15 9 13 11 6 10 135
K 12 5 8 56 16 10 23 18 148
L 8 6 2 2 0 0 2 1 21
M 0 0 9 35 1 0 10 0 55
N 14 4 10 31 5 7 2 5 78")



#' 
#' Read data into a dataframe
food = read.table(textConnection(food_text), sep = "",header = TRUE, stringsAsFactor = FALSE)
food = food %>% dplyr::select(-TOTAL)
food

#' 
#' Examine Data
str(food)

#' 
#' Convert dataframe into a matrix into a table
#' Conduct a Chi-Square Test to determine if there if breakfast foods vary by statement 
#' 
food_dt = as.table(as.matrix(food[,2:9]))
str(food_dt)
chisq.test(food_dt)


#' Basic Correspondence Analysis
food.ca = CA(food_dt, graph = TRUE)
food.ca

#' 
#' How many eigenvalues?
eig.val = get_eigenvalue(food.ca)
eig.val

#' 
#' Show the Asymmetric Plot of Statements
fviz_ca_row(food.ca, repel = TRUE)

#' 
#' 
#' Show the Asymmetric Plot of Breakfast Food Data
fviz_ca_col(food.ca, repel = TRUE)


#' Show the Symmetric Plot
fviz_ca(food.ca, repel = TRUE)

#' 
#' Show the contribution of Statements to Axis 1
fviz_contrib(food.ca, choice = "row", axes = 1)

#' 
#' Show the contribution of Statements to Axis 2
fviz_contrib(food.ca, choice = "row", axes = 2)

#' 
#' 
#' Show the contribution of Breakfast Foods to Axis 1
fviz_contrib(food.ca, choice = "col", axes = 1)

#' 
#' Show the contribution of Breakfast Foods to Axis 2
fviz_contrib(food.ca, choice = "col", axes = 2)

#' 
#' 
#' Breakfast foods are represented in Statement space.
fviz_ca_biplot(food.ca, map = "colprincipal", arrow = c(TRUE, TRUE), repel = TRUE)

#' 
#' Breakfast foods contribution to the axes

fviz_ca_biplot(food.ca, map = "rowgreen", arrow = c(TRUE, FALSE), repel = TRUE)
#' 
#' 
#' ======================================= +
#' Back to the States
#' Is it possible to Characterize the States in Terms of Economic Variables?
#' 
cnbc = read.csv("cnbc_data.csv")
cnbc_df = cnbc %>% dplyr::select(-X, -OVERALL) %>% tibble::column_to_rownames("State")
str(cnbc_df)

#' 
#' 
#' #===========================================
#' Here pick a small sample to illustrate balloonplots (include Connecticut)
#' 
dt = as.table(as.matrix(cnbc_df))
balloonplot(dt[c(1:5, 35),], main = "State Ranking", xlab = "", ylab = "",
            label = T, 
            show.margins = FALSE, 
            colsrt = c(45))



#' 
#' 
#' Do states vary by the variables examined?
#'

chisq.test(dt)
#' Correspondence Analysis ======================= +
#' 
dt = as.table(as.matrix(cnbc_df))
res.ca = CA(dt, graph = TRUE)
res.ca


(eig.val = get_eigenvalue(res.ca))

#' 
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0,50))

#' 
#' Symmetric 
#' 
fviz_ca_biplot(res.ca,
               arrow = c(TRUE, TRUE),
               repel = TRUE, title = c("State Performance"))

#' 
#' 
ellipseCA(res.ca,ellipse="row",
          col.row.ell=c(rep("transparent",30),
                                             rep("blue",10),
                                             rep("transparent",10)),
          repel = TRUE)
#' 
#' 
ellipseCA(res.ca,ellipse="col",
          col.col.ell=c(rep("blue",1),
                        rep("transparent",9))
          )


#' 
#' Asymmetric

fviz_ca_row(res.ca,
            repel = TRUE)


#' 
#' 
#' Asymmetric

fviz_ca_col(res.ca, map = "colgreen",
            arrow = c(TRUE, TRUE),
            repel = TRUE)

#' 
#' 
#' 
#' 
#' Show the contribution of Variables to Axis 1

fviz_contrib(res.ca, choice = "col", axes = 1)


#' 
#' Show the contribution of Variables to Axis 2

fviz_contrib(res.ca, choice = "col", axes = 2)


#' 
#' Show the contribution of States to Axis 1

fviz_contrib(res.ca, choice = "row", axes = 1)

#' 
#' Show the contribution of States to Axis 2
fviz_contrib(res.ca, choice = "row", axes = 2)

#' =====================================================================================



#' 
