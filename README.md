<h2>Predicting Credo in J&J Research and Development Purchase Orders</h2>  
<p> ---- Interpretation of Data II course project</p>
<h3><a href="https://www.dropbox.com/s/jf6ojrqx18jaqyf/IntpofDataII-Report.pdf?dl=0">>>Report Link<<</a></h3>
<h3>Keywords:</h3>
<p>Exploratory Data Analysis, Clustering, Factor Analysis, Bayesian Network, Generalized Linear Model</p>
<h3>Packages:</h3>
<p>dplyr, dlookr, biclust, bnlearn, glmnet, stats, utils, datasets</p>
<h3>Summary:</h3>
<p>We Used the dataset provided by J&J of the purchase orders from the previous 16 months to predict if supervisors are likely
to purchase from minority owned businesses in the 17th month. The data we focused on is the CLPS (Consulting Labor Professional Services) area data. The CLPS dataset is a collection of purchase 907,408 orders specifically in research and development. Dataset was analyzed with Bayesian Networks and GLMnet models.We wanted to establish connections between subgroups of the data over time and emphasis in small businesses.
</p>

<h3>Variable Groups:</h3>
<img width="350" alt="screen shot 2019-01-14 at 6 15 12 pm" src="https://user-images.githubusercontent.com/32077985/51147553-9a6b6a00-1828-11e9-8736-86ffcbe7d851.png">

<h3>Processes:</h3>
<p>
<ol>
<li>Exploratory Data Analysis</li>
<li>Biclustering and Grouping</li>
<ul>
<li>Clustering with PAM</li>
<li>Biclustering
<ul><li>Biclust:available from CRAN, can be used for biclustering analysis. Several algorithms can find biclusters in two dimensional data.</li>
<li>FABIA:Factor Analysis for Bicluster Acquisition, is a biclustering method based on the Factor Analysis Model.</li>
<li>Isa2:a biclustering algorithm which classifies simultaneously the rows and columns of an input matrix into biclusters.</li></ul></li>
</ul>
<li>Bayesian Networks</li>
<li>GLM</li>
</ol>
</p>
