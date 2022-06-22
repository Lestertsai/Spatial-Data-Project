# Using Spatial Statistics to Investigate Factors Affecting Crime Rate in Taipei City

Motivation

This was a graduate project conducted during my senior. We chose spatial-related datasets in Taipei City as the topic. It differs from the data we handled before as it allows us to consider the distance and boundary between each region when building the model. We hope to understand better whether the crime rate varies across different areas and how the data we found could explain them.

How were the datasets collected?

Most of the data were web scrawled from [Data.Taipei](https://doit.gov.taipei/News_Content_Pic.aspx?n=B8BB290411AD6ACB&amp;sms=4C4B9756C103EDDE&amp;s=8445981ECE92F8ED), a website containing hundreds of open datasets related to Taipei City. Since the crime data were in address format, we transformed them using Google Map into the coordinate format using Python. Then all the data were categorized into 907 secondary dissemination areas in Taipei City, which served as the basis for the regions we analyzed later. Here, the dissemination area is a  [statistical area categorization approach](https://ws.moi.gov.tw/001/Upload/OldFile/site_node_file/6263/%E7%B5%B1%E8%A8%88%E5%8D%80%E5%88%86%E9%A1%9E%E7%B3%BB%E7%B5%B1%E5%BB%BA%E7%BD%AE%E5%8F%8A%E6%87%89%E7%94%A8.pdf) administered by the Ministry of the Inferior in Taiwan to support spatial statistics analysis. On the other hand, all the explanatory variables were gathered from [social-economic information platform](https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx) also in the secondary dissemination area basis, including the number of hospitals, schools, stores, and security cameras, sex ratio, and the percentage of elderly and adolescents, to name a few.

How was the analysis performed?

The package we utilized is [INLA](https://www.r-inla.org/home), a popular package for analyzing spatial data. It uses the Integrated Nested Laplace Approximation, a deterministic Bayesian method, to model data. Our model's random effects included BYM(Besag-York-Mollié) and BYM2(Simpson et al., 2017). The former considers that data may be spatially correlated and the idea that neighboring areas might have similar observations. The latter is a new parametrization of the BYM model, making the parameters more interpretable.

Conclusion

We found out that the crime rate in Taipei City was mainly influenced by the population of elders and the number of stores. The interaction effects between each independent variable were also significantly related to the outcome variable. The result was not surprising as we realized there were scores of factors involved in the occurrence of crime. Also, the BYM2 was a more appropriate random effect for our model as the parameters were more spatially clustered than the BYM.

How to improve the project?

One improvement of the research is to include more temporal datasets from the previous years so we can apprehend whether time also plays a role in the crime rate. Moreover, spatial autocorrelation exists in the model's residual, suggesting that there might be other latent variables that are not included in our model. If we could gather other datasets, our conclusion would be more convincing.

Appendix

Below are some resources we found useful when doing the project.

[Hierarchical modelling of spatial data](https://ourcodingclub.github.io/tutorials/spatial-modelling-inla/)

[INTRO TO MODELLING USING INLA](https://ourcodingclub.github.io/tutorials/inla/)

[Chapter 5 Areal data](https://www.paulamoraga.com/book-geospatial/sec-arealdatatheory.html)

Here is the [link](https://uwnetid-my.sharepoint.com/:b:/r/personal/lester13_uw_edu/Documents/%E6%9B%B8%E9%9D%A2%E5%A0%B1%E5%91%8A-12.pdf?csf=1&amp;web=1&amp;e=lhpRSn) to the original project file we made in Chinese. Please do not hesitate to reach out if you want to learn more about our project!
