# rshiny app code for startup investments

# Introduction:
Invention and Innovation drive the economy. Investors want to invest in great products and services with a competitive edge that is long lasting. They look for products and services that customers cannot do without – because it is so much better or because it is so much cheaper than anything else in the market. So, naturally, Investors want to know what they are getting into when they take a stake in an early stage company. With so many investment opportunities and start-up pitches, Investors often have a set of criteria that they look for and evaluate before making an investment such as management team, business concept and plan, market opportunity, geographic location and risk judgement etc. and all play a role in making this decision. With such a motivation I took this opportunity to design an application using my knowledge in R shiny, that helps making sound investment decisions. 
# Glossary of Funding Terms
Seed Funding: 
Seed funding is the first official equity funding stage. It typically represents the first official money that a business venture or enterprise raises. 

Angel Funding: 
An angel investor (also known as a private investor, seed investor or angel funder) is a high net worth individual who provides financial backing for small startups or entrepreneurs, typically in exchange for ownership equity in the company. Often, angel investors are found among an entrepreneur's family and friends. The funds that angel investors provide may be a one-time investment to help the business get off the ground or an ongoing injection to support and carry the company through its difficult early stages.

Venture capital:
Venture capital is funding typically allocated to small companies with exceptional growth potential, or to companies that have grown quickly and appear poised to continue to expand by well-off investors.

Debt financing:
Debt financing occurs when a firm sells fixed income products, such as bonds, bills, or notes, to investors to obtain the capital needed to grow and expand its operations. 

Funding rounds:
Number of times an additional money that goes into investment called rounds (Series A, B.. Etc. or round 1, 2..etc), some companies never extend beyond seed funding into Series A rounds or beyond. 

# Summary of the data set:
The dataset I used is from Kaggle.com, and it is purely based on the data taken from CrunchBase, a leading investment platform. This dataset contains information on new companies started from year 1991 till 2014 in different market segments. Let us first look at how many new companies were founded by year.
 
We clearly see that there is a rise in no of companies during 2007 and 2012 and the slowed down the creation of new companies started to decline drastically in 2013 and 2014. It could be due to the recession during that time.
Now let us look at what market segment has . Do most of the companies belong to certain category? so we will have an idea of the leading markets to make investments on. As we have around 754 categories of startup, I have plotted the top 10. 

 
The most popular category is still about Software maybe because it is easily scalable. Why software write ..during recission less money so software is cheaper to invest on..
Funding plays a key role in any business, let us look at which category has got the most funding.

 



Software, Health care, biotechnology and semiconductors has got the higher funding compared to other categories. We need to find out if the highest funding has any role in the success or failure of the companies. It is a log scale.

 


Most of the companies (86.9 %) in this dataset are still operating, and around 5.4 % companies are closed.
Please use my interactive shiny app here to look at more closely by geographic location they were found and if it had any impact.

# Conclusion:
There is a clear potential in software or biotech industry in USA as the number of the new companies were high compared to the rest of the world. Unfortunately, the data is available only till 2014 at the time of this writing. 


