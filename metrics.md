
## Information about the metric reported in this app

### Missing rate

The proportion of students in each group with missing data. This is extremely important to check. If there is a large amount of missing data, or worse - if the proportion of missing data varies across groups, then the statistics reported by the app are likely to be highly misleading.

### Count

The total number of students in each group, as well as the number of identified students in both the baseline system and via Optimal Identification.

### Poportion identified 

The proportion of students in each group who are identified.

$$\text{Proportion identified} = \frac{\text{number of students in group and identified}}{\text{number of students in group}}$$

### Representation Index

Measures a group's representation in the gifted program relative to their proportion in the district or school population. Values near 1 indicate proportional representation; less than 1 indicate underrepresentation.

$$\text{Representation index} = \frac{\text{Proportion of gifted students in a group}}{\text{Proportion of population in a group}}$$

For example, if African American students are 20% of the gifted population but 40% of the district population, then $RI = \frac{.2}{.4} = 0.5$

### Relative Risk 

Measures a group's representation in the gifted program relative to a reference group. Values greater than one indicate that the target group is overrepresented relative to the reference group; values less than one indicate underrepresentation.

$$\text{Relative risk} = \frac{\text{Proportion of target group identified for gifted education}}{\text{Proportion of reference group identified for gifted education}}$$

For example, if 10% of African American students (the target group) are identified, while 16% of White students (the reference group) are identified, then $RR = \frac{0.10}{0.16} = 0.625$.

### Cramer's V 

**Note**: we have not found this statistic to be useful.

Cramer's V measures the discrepancy between two sets of proportions. To illustrate, suppose we have the following data:

| Category         	| District Population &nbsp; &nbsp; 	| Gifted Population 	|
|------------------	|---------------------	|-------------------	|
| White            	| 60%                 	| 70%               	|
| African American&nbsp; &nbsp;	| 10%                 	| 4%                	|
| Hispanic         	| 20%                 	| 6%                	|
| Asian            	| 10%                 	| 20%               	|


We note that the White and Asian groups are overrepresented while the Black and Hispanic groups are underrepresented. We could use the *Representation Index* to calculate a statistic for each group to indicate the severity of its under- or over-representation. But this would require four numbers. Similarly, we could compute the *Relative Risk* of each group compared to a reference (often, the White category). But this would require three numbers.

The purpose of Cramer's V is to provide a single value which measures the disproportionality of a system. Cramer's V ranges from 0 to 1, where 0 indicates no association between demographic categories and identification status and 1 indicates perfect association. (For example, if all gifted students came from a single group, this would be a V value of 1.0). Lower is better.

We investigated Cramer's V as a potential single-number measurement that would allow an easy comparison between an existing identification system and an alternative system. Unfortunately, we discovered a few important limitations to Cramer's V. The biggest one is that the two systems need to identify the same total number of students to make the values truly comparable.

We have left Cramer's V in the app for experimental purposes, but cannot recommend it for routine  interpretation. 

