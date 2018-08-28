# Data-Analytic-Examples
Predicting Employee Attrition


The goal of this project is to reduce turnover by predicting if an employee will leave the company.  This will allow the company to reduce the high cost of turnover and increase employee morale.  The challenge is to identify employees who were going to leave and not the employees who were going to stay even without any intervention.	
  The data set consists of 30 variables including demographics, employment and job details, subjective survey results and one attrition classifier.  There are 1470 observations, 237 of which were attritions; returning a turnover rate of 19%.  Almost double the Industry standard for turnover of 10%. I propose we require 1.5 years experience for positions that DO NOT have overtime, conduct a 360 job analysis for sales representatives, human resource personnel and lab technicians and overhaul our overtime policy to bring attrition in line with the industry standard.
  The first model I ran was a decision tree.  Whether an employee worked overtime immediately stood out as an influencing factor.  90% of employees who DO NOT work overtime stay with the company.  This is within the 10% industry standard and indicates that our high attrition rate is coming from our employees who DO work overtime.  To back up this statement, a logistic regression analysis of all employees shows employees who DO work overtime have an 89% probability of attrition.  Therefore our overtime policy should be looked at and possible revised.  For the remainder of the analysis, I will look at employees who DO work overtime separately from employees who DO NOT.
  Although employees who DO NOT work overtime have a turnover rate within industry standards, they currently make up 77% of the workforce and therefore should still be analyzed.  A random forest analysis indicates that the strongest factors contributing to attrition for this group are total working hours, age and job role.  Because we cannot affect the age of our employees, I looked at total working years and job role in a logistic regression analysis.
  This analysis shows that employees who DO NOT work overtime and have less than 1.5 total working years have an 88% probability of attrition.  We could consider adjusting our hiring practices to require more experience for positions that DO NOT have overtime.  Also, employees who DO NOT work overtime and work as a sales representative, in human resources or as a lab technician have an 84%, 78% and 73% probability of attrition respectively.  We may want to consider a 360 analysis of these job roles to help identify issues contributing to attrition.
  For employees who DO work overtime, a decision tree analysis shows that the highest number of attrition comes from employees who have a monthly income less than $3,750 and are less than 30.5 years old.  Again, adjusting our hiring practices to require more experience may elevate some of this attrition.  
	To reduce our turnover rate to be in line with the industry standard rate of 10%, we need to reduce attrition from 237 to 123 employees.  This means we need to stop 114 attritions from occurring.  I propose will follow the following three steps to make this happen.
  First, require 1.5 years experience for position that DO NOT have overtime (currently if less than 1.5 years there is an 88% probability of attrition), we could reasonably stop 30 of the 40 attritions.  Second, conduct a 360 job analysis for sales representatives, human resource personnel and lab technicians could reasonably stop 48 of the current 240 attritions.  And finally, change our overtime policy could reasonably stop 36 of the current 289 attritions occurring with our overtime employees.  
