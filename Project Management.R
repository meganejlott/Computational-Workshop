###PROJECT MANAGEMENT###
#Megan Lott, megan.lott@uga.edu

##################################################################################################################
####################################################NOTES#########################################################
##################################################################################################################

#File Structure - use one working directory for one project. Will help you map this to 
#Where should you keep the data? Unless the data is too large for storage, store and keep the data in your working 
#directory. Keep all of your statistical and analytical work in one R file. 
#Data pre-processig should be done in the R environment (preprocess.r), all visualization, etc. 
#source("preprocess.R") - as though all of the script written in preprocess document is in the current script.
#Git and Github - standard way for computer programmers to manage projects. #Git is a process (software) that manages changes, 
#Github is a repository that provides convenient means to archive information and openly present information 


#Statistically literate programming using R Markdown: 
  #R Markdown: markup languages, such as  html, is very simple. Markdown is a pun to help link R to create statistically 
  #literate reports - presentations, shiny apps, dashboards, books, websites 
  #all of the documents in this course were created in R Markdown
  #create cv in R Markdown

#Writing an R Markdown Document 
  #Ensure that you are using the correct writing format. 

echo "# Computational-Workshop" >> README.md
git init
git add README.md
git commit -m "first commit"
git remote add origin https://github.com/meganejlott/Computational-Workshop.git
git push -u origin master
