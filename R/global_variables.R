# Alternative to setting global variables to NULL first, appeasing R CMD check and CRAN
utils::globalVariables(c('plot_data','shaped_data','Use','Species',
'Expert','informant','sp_name','variable','value','strwidth'))
#Overcome issues with ggalluvial
#the ggplot2 stat argument pastes the string you give and then looks for that object (in this case "StatStratum") in the environment you're in. 
#Because you don't want to load the package, it won't be able to find it (and there's no way to change the argument itself).
#So you need to save that object from the ggalluvial package like so:
#StatStratum <- ggalluvial::StatStratum)

