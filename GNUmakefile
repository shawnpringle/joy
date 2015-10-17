.PHONY : all


all : svn_e.html joy_e.html


svn_e.html : svn.creole
	creole -f html svn.creole
	
joy_e.html : joy.creole
	creole -f html joy.creole
	
svn.creole : svn.e
	eudoc svn.e -o svn.creole

joy.creole : joy.e
	eudoc joy.e -o joy.creole
		

