wdjc
====

The WHET DJ Compiler 


TODO
----
- Decide whether we are implementing loop.
- Decide on usage and representation of arrays.
- Review 'Modifiers' section.

ANSWERED
--------
1. What are actuals

	they're function call parameters/arguments 
	
2. Why does this work:

		int i;
		int j;
		
		i = 0;
		j = 1;
	
	While this doesn't work:

		int i;
		i = 0;

		int j;
		j = 1;
		
	**Because** in fdecl `vdecl list` comes before `stmt list`