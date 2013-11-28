wdjc
====

The WHET DJ Compiler 


TODO
----
- Decide whether we are implementing loop.
- Decide on usage and representation of arrays.
- Review 'Modifiers' section.
- Need to review our decion on types in the ast. We have made our types part of the 'expr' section. Not sure yet on what emily will be pushing in terms of var decls, but I cannot build out the symbol table or the type checking mechanisms before we clean types in the ast. -Tom

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

?? mutable global/local in env