wdjc
====

The WHET DJ Compiler 


TODO
----
Semcheck
- sc_expr section
	- change NOTE_CR
	- modifier types, confirm behavior
	- check_vinit_type function probably wrong

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

3. Discuss vartype, vType, etc. different conventions for getting variable names + variable types;
	-what standard do we want to use?
	-are we using them correctly currently?
4. Streamline function names in semcheck