# the main class name
MAIN=$1

if [[ $1 =~ 'clean' ]]; then
	echo "Cleaning up"
	echo "rm -rf tests/*.java tests/*.class tests/*.mid"
	rm -rf tests/*.java tests/*.class tests/*.mid
else
	# jMusic Jars
	JM_JAR="java/jMusic/jMusic1.6.4.jar"
	JM_INSTR="java/jMusic/inst/"
	
	# Set the CLASSPATH
	CLASSPATH="tests:$JM_JAR:$JM_INSTR:."

	# Java tools
	JFLAGS="-classpath $CLASSPATH"
	JCFLAGS="-sourcepath tests -d tests -classpath $CLASSPATH"

	echo "Compiling"
	echo "javac $JCFLAGS tests/$MAIN.java"
	javac $JCFLAGS tests/$MAIN.java

	echo "Running"
	echo "java $JFLAGS tests/$MAIN.java"
	java $JFLAGS tests/$MAIN.java
fi



