#/bin/sh

declare -x PROGRAM="bin/Ensanche-Core"

if [[ -f $PROGRAM ]]
then
	$PROGRAM
else
	echo "Program not found. Please build the program"
fi
