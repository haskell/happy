# Mini-driver for Happy

# needs the following variables:
#	HAPPYLIB
#	HAPPYBIN

case $* in
*--template*) $HAPPYBIN $*;;
*)            $HAPPYBIN --template $HAPPYLIB $*;;
esac
