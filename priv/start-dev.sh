#! /bin/sh
exec erl +K true \
	-pa ebin deps/*/ebin \
	-config etc/app \
	-sname jvalidator \
	-s jvalidator_app start
