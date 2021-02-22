all: qlot

impl = $(shell (which ros >/dev/null && echo 'ros') || (which sbcl >/dev/null && echo 'sbcl'))
qlot:
	$(MAKE) "qlot_$(impl)"

.PHONY: qlot_ros
qlot_ros:
	ros -S . -e '(asdf:make :qlot)'

.PHONY: qlot_sbcl
qlot_sbcl:
	CL_SOURCE_REGISTRY=$(shell pwd) sbcl --eval '(asdf:make :qlot)'

.PHONY: qlot_ccl
qlot_ccl:
	CL_SOURCE_REGISTRY=$(shell pwd) ccl --eval '(asdf:make :qlot)'

.PHONY: clean
clean:
	rm qlot
