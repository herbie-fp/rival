.PHONY: nightly hook



nightly:
	bash infra/nightly.sh perf
	nightly-results publish report/

fmt:
	@raco fmt -i $(shell find infra/ ops/ eval/ ./ -name '*.rkt')
