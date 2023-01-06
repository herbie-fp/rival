.PHONY: nightly 



nightly:
	bash infra/nightly.sh perf
	nightly-results publish report/

