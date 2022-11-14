.PHONY: nightly 



nightly:
	bash infra/nightly.sh perf
	bash infra/publish.sh

