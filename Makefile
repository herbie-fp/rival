.PHONY: nightly 



nightly:
	bash infra/nightly.sh all
	bash infra/publish.sh

