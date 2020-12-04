.PHONY: nightly 



nightly:
	bash infra/nightly.sh
	bash infra/publish.sh

