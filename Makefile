.PHONY: nightly 



nightly: install
	bash infra/nightly.sh
	bash infra/publish.sh

