.PHONY: nightly hook



nightly:
	bash infra/nightly.sh perf
	nightly-results publish report/

hook:
	echo "#!/bin/sh" >.git/hooks/pre-commit
	echo "raco fmt -i \$$(find . -name '*.rkt')" >>.git/hooks/pre-commit
