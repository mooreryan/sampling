.PHONY: clean
clean:
	dune clean

.PHONY: build
build:
	SAMPLING_GIT_COMMIT_HASH=`git describe --always --dirty --abbrev=7` dune build --profile=release

.PHONY: test
test:
	SAMPLING_GIT_COMMIT_HASH=`git describe --always --dirty --abbrev=7` dune runtest --profile=release
