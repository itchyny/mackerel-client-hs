.PHONY: test
test:
	stack test

.PHONY: clean
clean:
	stack clean

VERSION := $$(sed -n '/^version:/s/^.*  *\([0-9][0-9.]*\).*$$/\1/p' mackerel-client.cabal)
GIT_DIFF := $$(git diff --name-only)

.PHONY: bump
bump:
ifneq ($(shell git status --porcelain),)
	$(error git workspace is dirty)
endif
ifneq ($(shell git rev-parse --abbrev-ref HEAD),master)
	$(error current branch is not master)
endif
	@printf "Bump up version in mackerel-client.cabal. Press Enter to proceed: "
	@read -n1
	@[ "$(GIT_DIFF)" == "mackerel-client.cabal" ] || { \
		echo "Version is not updated or unrelated file is updated:"; \
		[ -z "$(GIT_DIFF)" ] || printf "  %s\n" $(GIT_DIFF); \
		exit 1; \
	}
	git commit -am "bump up version to $(VERSION)"
	git tag "v$(VERSION)"
	git push origin master
	git push origin "refs/tags/v$(VERSION)"

.PHONY: sdist
sdist:
	stack sdist --tar-dir .
