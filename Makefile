.PHONY: publish-all
publish-all:
	vc verify -r. --tagged --clean
	cd core && cargo publish
	cd api && cargo publish
	cd schema && cargo publish
	cd expr && cargo publish --all-features
#	cd cmd && cargo publish
	git push && git push --tags
