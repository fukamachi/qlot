.PHONY: all
all: build

.PHONY: build
build: .qlot

.PHONY: bundle
bundle: build .bundle-libs

.PHONY: install
install: build
	./scripts/install.sh

.PHONY: uninstall
uninstall:
	./scripts/qlot-uninstaller.sh

.qlot:
	./scripts/setup.sh

.bundle-libs:
	./scripts/run.sh bundle --exclude qlot-tests --exclude "qlot-tests/*"

.PHONY: clean
clean:
	rm -rf .qlot .bundle-libs
