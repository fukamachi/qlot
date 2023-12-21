.PHONY: all
all: build

.PHONY: build
build:
	./scripts/setup.sh

.PHONY: install
install:
	./scripts/install.sh

.PHONY: uninstall
uninstall:
	./scripts/qlot-uninstaller.sh

.PHONY: clean
clean:
	rm -rf .qlot
