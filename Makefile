.PHONY: autotest build

# Default target - handle autotest framework requirements
compiler:
	cargo build --release
	@if [ -n "$(BUILD_DIR)" ]; then \
		mkdir -p $(BUILD_DIR); \
		cp target/release/sysy-compiler $(BUILD_DIR)/compiler; \
	fi

# Standard build target for autotest framework
build:
	cargo build --release
	@if [ -n "$(BUILD_DIR)" ]; then \
		mkdir -p $(BUILD_DIR); \
		cp target/release/sysy-compiler $(BUILD_DIR)/compiler; \
	fi

# Local testing with Docker
testkoopa:
	docker run -it --rm -v $(shell pwd):/root/compiler maxxing/compiler-dev \
		autotest -koopa -s lv5 /root/compiler

testallk:
	docker run -it --rm -v $(shell pwd):/root/compiler maxxing/compiler-dev \
		bash -c 'for i in 1 2 3 4 5; do autotest -koopa -s lv$$i /root/compiler; done'

testriscv:
	docker run -it --rm -v $(shell pwd):/root/compiler maxxing/compiler-dev \
		autotest -riscv -s lv5 /root/compiler

testallr:
	docker run -it --rm -v $(shell pwd):/root/compiler maxxing/compiler-dev \
		bash -c 'for i in 1 2 3 4 5; do autotest -riscv -s lv$$i /root/compiler; done'