

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
autotest:
	docker run -it --rm -v $(shell pwd):/root/compiler maxxing/compiler-dev \
		autotest -koopa -s lv1 /root/compiler
