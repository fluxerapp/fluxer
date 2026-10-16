# SPDX-License-Identifier: AGPL-3.0-or-later

GATEWAY_TOOL := cargo run --locked --quiet --manifest-path ../tools/ci/Cargo.toml -- gateway --step

.PHONY: all fmt fmt-check lint typecheck test check compile clean

all: check

fmt:
	@$(GATEWAY_TOOL) fmt

fmt-check:
	@$(GATEWAY_TOOL) fmt_check

lint:
	@$(GATEWAY_TOOL) lint

typecheck:
	@$(GATEWAY_TOOL) typecheck

test:
	@$(GATEWAY_TOOL) eunit

check:
	@$(GATEWAY_TOOL) all_checks

compile:
	@$(GATEWAY_TOOL) compile

clean:
	@$(GATEWAY_TOOL) clean
