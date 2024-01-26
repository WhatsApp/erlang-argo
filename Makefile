# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

PROJECT = argo
PROJECT_DESCRIPTION = argo
PROJECT_VERSION = 1.0.0

include erlang.mk

.PHONY: eqwalizer distclean-eqwalizer

# Configuration.
EQWALIZER_VERSION ?= 0.25.3

ELP ?= $(CURDIR)/elp
export ELP

ifeq ($(PLATFORM),darwin)
	EQWALIZER_URL ?= https://github.com/WhatsApp/eqwalizer/releases/download/v$(EQWALIZER_VERSION)/elp-macos.tar.gz
else
	EQWALIZER_URL ?= https://github.com/WhatsApp/eqwalizer/releases/download/v$(EQWALIZER_VERSION)/elp-linux.tar.gz
endif

EQWALIZER_OPTS ?=
EQWALIZER_BUILD_DIR ?= $(CURDIR)/_eqwalizer_build
EQWALIZER_ARCHIVE = $(EQWALIZER_VERSION).tar.gz

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"eqwalizer targets:" \
		"  eqwalizer    Run 'elp eqwalize-all' on the current project"

distclean:: distclean-eqwalizer

# Plugin-specific targets.

$(ELP):
	$(verbose) mkdir -p $(EQWALIZER_BUILD_DIR)
	$(verbose) echo "Downloading eqwalizer from: "$(EQWALIZER_URL)
	$(verbose) $(call core_http_get,$(EQWALIZER_BUILD_DIR)/$(EQWALIZER_ARCHIVE),$(EQWALIZER_URL))
	$(verbose) cd $(EQWALIZER_BUILD_DIR) && \
		tar -xzf $(EQWALIZER_ARCHIVE)
	$(gen_verbose) cp $(EQWALIZER_BUILD_DIR)/elp $(ELP)
	$(verbose) chmod +x $(ELP)
	$(verbose) rm -rf $(EQWALIZER_BUILD_DIR)

eqwalizer: $(ELP)
	$(verbose) $(ELP) eqwalize-all

distclean-eqwalizer:
	$(gen_verbose) rm -rf $(ELP)

.PHONY: erlfmt erlfmt-check distclean-erlfmt format

# Configuration.
ERLFMT_VERSION ?= 1.3.0

ERLFMT ?= $(CURDIR)/erlfmt
export ERLFMT

ERLFMT_URL ?= https://github.com/WhatsApp/erlfmt/archive/refs/tags/v$(ERLFMT_VERSION).tar.gz
ERLFMT_OPTS ?=
ERLFMT_BUILD_DIR ?= $(CURDIR)/_erlfmt_build
ERLFMT_CODE_ARCHIVE = $(ERLFMT_VERSION).tar.gz

ERLFMT_REBAR3_URL ?= https://s3.amazonaws.com/rebar3/rebar3
ERLFMT_REBAR3 ?= rebar3

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"erlfmt targets:" \
		"  erlfmt       Run erlfmt or download the default otherwise" \
		"  elrfmt-check Run erlfmt --check"

distclean:: distclean-erlfmt

# Plugin-specific targets.

$(ERLFMT):
	$(verbose) mkdir -p $(ERLFMT_BUILD_DIR)
ifeq ($(shell command -v $(ERLFMT_REBAR3)),)
	$(verbose) echo "Downloading Rebar3 from: "$(ERLFMT_REBAR3_URL)
	$(verbose) $(call core_http_get,$(ERLFMT_BUILD_DIR)/rebar3,$(ERLFMT_REBAR3_URL))
	$(verbose) chmod +x $(ERLFMT_BUILD_DIR)/rebar3
	$(eval ERLFMT_REBAR3 := $(ERLFMT_BUILD_DIR)/rebar3)
else
	$(verbose) echo "Using Rebar3: "$(ERLFMT_REBAR3)
endif
	$(verbose) echo "Downloading erlfmt from: "$(ERLFMT_URL)
	$(verbose) $(call core_http_get,$(ERLFMT_BUILD_DIR)/$(ERLFMT_CODE_ARCHIVE),$(ERLFMT_URL))
	$(verbose) cd $(ERLFMT_BUILD_DIR) && \
		tar -xzf $(ERLFMT_CODE_ARCHIVE) && \
		cd erlfmt-$(ERLFMT_VERSION) && \
		$(ERLFMT_REBAR3) as release escriptize
	$(gen_verbose) cp $(ERLFMT_BUILD_DIR)/erlfmt-$(ERLFMT_VERSION)/_build/release/bin/erlfmt $(ERLFMT)
	$(verbose) chmod +x $(ERLFMT)
	$(verbose) rm -rf $(ERLFMT_BUILD_DIR)/erlfmt-$(ERLFMT_VERSION)
	$(verbose) rm $(ERLFMT_BUILD_DIR)/$(ERLFMT_CODE_ARCHIVE)
	$(verbose) rm -f $(ERLFMT_BUILD_DIR)/rebar3
	$(verbose) rm -rf $(ERLFMT_BUILD_DIR)

erlfmt: $(ERLFMT)
	$(verbose) $(ERLFMT) --verbose --write --require-pragma --print-width=120 \
		'apps/**/{src,include,test}/**/*.{hrl,erl,app.src}' \
		'apps/**/{rebar.config,rebar.config.script}' \
		'{rebar.config,rebar.config.script}'

erlfmt-check: $(ERLFMT)
	$(verbose) $(ERLFMT) --check --require-pragma --print-width=120 \
		'apps/**/{src,include,test}/**/*.{hrl,erl,app.src,app.src.script}' \
		'apps/**/{rebar.config,rebar.config.script}' \
		'{rebar.config,rebar.config.script}'

distclean-erlfmt:
	$(gen_verbose) rm -rf $(ERLFMT)

format: $(ERLFMT)
	$(verbose) $(MAKE) erlfmt
