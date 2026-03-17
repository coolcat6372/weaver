SCHEME    = scheme
SCM_DIR   = weaver
SRCS      = $(wildcard $(SCM_DIR)/*.scm)
CHEZ_SRFI = $(shell nix-build '<nixpkgs>' -A chez-srfi --no-out-link 2>/dev/null)
SRFI_LIBS = $(CHEZ_SRFI)/lib/csv10.2-site

all: $(OUT)

run: $(SRCS)
	@$(SCHEME) --libdirs "$(SCM_DIR):$(SRFI_LIBS)" --program $(SCM_DIR)/main.scm

.PHONY: run