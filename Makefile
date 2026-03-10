SCHEME=scheme
SCM_DIR=weaver
SRCS=$(wildcard $(SCM_DIR)/*.scm)

all: $(OUT)

run: $(SRCS)
	@$(SCHEME) --libdirs $(SCM_DIR) --program $(SCM_DIR)/main.scm

.PHONY: run