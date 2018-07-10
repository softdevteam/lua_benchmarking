.SUFFIXES:

ifndef name
$(error build name is not set)
endif

BUILD_DIR:= builds/$(name)
BUILD_FULLDIR:= $(shell cd builds && pwd)/$(name)
BUILD_DIRS= $(BUILD_DIR) $(BUILD_DIR)/include $(BUILD_DIR)/jit $(BUILD_DIR)/jitlog
REPO:= $(BUILD_FULLDIR)/src
SRC:= $(BUILD_DIR)/src/src

# We need a branch name when first checking out the repo
ifndef branch
  $(if $(shell if [ -f $(REPO)/.git ]; then echo ok; fi), , $(error No 'branch' variable set))
endif

# Make sure all output build directories are created before we run anything
$(shell mkdir -p $(BUILD_DIRS))

SRC_C:= $(wildcard $(SRC)/*.c)
SRC_H:= $(wildcard $(SRC)/*.h)
SRC_JITLOG:= $(wildcard $(SRC)/jitlog/*.lua)

# Exclude generated headers from rebuild checking since they break pull_build/reset_build
GENHDR= lj_bcdef.h lj_ffdef.h lj_libdef.h lj_recdef.h lj_folddef.h  \
        lj_jitlog_def.h lj_jitlog_writers.h host/buildvm_arch.h
GENHDR:= $(addprefix $(SRC)/,$(GENHDR))     
SRC_H:= $(filter-out $(GENHDR),$(SRC_H))

FILES_INC:= luaconf.h lua.h luajit.h lualib.h lauxlib.h
SRC_INC:= $(addprefix $(SRC)/,$(FILES_INC))
INCLUDES:= $(addprefix $(BUILD_DIR)/include/,$(FILES_INC))

GITCMD:= git --git-dir="$(REPO)/.git" --work-tree="$(REPO)"
GITCMD_MAINREPO:= git --git-dir="luajit_repo/.git" --work-tree="luajit_repo"

build: $(BUILD_DIR)/libluajit.so $(BUILD_DIR)/libluajit-5.1.so.2 $(INCLUDES)

$(REPO)/.git:
	mkdir -p $(REPO)
	cd luajit_repo && git worktree add --detach $(REPO)
	$(GITCMD) checkout $(branch)
	@$(GITCMD) rev-parse --abbrev-ref HEAD > $(BUILD_DIR)/gitbranch.txt

reset: $(REPO)/.git clean
	$(GITCMD) reset --hard

pull: $(REPO)/.git
	$(GITCMD) pull

$(BUILD_DIR)/libluajit.so: $(REPO)/.git $(SRC_C) $(SRC_H) $(SRC_JITLOG)
# Remove the build audit text files since this is a new build and it might fail
	@rm -f $(BUILD_DIR)/gitstatus.txt
	@rm -f $(BUILD_DIR)/buildflags.txt
	@rm -f $(BUILD_DIR)/build.txt
	@rm -f $(BUILD_DIR)/libluajit.so
	@rm -f $(BUILD_DIR)/luajit
	@rm -f $(SRC)/libluajit.so
	@rm -f $(SRC)/luajit
	$(GITCMD) status
	@echo "===== Building LuaJIT ====="
	@$(GITCMD) rev-parse HEAD > $(BUILD_DIR)/githash.txt
	$(MAKE) -C $(SRC) -j CCDEBUG=-g XCFLAGS="$(XCFLAGS)" 2>&1 | tee $(BUILD_DIR)/build.txt
	cp $(SRC)/luajit $(BUILD_DIR)/
	cp $(SRC)/libluajit.so $(BUILD_DIR)/
	cp $(SRC)/jit/vmdef.lua $(BUILD_DIR)/jit/
# Ignore any errors trying to copy the jitlog lib if this branch doesn't have it 
	cp $(SRC)/jitlog/*.lua $(BUILD_DIR)/jitlog/ 2>/dev/null || :
	@$(GITCMD) status > $(BUILD_DIR)/gitstatus.txt
	@echo $(XCFLAGS) > $(BUILD_DIR)/buildflags.txt
	@echo "==== Successfully built LuaJIT ===="

# Make a symbolic link for the name nginx expects
$(BUILD_DIR)/libluajit-5.1.so.2: $(BUILD_DIR)/libluajit.so
	ln -fs $(BUILD_FULLDIR)/libluajit.so $(BUILD_DIR)/libluajit-5.1.so.2

# Copy public headers
$(BUILD_DIR)/include/%.h: $(REPO)/.git
	@cp $(SRC)/$*.h $(BUILD_DIR)/include/$*.h

clean: $(REPO)/.git
	rm -f $(BUILD_DIR)/luajit
	rm -f $(BUILD_DIR)/libluajit.so
	rm -f $(BUILD_DIR)/githash.txt
	rm -f $(BUILD_DIR)/gitstatus.txt
	rm -f $(BUILD_DIR)/buildflags.txt
	rm -f $(BUILD_DIR)/build.txt
	rm -f $(BUILD_DIR)/jit/*.lua
	rm -f $(BUILD_DIR)/jitlog/*.lua
	rm -f $(BUILD_DIR)/include/*.h
	$(MAKE) -C $(SRC) clean
