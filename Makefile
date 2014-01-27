TMP := /tmp/backup
_DUMMY_ := $(shell mkdir -p $(TMP))
ROOTFILES = .latexmkrc 
GENOME := $(HOME)/emacs-genome
SNPS := $(GENOME)/snps
GENES := $(GENOME)/genes
DEFAULTPATH := $(HOME)
TARGETS := $(foreach file,$(ROOTFILES),$(patsubst %,$(DEFAULTPATH)/%,$(file)))

default: config superman

all: config root master pull

master: 
	@git submodule foreach git checkout origin/master

pull: 
	@git submodule foreach git pull

init: config root	
	@$(MAKE) -si initsubmodules
	@$(MAKE) initcompile
	@$(MAKE) compile
	@echo "**** All done"

initsubmodules:
	@git submodule init
	@git submodule update	

initcompile:
	@mkdir -p $(TMP)/texmf
	@cd $(GENES)/auctex; autoconf && ./configure  --with-texmf-dir=$(HOME)/tmp/texmf && $(MAKE)

compile:
	@echo "***** compiling genes..."
	@cd $(GENES)/org-mode; $(MAKE) -si
	@cd $(GENES)/ess; ./configure; $(MAKE) -si
	@cd $(GENES)/auctex; ./configure; $(MAKE) -si 

config: 
	@echo "***** updating emacs genome..."
	@git pull

superman: 
	@echo "***** updating SuperMan..."
	@cd $(GENES)/SuperMan; git checkout master; git pull

root:
	@$(foreach file,$(ROOTFILES), $(MAKE) link file=$(file) target=$(HOME) name=$(file);)

link: 
	@echo "$(file) -> $(target)/$(file)"
	@if [ -a $(target)/$(name) ] ; \
	then \
		cp "$(target)/$(name)" $(TMP) ; rm -rf $(target)/$(name) ; \
	fi
	@ln -s $(GENOME)/$(file) $(target)/$(name) ; \


ls: 
	@ls -ldaG $(TARGETS)


.PHONY: all default link root git init config

