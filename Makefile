TMP := /tmp/backup
_DUMMY_ := $(shell mkdir -p $(TMP))
ROOTFILES = .latexmkrc 

GENOME := $(HOME)/emacs-genome
SNPS := $(GENOME)/snps
GENES := $(GENOME)/genes
DEFAULTPATH := $(HOME)
TARGETS := $(foreach file,$(ROOTFILES),$(patsubst %,$(DEFAULTPATH)/%,$(file)))

default: root git compile

all: root git 

ls: 
	@ls -ldaG $(TARGETS)


root:
	@$(foreach file,$(ROOTFILES), $(MAKE) link file=$(file) target=$(HOME) name=$(file);)

link: 
	@echo "$(file) -> $(target)/$(file)"
	@if [ -f "$(target)/$(name)" ] ; \
	then \
		cp "$(target)/$(name)" $(TMP) ; rm -rf $(target)/$(name) ; \
	fi
	cd $(target); ln -s $(GENOME)/$(file) $(name) ; \

topath := $(GENES)
gitone:
	@echo "************** $(topath)/$(dir)"
	@if [ ! -d $(topath)/$(dir) ] ; \
	then \
		cd $(topath); git clone $(url) $(dir) ; \
	else \
		cd $(topath)/$(dir); git pull ; \
	fi

git:
	@echo "************** Emacs-Genome..."
	@git pull
	$(MAKE) gitone url=git://github.com/jwiegley/auctex.git dir=auctex
	$(MAKE) gitone url=git://github.com/emacs-ess/ESS.git dir=ess
	$(MAKE) gitone url=git://orgmode.org/org-mode.git dir=org-mode
# $(MAKE) gitone url=git://github.com/ieure/ssh-el.git dir=ssh-el
	$(MAKE) gitone url=git://github.com/kkholst/SuperMan.git dir=SuperMan
	$(MAKE) gitone url=git://jblevins.org/git/deft.git dir=deft
# $(MAKE) gitone url=git://github.com/auto-complete/auto-complete.git dir=auto-complete
	$(MAKE) gitone url=git://github.com/joodland/bm.git dir=bm
# $(MAKE) gitone url=git://github.com/sellout/emacs-color-theme-solarized.git dir=emacs-color-theme-solarized
	$(MAKE) gitone url=git://github.com/roman/golden-ratio.el.git dir=golden-ratio
	$(MAKE) gitone url=git://github.com/emacs-helm/helm.git dir=helm
	$(MAKE) gitone url=git://github.com/dandavison/minimal.git dir=minimal
	$(MAKE) gitone url=git://randomsample.de/minimap.git dir=minimap
	$(MAKE) gitone url=git://github.com/djcb/mu.git dir=mu
	$(MAKE) gitone url=git://github.com/auto-complete/popup-el dir=popup
	$(MAKE) gitone url=git://github.com/escherdragon/sunrise-commander.git dir=sunrise-commander
	$(MAKE) gitone url=git://github.com/magit/magit.git dir=magit

compile:
	cd $(GENES)/org-mode; make
	cd $(GENES)/ess; make
	cd $(GENES)/auctex; make
	cd $(GENES)/mu; make

.PHONY: all default link root git 


