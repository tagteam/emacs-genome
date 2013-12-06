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
	@echo "************** emacs-genome..."
	@git pull
	$(MAKE) -si gitone url=git://orgmode.org/org-mode.git dir=org-mode
	$(MAKE) -si gitone url=git://github.com/kkholst/SuperMan.git dir=SuperMan
	$(MAKE) -si gitone url=git://github.com/jwiegley/auctex.git dir=auctex
	$(MAKE) -si gitone url=git://github.com/emacs-ess/ESS.git dir=ess
        $(MAKE) -si gitone url=git://github.com/ieure/ssh-el.git dir=ssh-el
# $(MAKE) gitone url=git://jblevins.org/git/deft.git dir=deft
# $(MAKE) gitone url=git://github.com/auto-complete/auto-complete.git dir=auto-complete
# $(MAKE) -si gitone url=git://github.com/emacs-helm/helm.git dir=helm
# $(MAKE) -si gitone url=git://github.com/magit/magit.git dir=magit

compile:
	cd $(GENES)/org-mode;$(MAKE) -si
	cd $(GENES)/ess; ./configure; $(MAKE) -si
	cd $(GENES)/auctex; ./configure; $(MAKE) -si 

.PHONY: all default link root git 


