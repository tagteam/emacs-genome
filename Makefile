# 
# This file makes the emacs genome
#

GENOME := .
SNPS := $(GENOME)/snps
GENES := $(GENOME)/genes

default: update 

all: update 

test: @echo "Hi"

update: 
	@echo "***** updating emacs genome..."
	@git pull
# @$(MAKE) -si submodules
# submodules:
	# @echo "***** updating submodules..."
	# @git submodule init
	# @git submodule update	
	# @cd $(GENES)/org-mode; $(MAKE) -si
	# @cd $(GENES)/ess; ./configure; $(MAKE) -si
	# @cd $(GENES)/auctex; ./configure; $(MAKE) -si 
	# @cd $(GENES)/helm; $(MAKE) -si 
# submaster: 
#	@git submodule foreach git checkout origin/master

.PHONY: all default update


