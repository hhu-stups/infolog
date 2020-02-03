ifndef PROJECTPATH
  ifndef PROBPATH
    $(error PROJECTPATH and PROBPATH are undefined)
  endif
endif

ifdef PROJECTPATH
  ABSOLUTE_PROJECT_PATH=$(realpath $(PROJECTPATH))
  PREPARE_ENV=true
  PLDEPS=
  ifndef MAINFILE
    $(error MAINFILE is undefined)
  endif
  TARGETS=['$(PROJECTPATH)/$(MAINFILE)']
endif

# where do we store ProB specific files:
PROBINFODIR=prob-files

ifdef PROBPATH
    ABSOLUTE_PROB_PATH=$(realpath $(PROBPATH))
    PREPARE_ENV=export PROB_HOME=$(ABSOLUTE_PROB_PATH)
    ABSOLUTE_PROJECT_PATH=$(ABSOLUTE_PROB_PATH)/src
    PROJECTPATH=$(PROBPATH)
    MAINFILE=prob_tcltk.pl
    PLDEPS=$(PROBINFODIR)/tcltk_calls.pl $(PROBINFODIR)/java_calls.pl
    TARGETS=['$(PROBPATH)/src/prob_tcltk.pl','$(PROBPATH)/src/prob_cli.pl']
endif


.PHONY: prob_test prob_test_verbose prob prob_cli prob_tk server updatedb all update
PROLOG_FLAGS=

all: updatedb

updatedb:
ifdef PROBPATH
	make prob
else
	make update
endif

infolog.pdf: infolog.dot Makefile
	#sfdp -Tpdf -x -Goverlap=scale <infolog.dot >infolog.pdf
	#sfdp -Tpdf -x -Goverlap=prism <infolog.dot >infolog.pdf
	#fdp <infolog.dot >infolog.pdf
	twopi <infolog.dot >infolog.pdf
	#circo <infolog.dot >infolog.pdf
	open infolog.pdf

prob_test:
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl','$(PROBINFODIR)/meta_user_pred_cache.pl','$(PROBINFODIR)/tcltk_calls.pl')."

prob_test_verbose:
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('$(PROBPATH)/src/tools.pl'),export(user_output)."

prob: $(PLDEPS)
	@echo "analyzing ProB Tcl/Tk and probcli together"
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze($(TARGETS),'$(PROBINFODIR)/meta_user_pred_cache.pl','$(PROBINFODIR)/tcltk_calls.pl'), update_problem_db('$(PROBPATH)/src/infolog_problem_db.pl')."

update:
	@echo "analyzing $(PROJECTPATH) starting from $(MAINFILE)"
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze($(TARGETS),'$(PROBINFODIR)/meta_user_pred_cache.pl','$(PROBINFODIR)/tcltk_calls.pl')."

infolog_problems.csv:  $(PROBINFODIR)/meta_user_pred_cache.pl $(PLDEPS) $(PROBINFODIR)/documentation.pl $(PROBINFODIR)/tcltk_calls.pl
	@echo "Generating CSV File"
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze($(TARGETS),'$(PROBINFODIR)/meta_user_pred_cache.pl','$(PROBINFODIR)/tcltk_calls.pl'), lint_to_csv_file('infolog_problems.csv'), halt."

infolog.edn:  $(PROBINFODIR)/meta_user_pred_cache.pl $(PLDEPS) prolog-analyzer/meta_preds.pl $(PROBINFODIR)/documentation.pl $(PROBINFODIR)/tcltk_calls.pl
	@echo "Generating Data for website"
	$(PREPARE_ENV); sicstus -l prolog-analyzer/analyzer.pl --goal "analyze($(TARGETS),'$(PROBINFODIR)/meta_user_pred_cache.pl','$(PROBINFODIR)/tcltk_calls.pl'),  export_to_clj_file('resources/public/infolog.edn'), halt."

indy.edn:
	@echo "Generating indentation analysis"
	rm -f resources/public/indy.edn
	touch resources/public/indy.edn
	@echo "{:complexity [" >> resources/public/indy.edn
	time find $(ABSOLUTE_PROJECT_PATH) -name *.pl -exec java -jar analyzers/indy.jar {} + >> resources/public/indy.edn
	@echo "]}" >> resources/public/indy.edn

clean:
	rm -f $(PROBINFODIR)/meta_user_pred_cache.pl
	rm -f $(PROBINFODIR)/documentation.pl
	echo ':- dynamic meta_user_pred/3.' > $(PROBINFODIR)/meta_user_pred_cache.pl
	rm -f infolog_problems*.csv
	rm -f resources/public/infolog.edn
	rm -f resources/public/indy.edn

ui:
	@echo "Compiling UI"
	chmod +x lein
	./lein clean
	./lein cljsbuild once min

run_server:
	@echo "Starting Python Simpleserver"
	pushd resources/public; python2 -m SimpleHTTPServer; popd

server: ui infolog.edn indy.edn docs run_server

$(PROBINFODIR)/java_calls.pl: Makefile
	@echo "Extracting Prolog calls from ProB 2.0 Java API using PROB2_PATH (which must be set)"
	@echo ":- dynamic java_call/2." > $(PROBINFODIR)/java_calls.pl
	find $(PROB2_PATH) -type f \( -iname \*.java -o -iname \*.groovy \) -exec perl -ne'print "java_call($$1,\"'{}'\").\n" if /.*?PROLOG_COMMAND_NAME\s*=\s*\"(.*)\"/' {} \; >> $(PROBINFODIR)/java_calls.pl

clean_tcltk:
	rm $(PROBINFODIR)/tcltk_calls.ack

$(PROBINFODIR)/tcltk_calls.ack: $(ABSOLUTE_PROB_PATH)/tcl/*.tcl Makefile
	ack -o '(?<=prolog)\s+(interruptable_call\()?("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl < /dev/null > $(PROBINFODIR)/tcltk_calls.ack
	ack -o '(?<=prologmnf)\s+("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl < /dev/null >> $(PROBINFODIR)/tcltk_calls.ack
	ack -o '(?<=prologmnfi)\s+("?(\{|\()?)([[a-zA-Z0-9_:\s]*)' $(ABSOLUTE_PROB_PATH)/tcl/*.tcl < /dev/null >> $(PROBINFODIR)/tcltk_calls.ack

$(PROBINFODIR)/tcltk_calls.pl: $(PROBINFODIR)/tcltk_calls.ack prolog-analyzer/tcltk_call_importer.pl
	@echo "Importing Calls from ProB Tcl/Tk interface"
	sicstus -l prolog-analyzer/tcltk_call_importer.pl --goal "process_file('$(PROBINFODIR)/tcltk_calls.ack'),generate_prolog_file('$(PROBINFODIR)/tcltk_calls.pl'),halt."

cp_tcltk_calls: $(PROBINFODIR)/tcltk_calls.pl
	@echo "Copying tcltk_calls.pl to ProB src/tcltk directory (for BBEdit search)"
	cp $(PROBINFODIR)/tcltk_calls.pl $(ABSOLUTE_PROB_PATH)/src/tcltk/

prolog-analyzer/meta_preds.pl: prolog-analyzer/meta_pred_generator.pl
	sicstus -l prolog-analyzer/meta_pred_generator.pl --goal "tell('prolog-analyzer/meta_preds.pl'),gen,told,halt."

analyzers/doc.jar: analyzers/doc/de/hhu/infolog/doc/*.java
	javac analyzers/doc/Main.java analyzers/doc/de/hhu/infolog/doc/*.java
	cd analyzers/doc; jar cfm ../doc.jar Manifest.txt *.class de/hhu/infolog/doc/*.class

$(PROBINFODIR)/documentation.pl: analyzers/doc.jar
	java -jar analyzers/doc.jar $(ABSOLUTE_PROJECT_PATH) --no-docs --export-prolog $(PROBINFODIR)/documentation.pl

docs: infolog_problems.csv analyzers/doc.jar
	mkdir -p resources/public/docs
	java -jar analyzers/doc.jar $(ABSOLUTE_PROJECT_PATH) --html --css analyzers/doc/purple.css --out resources/public/docs --problems-csv infolog_problems.csv

latex-out/infolog.tex: infolog_problems.csv analyzers/doc.jar
	mkdir -p latex-out
	rm -r latex-out/*
	java -jar analyzers/doc.jar $(ABSOLUTE_PROJECT_PATH) --latex --no-preamble --out latex-out --problems-csv infolog_problems.csv
	@echo >latex-out/infolog.tex "\input{../analyzers/doc/preamble.tex}"
	cd latex-out; find . -name \*.pl.tex -exec echo "\input{{}}" >>infolog.tex \;
	@echo >>latex-out/infolog.tex "\end{document}"

infologdoc.pdf: latex-out/infolog.tex
	cd latex-out; pdflatex infolog.tex && pdflatex infolog.tex
	cp latex-out/infolog.pdf infologdoc.pdf


simple1: examples/simple1/main.pl examples/simple1/tools.pl Makefile
	echo "Running Infolog on a simple example and generating : simple1/infolog_problems.csv"
	sicstus -l prolog-analyzer/analyzer.pl --goal "analyze('examples/simple1/main.pl'),lint_to_csv_file('examples/simple1/infolog_problems.csv'), halt."
