example:
	rm -rf "exampleGraphs"
	mkdir "exampleGraphs"
	stack run example
	for filename in exampleGraphs/*.dot ; \
	do \
		name=$${filename%.dot} ; \
		dot $$filename -Tpng > $$name.png ; \
	done
