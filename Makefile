clean:
	rm -f *.html
	rm -f *.xml

republish: clean
	./publish.clj posts
