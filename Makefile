clean:
	rm -f *.html
	rm -f *.xml

republish: clean
	emacs --batch -l ~/.emacs.d/init.el --eval="(org-static-blog-publish)"
