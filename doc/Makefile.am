cdoxdeps = Doxyfile

if DOXYDOC

all: html/index.html

html/index.html: $(cdoxdeps)
	doxygen  $<
install-data-local:
	for pkg in html ; do \
	  test -d $$pkg && \
	  $(mkinstalldirs) $(DESTDIR)$(docdir)/$$pkg && \
	  find $$pkg -type f -exec $(INSTALL_DATA) '{}' $(DESTDIR)$(docdir)/$$pkg/ \; ; \
	done || true

uninstall-local:
	for pkg in html ; do \
	  rm -rf $(DESTDIR)$(docdir)/$${pkg}; \
	done || true

clean-local:
	rm -rf html

endif

EXTRA_DIST = Doxyfile.in

