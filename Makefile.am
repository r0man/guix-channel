bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = \
  pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -L "$(top_srcdir)/modules" -o "$@" "$<"

NOCOMP_SOURCES = \
  modules/r0man/guix/files/gnu-head.svg

SOURCES = \
  modules/r0man/guix/packages/bootloader.scm \
  modules/r0man/guix/packages/claude.scm \
  modules/r0man/guix/packages/clojure.scm \
  modules/r0man/guix/packages/container.scm \
  modules/r0man/guix/packages/cpp.scm \
  modules/r0man/guix/packages/curl.scm \
  modules/r0man/guix/packages/display-managers.scm \
  modules/r0man/guix/packages/emacs.scm \
  modules/r0man/guix/packages/golang-charm.scm \
  modules/r0man/guix/packages/golang-dolthub.scm \
  modules/r0man/guix/packages/golang-maths.scm \
  modules/r0man/guix/packages/golang-web.scm \
  modules/r0man/guix/packages/golang-xyz.scm \
  modules/r0man/guix/packages/java.scm \
  modules/r0man/guix/packages/lisp.scm \
  modules/r0man/guix/packages/node.scm \
  modules/r0man/guix/packages/pi.scm \
  modules/r0man/guix/packages/task-management.scm \
  modules/r0man/guix/packages/wm.scm

# Regular, fast SRFI-64 tests — run by `make check`.
UNIT_TESTS = \
  tests/r0man/guix/packages/smoke.scm

# VM-based tests — run by `make check-system`.  Empty for this channel:
# guix-channel ships pure packages, no services or marionette tests.
SYSTEM_TEST_FILES =

TESTS = $(UNIT_TESTS)

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                                \
  $(top_builddir)/pre-inst-env                                  \
  $(GUILE) --no-auto-compile -e main                            \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not individual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST =					\
  bootstrap					\
  pre-inst-env.in				\
  build-aux/test-driver.scm			\
  m4/guile.m4					\
  .guix-channel					\
  .guix-authorizations				\
  .gitignore					\
  README.org					\
  $(SOURCES)					\
  $(NOCOMP_SOURCES)				\
  $(UNIT_TESTS)					\
  $(SYSTEM_TEST_FILES)

# No-op for this channel: no system tests yet.  The recipe is kept so
# the target exists and matches the convention used in other channels.
check-system:
	@if test -z "$(SYSTEM_TEST_FILES)"; then				\
	  echo "make check-system: no system tests defined; skipping.";		\
	else									\
	  $(MAKE) check TESTS="$(SYSTEM_TEST_FILES)";				\
	fi
.PHONY: check-system

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(UNIT_TESTS:tests/%.scm=%.log)		\
  $(UNIT_TESTS:tests/%.scm=%.trs)		\
  $(SYSTEM_TEST_FILES:tests/%.scm=%.log)	\
  $(SYSTEM_TEST_FILES:tests/%.scm=%.trs)
