= Bash-5.0 release available

source: http://lists.gnu.org/archive/html/bug-bash/2019-01/msg00063.html

From: Chet Ramey
Subject:  Bash-5.0 release available
Date: Mon, 7 Jan 2019 17:03:32 -0500


Introduction
============

The first public release of bash-5.0 is now available with the URLs

ftp://ftp.cwru.edu/pub/bash/bash-5.0.tar.gz
ftp://ftp.gnu.org/pub/gnu/bash/bash-5.0.tar.gz

and from the master branch of the bash git repository
(http://git.savannah.gnu.org/cgit/bash.git/log/)
and the usual GNU mirror sites.

Bash is the GNU Project's Bourne Again SHell, a complete
implementation of the POSIX shell spec, but also with interactive
command line editing, job control on architectures that support it,
csh-like features such as history substitution and brace expansion,
and a slew of other features.  For more information on the features
of Bash that are new to this type of shell, see the file
`doc/bashref.texi'.  There is also a large Unix-style man page.  The
man page is the definitive description of the shell's features. 

This tar file includes the formatted documentation (pdf, postscript,
dvi, info, and html, plus nroffed versions of the manual pages). 

Please use `bashbug' to report bugs with this version.  It is built
and installed at the same time as bash.

Installation
============

Please read the README file first.

Installation instructions are provided in the INSTALL file.

New Features
============

This is the fifth major release of bash.

Read the file NEWS in the bash-5.0 distribution for a complete description
of the new features.  A copy of the relevant portions is included below. 

This release fixes several outstanding bugs in bash-4.4 and introduces several
new features.  The most significant bug fixes are an overhaul of how
nameref variables resolve and a number of potential out-of-bounds memory
errors discovered via fuzzing. There are a number of changes to the
expansion of $@ and $\* in various contexts where word splitting is not
performed to conform to a Posix standard interpretation, and additional
changes to resolve corner cases for Posix conformance.

The most notable new features are several new shell variables: BASH_ARGV0,
EPOCHSECONDS, and EPOCHREALTIME. The `history' builtin can remove ranges of
history entries and understands negative arguments as offsets from the end
of the history list. There is an option to allow local variables to inherit
the value of a variable with the same name at a preceding scope. There is
a new shell option that, when enabled, causes the shell to attempt to
expand associative array subscripts only once (this is an issue when they
are used in arithmetic expressions).  The `globasciiranges' shell option
is now enabled by default; it can be set to off by default at configuration
time.

There are a few incompatible changes between bash-4.4 and bash-5.0. The
changes to how nameref variables are resolved means that some uses of
namerefs will behave differently, though I have tried to minimize the
compatibility issues. By default, the shell only sets BASH_ARGC and
BASH_ARGV at startup if extended debugging mode is enabled; it was an
oversight that it was set unconditionally and caused performance issues
when scripts were passed large numbers of arguments.

Bash can be linked against an already-installed Readline library rather
than the private version in lib/readline if desired.  Only readline-8.0 and
later versions are able to provide all of the symbols that bash-5.0 requires;
earlier versions of the Readline library will not work correctly. 

A complete list of changes between bash-4.4 and bash-5.0 is available in
the file CHANGES; the complete list is too large to include in this
message. 

Readline
========

Also available is a new release of the standalone Readline library,
version 8.0, with its own configuration scripts and Makefiles. 
It can be retrieved with the URLs

ftp://ftp.cwru.edu/pub/bash/readline-8.0.tar.gz
ftp://ftp.gnu.org/pub/gnu/readline/readline-8.0.tar.gz

and from the master branch of the GNU readline git repository
(http://git.savannah.gnu.org/cgit/readline.git/log/)
and the usual GNU mirror sites.

The formatted Readline documentation is included in the readline
distribution tar file.

A separate announcement listing the changes in Readline is being
distributed.

As always, thanks for your help.

Chet

+========== NEWS ==========+
This is a terse description of the new features added to bash-5.0 since
the release of bash-4.4.  As always, the manual page (doc/bash.1) is
the place to look for complete descriptions.

1.  New Features in Bash

a. The `wait' builtin can now wait for the last process substitution created.

b. There is an EPOCHSECONDS variable, which expands to the time in seconds
   since the Unix epoch.

c. There is an EPOCHREALTIME variable, which expands to the time in seconds
   since the Unix epoch with microsecond granularity.

d. New loadable builtins: rm, stat, fdflags.

e. BASH_ARGV0: a new variable that expands to $0 and sets $0 on assignment.

f. When supplied a numeric argument, the shell-expand-line bindable readline
   command does not perform quote removal and suppresses command and process
   substitution.

g. `history -d' understands negative arguments: negative arguments offset from
   the end of the history list.

h. The `name' argument to the `coproc' reserved word now undergoes word
   expansion, so unique coprocs can be created in loops.

i. A nameref name resolution loop in a function now resolves to a variable by
   that name in the global scope.

j. The `wait' builtin now has a `-f' option, which signfies to wait until the
   specified job or process terminates, instead of waiting until it changes
   state.

k. There is a define in config-top.h that allows the shell to use a static
   value for $PATH, overriding whatever is in the environment at startup, for
   use by the restricted shell.

l. Process substitution does not inherit the `v' option, like command
   substitution.

m. If a non-interactive shell with job control enabled detects that a foreground
   job died due to SIGINT, it acts as if it received the SIGINT.

n. The SIGCHLD trap is run once for each exiting child process even if job
   control is not enabled when the shell is in Posix mode.

o. A new shopt option: localvar_inherit; if set, a local variable inherits the
   value of a variable with the same name at the nearest preceding scope.

p. `bind -r' now checks whether a key sequence is bound before binding it to
   NULL, to avoid creating keymaps for a multi-key sequence.

q. A numeric argument to the line editing `operate-and-get-next' command
   specifies which history entry to use.

r. The positional parameters are now assigned before running the shell startup
   files, so startup files can use address@hidden

s. There is a compile-time option that forces the shell to disable the check
   for an inherited OLDPWD being a directory.

t. The `history' builtin can now delete ranges of history entries using
   `-d start-end'.

u. The `vi-edit-and-execute-command' bindable readline command now puts readline
   back in vi insertion mode after executing commands from the edited file.

v. The command completion code now matches aliases and shell function names
   case-insensitively if the readline completion-ignore-case variable is set.

w. There is a new `assoc_expand_once' shell option that attempts to expand
   associative array subscripts only once.

x. The shell only sets up BASH_ARGV and BASH_ARGC at startup if extended
   debugging mode is active. The old behavior of unconditionally setting them
   is available as part of the shell compatibility options.

y. The `umask' builtin now allows modes and masks greater than octal 777.

z. The `times' builtin now honors the current locale when printing a decimal
   point.

aa. There is a new (disabled by default, undocumented) shell option to enable
    and disable sending history to syslog at runtime.

bb. Bash no longer allows variable assignments preceding a special builtin that
    changes variable attributes to propagate back to the calling environment
    unless the compatibility level is 44 or lower.

cc. You can set the default value for $HISTSIZE at build time in config-top.h.

dd. The `complete' builtin now accepts a -I option that applies the completion
    to the initial word on the line.

ee.  The internal bash malloc now uses mmap (if available) to satisfy requests
    greater than 128K bytes, so free can use mfree to return the pages to the
    kernel.

ff. The shell doesn't automatically set BASH_ARGC and BASH_ARGV at startup
    unless it's in debugging mode, as the documentation has always said, but
    will dynamically create them if a script references them at the top level
    without having enabled debugging mode.

gg. The localvar_inherit option will not attempt to inherit a value from a
    variable of an incompatible type (indexed vs. associative arrays, for
    example).

hh. The `globasciiranges' option is now enabled by default; it can be set to
    off by default at configuration time.

ii. Associative and indexed arrays now allow subscripts consisting solely of
    whitespace.

jj.  `checkwinsize' is now enabled by default.

hh. The `globasciiranges' option is now enabled by default; it can be set to
    off by default at configuration time.

ii. Associative and indexed arrays now allow subscripts consisting solely of
    whitespace.
winsize' is now enabled by default.

kk. The `localvar_unset' shopt option is now visible and documented.

ll. The `progcomp_alias' shopt option is now visible and documented.

mm. The signal name processing code now understands `SIGRTMIN+n' all the way
    up to SIGRTMAX.

nn. There is a new `seq' loadable builtin.

oo. Trap execution now honors the (internal) max invocations of `eval', since
    traps are supposed to be executed as if using `eval'.

pp. The $_ variable doesn't change when the shell executes a command that forks.

qq. The `kill' builtin now supports -sSIGNAME and -nSIGNUM, even though
    conforming applications aren't supposed to use them.

rr. POSIX mode now enables the `shift_verbose' option.

2.  New Features in Readline

a. Non-incremental vi-mode search (`N', `n') can search for a shell pattern, as
   Posix specifies (uses fnmatch(3) if available).

b. There are new `next-screen-line' and `previous-screen-line' bindable
   commands, which move the cursor to the same column in the next, or previous,
   physical line, respectively.

c. There are default key bindings for control-arrow-key key combinations.

d. A negative argument (-N) to `quoted-insert' means to insert the next N
   characters using quoted-insert.

e. New public function: rl_check_signals(), which allows applications to
   respond to signals that readline catches while waiting for input using
   a custom read function.

f. There is new support for conditionally testing the readline version in an
   inputrc file, with a full set of arithmetic comparison operators available.

g. There is a simple variable comparison facility available for use within an
   inputrc file. Allowable operators are equality and inequality; string
   variables may be compared to a value; boolean variables must be compared to
   either `on' or `off'; variable names are separated from the operator by
   whitespace.

h. The history expansion library now understands command and process
   substitution and extended globbing and allows them to appear anywhere in a
   word.

i. The history library has a new variable that allows applications to set the
   initial quoting state, so quoting state can be inherited from a previous
   line.

j. Readline now allows application-defined keymap names; there is a new public
   function, rl_set_keymap_name(), to do that.

k. The "Insert" keypad key, if available, now puts readline into overwrite
   mode.

-- 
``The lyf so short, the craft so long to lerne.'' - Chaucer
                 ``Ars longa, vita brevis'' - Hippocrates
Chet Ramey, UTech, CWRU    address@hidden    http://tiswww.cwru.edu/~chet/
[2019-01-11 00:49]

