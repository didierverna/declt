## Version
- New function `nickname-package` in the assessment system.
- Require SBCL 2.3.1, and follow changes related to method combinations.
- Revert output file name's default value to the system name.

## Version 4.0b2
- Fix bug in the finalization phase's new funcoid discovery.

## Version 4.0b1
This release completes the assessment stage of the new processing pipeline.
- Backward Incompatible Changes.
  * Some keyword argument to the `declt` function have changed: :hyperlinks
	becomes :locations, :version becomes :library-version, :texi-directory
	becomes :output-directory, and :texi-name becomes :file-name.
- New Features.
  * Support for hiding default / standard values (e.g. standard method
	combination, :instance slot allocation, etc.).
  * Support for Aliases (funcoids which have their fdefinition, macro, or
	compiler macro function set manually to another, original, definition).
  * Support for typed structures and setf compiler macros.
  * Proper support for uninterened symbols, denoted by the "empty set" (∅)
	package.
  * SBCL 2.1.2 is now required for the following enhancements.
	* Short form setf expanders now get correct source information.
	* Method combinations lambda-lists are now documented.
  * Domestic definitions now include those created in one of the sources files
	of the library being documented, even if the symbol naming the definition
	is from a foreign package. If the source file is unknown, then the
	symbol’s package must be domestic.
  * Support for documenting foreign definitions.
  * New introspection heuristic, ensuring complete coverage at the expense of
	a much longer computation time.
  * Support for the Microsoft Public License.
  * The value of Texinfo’s @direntry command is now customizable.
- Improvements.
  * Documentation thinning.
	- packages reference lists do not point to methods directly anymore (as
	  they can be reached via the gneric function’s reference). Also, only
	  slots for which the parent classoid is named from another package are
	  now referenced.
	- Files reference lists do not point to slots anymore (as they can be
	  reached via the parent classoid’s reference). Also, only methods for
	  which the parent generic function is defined in another file are now
	  referenced.
	- The readability of long references has been improved. In particular,
	  they don’t advertise the type of the referenced definitions anymore,
	  when there is no ambiguity.
	- Slot documentation now advertises the slot name’s package only when
	  different from that of the parent classoid.
	- Non standalone method documentation now advertises the source file only
	  when different from that of the parent generic function.
	- The rendering of EQL specializers has been inmproved.
	- The documentation of setf / writer methods doesn’t render the “new
	  value” argument / specializer anymore.
  * Generic definitions containing only reader / writer methods are upgraded
	to specific reader / writer categories, and definition merging is now only
	attempted on those.
  * Lambda Lists.
	- Uninformative parts of lambda lists are now filtered out. This includes
	  &whole, &environment, &aux variables, along with options / keyword
	  variables and default values.
	- Method specializers in lambda lists now link back to their respective
	  class definitions.
  * The method combination discovery scheme has been upgraded to benefit from
	SBCL 1.4.8’s enhancements.
- Bug Fixes and Workarounds.
  * ASDF.
	- Better handling of missing / unloaded components or dependencies (this
	  can happen for instance with feature-dependent conditional inclusion).
	- Cope with the lack of specification of the license information in ASDF
	  systems by coercing to a string.
	- Fix several cases of system files documentation duplication. Declt
	  automatically documents .asd files as special cases of Lisp files.
	  However, some systems have the bad (IMHO) habit of mentioning them
	  explicitly as components (e.g. static files). When this happens, Declt
	  silently discards that definition and keeps its own (at the expense of
	  having a slightly incorrect system documentation).
  * Anchor names now use numerical definition UIDs.
  * Setf Expanders.
	- Fix the computation of short form setf epxander lambda lists (which
	  didn’t correctly handle the presence of optional or rest arguments
	  before.
	- Handle the potential unavailability of a setf expander’s update function
	  or short form operator. Document it if applicable. Also signal a warning
	  when the expander is domestic.

# Version 3.0 "Montgomery Scott"
- New requirements: Texinfo 6.7 and Unicode-aware documentation readers
  (including info ones).
- Bugfixes:
  *	Some method specializers were not handled properly.
  * The manuals were missing documentation for some non-Lisp source files.
  * There were some glitches in the pretty-printing of unreadable objects.

# Version 2.4.1 ""
- Fix (special case) support for ASDF and UIOP documentation generation.

# Version 2.4 "Will Decker"
- Support for Boost license.
- Better system packages detection scheme.
- Various pretting printing improvements.
- Fix and improve handling of Declt notices.
- Fix bug in symbol macro rendering.


# Version 2.3 "Robert April"
- Advertise file extensions in references.
- Advertise the type of foreign definitions.
- More robust display and indexing of, and with, lambda-lists.
- Use UTF8 special characters to denote invisble ones.
- More robust support for Texinfo brace escaping.
- Handle modules sharing the same location.
- Ensure output is done with standard IO syntax.
- Fix potential duplication of some (non-lisp) files and document all static
  files.
- Fix potential duplication of packages documentation.

# Version 2.2 "Christopher Pike"
- Require a UTF-8 environment.
- Understand ASDF’s notion of inferred system, and also be more protective
  against ASDF extensions.
- Support for improper lambda lists (e.g. destructuring ones).
- Improve contact defaulting code.
- Update support for SBCL’s setf expanders introspection.
- Accept ASDF system designators.
- Various bug fixes in the areas of method combinations, accessor definition
  merging and setf expanders.

# Version 2.1 "Jonathan Archer"
- Handle recent change in SBCL’s sb-int:info API.
- Handle list of contacts (strings) in ASDF systems author and maintainer
  slots.
- Some backward-incompatible changes in the keyword arguments to the declt
  function.
- More hyperlinks between systems and source files.
- More robust system’s packages collection (no more code walking).
- More robust handling of unavailable ASDF components.
- More robust naming of cross-references.

# Version 2.0.1 "Benjamin Sisko"
- Fix anchor generation for different symbols with the same name.

# Version 2.0 "Kathryn Janeway"
- Switch from GNU GPL to BSD licensing.
- Prefix change from com.dvlsoft to net.didierverna.
- Infrastructure revamp.

# Version 1.1 "Jean-Luc Picard"
- Support for libraries with multiple ASDF systems.
- Support for complex ASDF system and component dependencies, such as :feature
  :require and :version statements.
- Support for :if-feature.

# Version 1.0 "James T. Kirk"
- Upgraded requirements: ASDF 3 amd Texinfo 4.
- Declt now provides a user manual (as opposed to its self-generated reference
  manual).
- Much more documentation, including method combinations.

## Version 1.0b15
- Packages sections now advertise all definitions instead of just the symbols
  naming them. They also advertise their use-list and used-by-list, with
  cross-references.
- Conditions, structures and classes now advertise their sub- and
  super-classes, direct methods, initargs and slots, with cross-references.
- Slots documentation now include docstring, type, initargs, initforms,
  readers and writers with cross-references.
- Declt now documents symbol macros and compiler macros.
- The *LINK-FILES* special is gone.
- All ASDF components now advertise their descriptions and long descriptions,
  if any.
- Docstrings are displayed in a more reader-friendly fashion.
- Documentation entries for methods are nested within the corresponding
  generic function entry.

## Version 1.0b14
- Add control over the Declt notice.
- Support for MIT and LGPL licenses.
- Bug fix in lambda list management.

## Version 1.0b13
- Fix 2 calls to FIND-METHOD barfing on inexistent ones.

## Version 1.0b12
- Support for (generic) writer functions and methods. When possible, group
  documentation with corresponding reader.

## Version 1.0b11
- Check for file existence before creating links to them.

## Version 1.0b9
- Bugfix: rendering the documentation for methods with EQL specializers didn’t
  work.
- Feature: licensing and copyrighting the reference manual is now optional.
  Licenses currently supported are BSD and GPL.
- Declt now generates its own reference manual by default.
- Some package infrastructure changes that should remain transparent.

## Version 1.0b3
- Support ASDF Install.

## Version 1.0b2
- Don't process introduction string.
- Support for ASDF 2.

## Version 1.0b1
- First public release.
