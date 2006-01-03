SXML
by Sterling Stuart Stein
S3 <stein@ir.iit.edu>
http://lingcog.iit.edu/~scubed/



Introduction

SXML is an S-expression based XML processing library
for Bigloo Scheme.  It allows you to transform XML
similar to XSLT.  It also allows for dynamic XHTML
creation from a database, session management,
and cookie handling like PHP.

Feel free to contact me with any comments that
you have about it.



Benefits

Since there is already XSLT and PHP, why use something else?
XSLT does the built-in XML transformations very well.
However, while Turing complete, it is not very extensible.
For example, it is tricky to get it to do group by.
The syntax for the language is a bit hairy and applying
the transformations can be slow depending on which
tools are used.

Because it is Scheme, list manipulation is very easy.
Quasi-quote ('`') can be used and code fragments can
be inserted into the SXML with the standard syntax.
Because it is an ordinary programming langauge,
new features can easily be added.  Bigloo features
compiling into a native executable.  On my machine,
a native compiled stylesheet was MUCH faster than using Xalan
to perform the transformation.

PHP can be used very effectively, but typically it
is a series of echo statements that may or may not
result in something that resembles XML.  Also, if
an error occurs it can output it to the socket
revealing information about your database layout
and displaying scary messages that don't mean anything
to the users.

As long as the form is correct, SXML will generate
valid XML.  Errors can be handled by if statements,
and exception handlers.

It is meant to combine the best features of XSLT and PHP.



Installation
Note: ~> denotes the prompt and should not be typed

To install, do
~> ./configure
This will detect your computer's configuration
and save the settings for compiling.

~> make
This will compile the library and some examples
that use it.

As root,
~> make install
This will install the library and components into your system
directories so your programs may use them.
Note that this must be done as root,
such as preceeding the command by "sudo".



What is SXML?

Here is an example of XML and its SXML counterpart:
XML: (indented for readability)
<file>
  <group index="5">
    <value type="string" />
  </group>
</file>

SXML:
("file" ()
  ("group" (("index" . 5))
    ("value" (("type"."string")))
) )

Note that strings are used instead of symbols.
An SXML tag consists of:
1) The node name
2) The list of attributes
3) Zero or more children which can be SXML tags and strings

Strings represent text nodes.
Attributes are lists with each element being a cons
of the attribute's name and its value.

A basic example:

("name" (("attr1" . "val1") ("attr2" . "val2"))
  "child1" "child2" ("child3" ()))

which corresponds to: (indented)

<name attr1="val1" attr2="val2">
  child1
  child2
  <child3 />
</name>

More literally, there would be no extra spaces and newlines.
So, it would be: (unindented)
<name attr1="val1" attr2="val2">child1child2<child3/></name>



How to use

In your Bigloo programs
add this line to the module section:
(library sxml)
This will tell Bigloo Scheme to use the sxml library
and to look for the referenced functions there.



Warnings

The XML reader is not strictly XML compliant.

Entities are not treated specially
<? ?>
and
<! >
nodes are removed.
Comments are done by "--" toggling, not naively:
<! -- Comment -- off -- on -- off >

The XML written out may be invalid if characters that aren't
allowed occur in strings.  For example:
  Text nodes contain '<' or '\"'
  Node names contain ' '
  Attribute names contain ' '
