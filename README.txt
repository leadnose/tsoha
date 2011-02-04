
About Common Lisp:


COMMENTS start with ; and continue to end of line.


STRINGS look like "this", just like in most other languages.


NUMBERS look like numbers in most languages, but 1/3 denotes a
rational number, not a division operation,  and 2/6 would be the
same as 1/3.



PACKAGES are somewhat like packages in Java, and in this app they are
all defined in the file `packages.lisp´. They define what names are
visible to the outside of packages. 


MACROS look like function calls but are called (expanded) before
compilation. Macros are  not really important in this app, but
there are few typical macro-usages in  this app, eg.

    (with-connection
       <code that uses the database>)

This means that the database connection is established before
entering the body of the expression, and database is disconnected
after it, regardless of whether the body exited normally or if it
raised an error. In Java that would look something like this:

    Connection connection = null;
    try {
        connection = new Connection(foo, bar);
        <code that uses the database>
    } finally {
        if (connection != null)
        connection.close();
    }

This idiom of macros starting with "WITH-" is quite common and
it is almost always used like this, to acquire some resource,
execute code that uses the resource, and release the resource,
all in controlled and concise manner.
    
   
 


