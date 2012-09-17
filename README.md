simple-log
=======

The main ideas of this log library are:
1. We don't want to see all unnecessary trace messages when there are no errors.
2. But we want to have all possible information about error.

This library is based on scopes. Every scope have a name, and logs traces only if there are some errors. Otherwise it logs only message with 'Info' level.

Let's start by simple example:

<pre>
test :: ReaderT Log IO ()
test = scope "test" $ do
    log Trace "Trace message"
    log Info "Starting test"
    s &lt;- liftIO T.getLine
    when (T.null s) $ log Error "Oh no!"
    log Trace $ T.concat ["Your input: ", s]
</pre>

When you input some valid string, it will produce output:
<pre>
08/10/12 22:23:34	INFO	test> Starting test
abc
</pre>
wihtout any traces

But if you input empty strings, you'll get:
<pre>
08/10/12 22:24:20	INFO	test> Starting test

08/10/12 22:24:20	TRACE	test> Trace message
08/10/12 22:24:21	ERROR	test> Oh no!
08/10/12 22:24:21	TRACE	test> Your input: 
</pre>

Note, that first TRACE is written after INFO, that's because logger don't know whether TRACE message will be written or not, but he must write INFO message immediately.
But that's not a big problem.

There are three scope functions: 'scope_', 'scope' and 'scoper'.
'scope_' is basic function. 'scope' catches all exceptions and logs error with it, then rethrows. 'scoper' is like 'scope', but logs (with TRACE level) result of do-block.

Of course, scopes can be nested:

<pre>
test :: ReaderT Log IO ()
test = scope "test" $ do
    log Trace "test trace"
    foo
    log Info "some info"
    bar

foo :: ReaderT Log IO ()
foo = scope "foo" $ do
    log Trace "foo trace"

bar :: ReaderT Log IO ()
bar = scope "bar" $ do
    log Trace "bar trace"
    log Error "bar error"
</pre>

Output:

<pre>
08/10/12 22:32:53	INFO	test> some info
08/10/12 22:32:53	TRACE	test/bar> bar trace
08/10/12 22:32:53	ERROR	test/bar> bar error
</pre>

Note, no messages for 'foo' and no trace messages for 'test', because error was in 'bar', not in 'foo'.

Code to run log:
<pre>
rules :: Rules
rules = []

run :: IO ()
run = do
    l &lt;- newLog (constant rules) [logger text console]
    withLog l test
</pre>

Politics sets 'low' and 'high' levels. By default, 'low' and 'high' are INFO and WARN.
Levels below 'low' are "traces" (TRACE and DEBUG by default).
Levels above 'high' are "errors" (ERROR and FATAL by default).

If you set 'low' to TRACE, all messages will be written.
If you set 'low' to DEBUG and 'high' to FATAL, "traces" (in this case only TRACE) will be never written.

Sometimes we need to trace function, but we don't want to write all traces. We can get this by setting rules. Rules changes politics for specified scope-path (scope-path is list of nested scopes, for example ["test"], ["test", "bar"], ["test", "bar", "baz", "quux"] etc.)

For example, we want to trace function 'foo':
<pre>
rules = [
    rule root $ use defaultPolitics,
    rule (relative ["foo"]) $ low Trace]
</pre>
From now all scope-paths, that contains "foo" (all scopes with name "foo") will have politics with 'low' set to Trace.

We may adjust politics for scope 'foo', that is nested directly in scope 'quux':
<pre>
rules = [
    rule root $ use defaultPolitics,
    rule (relative ["quux", "foo"]) $ low Trace]
</pre>

And, of course, we may specify absolute path:
<pre>
rules = [
    rule root $ use defaultPolitics,
    rule (absolute ["bar", "baz", "foo"]) $ low Trace]
</pre>
Politics will be changed only for scope "foo", which is nested directly in "baz", which is nested in "bar".

Another way to define rule is using special functions from "System.Log.Config" module:
<pre>
rules = [
    "/" %= use defaultPolitics,
    "/bar/baz/foo" %= low Trace,
    "quux/foo" %= low Debug]
</pre>

One more way to use special syntax for rules:
<pre>
rules = parseRules_ $ T.unlines [
    "/: use default",
    "/bar/baz/foo: low trace",
    "quux/foo: low debug"]
</pre>

Here "/" is for root, "/path" for absolute path, "path" for relative and "path/" for child of "path" (which may be also prefixed with "/" to be absolute)

This syntax is useful to config log by file. Having file "log.cfg":
<pre>
/: use default
/bar/baz/foo: low trace
quux/foo: low debug
</pre>

We can use it to config log
<pre>
    l <- newLog (fileCfg "log.cfg" 60) [logger text console]
</pre>
where 60 is period (in seconds) of auto reload or 0 for no reloading.
