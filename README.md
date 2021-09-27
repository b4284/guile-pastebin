# guile-pastebin
A very simple pastebin written in Guile Scheme.

## How to run

Execute `guile -L . -e run-pastebin main.scm <DATA_DIR>` and then
browse to `http://localhost:8080`.

`<DATA_DIR>` is a writable filesystem location you want to use to
store pastes.  If it doesn't exist, it will be created.

It was tested on Guile 2.2.7 and 3.0.7.  Many thanks to the Guile
development team for such great software.

## Reverse Proxy

It may be desirable to setup a reverse proxy on Apache Httpd (also a memo
for myself), so you don't have to expose the 8080 or whatever port:

```
    <Location "/pastebin">
        ProxyPass "http://your_address:8080"
        ProxyPassReverse "http://your_address:8080"
        ProxyHTMLEnable On
        ProxyHTMLDocType "<!DOCTYPE html>"
        ProxyHTMLURLMap "/" "/pastebin/"
    </Location>
```

