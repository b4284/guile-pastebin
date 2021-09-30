# guile-pastebin

A very simple pastebin written in Guile Scheme.

## How to run

Execute `guile -L . -e run-pastebin main.scm <DATA_DIR>` and then
browse to `http://localhost:8080`.

`<DATA_DIR>` is a writable filesystem location you want to use to
store pastes.  If it doesn't exist, it will be created.

It was tested on Guile 2.2.7 and 3.0.7.  Many thanks to the Guile
development team for such great software.

## API

- `/post` -- create paste
  - method: POST only
  - type: `multipart/form-data`
  - fields
    - `text` -- the paste content
    - `showUrl=1` -- to show the raw URL after paste
- `/raw/<ID>` -- get paste content
  - method: GET only
- Anything else -- show top 5 paste list

## Paste a file from command line

Paste a text file from command line and get a URL back:
`curl -F "text=<helloworld.cs" -F showUrl=1 http://localhost:8080/post`

Notice the double-quotes around the parameters because `<` means IO
redirection in sh.

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

