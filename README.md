# Haskell Workshop 2024

* Install `ghcup`. Make sure it is available in PATH before starting
  vscode.
* With `ghcup`:
  * `ghcup install ghc latest`, version=9.8.1
  * `ghcup install stack latest`, version=2.13.1
  * `ghcup install hls latest`, version=2.5.0.0
  * `ghcup install cabal latest`, version=3.10.2.0 
* `cabal update`
* Install VS Code / VS Codium: https://wiki.haskell.org/IDEs#VSCodium.2FVisual_Studio_Code
* In vscode:
  * Install the HLS extension (2.4.2 is the current version): `ctrl+p` and then `ext install haskell.haskell`.

Open step 1:

```sh
codium step1
```


# Step 2

    cabal init -i step2
    cd step2
    cabal run

# Resources

* https://www.youtube.com/watch?v=jjuSXbv1nW8
* https://github.com/OpenAPITools/openapi-generator/tree/5eab3cef19675eebca9fc4e2d4905343e80fd316/samples/client/petstore/haskell-http-client
