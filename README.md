# How To Start A Development Environment

```
nix develop

foreman run liftbridge &

# restart on changes to source files
nix-shell -p ripgrep entr
rg --files | entr -r go run . all
```
