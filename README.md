# haskellpad

# Notes on building with nix
- watch out that there are no other `nix.conf` files, e.g at `~/.config/nix` that interfere with the caches provided by reflex-frp. See here: https://github.com/reflex-frp/reflex-platform/issues/335
- after the DONE label, if it is the initial install, it is okay to take quite some time, as it fetches and activates caches (est. 10 min). But make sure that binary chaches are setup correctly, otherwise it'll take forever.
