# lightningometry

This is a tool for choosing nodes with whom to open a channel based on some
rough heuristics.  Based on a set of target nodes we wish to pay, with some
priority weights, it finds nodes which independently maximize the routing
capacity to the target nodes and minimize the priority weighted hop count. 

### Disclaimer

This is **alpha** software.  Use at your own risk.  Contributions are welcome.

## Usage

Help text is available with `lightningometry --help`.  The software expects a
channel dump from `lightningd`.  This is simply the output of `lightning-cli
listchannels`.  It also requires a YAML file of payees structured like:

``` yaml
- node_id: $NODE_ID
  weight: $WEIGHT_COEFFICIENT
```


## Building & installation

Build with `cabal`:

``` shell
$ git clone https://github.com/GambolingPangolin/lightningometry
$ cd lightningometry
$ cabal build
```

Install to the default location for `cabal` (typically `$HOME/.cabal/bin`)

``` shell
$ cabal install 
```
