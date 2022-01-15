# rabin-cryptosystem

Haskell implementation of Rabin's Public Key Cryptosystem, written for the Public Key Cryptography course @ Babes-Bolyai University, Cluj-Napoca

## Prerequisites

* GHC >= 9.0
* cabal >= 3.6

(Both can be installed via [ghcup](https://www.haskell.org/ghcup/))

## Installing & Running

To install the project, simply run `cabal install`. Afterwards, you can simply run it as `rabin`.

If you don't want to install it, you can run it locally using the provided `run.sh` script.

## CLI Interface

```sh
rabin gen-priv-key [--bits N] # Writes the binary-encoded private key to stdout. Defaults to 256 bits
rabin gen-pub-key # Reads the private key from stdin, writes the public key to stdout
rabin encrypt --pub-key PATH # Reads the plaintext message from stdin, writes the ciphertext to stdout
rabin decrypt --priv-key PATH # Reads the ciphertext from stdin, writes the cypheretext to stdout
```

If you haven't installed the project, use `./run.sh` instead of `rabin`.

## Example usage

```sh
# Generate private/public key pair
rabin gen-priv-key >a.key
rabin gen-pub-key <a.key >a.pub

# Encrypt a file, e.g. an image
rabin encrypt --pub-key a.pub <image.png >image.enc
rabin decrypt --priv-key a.key <image.enc >decrypted.png
```

## Security Notice

Aside from the Rabin cryptosystem's [known weaknesses](https://en.wikipedia.org/wiki/Rabin_cryptosystem#Security), the implementation may also be vulnerable to [timing attacks](https://en.wikipedia.org/wiki/Timing_attack) or other exploits.

Do **not** use this for security-critical systems.

## License

This repository is licensed under the terms of the GNU General Public License v3.
For more details, see [the license file](LICENSE.txt).
