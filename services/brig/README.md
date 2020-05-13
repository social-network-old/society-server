## Brig

The main vessel of the server fleet with a dedicated [gundeck](https://github.com/social-network/society-server/tree/develop/services/gundeck)
carrying the [cannons](https://github.com/social-network/society-server/tree/develop/services/cannon).

## Table of Contents

* [Overview](#overview)

## Overview

### Verification Codes

    echo -n "hello" | openssl dgst -sha256 -binary | head -c 16 | openssl enc -base64
