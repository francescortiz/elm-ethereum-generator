# elm-ethereum-generator
Generate an [elm-ethereum](https://github.com/cmditch/elm-ethereum) binding for an Ethereum smart contract from it's ABI

Install and usage:
```
npm install -g elm-ethereum-contract  


elm-ethereum-generator some-abi.json src/MyContract.elm [--debug]
```
Use the debug flag to generate code which will log contract interactions to the browser console.
