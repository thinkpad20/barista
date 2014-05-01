# LatteScript

LatteScript strives to be to TypeScript as CoffeeScript is to JavaScript. LatteScript hews closely to CoffeeScript syntax (more closely, at least initially, than LiveScript), but does not aim to be a complete superset. Indeed, it attempts to fix a few niggling issues in CoffeeScript. However, the transition from existing CoffeeScript should be minimal. LatteScript extends CoffeeScript with TypeScript's types and interfaces, and some of its class system. It is compiled into TypeScript, where it will receive the same type checking that that receives. 

As this is being prototyped in Haskell, we will initially not have access to TypeScript's implementation in JavaScript, so we will generate actual TypeScript code, which can be compiled by `tsc`. However, eventually we will be implemented in JavaScript, allowing us to skip this step and generate a TypeScript AST directly.

## Examples:

Coming soon....
