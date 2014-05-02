# LatteScript

LatteScript strives to be to TypeScript as CoffeeScript is to JavaScript. LatteScript extends CoffeeScript with TypeScript's types and interfaces, and some of its class system. It is compiled into TypeScript, where it will receive the same type checking that that receives.

Initially, the goal is to be a complete superset of CoffeeScript. However; there might be a few departures; for example, variable declarations might be made more safe (e.g. having explicit `var`) instead of CoffeeScript's rule. Operator precedence might be shifted slightly (for example, so that `foo a and bar b == (foo a) and (bar b)`). However, since there is a large amount of extant CoffeeScript in production, it is more important to support the current language, warts and all. Any backwards-incompatible changes should be able to be disabled if not desired.

## Implementation

Currently, the implementation is in Haskell. Writing a complete CoffeeScript parser frontend in Haskell is probably worth something on its own, and certainly the language is easier and faster for this kind of project than almost any other. The downside is a big dependency, and that we will initially not have access to TypeScript's implementation in JavaScript, which would be optimal. We can thus either (a) output typescript code directly, or (b) output JSON representations of TypeScript objects. Eventually LatteScript will be implemented in JavaScript, TypeScript, or itself, allowing us to skip this step and generate a TypeScript AST and subsequent type checking and code generation without intermediate files.

## Current Status

### Parsing

We're already fairly close to finishing the parser, which is (arguably) the biggest part. It parses a somewhat large subset of CoffeeScript. A few notable missing pieces:
- Array comprehensions
- Switch statements
- Try/catch
- Dictionaries
- No doubt there are some other things. 

### Extending

The second part is extending the language, adding type annotations, interfaces, and a few other things. Work on this has not yet started.

### Compiling

The last part of the project is compiling the LatteScript to a TypeScript AST. We have nothing on this front yet.

## Name

Latte is a type of Coffee. Yeah.

## Examples:

Coming soon....
