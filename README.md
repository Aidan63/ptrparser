# PtrParser

Convert c types into a tree structure and optionally to a haxe complex type using hxcpp types.

## Usage

```haxe
import PtrParser;
import haxe.macro.Printer;

function main()
{
    final input   = 'const char ** const * [3]';
    final parser  = new PtrParser(input);
    final printer = new Printer();

    final expr  = parser.parse();
    final hxcpp = parser.exprToHxcppComplexType(expr);
    final str   = printer.printComplexType(hxcpp);

    trace(input); // const char ** const * [3]
    trace(expr); // EArray(None,3,EPointer(Const,EPointer(None,EPointer(None,EType(Const,None,char)))))
    trace(str); // cpp.RawPointer<cpp.ConstStar<cpp.Star<cpp.ConstCharStar>>>
}
```

## Limits

- Does not support function pointer syntax (e.g. `int (*)(int, const char*)`)
- Does not support many cpp concepts such as scope resolution, reference returns, etc.
- Hxcpp complex type generation does not map c numeric types into hxcpp numerics (e.g. `unsigned int` to `cpp.UInt32`)
- Probably lots of weird type edge cases I haven't accounted for.
- I have no idea how to write a parser so the codes probably a bit of a nightmare.