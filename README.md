# Lambia
Lambia /lamia/ the practical Lambda-Calculus language

## Usage
```
$ Lambia Pre.lm -i
> [Source]
(\ab.a)
> open Pre
> Fact = Fix (\fn.If (Zero n) (Succ 0) (Mul n (f (Pred n))))
> Fact (Succ (Succ (Succ 0)))
(\ab.a (a (a (a (a (a b))))))
>
