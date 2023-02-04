# Implementation Details: Untyped Lambda Calculus

The primary goal of Lambda Calculator is to be as close to pure Lambda Calculus as
possible, while providing enough extensions to be useful. In this document, we will
describe how the interpreter works and highlight differences from pure Lambda Calculus.

## Syntax

We support the usual synax forms: variable, abstraction, and application. Additionally,
let expressions may be used to bind an expression to a name. Subsequently, all references
to that name will be substituted with the bound expression.

All of the syntax forms are summarized in the table below:

|form       |name       |
|-----------|-----------|
|`x`        |variable   |
|`\x. t`    |abstraction|
|`f x`      |application|
|`let x = t`|let        |

Here are some examples of valid expressions:

    \x. x
    (\x. x) n
    (\n f x. f (n f x)) (\f x. f (f x))
    let id = (\x. x)
    
A let expression can only occur at the top-level. Here are some examples of invalid
expressions:

    let x = let y = z
    \x. let z = w

## Evaluation

In pure Lambda Calculus, abstractions are the only valid values. However, in Lambda
Calculator, any expression can be a value, as long as it is as reduced to normal
form. This means that free variables are allowed, and we reduce expressions as far as
we can.

Each evaluation rule has the general form:

    t → t' ⇒ u → u'
    
which means if `t` evaluates to `t'`, then `u` evaluates to `u'`

We will start with function application, which can be described by the two rules

    t → t', u → u' ⇒ t u → t' u' (E-App)
    (\x. t) w → [x ↦ w] t        (E-AppAbs)
    
The first rule, _E-App_ means that we attempt to reduce each operand before applying an
abstraction. The second rule, _E-AppAbs_ represents beta reduction. It says that an
abstraction `\x. t` applied to a term `w` is evaluated by substituting the abstraction's
argument `x` with `w` in the abstraction's body `t`.

While applying _E-AppAbs_, we also perform Alpha conversion to prevent shadowing free
variables in the first term by abstractions in the second. When we see the following form:

    (\w. x) (\y. z)

We rewrite `(\w. x)` to an another abstraction who's argument does not appear free in
`(\y. z)`.

Next, we have Eta conversion

    \x. f x → f (E-Eta)
    
Which converts any abstraction to it's point-free representation.

Finally, we have let expression evaluation

    t → u ⇒ let x = t → u   (E-Let)
    x = y ∈ Γ ⇒ x → y (E-Global)
    
Unlike in pure Lambda Calculus we have a globals context, we call &Gamma;. This contains
pairs names to expressions. The rule _E-Let_ allow the user to bind a global variable. We
first attempt to reduce its body to normal form, and then we add the pair `x = u` to
&Gamma;.

We use the next rule, _E-Global_, to replace free variables if they are bound in &Gamma;.

All of the evaluation rules are summarized in the table below:

|rule                          |name       |
|------------------------------|-----------|
|`t → t', u → u' ⇒ t u → t' u'`|E-App    |
|`(\x. t) w → [x ↦ w] t`       |E-AppAbs |
|`\x. f x → f`                 |E-Eta    |
|`t → u ⇒ let x = t → u`       |E-Let    |
|`x = y ∈ Γ ⇒ x → y`           |E-Global |
