# Implementation Details: System F

The primary goal of Lambda Calculator is to be as close to pure System F as possible,
while providing enough extensions to be useful. In this document, we will describe how the
interpreter works and highlight differences from pure System F.

## Syntax

We support the usual syntax forms: variable, application, abstraction, type application,
and type abstraction.

As in the Untyped interpreter, let expressions may be used to bind
an expression to a name. Subsequently, all references to that name will be substituted
with the bound expression.

Because we allow free variables and free types, we also allow a variable annotated with a
type. All syntax forms are summarized in the table below:

|form       |name                   |
|-----------|-----------------------|
|`x`        |variable               |
|`x:T`      |type annotated variable|
|`f x`      |application            |
|`\x:T. t`  |abstraction            |
|`x [T]`    |type application       |
|`\X. t`    |type abstraction       |
|`let x = t`|let                    |

We support pure System F types with no changes, summarized below:

|form       |name                |
|-------------|--------------------|
|`T`          |type variable       |
|`T->T`       |type of functions   |
|`forall X. T`|universal type      |

## Typechecking

We begin with a typing context &Gamma;, which is a mapping to free variables to their
types. Items in &Gamma; may also be type variables, which do not have associated types.
We will use the following notation for &Gaimma;:

|notation|name                 |
|--------|---------------------|
|`Γ,x:T` |term variable binding|
|`Γ,X`   |type variable binding|

Most typing rules has the general from:

    Γ ⊢ t:T ⇒ u : U
    
Which means if, in the typing context &Gamma;, `t` has type `T`, then `u` has type `U`.

We start with the type of variables:

    
    x:T  Γ ⇒ x : T      (T-Var)
    x ∉ Γ ⇒ x : Z       (T-Var2)

The first two rules, _T-Var_ means that a variable's type is looked up from the
context. Because we allow free variables, if the variable is not in the context, we
generate a unique type variable. We assume all free type variables are concrete types (and
not universal types).

Next, we have type annotated variables:

    x:T ∈ Γ ⇒ (x:T) : T (T-VarAnn)
    x ∉ Γ ⇒ (x:T) : T   (T-VarAnn2)
    
Which allows the user to specify the type of a variable. The variable is again looked up
in the context. If it does not match, it is a type error. If the variable is not in the
context, it has the specified type.

Next, we have the type of abstractions:

    Γ,x:T ⊢ t:U ⇒ (\x:T. t) : T -> U (T-Abs)

We first add the parameter `x:T` to the context, then calculate the type of its body
`t`. The resulting type is a function type mapping `T` to `U`.

Application is unchanged from pure System F:

    Γ ⊢ f:(T -> U), x:T ⇒ f x : U (T-App)
    
Next, we have type abstraction:

    Γ,X ⊢ t:T ⇒ \X. t : forall X. T       (T-TyAbs)
    
We add the type abstraction parameter `X` to &Gamma;, then calculate the type of its body.
    
Type application is unchanged from pure System F:

    Γ ⊢ t:(forall X. T) ⇒ t [V] : [X ↦ V] T (T-TyApp)

Given the rules above, if we have an abstraction, say `\x:(forall T. U). x`, then its type
would be `(forall T. U) -> (forall T. U)`. It would be more correct for it to be `forall T. U -> U`,
so we introduce variants of `T-Var` to account for this.

    x:(forall T. U), T ∈ Γ ⇒ x : U     (T-VarPoly)
    x:(forall T. U), T ∈ Γ ⇒ (x:U) : U (T-VarAnnPoly)
    Γ,T ⊢ t:V ⇒ (\x:(forall T. U). t) : forall T. U -> V (T-AbsPoly)
    Γ ⊢ f:(forall T. U -> V), x:(forall T. U -> V) ⇒ (f x) : forall T. V (T-App)
