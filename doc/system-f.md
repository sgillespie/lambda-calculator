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

    Γ,X ⊢ t:T ⇒ \X. t : forall X. T (T-TyAbs)
    
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

All of the typing rules are summarized below:

    x:T  Γ ⇒ x : T                   (T-Var)
    x ∉ Γ ⇒ x : Z                    (T-Var2)
    x:T ∈ Γ ⇒ (x:T) : T              (T-VarAnn)
    x ∉ Γ ⇒ (x:T) : T                (T-VarAnn2)
    Γ,x:T ⊢ t:U ⇒ (\x:T. t) : T -> U (T-Abs)
    Γ ⊢ f:(T -> U), x:T ⇒ f x : U    (T-App)
    Γ,X ⊢ t:T ⇒ \X. t : forall X. T  (T-TyAbs)
    Γ ⊢ t:(forall X. T) ⇒ t [V] : [X ↦ V] T
                                     (T-TyApp)
    x:(forall T. U), T ∈ Γ ⇒ x : U   (T-VarPoly)
    x:(forall T. U), T ∈ Γ ⇒ (x:U) : U
                                     (T-VarAnnPoly)
    Γ,T ⊢ t:V ⇒ (\x:(forall T. U). t) : forall T. U -> V
                                     (T-AbsPoly)
    Γ ⊢ f:(forall T. U -> V), x:(forall T. U -> V) ⇒ (f x) : forall T. V
                                     (T-AppPoly)
## Evaluation

In pure System F, only abstractions and type abstractins are valid values. Because we
allow free variables, as in the untyped interpreter, we allow any expression as a value.
Also in pure System F, free type variables are not allowed, but we allow them as well.

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

    (\w:T. x) (\y:U. z)

We rewrite `(\w:T. x)` to an another abstraction who's argument does not appear free in
`(\y:U. z)`.

Next, we have Eta conversion

    \x::T. f x → f (E-Eta)
    
Which converts any abstraction to it's point-free representation.

The following type application rules are unchanged from pure System F:

    t → t', ⇒ t [T] → t' [T] (E-TApp)
    (\X. t) [T] → [X ↦ T] t  (E-TAppTAbs)

We add polymorphic variants:

    x:(forall X. T) [U] (E-TAppVarPoly)
    (\x:(forall X. T). t) [U] → \x:([X ↦ U] T). t 
                        (E-TAppAbsPoly)

Finally, we have let expression evaluation

    t → u ⇒ let x = t → u   (E-Let)
    x = y ∈ Γ ⇒ x → y       (E-Global)
    
Unlike in pure System F we have a globals context, we call &Gamma;. This contains pairs
names to expressions. The rule _E-Let_ allows the user to bind a global variable. We first
attempt to reduce its body to normal form, and then we add the pair `x = u` to &Gamma;.

We use the next rule, _E-Global_, to replace free variables if they are bound in &Gamma;.

All of the evaluation rules are summarized below:

    t → t', u → u' ⇒ t u → t' u' (E-App)
    (\x. t) w → [x ↦ w] t        (E-AppAbs)
    \x::T. f x → f               (E-Eta)
    t → t', ⇒ t [T] → t' [T]     (E-TApp)
    (\X. t) [T] → [X ↦ T] t      (E-TAppTAbs)
    x:(forall X. T) [U]          (E-TAppVarPoly)
    (\x:(forall X. T). t) [U] → \x:([X ↦ U] T). t
                                 (E-TAppAbsPoly)
    t → u ⇒ let x = t → u        (E-Let)
    x = y ∈ Γ ⇒ x → y            (E-Global)
