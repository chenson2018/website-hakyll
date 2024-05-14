---
title: Quasiquotation of the Untyped Lambda Calculus
tags: Haskell, Template Haskell, Metaprogramming, Lambda Calculus
---

After getting a taste of Template Haskell for my last post, I wanted to try out
quasiquotation. This is all directly adapted from the original paper [Why It’s
Nice to be Quoted: Quasiquoting for
Haskell](https://dl.acm.org/doi/10.1145/1291201.1291211) and [this blog
post](https://well-typed.com/blog/2014/10/quasi-quoting-dsls/), but some
details took a bit so I thought I'd write a short post walking through the main
points. The full code can be found [at this
repo](https://github.com/chenson2018/quasi-lambda).

Quasiquotation isn't something unique to Haskell. My understanding is that its
history dates back to Lisp. I actually first [encountered it in
R](https://adv-r.hadley.nz/quasiquotation.html), a language that traces its
lineage to Lisp, where it is used extensively in the tidyverse package
ecosystem.

The main idea of quasiquotation as I'll be using it here is allowing the
embedding of a DSL, in this case the untyped lambda calculus, that allows not
only the parsing of literal expressions but interaction with Haskell terms. It
is probably simplest to just see an example:

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Exception (assert)
import Eval
import Lambda

main :: IO ()
main =
  do
    let scc = [ex|λn. λs. λz. s (n s z)|]
    let plus = [ex|λm. λn. λs. λz. m s (n s z)|]

    let zero = [ex| λs. λz. z|]
    let one = full_beta [ex| $e:scc $e:zero |]
    let two = full_beta [ex| $e:scc $e:one |]
    let three = full_beta [ex| $e:scc $e:two |]

    print $ assert (one == [ex| λs. λz. s z |]) one
    print $ assert (three == full_beta [ex| $e:plus $e:one $e:two|]) three
```

which will print:

```haskell
Lam (V "s") (Lam (V "z") (App (Var (V "s")) (Var (V "z"))))
Lam (V "s") (Lam (V "z") (App (Var (V "s")) (App (Var (V "s")) (App (Var (V "s")) (Var (V "z"))))))
```

Here the examples are some arithmetic with Church-encoded numerals. I think the
appeal is pretty immediately obvious. It is much nicer to be able to write our
lambda calculus terms in this recognizable format, and the second example shows
how we can embed Haskell terms as well. In fact, quasiquotation can even be
used within patterns! It should also be noted that all of this is happening at
compile time.

So, what all was required to set this up? It really is not that bad. Here are
the types for the lambda calculus:

```haskell
data Var
  = V String
  | AV String
  deriving (Show, Eq, Data)

data Exp
  = Var Var
  | Lam Var Exp
  | App Exp Exp
  | AE String
  deriving (Show, Data, Eq)
```

The two additions here from a usual definition are the constructors
`AV`{.haskell} and `AE`{.haskell}, which will be used for the antiquotation that
allows interaction with Haskell variables.

One nice thing about quasiquotation is that we'll be able to use an ordinary
parser:

```haskell
import Text.Megaparsec (Parsec, between, eof, errorBundlePretty, many, parse, satisfy, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, space, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- Parsing boilerplate
type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

parseIO :: Parser a -> String -> IO a
parseIO p str =
  case parse p "" str of
    Left err -> fail $ errorBundlePretty err
    Right a -> return a

topLevel :: Parser a -> Parser a
topLevel p = space *> p <* eof

-- untyped lambda calculus parser, including antiquotation
ident :: Parser String
ident = lexeme $
  do
    c <- satisfy $ ap ((&&) . isLower) (/= 'λ')
    cs <- many (alphaNumChar <|> char '_' <|> char '\'')
    return (c : cs)

var :: Parser Var
var = (V <$> ident) <|> (AV <$> (string "$v:" >> ident))

pexp :: Parser Exp
pexp = foldl1 App <$> many aexp

aexp :: Parser Exp
aexp = 
    (try $ Var <$> var)
    <|>
    do lexeme $ (char '\\' <|> char 'λ')
       v <- var
       lexeme $ char '.'
       Lam v <$> pexp
    <|>
    (between (lexeme $ char '(') (lexeme $ char ')') pexp)
    <|>
    (AE <$> (string "$e:" >> ident))
```

This is all pretty straightforward, though anyone who follows me on Twitter may
recognize this as the point where `'λ'`{.haskell} being counted as lowercase
caused me some difficulties.

Lastly, we need to be able define when exactly we want to be able to interact
with Haskell terms, which is precisely in this case when we encounter the
`AV`{.haskell} and `AE`{.haskell} constructors:


```haskell
antiVarE :: Var -> Maybe ExpQ
antiVarE (AV v) = Just $ varE $ mkName v
antiVarE _ = Nothing

antiExpE :: Exp -> Maybe ExpQ
antiExpE (AE v) = Just $ varE $ mkName v
antiExpE _ = Nothing

antiVarP :: Var -> Maybe PatQ
antiVarP (AV v) = Just $ varP $ mkName v
antiVarP _ = Nothing

antiExpP :: Exp -> Maybe PatQ
antiExpP (AE v) = Just $ varP $ mkName v
antiExpP _ = Nothing
```

And finally, we construct the quasiquoter:

```haskell
ex :: QuasiQuoter
ex =
  QuasiQuoter
    { quoteExp = \str -> (runIO $ parseIO (topLevel pexp) str) >>= dataToExpQ (const Nothing `extQ` antiVarE `extQ` antiExpE),
      quotePat = \str -> (runIO $ parseIO (topLevel pexp) str) >>= dataToPatQ (const Nothing `extQ` antiVarP `extQ` antiExpP),
      quoteDec = undefined,
      quoteType = undefined
    }
```

Here we leave `quoteDec`{.haskell} and `quoteType`{.haskell} undefined. These
would allow quasiquoters for types and declarations which we don't have a use
for here, but might make sense for instance in a typed lambda calculus that
uses Haskell types.

Recall that I mentioned that we can also use quasiquoters in patterns. Here's
an example of what that looks like:

```haskell
toChurch :: Int -> Exp
toChurch 0 = [ex| λs. λz. z|]
toChurch n = full_beta [ex| $e:scc $e:prev |]
  where
    scc = [ex|λn. λs. λz. s (n s z)|]
    prev = toChurch $ n - 1

fromChurch :: Exp -> Maybe Int
fromChurch [ex| λs. λz. z|] = Just 0
fromChurch [ex| λs. λz. s $e:e|] = (+1) <$> fromChurch [ex| λs. λz. $e:e|]
fromChurch _ = Nothing
```

Without quasiquotation, this would be much more difficult to read:

```haskell
toChurch :: Int -> Exp
toChurch 0 = Lam (V "s") $ Lam (V "z") (Var $ V "z")
toChurch n = full_beta $ App scc prev
  where
    scc = Lam (V "n") $ Lam (V "s") $ Lam (V "z") $ App (Var $ V "s") (App (App (Var $ V "n") (Var $ V "s")) (Var $ V "z"))
    prev = toChurch $ n - 1

fromChurch :: Exp -> Maybe Int
fromChurch (Lam (V "s") (Lam (V "z") (Var (V "z")))) = Just 0
fromChurch (Lam (V "s") (Lam (V "z") (App (Var (V "s")) e))) = (+1) <$> fromChurch (Lam (V "s") (Lam (V "z") e))
fromChurch _ = Nothing
```

My main difficulties in writing this post were not really technical, but in
finding documentation. Hopefully this will help the next person that comes
along!
