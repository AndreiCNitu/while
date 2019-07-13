
                   ██╗   ██╗ ██████╗ ██████╗  █████╗
                   ╚██╗ ██╔╝██╔═══██╗██╔══██╗██╔══██╗
                    ╚████╔╝ ██║   ██║██║  ██║███████║
                     ╚██╔╝  ██║   ██║██║  ██║██╔══██║
                      ██║   ╚██████╔╝██████╔╝██║  ██║
                      ╚═╝    ╚═════╝ ╚═════╝ ╚═╝  ╚═╝

                             Yoda Parsec

                                 by

                             Nicolas Wu



Introduction
============

Yoda is a small parser combinator library. It is not efficient, nor
beautiful, but it hopes to teach young padawans to use the source
and learn to write a parser.

    ╔═════════════════════════════════════════════════════════════╗
    ║                                                             ║
    ║  <(-,-)>  Do, or do not, there is no try.  -- Master Yoda   ║
    ║                                                             ║
    ╚═════════════════════════════════════════════════════════════╝

Yoda is a parser in the Parsec family of libraries, which includes
Parsec, attoparsec, Megaparsec, and trifecta. Once you have mastered
it, you can graduate to one of those. The main difference is that Yoda
does not require you to use the `try` function: it automatically tries
all alternatives for you.

The module exports the following functions and types. Some of these
functions are defined outside of this file, namely, those marked under
`Functor`, `Applicative`, and `Alternative`.

> module Yoda
>   ( Parser
>   , parse
>   , parseMaybe
>
>   -- Functor
>   , (<$>), (<$)
>
>   -- Applicative
>   , pure, (<*>), (<*), (*>), (<**>)
>
>   -- Alternative
>   , (<|>), empty, some, many
>
>   -- Monad
>   , return, (>>=)
>
>   -- Miscellaneous
>   , item, look, eof, char, string
>   , oneOf, noneOf, sepBy, sepBy1
>   , (<:>)
>
>   , try  -- not needed, but here for historic reasons
>
>   ) where

We have to import some classes whose instances we will be
implementing for our parsers.

> import Control.Monad
> import Control.Applicative


Parser
======

Our parsers will take in a `String` and produce a list of possible
parses, along with remaining unparsed strings.

> newtype Parser a = Parser { parse :: String -> [(String, a)] }

> parseMaybe :: Parser a -> String -> Maybe a
> parseMaybe px ts = case parse px ts of
>   []             -> Nothing
>   ((ts', x):txs) -> Just x

This parser tries to push out a character from the incoming stream. It
fails to parse if there is no remaining input.

> item :: Parser Char
> item = Parser (\ts -> case ts of
>   []      -> []
>   (t:ts') -> [(ts', t)])

Now we implement Luke, I mean, look:

> look :: Parser String
> look = Parser (\ts -> [(ts, ts)])

It is also useful to know if we have reached the end of the input:

> eof :: Parser ()
> eof = Parser (\ts -> case ts of
>   [] -> [(ts, ())]
>   _  -> [])

At this stage, we can output what has been given to us on the input,
but we have no way to change the outcome of what we do based on that
input.

We'll now start climbing the class hierarchy. Each class provides its
own ways of combining and working with parsers, and extends the power
of our combinator language with new functionality.


Functor
=======

The functor instance captures the idea of modifying the output of
successful parses.

> instance Functor Parser where
> --fmap :: (a -> b) -> Parser a -> Parser b
>   fmap f (Parser px) = Parser (\ts -> [ (ts', f x) | (ts', x) <- px ts])

Derived combinators:

< (<$>) :: Functor f => (a -> b) -> f a -> f b
< (<$>) = fmap
<
< (<$) :: Functor f => a -> f b -> f a
< (<$) = fmap . const


Applicative
===========

The applicative instance shows how parsers can be chained together.

> instance Applicative Parser where
> --pure :: a -> Parser a
>   pure x = Parser (\ts -> [(ts, x)])
>
> --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
>   Parser pf <*> Parser px =
>     Parser (\ts -> [ (ts'', f x) | (ts', f)  <- pf ts
>                                  , (ts'', x) <- px ts'])

Derived combinators:

< (<*) :: Applicative f => f a -> f b -> f a
< px <* py = const <$> px <*> py
<
< (*>) :: Applicative f => f a -> f b -> f b
< px *> py = flip const <$> px <*> py
<       -- = id <$ px <*> py
<
< (<**>) :: Applicative f => f a -> f (a -> b) -> f b
< px <**> pf = (flip ($)) <$> px <*> pf




Alternative
===========

Choices between parsers are given by the `Alternative` class. This
class assumes that the given Parser is already `Applicative`.

> instance Alternative Parser where
> --empty :: Parser a
>   empty = Parser (\ts -> [])
>
> --(<|>) :: Parser a -> Parser a -> Parser a
>   Parser px <|> Parser py = Parser (\ts -> px ts ++ py ts)


Derived combinators
-------------------

< some :: Alternative f => f a -> f [a]
< some px = px <:> many px
<
< many :: Alternative f => f a -> f [a]
< many px = some px <|> pure []


Monad
=====

The monad instance allows the value in the result of one parser to
influence the output of the parse.

> instance Monad Parser where
> --return :: a -> Parser a
>   return ofTheJedi = pure ofTheJedi   -- sorry, I couldn't help it.
>
> --(>>=) :: Parser a -> (a -> Parser b) -> Parser b
>   Parser px >>= f = Parser (\ts -> concat [ parse (f x) ts' | (ts', x) <- px ts ])

Derived combinators:

> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = item >>= \t -> if p t then pure t else empty

Or, if you prefer do notation:

< satisfy p = do t <- item
<                if p t then pure t
<                       else empty


> char :: Char -> Parser Char
> char c = satisfy (c ==)

Which, is equivalent to the following:

< char :: Char -> Parser Char
< char c = do t <- item
<             if c == t then pure c
<                       else empty

> oneOf :: [Char] -> Parser Char
> oneOf = satisfy . flip elem
>
> noneOf :: [Char] -> Parser Char
> noneOf cs = satisfy (not . flip elem cs)
>
> string :: String -> Parser String
> string []     = return ""
> string (c:cs) = char c <:> string cs
>
> sepBy  :: Alternative f => f a -> f sep -> f [a]
> sepBy px psep = sepBy1 px psep <|> pure []
>
> sepBy1 :: Alternative f => f a -> f sep -> f [a]
> sepBy1 px psep = px <:> (many (psep *> px))
>
> (<:>) :: Applicative f => f a -> f [a] -> f [a]
> px <:> pxs = (:) <$> px <*> pxs


There is a try after all, but it is only here to make this work with
code written for other members of the Parsec family.

> try :: Parser a -> Parser a
> try = id


Pronunciation    /prəˌnʌnsɪˈeɪʃ(ə)n/
====================================

Most of the symbols in this file are not easily pronounced, so let's establish
some nomenclature.

    Symbol   Name

    <$>      fmap
    <$       const fmap

    <*>      tie fighter, or just "tie", ap
    <*       left tie,
    *>       right tie,
    <**>     tie bomber, pa

    >>=      bind

    <|>      or

    <:>      lift cons
