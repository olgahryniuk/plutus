-- editorconfig-checker-disable-file
{-# LANGUAGE OverloadedStrings #-}

-- | Parsers for PLC terms in DefaultUni.

module PlutusCore.Parser
    ( module Export
    , program
    , parseProgram
    , parseTerm
    , parseType
    , SourcePos
    , ParserError(..)
    ) where

import PlutusCore.Core (Program (..), Term (..), Type)
import PlutusCore.Default
import PlutusCore.Error (AsParserErrorBundle, ParserError (..))
import PlutusCore.MkPlc (mkIterApp, mkIterInst)
import PlutusCore.Name (Name, TyName)
import PlutusCore.Parser.Builtin as Export
import PlutusCore.Parser.ParserCommon as Export
import PlutusCore.Parser.Type as Export
import PlutusCore.Quote (MonadQuote)

import Control.Monad.Except (MonadError)

import Data.Text (Text)

import Text.Megaparsec (MonadParsec (notFollowedBy), SourcePos, anySingle, choice, getSourcePos, many, some, try)

-- | A parsable PLC term.
type PTerm = Term TyName Name DefaultUni DefaultFun SourcePos

varTerm :: Parser PTerm
varTerm = Var <$> getSourcePos <*> name

tyAbsTerm :: Parser PTerm
tyAbsTerm = inParens $ TyAbs <$> wordPos "abs" <*> tyName  <*> kind <*> term

lamTerm :: Parser PTerm
lamTerm = inParens $ LamAbs <$> wordPos "lam" <*> name <*> pType <*> term

appTerm :: Parser PTerm
appTerm = do
    pos <- getSourcePos
    inBrackets $ mkIterApp <$> pure pos <*> term <*> some term

conTerm :: Parser PTerm
conTerm = inParens $ Constant <$> wordPos "con" <*> constant

builtinTerm :: Parser PTerm
builtinTerm = inParens $ Builtin <$> wordPos "builtin" <*> builtinFunction

tyInstTerm :: Parser PTerm
tyInstTerm = do
    pos <- getSourcePos
    inBraces $ do
        tm <- term
        tys <- many pType
        pure $ mkIterInst pos tm tys

unwrapTerm :: Parser PTerm
unwrapTerm = inParens $ Unwrap <$> wordPos "unwrap" <*> term

iwrapTerm :: Parser PTerm
iwrapTerm = inParens $ IWrap <$> wordPos "iwrap" <*> pType <*> pType <*> term

errorTerm
    :: Parser PTerm
errorTerm = inParens $ Error <$> wordPos "error" <*> pType

-- | Parser for all PLC terms.
term :: Parser PTerm
term = choice $ map try
    [ tyAbsTerm
    , lamTerm
    , appTerm
    , conTerm
    , builtinTerm
    , tyInstTerm
    , unwrapTerm
    , iwrapTerm
    , errorTerm
    , varTerm
    ]

-- | Parse a PLC program. The resulting program will have fresh names. The
-- underlying monad must be capable of handling any parse errors.  This passes
-- "test" to the parser as the name of the input stream; to supply a name
-- explicity, use `parse program <name> <input>`.
parseProgram ::
    (AsParserErrorBundle e, MonadError e m, MonadQuote m)
    => Text
    -> m (Program TyName Name DefaultUni DefaultFun SourcePos)
parseProgram = parseGen program

-- | Parser for PLC programs.
program :: Parser (Program TyName Name DefaultUni DefaultFun SourcePos)
program = whitespace >> do
    prog <- inParens $ Program <$> wordPos "program" <*> version <*> term
    notFollowedBy anySingle
    return prog

-- | Parse a PLC term. The resulting program will have fresh names. The underlying monad must be capable
-- of handling any parse errors.
parseTerm :: (AsParserErrorBundle e, MonadError e m, MonadQuote m) =>
    Text -> m (Term TyName Name DefaultUni DefaultFun SourcePos)
parseTerm = parseGen term

-- | Parse a PLC type. The resulting program will have fresh names. The underlying monad must be capable
-- of handling any parse errors.
parseType :: (AsParserErrorBundle e, MonadError e m, MonadQuote m) =>
    Text -> m (Type TyName DefaultUni SourcePos)
parseType = parseGen pType
