module FQuoter.Updater
where

import qualified Data.Map as Map

import FQuoter.Quote
import FQuoter.Actions
import FQuoter.Parser.ParserTypes
import FQuoter.Serialize.SerializedTypes

applyUpdate :: SerializedType -> Update -> TypeProperty -> ParsedType
applyUpdate (SAuthor a) u (ModifyAuthor p) = modifyAuthor u p a
applyUpdate (SSource _) u (ModifySource p) = modifySource u p
applyUpdate _ _ _ = error "Impossible case."

modifyAuthor :: Update -> AuthorProperty -> Author -> ParsedType
modifyAuthor u p a = PAuthor $ modifyAuthor' u p a
    where
        modifyAuthor' :: Update -> AuthorProperty -> Author -> Author
        modifyAuthor' Set (AuthorFirstName s) au = au { firstName = s }
        modifyAuthor' Set (AuthorLastName s) au = au { lastName = s }
        modifyAuthor' Set (AuthorNickName s) au = au { surname = s }
        modifyAuthor' Delete (AuthorFirstName _) au = au { firstName = Nothing }
        modifyAuthor' Delete (AuthorLastName _) au = au { lastName = Nothing }
        modifyAuthor' Delete (AuthorNickName _) au = au { surname = Nothing }
        modifyAuthor' _ _ _ = error "Add is invalid for authors."

modifySource :: Update -> SourceProperty -> ParsedType
modifySource Set (SourceTitle t) = PSource $ ParserSource t [] Map.empty
modifySource _ _ = error "Invalid case"

modifyQuote :: Update -> QuoteProperty -> Quote -> Quote
modifyQuote Set (QuoteContent c) q = q { content = c }
modifyQuote Set (QuoteComment c) q = q { comment = c }
modifyQuote Set (QuoteLocation l) q = q { location = l }
modifyQuote Set (QuoteTags t) q = q { tags = t }
modifyQuote _ _ _ = error "Invalid case."
