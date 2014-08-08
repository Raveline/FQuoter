module FQuoter.Templating.TemplateTypes
where

data Cardinality = All | Only Int deriving (Eq, Show)

data TokenNode =
    One [TokenModificator] TokenContent
    | SomeAuthors Cardinality [TokenNode]
    | Or [TokenNode] [TokenNode]
    deriving (Eq, Show)

data TokenContent =
    AuthorInfo TokenAuthorInfo
    | SourceInfo TokenSourceInfo
    | ConstantString String
    deriving (Eq, Show)

data TokenSourceInfo =
    SourceTitle
    | SourceMetadata String
    deriving (Eq, Show)

data TokenAuthorInfo =
    AuthorFirstName
    | AuthorLastName
    | AuthorNickName
    deriving (Eq, Show)

data TokenModificator =
    Capital
    | Initial
    | Italics
    deriving (Eq, Show)
