" Vim syntax file
" INFO: http://www.softpanorama.org/Editors/Vimorama/vim_regular_expressions.shtml
" Language: Zuse
" Maintainer: Simon Kunz <simon.kunz21@gmail.com>
" Latest Revision: 12.05.2021

if exists("b:current_syntax")
  finish
endif
syntax clear
let b:current_syntax = "zuse"


"
" Conditionals
"
"
syn keyword zuseIf if
syn keyword zuseElse else
syntax keyword zuseMatch match

hi def link zuseIf zuseConditional
hi def link zuseElse zuseConditional
hi def link zuseMatch zuseConditional

"
" Repeat
"

syn keyword zuseLoop loop
syn keyword zuseWhile while
syn keyword zuseFor for

hi def link zuseLoop zuseRepeat
hi def link zuseWhile zuseRepeat
hi def link zuseFor zuseRepeat

"
" other keywords 
"

syn keyword zuseAsync async
syn keyword zuseAwait await
syn keyword zuseBreak break
syn keyword zuseContinue continue
syn keyword zuseDyn dyn
syn keyword zuseMod mod
syn keyword zuseMove move
syn keyword zuseMut mut
syn keyword zusePub pub
syn keyword zuseReturn return
syn keyword zuseUnsafe unsafe
syn keyword zuseWhere where
syn keyword zuseConst const
syn keyword zuseMacroKW macro

hi def link zuseAsync zuseKeyword
hi def link zuseAwait zuseKeyword
hi def link zuseBreak zuseKeyword
hi def link zuseContinue zuseKeyword
hi def link zuseDyn zuseKeyword
hi def link zuseMod zuseKeyword
hi def link zuseMove zuseKeyword
hi def link zuseMut StorageClass
hi def link zusePub zuseKeyword
hi def link zuseReturn zuseKeyword
hi def link zuseUnsafe zuseKeyword
hi def link zuseWhere zuseKeyword
hi def link zuseConst zuseKeyword
hi def link zuseMacroKW zuseKeyword

"
" Booleans
"

syn keyword zuseTrue true
syn keyword zuseFalse false

hi def link zuseTrue zuseBoolean
hi def link zuseFalse zuseBoolean

"
" Identifier definitions
"

syn keyword zuseSelfValue self

syn match zuseNormalIdent '[a-z]\w*' contained display
syn match zuseUnusedIdent '_\+[a-z]\w*' contained display


hi def link zuseNormalIdent zuseIdent
hi def link zuseUnusedIdent zuseIdent

hi def link zuseIdentDef zuseIdent
hi def link zuseUnusedIdentDef zuseIdent
hi def link zuseSelfValue zuseIdent

syn match zuseGenericIdent '\$\w\+' display contained
hi def link zuseGenericIdent Special
syn cluster zuseIdent contains=zuseNormalIdent,zuseUnusedIdent,zuseGenericIdent

"
" Strings
"

syn region zuseString
			\ matchgroup=zuseQuote
			\ start='b\?"'
            \ skip='\\"'
            \ end='"'
            \ contains=@Spell

"
" Field access
"

" syn match zuseFieldAccess '\v(\.)@<=[a-z][a-z0-9_]*>(\()@!'



"
" Functions
"



syn match zuseFuncName "\w\+\s*::" display contained contains=zusePathSep
syn match zuseFuncInput '(.*)' contained contains=@zuseType,@zuseIdent,zuseRefMut

syn match zuseFuncHeader '\w\+\s*::\s*(\%([^)]*\))' contains=zuseFuncName,zusePathSep,zuseFuncInput

"
" Type specifiers
"
syn keyword zuseStruct struct
syn keyword zuseEnum enum
syn keyword zuseAlias type

hi def link zuseStruct zuseStructureKW
hi def link zuseEnum zuseStructureKW
hi def link zuseAlias zuseStructureKW

"
" Type definitions
"

syn keyword zuseSelfType Self

syn match zuseTypeName '\<_*[A-Z][a-zA-Z0-9_]*\>' display

syn cluster zuseType contains=zuseSelfType,zuseTypeDef,zuseBooleanType,zuseSignedType,zuseUnsignedType,zuseFloatType,zuseStrType

syn match zuseEnumHeader "\<_*[A-Z][a-zA-Z0-9_]*\>\s*::\s*enum\s*" contained contains=zuseTypeName nextgroup=zuseEnumBody
syn match zuseEnumVariant "\<_*[A-Z][a-zA-Z0-9_]*\>" contained display


"
" Builtin types
"

syn keyword zuseBooleanType bool

syn keyword zuseUnsignedType u1 u2 u4 u8 u16 u32 u64 u128 u256 u512
syn keyword zuseSignedType i2 i4 u8 i16 i32 i64 i128 i256 i512

syn keyword zuseFloatType f16 f32 f64 f128 f256 f512

syn keyword zuseStrType str

hi def link zuseBooleanType zuseType
hi def link zuseUnsignedType zuseType
hi def link zuseSignedType zuseType
hi def link zuseFloatType zuseType
hi def link zuseStrType zuseType

"
" Constants
"

syn match zuseConstant '\<_*[A-Z][A-Z0-9_]*\>' display

"
" Function definitions
"

"     hi def link zuseFuncHeader zuseFunc



"
" Directives
"

syn match zuseDirective '#[a-z_]*'
syn match zuseDirective '#\[[a-z_]*\]'
syn match zuseDirective '#\[[a-z_]*(.*)\]' contains=zuseNumber,zuseFloat

"
" Tags
"

syn match zuseTag "@[A-Z][a-zA-Z0-9_]*" 
syn match zuseTag "@[A-Z][a-zA-Z0-9_]*(.*)" 

hi def link zuseTag PreProc

"
" Characters
"

"syn match zuseCharacter "'.'"

"
" Delimiters
"

syn match zuseDelimiter '[(){}\[\]|\.,:;]\+' display

"
" Operators
"

syn match zuseOperator '+' display
syn match zuseOperator '\*' display
syn match zuseOperator '\\' display
syn match zuseOperator '-' display

syn match zuseOperator '%' display

syn match zuseOperator '|' display
syn match zuseOperator '&' display
syn match zuseOperator '\^' display
syn match zuseOperator '||' display
syn match zuseOperator '&&' display
syn match zuseOperator '^^' display

syn match zuseOperator '+=' display
syn match zuseOperator '\*=' display
syn match zuseOperator '\\=' display
syn match zuseOperator '-=' display

syn match zuseOperator '%=' display

syn match zuseOperator '|=' display
syn match zuseOperator '&=' display
syn match zuseOperator '^=' display

syn match zuseOperator '==' display
syn match zuseOperator '!=' display

syn match zuseRangeExclusive '\.\.' display
syn match zuseRangeInclusive '\.\.=' display

syn match zuseDefine ':='


hi def link zuseRangeInclusive zuseOperator
hi def link zuseRangeExclusive zuseOperator
hi def link zuseDefine zuseOperator

"
" Arrows
"
syn match zuseFatArrow '=>' display
syn match zuseThinArrow '->' display

" We highlight mutable references separately as an operator because otherwise
" they would be recognised as the ‘mut’ keyword, thus whatever comes after the
" ‘mut’ is highlighted as an identifier definition.
syntax match zuseRefMut '&mut' display

hi def link zuseRefMut Keyword


"
" Comments
"

syn keyword zuseTodo TODO INFO FIXME NOCHECKIN XXX NOTE SAFETY NB contained
syn match zuseComment "\v//.*$" contains=zuseTodo

syn region zuseComment start="/\*" end="\*/" contains=zuseTodo

"
" Numbers
"

syn match zuseNumber '\<[[:digit:]][[:digit:]]*\(\(u|i\)\(size|8|16|32|64|128|256|512\)\)\?' display
syn match zuseFloat '\<[[:digit:]][[:digit:]]*\.[[:digit:]]\+\(f\(16|32|64|128|256|512\)\)\?' display


"
" Macros
"

syn match zuseMacro '\v<[a-z][a-z0-9_]*!' display

"
" Other
"

syn match zusePathSep "::" display contained

"
" Blocks
"

"syn region zuseEnumBody start="{" end="}" fold transparent contained contains=zuseEnumVariant
syn region zuseFuncBody start="{" end="}" fold transparent contains=ALL nextgroup=zuseFuncBody


"
" Default linkages
"

hi def link zuseDirective PreProc
hi def link zuseBoolean Boolean
hi def link zuseCharacter Character
hi def link zuseComment Comment
hi def link zuseTodo Todo
hi def link zuseConditional Conditional
hi def link zuseConstant Constant
hi def link zuseDelimiter Delimiter
hi def link zuseFieldAccess Identifier
hi def link zuseFloat Float
hi def link zuseNumber Number
hi def link zuseMacro Macro
hi def link zuseFuncName Function

hi def link zuseTypeName Type
hi def link zuseEnumVariant Constant

hi def link zuseIdent Identifier
hi def link zuseKeyword Keyword
hi def link zuseOperator Operator
hi def link zuseQuote StringDelimiter
hi def link zuseRepeat Repeat
hi def link zuseString String
hi def link zuseTypeDef Typedef
hi def link zuseType Type
" hi def link zuseStructureKW Keyword
hi def link zuseStructureKW Structure


" Account for the vast majority of colourschemes not highlighting string
" delimiters explicitly.
highlight default link StringDelimiter String

syn sync minlines=200
syn sync maxlines=500
