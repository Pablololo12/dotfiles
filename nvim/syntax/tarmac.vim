" Vim syntax file
" Language: tarmac
" Maintainer: Pablo

if exists("b:current_syntax")
    finish
endif

" Keywords
syn match instruction ':\s\+[A-Z0-9.]\+\s'
syn match register '[rxw][0-9]'

" Hexadecimals
syn match hexa '0x[0-9_abcdefABCDEF]\+'

" Constants
syntax keyword typeExec IT ES
highlight link typeExec Keyword

syntax keyword typeSpecu IS
highlight link typeSpecu Special

" highlight 
hi def link instruction Type
hi def link register Statement
hi def link hexa Constant

"PreProc Constant Type Special Comment
