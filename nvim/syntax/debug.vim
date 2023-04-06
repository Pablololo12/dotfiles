" Vim syntax file
" Language: debuglog
" Maintainer: Pablo

if exists("b:current_syntax")
    finish
endif

" Keywords
syn match cycleCount '\d\+' contained
syn match cycle 'CYCLE \d\+' contains=cycleCount

" variable show
syn match value '[^=] \?[^) ]*' contained
syn match variable ' *[^= ]\+\s*=' contained
syn match asignment ' *[^= (]\+\s*= \?[^) ]*' contains=variable,value

" unit printing message
syn match unit '\([^\. ]*\.\)\+[^\. ]\+:'

" For upper case constants
syn match states ' [A-Z][A-Z0-9_]\+ '

syn match hexa '0x[0-9_abcdefABCDEF]\+'

" highlight 
hi def link cycle PreProc
hi def link cycleCount Constant
hi def link value Constant
hi def link variable Type
hi def link unit Special
hi def link states Comment
hi def link hexa Constant
