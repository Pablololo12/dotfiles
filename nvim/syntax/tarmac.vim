" Vim syntax file
" Language: Tarmac
" Maintainer: Pablo

if exists("b:current_syntax")
    finish
endif

" cycles
syn match cyclecount '\d\+' contained
syn match cyctype 'clk\|TIC' contained
syn match cycc '^\d\+ [A-Za-z]\+ ' contains=cyclecount,cyctype

" Keywords
syn match instId '\d\+' contained
syn match instIId '(\d\+)' contains=instId

" unit printing message
syn match unit '\([^\. ]*\.\)\+[^\. ]\+:'

" General registers
syn match register ' [A-Z][A-Z0-9_]\+ '

" Hexadecimal values with pre
syn match hexa '0x[0-9_abcdefABCDEF]\+'

" PC and code
syn match instruction ' [0-9abcdefABCDEF]\+ [0-9abcdefABCDEF]\+ '

" Assembly
syn match assembly '[a-z]\+' contained
syn match tmp '\: [a-z]\+' contains=assembly

" Security level
syn match three 'EL3h_[ns]' contained
syn match two 'EL2h_[sn]' contained
syn match one 'EL1h_[sn]' contained
syn match zero 'EL0h_[sn]' contained
syn match seclevel '[\_0-9A-Za-z]\+ \:' contains=three,two,one,zero

" highlight 
hi def link instId PreProc
hi def link cyclecount Constant
hi def link cyctype Comment
hi def link instruction Special
hi def link assembly Type
hi def link three Type
hi def link two Todo
hi def link one Statement
hi def link zero Constant
hi def link unit Special
hi def link register Special
hi def link hexa Constant
