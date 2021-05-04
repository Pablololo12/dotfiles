set nocompatible
syntax on
filetype plugin on

set number
set relativenumber
set ruler
set autoindent
set smartindent
set encoding=utf-8
set t_Co=256
set background=dark
set hlsearch
set colorcolumn=81
set mouse=a

set tabstop=4
set shiftwidth=4
set noexpandtab
set cindent

set wildmenu
set completeopt=longest,menuone

" Tab navigation like Firefox.
nnoremap <S-Tab> :tabprevious<CR>
nnoremap <Tab>   :tabnext<CR>
nnoremap <C-t>   :tabnew<CR>
map <C-n> :Vexplore<CR>

" FileExplorer
let g:netrw_banner=0        " disable annoying banner
let g:netrw_browse_split=4  " open in prior window
let g:netrw_altv=1          " open splits to the right
let g:netrw_liststyle=3     " tree view
let g:netrw_winsize=15

" Treat visual lines as lines
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Line
set noshowmode

function! GetMode()
	let m = mode()
	if m=='i'
		return 'Insert'
	elseif m=='n'
		return 'Normal'
	elseif m=='R'
		return 'Replace'
	elseif m==?'c'
		return 'Command'
	else
		return 'Visual'
	endif
endfunction

set laststatus=2
set statusline=
set statusline+=%5*\ %{GetMode()}
set statusline+=\ %1*\ \[%n]							"Buffernumber
set statusline+=\ %2*\ %<%f								"Filepath
set statusline+=%m										"modified flag
set statusline+=\ %3*%=									"Space
set statusline+=%{''.(&fenc!=''?&fenc:&enc).''}\ \|		"Encoding
set statusline+=\ %{&ff}\ \|							"Encoding2
set statusline+=\ %{&expandtab?'spaces':'tabs'}\ \|		"Check use of spaces or tabs
set statusline+=\ %Y									"Filetype
set statusline+=\ %4*\ %c:%l/%L							"Col:line/numlines
set statusline+=\ 										"Blank space at the end

hi User1 ctermbg=Blue
hi User2 ctermbg=Green ctermfg=Black
hi User3 ctermbg=Black
hi User4 ctermbg=Magenta ctermfg=Black

au BufNewFile,BufRead *.py,*.hs
	\ set tabstop=2 |
	\ set softtabstop=2 |
	\ set shiftwidth=2 |
	\ set textwidth=80 |
	\ set expandtab |
	\ set autoindent |
	\ set fileformat=unix

au BufNewFile,BufRead *.tex
	\ set colorcolumn=0
