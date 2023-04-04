set nocompatible
syntax on
filetype plugin on

set term=xterm-256color
set number
set relativenumber
set autoindent
set smartindent
set encoding=utf-8
set t_Co=256
set background=dark
set hlsearch
set colorcolumn=120
set tabstop=4
set shiftwidth=4
set expandtab
set cindent
set incsearch
set wildmenu
set wildmode=longest:list,full
set completeopt=longest,menuone

" Tab navigation like Firefox.
nnoremap <S-Tab> :tabprevious<CR>
nnoremap <Tab>   :tabnext<CR>
nnoremap <C-t>   :tabnew<CR>

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
set splitbelow
set splitright

" Line
set noshowmode

function Debug()
	packadd termdebug
	Termdebug()
endfunction

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
hi User2 ctermbg=Green ctermfg=Black "White "Black
hi User3 ctermbg=Black "White "Black
hi User4 ctermbg=Magenta ctermfg=Black "White "Black

au BufNewFile,BufRead *.py,*.hs
	\ set tabstop=2 |
	\ set softtabstop=2 |
	\ set shiftwidth=2 |
	\ set textwidth=120 |
	\ set expandtab |
	\ set autoindent |
	\ set fileformat=unix

