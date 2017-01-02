" Short-cut to toggle line numbers.
nnoremap <C-a> :set invnumber<CR>
inoremap <C-a> <C-O>:set invnumber<CR>

set nobackup
set noundofile
set nocompatible
set nopaste
set novisualbell

set nomodeline
set modelines=0

set shortmess=I
set laststatus=1
set cpoptions=$
set backspace=indent,eol,start

set noexpandtab
set nosmarttab
set softtabstop=8
set tabstop=8
set shiftwidth=8
set textwidth=78

set ignorecase
set smartcase

set wildmode=longest,full
if has("wildmenu") | set wildmenu | endif

if has("cmdline_info") | set noruler showcmd | endif
if has("eval") | let g:loaded_matchparen = 1 | endif
if has("folding") | set foldmethod=indent foldlevel=100 | endif
if has("extra_search") | set hlsearch noincsearch | endif

set list
set listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:_
if has("multi_byte")
	set encoding=utf-8
	set listchars=tab:·\ ,trail:█,extends:>,precedes:<,nbsp:░
endif

if has("autocmd")
	filetype plugin indent on
	augroup raghu_augroup
		au!
		au BufWinEnter,BufRead,BufNewFile * setlocal formatoptions=qnm1

		au FileType svn,gitcommit if has("cmdline_info") | setlocal ruler | endif
		au BufWinEnter,BufRead,BufNewFile .gitconfig if has("cmdline_info") | setlocal ruler | endif

		au FileType python if has("cmdline_info") | setlocal ruler | endif
		au FileType python setlocal expandtab softtabstop=4 shiftwidth=4 tabstop=8 textwidth=98

		au BufWinEnter,BufRead,BufNewFile *.go if has("cmdline_info") | setlocal ruler | endif
		au BufWinEnter,BufRead,BufNewFile *.go if has("smartindent") | setlocal smartindent | endif
		au BufWinEnter,BufRead,BufNewFile *.go setlocal filetype=go textwidth=0
		au BufWinEnter,BufRead,BufNewFile *.go setlocal list listchars=tab:\ \ ,trail:#,extends:>,precedes:<,nbsp:_
		au BufWinEnter,BufRead,BufNewFile *.go if has("multi_byte") | setlocal listchars=tab:\ \ ,trail:█,extends:>,precedes:<,nbsp:░ | endif

		au FileType sh if has("cmdline_info") | setlocal ruler | endif
		au FileType sh setlocal expandtab softtabstop=4 shiftwidth=4 tabstop=8
		au BufWinEnter,BufRead,BufNewFile *.sh,*.bash if has("cmdline_info") | setlocal ruler | endif
		au BufWinEnter,BufRead,BufNewFile *.sh,*.bash setlocal expandtab softtabstop=4 shiftwidth=4 tabstop=8
	augroup END
endif

" Disable automatic syntax highlighting for all files!
syntax off
