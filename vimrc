set nobackup
if has("persistent_undo") | set noundofile | endif
set nocompatible
set nopaste
set novisualbell

set nomodeline
set modelines=0

set shortmess=I
set laststatus=1
set cpoptions=$
set backspace=indent,eol,start
set lazyredraw

set noexpandtab
set nosmarttab
set softtabstop=8
set tabstop=8
set shiftwidth=8

set ignorecase
set smartcase

set wildmode=longest,full
if has("wildmenu") | set wildmenu | endif

set autoindent
if has("smartindent") | set nosmartindent | endif

if has("cmdline_info") | set ruler showcmd | endif
if has("eval") | let g:loaded_matchparen = 1 | endif
if has("folding") | set foldmethod=indent foldlevel=100 | endif
if has("extra_search") | set hlsearch noincsearch | endif

set list
set listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:_
if has("multi_byte")
	set encoding=utf-8
	set listchars=tab:·\ ,trail:█,extends:>,precedes:<,nbsp:░
endif

set textwidth=78
if has("autocmd")
	filetype plugin indent on
	augroup raghu_augroup
		au!
		au BufWinEnter,BufRead,BufNewFile * setlocal formatoptions=qnm1

		au BufWinEnter,BufRead,BufNewFile *.go if has("smartindent") | setlocal smartindent | endif
		au BufWinEnter,BufRead,BufNewFile *.go setlocal filetype=go textwidth=0
		au BufWinEnter,BufRead,BufNewFile *.go setlocal list listchars=tab:\ \ ,trail:#,extends:>,precedes:<,nbsp:_
		au BufWinEnter,BufRead,BufNewFile *.go if has("multi_byte") | set listchars=tab:\ \ ,trail:█,extends:>,precedes:<,nbsp:░ | endif

		au FileType sh setlocal expandtab softtabstop=2 shiftwidth=2
		au FileType *.sh,*.bash setlocal expandtab softtabstop=2 shiftwidth=2
	augroup END
endif

" Disable automatic syntax highlighting for all files!
syntax off

" Make vim more secure.  See |trojan-horse|.
set noexrc
set secure
