" Disable macro-recording by unbinding it from 'q'.  I am always misfiring.
map q <Nop>

set nocompatible
set nopaste

set shortmess=Ia
set laststatus=1
set cpoptions=$
set nomodeline
set modelines=0
set scrolloff=2
set backspace=indent,eol,start
set visualbell

set expandtab
set nosmarttab
set softtabstop=4
set tabstop=8
set shiftwidth=4

set ignorecase
set smartcase
if has("extra_search")
    set noincsearch
endif

set wildmode=longest,full
if has("wildmenu")
    set wildmenu
endif

if has("cmdline_info")
    set ruler
    set showcmd
endif

set list
set listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:_
if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:·\ ,trail:█,extends:>,precedes:<,nbsp:░
endif

if has("eval")
    let g:loaded_matchparen = 1
endif

if has("folding")
    set foldmethod=indent
    set foldlevel=100
endif

if has("autocmd") && has("smartindent")
    set autoindent
    set smartindent
    filetype plugin indent on
    au FileType svn,gitcommit setlocal textwidth=80
    au BufRead,BufNewFile *.conf set filetype=conf
    au BufRead,BufNewFile *.py set textwidth=160
endif

if &t_Co > 2 && has("syntax")
    set background=dark
    syntax on
    if has("extra_search")
        set hlsearch
    endif
endif
