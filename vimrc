set nocompatible
set nopaste

set shortmess=Ia
set laststatus=1
set cpoptions=$
set nomodeline
set modelines=0
set scrolloff=2
set wildmode=longest,full
set backspace=indent,eol,start
set visualbell

set ignorecase
set smartcase

set expandtab
set nosmarttab
set softtabstop=4
set tabstop=4
set shiftwidth=4

set list
set listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:_

if has("extra_search")
    set hlsearch
    set noincsearch
endif

if has("wildmenu")
    set wildmenu
endif

if has("cmdline_info")
    set ruler
    set showcmd
endif

if has("multi_byte")
    set encoding=utf-8
    set listchars=tab:»\ ,trail:█,extends:>,precedes:<,nbsp:░
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
    au FileType svn,gitcommit setlocal textwidth=78
    au BufNewFile,BufRead *.conf set filetype=conf
endif

if &t_Co > 2 && has("syntax")
    set background=light
    syntax on
endif
