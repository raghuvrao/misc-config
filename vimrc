" Kludge to prevent vimrc from being read in if vim is started as vi
" (here until I can figure out a cleaner way)
if !has("compatible")

    set nocompatible
    set encoding=utf-8

    set nopaste
    filetype plugin indent on
    set autoindent
    set smartindent

    set shortmess=Ia
    set ruler
    set laststatus=1
    set cpoptions=$
    set nomodeline
    set modelines=0
    set showcmd
    set scrolloff=2
    set wildmenu
    set wildmode=full
    set backspace=indent,eol,start
    set visualbell

    set hlsearch
    set noincsearch
    set ignorecase
    set smartcase

    set expandtab
    set nosmarttab
    set softtabstop=4
    set tabstop=4
    set shiftwidth=4

    set foldmethod=indent
    set foldlevel=10

    set list
    set listchars=tab:»\ ,trail:█,extends:>,precedes:<,nbsp:░

    if has("autocmd")
        au FileType svn,gitcommit setlocal textwidth=50
    endif

    set background=light
    syntax reset
    hi MatchParen ctermfg=Magenta guifg=Magenta ctermbg=NONE guibg=NONE term=reverse cterm=NONE gui=NONE
    syntax on

endif
