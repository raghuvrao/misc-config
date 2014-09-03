let g:loaded_matchparen = 1

set shortmess=Ilmnrx
set statusline=[%{&ff}]%f%M%R\ %=\ %l/%L\ %c
set laststatus=2
set ruler
set cpoptions=$
set nomodeline
set modelines=0
set encoding=utf-8
set showcmd
set scrolloff=2
set wildmenu
set wildmode=full
set backspace=indent,eol,start
set visualbell

set hlsearch
set incsearch
set ignorecase
set smartcase

filetype indent off
set noautoindent
set paste

set expandtab
set smarttab
set softtabstop=4
set tabstop=4
set shiftwidth=4

set foldmethod=indent
set foldlevel=99

map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

syntax off
set list
set listchars=tab:▶\ ,trail:◆
