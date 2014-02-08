"set cursorline
"set cursorcolumn
set shortmess=Ilmnrx
set statusline=[%{&ff}]\ %f%m%r%=[%l/%L,%c]
set laststatus=2
"set nowrap
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

"filetype indent on
"set autoindent
"set paste

set expandtab
set smarttab
set softtabstop=4
set tabstop=4
set shiftwidth=4

syntax on
set background=light
hi StatusLine ctermbg=NONE guibg=NONE ctermfg=NONE guifg=NONE cterm=reverse gui=reverse term=reverse
hi Visual ctermbg=DarkBlue guibg=DarkBlue ctermfg=Gray guifg=Gray cterm=NONE gui=NONE term=reverse
