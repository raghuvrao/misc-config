set nocompatible
set laststatus=2
set nobackup
if has("persistent_undo") | set noundofile | endif
set nomodeline modelines=0
set backspace=indent,eol,start
set lazyredraw
set softtabstop=8  " Matches default values of tabstop and shiftwidth.
set ignorecase smartcase
set nowrap sidescroll=1 sidescrolloff=3
if has("wildmenu") | set nowildmenu wildmode=longest,list,full | endif
set autoindent
if has("smartindent") | set nosmartindent | endif
if has("cmdline_info") | set ruler showcmd | endif
if has("folding") | set foldmethod=indent foldlevel=100 | endif
if has("extra_search") | set hlsearch incsearch | endif
set list
if has("multi_byte")
  set encoding=utf-8 listchars=tab:→\ ,trail:■,extends:▶,precedes:◀,nbsp:░
else
  set listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:_
endif
set textwidth=0

if has("autocmd")
  filetype plugin indent on
  augroup raghu_augroup
    au!
    au BufWinEnter,BufRead,BufNewFile * setlocal formatoptions=qnm1
    au FileType python setlocal expandtab softtabstop=4 shiftwidth=4
    au FileType sh,vim setlocal expandtab softtabstop=2 shiftwidth=2
    au FileType *.sh,*.bash,*.vim setlocal expandtab softtabstop=2 shiftwidth=2
  augroup END
endif

nnoremap <Leader>\ :nohlsearch<CR>
nnoremap <Leader>e :silent edit<CR>

" Disable automatic syntax highlighting in all files!
if has("syntax") | syntax off | endif

" Make vim more secure.  See |trojan-horse|.
set noexrc secure
