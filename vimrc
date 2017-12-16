set mouse=
set nobackup
if has("persistent_undo") | set noundofile | endif
set nomodeline modelines=0
set backspace=indent,eol,start
set lazyredraw
let &softtabstop=&tabstop
set ignorecase smartcase
set nowrap sidescroll=1
nnoremap <Leader>w :set invwrap<CR>
if exists("+linebreak") | set linebreak | endif
if has("wildmenu") | set nowildmenu wildmode=longest,list,full | endif
if has("cmdline_info") | set ruler showcmd | endif
if has("folding")
  set foldmethod=indent foldlevel=100
  if has("windows")
    set fillchars=vert:\|,stl:-,stlnc:\ ,fold:-,diff:-
  endif
endif
if has("extra_search")
  set incsearch
  nnoremap <Leader>\ :nohlsearch<CR>
endif
set list listchars=tab:>\ ,trail:#,extends:>,precedes:<,nbsp:%
if has("multi_byte")
  set encoding=utf-8
  set listchars=tab:·\ ,trail:¬,extends:>,precedes:<,nbsp:%
endif
set laststatus=2
if exists("+statusline")
  set statusline=%(\ %f%M%)%(\ %h%r%w%)%(\ %l,%c%V\ %)
endif
set textwidth=0
if exists("+autochdir") | set autochdir | endif

if has("autocmd")
  filetype plugin indent on
  augroup raghu_augroup
    au!
    au BufWinEnter,BufNewFile * setlocal formatoptions-=c
    au BufWinEnter,BufNewFile * setlocal formatoptions-=o
    au BufWinEnter,BufNewFile * setlocal formatoptions-=r
    au BufWinEnter,BufNewFile * setlocal formatoptions-=t
    au BufWinEnter,BufNewFile * setlocal formatoptions+=1jlmnq
    au FileType json
          \ setlocal expandtab softtabstop=2 shiftwidth=2
    au FileType sh
          \ setlocal expandtab softtabstop=2 shiftwidth=2
    au FileType vim
          \ setlocal expandtab softtabstop=2 shiftwidth=2 keywordprg=:help
  augroup END
endif

" When running vim in tmux, allow C-<arrow> to work, useful in the cmdline and
" in insert mode.
if &term =~ '^screen'
  " tmux sends xterm-style key sequences when its xterm-keys option is on.
  " Need `execute' because we want \e to be parsed into literal Esc before
  " being assigned.
  execute "set <xUp>=\e[1;*A"
  execute "set <xDown>=\e[1;*B"
  execute "set <xRight>=\e[1;*C"
  execute "set <xLeft>=\e[1;*D"
endif

nnoremap <Leader>e :silent edit<CR>
nnoremap <Leader>tu i<CR><Esc>k:put =strftime('%s')<CR>kJJ
nnoremap <Leader>ts i<CR><Esc>k:put =strftime('%Y-%m-%d %H:%M:%S %Z(UTC%z)')<CR>kJJ
nnoremap <Leader>m :set invmodifiable<CR>

function! ToggleSyntaxHighlight()
  if has("syntax")
    if exists("g:syntax_on")
      syntax off
    else
      syntax on
    endif
  endif
endfunction
nnoremap <Leader>P :call ToggleSyntaxHighlight()<CR>

set background=light
if has("syntax") | syntax off | endif

let g:loaded_matchparen=1
packadd! matchit

" Make vim more secure.  See `:h trojan-horse'.
set noexrc secure
