" maxtab.vim
" Author: Raghu V. Rao <raghu.v.rao@gmail.com>

function! maxtab#MaximizeToTab() abort
  if winnr('$') == 1
    echo 'Cannot maximize: window already maximized'
    return
  endif
  let l:destTabNum = -1
  let l:curBufNum = bufnr('%')
  " Find the tab page where the current buffer is the only one
  " displayed.  If it exists, we will simply switch to that tab page
  " instead of creating a new one.
  for l:i in range(tabpagenr('$'))
    " range() starts from 0, tab pages are numbered starting from 1.
    let l:tabNum = l:i + 1
    if tabpagewinnr(l:tabNum, '$') == 1 && tabpagebuflist(l:tabNum)[0] == l:curBufNum
      let l:destTabNum = l:tabNum
      break
    endif
  endfor
  let l:winID = win_getid()
  let l:lineNum = line('.')
  let l:colNum = col('.')
  if l:destTabNum > -1
    execute 'tabnext ' . l:destTabNum
    call cursor(l:lineNum, l:colNum)
  else
    tab split
  endif
  let w:maximizedFromWinID = l:winID
  echo ''
endfunction

function! maxtab#UnmaximizeFromTab() abort
  let l:numWindowsInTab = winnr('$')
  if l:numWindowsInTab > 1
    echo 'Cannot unmaximize: window not maximized'
    return
  endif
  if tabpagenr('$') == 1 && l:numWindowsInTab == 1
    echo 'Cannot unmaximize: only window in only tab'
    return
  endif
  if !exists('w:maximizedFromWinID') || type(w:maximizedFromWinID) != type(win_getid())
    echo 'Cannot unmaximize: origin window unknown'
    return
  endif
  let l:bufNrInOriginWin = winbufnr(w:maximizedFromWinID)
  if l:bufNrInOriginWin == -1
    echo 'Cannot unmaximize: origin window (ID ' . w:maximizedFromWinID . ') does not exist'
    return
  endif
  let l:thisBufNr = bufnr('%')
  if l:bufNrInOriginWin != l:thisBufNr
    echo 'Cannot unmaximize: expected buffer '
          \ . l:thisBufNr
          \ . ' (this buffer) in origin window (ID '
          \ . w:maximizedFromWinID
          \ . '), found buffer '
          \ . l:bufNrInOriginWin
    return
  endif
  let l:maxdTab = tabpagenr()
  let l:lineNum = line('.')
  let l:colNum = col('.')
  if win_gotoid(w:maximizedFromWinID)
    call cursor(l:lineNum, l:colNum)
    execute 'tabclose ' . l:maxdTab
    echo ''
  else
    " Probably no way to reach here.  The check above for winbufnr()
    " returning -1 will catch the case where the origin window does not
    " exist.
    echo 'Cannot unmaximize: origin window not found'
    return
  endif
endfunction
