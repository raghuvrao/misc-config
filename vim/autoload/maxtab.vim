vim9script

# maxtab.vim
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

export def MaximizeToTab()
    if winnr('$') == 1
        echo 'Cannot maximize: window already maximized'
        return
    endif
    var destTabNum = -1
    var curBufNum = bufnr('%')
    # Find the tab page where the current buffer is the only one
    # displayed.  If it exists, we will simply switch to that tab page
    # instead of creating a new one.
    for i in range(tabpagenr('$'))
        # range() starts from 0, tab pages are numbered starting from 1.
        var tabNum = i + 1
        if tabpagewinnr(tabNum, '$') == 1 && tabpagebuflist(tabNum)[0] == curBufNum
            destTabNum = tabNum
            break
        endif
    endfor
    var winID = win_getid()
    var lineNum = line('.')
    var colNum = col('.')
    if destTabNum > -1
        execute 'tabnext ' .. destTabNum
        call cursor(lineNum, colNum)
    else
        tab split
    endif
    w:maximizedFromWinID = winID
    echo ''
enddef

export def UnmaximizeFromTab()
    var numWindowsInTab = winnr('$')
    if numWindowsInTab > 1
        echo 'Cannot unmaximize: window not maximized'
        return
    endif
    if tabpagenr('$') == 1 && numWindowsInTab == 1
        echo 'Cannot unmaximize: only window in only tab'
        return
    endif
    if !exists('w:maximizedFromWinID') || type(w:maximizedFromWinID) != type(win_getid())
        echo 'Cannot unmaximize: origin window unknown'
        return
    endif
    var bufNrInOriginWin = winbufnr(w:maximizedFromWinID)
    if bufNrInOriginWin == -1
        echo 'Cannot unmaximize: origin window (ID ' .. w:maximizedFromWinID .. ') does not exist'
        return
    endif
    var thisBufNr = bufnr('%')
    if bufNrInOriginWin != thisBufNr
        echo 'Cannot unmaximize: expected buffer '
                    \ .. thisBufNr
                    \ .. ' (this buffer) in origin window (ID '
                    \ .. w:maximizedFromWinID
                    \ .. '), found buffer '
                    \ .. bufNrInOriginWin
        return
    endif
    var maxdTab = tabpagenr()
    var lineNum = line('.')
    var colNum = col('.')
    if win_gotoid(w:maximizedFromWinID)
        call cursor(lineNum, colNum)
        execute 'tabclose ' .. maxdTab
        echo ''
    else
        # Probably no way to reach here.  The check above for winbufnr()
        # returning -1 will catch the case where the origin window does
        # not exist.
        echo 'Cannot unmaximize: origin window not found'
        return
    endif
enddef

export def ToggleMaximizeToTab()
    if winnr('$') > 1
        call MaximizeToTab()
    else
        call UnmaximizeFromTab()
    endif
enddef
