vim9script

# synids.vim
# Author: Raghu V. Rao <raghu.v.rao@gmail.com>

export def SynIDs(line_num: number, col_num: number): string
    var id = synID(line_num, col_num, 1)
    var id_name = synIDattr(id, 'name')
    if id_name == ''
        return id_name
    endif
    var final_id = synIDtrans(id)
    if final_id == id
        return id_name
    endif
    return id_name .. ': ' .. synIDattr(final_id, 'name')
enddef
