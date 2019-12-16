setlocal shiftwidth=2
setlocal softtabstop=-1

if !has('patch-8.0.1290')
  setlocal keywordprg=:help
endif
