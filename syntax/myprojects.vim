"=============================================================================
" File:        myprojects.vim
" Author:      Frédéric Hardy (fhardy at noparking.net)
" Last Change: Mon Mar  9 17:51:01 CET 2009
" Licence:     GPL version 2.0 license
"=============================================================================
if !exists('b:current_syntax')
	syntax match MyProjectsFile '.'
	highlight default MyProjectsFile ctermfg=green guifg=#60ff60 gui=none cterm=none

	syntax match MyProjectsFolder '^\(\t*\)[^\t\n][^\t]*\n\ze\t\1'
	highlight default MyProjectsFolder guifg=cyan ctermfg=cyan

	let b:current_syntax = "myprojects"
endif
