"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Mar 20 16:35:28 CET 2009
" Licence:					GPL version 2.0 license
"=============================================================================
if !exists('b:current_syntax')
	syn case match

	syntax match MyProjectsFile '^\(\t*\)\zs[^\t\n][^\t]*\n\ze'
	highlight default MyProjectsFile guifg=lightgreen

	syntax match MyProjectsFolder '^\(\t*\)\zs[^\t\n][^\t]*\n\ze\t\1'
	highlight default MyProjectsFolder guifg=lightblue

	let b:current_syntax = "myprojects"
endif
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
