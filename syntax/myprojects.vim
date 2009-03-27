"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy (fhardy at noparking.net)
" Date:						Fri Mar 20 16:35:28 CET 2009
" Licence:					GPL version 2.0 license
"=============================================================================
if !exists('b:current_syntax')
	syn case match

	syntax match MyProjectsFile '.'
	highlight default MyProjectsFile ctermfg=lightgreen guifg=#60ff60 gui=none cterm=none

	syntax match MyProjectsFolder '^\(\t*\)[^\t\n][^\t]*\n\ze\t\1'
	highlight default MyProjectsFolder guifg=cyan ctermfg=cyan

	syntax match MyProjectsSvnDialog '^\[myprojects\]\s[^\s].\+$'
	highlight default MyProjectsSvnDialog guifg=white ctermfg=white

	syntax match MyProjectsSvnA '^A.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnA guifg=blue ctermfg=blue

	syntax match MyProjectsSvnU '^U.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnU guifg=yellow ctermfg=yellow

	syntax match MyProjectsSvnC '^\%(C.\{0,5}\|.C.\{0,4}\)\s[^\s].\+$'
	highlight default MyProjectsSvnC guifg=magenta ctermfg=magenta

	syntax match MyProjectsSvnD '^D.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnD guifg=red ctermfg=red

	syntax match MyProjectsSvnI '^I.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnI guifg=lightgray ctermfg=lightgray

	syntax match MyProjectsSvnM '^\%(M.\{0,5}\|.M.\{0,4}\)\s[^\s].\+$'
	highlight default MyProjectsSvnM guifg=green ctermfg=green

	syntax match MyProjectsSvnR '^(R.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnR guifg=yellow ctermfg=yellow

	syntax match MyProjectsSvnX '^X.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnX guifg=gray ctermfg=gray

	syntax match MyProjectsSvnQuestion '^?.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnQuestion guifg=brown ctermfg=brown

	syntax match MyProjectsSvnExclamation '^!.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnExclamation guifg=purple

	syntax match MyProjectsSvnTilde '^\~.\{0,5}\s[^\s].\+$'
	highlight default MyProjectsSvnTilde guifg=orange

	let b:current_syntax = "myprojects"
endif
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
