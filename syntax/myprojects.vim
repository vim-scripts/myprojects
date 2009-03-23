"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy (fhardy at noparking.net)
" Date:						Fri Mar 20 16:35:28 CET 2009
" Licence:					GPL version 2.0 license
"=============================================================================
if !exists('b:current_syntax')
	syntax match MyProjectsFile '.'
	highlight default MyProjectsFile ctermfg=green guifg=#60ff60 gui=none cterm=none

	syntax match MyProjectsFolder '^\(\t*\)[^\t\n][^\t]*\n\ze\t\1'
	highlight default MyProjectsFolder guifg=cyan ctermfg=cyan

	syntax match MyProjectsSvnDialog '^\[myprojects\]\s.\+$'
	highlight default MyProjectsSvnDialog guifg=white ctermfg=white

	syntax match MyProjectsSvnA '^A.....\s.\+$'
	highlight default MyProjectsSvnA guifg=blue ctermfg=blue

	syntax match MyProjectsSvnC '^\%(C.\|.C\)....\s.\+$'
	highlight default MyProjectsSvnC guifg=magenta ctermfg=magenta

	syntax match MyProjectsSvnD '^D.....\s.\+$'
	highlight default MyProjectsSvnD guifg=red ctermfg=red

	syntax match MyProjectsSvnI '^I.....\s.\+$'
	highlight default MyProjectsSvnI guifg=lightgray ctermfg=lightgray

	syntax match MyProjectsSvnM '^\%(M.\|.M\)....\s.\+$'
	highlight default MyProjectsSvnM guifg=green ctermfg=green

	syntax match MyProjectsSvnR '^(R.....\s.\+$'
	highlight default MyProjectsSvnR guifg=yellow ctermfg=yellow

	syntax match MyProjectsSvnX '^X.....\s.\+$'
	highlight default MyProjectsSvnX guifg=gray ctermfg=gray

	syntax match MyProjectsSvnQuestion '^?.....\s.\+$'
	highlight default MyProjectsSvnQuestion guifg=brown ctermfg=brown

	syntax match MyProjectsSvnExclamation '^!.....\s.\+$'
	highlight default MyProjectsSvnExclamation guifg=purple

	syntax match MyProjectsSvnTilde '^\~.....\s.\+$'
	highlight default MyProjectsSvnTilde guifg=orange

	let b:current_syntax = "myprojects"
endif
