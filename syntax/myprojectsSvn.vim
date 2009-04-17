"=============================================================================
" File:						myprojectsSvn.vim
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Fri Apr 17 09:58:00 CEST 2009
" Licence:					GPL version 2.0 license
"=============================================================================
if !exists('b:current_syntax')
	syn case match

	syntax match MyProjectsSvnA '^A[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnA guifg=blue

	syntax match MyProjectsSvnU '^U[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnU guifg=yellow

	syntax match MyProjectsSvnC '^\%(C[ CM][ L][ +][ S]\?[ KOTB]\?\|[ AKDIMRX?!~]C[ L][ +][ S]\?[ KOTB]\?\)\s[^\s].\+$'
	highlight default MyProjectsSvnC guifg=red

	syntax match MyProjectsSvnD '^D[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnD guifg=magenta

	syntax match MyProjectsSvnI '^I[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnI guifg=gray

	syntax match MyProjectsSvnM '^\%(M[ CM][ L][ +][ S]\?[ KOTB]\?\|[ AKDIMRX?!~]M[ L][ +][ S]\?[ KOTB]\?\)\s[^\s].\+$'
	highlight default MyProjectsSvnM guifg=lightgreen

	syntax match MyProjectsSvnR '^R[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnR guifg=violet

	syntax match MyProjectsSvnX '^X[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnX guifg=gray

	syntax match MyProjectsSvnQuestion '^?[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnQuestion guifg=brown

	syntax match MyProjectsSvnExclamation '^![ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnExclamation guifg=purple

	syntax match MyProjectsSvnTilde '^\~[ CM][ L][ +][ S]\?[ KOTB]\?\s[^\s].\+$'
	highlight default MyProjectsSvnTilde guifg=orange

	let b:current_syntax = "myprojectsSvn"
endif
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
