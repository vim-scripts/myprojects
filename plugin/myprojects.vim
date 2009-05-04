"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy - http://blog.mageekbox.net
" Date:						Mon May  4 15:28:38 CEST 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	2556 10039 :AutoInstall: myprojects.vim
"=============================================================================
" Check version {{{1
if v:version < 700
    echoerr "myprojects.vim requires vim >= 7. DOWNLOAD IT! You'll thank me later."
" Check folding {{{1
elseif !has('folding')
    echoerr "myprojects.vim requires folding."
" Check compatible mode {{{1
elseif &cp
    echoerr "myprojects.vim requires no compatible mode."
" plug-in's code {{{1
elseif !exists('myprojects_enable')
	" Save cpo {{{2
	let s:keepCpo= &cpo
	setlocal cpo&vim

	" Initialize variables {{{2
	" Initialize script variables {{{3
	let s:plugin = 'myprojects'
	let s:version = '0.0.96'
	let s:copyright = '2009'
	let s:author = 'Frédéric Hardy'
	let s:email = 'myprojects.vim@mageekbox.net'
	let s:webSite = 'http://blog.mageekbox.net'
	let s:prompt = '[' . s:plugin . '] '
	let s:oldWidth = 0
	let s:windowsOs = has('win16') || has('win32') || has('win64')
	let s:osSlash = s:windowsOs ? '\' : '/'
	let s:home = expand('$HOME', ':p')
	let s:quitVimIfMyProjectsIsAlone = 1
	let s:refreshProjectBuffers = 1
	let s:loadAttributes = 1
	let s:diffBuffers = []
	let s:preferences = {}

	" Function s:initVariable() {{{3
	function s:initVariable(name, value)
		if !exists(a:name)
			let {a:name} = a:value
		endif
	endfunction

	" Initialize global variables {{{3
	call s:initVariable('g:myprojects_width', 30)
	call s:initVariable('g:myprojects_file', s:home . s:osSlash . '.' . s:plugin)
	call s:initVariable('g:myprojects_tags_file', '.tags')
	call s:initVariable('g:myprojects_auto_close', 0)
	call s:initVariable('g:myprojects_auto_resize', 0)
	call s:initVariable('g:myprojects_auto_open', 1)
	call s:initVariable('g:myprojects_version_at_startup', 1)
	call s:initVariable('g:myprojects_resize_step', 10)
	call s:initVariable('g:myprojects_syntax', 1)
	call s:initVariable('g:myprojects_display_empty_folder', 0)
	call s:initVariable('g:myprojects_version_control_system', 'svn')
	call s:initVariable('g:myprojects_display_number', 0)
	call s:initVariable('g:myprojects_cursorline', 1)
	call s:initVariable('g:myprojects_cursorcolumn', 1)
	call s:initVariable('g:myprojects_foldcolumn', 0)
	call s:initVariable('g:myprojects_display_path_in_statusline', 1)
	call s:initVariable('g:myprojects_tags_generator', '')
	call s:initVariable('g:myprojects_sessions_directory', s:home . s:osSlash . '.vim' . s:osSlash . 'plugin/myprojects/sessions')
	call s:initVariable('g:myprojects_preferences_file', s:home . s:osSlash . '.vim' . s:osSlash . 'plugin/myprojects/preferences')
	call s:initVariable('g:myprojects_new_file_on_bottom', 1)
	call s:initVariable('g:myprojects_sort_svn', 1)
	call s:initVariable('g:myprojects_sort_buffers', 1)
	call s:initVariable('g:myprojects_quit_vim_if_alone', 1)

	" Initialize command {{{2
	command -nargs=? -complete=file MyProjectsToggle call <SID>toggleMyProjectsWindow()

	if !hasmapto('<Plug>MyProjectsToggle')
		map <unique> <silent> <Leader>p <Plug>MyProjectsToggle
	endif

	noremap <unique> <script> <Plug>MyProjectsToggle <SID>toggle
	noremap <SID>toggle :call <SID>toggleMyProjectsWindow()<CR>

	command -nargs=? -complete=file MyProjectsGoTo call <SID>goToMyProjectsWindow()

	if !hasmapto('<Plug>MyProjectsGoTo')
		map <unique> <silent> <Leader>P <Plug>MyProjectsGoTo
	endif

	noremap <unique> <script> <Plug>MyProjectsGoTo <SID>goTo
	noremap <SID>goTo :call <SID>goToMyProjectsWindow()<CR>

	if g:myprojects_auto_open
		autocmd VimEnter * nested call <SID>toggleMyProjectsWindow()
	endif

	" Function s:goToMyProjectsWindow() {{{2
	function s:goToMyProjectsWindow()
		let window = bufwinnr(g:myprojects_file)

		if window == -1
			return 0
		else
			silent execute window . 'wincmd w'
			return 1
		endif
	endfunction

	" Function s:writeMyProjectsPreferences() {{{2
	function s:writeMyProjectsPreferences()
		let preferencesDirectory = fnamemodify(g:myprojects_preferences_file, ':h')
		let type = getftype(preferencesDirectory)

		if type == ''
			call s:mkdir(preferencesDirectory)
		elseif type != 'dir'
			throw 'Path ''' . preferencesDirectory . ''' exists but it is not a directory.'
		endif

		if !filewritable(preferencesDirectory)
			throw 'Unable to write in ''' . preferencesDirectory . '''.'
		else
			call writefile([string(s:preferences)], g:myprojects_preferences_file)
		endif
	endfunction

	" Function s:readMyProjectsPreferences() {{{2
	function s:readMyProjectsPreferences()
		if getftype(g:myprojects_preferences_file) == 'file' && filereadable(g:myprojects_preferences_file)
			let preferencesFileContent = readfile(g:myprojects_preferences_file, '', 1)

			if len(preferencesFileContent) == 1 && preferencesFileContent[0] =~ '^{.*}$'
				execute 'let preferences =  ' . preferencesFileContent[0]

				if type(preferences) == 4
					let s:preferences = preferences
				endif
			endif
		endif
	endfunction

	" Function s:definePreferences()
	function s:definePreferences()
		let type = s:input('Preferences for type: ', '')

		if type == ''
			call s:error('Preferences type must not be empty.')
		else
			call s:readMyProjectsPreferences()

			let mappings = {'1': '', '2': '', '3': '', '4': '', '5': '', '6': '', '7': '', '8': '', '9': '', '10': '', '11': '', '12': ''}

			if !has_key(s:preferences, type)
				let s:preferences[type] = {'path': '', 'cd': '', 'filter': '', 'make': '', 'errorFormat': '', 'test': '', 'mappings': mappings}
			else
				let s:preferences[type]['path'] = !has_key(s:preferences[type], 'path') ? '': s:preferences[type]['path']
				let s:preferences[type]['cd'] = !has_key(s:preferences[type], 'cd') ? '': s:preferences[type]['cd']
				let s:preferences[type]['filter'] = !has_key(s:preferences[type], 'filter') ? '': s:preferences[type]['filter']
				let s:preferences[type]['make'] = !has_key(s:preferences[type], 'make') ? '': s:preferences[type]['make']
				let s:preferences[type]['errorFormat'] = !has_key(s:preferences[type], 'errorFormat') ? '': s:preferences[type]['errorFormat']
				let s:preferences[type]['test'] = !has_key(s:preferences[type], 'test') ? '': s:preferences[type]['test']

				if has_key(s:preferences[type], 'mappings')
					for [key, mapping] in items(s:preferences[type]['mappings'])
						let mappings[key] = mapping
					endfor
				endif
			endif

			let s:preferences[type]['path'] = s:inputRealPath('Path for type ''' . type . ''': ', s:preferences[type]['path'], 0)
			let s:preferences[type]['cd'] = s:inputCd('Working directory for type ''' . type . ''': ', s:preferences[type]['cd'], s:preferences[type]['cd'])
			let s:preferences[type]['filter'] = s:inputFilter('Filter for type ''' . type . ''': ', s:preferences[type]['filter'])
			let s:preferences[type]['make'] = s:inputMake('Make for type ''' . type . ''': ', s:preferences[type]['make'])
			let s:preferences[type]['errorformat'] = s:inputErrorFormat('Error format for type ''' . type . ''': ', s:preferences[type]['errorFormat'])
			let s:preferences[type]['test'] = s:inputTest('Test extension for type ''' . type . ''': ', s:preferences[type]['test'])
			let s:preferences[type]['mappings'] = {'1': '', '2': '', '3': '', '4': '', '5': '', '6': '', '7': '', '8': '', '9': '', '10': '', '11': '', '12': ''}

			let mappings = s:inputMappings('Mappings for type ''' . type . ''': ', mappings)

			for [key, mapping] in items(mappings)
					let s:preferences[type]['mappings'][key] = mapping
			endfor

			call s:message('Save preferences for type ''' . type . '''...')
			call s:writeMyProjectsPreferences()
			call s:message('Preferences saved for type ''' . type . '''.')
		endif
	endfunction

	" Function s:openMyProjectsWindow() {{{2
	function s:openMyProjectsWindow()
		if !s:goToMyProjectsWindow()
			silent execute 'leftabove vertical new'

			call s:moveWindowToTopLeftCorner()
			call s:setWindowWidth(g:myprojects_width)

			let myprojects_file = fnameescape(g:myprojects_file)

			if bufexists(g:myprojects_file)
				silent execute 'buffer ' . myprojects_file
			else
				silent execute 'edit ' . myprojects_file

				nnoremap <silent> <buffer> <LeftMouse> <LeftMouse>:echo<CR>
				nnoremap <silent> <buffer> <S-LeftMouse> <LeftMouse>:echo<CR>
				nnoremap <silent> <buffer> <Return> :call <SID>open('edit')<CR>
				nnoremap <silent> <buffer> <2-Leftmouse> :call <SID>open('edit')<CR>
				nnoremap <silent> <buffer> <S-Return> :call <SID>open('sp')<CR>
				nnoremap <silent> <buffer> <S-2-Leftmouse> :call <SID>open('sp')<CR>
				nnoremap <silent> <buffer> <C-Return> :call <SID>open('vs')<CR>
				nnoremap <silent> <buffer> <C-2-Leftmouse> :call <SID>open('vs')<CR>
				nnoremap <silent> <buffer> <C-Tab> :call <SID>goToAnEditionWindow()<CR>
				nnoremap <silent> <buffer> <C-Right> :call <SID>setWindowWidth(winwidth(0) + g:myprojects_resize_step)<CR>
				nnoremap <silent> <buffer> <C-l> :call <SID>setWindowWidth(winwidth(0) + g:myprojects_resize_step)<CR>
				nnoremap <silent> <buffer> <C-Left> :call <SID>setWindowWidth(winwidth(0) - g:myprojects_resize_step)<CR>
				nnoremap <silent> <buffer> <C-h> :call <SID>setWindowWidth(winwidth(0) - g:myprojects_resize_step)<CR>
				nnoremap <silent> <buffer> <C-Space> :call <SID>toggleFullscreen()<CR>
				nnoremap <silent> <buffer> <LocalLeader>b :call <SID>displayProjectBuffers(<SID>getProjectName(line('.')), <SID>getProjectPath(line('.')), '')<CR>
				nnoremap <silent> <buffer> <LocalLeader>c :call <SID>create(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>r :call <SID>refresh(line('.'), 0)<CR>
				nnoremap <silent> <buffer> <LocalLeader>R :call <SID>refresh(line('.'), 1)<CR>
				nnoremap <silent> <buffer> <LocalLeader>g :call <SID>grep(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>t :call <SID>generateTags(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>e :call <SID>explore('E')<CR>
				nnoremap <silent> <buffer> <LocalLeader>E :call <SID>explore('Se')<CR>
				nnoremap <silent> <buffer> <LocalLeader>a :call <SID>append(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>d :call <SID>delete(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>s :call <SID>saveSession(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>S :call <SID>loadSession(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader><A-s> :call <SID>deleteSession(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>p :call <SID>setPath(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>P :call <SID>updatePath(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>f :call <SID>setFilter(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>F :call <SID>updateFilter(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>w :call <SID>setCd(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>W :call <SID>updateCd(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>m :call <SID>setMappings(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>M :call <SID>updateMappings(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>k :call <SID>setMake(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>K :call <SID>updateMake(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>ef :call <SID>setErrorFormat(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>Ef :call <SID>updateErrorFormat(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>te :call <SID>setTest(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>Te :call <SID>updateTest(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>i :call <SID>echo('Path: ' . <SID>getPath(line('.')))<CR>
				nnoremap <silent> <buffer> <LocalLeader>v :call <SID>echoVersion()<CR>
				nnoremap <silent> <buffer> <LocalLeader>V :call <SID>echoMyprojectsFile()<CR>
				nnoremap <silent> <buffer> <LocalLeader>ss :call <SID>svnStatus(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>su :call <SID>svnUpdate(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sa :call <SID>svnAddStepOne(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sr :call <SID>svnRevertStepOne(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sd :call <SID>svnDiff(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sc :call <SID>svnCommitStepOne(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sb :call <SID>svnBlame(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>si :call <SID>svnInfo(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sC :call <SID>svnCheckout(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>src :call <SID>svnResolve(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>sl :call <SID>svnLog(line('.'))<CR>
				nnoremap <silent> <buffer> <LocalLeader>df :call <SID>definePreferences()<CR>

				if g:myprojects_display_path_in_statusline
					nnoremap <silent> <buffer> <Down> <Down>:call <SID>echoPath()<CR>
					nnoremap <silent> <buffer> j j:call <SID>echoPath()<CR>
					nnoremap <silent> <buffer> <Up> <Up>:call <SID>echoPath()<CR>
					nnoremap <silent> <buffer> k k:call <SID>echoPath()<CR>
					nnoremap <silent> <buffer> <LeftMouse> <LeftMouse>:call <SID>echoPath()<CR>
					nnoremap <silent> <buffer> <S-LeftMouse> <LeftMouse>:call <SID>echoPath()<CR>
				endif

				setlocal autoindent
				setlocal autoread
				setlocal cindent
				setlocal expandtab
				setlocal foldenable
				setlocal foldlevel=0
				setlocal foldmethod=expr
				setlocal nobuflisted
				setlocal noequalalways
				setlocal noexpandtab
				setlocal nolist
				setlocal nomodeline
				setlocal noruler
				setlocal nospell
				setlocal noswapfile
				setlocal nowrap
				setlocal shiftwidth=3
				setlocal splitbelow
				setlocal splitright
				setlocal tabstop=3
				setlocal winfixwidth

				call s:setLocal('number', g:myprojects_display_number)
				call s:setLocal('cursorcolumn', g:myprojects_cursorcolumn)
				call s:setLocal('cursorline', g:myprojects_cursorline)

				abclear <buffer>

				let s:sid = s:sid()

				silent execute 'setlocal statusline=' . escape(s:prompt, ' ') . '%=[%f\ %3p%%]'
				silent execute 'setlocal foldtext=' . s:sid . 'foldtext()'
				silent execute 'setlocal foldexpr=' . s:sid . 'foldexpr()'
				silent execute 'setlocal foldcolumn=' . g:myprojects_foldcolumn

				silent execute 'augroup ' . s:plugin
				silent au!

				if g:myprojects_cursorline
					silent au WinEnter <buffer> set cursorline
				else
					silent au WinEnter <buffer> set nocursorline
				endif

				if g:myprojects_cursorcolumn
					silent au WinEnter <buffer> set cursorcolumn
				else
					silent au WinEnter <buffer> set nocursorcolumn
				endif

				silent execute 'au BufEnter * let &titlestring = ''' . substitute(&titlestring, "'", "''", 'g') . ''''
				silent execute 'au BufRead * call ' . s:sid . 'loadMyProjectsAttributes()'
				silent execute 'au BufEnter <buffer> let &titlestring = ''' . substitute(s:prompt, "'", "''", 'g') . ''''
				silent execute 'au WinEnter ' . g:myprojects_file . ' call ' . s:sid . 'quitVim()'
				silent execute 'au WinEnter ' . s:sid . '* call ' . s:sid . 'quitVim()'
				silent augroup END

				silent execute 'setlocal filetype=' . s:plugin

				if has('syntax') && g:myprojects_syntax
					syntax on
				endif

				if foldlevel(line('.'))
					silent normal! zo
				endif

				if g:myprojects_display_path_in_statusline
					call <SID>echo(<SID>getPath(line('.')))
				endif

				if g:myprojects_version_at_startup
					call s:echoVersion()
				endif

				let &titlestring = s:prompt
			endif
		endif
	endfunction

	" Function s:closeMyProjectsWindow() {{{2
	function s:closeMyProjectsWindow()
		if s:goToMyProjectsWindow()
			hide
			echo
		endif
	endfunction

	" Function s:toggleMyProjectsWindow() {{{2
	function s:toggleMyProjectsWindow()
		if s:goToMyProjectsWindow()
			call s:closeMyProjectsWindow()
		else
			call s:openMyProjectsWindow()
		endif
	endfunction

	" Function s:isOneOfMyProjectsWindow() {{{2
	function s:isOneOfMyProjectsWindow(window)
		let windowBuffer = winbufnr(a:window)

		return windowBuffer == bufnr(g:myprojects_file) ? 1 : bufname(windowBuffer) =~ '^' . s:sid .'.\+$'
	endfunction
	
	" Function s:isAnEditionWindow() {{{2
	function s:isAnEditionWindow(window)
		return !s:isOneOfMyProjectsWindow(a:window) && getbufvar(a:window, '&buftype') == '' && getbufvar(a:window, '&previewwindow') == 0
	endfunction
	
	" Function s:goToAnEditionWindow() {{{2
	function s:goToAnEditionWindow()
		wincmd p

		let window = winnr()

		if !s:isAnEditionWindow(window)
			let window = 1
			let maxWindow = winnr('$')

			while window <= maxWindow
				if !s:isAnEditionWindow(window)
					let window += 1
				else
					silent execute window . 'wincmd w'
					return window
				endif
			endwhile

			let width = &columns - g:myprojects_width

			silent vnew
			call s:setWindowWidth(width)

			return winnr()
		endif
	endfunction

	" Function s:createMyProjectsWindow() {{{2
	function s:createMyProjectsWindow(title, buffer, filetype)
		let buffer = bufnr(s:sid . a:buffer)

		if buffer == -1
			silent execute 'botright new ' . s:sid . a:buffer

			setlocal buftype=nofile
			setlocal nobuflisted
			setlocal nocursorcolumn
			setlocal noexpandtab
			setlocal nolist
			setlocal nomodeline
			setlocal nonumber
			setlocal noruler
			setlocal nospell
			setlocal noswapfile
			setlocal nowrap

			call s:setLocal('cursorline', g:myprojects_cursorline)

			let &titlestring = s:prompt . a:title

			silent execute 'setlocal filetype=' . a:filetype
			silent execute 'setlocal statusline=' . escape(&titlestring, ' ') . '%=[%3p%%]'

			silent execute 'augroup ' . s:plugin

			if g:myprojects_cursorline
				silent au! WinEnter <buffer> set cursorline nocursorcolumn
			else
				silent au! WinEnter <buffer> set nocursorline nocursorcolumn
			endif

			silent execute 'au! BufEnter <buffer> let &titlestring = ''' . substitute(s:prompt . a:title, "'", "''", 'g') . ''''
			silent augroup END

			if has('syntax') && g:myprojects_syntax
				syntax on
			endif

			abclear <buffer>
		else
			let window = bufwinnr(buffer)

			if window != -1
				silent execute window . 'wincmd w'
			else
				silent execute 'botright new ' . s:sid . a:buffer

				setlocal buftype=nofile
				setlocal nobuflisted
				setlocal nocursorcolumn
				setlocal noexpandtab
				setlocal nolist
				setlocal nomodeline
				setlocal nonumber
				setlocal noruler
				setlocal nospell
				setlocal noswapfile
				setlocal nowrap

				call s:setLocal('cursorline', g:myprojects_cursorline)

				let &titlestring = s:prompt . a:title

				silent execute 'setlocal filetype=' . a:filetype
				silent execute 'setlocal statusline=' . escape(&titlestring, ' ') . '%=[%3p%%]'
			endif
		endif

		call s:setWindowHeight(0)

		return buffer == -1
	endfunction

	" Function s:createSvnWindow() {{{2
	function s:createSvnWindow(title)
		call s:createMyProjectsWindow(a:title, 'svn', s:plugin . 'Svn')

		setlocal modifiable
		silent! %d
		setlocal nomodifiable

		call s:setWindowHeight(line('$'))

		redraw

		nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
		nnoremap <buffer> <silent> <Return> :call <SID>openFromSvnWindow('edit')<CR>
		nnoremap <buffer> <silent> <2-Leftmouse> :call <SID>openFromSvnWindow('edit')<CR>
		nnoremap <buffer> <silent> <S-Return> :call <SID>openFromSvnWindow('sp')<CR>
		nnoremap <buffer> <silent> <S-2-Leftmouse> :call <SID>openFromSvnWindow('sp')<CR>
		nnoremap <buffer> <silent> <C-Return> :call <SID>openFromSvnWindow('vs')<CR>
		nnoremap <buffer> <silent> <C-2-Leftmouse> :call <SID>openFromSvnWindow('vs')<CR>
		nnoremap <buffer> <silent> <LocalLeader>sd :call <SID>svnDiffFromSvnWindow()<CR>
	endfunction

	" Function s:createSvnConflictWindow() {{{2
	function s:createSvnConflictWindow(title, path)
		call s:createMyProjectsWindow(a:title, 'svn', s:plugin . 'Svn')

		nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
		silent execute 'noremap <buffer> <silent> <Return> :call <SID>resolveSvnConflict(''edit'', ''' . a:path . ''')<CR>'
		silent execute 'noremap <buffer> <silent> <2-Leftmouse> :call <SID>resolveSvnConflict(''edit'', ''' . a:path . ''')<CR>'
		silent execute 'noremap <buffer> <silent> <S-Return> :call <SID>resolveSvnConflict(''sp'', ''' . a:path . ''')<CR>'
		silent execute 'noremap <buffer> <silent> <S-2-Leftmouse> :call <SID>resolveSvnConflict(''sp'', ''' . a:path . ''')<CR>'
		silent execute 'noremap <buffer> <silent> <C-Return> :call <SID>resolveSvnConflict(''vs'', ''' . a:path . ''')<CR>'
		silent execute 'noremap <buffer> <silent> <C-2-Leftmouse> :call <SID>resolveSvnConflict(''vs'', ''' . a:path . ''')<CR>'
	endfunction

	" Function s:createBufferWindow() {{{2
	function s:createBufferWindow(title, buffer, path)
		let bufferCreated = s:createMyProjectsWindow(a:title, a:buffer, s:plugin)

		if bufferCreated
			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			silent execute 'noremap <buffer> <silent> <Return> :call <SID>openFromBuffersWindow(''edit'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <2-Leftmouse> :call <SID>openFromBuffersWindow(''edit'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <S-Return> :call <SID>openFromBuffersWindow(''sp'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <S-2-Leftmouse> :call <SID>openFromBuffersWindow(''sp'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <C-Return> :call <SID>openFromBuffersWindow(''vs'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <C-2-Leftmouse> :call <SID>openFromBuffersWindow(''vs'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <LocalLeader>sd :call <SID>svnDiffFromBuffersWindow(''' . a:path . ''')<CR>'
		endif

		return bufferCreated
	endfunction


	" Function s:quitVim() {{{2
	function s:quitVim()
		if !g:myprojects_quit_vim_if_alone
			if s:myProjectsIsAlone()
				call s:goToAnEditionWindow()
			endif

			return 0
		elseif !s:quitVimIfMyProjectsIsAlone || !s:myProjectsIsAlone()
			return 0
		else
			confirm qall
			return 1
		endif
	endfunction

	" Function s:myProjectsIsAlone() {{{2
	function s:myProjectsIsAlone()
		let windows = winnr('$')

		while windows > 0
			if s:isAnEditionWindow(windows)
				return 0
			else
				let windows -= 1
			endif
		endwhile

		return 1
	endfunction

	" Function s:toggleFullscreen() {{{2
	function s:toggleFullscreen()
		if s:oldWidth == 0
			let s:oldWidth = winwidth(0)
			call s:setWindowWidth('')
		else
			call s:setWindowWidth(s:oldWidth)
			let s:oldWidth = 0
		endif
	endfunction

	" Function s:foldtext() {{{2
	function s:foldtext()
		let text = repeat(' ', indent(v:foldstart)) . '+ ' . s:getName(v:foldstart) . '-[' . (v:foldend - v:foldstart) . ']'

		if foldclosed(line('.')) == - 1
			let virtcol = virtcol('.')
			let wincol = wincol()

			if virtcol > (wincol - &sidescrolloff)
				let text = strpart(text, virtcol - wincol)
			endif
		endif

		return text
	endfunction

	" Function s:foldexpr() {{{2
	function s:foldexpr()
		let currentIndent = s:indent(v:lnum)
		let nextIndent = s:indent(nextnonblank(v:lnum + 1))
		return currentIndent >= nextIndent ? currentIndent : '>' . nextIndent
	endfunction

	" Function s:isFolder() {{{2
	function s:isFolder(line)
		return s:indent(a:line) < s:indent(nextnonblank(a:line + 1))
	endfunction

	" Function s:getFirstFolderLine() {{{2
	function s:getFirstFolderLine(line)
		let line = 0

		let indent = s:indent(a:line)

		if indent > 0
			let oldPosition = s:setCursorPosition([0, a:line, 1])
			let line = search('^\t\{' . (indent - 1) . '}[^\t]\+$', 'bnW')
			call s:setCursorPosition(oldPosition)
		endif

		return line
	endfunction

	" Function s:getLastFolderLine() {{{2
	function s:getLastFolderLine(line)
		let line = 0

		let firstFolderLine = a:line

		if !s:isFolder(firstFolderLine)
			let firstFolderLine = s:getFirstFolderLine(firstFolderLine)
		endif

		if firstFolderLine > 0
			let oldPosition = s:setCursorPosition([0, firstFolderLine, 1])
			let line = search('^\t\{0,' . s:indent(firstFolderLine) . '\}[^\t].*$', 'nW') - 1
			call s:setCursorPosition(oldPosition)

			if line <= 0
				let line = line('$')
			endif
		endif

		return line
	endfunction

	" Function s:getName() {{{2
	function s:getName(line)
		let name = ''

		let line = getline(a:line)

		if line != -1
			let name = substitute(line, '^\t*\([^\t].\{-}\)\%(\%(\\\)\@<!\%(=\| \).*\|$\)', '\1', '')
		endif

		return name
	endfunction

	" Function s:getPath() {{{2
	function s:getPath(line)
		let path = s:extractPath(a:line)

		if !s:isAbsolutePath(path)
			let folderLine = s:getFirstFolderLine(a:line)

			while folderLine > 0
				let path = s:extractPath(folderLine) . s:osSlash . path
				let folderLine = s:getFirstFolderLine(folderLine)
			endwhile
		endif

		return !s:isAbsolutePath(path) ? '' : resolve(s:unescape(path))
	endfunction

	" Function s:getProjectLine() {{{2
	function s:getProjectLine(line)
		let line = 0

		let indent = indent(a:line)

		if indent >= 0
			let line = indent == 0 ? a:line : search('^[^\t]\+$', 'bnW')
		endif

		return line
	endfunction

	" Function s:getProjectName() {{{2
	function s:getProjectName(line)
		let line = a:line

		while indent(line) > 0
			let line = s:getFirstFolderLine(line)
		endwhile

		return s:getName(line)
	endfunction

	" Function s:getProjectPath() {{{2
	function s:getProjectPath(line)
		let path = ''

		let line = s:getProjectLine(a:line)

		if line > 0
			let path = s:getPath(line)
		endif

		return path
	endfunction

	" Function s:extractAttributeFromLine() {{{2
	function s:extractAttributeFromLine(name, line)
		return !s:hasAttribute(a:name, a:line) ? '' : substitute(getline(a:line), '.*\%(\%(\\\)\@<! \)\+' . a:name . '="\([^"]\{-}\)".*', '\1', '')
	endfunction

	" Function s:extractAttribute() {{{2
	function s:extractAttribute(name, line)
		let attribute = ''

		let line = a:line

		while line > 0 && !s:hasAttribute(a:name, line)
			let line = s:getFirstFolderLine(line)
		endwhile

		if s:hasAttribute(a:name, line)
			let attribute = s:extractAttributeFromLine(a:name, line)
		endif

		return attribute
	endfunction

	" Function s:extractPathFromLine() {{{2
	function s:extractPathFromLine(line)
		let path = ''

		if s:hasPath(a:line)
			let lineContent = substitute(getline(a:line), '^\t*[^\t].\{-}\%(\\\)\@<!\%( \|=\)', '', '')

			let path = substitute(lineContent, '^\(.\{-}\)\%(\\\)\@<! .*$', '\1', '')
		endif

		return path
	endfunction

	" Function s:extractPath() {{{2
	function s:extractPath(line)
		let path = s:extractPathFromLine(a:line)

		if path == ''
			let path = s:getName(a:line)
		endif

		return path
	endfunction

	" Function s:extractCdFromLine() {{{2
	function s:extractCdFromLine(line, resolveDot)
		let cd = s:extractAttributeFromLine('cd', a:line)

		if cd == '.' && a:resolveDot
			let cd = s:getPath(a:line)
		endif

		return cd
	endfunction

	" Function s:extractCd() {{{2
	function s:extractCd(line)
		let line = a:line

		let cd = s:extractCdFromLine(line, 1)

		while cd == '' && line > 0
			let line = s:getFirstFolderLine(line)

			if line > 0
				let cd = s:extractCdFromLine(line, 1)
			endif
		endwhile

		return cd
	endfunction

	" Function s:extractFilterFromLine() {{{2
	function s:extractFilterFromLine(line)
		return s:extractAttributeFromLine('filter', a:line)
	endfunction

	" Function s:extractFilter() {{{2
	function s:extractFilter(line)
		return s:extractAttribute('filter', a:line)
	endfunction

	" Function s:extractMappingsFromLine() {{{2
	function s:extractMappingsFromLine(line)
		let mappings = {}

		let line = getline(a:line)

		if line != ''
			let index = 1

			while index <= 12
				if s:hasMapping('F' . index, a:line)
					let mapping = substitute(line, '.*\s\+F' . index . '="\([^"]\+\)".*', '\1', '')

					if mapping != ''
						let mappings[index] = mapping
					endif
				endif

				let index += 1
			endwhile
		endif

		return mappings
	endfunction

	" Function s:extractMappings() {{{2
	function s:extractMappings(line)
		let mappings = {}

		let key = 1

		while key <= 12
			let mapping = ''

			let line = a:line

			while line > 0 && !s:hasMapping('F' . key, line)
				let line = s:getFirstFolderLine(line)
			endwhile

			if line > 0
				let mapping = s:extractAttributeFromLine('F' . key, line)

				if has_key(mappings, line)
					let mappings[line][key] = mapping
				else
					let mappings[line] = { key : mapping }
				endif
			endif

			let key += 1
		endwhile

		return mappings
	endfunction

	" Function s:extractMakeFromLine() {{{2
	function s:extractMakeFromLine(line)
		return s:extractAttributeFromLine('make', a:line)
	endfunction

	" Function s:extractMake() {{{2
	function s:extractMake(line)
		return s:extractAttribute('make', a:line)
	endfunction

	" Function s:extractErrorFormatFromLine() {{{2
	function s:extractErrorFormatFromLine(line)
		return s:extractAttributeFromLine('errorformat', a:line)
	endfunction

	" Function s:extractErrorFormat() {{{2
	function s:extractErrorFormat(line)
		return s:extractAttribute('errorformat', a:line)
	endfunction

	" Function s:extractTestFromLine() {{{2
	function s:extractTestFromLine(line)
		return s:extractAttributeFromLine('test', a:line)
	endfunction

	" Function s:extractTest() {{{2
	function s:extractTest(line)
		return s:extractAttribute('test', a:line)
	endfunction

	" Function s:extractRefreshFromLine() {{{2
	function s:extractRefreshFromLine(line)
		return s:extractAttributeFromLine('refresh', a:line)
	endfunction

	" Function s:extractRefresh() {{{2
	function s:extractRefresh(line)
		return s:extractAttribute('refresh', a:line)
	endfunction

	" Function s:inputType() {{{2
	function s:inputType(message)
		let type = ''

		return type
	endfunction

	" Function s:inputName() {{{2
	function s:inputName(message)
		let name = s:input(a:message, '')

		if name == ''
			throw 'Name must not be empty.'
		else
			return name
		endif
	endfunction

	" Function s:isAbsolutePath() {{{2
	function s:isAbsolutePath(path)
		return s:windowsOs ? a:path =~ '^.:\(\\\|\/\)' : a:path =~ '^/'
	endfunction

	" Function s:inputPath() {{{2
	function s:inputPath(message, defaultPath, emptyPath)
		let path = s:input(a:message, a:defaultPath, 'file')

		if a:emptyPath == 0 && path == ''
			throw 'Path must not be empty.'
		else
			let path = expand(path, ':p')

			if !s:isAbsolutePath(path)
				throw 'Path must be absolute.'
			else
				return path
			endif
		endif
	endfunction

	" Function s:inputRealPath() {{{2
	function s:inputRealPath(message, defaultPath, emptyPath)
		let path = s:inputPath(a:message, a:defaultPath, a:emptyPath)

		if !s:pathExists(path)
			throw 'Path ''' . path . ''' does not exist.'
		else
			return resolve(path)
		endif
	endfunction

	" Function s:inputCd() {{{2
	function s:inputCd(message, path, value)
		let cd = s:input(a:message, a:value, 'file')

		if cd != ''
			if cd != '.'
				let cd = fnamemodify(s:cleanPath(cd), ':p')

				if cd == a:path
					let cd = '.'
				endif
			endif

			if cd != '.' && getftype(cd) != 'dir'
				throw 'Working directory ' . cd . ' of project ' . a:project['name'] . ' is invalid.'
			else
				let cd = resolve(cd)
			endif
		endif

		return cd
	endfunction

	" Function s:inputFilter() {{{2
	function s:inputFilter(message, value)
		return s:input(a:message, a:value)
	endfunction

	" Function s:inputMappings() {{{2
	function s:inputMappings(message, mappings)
		let mappings = {}

		let inputs = a:mappings

		if empty(inputs)
			let inputs = {1: '', 2: '', 3: '', 4: '', 5: '', 6: '', 7: '', 8: '', 9: '', 10: '', 11: '', 12: ''}
		endif

		let index = 1

		while index >= 1 && index <= 13
			let list = [a:message]
			let keys = {}

			let index = 1

			while index <= 12
				if has_key(inputs, index)
					let list = add(list, index . '. F' . index . ': ' . inputs[index])
					let keys[index] = index
				endif

				let index += 1
			endwhile

			let index = inputlist(list)

			if has_key(keys, index)
				let inputs[keys[index]] = s:input('Mapping for F' . keys[index] . ': ', !has_key(inputs, index) ? '' : inputs[index])
				redraw
			endif
		endwhile

		for [key, mapping] in items(inputs)
			let mappings[key] = mapping
		endfor

		call filter(mappings, 'v:val != ''''')

		return mappings
	endfunction

	" Function s:inputMake() {{{2
	function s:inputMake(message, value)
		return s:input(a:message, a:value)
	endfunction

	" Function s:inputErrorFormat() {{{2
	function s:inputErrorFormat(message, value)
		return substitute(s:input(a:message, a:value), '\(\s\)', '\\\1', 'g')
	endfunction

	" Function s:inputTest() {{{2
	function s:inputTest(message, value)
		return s:input(a:message, a:value)
	endfunction

	" Function s:create() {{{2
	function s:create(line)
		let indent = s:indent(a:line)

		if indent >= 0
			try
				let myprojects = {}
				let attributes = {'path': '', 'filter': '', 'make': '', 'errorFormat': '', 'mappings': {}, 'test': ''}

				call s:readMyProjectsPreferences()

				if len(s:preferences) > 0
					let availableTypes = keys(s:preferences)
					let index = 1
					let types = ['Type of project: ']

					for type in availableTypes
						call add(types, index . '. ' . type)
						let index += 1
					endfor

					try
						call extend(attributes, s:preferences[availableTypes[inputlist(types) - 1]], 'force')
					catch /.*/
						throw 'Project type is invalid.'
					endtry
				endif

				let name = s:inputName('Name of new project: ')
				let myprojects[name] = {'attributes': {}, 'files': []}
				let myprojects[name]['attributes']['path'] = s:inputRealPath('Path of project ''' . name . ''': ', attributes['path'], 0)
				let myprojects[name]['attributes']['cd'] = s:inputCd('Working directory of project ''' . name . ''': ', myprojects[name]['attributes']['path'], myprojects[name]['attributes']['path'])
				let filter = s:inputFilter('Filter of project ''' . name . ''': ', attributes['filter'])

				if filter != ''
					let myprojects[name]['attributes']['filter'] = filter
				endif

				let myprojects[name]['attributes']['make'] = s:inputMake('Make of project ''' . name . ''': ', attributes['make'])
				let myprojects[name]['attributes']['errorformat'] = s:inputErrorFormat('Error format of project ''' . name . ''': ', attributes['errorFormat'])
				let myprojects[name]['attributes']['mappings'] = s:inputMappings('Mappings of project ''' . name . ''': ', attributes['mappings'])
				let myprojects[name]['attributes']['test'] = s:inputTest('Test extension of project ''' . name . ''': ', attributes['test'])

				call s:echo('Create project ''' . name . ''' from path ''' . myprojects[name]['attributes']['path'] . '''...')
				call s:put(s:buildMyProjects('', filter, myprojects, '', indent), a:line)
				call s:message('Project ''' . name . ''' created.')

				call s:echo(s:getPath(a:line))
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:refresh() {{{2
	function s:refresh(line, refreshFolder)
		let line = a:line

		if a:refreshFolder
			let line = s:isFolder(a:line) ? a:line : s:getFirstFolderLine(a:line)
		endif

		let path = s:getPath(line)

		if path != ''
			let indent = s:indent(line)

			if indent >= 0
				call s:echo('Do refresh of ''' . path . ''' in project ''' . s:getProjectName(line) . '''...')

				let myprojects = s:buildMyProjects(substitute(path, '[^' . s:osSlash . ']\+$', '', ''), s:extractFilter(line), s:getMyProjects(line), s:extractRefresh(line), indent)

				let range = line
				let foldlevel = 0
				
				if s:isFolder(line)
					let range .= ',' . s:getLastFolderLine(line)
					let foldlevel = foldlevel(line)
				endif

				silent! execute ':' . range . 'd'

				if myprojects != ''
					call s:put(myprojects, line)

					silent! execute ':' . range . 'foldclose!'

					while foldlevel > 0
						silent normal! zo
						let foldlevel -= 1
					endwhile

				endif

				call s:echo('Refresh done for ''' . path . '''.')
			endif
		endif
	endfunction

	" Function s:open() {{{2
	function s:open(command)
		let line = line('.')

		if s:isFolder(line)
			silent normal! za
		else
			try
				call s:edit(a:command, line)
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:delete() {{{2
	function s:delete(line)
		let path = s:getPath(a:line)

		if path != ''
			if getftype(path) == 'file' && delete(path) == 0
				call s:refresh(a:line, 1)
			else
				call s:error('Unable to delete ' . path . '.')
			endif
		endif
	endfunction

	" Function s:grep() {{{2
	function s:grep(line)
		let path = s:getPath(a:line)

		if path != ''
			let pattern = s:input('Grep in ''' . path . ''': ', '')

			if pattern != ''
				if s:isFolder(a:line)
					let files = s:getFiles(path, s:extractFilter(a:line))
				else
					let files = path
				endif

				if files != ''
					let s:loadAttributes = 0

					try
						call s:echo('Do grep on ''' . path . ''' with pattern ''' . pattern . '''...')
						silent execute 'vimgrep /' . escape(pattern, '/') . '/jg ' . files
						silent cw
						call s:echo('Grep done on ''' . path . ''' with pattern ''' . pattern . '''.')
					catch /E480/
						call s:error('No match found for grep with ''' . pattern . ''' in ''' . path . '''.')
					catch
						call s:error(v:exception)
					endtry

					let s:loadAttributes = 1
				endif
			endif
		endif
	endfunction

	" Function s:explore() {{{2
	function s:explore(mode)
		let line = line('.')

		if !s:isFolder(line)
			let line = s:getFirstFolderLine(line)
		endif

		if line > 0
			let path = s:getPath(line)

			if path != ''
				call s:goToAnEditionWindow()
				silent execute a:mode . 'xplore ' . path
			endif
		endif
	endfunction

	" Function s:getTagsGenerator() {{{2
	function s:setTagsGenerator()
		for generator in ['exuberant-ctags', 'exctags', 'ctags', 'ctags.exe', 'tags']
			if executable(generator)
				let g:myprojects_tags_generator = generator
				break
			endif
		endfor
	endfunction

	" Function s:generateTags() {{{2
	function s:generateTags(line)
		if g:myprojects_tags_file != ''
			if g:myprojects_tags_generator == ''
				call s:setTagsGenerator()
			endif

			if !executable(g:myprojects_tags_generator)
				call s:error('Unable to find a tags generator, please define g:myprojects_tags_generator variable.')
			else
				let rootPath = s:getProjectPath(a:line)

				if rootPath != ''
					let tagsPath = rootPath . s:osSlash . g:myprojects_tags_file

					try
						call s:echo('Generate tags file in ''' . tagsPath . ''' for project ''' . s:getProjectName(a:line) . '''...')
						call s:system(g:myprojects_tags_generator, '-f ' . tagsPath . ' -R ' . rootPath)
						call s:message('Tags file generated for project ''' . s:getProjectName(a:line) . ''' and stored in ''' . tagsPath . '''.')

						if &tags !~ '^' . tagsPath . ',\?'
							silent execute 'set tags=' . tagsPath . ',' . &tags
						endif
					catch /.*/
						call s:message('Unable to generate tags file for project ''' . s:getProjectName(a:line) . ''' : ' . v:exception)
					endtry
				endif
			endif
		endif
	endfunction

	" Function s:edit() {{{2
	function s:edit(command, line)
		let path = s:getPath(a:line)
		let projectPath = s:getProjectPath(a:line)
		let projectName = s:getProjectName(a:line)
		let cd = s:extractCd(a:line)
		let mappings = s:extractMappings(a:line)
		let make = s:extractMake(a:line)
		let errorFormat = s:extractErrorFormat(a:line)
		let test = s:extractTest(a:line)
		let type = getftype(path)

		if type == ''
			let head = fnamemodify(path, ':h')

			let headType = getftype(head)

			if headType == ''
				call s:mkdir(head)
			elseif headType != 'dir'
				throw 'Path ''' . head . ''' exists but it is not a directory.'
			endif
		elseif type != 'file'
			throw 'Unable to open ''' . path . ''' of type ''' . type . '''.'
		elseif !filereadable(path)
			throw 'Unable to read file ''' . path . '''.'
		endif

		if g:myprojects_auto_resize
			call s:setWindowWidth(g:myprojects_width)
		endif
	
		if g:myprojects_auto_close
			call s:closeMyProjectsWindow()
		endif

		let window = bufwinnr('^' . path . '$')

		if window != -1
			silent execute window . 'wincmd w'
			silent execute 'buffer ' . path
		else
			call s:goToAnEditionWindow()

			let command =  a:command . ' ' . fnameescape(path)

			try
				silent execute command
			catch E37
				if s:input('Save ''' . expand(bufname('%'), ':p') . ''' and load ''' . path . ''' ? [y/N]: ', '') != 'y'
					return 0
				else
					write
					silent execute command
				endif
			endtry
		endif

		if g:myprojects_tags_file != '' && projectPath != ''
			let tagsPath = projectPath . s:osSlash . g:myprojects_tags_file

			if getftype(tagsPath) == 'file'
				if &tags !~ '^' . tagsPath . ',\?'
					silent execute 'set tags=' . tagsPath . ',' . &tags
				endif
			endif
		endif

		if cd != ''
			let cd = s:unescape(cd)

			if getftype(cd) != 'dir'
				throw 'Unable to change directory to ' . cd . '.'
			else
				let cd = resolve(cd)

				if getftype(cd) != 'dir'
					call s:warningMessage('Path ''' . cd . ''' is not a valid working directory.')
				else
					let cd = fnameescape(cd)

					try
						silent execute 'lcd ' . cd

						if !exists('#' . s:plugin . '#BufEnter#<buffer>')
							silent execute 'augroup ' . s:plugin
							silent execute 'au! BufEnter <buffer> lcd ' . cd
							silent augroup END
						endif
					catch /E472/
						call s:warningMessage('Unable to go to working directory ''' . cd . '''.')
					endtry
				endif
			endif
		endif

		for [line, mapping] in items(mappings)
			for [key, value] in items(mapping)
				silent execute 'nnoremap <buffer> <silent> <F' . key . '> ' . expand(value)
			endfor
		endfor

		if make != ''
			silent execute 'setlocal makeprg=' . s:escape(make)
		endif

		if errorFormat != ''
			silent execute 'setlocal errorformat=' . s:escape(errorFormat)
		endif

		if test != ''
			let tail = fnamemodify(path, ':t')
			let rootTail = fnamemodify(tail, ':r')

			while rootTail != tail
				let tail = rootTail
				let rootTail = fnamemodify(tail, ':r')
			endwhile

			silent execute 'nnoremap <silent> <buffer> <LocalLeader>et :call <SID>openTest(''split'', ''' . path . ''', ''' . tail . test . ''')<CR>'
		endif

		setlocal more

		if !hasmapto('<Plug>MyProjectsGoTo')
			map <buffer> <silent> <C-Tab> <Plug>MyProjectsGoTo
		endif

		nnoremap <silent> <buffer> <LocalLeader>ra :call <SID>openFromMyProjectsWindow('edit', fnamemodify(bufname('%'), ':p'))<CR>

		silent execute 'nnoremap <buffer> <silent> <LocalLeader>b :call <SID>displayProjectBuffers(''' . projectName. ''', ''' . projectPath . ''', '''')<CR>'

		let b:myprojectsAttributesLoaded = 1
	endfunction

	" Function s:append() {{{2
	function s:append(line)
		let path = s:getPath(a:line)

		if path != ''
			if getftype(path) != 'file' || !filereadable(path)
				call s:error('Unable to read file ' . path . '.')
			else
				call s:goToAnEditionWindow()
				silent execute ':r ' . path
				silent normal! k
				silent normal! dd
			endif
		endif
	endfunction

	" Function s:getNestedAttribute() {{{2
	function s:getNestedAttribute(name, line)
		let attribute = []

		let line = a:line

		if !exists('*s:has' . a:name)
			while line > 0 && empty(attribute)
				if s:hasAttribute(a:name, line)
					let attribute = [line, s:extractAttribute(a:name, line)]
				else
					let line = s:getFirstFolderLine(line)
				endif
			endwhile

			if empty(attribute) && s:hasAttribute(a:name, line)
				let attribute = [line, s:extractAttribute(a:name, line)]
			endif
		else
			while line > 0 && empty(attribute)
				if s:has{a:name}(line)
					let attribute = [line, s:extract{a:name}(line)]
				else
					let line = s:getFirstFolderLine(line)
				endif
			endwhile

			if empty(attribute) && s:has{a:name}(line)
				let attribute = [line, s:extract{a:name}(line)]
			endif
		endif

		return attribute
	endfunction

	" Function s:getNestedPath() {{{2
	function s:getNestedPath(line)
		return s:getNestedAttribute('Path', a:line)
	endfunction

	" Function s:getNestedCd() {{{2
	function s:getNestedCd(line)
		return s:getNestedAttribute('Cd', a:line)
	endfunction

	" Function s:getNestedMappings() {{{2
	function s:getNestedMappings(line)
		let mappings = {}

		let index = 1

		while index <= 12
			let mapping = s:getNestedAttribute('F' . index, a:line)

			if !empty(mapping)
				let mappings[index] = mapping
			endif

			let index += 1
		endwhile

		return mappings
	endfunction

	" Function s:getNestedFilter() {{{2
	function s:getNestedFilter(line)
		return s:getNestedAttribute('Filter', a:line)
	endfunction

	" Function s:getNestedMake() {{{2
	function s:getNestedMake(line)
		return s:getNestedAttribute('Make', a:line)
	endfunction

	" Function s:getNestedErrorFormat() {{{2
	function s:getNestedErrorFormat(line)
		return s:getNestedAttribute('errorformat', a:line)
	endfunction

	" Function s:getNestedTest() {{{2
	function s:getNestedTest(line)
		return s:getNestedAttribute('test', a:line)
	endfunction

	" Function s:hasAttribute() {{{2
	function s:hasAttribute(name, line)
		return getline(a:line) =~ '.*\%(\%(\\\)\@<! \)\+' . a:name . '="[^"]\{-}"'
	endfunction

	" Function s:hasPath() {{{2
	function s:hasPath(line)
		return substitute(getline(a:line), '^\t*[^\t].\{-}\(\%(\\\)\@<!\%( \|=\)\)', '\1', '') =~ '^='
	endfunction

	" Function s:hasCd() {{{2
	function s:hasCd(line)
		return s:hasAttribute('cd', a:line)
	endfunction

	" Function s:hasFilter() {{{2
	function s:hasFilter(line)
		return s:hasAttribute('filter', a:line)
	endfunction

	" Function s:hasMapping() {{{2
	function s:hasMapping(mapping, line)
		return s:hasAttribute(a:mapping, a:line)
	endfunction

	" Function s:hasMake() {{{2
	function s:hasMake(line)
		return s:hasAttribute('make', a:line)
	endfunction

	" Function s:hasErrorFormat() {{{2
	function s:hasErrorFormat(line)
		return s:hasAttribute('errorformat', a:line)
	endfunction

	" Function s:hasTest() {{{2
	function s:hasTest(line)
		return s:hasAttribute('test', a:line)
	endfunction

	" Function s:setPath() {{{2
	function s:setPath(line)
		if !s:hasPath(a:line)
			let currentPath = s:getPath(a:line)

			if currentPath != ''
				let name = s:getName(a:line)

				try
					let newPath = s:cleanPath(s:inputRealPath('Set path for ''' . name . ''': ', '', 1))

					if newPath != '' && newPath != currentPath
						call s:echo('Set path with ''' . newPath . ''' on ''' . name . '''...')
						call s:substitute(a:line, '\(^\t*[^\t]\%(\\ \|\f\)\+\)', '\1=' . newPath, '')
						call s:refresh(pathLine, 0)
						call s:message('Path set with ''' . newPath . ''' on ''' . name . '''.')
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:setCd() {{{2
	function s:setCd(line)
		if !s:hasCd(a:line)
			let path = s:getPath(a:line)

			if path != ''
				let cd = s:getNestedCd(a:line)
				let currentCd = empty(cd) ? '' : cd[1]

				try
					let newCd = s:inputCd('Set working directory for ''' . path . ''': ', path, currentCd)

					if newCd != '' && newCd != currentCd
						call s:echo('Set working directory with ''' . newCd . ''' on ''' . path . '''...')
						call s:updateAttribute(a:line, 'cd', s:escape(newCd))
						call s:message('Working directory set with ''' . newCd . ''' on ''' . path . '''.')
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:setFilter() {{{2
	function s:setFilter(line)
		let line = s:isFolder(a:line) ? a:line : s:getFirstFolderLine(a:line)

		if line > 0 && !s:hasFilter(line)
			let path = s:getPath(line)

			if path != ''
				let filter = s:getNestedFilter(line)
				let currentFilter = empty(filter) ? '' : filter[1]
				let newFilter = s:inputFilter('Set filter for ''' . path . ''': ', currentFilter)

				if newFilter != currentFilter
					call s:echo('Set filter with ''' . newFilter . ''' on ''' . path . '''...')
					call s:updateAttribute(line, 'filter', newFilter)
					call s:updateRefresh(line, '')
					call s:refresh(line, 0)
					call s:message('Filter set with ''' . newFilter . ''' on ''' . path . '''.')
				endif
			endif
		endif
	endfunction

	" Function s:setMappings() {{{2
	function s:setMappings(line)
		if !s:hasMapping('F[1-9][0-2]\?', a:line)
			let path = s:getPath(a:line)

			if path != ''
				for [key, mapping] in items(s:inputMappings('Set mapping for ''' . path . ''': ', {}))
					if mapping != ''
						call s:echo('Set mappings on ''' . path . '''...')
						call s:updateAttribute(a:line, 'F' . key, mapping)
						call s:message('Mappings set on ''' . path . '''.')
					endif
				endfor
			endif
		endif
	endfunction

	" Function s:setMake() {{{2
	function s:setMake(line)
		if !s:hasMake(a:line)
			let path = s:getPath(a:line)

			if path != ''
				let make = s:getNestedMake(a:line)
				let currentMake = empty(make) ? '' : make[1]

				try
					let newMake = s:inputMake('Set make for ''' . path . ''': ', currentMake)

					if newMake != currentMake
						call s:echo('Set make with ''' . newMake . ''' on ''' . path . '''...')
						call s:updateAttribute(a:line, 'make', newMake)
						call s:message('Make set with ''' . newMake . ''' on ''' . path . '''.')
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:setErrorFormat() {{{2
	function s:setErrorFormat(line)
		if !s:hasErrorFormat(a:line)
			let path = s:getPath(a:line)

			if path != ''
				let errorFormat = s:getNestedErrorFormat(a:line)
				let currentErrorFormat = empty(errorFormat) ? '' : errorFormat[1]

				try
					let newErrorFormat = s:inputErrorFormat('Set error format for ''' . path . ''': ', currentErrorFormat)

					if newErrorFormat != currentErrorFormat
						call s:echo('Set error format with ''' . newErrorFormat . ''' on ''' . path . '''...')
						call s:updateAttribute(a:line, 'errorformat', newErrorFormat)
						call s:message('Error format set with ''' . newErrorFormat . ''' on ''' . path . '''.')
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:setTest() {{{2
	function s:setTest(line)
		if !s:hasTest(a:line)
			let path = s:getPath(a:line)

			if path != ''
				let test = s:getNestedTest(a:line)
				let currentTest = empty(test) ? '' : test[1]

				try
					let newTest = s:inputTest('Set test for ''' . path . ''': ', currentTest)

					if newTest != currentTest
						call s:echo('Set test with ''' . newTest . ''' on ''' . path . '''...')
						call s:updateAttribute(a:line, 'test', newTest)
						call s:message('Test set with ''' . newTest . ''' on ''' . path . '''.')
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:updateAttribute() {{{2
	function s:updateAttribute(line, attribute, value)
		if getline(a:line) =~ '\s\+' . a:attribute . '="[^"]\+"'
			if a:value == '' && a:attribute != 'filter'
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\{-}"', '', '')
			else
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\{-}"', ' ' . a:attribute . '="' . escape(a:value, '\%&') . '"', '')
			endif
		else
			call s:substitute(a:line, '\(^.\+$\)', '\1 ' . a:attribute . '="' . escape(a:value, '\%&') . '"', '')
		endif
	endfunction

	" Function s:updatePath() {{{2
	function s:updatePath(line)
		let path = s:getNestedPath(a:line)

		if !empty(path)
			let [pathLine, currentPath] = path

			try
				let name = s:getName(pathLine)
				let newPath = s:cleanPath(s:inputRealPath('Update path for ''' . name . ''': ', '', 1))

				if newPath != ''
					if newPath == currentPath
						call s:echo('Path not updated for ''' . name . '''.')
					else
						call s:echo('Update path with ''' . newPath . ''' for ''' . name . '''...')
						call s:substitute(pathLine, '^.*$', s:getName(pathLine) . '=' . newPath . ' ' . substitute(substitute(getline(pathLine), '^\t*[^\t].\{-}\(\%(\\\)\@<!\%( \|=\)\)', '\1', ''), '^=.\{-}\%(\\\)\@<! \(.*$\)', '\1', ''), '')
						call s:refresh(pathLine, 0)
						call s:message('Path updated with ''' . newPath . ''' for name ''' . name . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:updateCd() {{{2
	function s:updateCd(line)
		let cd = s:getNestedCd(a:line)

		if !empty(cd)
			let [cdLine, currentCd] = cd

			try
				let path = s:getPath(cdLine)
				let newCd = s:inputCd('Update working directory for ''' . path . ''': ', path, currentCd)

				if newCd == currentCd
					call s:echo('Working directory not updated for ''' . path . '''.')
				else
					call s:echo('Update working directory with ''' . newCd . ''' on ''' . path . '''...')
					call s:updateAttribute(cdLine, 'cd', s:escape(newCd))
					call s:message('Working directory updated with ''' . newCd . ''' on ''' . path . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:updateFilter() {{{2
	function s:updateFilter(line)
		let filter = s:getNestedFilter(a:line)

		if !empty(filter)
			let [filterLine, currentFilter] = filter

			try
				let path = s:getPath(filterLine)
				let newFilter = s:inputFilter('Update filter for ''' . path . ''': ', currentFilter)

				if newFilter == currentFilter
					call s:echo('Filter not updated for ''' . path . '''.')
				else
					call s:echo('Update filter with ''' . newFilter . ''' on ''' . path . '''...')
					call s:updateAttribute(filterLine, 'filter', newFilter)
					call s:updateRefresh(filterLine, '')
					call s:refresh(filterLine, 0)
					call s:message('Filter updated with ''' . newFilter . ''' on ''' . path . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:updateMappings() {{{2
	function s:updateMappings(line)
		let currentMappings = s:getNestedMappings(a:line)

		if !empty(currentMappings)
			let lines = {1: '', 2: '', 3: '', 4: '', 5: '', 6: '', 7: '', 8: '', 9: '', 10: '', 11: '', 12: ''}
			let mappings = {1: '', 2: '', 3: '', 4: '', 5: '', 6: '', 7: '', 8: '', 9: '', 10: '', 11: '', 12: ''}

			for [key, value] in items(currentMappings)
				let lines[key] = value[0]
				let mappings[key] = value[1]
			endfor

			for [key, mapping] in items(s:inputMappings('Update mapping: ', mappings))
				if has_key(lines, key)
					call s:updateAttribute(lines[key], 'F' . key, mapping)
				endif
			endfor
		endif
	endfunction

	" Function s:updateMake() {{{2
	function s:updateMake(line)
		let make = s:getNestedMake(a:line)

		if !empty(make)
			let [makeLine, currentMake] = make

			try
				let path = s:getPath(makeLine)
				let newMake = s:inputMake('Update make for ''' . path . ''': ', currentMake)

				if newMake == currentMake
					call s:echo('Make not updated for ''' . path . '''.')
				else
					call s:echo('Update make with ''' . newMake . ''' on ''' . path . '''...')
					call s:updateAttribute(makeLine, 'make', newMake)
					call s:message('Make updated with ''' . newMake . ''' on ''' . path . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:updateErrorFormat() {{{2
	function s:updateErrorFormat(line)
		let errorFormat = s:getNestedErrorFormat(a:line)

		if !empty(errorFormat)
			let [errorFormatLine, currentErrorFormat] = errorFormat

			try
				let path = s:getPath(errorFormatLine)
				let newErrorFormat = s:inputErrorFormat('Update error format for ''' . path . ''': ', currentErrorFormat)

				if newErrorFormat == currentErrorFormat
					call s:echo('Errorformat not updated for ''' . path . '''.')
				else
					call s:echo('Update error format with ''' . newErrorFormat . ''' on ''' . path . '''...')
					call s:updateAttribute(errorFormatLine, 'errorformat', newErrorFormat)
					call s:message('Error format updated with ''' . newErrorFormat . ''' on ''' . path . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction


	" Function s:updateTest() {{{2
	function s:updateTest(line)
		let test = s:getNestedTest(a:line)

		if !empty(test)
			let [testLine, currentTest] = test

			try
				let path = s:getPath(testLine)
				let newTest = s:inputTest('Update test for ''' . path . ''': ', currentTest)

				if newTest == currentTest
					call s:echo('Test not updated for ''' . path . '''.')
				else
					call s:echo('Update test with ''' . newTest . ''' on ''' . path . '''...')
					call s:updateAttribute(testLine, 'test', newTest)
					call s:message('Test updated with ''' . newTest . ''' on ''' . path . '''.')
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction


	" Function s:updateRefresh() {{{2
	function s:updateRefresh(line, timestamp)
		let path = s:getPath(a:line)

		if path != '' && getftype(path) == 'dir'
			call s:updateAttribute(a:line, 'refresh', a:timestamp)
		endif
	endfunction

	" Function s:system() {{{2
	function s:system(bin, arguments)
		if !exists('*system')
			throw 'Vim system() built-in function is not available.'
		elseif !executable(a:bin)
			throw '''' . a:bin . ''' executable is not available.'
		else
			let output = system(a:bin . ' ' . a:arguments)

			if v:shell_error
				throw output
			else
				return output
			endif
		endif
	endfunction

	" Function s:svn() {{{2
	function s:svn(arguments)
		return s:system('svn', a:arguments)
	endfunction

	" Function s:svnError() {{{2
	function s:svnError(error, exception)
		call s:errorMessage(a:error)

		for error in split(a:exception, "\n")
			call s:errorMessage("   " . error)
		endfor
	endfunction

	" Function s:getSvnStatus() {{{2
	function s:getSvnStatus(path)
		let files = filter(split(s:svn('status ' . shellescape(a:path)), "\n"), "v:val =~# '^[[:space:]ACDIMRX?!~L+SKOTB]\\{6}\\s'")

		if g:myprojects_sort_svn
			let files = sort(files)
		endif

		return files
	endfunction

	" Function s:svnStatus() {{{2
	function s:svnStatus(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Svn status of ''' . path . '''')
				call s:echo('Do svn status on ''' . path . '''...')
				call s:putInMyProjectsWindow(s:getSvnStatus(path))
				call s:message('Svn status done on path ''' . path . '''.')
			catch /.*/
				call s:svnError('Svn status failed on ''' . path . ''' :', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnUpdate() {{{2
	function s:svnUpdate(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Svn update of ''' . path . '''')
				call s:echo('Do svn update on ''' . path . '''...')

				let files = filter(split(s:svn('update --accept postpone ' . shellescape(path)), "\n"), "v:val =~# '^[ADUCGE[:space:]]\\{4}\\s'")

				if !empty(files)
					if g:myprojects_sort_svn
						let files = sort(files)
					endif

					call s:refresh(a:line, 0)
				endif

				call s:putInMyProjectsWindow(files)
				call s:message('Svn update done on ''' . path . '''.')
			catch /.*/
				call s:svnError('Svn update failed on ''' . path . ''' :', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnCommitStepOne() {{{2
	function s:svnCommitStepOne(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Files to commit in ''' . path . '''')

				call s:echo('Retrieve files to commit in ''' . path . '''...')
				call s:putInMyProjectsWindow(filter(s:getSvnStatus(path), 'strpart(v:val, 0, 6) =~# "[MDAR]"'))
				call s:echo('Files to commit retrieved in ''' . path . '''.')

				setlocal buftype=acwrite
				setlocal modifiable

				execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnCommitStepTwo()'
			catch /.*/
				call s:svnError('Unable to retrieve files to revert in ''' . path . ''' :', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnCommitStepTwo() {{{2
	function s:svnCommitStepTwo()
		setlocal nomodified

		let files = getbufline('%', 1, '$')

		call filter(files, 'strpart(v:val, 0, 6) =~# "[MDAR]"')

		if empty(files)
			bwipeout
		else
			let b:files = copy(files)

			call insert(files, '')
			call s:createSvnWindow('Define log message and type :w to commit...')
			call s:putInMyProjectsWindow(files)

			setlocal modifiable
			setlocal buftype=acwrite

			execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnCommitStepThree()'
		endif
	endfunction

	" Function s:svnCommitStepThree() {{{2
	function s:svnCommitStepThree()
		setlocal nomodified

		let files = b:files
		let message = filter(getbufline('%', 1, '$'), "v:val !~# '^" . escape(s:prompt, '[]') . "'")

		bwipeout

		call map(files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

		try
			call s:echo('Do svn commit of ' . join(files, ', ') . '...')
			call s:svn('commit ' . join(map(copy(files), 'shellescape(v:val)'), ' ') . ' -m ' . shellescape(join(message, "\n")))
			call s:message('Svn commit done for files ' . join(files, ', ') . '.')
		catch /.*/
			call s:svnError('Svn commit failed on files ''' . join(files, ''', ''') . ''':', v:exception)
		endtry
	endfunction

	" Function s:svnRevertStepOne() {{{2
	function s:svnRevertStepOne(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Files to revert in ''' . path . ''', type :w to revert them...')
				call s:echo('Retrieve files to revert in ''' . path . '''...')
				call s:putInMyProjectsWindow(filter(s:getSvnStatus(path), 'strpart(v:val, 0, 6) =~# ''\%(M\|D\|R\|C\)'''))
				call s:echo('Files to revert retrieved in ''' . path . '''.')

				setlocal buftype=acwrite
				setlocal modifiable

				execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnRevertStepTwo()'
			catch /.*/
				call s:svnError('Unable to retrieve files to revert in ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnRevertStepTwo() {{{2
	function s:svnRevertStepTwo()
		setlocal nomodified

		let files = filter(getbufline('%', 1, '$'), 'strpart(v:val, 0, 6) =~# ''\%(M\|D\|R\|C\)''')

		bwipeout

		if !empty(files)
			call map(files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

			try
				call s:echo('Do svn revert on files ' . join(files, ', ') . '...')
				call s:svn('revert ' . join(map(copy(files), 'shellescape(v:val)'), ' '))
				call s:message('Svn revert done for files ' . join(files, ', ') . '.')
			catch /.*/
				call s:svnError('Svn revert failed on ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnAddStepOne() {{{2
	function s:svnAddStepOne(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Files to add in ''' . path . ''', type :w to add them...')
				call s:echo('Retrieve files to add in ''' . path . '''...')
				call s:putInMyProjectsWindow(filter(s:getSvnStatus(path), 'v:val =~ "^?"'))
				call s:echo('Files to add retrieved in ''' . path . '''.')

				setlocal buftype=acwrite
				setlocal modifiable

				execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnAddStepTwo()'
			catch /.*/
				call s:svnError('Unable to retrieve files to add in ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnAddStepTwo() {{{2
	function s:svnAddStepTwo()
		setlocal nomodified

		let files = getbufline('%', 1, '$')

		bwipeout

		call filter(files, "v:val =~ '^?'")

		if !empty(files)
			call map(files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

			try
				call s:echo('Do svn add for files ' . join(files, ', ') . '...')
				call s:svn('add ' . join(map(copy(files), 'shellescape(v:val)'), ' '))
				call s:message('Svn add done for files ' . join(files, ', ') . '.')
			catch /.*/
				call s:svnError('Svn add failed on ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnDiff() {{{2
	function s:svnDiff(line)
		let path = s:getPath(a:line)

		if path != ''
			let path = resolve(path)

			if getftype(path) != 'file'
				call s:error('Unable to diff ''' . path . ''' because it is not a file.')
			else
				try
					call s:echo('Do svn diff on ''' . path . '''...')
					let previousVersion = s:svn('cat ' . shellescape(path) . ' -r HEAD')
					call s:message('Svn diff done on ''' . path . '''.')

					if !empty(s:diffBuffers)
						if bufexists(s:diffBuffers[0]['buffer'])
							let window = bufwinnr(s:diffBuffers[0]['buffer'])

							silent execute window . 'wincmd w'
							silent diffoff

							if !s:diffBuffers[0]['wrap']
								setlocal nowrap
							endif

							silent execute 'setlocal foldmethod=' . s:diffBuffers[0]['foldmethod']
							silent execute 'setlocal foldcolumn=' . s:diffBuffers[0]['foldcolumn']
							silent wincmd p
						endif

						if bufexists(s:diffBuffers[1])
							silent execute 'bwipeout ' . s:diffBuffers[1]
						endif
					endif

					let s:diffBuffers = []

					call s:edit('edit', a:line)

					call add(s:diffBuffers, {'buffer': bufnr('%'), 'wrap': &wrap, 'foldmethod': &foldmethod, 'foldcolumn': &foldcolumn})

					let filetype = &filetype

					diffthis

					silent vnew
					silent execute 'setlocal filetype=' . filetype
					setlocal bufhidden=delete
					setlocal buftype=nofile
					setlocal nobuflisted
					setlocal noswapfile
					silent! 0put=previousVersion
					silent! normal! $d
					silent normal! 1gg
					setlocal nomodifiable

					diffthis

					call add(s:diffBuffers, bufnr('%'))

					let file = s:prompt .'Svn diff of ''' . path . ''''
					silent file `=file`

					let &titlestring = file

					silent execute 'augroup ' . s:plugin
					silent execute 'au! BufEnter <buffer> let &titlestring ="' . file . '"'
					silent augroup END

					silent normal! ]c
				catch /.*/
					call s:error('Unable to diff ''' . path . ''' : ' . v:exception . '.')
				endtry
			endif
		endif
	endfunction

	" Function s:svnBlame() {{{2
	function s:svnBlame(line)
		let path = s:getPath(a:line)

		if path != ''
			let path = resolve(path)

			if getftype(path) != 'file'
				call s:error('Unable to blame ''' . path . ''' because it is not a file.')
			else
				try
					call s:echo('Do svn blame on ''' . path . '''...')
					let output = s:svn('blame -v ' . shellescape(path))
					call s:message('Svn blame on ''' . path . ''' done.')

					let blame = split(output, "\n")
					call map(blame, 'substitute(v:val, "^\\([^)]\\+)\\s\\).*$", "\\1|", "")')
					call map(blame, 'substitute(v:val, "\\s([^)]\\+)", "", "")')

					call s:edit('edit', a:line)
					setlocal bufhidden=delete
					setlocal buftype=nofile
					setlocal nobuflisted
					setlocal noswapfile
					setlocal nofoldenable

					let buffer = getbufline('%', 1, '$')

					for line in range(0, len(buffer) - 1)
						let buffer[line] = blame[line] . buffer[line]
					endfor

					silent! normal! %d
					silent! 0put=buffer
					silent! normal! $d
					silent normal! 1gg

					setlocal nomodifiable

					let file = s:prompt .'Svn blame of ''' . path . ''''
					silent file `=file`

					let &titlestring = file

					silent execute 'augroup ' . s:plugin
					silent execute 'au! BufEnter <buffer> let &titlestring ="' . file . '"'
					silent augroup END
				catch /.*/
					call s:error('Unable to blame ''' . path . ''' : ' . v:exception . '.')
				endtry
			endif
		endif
	endfunction

	" Function s:svnCheckout() {{{2
	function s:svnCheckout(line)
		let indent = s:indent(a:line)

		if indent >= 0
			try
				let myprojects = {}
				let name = s:inputName('Name of new project: ')
				let myprojects[name] = {'attributes': {}, 'files': []}
				let myprojects[name]['attributes']['path'] = s:inputPath('Path of project ''' . name . ''': ', '', 0)
				let myprojects[name]['attributes']['cd'] = s:inputCd('Working directory of project ''' . name . ''': ', myprojects[name]['attributes']['path'], '')
				let filter = s:inputFilter('Filter of project ''' . name . ''': ', '')

				if filter != ''
					let myprojects[name]['attributes']['filter'] = filter
				endif

				let myprojects[name]['attributes']['make'] = s:inputMake('Make of project ''' . name . ''': ', '')
				let myprojects[name]['attributes']['errorformat'] = s:inputErrorFormat('Error format of project ''' . name . ''': ', '')
				let myprojects[name]['attributes']['mappings'] = s:inputMappings('Mappings of project ''' . name . ''': ', {})

				let svn = s:input('Svn reporitory of project ''' . name . ''': ', '')

				if svn == ''
					throw 'Svn repository must not be empty.'
				else
					let svnCommand = 'checkout --non-interactive ' . shellescape(svn)

					let svnUser = s:input('Svn user of project ''' . name . ''': ', '')

					if svnUser != ''
						let svnCommand .=  ' --username ' . shellescape(svnUser)

						let svnPassword = s:inputSecret('Svn password of project ''' . name . ''': ', '')

						if svnPassword != ''
							let svnCommand .= ' --password ' . shellescape(svnPassword)
						endif
					endif

					let svnCommand .= ' ' . shellescape(myprojects[name]['attributes']['path'])

					try
						call s:echo('Do svn checkout of ''' . svn . ''' in ''' . myprojects[name]['attributes']['path'] . '''...')
						call s:svn(svnCommand)
						call s:message('Svn checkout of ''' . svn . ''' in ''' . myprojects[name]['attributes']['path'] . ''' done.')
						call s:echo('Create project ''' . name . ''' from path ''' . myprojects[name]['attributes']['path'] . '''...')
						call s:put(s:buildMyProjects('', filter, myprojects, '', indent), '', a:line)
						call s:message('Project ''' . name . ''' created.')
						call s:echo(s:getPath(a:line))
					catch /.*/
						call s:svnError('Unable to checkout ''' . svn . ''' in ''' . myprojects[name]['attributes']['path'] . ''':', v:exception)
					endtry
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:svnLog() {{{2
	function s:svnLog(line)
		let path = s:getPath(a:line)

		if path != ''
			let path = resolve(path)

			if getftype(path) == ''
				call s:error('Unable to get log file of ''' . path . ''' because it is not a file or a directory.')
			else
				try
					let path = s:getPath(a:line)

					call s:createSvnWindow('Svn log of ''' . path . '''')
					call s:echo('Do svn log on ''' . path . '''...')

					let log = map(split(substitute(substitute(s:svn('log -v ' . shellescape(path)), '\n\n\+\(-\{72}\)', '\n\1', 'g'), '[[:space:]ACDIMRX?!~][[:space:]CM][[:space:]L][[:space:]+][[:space:]SX][[:space:]K].\{-}\(-\{72}\)\@=', '\n', 'g'), "\n"), 'substitute(v:val, "\\\\$", "", "")')

					let info = s:svn('info ' . shellescape(path))
					let root = substitute(info, '^.*Repository\sRoot:\s\([^\n]\{-}\)\n.*$', '\1', '')
					let url = substitute(info, '^.*URL:\s\([^\n]\{-}\)\n.*$', '\1', '')
					let delta = substitute(url, '^' . root, '', '')

					echomsg delta

					let log = map(log, 'substitute(v:val, "^\\s\\+\\([ACDIMRX]\\s\\)' . substitute(url, '^' . root, '', '') . '\\(.\\+\\)$", "\\1      " . path . "\\2", "")')
					let log = filter(log, 'v:val !~ ''^\s\+[ASDIMRX]''')

					call s:putInMyProjectsWindow(log)
					call s:message('Svn log done on path ''' . path . '''.')
				catch /.*/
					call s:svnError('Svn status failed on ''' . path . ''' :', v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:refreshSvnConflictWindow() {{{2
	function s:refreshSvnConflictWindow(path)
		try
			call s:createSvnConflictWindow('Conflict files in ''' . a:path . '''', a:path)
			call s:echo('Retrieve conflict files in ''' . a:path . '''...')
			call s:putInMyProjectsWindow(filter(s:getSvnStatus(a:path), 'strpart(v:val, 0, 6) =~# ''C'''))
			call s:echo('Conflict files retrieved in ''' . a:path . '''.')
		catch /.*/
			call s:svnError('Unable to retrieve conflict files in ''' . a:path . ''':', v:exception)
		endtry
	endfunction

	" Function s:svnResolve() {{{2
	function s:svnResolve(line)
		let path = s:getPath(a:line)

		if path != ''
			call s:refreshSvnConflictWindow(path)
		endif
	endfunction

	" Function s:svnMarkAsResolved() {{{2
	function s:svnMarkAsResolved(path)
		let conflictAreResolved = s:input('All conflicts are resolved [yN] ? [y/N] : ', 'N')

		if conflictAreResolved != 'y'
			call s:echo('Conflicts not resolved on ''' . path . '''.')
		else
			let path = fnamemodify(bufname('%'), ':p')

			try
				call s:echo('Resolve conflicts on ''' . path . '''...')
				call s:svn('resolved --non-interactive ' . shellescape(path))
				call s:message('Conflicts resolved on ''' . path . '''.')
				call s:refreshSvnConflictWindow(a:path)
			catch /.*/
				call s:svnError('Unable to resolve conflict for file ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:svnInfo() {{{2
	function s:svnInfo(line)
		let path = s:getPath(a:line)

		if path != ''
			try
				call s:createSvnWindow('Svn info of ''' . path . '''')
				call s:echo('Do svn info on ''' . path . '''...')

				let lines = split(s:svn('info --non-interactive ' . shellescape(path)), "\n")

				let maxLength = 0

				for line in lines
					let lineLength = strlen(substitute(line, '^\([^:]\+:\).*$', '\1', ''))

					if lineLength > maxLength
						let maxLength = lineLength
					endif
				endfor

				call map(lines, 'repeat('' '', ' . maxLength . ' - strlen(substitute(v:val, ''^\([^:]\+:\).*$'', ''\1'', ''''))) . v:val')
				call s:putInMyProjectsWindow(lines)
				call s:message('Svn info done on path ''' . path . '''.')
			catch /.*/
				call s:svnError('Svn info failed on ''' . path . ''':', v:exception)
			endtry
		endif
	endfunction

	" Function s:getRawBuffers() {{{2
	function s:getRawBuffers(hidden)
		redir => rawBuffers

		if a:hidden
			buffers!
		else
			buffers
		endif

		redir END

		return split(rawBuffers, "\n")
	endfunction

	" Function s:getBuffers() {{{2
	function s:getBuffers(hidden)
		let buffers = {}

		silent let rawBuffers = s:getRawBuffers(a:hidden)

		for buffer in rawBuffers
			let path = resolve(fnamemodify(expand(substitute(buffer, '^[^"]\+"\([^"]\+\)".*$', '\1', ''), ':p'), ':p'))

			if getftype(path) == 'file'
				let buffers[substitute(buffer, '^\s*\([^ ]\+\).*$', '\1', '')] = path
			endif
		endfor

		return buffers
	endfunction

	" Function s:getProjectBuffers() {{{2
	function s:getProjectBuffers(project, path, delete)
		let projectBuffers = []

		if a:path != ''
			silent let buffers = filter(s:getBuffers(0), 'v:val =~ ''^' . a:path . s:osSlash . '.*$''')

			if a:delete != '' && has_key(buffers, a:delete)
				call remove(buffers, a:delete)
			endif

			let projectBuffers = values(buffers)

			if g:myprojects_sort_buffers
				let projectBuffers = sort(projectBuffers)
			endif

			call map(projectBuffers, 'substitute(v:val, ''^' . a:path . ''', '''', '''')')
		endif

		return projectBuffers
	endfunction

	" Function s:displayProjectBuffers() {{{2
	function s:displayProjectBuffers(project, path, delete)
		if a:path != ''
			if s:createBufferWindow('Buffers of project ''' . a:project . ''' in ''' . a:path . '''', a:project, a:path)
				silent execute 'nnoremap <buffer> <silent> d :call <SID>deleteProjectBuffers(line(''.''), ''' . a:path .''')<CR>'
				silent execute 'au! ' . s:plugin . ' BufWinLeave <buffer> au! ' . s:plugin . ' BufNew,BufDelete ' . a:path . '/*'
			endif

			silent execute 'augroup ' . s:plugin
			silent execute 'au! BufNew ' . a:path . '/* call' . s:sid . 'refreshProjectBuffers(''' . a:project . ''', ''' . a:path . ''', '''')'
			silent execute 'au! BufDelete ' . a:path . '/* execute "call ' . s:sid . 'refreshProjectBuffers(''' . a:project . ''', ''' . a:path . ''', " . expand(''<abuf>'') . ")"'
			silent augroup END

			call s:putInMyProjectsWindow(s:getProjectBuffers(a:project, a:path, a:delete))
		endif
	endfunction

	" Function s:refreshProjectBuffers() {{{2
	function s:refreshProjectBuffers(project, path, delete)
		if s:refreshProjectBuffers
			let window = bufwinnr(s:sid . a:project)

			if window != -1
				silent execute window . 'wincmd w'
				call s:putInMyProjectsWindow(s:getProjectBuffers(a:project, a:path, a:delete))
				wincmd p
			endif
		endif
	endfunction

	" Function s:deleteProjectBuffers() {{{2
	function s:deleteProjectBuffers(line, path)
		let line = a:path . getline(a:line)

		if line != ''
			let buffer = bufnr(line)

			if buffer != -1
				if getbufvar(buffer, '&modified') == 1
					call s:error('Sorry, no write since last change for buffer ' . line . ', unable to delete')
				else
					let position = getpos('.')
					let currentBuffer = bufnr('%')

					silent! execute 'bdelete ' . buffer

					if !s:quitVim()
						silent! execute bufwinnr(currentBuffer) . ' wincmd w'
						call setpos('.', position)
					endif
				endif
			endif
		endif
	endfunction

	" Function s:echoVersion() {{{2
	function s:echoVersion()
		call s:echo('Version ' . s:version . ' - ' . s:email . ' - (c) ' . s:author . ' ' . s:copyright . ' - ' . s:webSite)
	endfunction

	" Function s:echoMyprojectsFile() {{{2
	function s:echoMyprojectsFile()
		call s:echo('Currently used file ' . g:myprojects_file)
	endfunction

	" Function s:getSessionFile() {{{2
	function s:getSessionFile(line)
		let path = ''

		let projectPath = s:getProjectPath(a:line)

		if s:windowsOs
			let projectPath = substitute(projectPath, '^[a-zA-Z]:\(.*\)', '', '')
		endif

		if projectPath != ''
			let path = g:myprojects_sessions_directory . projectPath
		endif

		return path
	endfunction

	" Function s:saveSession() {{{2
	function s:saveSession(line)
		call s:goToMyProjectsWindow()

		try
			if !isdirectory(g:myprojects_sessions_directory)
				call s:mkdir(g:myprojects_sessions_directory)
			endif

			if !isdirectory(g:myprojects_sessions_directory)
				throw 'Unable to create session, directory ' . g:myprojects_sessions_directory . ' does not exist.'
			else
				let session = s:getSessionFile(a:line)

				if session == ''
					throw 'Unable to create session.'
				else
					let head = fnamemodify(session, ':h')

					let headType = getftype(head)

					if headType == ''
						call s:mkdir(head)
					elseif headType != 'dir'
						throw 'Path ' . head . ' exists but it is not a directory.'
					elseif !filewritable(head)
						throw 'Unable to write in directory ' . head . ' to save session.'
					endif

					silent let buffers = s:getRawBuffers(1)

					for buffer in buffers
						if buffer =~# '\s"' . s:sid
							silent execute 'bwipeout ' . substitute(buffer, '^\s*\([0-9]\+\).\+$', '\1', '')
						endif
					endfor

					execute 'mksession! ' . session

					call s:echo('Session saved in ''' . session . '''.')
				endif
			endif
		catch /.*/
			call s:error(v:exception)
		endtry
	endfunction

	" Function s:loadSession() {{{2
	function s:loadSession(line)
		call s:goToMyProjectsWindow()

		let session = s:getSessionFile(a:line)

		if !filereadable(session)
			call s:error('Unable to read session file ''' . session . '''.')
		else
			let s:quitVimIfMyProjectsIsAlone = 0
			execute 'source ' . session
			let s:quitVimIfMyProjectsIsAlone = 1

			if has('syntax') && g:myprojects_syntax
				syntax on
			endif

			call s:echo('Session loaded from ''' . session . '''.')
		endif
	endfunction

	" Function s:deleteSession() {{{2
	function s:deleteSession(line)
		call s:goToMyProjectsWindow()

		let session = s:getSessionFile(a:line)

		if !filereadable(session) || delete(session) != 0
			call s:error('Unable to delete session file ' . session . '.')
		endif
	endfunction
	
	" Function s:getMyProjects() {{{2
	function s:getMyProjects(line)
		let myprojects = {}

		let name = s:unescape(s:getName(a:line))

		if name != ''
			let myprojects[name] = {}

			let attributes = {}

			let pathAttribute = s:extractPathFromLine(a:line)

			if pathAttribute != ''
				let attributes['path'] = pathAttribute
			endif

			let cdAttribute = s:extractCdFromLine(a:line, 0)

			if cdAttribute != ''
				let attributes['cd'] = cdAttribute
			endif

			if s:hasFilter(a:line)
				let attributes['filter'] = s:extractFilterFromLine(a:line)
			endif

			let mappings = s:extractMappingsFromLine(a:line)

			if !empty(mappings)
				let attributes['mappings'] = mappings
			endif

			let make = s:extractMakeFromLine(a:line)

			if !empty(make)
				let attributes['make'] = make
			endif

			let errorFormat = s:extractErrorFormatFromLine(a:line)

			if !empty(errorFormat)
				let attributes['errorformat'] = errorFormat
			endif

			let test = s:extractTestFromLine(a:line)

			if !empty(test)
				let attributes['test'] = test
			endif

			let refresh = s:extractRefreshFromLine(a:line)

			if !empty(refresh)
				let attributes['refresh'] = refresh
			endif

			if !empty(attributes)
				let myprojects[name]['attributes'] = attributes
			endif

			if s:isFolder(a:line)
				let oldPosition = s:setCursorPosition([0, a:line, 1])
				let files = []
				let endLine = s:getLastFolderLine(a:line)
				let regex = '^' . repeat('\t', (s:indent(a:line) + 1)) . '[^\t]\+$'
				let file = search(regex, '', endLine)

				while file
					call add(files, s:getMyProjects(file))
					let file = search(regex, '', endLine)
				endwhile

				if !empty(files)
					let myprojects[name]['files'] = files
				endif

				call s:setCursorPosition(oldPosition)
			endif
		endif

		return myprojects
	endfunction

	" Function s:buildMyProjects() {{{2
	function s:buildMyProjects(path, filter, myprojects, refresh, indent)
		let myprojects = ''

		let filter = a:filter

		for [name, meta] in items(a:myprojects)
			let path = resolve(s:unescape(!has_key(meta, 'attributes') || !has_key(meta['attributes'], 'path') ? a:path . s:osSlash . name : meta['attributes']['path']))
			let refresh = a:refresh

			if s:pathExists(path) && (getftype(path) == 'dir' || filter == '' || match(name, filter) != -1)
				let myprojects .= repeat("\t", a:indent) . s:escape(name)

				if has_key(meta, 'attributes')
					if has_key(meta['attributes'], 'path')
						let myprojects .= '=' . s:escape(s:cleanPath(meta['attributes']['path']))
					endif

					if has_key(meta['attributes'], 'cd') && meta['attributes']['cd'] != ''
						let myprojects .= ' cd="' . s:escape(s:cleanPath(meta['attributes']['cd'])) . '"'
					endif

					if has_key(meta['attributes'], 'filter')
						let myprojects .= ' filter="' . meta['attributes']['filter'] . '"'
						let filter = meta['attributes']['filter']
					endif

					if has_key(meta['attributes'], 'make') && meta['attributes']['make'] != ''
						let myprojects .= ' make="' . meta['attributes']['make'] . '"'
					endif

					if has_key(meta['attributes'], 'errorformat') && meta['attributes']['errorformat'] != ''
						let myprojects .= ' errorformat="' . meta['attributes']['errorformat'] . '"'
					endif

					if has_key(meta['attributes'], 'refresh') && meta['attributes']['refresh'] != ''
						let refresh = meta['attributes']['refresh']
					endif

					if has_key(meta['attributes'], 'test') && meta['attributes']['test'] != ''
						let myprojects .= ' test="' . meta['attributes']['test'] . '"'
					endif

					if has_key(meta['attributes'], 'mappings')
						for [index, mapping] in items(meta['attributes']['mappings'])
							let myprojects .= ' F' . index . '="' . mapping . '"'
						endfor
					endif
				endif

				if getftype(path) != 'dir'
					let myprojects .= "\n"
				else
					let myprojects .= ' refresh="' . localtime() . '"' . "\n"

					let subFiles = !has_key(meta, 'files') ? [] : meta['files']

					let files = ''

					for subName in subFiles
						let files .= s:buildMyProjects(path, filter, subName, refresh, a:indent + 1)
					endfor

					let notInMyProjectsFiles = s:getFilesNotInMyprojects(path, filter, a:indent, refresh, subFiles)

					if g:myprojects_new_file_on_bottom
						let files .= notInMyProjectsFiles
					else
						let files = notInMyProjectsFiles . files
					endif

					if files != '' || g:myprojects_display_empty_folder
						let myprojects .= files
					else
						let myprojects = ''
					endif
				endif
			endif
		endfor

		return myprojects
	endfunction

	" Function s:isInMyprojects() {{{2
	function s:isInMyprojects(name, files)
		for file in a:files
			if has_key(file, a:name)
				return 1
			endif
		endfor

		return 0
	endfunction

	" Function s:getFilesNotInMyprojects() {{{2
	function s:getFilesNotInMyprojects(path, filter, indent, refresh, myprojects)
		let myprojects = ''

		let path = resolve(a:path)

		if getftype(path) != 'dir'
			call s:warningMessage('Path ''' . path . ''' is not a directory.')
		else
			let cwd = getcwd()

			try
				silent execute 'lcd ' . fnameescape(path)

				for globName in sort(filter(split(glob('*') . "\n" . glob('.*'), "\n"), 'v:val != "." && v:val != ".."'))
					if !s:isInMyprojects(globName, a:myprojects) && getftime(fnamemodify(globName, ':p')) > a:refresh
						let myprojects .= s:buildMyProjects(path, a:filter, {globName : {}}, a:refresh, a:indent + 1)
					endif
				endfor

				silent execute 'lcd ' . fnameescape(cwd)
			catch /E472/
				call s:warningMessage('Unable to go to working directory ''' . path . '''.')
			endtry
		endif

		return myprojects
	endfunction

	" Function s:getLineOfPath() {{{2
	function s:getLineOfPath(path)
		let line = 0

		if getftype(a:path) == 'file'
			if !s:goToMyProjectsWindow()
				call s:openMyProjectsWindow()
			endif

			let line = s:searchPath(a:path, 0)
		endif

		return line
	endfunction

	" Function s:openFromMyProjectsWindow() {{{2
	function s:openFromMyProjectsWindow(command, path)
		let path = resolve(a:path)

		let line = s:getLineOfPath(path)

		if line > 0
			call s:edit(a:command, line)
		endif
	endfunction

	" Function s:openTest() {{{2
	function s:openTest(command, path, test)
		let line = s:getLineOfPath(a:path)

		if line == 0
			wincmd p
		else
			let firstProjectLine = s:getProjectLine(line)
			let lastProjectLine = s:getLastFolderLine(firstProjectLine)
			let oldPosition = s:setCursorPosition([0, firstProjectLine, 1])

			let line = search('\%(^\t*\|\/\)' . a:test . '\%(\s\|=\|$\)', 'W', lastProjectLine)

			let lines = {}

			while line > 0
				let lines[line] = s:getPath(line)
				let line = search('\%(^\t*\|\/\)' . a:test . '\%(\s\|=\|$\)', 'W', lastProjectLine)
			endwhile

			call s:setCursorPosition(oldPosition)

			let linesLength = len(lines)

			if len(linesLength) <= 0
				call s:error('No test file ''' . a:test . ''' found for ''' . a:path . ''')
			else
				let line = 0

				if linesLength == 1
					let line = get(keys(lines), 0)
				else
					let list = ['There is several test file ''' . a:test . ''' found for ''' . a:path . ''':']
					let keys = {}

					let index = 1

					for [line, path] in items(lines)
						let list = add(list, index . '. ' . path)
						let keys[index] = line
						let index += 1
					endfor

					let index = inputlist(list)

					if has_key(keys, index)
						let line = keys[index]
					endif
				endif

				if line > 0
					try
						call s:edit(a:command, line)
					catch /.*/
						call s:error(v:exception)
					endtry
				endif
			endif

		endif
	endfunction

	" Function s:loadMyProjectsAttributes() {{{2
	function s:loadMyProjectsAttributes()
		let bufferName = fnamemodify(bufname('%'), ':p')

		if bufferName !~ g:myprojects_file && bufferName !~ '^' . s:sid && s:loadAttributes == 1 && !exists('b:myprojectsAttributesLoaded')
			call s:openFromMyProjectsWindow('edit', bufferName)
			let b:myprojectsAttributesLoaded = 1
		endif
	endfunction

	" Function s:resolveSvnConflict() {{{2
	function s:resolveSvnConflict(command, path)
		try
			call s:openFromMyProjectsWindow(a:command, getline('.'))
			execute 'au! ' . s:plugin . ' BufWritePost <buffer> call ' . s:sid . 'svnMarkAsResolved(''' . a:path . ''')'
		catch /.*/
			call s:error(v:exception)
		endtry
	endfunction

	" Function s:openFromSvnWindow() {{{2
	function s:openFromSvnWindow(command)
		try
			call s:openFromMyProjectsWindow(a:command, substitute(getline('.'), '^[[:space:]ACDIMRX?!~][[:space:]CM][[:space:]L][[:space:]+][[:space:]SX][[:space:]K][[:space:]C]\s', '', ''))
		catch /.*/
			call s:error(v:exception)
		endtry
	endfunction

	" Function s:openFromBuffersWindow() {{{2
	function s:openFromBuffersWindow(command, path)
		try
			call s:openFromMyProjectsWindow(a:command, a:path . getline('.'))
		catch /.*/
			call s:error(v:exception)
		endtry
	endfunction

	" Function s:svnDiffFromMyProjectWindow() {{{2
	function s:svnDiffFromMyProjectWindow(path)
		let path = resolve(substitute(a:path, '^[[:space:]A-Z]\+\s', '', ''))

		let line = s:getLineOfPath(path)

		if line > 0
			call s:svnDiff(line)
		endif
	endfunction

	" Function s:svnDiffFromSvnWindow() {{{2
	function s:svnDiffFromSvnWindow()
		call s:svnDiffFromMyProjectWindow(getline('.'))
	endfunction


	" Function s:setCursorPosition() {{{2
	function s:setCursorPosition(position)
		let oldPosition = getpos('.')
		call setpos('.', a:position)

		return oldPosition
	endfunction
	" Function s:svnDiffFromBuffersWindow() {{{2
	function s:svnDiffFromBuffersWindow(path)
		let s:refreshProjectBuffers = 0

		call s:svnDiffFromMyProjectWindow(a:path . getline('.'))

		let s:refreshProjectBuffers = 1
	endfunction

	" Function s:sid() {{{2
	function s:sid()
		return matchstr(expand('<sfile>'), '<SNR>\d\+_\zesid$')
	endfunction

	" Function s:setLocal() {{{2
	function s:setLocal(name, bool)
		silent execute 'setlocal ' . (a:bool ? a:name : 'no' . a:name)
	endfunction

	" Function s:substitute() {{{2
	function s:substitute(line, search, replace, flags)
		silent execute ':' . a:line . 's/' . a:search . '/' . escape(a:replace, '/') . '/' . a:flags

		if &hlsearch
			set nohlsearch
		endif
	endfunction

	" Function s:input() {{{2
	function s:input(prompt, ...)
		redraw

		let prompt = s:getPromptedString(a:prompt)

		if a:0 == 0
			return input(prompt)
		elseif a:0 == 1
			return input(prompt, a:1)
		elseif a:0 == 2
			return input(prompt, a:1, a:2)
		endif
	endfunction

	" Function s:inputSecret() {{{2
	function s:inputSecret(prompt, ...)
		redraw

		let prompt = s:getPromptedString(a:prompt)
			call s:error(v:exception)

		if a:0 == 0
			return inputsecret(prompt)
		elseif a:0 == 1
			return inputsecret(prompt, a:1)
		endif
	endfunction
	" Function s:indent() {{{2
	function s:indent(line)
		let indent = indent(a:line)

		if indent > 0
			let indent = indent / &tabstop
		endif

		return indent
	endfunction

	" Function s:put() {{{2
	function s:put(data, line)
		if a:data != ''
			silent execute a:line - 1 . 'put =a:data'

			let nextLine = line('.') + 1

			if nextLine <= line('$') && getline(nextLine) =~ '^$'
				silent! execute ':' . nextLine . 'd'
			endif

			call s:goToLine(a:line)
		endif
	endfunction

	" Function s:putInMyProjectsWindow() {{{2
	function s:putInMyProjectsWindow(lines)
		setlocal modifiable
		silent! %d
		silent! 0put =substitute(join(a:lines, \"\n\"), '\n\s*$', '', '')
		silent! $d
		setlocal nomodifiable
		setlocal nomodified
		setlocal buftype=nofile

		call s:goToLine(1)
		call s:setWindowHeight(line('$'))
	endfunction

	" Function s:getPromptedString() {{{2
	function s:getPromptedString(message)
		return s:prompt .  substitute(a:message, '^@', '', 'g')
	endfunction

	" Function s:getPromptedStringLength() {{{2
	function s:getPromptedStringLength(message)
		return strlen(s:getPromptedString(a:message)) + 20
	endfunction

	" Function s:echo() {{{2
	function s:echo(message)
		redraw
		echo s:getPromptedString(a:message)
	endfunction

	" Function s:echoPath() {{{2
	function s:echoPath()
		if winbufnr(0) == bufnr(g:myprojects_file)
			let path = s:getPath(line('.'))

			if s:getPromptedStringLength(path) > &columns
				let path = pathshorten(path)

				if s:getPromptedStringLength(path) > &columns
					let path = s:getName(line('.'))

					if s:getPromptedStringLength(path) > &columns
						let path = ''
					endif
				endif
			endif

			if path != ''
				call s:echo(path)
			endif
		endif
	endfunction

	" Function s:error() {{{2
	function s:error(message)
		echohl ErrorMsg
		call s:echo(a:message)
		echohl Normal
	endfunction

	" Function s:message() {{{2
	function s:message(message)
		redraw
		echomsg s:getPromptedString(a:message)
	endfunction

	" Function s:errorMessage() {{{2
	function s:errorMessage(message)
		echohl ErrorMsg
		call s:message(a:message)
		echohl Normal
	endfunction

	" Function s:warningMessage() {{{2
	function s:warningMessage(message)
		echohl WarningMsg
		call s:message(a:message)
		echohl Normal
	endfunction

	" Function s:escape() {{{2
	function s:escape(path)
		return substitute(a:path, '\%(\\\)\@<!\([ "=]\)', '\\\1', 'g')
	endfunction

	" Function s:unescape() {{{2
	function s:unescape(path)
		return substitute(a:path, '\\\([ ="]\)', '\1', 'g')
	endfunction

	" Function s:goToLine() {{{2
	function s:goToLine(line)
		silent execute 'normal! ' . a:line . 'G'
		silent normal |
	endfunction

	" Function s:getFiles() {{{2
	function s:getFiles(path, filter)
		let files = ''

		let path = resolve(a:path)

		if !isdirectory(path)
			call s:warningMessage('Path ''' . path . ''' is not a directory.')
		else
			let cwd = getcwd()

			try
				silent execute 'lcd ' . path

				for inode in sort(split(glob('*'), "\n"))
					let inode = resolve(inode)

					if isdirectory(inode)
						let files .= ' ' . s:getFiles(path . s:osSlash . inode, a:filter)
					elseif a:filter == '' || match(inode, a:filter) != -1
						let files .= ' ' . path . s:osSlash . inode
					endif
				endfor

				silent execute 'lcd ' . cwd
			catch /E472/
				call s:warningMessage('Unable to go to directory ''' . path . '''.')
			endtry
		endif

		return files
	endfunction

	" Function s:pathExists() {{{2
	function s:pathExists(path)
		let type = getftype(a:path)

		return type == 'dir' || type == 'file'
	endfunction

	" Function s:searchPath() {{{2
	function s:searchPath(path, line)
		let line = 0

		let file = fnamemodify(a:path, ':t')

		if file != ''
			let oldPosition = s:setCursorPosition([0, a:line, 1])

			let line = search('\%(^\t*\|\/\)' . file . '\%(\s\|=\|$\)', 'W')

			while line > 0
				let path = s:getPath(line)

				if a:path == path
					call s:setCursorPosition(oldPosition)
					return line
				else
					let line = search('\%(^\t*\|\/\)' . file . '\%(\s\|=\|$\)', 'W')
				endif
			endwhile

		endif

		call s:setCursorPosition(oldPosition)
		return line
	endfunction

	" Function s:cleanPath() {{{2
	function s:cleanPath(path)
		return substitute(a:path, escape(s:osSlash, '\') . '\+$', '', '')
	endfunction

	" Function s:mkdir() {{{2
	function s:mkdir(path)
		if !exists("*mkdir")
			throw 'Unable to create directory ' . a:path . ', mkdir() function does not exist.')
		else
			 call mkdir(a:path, 'p', 0700)

			if getftype(a:path) != 'dir'
				throw 'Unable to create directory ' . a:path . '.')
			endif
		endif
	endfunction

	" Function s:moveWindowToTopLeftCorner() {{{2
	function s:moveWindowToTopLeftCorner()
		silent wincmd H
	endfunction

	" Function s:setWindowWidth() {{{2
	function s:setWindowWidth(width)
		silent execute 'vertical resize ' . a:width
	endfunction

	" Function s:setWindowHeight() {{{2
	function s:setWindowHeight(height)
		silent execute 'resize ' . a:height
	endfunction

	" Restore cpo {{{2
	let &cpo= s:keepCpo
	unlet s:keepCpo

	" Enable myprojects {{{2
	let g:myprojects_enable = 1
endif

" finish {{{1
finish
" vim:filetype=vim foldmethod=marker shiftwidth=3 tabstop=3
