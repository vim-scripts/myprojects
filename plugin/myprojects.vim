"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy (fhardy at noparking.net)
" Date:						Fri Mar 27 14:24:05 CET 2009
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

	" Initialize script's variable {{{2
	let s:plugin = 'myprojects'
	let s:version = '0.0.60'
	let s:copyright = '2009'
	let s:author = 'Frédéric Hardy'
	let s:email = 'fhardy at noparking.net'
	let s:webSite = 'http://blog.mageekbox.net'
	let s:prompt = '[' . s:plugin . '] '
	let s:buffer = -1
	let s:workingBuffer = -1
	let s:oldWidth = 0
	let s:windowsOs = has('win16') || has('win32') || has('win64')
	let s:osSlash = s:windowsOs ? '\' : '/'
	let s:home = expand('$HOME')
	let s:closeIfAlone = 1

	" Initialize command and mapping {{{2
	command -nargs=? -complete=file MyProjectsToggle call <sid>toggle()

	if !hasmapto('<Plug>MyProjectsToggle')
		map <unique> <silent> <Leader>p <Plug>MyProjectsToggle
	endif

	noremap <unique> <script> <Plug>MyProjectsToggle <sid>toggle
	noremap <sid>toggle :call <sid>toggleMyProjects()<CR>

	command -nargs=? -complete=file MyProjectsGoTo call <sid>goTo()

	if !hasmapto('<Plug>MyProjectsGoTo')
		map <unique> <silent> <Leader>P <Plug>MyProjectsGoTo
	endif

	noremap <unique> <script> <Plug>MyProjectsGoTo <sid>goTo
	noremap <sid>goTo :call <sid>goToMyProjectsWindow(1)<CR>

	" Function s:myProjects() {{{2
	function s:myProjects()
		call s:initVariable('g:myprojects_width', 30)
		call s:initVariable('g:myprojects_file', s:home . s:osSlash . '.' . s:plugin)
		call s:initVariable('g:myprojects_tags_file', '.tags')
		call s:initVariable('g:myprojects_auto_close', 0)
		call s:initVariable('g:myprojects_auto_resize', 0)
		call s:initVariable('g:myprojects_resize_step', 10)
		call s:initVariable('g:myprojects_syntax', 1)
		call s:initVariable('g:myprojects_display_empty_folder', 0)
		call s:initVariable('g:myprojects_version_control_system', 'svn')
		call s:initVariable('g:myprojects_display_number', 0)
		call s:initVariable('g:myprojects_cursorline', 1)
		call s:initVariable('g:myprojects_cursorcolumn', 1)
		call s:initVariable('g:myprojects_foldcolumn', 0)
		call s:initVariable('g:myprojects_display_path_in_statusline', 1)
		call s:initVariable('g:myprojects_tags_generator', 'exctags')
		call s:initVariable('g:myprojects_sessions_directory', s:home . s:osSlash . '.vim' . s:osSlash . 'myprojects_sessions')
		call s:initVariable('g:myprojects_new_file_on_bottom', 1)
		call s:initVariable('g:myprojects_sort_svn', 1)
		call s:initVariable('g:myprojects_sort_buffers', 1)

		if s:goToMyProjectsWindow(0) < 0
			execute 'leftabove vertical new ' . fnameescape(g:myprojects_file)

			let s:buffer = winbufnr(0)

			nnoremap <buffer> <silent> <LeftMouse> <LeftMouse>:call <sid>echo(<sid>getPath(line('.')))<CR>
			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <sid>open('edit')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <sid>open('edit')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <sid>open('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <sid>open('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <sid>open('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <sid>open('vs')<CR>
			nnoremap <buffer> <silent> <C-Tab> :call <sid>goToEditWindow()<CR>
			nnoremap <buffer> <silent> <C-Right> :call <sid>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-l> :call <sid>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Left> :call <sid>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-h> :call <sid>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Space> :call <sid>toggleFullscreen()<CR>
			nnoremap <buffer> <silent> <LocalLeader>b :call <sid>exploreProjectBuffers(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>c :call <sid>create(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>r :call <sid>refresh(line('.'), 0)<CR>
			nnoremap <buffer> <silent> <LocalLeader>R :call <sid>refresh(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>g :call <sid>grep(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>t :call <sid>generateTags(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>e :call <sid>explore('E')<CR>
			nnoremap <buffer> <silent> <LocalLeader>E :call <sid>explore('Se')<CR>
			nnoremap <buffer> <silent> <LocalLeader>a :call <sid>append(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>d :call <sid>delete(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>s :call <sid>saveSession(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>S :call <sid>loadSession(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader><A-s> :call <sid>deleteSession(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>p :call <sid>setPath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>P :call <sid>updatePath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>f :call <sid>setFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>F :call <sid>updateFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>w :call <sid>setCd(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>W :call <sid>updateCd(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>m :call <sid>setMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>M :call <sid>updateMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>k :call <sid>setMake(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>K :call <sid>updateMake(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>ef :call <sid>setErrorFormat(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>Ef :call <sid>updateErrorFormat(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>i :call <sid>echo('Path: ' . <sid>getPath(line('.')))<CR>
			nnoremap <buffer> <silent> <LocalLeader>v :call <sid>echoVersion()<CR>
			nnoremap <buffer> <silent> <LocalLeader>V :call <sid>echoMyprojectsFile()<CR>
			nnoremap <buffer> <silent> <LocalLeader>ss :call <sid>svnStatus(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>su :call <sid>svnUpdate(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>sa :call <sid>svnAdd(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>sr :call <sid>svnRevert(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>sd :call <sid>svnDiff(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>sc :call <sid>svnCommit(line('.'))<CR>

			if g:myprojects_display_path_in_statusline
				nnoremap <buffer> <silent> <Down> <Down>:call <sid>echo(<sid>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> j j:call <sid>echo(<sid>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> <Up> <Up>:call <sid>echo(<sid>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> k k:call <sid>echo(<sid>getPath(line('.')))<CR>
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

			let titlestring=&titlestring
			let &titlestring=s:prompt

			silent execute 'augroup ' . s:plugin
			silent au!
			silent execute 'au WinEnter * call' . s:sid . 'floatMyProjects()'
			silent execute 'au WinEnter <buffer> call' . s:sid . 'enterInMyProjectsWindow()'
			silent execute 'au WinEnter <buffer> let &titlestring ="' . &titlestring . '"'

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

			silent execute 'au WinLeave <buffer> let &titlestring ="' . titlestring . '"'
			silent augroup END

			if !has('syntax') || !g:myprojects_syntax
				setlocal filetype=""
			else
				silent execute 'setlocal filetype=' . s:plugin
				syntax on
			endif

			call s:floatWindow()
			call s:resizeWindow(g:myprojects_width)

			if foldlevel(line('.'))
				silent normal! zo
			endif

			redraw

			if g:myprojects_display_path_in_statusline
				call <sid>echo(<sid>getPath(line('.')))
			endif
		endif
	endfunction

	" Function s:wipeoutMyProjects() {{{2
	function s:wipeoutMyProjects()
		try
			silent bwipeout
		catch E89
			if s:input('Save [y|N] ? ', '') != 'y'
				return 0
			else
				silent write
				silent bwipeout
			endif
		endtry

		return 1
	endfunction

	" Function s:getMyProjectsWindow() {{{2
	function s:getMyProjectsWindow()
		return bufwinnr(s:buffer)
	endfunction

	" Function s:getWorkingWindow() {{{2
	function s:getWorkingWindow()
		return bufwinnr(s:workingBuffer)
	endfunction

	" Function s:goToEditWindow() {{{2
	function s:goToEditWindow()
		wincmd p

		let window = winnr()

		if window == s:getMyProjectsWindow() || window == s:getWorkingWindow()
			2 wincmd w

			let window = winnr()

			if window == s:getMyProjectsWindow() || window == s:getWorkingWindow()
				call s:goToMyProjectsWindow(0)
				vnew
			endif
		endif
	endfunction

	" Function s:goToMyProjectsWindow() {{{2
	function s:goToMyProjectsWindow(open)
		let window = s:getMyProjectsWindow()

		if window != -1
			silent execute window . 'wincmd w'
		elseif a:open
			call s:myProjects()
			call s:goToMyProjectsWindow(0)
		endif

		return window
	endfunction

	" Function s:goToWorkingWindow() {{{2
	function s:goToWorkingWindow(statusline, buffer)
		let window = s:getWorkingWindow()

		let buffer = s:sid . a:buffer

		if window == -1
			silent execute 'botright split'
		else
			silent execute window . 'wincmd w'
		endif

		let bufferExists = bufexists(buffer)

		if bufferExists
			silent execute 'buffer ' . buffer
		else
			silent execute 'edit ' . buffer

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

			silent execute 'setlocal filetype=' . s:plugin
			silent execute 'setlocal statusline=' . escape(s:prompt, ' ') . escape(a:statusline, ' ')

			let b:titlestring = &titlestring
			let &titlestring = &statusline

			silent execute 'augroup ' . s:plugin
			if g:myprojects_cursorline
				silent au WinEnter <buffer> set cursorline nocursorcolumn
			else
				silent au WinEnter <buffer> set nocursorline nocursorcolumn
			endif

			silent execute 'au WinEnter <buffer> let &titlestring ="' . &statusline . '"'
			silent execute 'au WinLeave <buffer> let &titlestring ="' . b:titlestring . '"'
			silent augroup END

		endif

		if has('syntax') && g:myprojects_syntax
			syntax on
		endif

		let s:workingBuffer = winbufnr(0)

		call s:floatMyProjects()

		return bufferExists
	endfunction

	" Function s:goToSvnWindow()
	function s:goToSvnWindow(statusline, buffer)
		let bufferExists = s:goToWorkingWindow(a:statusline, a:buffer)

		if !bufferExists
			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <sid>openFromSvnWindow('edit')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <sid>openFromSvnWindow('edit')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <sid>openFromSvnWindow('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <sid>openFromSvnWindow('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <sid>openFromSvnWindow('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <sid>openFromSvnWindow('vs')<CR>
			nnoremap <buffer> <silent> <LocalLeader>sd :call <sid>svnDiffFromSvnWindow()<CR>
		endif

		return bufferExists
	endfunction

	" Function s:goToBuffersWindow()
	function s:goToBuffersWindow(statusline, buffer, path)
		let bufferExists = s:goToWorkingWindow(a:statusline, a:buffer)

		if !bufferExists
			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			silent execute 'noremap <buffer> <silent> <Return> :call <sid>openFromBuffersWindow(''edit'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <2-Leftmouse> :call <sid>openFromBuffersWindow(''edit'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <S-Return> :call <sid>openFromBuffersWindow(''sp'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <S-2-Leftmouse> :call <sid>openFromBuffersWindow(''sp'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <C-Return> :call <sid>openFromBuffersWindow(''vs'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <C-2-Leftmouse> :call <sid>openFromBuffersWindow(''vs'', ''' . a:path . ''')<CR>'
			silent execute 'noremap <buffer> <silent> <LocalLeader>sd :call <sid>svnDiffFromBuffersWindow(''' . a:path . ''')<CR>'
		endif

		return bufferExists
	endfunction

	" Function s:enterInMyProjectsWindow() {{{2
	function s:enterInMyProjectsWindow()
		if s:closeIfAlone
			let windows = winnr('$')
			let myProjects = 0

			while windows > 0
				if winbufnr(windows) == s:buffer
					let myProjects += 1
				endif

				let windows -= 1
			endwhile

			if myProjects == winnr('$')
				confirm qall
			endif
		endif
	endfunction

	" Function s:floatMyProjects() {{{2
	function s:floatMyProjects()
		let currentWindow = winnr()
		let myProjectsWindow = s:getMyProjectsWindow()
		let workingWindow = s:getWorkingWindow()

		if myProjectsWindow != -1 || workingWindow != -1
			if myProjectsWindow != -1 && myProjectsWindow != 1
				silent execute myProjectsWindow . 'wincmd w'
				let width = winwidth(0)
				silent wincmd H
				silent execute 'vertical resize ' . width
			endif

			if workingWindow != -1
				silent execute workingWindow . 'wincmd w'
				let height = winheight(0)
				silent wincmd J
				silent execute 'resize ' . height
			endif

			silent execute currentWindow . 'wincmd w'
		endif
	endfunction

	" Function s:closeMyProjectsWindow() {{{2
	function s:closeMyProjectsWindow()
		if s:goToMyProjectsWindow(0) == -1
			return 0
		else
			hide
			return 1
		endif
	endfunction

	" Function s:toggleMyProjects() {{{2
	function s:toggleMyProjects()
		let window = s:goToMyProjectsWindow(0)

		call s:myProjects()

		if window != -1
			call s:closeMyProjectsWindow()
		endif
	endfunction

	" Function s:toggleFullscreen() {{{2
	function s:toggleFullscreen()
		if s:oldWidth == 0
			let s:oldWidth = winwidth(0)
			call s:resizeWindow('')
		else
			call s:resizeWindow(s:oldWidth)
			let s:oldWidth = 0
		endif
	endfunction

	" Function s:foldtext() {{{2
	function s:foldtext()
		let text = repeat(' ', indent(v:foldstart)) . '+ ' . substitute(getline(v:foldstart), '^\t*\([^\t].\{-}\)\%(\%(\\\)\@<!\%(=\| \).*\|$\)', '\1', 'g') . '-[' . (v:foldend - v:foldstart) . ']'

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
		return indent(a:line) < indent(nextnonblank(a:line + 1))
	endfunction

	" Function s:getFirstFolderLine() {{{2
	function s:getFirstFolderLine(line)
		let line = 0

		let indent = s:indent(a:line)

		if indent > 0
			let position = getpos('.')

			call setpos('.', [0, a:line, 1])

			let line = search('^' . repeat('	', indent - 1) . '[^\t]\+$', 'bnW')

			call setpos('.', position)
		endif

		return line
	endfunction

	" Function s:getLastFolderLine() {{{2
	function s:getLastFolderLine(line)
		let endLine = 0

		let line = a:line

		if !s:isFolder(line)
			let line = s:getFirstFolderLine(line)
		endif

		if line > 0
			let endLine = search('^\t\{0,' . s:indent(line) . '\}[^\t].*$', 'nW') - 1

			if endLine <= 0
				let endLine = line('$')
			endif
		endif

		return endLine
	endfunction

	" Function s:getRootLine() {{{2
	function s:getRootLine(line)
		let line = 0

		let indent = indent(a:line)

		if indent != -1
			let line = indent == 0 ? a:line : search('^[^\t]\+$', 'bnW')
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

		let isAbsolutePath = s:isAbsolutePath(path)

		if !isAbsolutePath
			let folderLine = s:getFirstFolderLine(a:line)

			while !isAbsolutePath && folderLine > 0
				let path = s:extractPath(folderLine) . s:osSlash . path
				let isAbsolutePath = s:isAbsolutePath(path)
				let folderLine = s:getFirstFolderLine(folderLine)
			endwhile
		endif

		return resolve(s:unescape(path))
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

		let line = s:getRootLine(a:line)

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

		let line = a:line

		while line > 0 && line <= line('$') && len(mappings) < 12
			let currentLine = getline(line)

			let index = 1

			while index <= 12
				if !has_key(mappings, index) && s:hasMapping('F' . index, line)
					let mapping = substitute(currentLine, '.*\s\+F' . index . '="\([^"]\+\)".*', '\1', '')

					if mapping != ''
						let mappings[line] = {index : mapping}
					endif
				endif

				let index += 1
			endwhile

			let line = s:getFirstFolderLine(line)
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

	" Function s:isAbsolutePath() {{{2
	function s:isAbsolutePath(path)
		return s:windowsOs ? a:path =~ '^.:\(\\\|\/\)' : a:path =~ '^/'
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

	" Function s:inputPath() {{{2
	function s:inputPath(message, emptyPath)
		let path = s:input(a:message, '', 'file')

		if a:emptyPath == 0 && path == ''
			throw 'Path must not be empty.'
		elseif !s:isAbsolutePath(path)
			throw 'Path must be absolute.'
		elseif !s:pathExists(path)
			throw 'Path ' . path . ' does not exist.'
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
				cd = resolve(cd)
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
				let inputs[keys[index]] = s:input('Mapping for F' . keys[index] . ': ', '')
				redraw
			endif
		endwhile

		for [key, mapping] in items(inputs)
			let mappings[key] = mapping
		endfor

		return mappings
	endfunction

	" Function s:inputMake() {{{2
	function s:inputMake(message, value)
		return s:input(a:message, a:value)
	endfunction

	" Function s:inputErrorFormat() {{{2
	function s:inputErrorFormat(message, value)
		return s:input(a:message, a:value)
	endfunction

	" Function s:create() {{{2
	function s:create(line)
		let indent = s:indent(a:line)

		if indent >= 0
"			try
				let myprojects = {}
				let name = s:inputName('Name of new project: ')
				let myprojects[name] = {'attributes': {}, 'files': []}
				let myprojects[name]['attributes']['path'] = s:inputPath('Path of project ' . name . ': ', 0)
				let myprojects[name]['attributes']['cd'] = s:inputCd('Working directory of project ' . name . ': ', myprojects[name]['attributes']['path'], '')
				let filter = s:inputFilter('Filter of project ' . name . ': ', '')

				if filter != ''
					let myprojects[name]['attributes']['filter'] = filter
				endif

				let myprojects[name]['attributes']['make'] = s:inputMake('Make of project ' . name . ': ', '')
				let myprojects[name]['attributes']['errorformat'] = s:inputErrorFormat('Error format of project ' . name . ': ', '')
				let myprojects[name]['attributes']['mappings'] = s:inputMappings('Mappings of project ' . name . ': ', {})

				call s:echo('Create project ' . name . ' from path ' . myprojects[name]['attributes']['path'] . ', please wait...')
				call s:put(s:buildMyProjects('', filter, myprojects, indent), a:line)
				call s:echo('Project ' . name . ' created.')

				call s:echo(s:getPath(a:line))
"			catch /.*/
"				call s:error(v:exception)
"			endtry
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
				call s:echo('Refresh ' . path . ' in project ' . s:getProjectName(line) . ', please wait...')

				let myprojects = s:buildMyProjects(substitute(path, '[^' . s:osSlash . ']\+$', '', ''), s:extractFilter(line), s:getMyProjects(line), indent)

				let range = line
				let foldlevel = 0
				
				if s:isFolder(line)
					let range .= ',' . s:getLastFolderLine(line)
					let foldlevel = foldlevel(line)
				endif

				silent execute ':' . range . 'd'

				call s:put(myprojects, line)

				silent execute ':' . range . 'foldclose!'

				while foldlevel > 0
					silent normal! zo
					let foldlevel -= 1
				endwhile

				call s:echo(path . ' was refreshed.')
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
			let pattern = s:input('Grep in ' . path . ': ', '')

			if pattern != ''
				if s:isFolder(a:line)
					let files = s:getFiles(path, s:extractFilter(a:line))
				else
					let files = path
				endif

				if files != ''
					wincmd p
					silent execute 'vimgrep /' . escape(pattern, '/') . '/jg ' . files
					silent cw
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
				call s:goToEditWindow()
				silent execute a:mode . 'xplore ' . path
			endif
		endif
	endfunction

	" Function s:generateTags() {{{2
	function s:generateTags(line)
		if !executable(g:myprojects_tags_generator)
			call s:error(g:myprojects_tags_generator . ' is not available.')
		elseif g:myprojects_tags_file != ''
			let rootPath = s:getProjectPath(a:line)

			if rootPath != ''
				let tagsPath = rootPath . s:osSlash . g:myprojects_tags_file

				call s:echo('Generate tags file ' . tagsPath . ' for project ' . s:getProjectName(a:line) . ', please wait...')
				silent execute '!' . g:myprojects_tags_generator . ' -f ' . tagsPath . ' -R ' . rootPath
				call s:echo('Tags file generated and stored in ' . tagsPath . '.')
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

		let window = bufwinnr(path)

		if window != -1
			silent execute window . 'wincmd w'
			silent execute 'buffer ' . bufnr(path)
		else
			let type = getftype(path)

			if type == ''
				let head = fnamemodify(path, ':h')

				let headType = getftype(head)

				if headType == ''
					call s:mkdir(path)
				elseif headType != 'dir'
					throw 'Path ' . head . ' exists but it is not a directory'
				endif
			elseif type != 'file' && type != 'link'
				throw 'Unable to open file ' . path . ' of type ' . type . '.'
			elseif !filereadable(path)
				throw 'Unable to read file ' . path . '.'
			endif

			call  s:goToEditWindow()

			try
				silent execute a:command ' ' . fnameescape(path)
			catch E37
				if s:input('Save ' . bufname('%') . ' and load ' . path . ' ? [y/N]: ', '') != 'y'
					return 0
				else
					silent write
					silent execute a:command ' ' . fnameescape(path)
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
				let cd = fnameescape(cd)
				silent execute 'lcd ' . cd
				silent execute 'augroup ' . s:plugin
				silent execute 'au BufEnter <buffer> lcd ' . cd
				silent augroup END
			endif
		endif

		for [line, mapping] in items(mappings)
			for [key, value] in items(mapping)
				silent execute 'nmap <buffer> <silent> <F' . key . '> ' . expand(value)
			endfor
		endfor

		if make != ''
			silent execute 'setlocal makeprg=' . s:escape(make)
		endif

		if errorFormat != ''
			silent execute 'setlocal errorformat=' . s:escape(errorFormat)
		endif

		setlocal more

		if !hasmapto('<Plug>MyProjectsGoTo')
			map <buffer> <silent> <C-Tab> <Plug>MyProjectsGoTo
		endif

		if g:myprojects_auto_resize || g:myprojects_auto_close
			call s:goToMyProjectsWindow(0)

			if g:myprojects_auto_resize
				call s:resizeWindow(g:myprojects_width)
			endif

			if g:myprojects_auto_close
				call s:closeMyProjectsWindow()
			endif

			call s:goToEditWindow()
		endif

		silent execute 'nnoremap <buffer> <silent> <LocalLeader>b :call <sid>listProjectBuffers(''' . projectName. ''', ''' . projectPath . ''')<CR>'

		return 1
	endfunction

	" Function s:append() {{{2
	function s:append(line)
		let path = s:getPath(a:line)

		if path != ''
			if getftype(path) != 'file' || !filereadable(path)
				call s:error('Unable to read file ' . path . '.')
			else
				call s:goToEditWindow()
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

	" Function s:setPath() {{{2
	function s:setPath(line, refresh)
		if !s:hasPath(a:line)
			let currentPath = s:getPath(a:line)

			if currentPath != ''
				try
					let newPath = s:cleanPath(s:inputPath('Set path for ' . s:getName(a:line) . ': ', 1))

					if newPath != '' && newPath != currentPath
						call s:substitute(a:line, '\(^\t*[^\t]\%(\\ \|\f\)\+\)', '\1=' . newPath, '')

						if a:refresh
							call s:refresh(pathLine, 0)
						endif
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
					let newCd = s:inputCd('Set working directory for ' . path . ': ', path, currentCd)

					if newCd != '' && newCd != currentCd
						call s:updateAttribute(a:line, 'cd', s:escape(newCd))
					endif
				catch /.*/
					call s:error(v:exception)
				endtry
			endif
		endif
	endfunction

	" Function s:setFilter() {{{2
	function s:setFilter(line, refresh)
		let line = s:isFolder(a:line) ? a:line : s:getFirstFolderLine(a:line)

		if line > 0 && !s:hasFilter(line)
			let filter = s:getNestedFilter(line)
			let currentFilter = empty(filter) ? '' : filter[1]
			let newFilter = s:inputFilter('Set filter for ' . s:getPath(line) . ': ', currentFilter)

			if newFilter != currentFilter
				call s:updateAttribute(line, 'filter', newFilter)

				if a:refresh
					call s:refresh(line, 0)
				endif
			endif
		endif
	endfunction

	" Function s:setMappings() {{{2
	function s:setMappings(line)
		if !s:hasMapping('F[1-9][0-2]\?', a:line)
			let path = s:getPath(a:line)

			if path != ''
				for [key, mapping] in items(s:inputMappings('Set mapping for ' . path . ': ', {}))
					if mapping != ''
						call s:updateAttribute(a:line, 'F' . key, mapping)
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
					let newMake = s:inputMake('Set make for ' . path . ': ', currentMake)

					if newMake != currentMake
						call s:updateAttribute(a:line, 'make', newMake)
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
					let newErrorFormat = s:inputErrorFormat('Set error format for ' . path . ': ', currentErrorFormat)

					if newErrorFormat != currentErrorFormat
						call s:updateAttribute(a:line, 'errorformat', newErrorFormat)
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
	function s:updatePath(line, refresh)
		let path = s:getNestedPath(a:line)

		if !empty(path)
			let [pathLine, currentPath] = path

			try
				let newPath = s:cleanPath(s:inputPath('Update path for ' . s:getName(pathLine) . ': ', 1))

				if newPath != '' && newPath != currentPath
					call s:substitute(pathLine, '^.*$', s:getName(pathLine) . '=' . newPath . ' ' . substitute(substitute(getline(pathLine), '^\t*[^\t].\{-}\(\%(\\\)\@<!\%( \|=\)\)', '\1', ''), '^=.\{-}\%(\\\)\@<! \(.*$\)', '\1', ''), '')

					if a:refresh
						call s:refresh(pathLine, 0)
					endif
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

				let newCd = s:inputCd('Update working directory for ' . path . ': ', path, currentCd)

				if newCd != currentCd
					call s:updateAttribute(cdLine, 'cd', s:escape(newCd))
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:updateFilter() {{{2
	function s:updateFilter(line, refresh)
		let filter = s:getNestedFilter(a:line)

		if !empty(filter)
			let [filterLine, currentFilter] = filter

			let newFilter = s:inputFilter('Update filter for ' . s:getPath(filterLine) . ': ', currentFilter)

			if newFilter != currentFilter
				call s:updateAttribute(filterLine, 'filter', newFilter)

				if a:refresh
					call s:refresh(filterLine, 0)
				endif
			endif
		endif
	endfunction

	" Function s:updateMappings() {{{2
	function s:updateMappings(line)
		let currentMappings = s:getNestedMappings(a:line)

		if !empty(currentMappings)
			let lines = {}
			let mappings = {}

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

				let newMake = s:inputMake('Update make for ' . path . ': ', currentMake)

				if newMake != currentMake
					call s:updateAttribute(makeLine, 'make', newMake)
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

				let newErrorFormat = s:inputErrorFormat('Update error format for ' . path . ': ', currentErrorFormat)

				if newErrorFormat != currentErrorFormat
					call s:updateAttribute(errorFormatLine, 'errorformat', newErrorFormat)
				endif
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:svnIsAvailable() {{{2
	function s:svnIsAvailable()
		let exists = executable('svn')

		if !exists
			call s:error('svn is not available.')
		endif

		return exists
	endfunction

	" Function s:svnStatus() {{{2
	function s:svnStatus(line)
		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				try
					let lines = s:buildSvnMessage('', s:svnDoStatus(path), '')
				catch /.*/
					let lines = s:buildSvnMessage('Unable to status path ' . path . ':', [], v:exception)
				endtry

				call s:goToSvnWindow('Svn status of ' . path . '%=[%3p%%]', 'svn')

				setlocal buftype=nofile

				call s:putList(lines, 1)

				setlocal nomodifiable
			endif
		endif
	endfunction

	" Function s:svnDoStatus() {{{2
	function s:svnDoStatus(path)
		let files = []

		if s:svnIsAvailable() && s:pathExists(a:path)
			call s:echo('Performing svn status on ' . a:path . '...')
			let output = system('svn status ' . shellescape(a:path))
			call s:echo('Svn status done.')

			redraw

			if v:shell_error
				throw output
			else
				let files = filter(split(output, "\n"), "v:val =~# '^[[:space:]ACDIMRX?!~L+SKOTB]\\{6}\\s'")

				if g:myprojects_sort_svn
					let files = sort(files)
				endif
			endif
		endif

		return files
	endfunction

	" Function s:svnUpdate() {{{2
	function s:svnUpdate(line)
		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				call s:echo('Performing svn update on ' . path . '...')
				let output = system('svn update --accept postpone ' . shellescape(path))
				call s:echo('Svn update done.')

				if v:shell_error
					let lines = s:buildSvnMessage('Unable to update path ' . path . ':', [], output)
				else
					let files = filter(split(output, "\n"), "v:val =~# '^[ADUCGE[:space:]]\\{4}\\s'")

					if empty(files)
						let lines = s:buildSvnMessage('There is no file to update in ' . path . '.', [], '')
					else
						if g:myprojects_sort_svn
							let files = sort(files)
						endif

						let lines = s:buildSvnMessage('', files, '')

						call s:refresh(a:line, 0)
					endif
				endif

				call s:goToSvnWindow('Svn update of ' . path . '%=[%3p%%]', 'svn')

				setlocal buftype=nofile

				call s:putList(lines, 1)

				setlocal nomodifiable
			endif
		endif
	endfunction

	" Function s:svnCommit() {{{2
	function s:svnCommit(line)
		let b:files = ''
		let b:message = ''

		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'strpart(v:val, 0, 6) =~# "[MDAR]"')

					if empty(files)
						let lines = s:buildSvnMessage('There is no file to commit on ' . path '.', [], '')
					else
						let lines = s:buildSvnMessage('Delete file if you don''t want to commit it and type :w...', files, '')
					endif
				catch /.*/
					let lines = s:buildSvnMessage('Unable to commit path ' . path . ':', [], v:exception)
				endtry

				call s:goToSvnWindow('Svn commit of ' . path . '%=[%3p%%]', 'svn')
				
				if !v:exception
					setlocal nomodified
					setlocal buftype=acwrite
					setlocal modifiable

					execute 'au! ' . s:plugin . ' BufWriteCmd'
					execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnGetCommitMessage()'
				endif

				call s:putList(lines, empty(files) ? '' : 2)
			endif
		endif
	endfunction

	" Function s:svnGetCommitMessage() {{{2
	function s:svnGetCommitMessage()
		if s:svnIsAvailable()
			setlocal nomodified

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, 'strpart(v:val, 0, 6) =~# "[MDAR]"')

			if empty(b:files)
				setlocal buftype=nofile
				let lines = s:buildSvnMessage('No files to commit.', [], '')
			else
				let files = copy(b:files)

				call insert(files, '')

				let lines = s:buildSvnMessage('Define log message and type :w...', files, '')

				execute 'au! ' . s:plugin . ' BufWriteCmd'
				execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoCommit()'
			endif

			call s:putList(lines, empty(b:files) ? '' : 2)
		endif
	endfunction

	" Function s:svnDoCommit() {{{2
	function s:svnDoCommit()
		if s:svnIsAvailable()
			setlocal nomodified

			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:message = filter(getbufline('%', 1, '$'), "v:val !~# '^" . escape(s:prompt, '[]') . "'")

			call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

			call s:echo('Performing svn commit...')
			let output = system('svn commit ' . join(map(copy(b:files), 'shellescape(v:val)'), ' ') . ' -m ' . shellescape(join(b:message, "\n")))
			call s:echo('Svn commit done.')

			if v:shell_error
				let lines = s:buildSvnMessage('Unable to commit these files:', b:files, output)
			else
				let lines = s:buildSvnMessage('These files as been committed:', b:files, '')
			endif

			call s:putList(lines, '')

			setlocal nomodifiable
		endif
	endfunction

	" Function s:svnRevert() {{{2
	function s:svnRevert(line)
		let b:files = ''
		let b:message = ''

		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'strpart(v:val, 0, 6) =~# ''\%(M\|D\|R\|C\)''')

					let goToLine = ''

					if empty(files)
						let lines = s:buildSvnMessage('There is no file to revert on ' . path . '.', [], '')
					else
						let goToLine = 2

						if g:myprojects_sort_svn
							let files = sort(files)
						endif

						let lines = s:buildSvnMessage('Delete file if you don''t want to revert it and type :w...', files, '')
					endif
				catch /.*/
					let lines = s:buildSvnMessage('Unable to revert path ' . path . ':', [], v:exception)
				endtry

				call s:goToSvnWindow('Svn revert of ' . path . '%=[%3p%%]', 'svn')

				if !v:exception
					setlocal nomodified
					setlocal buftype=acwrite
					setlocal modifiable

					execute 'au! ' . s:plugin . ' BufWriteCmd'
					execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoRevert()'
				endif

				call s:putList(lines, goToLine)
			endif
		endif
	endfunction

	" Function s:svnDoRevert() {{{2
	function s:svnDoRevert()
		if s:svnIsAvailable()
			setlocal nomodified
			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, 'strpart(v:val, 0, 6) =~# ''\%(M\|D\|R\|C\)''')

			if empty(b:files)
				let lines = s:buildSvnMessage('No files to revert.', [], '')
			else
				call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

				call s:echo('Performing svn revert...')
				let output = system('svn revert ' . join(map(copy(b:files), 'shellescape(v:val)'), ' '))
				call s:echo('Svn revert done.')

				if v:shell_error
					let lines = s:buildSvnMessage('Unable to revert these files:', b:files, output)
				else
					let lines = s:buildSvnMessage('These files have been reverted:', b:files, '')
				endif
			endif

			call s:putList(lines, '')

			setlocal nomodifiable
		endif
	endfunction

	" Function s:svnAdd() {{{2
	function s:svnAdd(line)
		let b:files = ''
		let b:message = ''

		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'v:val =~ "^?"')

					if empty(files)
						let lines = s:buildSvnMessage('There is no file to add on path ' . path . '.', [], '')
					else
						let lines = s:buildSvnMessage('Delete file if you don''t want to add it and type :w...', files, '')
					endif
				catch /.*/
					let lines = s:buildSvnMessage('Unable to add from path ' . path . ':', [], v:exception)
				endtry

				call s:goToSvnWindow('Svn add of ' . path . '%=[%3p%%]', 'svn')

				if !v:exception
						setlocal nomodified
						setlocal buftype=acwrite
						setlocal modifiable

						execute 'au! ' . s:plugin . ' BufWriteCmd'
						execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoAdd()'
				endif

				call s:putList(lines, empty(files) ? '' : 2)
			endif
		endif
	endfunction

	" Function s:svnDoAdd() {{{2
	function s:svnDoAdd()
		if s:svnIsAvailable()
			setlocal nomodified

			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, "v:val =~ '^?'")

			if empty(b:files)
				let lines = s:buildSvnMessage('No files to add.', [], '')
			else
				call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

				call s:echo('Performing svn add...')
				let output = system('svn add ' . join(map(copy(b:files), 'shellescape(v:val)'), ' '))
				call s:echo('Svn add done.')

				if v:shell_error
					let lines = s:buildSvnMessage('These files as been added:', b:files, output)
				else
					let lines = s:buildSvnMessage('These files as been added:', b:files, '')
				endif
			endif

			call s:putList(lines, '')

			setlocal nomodifiable
		endif
	endfunction

	" Function s:svnDiff() {{{2
	function s:svnDiff(line)
		if s:svnIsAvailable()
			let path = s:getPath(a:line)

			if path != ''
				if getftype(path) != 'file'
					call s:error('File ' . path . ' is invalid.')
				else
					let previousVersion = system('svn cat ' . shellescape(path) . ' -r HEAD')

					if previousVersion == ''
						call s:error('No previous version available for  ' . path . '.')
					else
						call s:open('edit')

						let filetype = &filetype

						diffthis
						silent vnew
						setlocal bufhidden=delete
						setlocal buftype=nofile
						setlocal nobuflisted
						setlocal noswapfile
						silent 0put=previousVersion
						silent $d
						silent normal! 1gg
						silent execute 'setlocal filetype=' . filetype
						setlocal nomodifiable
						diffthis

						let file = s:prompt .'Svn diff of ' . path 
						silent file `=file`

						let titlestring=&titlestring
						let &titlestring=s:prompt . 'Svn diff of ' . path

						silent execute 'augroup ' . s:plugin
						silent execute 'au WinEnter <buffer> let &titlestring ="' . &titlestring . '"'
						silent execute 'au WinLeave <buffer> let &titlestring ="' . titlestring . '"'
						silent augroup END

						silent normal! ]c
					endif
				endif
			endif
		endif
	endfunction

	" Function s:svnCheckout() {{{2
	function s:svnCheckout(line)
	endfunction

	" Function s:buildSvnMessage() {{{2
	function s:buildSvnMessage(message, files, output)
		let lines = []
		
		if a:message != ''
			call add(lines, s:prompt . a:message)
		endif

		for file in a:files
			call add(lines, file)
		endfor

		for line in split(a:output, "\n")
			call add(lines, s:prompt . line)
		endfor

		return lines
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
		let buffers = []

		silent let rawBuffers = s:getRawBuffers(a:hidden)

		for buffer in rawBuffers
			call add(buffers, fnamemodify(expand(substitute(buffer, '^[^"]\+"\([^"]\+\)".*$', '\1', ''), ':p'), ':p'))
		endfor

		return buffers
	endfunction

	" Function s:refreshProjectBuffers()
	function s:refreshProjectBuffers(projectName, path)
		let currentPath = fnamemodify(expand(bufname('%')), ':p')

		if currentPath =~ '^' . a:path
			let currentWindow = winnr()

			call s:listProjectBuffers(a:projectName, a:path)

			silent execute currentWindow . 'wincmd w'
		endif
	endfunction

	" Function s:listProjectBuffers()
	function s:listProjectBuffers(projectName, path)
		let projectBuffers = []

		if a:path != ''
			silent let buffers = filter(s:getBuffers(0), 'v:val =~ ''^' . a:path . s:osSlash . '.*$''')

			if g:myprojects_sort_buffers
				let buffers = sort(buffers)
			endif

			for buffer in buffers
				call add(projectBuffers, substitute(buffer, '^' . a:path, '', ''))
			endfor

			if !s:goToBuffersWindow('Buffers of project ' . a:projectName . ' in ' . a:path, a:projectName, a:path)
				silent execute 'nnoremap <buffer> <silent> d :call <SID>deleteProjectBuffers(line(''.''), ''' . a:path .''')<CR>'
				silent execute 'augroup ' . s:plugin
				silent execute 'au BufNew,BufDelete * call' . s:sid . 'refreshProjectBuffers(''' . a:projectName . ''', ''' . a:path . ''')'
				silent augroup END
			endif

			call s:putList(projectBuffers, 1)
		endif
	endfunction

	" Function s:exploreProjectBuffers()
	function s:exploreProjectBuffers(line)
		call s:listProjectBuffers(s:getProjectName(a:line), s:getProjectPath(a:line))
	endfunction

	" Function s:deleteProjectBuffers()
	function s:deleteProjectBuffers(line, path)
		let line = a:path . getline(a:line)

		if line != ''
			let buffer = bufname(line)

			if buffer != '' && bufexists(buffer)
				if getbufvar(buffer, '&modified') == 1
						call s:error('Sorry, no write since last change for buffer ' line . ', unable to delete')
				else
					setlocal modifiable
					silent execute 'bdelete ' . buffer
					silent normal! dd
					setlocal nomodifiable
				endif
			endif
		endif
	endfunction

	" Function s:echoVersion() {{{2
	function s:echoVersion()
		call s:echo('Version ' . s:version . ' (c) ' . s:author . ' ' . s:copyright . ' - ' . s:email . ' - ' . s:webSite)
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
		call s:goToMyProjectsWindow(0)

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
					endif

					silent let buffers = s:getRawBuffers(1)

					for buffer in buffers
						echomsg buffer
						if buffer =~# '\s"' . s:sid
							silent execute 'bdelete ' . substitute(buffer, '^\s*\([0-9]\+\).\+$', '\1', '')
						endif
					endfor

					execute 'mksession! ' . session
					call s:echo('Session saved in ' . session . '.')
				endif
			endif
		catch /.*/
			call s:error(v:exception)
		endtry
	endfunction

	" Function s:loadSession() {{{2
	function s:loadSession(line)
		call s:goToMyProjectsWindow(0)

		let session = s:getSessionFile(a:line)

		if !filereadable(session)
			call s:error('Unable to read session file ' . session . '.')
		else
			let s:closeIfAlone = 0
			execute 'source ' . session
			let s:closeIfAlone = 1
			call s:echo('Session loaded from ' . session . '.')
			call s:floatMyProjects()
		endif
	endfunction

	" Function s:deleteSession() {{{2
	function s:deleteSession(line)
		call s:goToMyProjectsWindow(0)

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

			if !empty(attributes)
				let myprojects[name]['attributes'] = attributes
			endif

			if s:isFolder(a:line)
				let files = []
				let position = getpos('.')

				call setpos('.', [0, a:line, 1])

				let endLine = s:getLastFolderLine(a:line)
				let regex = '^' . repeat('\t', (s:indent(a:line) + 1)) . '[^\t]\+$'
				let file = search(regex, '', endLine)

				while file
					call add(files, s:getMyProjects(file))
					let file = search(regex, '', endLine)
				endwhile

				call setpos('.', position)

				if !empty(files)
					let myprojects[name]['files'] = files
				endif
			endif
		endif

		return myprojects
	endfunction

	" Function s:buildMyProjects() {{{2
	function s:buildMyProjects(path, filter, myprojects, indent)
		let myprojects = ''

		let filter = a:filter

		for [name, meta] in items(a:myprojects)
			let path = s:unescape(!has_key(meta, 'attributes') || !has_key(meta['attributes'], 'path') ? a:path . s:osSlash . name : meta['attributes']['path'])

			if s:pathExists(path) && (filter == '' || match(name, filter) != -1)
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

					if has_key(meta['attributes'], 'mappings')
						for [index, mapping] in items(meta['attributes']['mappings'])
							let myprojects .= ' F' . index . '="' . mapping . '"'
						endfor
					endif
				endif

				let myprojects .= "\n"

				if getftype(path) == 'dir'
					let subFiles = !has_key(meta, 'files') ? [] : meta['files']

					let files = ''

					for subName in subFiles
						let files .= s:buildMyProjects(path, filter, subName, a:indent + 1)
					endfor

					let notInMyProjectsFiles = s:getFilesNotInMyprojects(path, filter, a:indent, subFiles)

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
	function s:getFilesNotInMyprojects(path, filter, indent, myprojects)
		let myprojects = ''

		let cwd = getcwd()
"
		silent execute 'lcd ' . fnameescape(a:path)

		for globName in sort(filter(split(glob('*') . "\n" . glob('.*'), "\n"), 'v:val != "." && v:val != ".."'))
			if !s:isInMyprojects(globName, a:myprojects)
				let myprojects .= s:buildMyProjects(a:path, a:filter, {globName : {}}, a:indent + 1)
			endif
		endfor

		silent execute 'lcd ' . fnameescape(cwd)

		return myprojects
	endfunction

	" Function s:getLineOfPath() {{{2
	function s:getLineOfPath(path)
		let line = 0

		let path = substitute(a:path, '^[[:space:]A-Z]\+\s', '', '')

		if getftype(path) == 'file'
			if s:goToMyProjectsWindow(0) == -1
				call s:myProjects()
			endif

			let line = s:searchPath(path, 0)
		endif

		return line
	endfunction

	" Function s:openFromWorkingWindow() {{{2
	function s:openFromWorkingWindow(command, path)
		let line = s:getLineOfPath(a:path)

		if line > 0
			try
				call s:edit(a:command, line)
			catch /.*/
				call s:error(v:exception)
			endtry
		endif
	endfunction

	" Function s:openFromSvnWindow() {{{2
	function s:openFromSvnWindow(command)
		call s:openFromWorkingWindow(a:command, getline('.'))
	endfunction

	" Function s:openFromBuffersWindow() {{{2
	function s:openFromBuffersWindow(command, path)
		call s:openFromWorkingWindow(a:command, a:path . getline('.'))
	endfunction

	" Function s:svnDiffFromWorkingWindow() {{{2
	function s:svnDiffFromWorkingWindow(path)
		let line = s:getLineOfPath(a:path)

		if line > 0
			call s:svnDiff(line)
		endif
	endfunction

	" Function s:svnDiffFromSvnWindow()
	function s:svnDiffFromSvnWindow()
		call s:svnDiffFromWorkingWindow(getline('.'))
	endfunction

	" Function s:svnDiffFromBuffersWindow()
	function s:svnDiffFromBuffersWindow(path)
		call s:svnDiffFromWorkingWindow(a:path . getline('.'))
	endfunction

	" Function s:sid() {{{2
	function s:sid()
		return matchstr(expand('<sfile>'), '<SNR>\d\+_\zesid$')
	endfunction

	" Function s:initVariable() {{{2
	function s:initVariable(name, value)
		if !exists(a:name)
			let {a:name} = a:value
		endif
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

		let prompt = s:prompt . a:prompt

		if a:0 == 0
			return input(prompt)
		elseif a:0 == 1
			return input(prompt, a:1)
		elseif a:0 == 2
			return input(prompt, a:1, a:2)
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
			silent normal! j
			if getline('.') =~ '^$'
				silent normal! dd
			endif
			call s:goToLine(a:line)
		endif
	endfunction

	" Function s:putList() {{{2
	function s:putList(lines, goToLine)
		setlocal modifiable

		let put = join(a:lines, "\n")

		silent execute ':%d'
		silent execute ':resize ' . len(a:lines)
		silent 0put =put
		silent $d
		call s:goToLine(a:goToLine)
	endfunction

	" Function s:error() {{{2
	function s:error(message)
		echohl errorMsg
		call s:echo(a:message)
		echohl normal
	endfunction

	" Function s:echo() {{{2
	function s:echo(message)
		redraw
		echo s:prompt . a:message
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
	endfunction

	" Function s:getFiles() {{{2
	function s:getFiles(path, filter)
		let files = ''

		if isdirectory(a:path)
			let cwd = getcwd()

			silent execute 'lcd ' . a:path

			for inode in sort(split(glob('*'), "\n"))
				if isdirectory(inode)
					let files .= ' ' . s:getFiles(a:path . s:osSlash . inode, a:filter)
				elseif a:filter == '' || match(inode, a:filter) != -1
					let files .= ' ' . a:path . s:osSlash . inode
				endif
			endfor

			silent execute 'lcd ' . cwd
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
			let position = getpos('.')

			call setpos('.', [0, line, 1])

			let line = search(file, 'W')

			while line > 0
				let path = s:getPath(line)

				if path != '' && a:path == path
					return line
				else
					let line = search(file, 'W')
				endif
			endwhile

			call setpos('.', position)
		endif

		return line
	endfunction

	" Function s:cleanPath() {{{2
	function s:cleanPath(path)
		return substitute(a:path, escape(s:osSlash, '\') . '\+$', '', '')
	endfunction

	" Function s:mkdir(path)
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

	" Function s:floatWindow() {{{2
	function s:floatWindow()
		silent wincmd H
	endfunction

	" Function s:resizeWindow() {{{2
	function s:resizeWindow(width)
		silent execute 'vertical resize ' . a:width
	endfunction

	" Restore cpo {{{2
	let &cpo= s:keepCpo
	unlet s:keepCpo

	" Enable myprojects {{{2
	let g:myprojects_enable = 1
endif

" finish {{{1
finish
