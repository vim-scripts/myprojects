"=============================================================================
" File:						myprojects.vim
" Author:					Frédéric Hardy (fhardy at noparking.net)
" Date:						Mon Mar 23 15:41:18 CET 2009
" Licence:					GPL version 2.0 license
" GetLatestVimScripts:	2556 10039 :AutoInstall: myprojects.vim
"=============================================================================
if v:version < 700
    echoerr "myprojects.vim requires vim >= 7. DOWNLOAD IT! You'll thank me later."
elseif !has('folding')
    echoerr "myprojects.vim requires folding."
elseif &cp
    echoerr "myprojects.vim requires no compatible mode."
elseif !exists('myprojects_enable')
	let s:keepCpo= &cpo
	setlocal cpo&vim

	let s:plugin = 'myprojects'
	let s:version = '0.0.50'
	let s:copyright = '2009'
	let s:author = 'Frédéric Hardy'
	let s:email = 'fhardy at noparking.net'
	let s:webSite = 'http://blog.mageekbox.net'
	let s:prompt = '[' . s:plugin . '] '
	let s:buffer = -1
	let s:workingBuffer = -1
	let s:filename = ''
	let s:oldWidth = 0
	let s:windowsOs = has('win16') || has('win32') || has('win64')
	let s:osSlash = s:windowsOs ? '\' : '/'
	let s:home = expand('$HOME')

	command -nargs=? -complete=file MyProjectsToggle call <SID>toggle('<args>')

	if !hasmapto('<Plug>MyProjectsToggle')
		map <unique> <silent> <Leader>p <Plug>MyProjectsToggle
	endif

	noremap <unique> <script> <Plug>MyProjectsToggle <SID>toggle
	noremap <SID>toggle  :call <SID>toggle('')<CR>

	if !hasmapto('<Plug>MyProjectsGoHome')
		map <unique> <silent> <Leader><Tab> <Plug>MyProjectsGoHome
	endif

	noremap <unique> <script> <Plug>MyProjectsGoHome <SID>goHome
	noremap <SID>goHome  :call <SID>goHome()<CR>

	function s:myProjects(filename)
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
		call s:initVariable('g:myprojects_svn_sort', 1)

		if s:goToWindow() < 0
			let s:filename = a:filename == '' ? g:myprojects_file : a:filename

			execute 'leftabove vertical new ' . s:filename

			let s:buffer = winbufnr(0)

			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <SID>open('edit')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <SID>open('edit')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-Tab> :call <SID>goToEditWindow()<CR>
			nnoremap <buffer> <silent> <C-Right> :call <SID>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-l> :call <SID>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Left> :call <SID>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-h> :call <SID>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Space> :call <SID>toggleMaximize()<CR>
			nnoremap <buffer> <silent> <LocalLeader>d :call <SID>delete(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>c :call <SID>create(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>r :call <SID>refresh(line('.'), 0)<CR>
			nnoremap <buffer> <silent> <LocalLeader>R :call <SID>refresh(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>g :call <SID>grep(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>t :call <SID>generateTags(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>e :call <SID>explore('E')<CR>
			nnoremap <buffer> <silent> <LocalLeader>E :call <SID>explore('Se')<CR>
			nnoremap <buffer> <silent> <LocalLeader>a :call <SID>appendFile(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>s :call <SID>saveSession(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>S :call <SID>loadSession(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>p :call <SID>setPath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>P :call <SID>updatePath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>f :call <SID>setFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>F :call <SID>updateFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <LocalLeader>w :call <SID>setCd(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>W :call <SID>updateCd(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>m :call <SID>setMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>M :call <SID>updateMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <LocalLeader>i :call <SID>echo('Path: ' . <SID>getPath(line('.')))<CR>
			nnoremap <buffer> <silent> <LocalLeader>v :call <SID>echoVersion()<CR>
			nnoremap <buffer> <silent> <LocalLeader>V :call <SID>echoMyprojectsFile()<CR>
			nnoremap <buffer> <silent> <A-s> :call <SID>svnStatus(line('.'))<CR>
			nnoremap <buffer> <silent> <A-u> :call <SID>svnUpdate(line('.'))<CR>
			nnoremap <buffer> <silent> <A-a> :call <SID>svnAdd(line('.'))<CR>
			nnoremap <buffer> <silent> <A-r> :call <SID>svnRevert(line('.'))<CR>
			nnoremap <buffer> <silent> <A-d> :call <SID>svnDiff(line('.'))<CR>
			nnoremap <buffer> <silent> <A-c> :call <SID>svnCommit(line('.'))<CR>

			if g:myprojects_display_path_in_statusline
				nnoremap <buffer> <silent> <Down> <Down>:call <SID>echo(<SID>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> j j:call <SID>echo(<SID>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> <Up> <Up>:call <SID>echo(<SID>getPath(line('.')))<CR>
				nnoremap <buffer> <silent> k k:call <SID>echo(<SID>getPath(line('.')))<CR>
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

			let s:sid = s:SID()

			silent execute 'setlocal statusline=' . escape(s:prompt, ' ') . '%=[%f\ %3p%%]'
			silent execute 'setlocal foldtext=' . s:sid . 'foldtext()'
			silent execute 'setlocal foldexpr=' . s:sid . 'foldexpr()'
			silent execute 'setlocal foldcolumn=' . g:myprojects_foldcolumn
			silent execute 'au! winEnter <buffer> call' . s:sid . 'winEnter()'

			let titlestring=&titlestring
			let &titlestring=s:plugin

			silent execute 'augroup ' . s:plugin
			silent execute 'au WinEnter <buffer> let &titlestring ="' . &titlestring . '"'
			silent execute 'au WinLeave <buffer> let &titlestring ="' . titlestring . '"'
			silent augroup END

			if !has('syntax') || !g:myprojects_syntax
				setlocal filetype=""
			else
				silent execute 'setlocal filetype=' . s:plugin
				syntax on
			endif

			call s:float()
			call s:resize(g:myprojects_width)

			if foldlevel(line('.'))
				silent normal! zo
			endif

			redraw

			if g:myprojects_display_path_in_statusline
				call <SID>echo(<SID>getPath(line('.')))
			endif
		endif
	endfunction

	function s:wipeout()
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

	function s:getSessionFile(line)
		let path = ''

		let line = s:getRootLine(a:line)

		if line > 0
			let path = g:myprojects_sessions_directory . s:osSlash . fnameescape(s:getName(line))
		endif

		return path
	endfunction

	function s:saveSession(line)
		call s:goHome()

		if !isdirectory(g:myprojects_sessions_directory)
			if !exists("*mkdir")
				call s:error('Unable to create sessions directory ' . g:myprojects_sessions_directory . ', mkdir() function does not exists, please create it manualy.')
			else
				 call mkdir(g:myprojects_sessions_directory, 'p', 0700)
			endif
		endif

		if !isdirectory(g:myprojects_sessions_directory)
			call s:error('Unable to create session, directory ' . g:myprojects_sessions_directory . ' does not exist.')
		else
			let session = s:getSessionFile(a:line)

			if session == ''
				call s:error('Unable to create session.')
			else
				silent mkview

				if s:wipeout()
					execute 'mksession! ' . session
					call s:myProjects(s:filename)
					call s:echo('Session saved.')
				endif

				silent loadview
			endif
		endif
	endfunction

	function s:loadSession(line)
		call s:goHome()

		let session = s:getSessionFile(a:line)

		if !filereadable(session)
			call s:error('Unable to read session file ' . session . '.')
		else
			silent mkview

			if s:wipeout()
				execute 'source ' . session
				call s:echo('Session loaded.')
				call s:myProjects(s:filename)
			endif

			silent loadview
		endif
	endfunction

	function s:getWindow(buffer)
		let window = -1

		if a:buffer != -1
			let window = bufwinnr(a:buffer)
		endif

		return window
	endfunction

	function s:getMyProjectsWindow()
		return s:getWindow(s:buffer)
	endfunction

	function s:getWorkingWindow()
		return s:getWindow(s:workingBuffer)
	endfunction

	function s:goToEditWindow()
		wincmd p

		let window = winnr()

		if window == s:getMyProjectsWindow() || window == s:getWorkingWindow()
			2 wincmd w

			let window = winnr()

			if window == s:getMyProjectsWindow() || window == s:getWorkingWindow()
				call s:goHome()
				vnew
			endif
		endif
	endfunction

	function s:goToWindow()
		let window = s:getMyProjectsWindow()

		if window != -1
			silent execute window . 'wincmd w'
		endif

		return window
	endfunction

	function s:goHome()
		if s:goToWindow() == -1
			call s:myProjects(s:filename)
		endif
	endfunction

	function s:winEnter()
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
	endfunction

	function s:close()
		if s:goToWindow() == -1
			return 0
		else
			hide
			return 1
		endif
	endfunction

	function s:float()
		let window = s:goToWindow()

		if window != -1
			wincmd H
		endif
	endfunction

	function s:resize(width)
		let window = s:goToWindow()

		if window != -1
			silent execute 'vertical resize ' . a:width
		endif
	endfunction

	function s:toggleMaximize()
		if s:oldWidth == 0
			let s:oldWidth = winwidth(0)
			call s:resize('')
		else
			call s:resize(s:oldWidth)
			let s:oldWidth = 0
		endif
	endfunction

	function s:foldtext()
		let text = repeat(' ', indent(v:foldstart)) . '+ ' . substitute(getline(v:foldstart), '^\s*\(\f\+\).*$', '\1', 'g') . '-[' . (v:foldend - v:foldstart) . ']'

		if foldclosed(line('.')) == - 1
			let virtcol = virtcol('.')
			let wincol = wincol()

			if virtcol > (wincol - &sidescrolloff)
				let text = strpart(text, virtcol - wincol)
			endif
		endif

		return text
	endfunction

	function s:foldexpr()
		let currentIndent = s:indent(v:lnum)
		let nextIndent = s:indent(nextnonblank(v:lnum + 1))
		return currentIndent >= nextIndent ? currentIndent : '>' . nextIndent
	endfunction

	function s:toggle(filename)
		let window = s:goToWindow()

		call s:myProjects(a:filename)

		if window != -1
			call s:close()
		endif
	endfunction

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

	function s:getRootLine(line)
		let line = 0

		let indent = indent(a:line)

		if indent != -1
			let line = indent == 0 ? a:line : search('^[^\t]\+$', 'bnW')
		endif

		return line
	endfunction

	function s:getName(line)
		let name = ''

		let line = getline(a:line)

		if line != -1
			let name = substitute(line, '^\t*\(\%([^=]\|\\\s\)\+\).*$', '\1', '')
		endif

		return name
	endfunction

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

		return path
	endfunction

	function s:hasAttribute(name, line)
		return getline(a:line) =~ '.*\s\+' . a:name . '="[^"]\{-}"'
	endfunction

	function s:extractAttributeFromLine(name, line)
		return !s:hasAttribute(a:name, a:line) ? '' : substitute(getline(a:line), '.*\s\+' . a:name . '="\([^"]\{-}\)".*', '\1', '')
	endfunction

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

	function s:hasPath(line)
		return getline(a:line) =~ '^\t*[^=]\+=\(\(\\ \|\f\)\+\).*$'
	endfunction

	function s:extractPathFromLine(line)
		return !s:hasPath(a:line) ? '' : substitute(substitute(getline(a:line), '^\s*[^=]\+=\(\(\\ \|\f\)\+\).*$', '\1', ''), s:osSlash . '\+$', '', '')
	endfunction

	function s:extractPath(line)
		let path = s:extractPathFromLine(a:line)

		if path == ''
			let path = s:getName(a:line)
		endif

		return path
	endfunction

	function s:hasCd(line)
		return s:hasAttribute('cd', a:line)
	endfunction

	function s:extractCdFromLine(line)
		let cd = s:extractAttributeFromLine('cd', a:line)

		if cd == '.'
			let cd = s:getPath(a:line)
		endif

		return cd
	endfunction

	function s:extractCd(line)
		let line = a:line

		let cd = s:extractCdFromLine(line)

		while cd == '' && line > 0
			let line = s:getFirstFolderLine(line)

			if line > 0
				let cd = s:extractCdFromLine(line)
			endif
		endwhile

		return cd
	endfunction

	function s:hasFilter(line)
		return s:hasAttribute('filter', a:line)
	endfunction

	function s:extractFilterFromLine(line)
		return s:extractAttributeFromLine('filter', a:line)
	endfunction

	function s:extractFilter(line)
		return s:extractAttribute('filter', a:line)
	endfunction

	function s:hasMapping(mapping, line)
		return s:hasAttribute(a:mapping, a:line)
	endfunction

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

	function s:isFolder(line)
		return indent(a:line) < indent(nextnonblank(a:line + 1))
	endfunction

	function s:cleanPath(path)
		return substitute(a:path, escape(s:osSlash, '/') . '\+$', '', '')
	endfunction

	function s:isAbsolutePath(path)
		return s:windowsOs ? a:path =~ '^.:\(\\\|\/\)' : a:path =~ '^/'
	endfunction

	function s:indent(line)
		let indent = indent(a:line)

		if indent > 0
			let indent = indent / &tabstop
		endif

		return indent
	endfunction

	function s:create(line)
		let indent = s:indent(a:line)

		if indent >= 0
			let name = s:input('Name of new project: ', '')

			if name == ''
				call s:error('Name must not be empty.')
			elseif !s:projectIsUnique(name)
				call s:error('There is already a project with name ' . name)
			else
				let myprojects = {name : {'attributes':{}, 'files': []}}

				let path = s:input('Path of project ' . name . ': ', '', 'file')

				if path == ''
					call s:error('Path must not be empty.')
				else
					let path = s:cleanPath(path)

					if !s:isAbsolutePath(path)
						call s:error('Path must be absolute.')
					elseif !s:pathExists(path)
						call s:error('Path ' . path . ' is invalid.')
					else
						let myprojects[name]['attributes']['path'] = path

						let cd = s:input('Working directory of project ' . name . ': ', '', 'file')

						if cd != '.'
							let cd = s:cleanPath(fnamemodify(cd, ':p'))

							if cd == path
								let cd = '.'
							endif
						endif

						if cd != '.' && getftype(cd) != 'dir'
							call s:error('Working directory ' . cd . ' is not a valid directory.')
						else
							let myprojects[name]['attributes']['cd'] = cd

							let filter = s:input('Filter of project ' . name . ': ', '')

							if filter != ''
								let myprojects[name]['attributes']['filter'] = filter
							endif

							let mappings = {}

							for [key, mapping] in items(s:defineMappings('Define mapping for project ' . name . ': ', {}))
								if mapping != ''
									let mappings[key] = mapping
								endif
							endfor

							if !empty(mappings)
								let myprojects[name]['attributes']['mappings'] = mappings
							endif

							call s:echo('Create project ' . name . ' from path ' . path . ', please wait...')
							call s:put(s:buildMyProjects('', filter, myprojects, indent), a:line)
							call s:echo('Project ' . name . ' created.')
							call s:echo(s:getPath(a:line))
						endif
					endif
				endif
			endif
		endif
	endfunction

	function s:buildMyProjects(path, filter, myprojects, indent)
		let myprojects = ''

		let filter = a:filter

		for [name, meta] in items(a:myprojects)
			let path = !has_key(meta, 'attributes') || !has_key(meta['attributes'], 'path') ? a:path . s:osSlash . name : meta['attributes']['path']


			if s:pathExists(path) && (filter == '' || match(name, filter) != -1)
				let myprojects .= repeat("\t", a:indent) . name

				if has_key(meta, 'attributes')
					if has_key(meta['attributes'], 'path')
						let myprojects .= '=' . meta['attributes']['path']
					endif

					if has_key(meta['attributes'], 'cd')
						let myprojects .= ' cd="' . meta['attributes']['cd'] . '"'
					endif

					if has_key(meta['attributes'], 'filter')
						let myprojects .= ' filter="' . meta['attributes']['filter'] . '"'
						let filter = meta['attributes']['filter']
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

	function s:getFilesNotInMyprojects(path, filter, indent, myprojects)
		let myprojects = ''

		let cwd = getcwd()
"
		silent execute 'lcd ' . a:path

		for globName in sort(filter(split(glob('*') . "\n" . glob('.*'), "\n"), 'v:val != "." && v:val != ".."'))
			if !s:isInMyprojects(globName, a:myprojects)
				let myprojects .= s:buildMyProjects(a:path, a:filter, {globName : {}}, a:indent + 1)
			endif
		endfor

		silent execute 'lcd ' . cwd

		return myprojects
	endfunction

	function s:isInMyprojects(name, files)
		for file in a:files
			if has_key(file, a:name)
				return 1
			endif
		endfor

		return 0
	endfunction

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

				silent execute ':' . line . (!s:isFolder(line) ? '' : ',' . s:getLastFolderLine(line)) . 'd'

				call s:put(myprojects, line)
				call s:echo(path . ' was refreshed.')
			endif
		endif
	endfunction

	function s:getHeadOfPath(path)
		return fnamemodify(a:path, ':h')
	endfunction

	function s:getDriveLetter(path)
		return !s:windowsOs ? '' : substitute(a:path, '\(^[a-zA-Z]:)\.*', '\1', '')
	endfunction

	function s:directoryExists(path)
		if getftype(a:path) == ''
			if !exists("*mkdir")
				call mkdir(a:path, 'p')
			else
				call s:error('Unable to create directory ' . a:path)
			endif
		endif

		return getftype(a:path) == 'dir'
	endfunction

	function s:checkPath(path)
		let type = getftype(a:path)

		if type == ''
			return 1
		elseif !filereadable(a:path)
			call s:error('Unable to read file ' . a:path . '.')
			return 0
		elseif type != 'file' && type != 'link'
			call s:error('Unable to open file ' . a:path . ' of type ' . type . '.')
			return 0
		else
			return 1
		endif
	endfunction

	function s:edit(command, path, rootPath, cd, mappings)
		if !s:checkPath(a:path)
			return 0
		else
			let window = bufwinnr(a:path)

			if window != -1
				silent execute window . 'wincmd w'
				silent execute 'buffer ' . bufnr(a:path)
			elseif !s:directoryExists(s:getHeadOfPath(a:path))
				return 0
			else
				call  s:goToEditWindow()

				try
					silent execute a:command ' ' . fnameescape(a:path)
				catch E37
					if s:input('Save ' . bufname('%') . ' and load ' . a:path . ' ? [y/N]: ', '') != 'y'
						return 0
					else
						silent write
						silent execute a:command ' ' . fnameescape(a:path)
					endif
				endtry
			endif

			if g:myprojects_tags_file != '' && a:rootPath != ''
				let tagsPath = a:rootPath . s:osSlash . g:myprojects_tags_file

				if getftype(tagsPath) == 'file'
					if &tags !~ '^' . tagsPath . ',\?'
						silent execute 'set tags=' . tagsPath . ',' . &tags
					endif
				endif
			endif

			if a:cd != ''
				if getftype(a:cd) != 'dir'
					call s:error('Unable to change directory to ' . a:cd . '.')
				else
					silent execute 'lcd ' . a:cd
					silent execute 'augroup ' . s:plugin
					silent execute 'au BufEnter <buffer> lcd ' . a:cd
					silent augroup END
				endif
			endif

			for [line, mapping] in items(a:mappings)
				for [key, value] in items(mapping)
					silent execute 'nmap <buffer> <silent> <F' . key . '> ' . expand(value)
				endfor
			endfor

			setlocal more

			if !hasmapto('<Plug>MyProjectsGoHome')
				map <buffer> <silent> <C-Tab> <Plug>MyProjectsGoHome
			endif

			if g:myprojects_auto_resize || g:myprojects_auto_close
				if g:myprojects_auto_resize
					call s:resize(g:myprojects_width)
				endif

				if g:myprojects_auto_close
					call s:close()
				endif

				call s:goToEditWindow()
			endif

			return 1
		endif
	endfunction

	function s:getPathLineFromWorkingWindow(line)
		let line = 0

		let lineContent = getline(a:line)

		let path = substitute(lineContent, '^[[:space:]A-Z]\+\s\([^\s].*\)$', '\1', '')

		if path != lineContent && path != '' && getftype(path) == 'file'
			call s:goHome()

			let line = s:searchPath(path, 0)
		endif

		return line
	endfunction

	function s:openFromWorkingWindow(command)
		let line = s:getPathLineFromWorkingWindow(line('.'))

		if line > 0
			call s:edit(a:command, s:getPath(line), s:getRootPath(line), s:extractCd(line), s:extractMappings(line))
		endif
	endfunction

	function s:svnDiffFromWorkingWindow()
		let line = s:getPathLineFromWorkingWindow(line('.'))

		if line > 0
			call s:svnDiff(line)
		endif
	endfunction

	function s:open(command)
		let line = line('.')

		if s:isFolder(line)
			silent normal! za
		else
			let path = s:getPath(line)
			let rootPath = s:getRootPath(line)
			let cd = s:extractCd(line)
			let mappings = s:extractMappings(line)

			call s:edit(a:command, path, rootPath, cd, mappings)
		endif
	endfunction

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

	function s:getFiles(path, filter)
		let files = ''

		if isdirectory(a:path)
			let cwd = getcwd()

			silent execute 'cd ' . a:path

			for inode in sort(split(glob('*'), "\n"))
				if isdirectory(inode)
					let files .= ' ' . s:getFiles(a:path . s:osSlash . inode, a:filter)
				elseif a:filter == '' || match(inode, a:filter) != -1
					let files .= ' ' . a:path . s:osSlash . inode
				endif
			endfor

			silent execute 'cd ' . cwd
		endif

		return files
	endfunction

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

	function s:generateTags(line)
		if !executable(g:myprojects_tags_generator)
			call s:error(g:myprojects_tags_generator . ' is not available.')
		elseif g:myprojects_tags_file != ''
			let rootPath = s:getRootPath(a:line)

			if rootPath != ''
				let tagsPath = rootPath . s:osSlash . g:myprojects_tags_file

				call s:echo('Generate tags file ' . tagsPath . ' for project ' . s:getProjectName(a:line) . ', please wait...')
				silent execute '!' . g:myprojects_tags_generator . ' -f ' . tagsPath . ' -R ' . rootPath
				call s:echo('Tags file generated and stored in ' . tagsPath . '.')
			endif
		endif
	endfunction

	function s:appendFile(line)
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

	function s:put(myprojects, line)
		if a:myprojects != ''
			silent execute a:line - 1 . 'put =a:myprojects'
			silent normal! j
			if getline('.') =~ '^$'
				silent normal! dd
			endif
			call s:goToLine(a:line)
			silent normal! zx
		endif
	endfunction

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

	function s:getNestedPath(line)
		return s:getNestedAttribute('Path', a:line)
	endfunction

	function s:pathExists(path)
		let type = getftype(a:path)

		return type == 'dir' || type == 'file'
	endfunction

	function s:setPath(line, refresh)
		let pathLine = a:line

		if pathLine > 0 && pathLine <= line('$') && !s:hasPath(pathLine)
			let currentPath = s:getPath(pathLine)

			let newPath = s:cleanPath(s:input('Set path for ' . s:getName(pathLine) . ': ', currentPath, 'file'))

			if newPath != currentPath
				if !s:isAbsolutePath(newPath)
					call s:error('Path must be absolute.')
				elseif !s:pathExists(newPath)
					call s:error('Path ' . newPath . ' is invalid.')
				else
					call s:substitute(pathLine, '\(^\t*[^\t]\%(\\ \|\f\)\+\)', '\1=' . newPath, '')

					if a:refresh
						call s:refresh(pathLine, 0)
					endif
				endif
			endif
		endif
	endfunction

	function s:updatePath(line, refresh)
		let path = s:getNestedPath(a:line)

		if !empty(path)
			let [pathLine, currentPath] = path

			let newPath = s:input('Update path for ' . s:getName(pathLine) . ': ', currentPath, 'file')

			if newPath != currentPath
				call s:substitute(pathLine, '\(^\t*[^=]\+\)=\%(\\ \|\f\)\+', '\1', '')

				if newPath != ''
					let newPath = s:cleanPath(newPath)

					if !s:isAbsolutePath(newPath)
						call s:error('Path must be absolute.')
					elseif !s:pathExists(newPath)
						call s:error('Path ' . path . ' is invalid.')
					elseif newPath != s:getPath(pathLine)
						call s:substitute(pathLine, '\(^\t*[^=]\+=\)\%(\\ \|\f\)\+', '\1' . newPath, '')
					endif
				endif

				if a:refresh
					call s:refresh(pathLine, 0)
				endif
			endif
		endif
	endfunction

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

	function s:setMappings(line)
		let mappingsLine = a:line

		if mappingsLine > 0 && mappingsLine <= line('$') && getline(mappingsLine) !~ '\sF[1-9][0-2]\?="'
			let path = s:getPath(mappingsLine)

			for [key, mapping] in items(s:defineMappings('Set mapping for ' . path . ': ', {}))
				if mapping != ''
					call s:updateAttribute(mappingsLine, 'F' . key, mapping)
				endif
			endfor
		endif
	endfunction

	function s:updateMappings(line)
		let currentMappings = s:getNestedMappings(a:line)

		if !empty(currentMappings)
			let lines = {}
			let mappings = {}

			for [key, value] in items(currentMappings)
				let lines[key] = value[0]
				let mappings[key] = value[1]
			endfor

			for [key, mapping] in items(s:defineMappings('Update mapping: ', mappings))
				if has_key(lines, key)
					call s:updateAttribute(lines[key], 'F' . key, mapping)
				endif
			endfor
		endif
	endfunction

	function s:updateAttribute(line, attribute, value)
		if getline(a:line) =~ '\s\+' . a:attribute . '="[^"]\+"'
			if a:value == '' && a:attribute != 'filter'
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\{-}"', '', '')
			else
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\{-}"', ' ' . a:attribute . '="' . escape(a:value, '\%&') . '"', '')
			endif
		else
			call s:substitute(a:line, '\(^.\+$\)', '\1 ' . a:attribute . '="' . escape(a:value, '\') . '"', '')
		endif
	endfunction

	function s:getNestedFilter(line)
		return s:getNestedAttribute('Filter', a:line)
	endfunction

	function s:setFilter(line, refresh)
		let filterLine = s:isFolder(a:line) ? a:line : s:getFirstFolderLine(a:line)

		if filterLine > 0 && !s:hasFilter(filterLine)
			let filter = s:getNestedFilter(a:line)

			let currentFilter = empty(filter) ? '' : filter[1]

			let newFilter = s:input('Set filter for ' . s:getPath(filterLine) . ': ', currentFilter)

			if newFilter != currentFilter
				call s:updateAttribute(filterLine, 'filter', newFilter)

				if a:refresh
					call s:refresh(filterLine, 0)
				endif
			endif
		endif
	endfunction

	function s:updateFilter(line, refresh)
		let filter = s:getNestedFilter(a:line)

		if !empty(filter)
			let [filterLine, currentFilter] = filter

			let newFilter = s:input('Update filter for ' . s:getPath(filterLine) . ': ', currentFilter)

			if newFilter != currentFilter
				call s:updateAttribute(filterLine, 'filter', newFilter)

				if a:refresh
					call s:refresh(filterLine, 0)
				endif
			endif
		endif
	endfunction

	function s:getNestedCd(line)
		return s:getNestedAttribute('Cd', a:line)
	endfunction

	function s:updateCdAtttribute(line, cd)
		let cd = s:cleanPath(a:cd)

		if getftype(a:cd) != 'dir'
			call s:error('Working directory ' . a:cd . ' is not a valid directory.')
		else
			if cd == s:getPath(a:line)
				let cd = '.'
			endif

			call s:updateAttribute(a:line, 'cd', cd)
		endif
	endfunction

	function s:setCd(line)
		let cdLine = a:line

		if cdLine > 0 && cdLine <= line('$') && !s:hasCd(cdLine)
			let path = s:getPath(cdLine)
			let cd = s:getNestedCd(a:line)
			let currentCd = empty(cd) ? '' : cd[1]
			let newCd = s:input('Set working directory for ' . path . ': ', currentCd, 'file')

			if newCd != currentCd
				call s:updateCdAtttribute(cdLine, newCd)
			endif
		endif
	endfunction

	function s:updateCd(line)
		let cd = s:getNestedCd(a:line)

		if !empty(cd)
			let [cdLine, currentCd] = cd
			let path = s:getPath(cdLine)
			let newCd = s:input('Update working directory for ' . path . ': ', currentCd, 'file')

			if newCd != currentCd
				call s:updateCdAtttribute(cdLine, newCd)
			endif
		endif
	endfunction

	function s:getProjectName(line)
		let line = a:line

		while indent(line) > 0
			let line = s:getFirstFolderLine(line)
		endwhile

		return s:getName(line)
	endfunction

	function s:goToLine(line)
		silent execute 'normal! ' . a:line . 'G'
	endfunction

	function s:SID()
		return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID$')
	endfunction

	function s:substitute(line, search, replace, flags)
		silent execute ':' . a:line . 's/' . a:search . '/' . escape(a:replace, '/') . '/' . a:flags

		if &hlsearch
			set nohlsearch
		endif
	endfunction

	function s:getRootPath(line)
		let path = ''

		let line = s:getRootLine(a:line)

		if line > 0
			let path = s:getPath(line)
		endif

		return path
	endfunction

	function s:getFolderAttributes(line)
		let paths = {}

		let line = a:line

		if !s:isFolder(line)
			let line = s:getFirstFolderLine(line)
		endif

		if line > 0
			let endLine = s:getLastFolderLine(line)

			let position = getpos('.')

			call setpos('.', [0, line, 1])

			let lineAttribute = search('\(cd\|filter\|F[1-9][0-2]\?\)\?=', '', endLine)

			while lineAttribute
				let attributes = {}

				let pathAttribute = s:extractPathFromLine(lineAttribute)

				if pathAttribute != ''
					let attributes['path'] = pathAttribute
				endif

				let cdAttribute = s:extractCdFromLine(lineAttribute)

				if cdAttribute != ''
					let attributes['cd'] = cdAttribute
				endif

				if s:hasFilter(lineAttribute)
					let attributes['filter'] = s:extractFilterFromLine(lineAttribute)
				endif

				let mappings = s:extractMappingsFromLine(lineAttribute)

				if !empty(mappings)
					let attributes['mappings'] = mappings
				endif
					
				if !empty(attributes)
					let paths[s:getPath(lineAttribute)] = attributes
				endif

				let lineAttribute = search('\(cd\|filter\|F[1-9][0-2]\?\)\?=', '', endLine)
			endwhile

			call setpos('.', position)
		endif

		return paths
	endfunction

	function s:defineMappings(message, mappings)
		let mappings = a:mappings

		if empty(mappings)
			let mappings = {1: '', 2: '', 3: '', 4: '', 5: '', 6: '', 7: '', 8: '', 9: '', 10: '', 11: '', 12: ''}
		endif

		let index = 1

		while index >= 1 && index <= 13
			let list = [a:message]
			let keys = {}

			let index = 1

			while index <= 12
				if has_key(mappings, index)
					let list = add(list, index . '. F' . index . ': ' . mappings[index])
					let keys[index] = index
				endif

				let index += 1
			endwhile

			let index = inputlist(list)

			if has_key(keys, index)
				let mappings[keys[index]] = s:input('Mapping for F' . keys[index] . ': ', '')
				redraw
			endif
		endwhile

		return mappings
	endfunction

	function s:goToWorkingWindow(statusline)
		let window = s:getWorkingWindow()

		if window != -1
			silent execute window . 'wincmd w'
			silent execute 'buffer ' . s:workingBuffer
		else
			call s:goToEditWindow()

			silent execute 'split ' . s:sid . 'WorkingBuffer'

			let s:workingBuffer = winbufnr(0)

			setlocal buftype=nofile
			setlocal cursorline
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

			silent execute 'setlocal filetype=' . s:plugin

			if !has('syntax') || !g:myprojects_syntax
				setlocal filetype=""
			else
				silent execute 'setlocal filetype=' . s:plugin
				syntax on
			endif

			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <SID>openFromWorkingWindow('edit')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <SID>openFromWorkingWindow('edit')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <SID>openFromWorkingWindow('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <SID>openFromWorkingWindow('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <SID>openFromWorkingWindow('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <SID>openFromWorkingWindow('vs')<CR>
			nnoremap <buffer> <silent> <A-d> :call <SID>svnDiffFromWorkingWindow()<CR>

		endif

		silent execute 'setlocal statusline=' . escape(s:prompt, ' ') . a:statusline

		let b:titlestring = &titlestring
		let &titlestring = &statusline

		silent execute 'augroup ' . s:plugin
		silent execute 'au WinEnter <buffer> let &titlestring ="' . &statusline . '"'
		silent execute 'au WinLeave <buffer> let &titlestring ="' . b:titlestring . '"'
		silent execute 'au WinLeave <buffer> wincmd J'
		silent augroup END

		wincmd J
	endfunction

	function s:putLinesInWorkingWindow(lines, goToLine)
		let put = join(a:lines, "\n")

		silent execute ':%d'
		silent execute ':resize ' . len(a:lines)
		silent 0put =put
		silent $d
		silent execute 'normal! ' . a:goToLine . 'gg'
	endfunction

	function s:svnExists()
		let exists = executable('svn')

		if !exists
			call s:error('svn is not available.')
		endif

		return exists
	endfunction

	function s:svnStatus(line)
		let path = s:getPath(a:line)

		if path != ''

			try
				let lines = [s:prompt . 'Svn status on ' . path . ':']

				call extend(lines, s:svnDoStatus(path))
			catch /.*/
				let lines = [s:prompt . 'Unable to status path ' . path . ':']

				for line in split(v:exception, "\n")
					call add(lines, s:prompt . line)
				endfor
			endtry

			call s:goToWorkingWindow('Svn\ status\ of\ ' . path . '%=[%3p%%]')

			setlocal buftype=nofile

			call s:putLinesInWorkingWindow(lines, 2)
		endif
	endfunction

	function s:svnDoStatus(path)
		let files = []

		if s:svnExists() && s:pathExists(a:path)
			call s:echo('Performing svn status on ' . a:path . '...')
			let output = system('svn status ' . shellescape(a:path))
			call s:echo('Svn status done.')

			redraw

			if v:shell_error
				throw output
			else
				let files = filter(split(output, "\n"), "v:val =~ '^[[:space:]ACDIMRX?!~L+SKOTB]\\{6}\\s'")

				if g:myprojects_svn_sort
					let files = sort(files)
				endif
			endif
		endif

		return files
	endfunction

	function s:svnUpdate(line)
		let path = s:getPath(a:line)

		if path != ''
			call s:echo('Performing svn update on ' . path . '...')
			let output = system('svn update --accept postpone ' . shellescape(path)), "\n")
			call s:echo('Svn update done.')

			if v:shell_error
				let lines = [s:prompt . 'Unable to update path ' . path . ':']

				for line in split(output, "\n")
					call add(lines, s:prompt . line)
				endfor
			else
				let files = filter(split(output, "\n"), "v:val =~ '^[ADUCGE[:space:]]\\{4}\\s'")

				if empty(files)
					let lines = [s:prompt . 'There is no file to update in ' . path . '.']
				else
					let lines = [s:prompt . 'Svn update on ' . path . ':']

					if g:myprojects_svn_sort
						let files = sort(files)
					endif

					call extend(lines, files)

					call s:refresh(a:line, 0)
				endif
			endif

			call s:goToWorkingWindow('Svn\ update\ of\ ' . path . '%=[%3p%%]')

			setlocal buftype=nofile

			call s:putLinesInWorkingWindow(lines, 2)
		endif
	endfunction

	function s:svnCommit(line)
		let b:files = ''
		let b:message = ''

		if s:svnExists()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'strpart(v:val, 0, 6) =~ ''\%(M\|D\|A\|R\)''')

					if empty(files)
						let lines = [s:prompt . 'No file to commit.']
					else
						let lines = [s:prompt . 'Delete file if you don''t want to commit it and type :w...']

						call extend(lines, files)
					endif
				catch /.*/
					let lines = [s:prompt . 'Unable to commit path ' . path . ':']

					for line in split(v:exception, "\n")
						call add(lines, s:prompt . line)
					endfor
				endtry

				call s:goToWorkingWindow('Svn\ commit\ of\ ' . path . '%=[%3p%%]')

				if empty(files)
					let lines = [s:prompt . 'There is no file to commit on ' . path '.']
				else
						setlocal nomodified
						setlocal buftype=acwrite

						execute 'au! ' . s:plugin . ' BufWriteCmd'
						execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnGetCommitMessage()'
				endif

				call s:putLinesInWorkingWindow(lines, empty(files) ? '' : 2)
			endif
		endif
	endfunction

	function s:svnGetCommitMessage()
		if s:svnExists()
			setlocal nomodified

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, "v:val =~ 'strpart(v:val, 0, 6) =~ ''\%(M\|D\|A\|R\)'")

			if empty(b:files)
				setlocal buftype=nofile
				let lines = [s:prompt . 'No files to commit.']
			else
				let lines = [s:prompt . 'Define log message and type :w...', '']

				call extend(lines, b:files)

				execute 'au! ' . s:plugin . ' BufWriteCmd'
				execute 'au! ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoCommit()'
			endif

			call s:putLinesInWorkingWindow(lines, empty(b:files) ? '' : 2)
		endif
	endfunction

	function s:svnDoCommit()
		if s:svnExists()
			setlocal nomodified

			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:message = filter(getbufline('%', 1, '$'), "v:val !~ '^" . escape(s:prompt, '[]') . "'")

			call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

			call s:echo('Performing svn commit...')
			let output = system('svn commit ' . join(map(b:files, 'shellescape(v:val)'), ' ') . ' -m ' . shellescape(join(b:message, "\n")))
			call s:echo('Svn commit done.')

			if !v:shell_error
				let lines = [s:prompt . 'These files as been committed:']

				for file in b:files
					call add(lines,  s:prompt . file)
				endfor
			else
				let lines = [s:prompt . 'Unable to commit these files:']

				for file in b:files
					call add(lines, s:prompt . file)
				endfor

				for line in split(output, "\n")
					call add(lines, s:prompt . line)
				endfor
			endif

			call s:putLinesInWorkingWindow(lines, '')
		endif
	endfunction

	function s:svnRevert(line)
		let b:files = ''
		let b:message = ''

		if s:svnExists()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'strpart(v:val, 0, 6) =~ ''\%(M\|D\|R\|C\)''')

					if empty(files)
						let lines = [s:prompt . 'No file to revert.']
					else
						let lines = [s:prompt . 'Delete file if you don''t want to revert it and type :w...']

						call extend(lines, files)
					endif
				catch /.*/
					let lines = [s:prompt . 'Unable to revert path ' . path . ':']

					for line in split(v:exception, "\n")
						call add(lines, s:prompt . line)
					endfor
				endtry

				call s:goToWorkingWindow('Svn\ revert\ of\ ' . path . '%=[%3p%%]')

				if empty(files)
					let lines = [s:prompt . 'There is no file to revert on ' . path '.']
				else
					if g:myprojects_svn_sort
						let files = sort(files)
					endif

					setlocal nomodified
					setlocal buftype=acwrite

					execute 'au! ' . s:plugin . ' BufWriteCmd'
					execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoRevert()'
				endif

				call s:putLinesInWorkingWindow(lines, empty(files) ? '' : 2)
			endif
		endif
	endfunction

	function s:svnDoRevert()
		if s:svnExists()
			setlocal nomodified

			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, 'strpart(v:val, 0, 6) =~ ''\%(M\|D\|R\|C\)''')

			if empty(b:files)
				let lines = [s:prompt . 'No files to revert.']
			else
				call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

				call s:echo('Performing svn revert...')
				let output = system('svn revert ' . join(map(b:files, 'shellescape(v:val)'), ' '))
				call s:echo('Svn revert done.')

				if !v:shell_error
					let lines = [s:prompt . 'These files as been reverted:']

					for file in b:files
						call add(lines,  s:prompt . file)
					endfor
				else
					let lines = [s:prompt . 'Unable to revert these files:']

					for file in b:files
						call add(lines, s:prompt . file)
					endfor

					for line in split(output, "\n")
						call add(lines, s:prompt . line)
					endfor
				endif
			endif

			call s:putLinesInWorkingWindow(lines, '')
		endif
	endfunction

	function s:svnAdd(line)
		let b:files = ''
		let b:message = ''

		if s:svnExists()
			let path = s:getPath(a:line)

			if path != ''
				let files = []

				try
					let files = filter(s:svnDoStatus(path), 'v:val =~ "^?"')

					if empty(files)
						let lines = [s:prompt . 'There is no file to add on path ' . path . '.']
					else
						let lines = [s:prompt . 'Delete file if you don''t want to add it and type :w...']

						call extend(lines, files)
					endif
				catch /.*/
					let lines = [s:prompt . 'Unable to add from path ' . path . ':']

					for line in split(v:exception, "\n")
						call add(lines, s:prompt . line)
					endfor
				endtry

				call s:goToWorkingWindow('Svn\ add\ of\ ' . path . '%=[%3p%%]')

				if !empty(files)
						setlocal nomodified
						setlocal buftype=acwrite

						execute 'au! ' . s:plugin . ' BufWriteCmd'
						execute 'au ' . s:plugin . ' BufWriteCmd <buffer> call ' . s:sid . 'svnDoAdd()'
				endif

				call s:putLinesInWorkingWindow(lines, empty(files) ? '' : 2)
			endif
		endif
	endfunction

	function s:svnDoAdd()
		if s:svnExists()
			setlocal nomodified

			setlocal buftype=nofile

			execute 'au! ' . s:plugin . ' BufWriteCmd'

			let b:files = getbufline('%', 1, '$')

			call filter(b:files, "v:val =~ '^?'")

			if empty(b:files)
				let lines = [s:prompt . 'No files to add.']
			else
				call map(b:files, "substitute(v:val, '^[^\\s]\\+\\s', '', '')")

				call s:echo('Performing svn add...')
				let output = system('svn add ' . join(map(b:files, 'shellescape(v:val)'), ' '))
				call s:echo('Svn add done.')

				if !v:shell_error
					let lines = [s:prompt . 'These files as been added:']

					for file in b:files
						call add(lines,  s:prompt . file)
					endfor
				else
					let lines = [s:prompt . 'Unable to add these files:']

					for file in b:files
						call add(lines, s:prompt . file)
					endfor

					for line in split(output, "\n")
						call add(lines, s:prompt . line)
					endfor
				endif
			endif

			call s:putLinesInWorkingWindow(lines, '')
		endif
	endfunction

	function s:svnDiff(line)
		if s:svnExists()
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

	function s:projectIsUnique(project)
		return search('^\t*' . a:project . '=.\+$', 'nw') == 0
	endfunction

	function s:svn(command, line, refresh)
		let path = s:getPath(a:line)

		if path != ''
			execute '!svn ' . a:command . ' ' . path
		endif

		if a:refresh
			call s:refresh(a:line, 0)
		endif
	endfunction

	function s:error(message)
		echohl errorMsg
		call s:echo(a:message)
		echohl normal
	endfunction

	function s:echo(message)
		redraw
		echo s:prompt . a:message
	endfunction

	function s:echoVersion()
		call s:echo('Version ' . s:version . ' (c) ' . s:author . ' ' . s:copyright . ' - ' . s:email . ' - ' . s:webSite)
	endfunction

	function s:echoMyprojectsFile()
		call s:echo('Currently used file ' . s:filename)
	endfunction

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

	function s:initVariable(name, value)
		if !exists(a:name)
			let {a:name} = a:value
		endif
	endfunction

	function s:setLocal(name, bool)
		silent execute 'setlocal ' . (a:bool ? a:name : 'no' . a:name)
	endfunction

	function s:getMyProjects(line)
		let myprojects = {}

		let name = s:getName(a:line)

		if name != ''
			let myprojects[name] = {}

			let attributes = {}

			let pathAttribute = s:extractPathFromLine(a:line)

			if pathAttribute != ''
				let attributes['path'] = pathAttribute
			endif

			let cdAttribute = s:extractCdFromLine(a:line)

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

	let &cpo= s:keepCpo
	unlet s:keepCpo

	let g:myprojects_enable = 1
endif

finish
