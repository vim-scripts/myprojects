"=============================================================================
" File:        myprojects.vim
" Author:      Frédéric Hardy (fhardy at noparking.net)
" Last Change: Wed Mar 11 08:48:45 CET 2009
" Licence:     GPL version 2.0 license
" GetLatestVimScripts: 2556 10039 :AutoInstall: myprojects.vim
"=============================================================================
if v:version < 700
    echoerr "myprojects.vim requires vim >= 7. DOWNLOAD IT! You'll thank me later"
elseif !has('folding')
    echoerr "myprojects.vim requires folding"
elseif !exists('myprojects_enable')
	let s:keepCpo= &cpo
	setlocal cpo&vim

	let s:version = '0.0.29'
	let s:copyright = '2009'
	let s:author = 'Frédéric Hardy'
	let s:email = 'fhardy at noparking.net'
	let s:webSite = 'http://blog.mageekbox.net'
	let s:prompt = '[myproject] '
	let s:buffer = -1
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
		call s:initVariable('g:myprojects_file', s:home . s:osSlash . '.myprojects')
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

		if s:goToWindow() < 0
			let s:filename = a:filename == '' ? g:myprojects_file : a:filename

			execute 'leftabove vertical new ' . s:filename

			let s:buffer = bufnr('%')

			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <SID>open('')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <SID>open('')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-d> :call <SID>delete(line('.'))<CR>
			nnoremap <buffer> <silent> <C-c> :call <SID>create(line('.'))<CR>
			nnoremap <buffer> <silent> <C-r> :call <SID>refresh(line('.'))<CR>
			nnoremap <buffer> <silent> <C-g> :call <SID>grep(line('.'))<CR>
			nnoremap <buffer> <silent> <C-t> :call <SID>generateTags(line('.'))<CR>
			nnoremap <buffer> <silent> <C-e> :call <SID>explore('E')<CR>
			nnoremap <buffer> <silent> <C-S-e> :call <SID>explore('Se')<CR>
			nnoremap <buffer> <silent> <C-a> :call <SID>appendFile(line('.'))<CR>
			nnoremap <buffer> <silent> <C-s> :call <SID>saveSession(line('.'))<CR>
			nnoremap <buffer> <silent> <C-A-s> :call <SID>loadSession(line('.'))<CR>
			nnoremap <buffer> <silent> <C-A-p> :call <SID>updatePath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <C-A-f> :call <SID>updateFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <C-A-c> :call <SID>updateCd(line('.'))<CR>
			nnoremap <buffer> <silent> <C-A-m> :call <SID>updateMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <C-i> :call <SID>echo('Path: ' . <SID>getPath(line('.')))<CR>
			nnoremap <buffer> <silent> <C-Tab> :call <SID>goToPreviousWindow()<CR>
			nnoremap <buffer> <silent> <C-Up> :call <SID>moveUp(line('.'))<CR>
			nnoremap <buffer> <silent> <C-k> :call <SID>moveUp(line('.'))<CR>
			nnoremap <buffer> <silent> <C-Down> :call <SID>moveDown(line('.'))<CR>
			nnoremap <buffer> <silent> <C-j> :call <SID>moveDown(line('.'))<CR>
			nnoremap <buffer> <silent> <C-Right> :call <SID>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-l> :call <SID>resize(winwidth(0) + g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Left> :call <SID>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-h> :call <SID>resize(winwidth(0) - g:myprojects_resize_step)<CR>
			nnoremap <buffer> <silent> <C-Space> :call <SID>toggleMaximize()<CR>
			nnoremap <buffer> <silent> <C-v> :call <SID>echoVersion()<CR>
			nnoremap <buffer> <silent> <C-p> :call <SID>echoMyprojectsFile()<CR>
			nnoremap <buffer> <silent> <A-u> :call <SID>{g:myprojects_version_control_system}('update', line('.'), 1)<CR>
			nnoremap <buffer> <silent> <A-r> :call <SID>{g:myprojects_version_control_system}('revert', line('.'), 1)<CR>
			nnoremap <buffer> <silent> <A-d> :call <SID>{g:myprojects_version_control_system}('diff', line('.'), 0)<CR>
			nnoremap <buffer> <silent> <A-c> :call <SID>{g:myprojects_version_control_system}('commit', line('.'), 0)<CR>
			nnoremap <buffer> <silent> <A-s> :call <SID>{g:myprojects_version_control_system}('status', line('.'), 0)<CR>

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

			let sid = s:SID()

			execute 'setlocal statusline=' . escape(s:prompt, ' ') . '%=[%f\ %3p%%]'
			execute 'setlocal foldtext=' . sid . 'foldtext()'
			execute 'setlocal foldexpr=' . sid . 'foldexpr()'
			execute 'setlocal foldcolumn=' . g:myprojects_foldcolumn
			execute 'au! BufEnter <buffer> call' . sid . 'bufEnter()'

			if !has('syntax') || !g:myprojects_syntax
				setlocal filetype=""
			else
				setlocal filetype=myprojects
				syntax on
			endif

			call s:float()
			call s:resize(g:myprojects_width)

			if foldlevel(line('.'))
				normal! zo
			endif

			redraw

			if g:myprojects_display_path_in_statusline
				call <SID>echo(<SID>getPath(line('.')))
			endif
		endif
	endfunction

	function s:getSessionFile(line)
		let path = ''

		let line = s:getRootLine(a:line)

		if line > 0
			let path = g:myprojects_sessions_directory . s:osSlash . fnameescape(s:getName(line))
		endif

		return path
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

	function s:saveSession(line)
		call s:goHome()

		if !isdirectory(g:myprojects_sessions_directory)
			if !exists("*mkdir")
				call s:error('Unable to create sessions directory ' . g:myprojects_sessions_directory . ', mkdir() function does not exists, please create it manualy')
			else
				 call mkdir(g:myprojects_sessions_directory, 'p', 0700)
			endif
		endif

		if !isdirectory(g:myprojects_sessions_directory)
			call s:error('Unable to create session, directory ' . g:myprojects_sessions_directory . ' does not exist')
		else
			let session = s:getSessionFile(a:line)

			if session == ''
				call s:error('Unable to create session')
			elseif s:wipeout()
				execute 'mksession! ' . session
				call s:myProjects(s:filename)
				call s:echo('Session saved.')
			endif
		endif
	endfunction

	function s:loadSession(line)
		call s:goHome()

		let session = s:getSessionFile(a:line)

		if !filereadable(session)
			call s:error('Unable to read session file ' . session)
		elseif s:wipeout()
			execute 'source ' . session
			call s:myProjects(s:filename)
			call s:echo('Session loaded.')
		endif
	endfunction

	function s:goToPreviousWindow()
		wincmd p

		if winnr() == s:getWindow()
			width = winwidth(0)
			silent execute 'vertical rightbelow new'
			call s:float()
			call s:resize(width)
		endif
	endfunction

	function s:getWindow()
		let window = -1

		if s:buffer != -1
			let window = bufwinnr(s:buffer)
		endif

		return window
	endfunction

	function s:goHome()
		if s:goToWindow() == -1
			call s:myProjects(s:filename)
		endif
	endfunction

	function s:goToWindow()
		let window = s:getWindow()

		if window != -1
			execute window . 'wincmd w'
		endif

		return window
	endfunction

	function s:bufEnter()
		if winnr('$') == 1
			quit
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
			execute 'vertical resize ' . a:width
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
		let currentIndent = indent(v:lnum) / &tabstop
		let nextIndent = indent(nextnonblank(v:lnum + 1)) / &tabstop
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

		let indent = indent(a:line)

		if indent > 0
			let line = search('^' . repeat('	', (indent / &tabstop) - 1) . '[^\t]\+$', 'bnW')
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
			let endLine = search('^\t\{0,' . (indent(line) / &tabstop) . '\}[^\t].*$', 'nW') - 1

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
			let name = substitute(line, '^\s*\([^=[:space:]]\+\).*$', '\1', '')
		endif

		return name
	endfunction

	function s:getPath(line)
		let path = s:extractPath(a:line)
		let folderLine = s:getFirstFolderLine(a:line)

		while folderLine > 0
			let path = s:extractPath(folderLine) . s:osSlash . path
			let folderLine = s:getFirstFolderLine(folderLine)
		endwhile

		return path
	endfunction

	function s:hasPath(line)
		return getline(a:line) =~ '^\s*[^=]\+=\(\(\\ \|\f\)\+\).*$'
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

	function s:hasAttribute(name, line)
		return s:isFolder(a:line) && getline(a:line) =~ '.*\s\+' . a:name . '="[^"]\+"'
	endfunction

	function s:extractAttributeFromLine(name, line)
		return !s:hasAttribute(a:name, a:line) ? '' : substitute(getline(a:line), '.*\s\+' . a:name . '="\([^"]\+\)".*', '\1', '')
	endfunction

	function s:extractAttribute(name, line)
		let line = a:line

		let attribute = s:extractAttributeFromLine(a:name, line)

		while line > 0 && attribute == ''
			let line = s:getFirstFolderLine(line)
			let attribute = s:extractAttributeFromLine(a:name, line)
		endwhile

		return attribute
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
			let cd = s:extractCdFromLine(line)
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
				call s:refresh(s:getFirstFolderLine(a:line))
			else
				call s:error('Unable to delete ' . path)
			endif
		endif
	endfunction

	function s:extractMappings(line)
		let mappings = {}

		let line = s:getFirstFolderLine(a:line)

		if line == 0
			let line = a:line
		endif

		while line > 0 && len(mappings) < 12
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

	function s:buildFolder(name, level, path, addPath, cd, addCd, filter, addFilter, mappings, folderAttributes)
		let folder = ''

		let cwd = getcwd()

		silent execute 'cd ' . a:path

		let inodes = sort(split(glob('*') . "\n" . glob('.*'), "\n"))

		for inode in inodes
			if inode != '.' && inode != '..' && (a:filter == '' || match(inode, a:filter) != -1)
				if isdirectory(inode)
					let path = a:path . s:osSlash . inode

					if !has_key(a:folderAttributes, path)
						let cd = ''
						let filter = a:filter
						let mappings = {}
					else
						let cd = !has_key(a:folderAttributes[path], 'cd') ? '' : a:folderAttributes[path]['cd'] == path ? '.' : a:folderAttributes[path]['cd']
						let filter = !has_key(a:folderAttributes[path], 'filter') ? a:filter : a:folderAttributes[path]['filter']
						let mappings = !has_key(a:folderAttributes[path], 'mappings') ? {} :  a:folderAttributes[path]['mappings']
					endif

					let files = s:buildFolder(inode, a:level + 1, path, 0, cd, cd != '', filter, filter != '' && filter != a:filter, mappings, a:folderAttributes)

					if files != ''
						let folder .= "\n" . files
					endif
				else
					let folder .= "\n" . repeat('	', a:level + 1) . inode
				endif
			endif
		endfor

		silent execute 'cd ' . cwd

		if folder != '' || g:myprojects_display_empty_folder == 1
			let name = repeat('	', a:level) . a:name

			if a:addPath
				let name .= '=' . a:path
			endif

			if a:addCd
				let name .= ' cd="' . a:cd . '"'
			endif

			if a:addFilter
				let name .= ' filter="' . a:filter . '"'
			endif

			for [index, value] in items(a:mappings)
				if value != ''
					let name .= ' F' . index . '="' . value . '"'
				endif
			endfor

			let folder = name . folder
		endif

		return folder
	endfunction

	function s:isAbsolutePath(path)
		return s:windowsOs ? a:path =~ '^.:\(\\\|\/\)' : a:path =~ '^/'
	endfunction

	function s:create(line)
		let line = search('^[^\t]', 'bnW')

		if line <= 0
			let line = 1
		endif

		let name = s:input('Name: ', '')

		if name == ''
			call s:error('Name must not be empty')
		else
			let path = fnamemodify(s:input('Path: ', '', 'file'), ':p')
			let path = substitute(path, escape(s:osSlash, '/') . '\+$', '', '')

			if !s:isAbsolutePath(path)
				call s:error('Path must be absolute')
			elseif getftype(path) != 'dir'
				call s:error('Path ' . path . ' is not a valid directory')
			else
				let cd = s:input('Working directory: ', '', 'file')

				if cd != '.'
					let cd = fnamemodify(cd, ':p')
				endif

				if cd != '.' && getftype(cd) != 'dir'
					call s:error('Working directory ' . cd . ' is not a valid directory')
				else
					let filter = s:input('Filter :', '')
					let mappings = s:defineMappings({})

					call s:echo('Create project ' . name . ' from path ' . path . ', please wait...')
					call s:appendFolder(s:buildFolder(name, 0, path, 1, cd, cd != '', filter, filter != '', mappings, {}), line)
					call s:echo('Project ' . name . ' created.')
				endif
			endif
		endif
	endfunction

	function s:refresh(line)
		let line = a:line

		if !s:isFolder(line)
			let line = s:getFirstFolderLine(line)

			if line <= 0
				let line = a:line
			endif
		endif

		let level = foldlevel(line)

		if s:isFolder(line)
			let level -= 1
		endif

		let name = s:getName(line)
		let path = s:getPath(line)
		let addPath = s:hasPath(line)
		let addCd = s:hasCd(line)
		let cd = s:extractCd(line)
		let addFilter = s:hasFilter(line)
		let filter = s:extractFilter(line)
		let mappings = s:extractMappingsFromLine(line)
		let folderAttributes = s:getFolderAttributes(line)

		if cd != '' && cd == path
			let cd = '.'
		endif

		call s:goToLine(line)

		if s:isFolder(line) && foldclosed(line) == -1
			normal! zc
		endif

		silent normal! dd

		if getftype(path) == 'dir'
			call s:echo('Refresh ' . path . ' in project ' . s:getProjectName(line) . ', please wait...')
			call s:appendFolder(s:buildFolder(name, level, path, addPath, cd, addCd, filter, addFilter, mappings, folderAttributes), line)
			call s:echo(path . ' was refreshed.')
		endif
	endfunction

	function s:open(split)
		let line = line('.')

		if s:isFolder(line)
			normal! za
		else
			let path = s:getPath(line)
			let rootPath = s:getRootPath(line)
			let cd = s:extractCd(line)
			let mappings = s:extractMappings(line)
			let split = a:split == '' ? 'edit' : a:split
			let window = bufwinnr(path)
			let type = getftype(path)

			if type != '' && !filereadable(path)
				call s:error('Unable to read file ' . path)
			elseif (type != '' && type != 'file' && type != 'link')
				call s:error('Unable to open file of type ' . type)
			else
				if window != -1
					silent execute window . 'wincmd w'
					execute 'buffer ' . bufnr(path)
				else
					let directories = split(strpart(path, 1), s:osSlash)
					call remove(directories, -1)

					let dirname = ''

					for directory in directories
						let dirname .= s:osSlash . directory

						if getftype(dirname) == ''
							call mkdir(dirname)
						endif
					endfor

					call  s:goToPreviousWindow()

					try
						execute split ' ' . path
					catch E37
						if s:input('Unsaved buffer. Save and load ' . path . ' ? [y/N]: ', '') != 'y'
							return
						else
							write
							execute split ' ' . path
						endif
					endtry
				endif

				if cd != ''
					if getftype(cd) != 'dir'
						call s:error("Unable to change directory to " . cd)
					else
						silent execute 'cd ' . cd
						silent augroup myprojects
						silent execute 'au BufEnter <buffer> cd ' . cd
						silent augroup END
					endif
				endif

				if g:myprojects_tags_file != '' && rootPath != ''
					let tagsPath = rootPath . s:osSlash . g:myprojects_tags_file

					if getftype(tagsPath) == 'file'
						if &tags !~ '^' . tagsPath . ',\?'
							silent execute 'set tags=' . tagsPath . ',' . &tags
						endif
					endif
				endif

				setlocal more

				for [line, mapping] in items(mappings)
					for [key, value] in items(mapping)
						execute 'nmap <buffer> <silent> <F' . key . '> ' . expand(value)
					endfor
				endfor

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

					call s:goToPreviousWindow()
				endif
			endif
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
					silent! execute 'vimgrep /' . escape(pattern, '/') . '/jg ' . files
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
				call s:goToPreviousWindow()
				silent execute a:mode . 'xplore ' . path
			endif
		endif
	endfunction

	function s:generateTags(line)
		if g:myprojects_tags_file != ''
			let rootPath = s:getRootPath(line('.'))

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
				call s:error('Unable to read file ' . path)
			else
				call s:goToPreviousWindow()
				execute ':r ' . path
				normal k
				normal dd
			endif
		endif
	endfunction

	function s:appendFolder(folder, line)
		call s:goToLine(a:line)

		if a:folder != ''
			call append(a:line - 1, split(a:folder, '\n'))
			normal! zM
			call s:goToLine(a:line)
			normal! zv
		endif
	endfunction

	function s:getNestedAttribute(name, line)
		let attribute = []

		let line = a:line

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

		return attribute
	endfunction

	function s:getNestedPath(line)
		return s:getNestedAttribute('Path', a:line)
	endfunction

	function s:getNestedCd(line)
		return s:getNestedAttribute('Cd', a:line)
	endfunction

	function s:getNestedFilter(line)
		return s:getNestedAttribute('Filter', a:line)
	endfunction

	function s:updatePath(line, refresh)
		let path = s:getNestedPath(a:line)

		if !empty(path)
			let [pathLine, currentPath] = path

			let newPath = s:input('Path: ', currentPath, 'file')

			if newPath != '' && newPath != currentPath
				call s:substitute(pathLine, '\(^\s*[^=]\+=\)\%(\\ \|\f\)\+', '\1' . fnamemodify(newPath, ':p'), '')

				if a:refresh
					call s:refresh(pathLine)
				endif
			endif
		endif
	endfunction

	function s:updateMappings(line)
		let currentMappings = s:extractMappings(a:line)

		if !empty(currentMappings)
			let lines = {}

			for [line, mappings] in items(currentMappings)
				for [key, mapping] in items(mappings)
					if mapping != ''
						let mappings[key] = mapping
						let lines[key] = line
					endif
				endfor
			endfor

			for [key, mapping] in items(s:defineMappings(mappings))
				if has_key(lines, key)
					call s:updateAttribute(lines[key], 'F' . key, mapping)
				endif
			endfor
		endif
	endfunction

	function s:updateAttribute(line, attribute, value)
		if getline(a:line) =~ '\s\+' . a:attribute . '="[^"]\+"'
			if a:value == ''
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\+"', '', '')
			else
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\+"', ' ' . a:attribute . '="' . escape(a:value, '\') . '"', '')
			endif
		elseif a:value != ''
			call s:substitute(a:line, '\(^.\+$\)', '\1 ' . a:attribute . '="' . escape(a:value, '\') . '"', '')
		endif
	endfunction

	function s:updateFilter(line, refresh)
		let filter = s:getNestedFilter(a:line)

		if !empty(filter)
			let [filterLine, currentFilter] = filter
		else
			let filterLine = s:getFirstFolderLine(a:line)

			if filterLine == 0
				let filterLine = a:line
			endif

			let currentFilter = ''
		endif

		let newFilter = s:input('Filter: ', currentFilter)

		if newFilter != currentFilter
			call s:updateAttribute(filterLine, 'filter', newFilter)

			if a:refresh
				call s:refresh(filterLine)
			endif
		endif
	endfunction

	function s:updateCd(line)
		let cd = s:getNestedCd(a:line)

		if !empty(cd)
			let [cdLine, currentCd] = cd
		else
			let cdLine = s:getFirstFolderLine(a:line)

			if cdLine == 0
				let cdLine = a:line
			endif

			let currentCd = ''
		endif

		let newCd = s:input('Cd: ', currentCd)

		if newCd != currentCd
			call s:updateAttribute(cdLine, 'cd', newCd)
		endif
	endfunction

	function s:getProjectName(line)
		let line = a:line

		while line > 0
			let line = s:getFirstFolderLine(line)
		endwhile

		return s:getName(line)
	endfunction

	function s:goToLine(line)
		execute 'normal! ' . a:line . 'G'
	endfunction

	function s:SID()
		return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID$')
	endfunction

	function s:substitute(line, search, replace, flags)
		let hlsearch = &hlsearch

		if hlsearch
			set nohlsearch
		endif

		execute ':' . a:line . 's/' . a:search . '/' . escape(a:replace, '/') . '/' . a:flags

		if hlsearch
			set hlsearch
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

	function s:moveUp(line)
		let isFolder = s:isFolder(a:line)

		if isFolder && foldclosed(a:line) == -1
			normal! zc
		endif

		normal! dd
		normal! k
		normal! P

		if isFolder
			normal! zc
		endif
	endfunction

	function s:moveDown(line)
		let isFolder = s:isFolder(a:line)

		if isFolder && foldclosed(a:line) == -1
			normal! zc
		endif

		normal! dd
		normal! p

		if isFolder
			normal! zc
		endif
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
				let linePath = s:extractPathFromLine(lineAttribute)
				let lineCd = s:extractCdFromLine(lineAttribute)
				let lineFilter = s:extractFilterFromLine(lineAttribute)
				let lineMappings = s:extractMappingsFromLine(lineAttribute)

				if linePath != '' || lineCd != '' || lineFilter != '' || !empty(lineMappings)
					let attributes = {}

					if linePath != ''
						let attributes['path'] = linePath
					endif

					if lineCd != ''
						let attributes['cd'] = lineCd
					endif

					if lineFilter != ''
						let attributes['filter'] = lineFilter
					endif

					if !empty(lineMappings)
						let attributes['mappings'] = lineMappings
					endif

					let paths[s:getPath(lineAttribute)] = attributes
				endif

				let lineAttribute = search('\(cd\|filter\|F[1-9][0-2]\?\)\?=', '', endLine)
			endwhile

			call setpos('.', position)
		endif

		return paths
	endfunction

	function s:defineMappings(mappings)
		let mappings = a:mappings

		if empty(mappings)
			let mappings = {1: '', 2: '', 3: '', 4: '', 5: '', 6: '', 7: '', 8: '', 9: '', 10: '', 11: '', 12: ''}
		endif

		let index = 1

		while index >= 1 && index <= 13
			let list = ['Define mapping for key: ']
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

			if index >= 1 && index <= len(list)
				let mapping = s:input('Mapping for F' . keys[index] . ': ', '')

				if mapping != ''
					let mappings[keys[index]] = mapping
				endif

				redraw
			endif
		endwhile

		return mappings
	endfunction

	function s:svn(command, line, refresh)
		let path = s:getPath(a:line)

		if path != ''
			execute '!svn ' . a:command . ' ' . path
		endif

		if a:refresh
			call s:refresh(a:line)
		endif
	endfunction

	function s:echoVersion()
		call s:echo('Version ' . s:version . ' (c) ' . s:author . ' ' . s:copyright . ' - ' . s:email . ' - ' . s:webSite)
	endfunction

	function s:echoMyprojectsFile()
		call s:echo('Currently used file ' . s:filename)
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

	function s:input(prompt, ...)
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
		execute 'setlocal ' . (a:bool ? a:name : 'no' . a:name)
	endfunction

	let &cpo= s:keepCpo
	unlet s:keepCpo

	let g:myprojects_enable = 1
endif

finish
