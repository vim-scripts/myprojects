"=============================================================================
" File:        myprojects.vim
" Author:      Frédéric Hardy (fhardy at noparking.net)
" Last Change: Mon Mar  2 09:37:42 CET 2009
" Version:     0.0.10
" Licence:     GPL version 2.0 license
" GetLatestVimScripts: 2556 10039 :AutoInstall: myprojects.vim
"=============================================================================
if !exists('myprojects_enable')
	let s:buffer = -1
	let s:filename = ''

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

	if !exists('g:myprojects_width')
		let g:myprojects_width = 30
	endif

	if !exists('g:myprojects_file')
		let g:myprojects_file = '~/.myprojects'
	endif

	if !exists('g:myprojects_tags_file')
		let g:myprojects_tags_file = '.tags'
	endif

	function s:myProjects(filename)
		if s:goToWindow() < 0
			if a:filename == ''
				let s:filename = g:myprojects_file
			else
				let s:filename = a:filename
			endif

			silent execute 'leftabove vertical new ' . s:filename

			let s:buffer = bufnr('%')

			nnoremap <buffer> <silent> <S-LeftMouse> <LeftMouse>
			nnoremap <buffer> <silent> <Return> :call <SID>open('')<CR>
			nnoremap <buffer> <silent> <2-Leftmouse> :call <SID>open('')<CR>
			nnoremap <buffer> <silent> <S-Return> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <S-2-Leftmouse> :call <SID>open('sp')<CR>
			nnoremap <buffer> <silent> <C-Return> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-2-Leftmouse> :call <SID>open('vs')<CR>
			nnoremap <buffer> <silent> <C-c> :call <SID>create(line('.'))<CR>
			nnoremap <buffer> <silent> <C-r> :call <SID>refresh(line('.'))<CR>
			nnoremap <buffer> <silent> <C-g> :call <SID>grep(line('.'))<CR>
			nnoremap <buffer> <silent> <C-t> :call <SID>generateTags(line('.'))<CR>
			nnoremap <buffer> <silent> <C-e> :call <SID>explore('E')<CR>
			nnoremap <buffer> <silent> <C-S-e> :call <SID>explore('Se')<CR>
			nnoremap <buffer> <silent> <C-A-p> :call <SID>updatePath(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <C-A-f> :call <SID>updateFilter(line('.'), 1)<CR>
			nnoremap <buffer> <silent> <C-A-c> :call <SID>updateCd(line('.'))<CR>
			nnoremap <buffer> <silent> <C-A-m> :call <SID>updateMappings(line('.'))<CR>
			nnoremap <buffer> <silent> <C-Tab> :call <SID>goToPreviousWindow()<CR>
			nnoremap <buffer> <silent> <C-Up> :call <SID>moveUp(line('.'))<CR>
			nnoremap <buffer> <silent> <C-Down> :call <SID>moveDown(line('.'))<CR>

			setlocal autoindent
			setlocal autoread
			setlocal cindent
			setlocal cursorcolumn
			setlocal cursorline
			setlocal expandtab
			setlocal foldclose=""
			setlocal foldcolumn=0
			setlocal foldenable
			setlocal foldmethod=expr
			setlocal nobuflisted
			setlocal noequalalways
			setlocal noexpandtab
			setlocal nolist
			setlocal nomodeline
			setlocal nonumber
			setlocal noruler
			setlocal noswapfile
			setlocal nowrap
			setlocal shiftwidth=3
			setlocal splitbelow
			setlocal splitright
			setlocal statusline=%f%=\ [%3p%%]
			setlocal tabstop=3

			let sid = s:SID()

			execute 'setlocal foldtext=' . sid . 'foldtext()'
			execute 'setlocal foldexpr=' . sid . 'foldexpr()'

			execute 'au! BufEnter <buffer> call' . sid . 'bufEnter()'

			if has('syntax')
				syntax on

				syntax match MyProjectsFile '.'
				highlight default MyProjectsFile ctermfg=green guifg=#60ff60 gui=none cterm=none

				syntax match MyProjectsFolder '^\(\t*\)[^\t\n][^\t]*\n\ze\t\1'
				highlight default MyProjectsFolder guifg=cyan ctermfg=cyan
			endif

			call s:float()
			call s:resize(g:myprojects_width)

			if foldlevel(line('.'))
				normal! zo
			endif
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
		let window = s:goToWindow()

		if window != -1
			hide
		endif

		return window != -1
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

	function s:getFolderLine(line)
		let line = 0

		let indent = indent(a:line)

		if indent > 0
			let line = a:line

			while line > 0 && indent(line) >= indent
				let line -= 1
			endwhile
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
		let folderLine = s:getFolderLine(a:line)

		while folderLine > 0
			let path = s:extractPath(folderLine) . '/' . path
			let folderLine = s:getFolderLine(folderLine)
		endwhile

		return path
	endfunction

	function s:hasPath(line)
		return getline(a:line) =~ '^\s*[^=]\+=\(\(\\ \|\f\)\+\).*$'
	endfunction

	function s:extractPathFromLine(line)
		return !s:hasPath(a:line) ? '' : substitute(getline(a:line), '^\s*[^=]\+=\(\(\\ \|\f\)\+\).*$', '\1', '')
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
		let attribute = ''
		let line = a:line + 1

		while line > 0 && indent(line) > 0 && attribute == ''
			let line -= 1
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
		let cd = ''
		let line = a:line + 1

		while line > 0 && indent(line) > 0 && cd == ''
			let line -= 1
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

	function s:extractMappings(line)
		let mappings = {}

		let line = a:line + 1

		while line > 0 && indent(line) > 0 && len(mappings) < 12
			let line -= 1

			let index = 1

			while index <= 12
				if !has_key(mappings, index) && s:hasMapping('F' . index, line)
					let mapping = substitute(getline(line), '.*\s\+F' . index . '="\([^"]\+\)".*', '\1', '')

					if mapping != ''
						let mappings[line] = {index : mapping}
					endif
				endif

				let index += 1
			endwhile
		endwhile

		return mappings
	endfunction

	function s:isFolder(line)
		return indent(a:line) < indent(nextnonblank(a:line + 1))
	endfunction

	function s:buildFolder(name, level, path, addPath, cd, addCd, filter, addFilter, functions, folderAttributes)
		let folder = ''

		let cwd = getcwd()

		silent execute 'cd ' . a:path

		for inode in sort(split(glob('*'), "\n"))
			if isdirectory(inode)
				let path = a:path . '/' . inode

				if !has_key(a:folderAttributes, path)
					let cd = ''
					let filter = a:filter
					let functions = {}
				else
					let cd = a:folderAttributes[path]['cd'] == path ? '.' : a:folderAttributes[path]['cd']
					let filter = a:folderAttributes[path]['filter']
					let functions = a:folderAttributes[path]['functions']
				endif

				let files = s:buildFolder(inode, a:level + 1, a:path . '/' . inode, 0, cd, cd != '', filter, filter != '' && filter != a:filter, functions, a:folderAttributes)

				if files != ''
					let folder .= "\n" . files
				endif
			elseif a:filter == '' || match(inode, a:filter) != -1
				let folder .= "\n" . repeat('	', a:level + 1) . inode
			endif
		endfor

		silent execute 'cd ' . cwd

		if folder != ''
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

			if len(a:functions) > 0
				for [index, value] in items(a:functions)
					let name .= ' F' . index . '="' . value . '"'
				endfor
			endif

			let folder = name . folder
		endif

		return folder
	endfunction

	function s:create(line)
		let line = a:line

		while indent(line) > 0
			let line -= 1
		endwhile

		let name = input('Name: ')

		if name == ''
			call s:error('Name must not be empty')
		else
			let path = fnamemodify(input('Path: ', '', 'file'), ':p')

			if getftype(path) != 'dir'
				call s:error('Path ' . path . ' is not a valid directory')
			else
				let cd = fnamemodify(input('Working directory: ', '', 'file'), ':p')

				if cd != '.' && getftype(cd) != 'dir'
					call s:error('Working directory ' . cd . ' is not a valid directory')
				else
					let filter = input('Filter :')

					echomsg 'Create project ' . name . ' from path ' . path . ', please wait...'
					call s:appendFolder(s:buildFolder(name, 0, path, 1, cd, cd != '', filter, filter != '', s:defineMappings({}), {}), line)
					echomsg 'Project ' . name . ' created.'
				endif
			endif
		endif
	endfunction

	function s:refresh(line)
		let line = a:line

		if !s:isFolder(line)
			let line = s:getFolderLine(line)

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
		let cd = addCd ? s:extractCd(line) : ''
		let addFilter = s:hasFilter(line)
		let filter = addFilter ? s:extractFilter(line) : ''
		let functions = s:extractMappingsFromLine(line)
		let folderAttributes = s:getFolderAttributes(line)

		call s:goToLine(line)

		if s:isFolder(line) && foldclosed(line) == -1
			normal! zc
		endif

		silent normal! dd

		if getftype(path) == 'dir'
			echomsg 'Refresh ' . path . ' in project ' . s:getProjectName(line) . ', please wait...'
			call s:appendFolder(s:buildFolder(name, level, path, addPath, cd, addCd, filter, addFilter, functions, folderAttributes), line)
			echomsg path . ' was refreshed.'
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

			if getftype(path) == '' || window == -1
				wincmd p

				execute split ' ' . path
			else
				silent execute window . 'wincmd w'
				execute 'buffer ' . bufnr(path)
			endif

			if cd != ''
				if getftype(cd) != 'dir'
					call s:error("Unable to change directory to " . cd)
				else
					silent execute 'cd ' . cd
					silent execute 'au BufEnter <buffer> cd ' . cd
				endif
			endif

			if g:myprojects_tags_file != '' && rootPath != ''
				let tagsPath = rootPath . '/' . g:myprojects_tags_file

				if getftype(tagsPath) == 'file'
					silent execute 'setlocal tags=' . rootPath . '/' . g:myprojects_tags_file
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
		endif
	endfunction

	function s:grep(line)
		let path = s:getPath(a:line)

		if path != ''
			let pattern = input("Grep in " . path . ": ")

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
	endfunction

	function s:getFiles(path, filter)
		let files = ''

		if isdirectory(a:path)
			let cwd = getcwd()

			silent execute 'cd ' . a:path

			for inode in sort(split(glob('*'), "\n"))
				if isdirectory(inode)
					let files .= ' ' . s:getFiles(a:path . '/' . inode, a:filter)
				elseif a:filter == '' || match(inode, a:filter) != -1
					let files .= ' ' . a:path . '/' . inode
				endif
			endfor

			silent execute 'cd ' . cwd
		endif

		return files
	endfunction

	function s:explore(mode)
		let line = line('.')

		if !s:isFolder(line)
			let line = s:getFolderLine(line)
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
				let tagsPath = rootPath . '/' . g:myprojects_tags_file

				echomsg 'Generate tags file ' . tagsPath . ' for project ' . s:getProjectName(a:line) . ', please wait...'
				execute '!exctags -f ' . tagsPath . ' --sort=yes -R ' . rootPath
				echomsg 'Tags file generated and stored in ' . tagsPath . '.'
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

		while line > 0 && indent(line) > 0 && empty(attribute)
			if s:has{a:name}(line)
				let attribute = [line, s:extract{a:name}(line)]
			else
				let line = s:getFolderLine(line)
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

			let newPath = input('Path: ', currentPath, 'file')

			if newPath != '' && newPath != currentPath
				call s:substitute(pathLine, '\(^\s*[^=]\+=\)\%(\\ \|\f\)\+', '\1' . fnamemodify(newPath, ':p'), '')

				if a:refresh
					call s:refresh(pathLine)
				endif
			endif
		endif
	endfunction

	function s:updateMappings(line)
		let lines = {}
		let currentMappings = {}

		for [line, mappings] in items(s:extractMappings(a:line))
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
	endfunction

	function s:updateAttribute(line, attribute, value)
		if getline(a:line) =~ '\s\+' . a:attribute . '="[^"]\+"'
			if a:value == ''
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\+"', '', '')
			else
				call s:substitute(a:line, '\s\+' . a:attribute . '="[^"]\+"', ' ' . a:attribute . '="' . escape(a:value, '|&') . '"', '')
			endif
		elseif a:value != ''
			call s:substitute(a:line, '\(^.\+$\)', '\1 ' . a:attribute . '="' . a:value . '"', '')
		endif
	endfunction

	function s:updateFilter(line, refresh)
		let filter = s:getNestedFilter(a:line)

		if !empty(filter)
			let [filterLine, currentFilter] = filter
		else
			let filterLine = s:getFolderLine(a:line)

			if filterLine == -1
				let filterLine = a:line
			endif

			let currentFilter = ''
		endif

		let newFilter = input('Filter: ', currentFilter)

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
			let cdLine = s:getFolderLine(a:line)

			if cdLine == -1
				let cdLine = a:line
			endif

			let currentCd = ''
		endif

		let newCd = input('Cd: ', currentCd)

		if newCd != currentCd
			call s:updateAttribute(cdLine, 'cd', newCd)
		endif
	endfunction

	function s:getProjectName(line)
		let line = a:line

		while line > 0 && indent(line) > 0
			let line = s:getFolderLine(line)
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
		let line = line('.')

		call s:goToLine(a:line)

		let hlsearch = &hlsearch

		if hlsearch
			set nohlsearch
		endif

		execute ':s/' . a:search . '/' . escape(a:replace, '/') . '/' . a:flags

		if hlsearch
			set hlsearch
		endif

		call s:goToLine(line)
	endfunction

	function s:getRootLine(line)
		let line = a:line

		while line > 0 && indent(line) > 0
			let line = line - 1
		endwhile

		return line
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
			let line = s:getFolderLine(line)
		endif

		if line > 0
			let folderIndent = indent(line)

			if folderIndent != -1
				let paths[s:getPath(line)] = {'path': s:extractPathFromLine(line), 'cd': s:extractCdFromLine(line), 'filter': s:extractFilterFromLine(line), 'functions': s:extractMappingsFromLine(line)}

				let line += 1

				while indent(line) > folderIndent
					if s:isFolder(line)
						let paths[s:getPath(line)] = {'path': s:extractPathFromLine(line), 'cd': s:extractCdFromLine(line), 'filter': s:extractFilterFromLine(line), 'functions': s:extractMappingsFromLine(line)}
					endif

					let line += 1
				endwhile
			endif
		endif

		return paths
	endfunction

	function s:defineMappings(mappings)
		let mappings = a:mappings

		if len(mappings) <= 0
			let key = 1

			while key <= 12
				let mappings[key] = ''
			endwhile
		endif

		let index = 1

		while index >= 1 && index <= 13
			let list = ['Define mapping for key: ']
			let keys = {}

			let index = 1

			for [key, mapping] in items(mappings)
				let list = add(list, index . '. F' . key . ': ' . mapping)
				let keys[index] = key
				let index += 1
			endfor

			let index = inputlist(list)

			if index >= 1 && index <= len(list)
				let mapping = input('Mapping for F' . keys[index] . ': ')

				if mapping != ''
					let mappings[keys[index]] = mapping
				endif

				redraw
			endif
		endwhile

		return mappings
	endfunction

	function s:error(message)
		echohl WarningMsg | echo a:message | echohl None
	endfunction

	let g:myprojects_enable = 1
endif

finish
