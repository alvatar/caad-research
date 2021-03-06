" Quick settings
let g:projectPath = "/data/projects/ensanche-core/"
let g:programName = "ensanche-core"
let makeString = "./build.sh"

" Paths and global variables for custom functions
execute "cd ".g:projectPath."__scripts"
" Load Session before loading custom configuration
source Session.vim
let CTags_CScope_Top_Dir = g:projectPath."__scripts"
let CTags_CScope_Dir_List = g:projectPath.".."
let Make_Dir = g:projectPath."__scripts"
execute "set path+=".g:projectPath."**"

" Make program
execute "set makeprg=".makeString

" Run program
function! RunProgram()
	:wa
	execute "cd ".g:projectPath."__scripts"
	execute "!./run.sh"
endfunction

" Clean excrements
function! CleanProgram()
	execute "cd ".g:projectPath."__scripts"
	execute "!./clean.sh"
endfunction

" Shortcuts
nnoremap <F3> :vimgrep /<C-R><C-W>/ **<CR>
noremap <F9> :call RunProgram()<CR>
noremap <F10> :call CleanProgram()<CR>
noremap <F11> :call Compile(1)<CR>
inoremap <F11> <ESC>:call Compile(1)<CR>
nnoremap <F12> :call BuildCTagsAndCSCopeDatabase("d")<CR>
