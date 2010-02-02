" Quick settings
let g:projectPath = "/data/projects/ensanche-core/"
let g:programName = "ensanche-core"
let makeString = "./build.sh"

" Paths and global variables for custom functions
execute "cd ".g:projectPath."__deploy"
" Load Session before loading custom configuration
source Session.vim
let CTags_CScope_Top_Dir = g:projectPath."__deploy"
let CTags_CScope_Dir_List = g:projectPath.".."
let Make_Dir = g:projectPath."__deploy"
execute "set path+=".g:projectPath."**"

" Make program
execute "set makeprg=".makeString

" Run program
function! RunProgram()
	execute "cd ".g:projectPath."__deploy"
	execute "!./run.sh"
endfunction

" Clean excrements
function! CleanProgram()
	execute "cd ".g:projectPath."__deploy"
	execute "!./clean.sh"
endfunction

" Run Chicken interpreter
function! RunInterpreter()
	execute "cd ".g:projectPath."__deploy"
	execute "!./interpret.sh"
endfunction

" Shortcuts
nnoremap <F3> :vimgrep /<C-R><C-W>/ **<CR>
noremap <F8> :wa<CR>:call RunInterpreter()<CR>
noremap <F9> :call RunProgram()<CR>
noremap <F10> :call CleanProgram()<CR>
noremap <F11> :call Compile(1)<CR>
inoremap <F11> <ESC>:call Compile(1)<CR>
nnoremap <F12> :call BuildCTagsAndCSCopeDatabase("d")<CR>
