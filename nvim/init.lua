local set = vim.opt

set.number=true
set.relativenumber=true
set.autoindent=true
set.smartindent=true
set.encoding="utf-8"
set.bg="light"
set.hlsearch=true
set.colorcolumn="120"
set.tabstop=4
set.shiftwidth=4
set.expandtab=true
set.cindent=true
set.incsearch=true
set.wildmenu=true
set.wildmode="longest:list,full"
set.completeopt="longest,menuone"
set.splitbelow=true
set.splitright=true
set.showmode=false
set.mouse=''

local bind = vim.keymap
-- Space as leader key
vim.g.mapleader=' '
-- Remaps
bind.set('n', '<S-Tab>', ':tabprevious<CR>')
bind.set('n', '<Tab>', ':tabnext<CR>')

bind.set({'n','v'}, 'j', 'gj')
bind.set({'n','v'}, 'k', 'gk')

bind.set('n', '<leader>l', ':ls<CR>')
bind.set('n', '<leader>f', ':Vexplore<CR>')

bind.set('n', '<C-J>', '<C-W><C-J>')
bind.set('n', '<C-K>', '<C-W><C-K>')
bind.set('n', '<C-L>', '<C-W><C-L>')
bind.set('n', '<C-H>', '<C-W><C-H>')

-- Netrw options
vim.g.netrw_banner=0
vim.g.netrw_browse_split=4
vim.g.netrw_altv=1
vim.g.netrw_liststyle=3
vim.g.netrw_winsize=15

-- Personal commands
vim.api.nvim_create_user_command(
  'Debugger',
  function(input)
    vim.cmd('packadd termdebug')
    vim.cmd('Termdebug()')
  end,
  {}
)

-- Set configs for individual filetypes
vim.api.nvim_create_autocmd('filetype', {
    pattern = '*.py',
    desc = 'Tabs for python',
    callback = function ()
        vim.opt.tabstop=2
        vim.opt.softtabstop=2
        vim.opt.shiftwidth=2
        vim.opt.textwidth=120
        vim.opt.fileformat='unix'
    end
})


local function status_line()
  local mode = "%2{%v:lua.string.upper(v:lua.vim.fn.mode())%}"
  local file_name = " %.16t"
  local buf_nr = " [%n]"
  local modified = " %-m"
  local file_type = " %y"
  local right_align = "%="
  local line_no = " %5([%c:%l/%L%)]"
  local encoding = "%{&ff}"

  return string.format(
    "%s%s%s%s%s%s%s%s",
    mode,
    file_name,
    buf_nr,
    modified,
    right_align,
    encoding,
    file_type,
    line_no
  )
end

vim.opt.statusline = status_line()

local function netrw_mapping()
  local bufmap = function(lhs, rhs)
    local opts = {buffer = true, remap = true}
    vim.keymap.set('n', lhs, rhs, opts)
  end

  -- close window
  bufmap('<leader>f', ':q<CR>')

  -- Better navigation
  bufmap('l', ':q<CR>')

  -- Toggle dotfiles
  bufmap('.', 'gh')
end

local user_cmds = vim.api.nvim_create_augroup('user_cmds', {clear = true})
vim.api.nvim_create_autocmd('filetype', {
  pattern = 'netrw',
  group = user_cmds,
  desc = 'Keybindings for netrw',
  callback = netrw_mapping
})
