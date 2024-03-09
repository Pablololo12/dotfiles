require('plugins')

local set = vim.opt
set.number=true
set.relativenumber=true
set.encoding="utf-8"
--set.bg="dark"
set.colorcolumn="120"
-- Search settings
set.hlsearch=true
set.incsearch=true
-- tabs and identation
set.autoindent=true
set.smartindent=true
set.tabstop=4
set.shiftwidth=4
set.expandtab=true
set.list=true
-- Settings for completion
set.wildmenu=true
--set.wildoptions="fuzzy"
set.wildmode="list:longest,full"
set.completeopt="longest,menuone"
-- Change how we split window
set.splitbelow=true
set.splitright=true
-- Hide underneath status bar
set.showmode=false
-- No mouse for hardcore mode
set.mouse=''
-- To auto complete paths
set.completeslash="/"
-- To auto read files when they have been modified
set.autoread=true

local bind = vim.keymap
-- Space as leader key
vim.g.mapleader=' '
-- Remaps
bind.set('n', '<S-Tab>', ':tabprevious<CR>')
bind.set('n', '<Tab>', ':tabnext<CR>')

bind.set({'n','v'}, 'j', 'gj')
bind.set({'n','v'}, 'k', 'gk')

bind.set('n', '<C-J>', '<C-W><C-J>')
bind.set('n', '<C-K>', '<C-W><C-K>')
bind.set('n', '<C-L>', '<C-W><C-L>')
bind.set('n', '<C-H>', '<C-W><C-H>')

bind.set('n', '<C-d>', '<C-d>zz')
bind.set('n', '<C-u>', '<C-u>zz')

bind.set('n', '<leader>o', ':OpenOtherFile<CR>')

local builtin = require('telescope.builtin')
require("telescope").load_extension "file_browser"
bind.set('n', '<leader>ff', builtin.find_files, {})
bind.set('n', '<leader>fg', builtin.live_grep, {})
bind.set('n', '<leader>fb', builtin.buffers, {})
bind.set('n', '<leader>fh', builtin.help_tags, {})
bind.set('n', '<leader>n', ':Telescope file_browser<CR>')

-- Personal commands
vim.api.nvim_create_user_command(
  'Debugger',
  function(input)
    vim.cmd('packadd termdebug')
    vim.cmd('Termdebug()')
  end,
  {}
)

vim.api.nvim_create_user_command(
    'OpenOtherFile',
    function()
        local current_file = vim.api.nvim_eval('expand("%:p")')
        local path, filename, extension = string.match(current_file, "(.-)([^\\/]-)([^\\/%.]+)$")
        otherFile = path .. filename
        if extension == "cpp" then
            otherFile = otherFile .. "h"
        elseif extension == "h" then
            otherFile = otherFile .. "cpp"
        else
            return
        end
        vim.cmd(":vs " .. otherFile)
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


-- Status line
require('lualine').setup{
    options = {
        theme = 'modus-vivendi',
    }
}

require("catppuccin").setup {
    background = {
        light = "latte",
        dark = "frappe"
    }
}

vim.cmd.colorscheme "catppuccin-latte"
vim.o.background = "light"
