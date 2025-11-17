require('plugins')

local set = vim.opt
set.number=true
set.relativenumber=true
set.encoding="utf-8"
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
set.wildmode="list:longest,full"
set.completeopt="longest,menuone"
-- Change how we split window
set.splitbelow=true
set.splitright=true
-- Hide underneath status bar
set.showmode=false
-- No mouse for hardcore mode
set.mouse=''
set.cursorline=true
-- To auto read files when they have been modified
set.autoread=true
vim.api.nvim_create_autocmd({"FocusGained","BufEnter","VimResume", "CursorHold"}, {
  pattern = '*',
  command = 'checktime'
})

local bind = vim.keymap
-- Space as leader key
vim.g.mapleader=' '
-- Remaps
bind.set('n', '<S-Tab>', ':tabprevious<CR>')
bind.set('n', '<Tab>', ':tabnext<CR>')

bind.set('n', '<C-J>', '<C-W><C-J>')
bind.set('n', '<C-K>', '<C-W><C-K>')
bind.set('n', '<C-L>', '<C-W><C-L>')
bind.set('n', '<C-H>', '<C-W><C-H>')

bind.set('n', '<C-d>', '<C-d>zz')
bind.set('n', '<C-u>', '<C-u>zz')

bind.set('n', '<leader>q', ':bp<bar>sp<bar>bn<bar>bd<CR>')

bind.set('n', '<leader>o', ':OpenOtherFile<CR>')
bind.set('n', '<leader>O', ':OpenOtherFileInPlace<CR>')

local builtin = require('telescope.builtin')
require("telescope").load_extension "file_browser"
bind.set('n', '<leader>ff', builtin.find_files, {})
bind.set('n', '<leader>fg', builtin.live_grep, {})
bind.set('n', '<leader>fb', builtin.buffers, {})
bind.set('n', '<leader>fh', builtin.help_tags, {})
bind.set('n', '<leader>d', ':Telescope file_browser<CR>')

bind.set('n', '<leader>li', builtin.lsp_implementations, {})
bind.set('n', '<leader>ld', builtin.lsp_definitions, {})
bind.set('n', '<leader>lr', builtin.lsp_references, {})
bind.set('n', '<leader>i', builtin.lsp_document_symbols, {})
bind.set('n', '<leader>ll', builtin.lsp_workspace_symbols, {})

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
    'OpenOtherFileInPlace',
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
        vim.cmd(":e " .. otherFile)
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

-- lsp
local lspconfig = require('lspconfig')

-- Configure clangd
lspconfig.clangd.setup{}

-- Status line
require('lualine').setup{
    options = {
        theme = 'ayu',
        section_separators = '',
        component_separators = '',
    }
}

require("auto-dark-mode").setup {
    fallback = "light"
}

require("catppuccin").setup {
    background = {
        light = "latte",
        dark = "frappe"
    },
    flavour = "auto",
    integrations = {
        telescope = true,
    }
}

vim.cmd.colorscheme("catppuccin")
