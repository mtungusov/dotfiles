local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- show line num
vim.o.number = true
vim.o.cursorline = true
vim.o.wrap = false
vim.o.incsearch = true

-- remap leader key
keymap("n", "<Space>", "", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- yank to system clipboard
keymap({ "n", "v" }, "<leader>y", '"+y', opts)

-- paste from system clipboard
keymap({ "n", "v" }, "<leader>p", '"+p', opts)

-- better indent handling
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- move text up and down
keymap("v", "J", ":m .+1<CR>==", opts)
keymap("v", "K", ":m .-2<CR>==", opts)
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)

-- paste preserves primal yanked piece
keymap("v", "p", '"_dP', opts)

-- removes highlighting after escaping vim search
keymap("n", "<Esc>", "<Esc>:noh<CR>", opts)

if vim.g.vscode then
    -- VSCode Neovim
    -- call vscode commands from neovim
    keymap({ "n", "v" }, "<leader>t", "<cmd>lua require('vscode').action('workbench.action.terminal.toggleTerminal')<CR>")
    keymap({ "n", "v" }, "<leader>b", "<cmd>lua require('vscode').action('editor.debug.action.toggleBreakpoint')<CR>")
    keymap({ "n", "v" }, "<leader>d", "<cmd>lua require('vscode').action('editor.action.showHover')<CR>")
    keymap({ "n", "v" }, "<leader>a", "<cmd>lua require('vscode').action('editor.action.quickFix')<CR>")
    keymap({ "n", "v" }, "<leader>sp", "<cmd>lua require('vscode').action('workbench.actions.view.problems')<CR>")
    keymap({ "n", "v" }, "<leader>cn", "<cmd>lua require('vscode').action('notifications.clearAll')<CR>")
    keymap({ "n", "v" }, "<leader>ff", "<cmd>lua require('vscode').action('workbench.action.quickOpen')<CR>")
    keymap({ "n", "v" }, "<leader>cp", "<cmd>lua require('vscode').action('workbench.action.showCommands')<CR>")
    keymap({ "n", "v" }, "<leader>pr", "<cmd>lua require('vscode').action('code-runner.run')<CR>")
    keymap({ "n", "v" }, "<leader>fd", "<cmd>lua require('vscode').action('editor.action.formatDocument')<CR>")
else
    -- Ordinary Neovim
    -- windows
    keymap("n", "<C-n>", ":botright vnew<CR>", opts)
    keymap("n", "<Tab>", ":wincmd w<CR>", opts)
    keymap("n", "<S-Tab>", ":wincmd r<CR>", opts)
    --
    -- Plugins
    --
    -- Bootstrap lazy.nvim
    local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
    if not (vim.uv or vim.loop).fs_stat(lazypath) then
        local lazyrepo = "https://github.com/folke/lazy.nvim.git"
        local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
        if vim.v.shell_error ~= 0 then
            vim.api.nvim_echo({
                      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
                      { out, "WarningMsg" },
                      { "\nPress any key to exit..." },
                }, true, {})
                vim.fn.getchar()
                os.exit(1)
            end
        end
    vim.opt.rtp:prepend(lazypath)
    -- Setup lazy.nvim
    require("lazy").setup({
        {
            "nvim-telescope/telescope.nvim", version = "*",
            dependencies = {
                "nvim-lua/plenary.nvim",
                "nvim-telescope/telescope-file-browser.nvim",
            },
            config = function()
                require("telescope").setup {
                    extensions = {
                        file_browser = {
                            -- disables netrw and use telescope-file-browser in its place
                            hijack_netrw = true,
                        },
                    },
                }
                require("telescope").load_extension("file_browser")

                local builtin = require("telescope.builtin")
                keymap("n", "<leader>fb", ":Telescope file_browser path=%:p:h select_buffer=true<CR>")
                keymap("n", "<leader>ff", builtin.find_files, { desc = "Telescope find files" })
                keymap("n", "<leader>fg", builtin.live_grep, { desc = "Telescope live grep" })
            end
        },
        {
            "tpope/vim-fugitive"
        },
        {
            'nvim-lualine/lualine.nvim',
            dependencies = { 'nvim-tree/nvim-web-devicons' },
            config = function()
                require('lualine').setup({})
            end
        }
    })
end
