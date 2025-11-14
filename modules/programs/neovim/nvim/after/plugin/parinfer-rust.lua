-- after/plugin/parinfer-rust.lua

---Returns a string that represents the directory in which the pack with
---`pack_name` is installed.
---@param pack_name string|nil If nil, returns the 'base' path.
---@return string|nil
local function get_pack_dir_of(pack_name)
    local base_path = vim.fn.stdpath('data') .. '/site/pack/core/opt'
    local full_path
    if pack_name == nil then
        full_path = base_path
    else
        full_path = base_path .. '/' .. pack_name
    end
    if io.open(full_path, 'r') then
        return full_path
    end
end

-- Used to just be a `build = ...` directive with lazy.nvim. Now, we need to
-- put in a little more work but oh well... still worth it though.

---Initialize parinfer rust. Requires compiling some rust code. Cargo comes
---from the nix environment. Runs on every startup, but completes almost
---instantaneously, so nevermind about calling cargo every time.
local function init_parinfer_rust()
    local cwd = vim.fn.getcwd()
    vim.cmd('cd ' .. get_pack_dir_of('parinfer-rust'))
    local output = vim.fn.system('cargo build --release')
    local ok = vim.v.shell_error == 0
    local err = not ok and output or nil
    if err then
        error(err)
    end
    vim.cmd('cd ' .. cwd)
end

-- init_parinfer_rust()
