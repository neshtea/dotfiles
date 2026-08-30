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

-- https://github.com/eraserhd/parinfer-rust#vimpack-neovim--012
vim.api.nvim_create_autocmd('PackChanged', {
  callback = function(ev)
    local name, kind = ev.data.spec.name, ev.data.kind
    if name == 'parinfer-rust' and (kind == 'install' or kind == 'update') then
      vim.system({ 'cargo', 'build', '--release' }, { cwd = ev.data.path })
    end
  end,
})
