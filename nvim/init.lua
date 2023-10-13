require('emma')

-- Allow require to look in after/plugin folder
local config_dir = os.getenv("XDG_CONFIG_HOME")
package.path = config_dir .. "/nvim/after/plugin/?.lua;" .. package.path
