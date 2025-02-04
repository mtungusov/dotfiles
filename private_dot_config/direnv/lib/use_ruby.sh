# Usage with MISE: use ruby
#
# Loads the mise specified ruby version from .mise.toml into the environment
#
use_ruby() {
  local ruby_dir=$(mise where ruby)
  load_prefix $ruby_dir
  layout ruby
}
