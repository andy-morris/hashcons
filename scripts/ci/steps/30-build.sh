cabal --sandbox-config="$sandbox_config" configure --builddir="$build_dir" \
  --enable-tests --enable-benchmarks
cabal --sandbox-config="$sandbox_config" build     --builddir="$build_dir"
cabal --sandbox-config="$sandbox_config" haddock   --builddir="$build_dir"
