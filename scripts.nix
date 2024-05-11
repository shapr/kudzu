{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:kudzu' --allow-eval --warnings";
  testScript = s "test" "cabal run test:kudzu-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
