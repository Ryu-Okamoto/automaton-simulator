# オートマトンシミュレータ
有限オートマトンのシミュレータです。  
決定性 (DFA)、非決定性 (NFA) に対応しています。
定義の与え方および受理する語の取得方法は ***Example.hs を参考にしてください。

## 実行方法
[ghci](https://www.haskell.org/ghc/)、[cabal](https://www.haskell.org/cabal/) のインストールが必要です。  
init を叩くと必要な外部パッケージがインストールされます。  
テストを含め実行は ghci インタプリタから行います。

例）NFAExample の受理する言語を確認する
~~~
$ ghci
ghci > :l Src.Src.NFAExample
ghci > acceptedWords
fromList ["aaaab","aaab","aab","aabab","ab","abaab","abab","abbab","baaab","baab","bab","babab","bbaab","bbab","bbbab"]
~~~