
                               スライス解析
                             
                            株式会社数理システム

                                        LastChange: Tue Feb 07 13:24:11 2012

* 内容 ------------------------------------------------------------------------

Slice.hs                - スライス解析ライブラリ
Main.hs                 - テスト用メインプログラム
Makefile                - ghc 用 Makefile
prog*.prg               - 解析対象のテストプログラム
prog1.prg               - 参考文献 mizobuchi-03.pdf の小さい例
prog2.prg               - Unix wc と同様のプログラム例

* コンパイルと実行確認 --------------------------------------------------------

ghc がインストールされている環境で

% make
% slice prog1.prg

* 使用法 ----------------------------------------------------------------------

Usage: slice [-dctfDCTFr] program.prg [StmtNo]
  -d : Data dependency
  -D : Data dependency (inverse relatoin)
  -c : Control dependency
  -C : Control dependency (inverse relatoin)
  -t : Dominator tree
  -T : Dominator tree (of reverse flow graph)
  -f : Dominance frontier
  -F : Dominance frontier (of reverse flow graph)
  -r : Reaching definitions
Note:
  Data dependency is computed from reaching definitions.
  Dominance frontier is computed from dominator tree.
  -C and -F should print the same reslut.

* 参考文献 --------------------------------------------------------------------

weiser-82.pdf
        Mark Weiser
        Program slices when debugging
        Communications of the ACM, vol.25, pp.446-452, 1982.
        スライス解析の最初の論文

mizobuchi-03.pdf
        コンパイラ・インフラストラクチャを用いた
                静的プログラムスライシングツール
        溝斑祐司, 中谷俊晴, 佐々政孝
        日本ソフトウェア科学会第 20 回大会, 2003.

Appel 1998
        Modern Compiler Implementation in ML
        コンパイラの教科書

-------------------------------------------------------------------------------
