{
  description = "My Work Toolchains";

  # 1. 依存関係の定義（どのバージョンのパッケージ集を使うか）
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  # 2. このflakeが、何を提供するかの定義
  outputs = { self, nixpkgs }:
    let
      # サポートするシステム（PCのアーキテクチャ）を指定
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      # 各システムごとに、設定を生成するための関数
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      # nixpkgsを、各システムに合わせて取り出す
      pkgsFor = system: import nixpkgs { inherit system; };

    in {
      nixpkgs.config.allowUnfree = true;
      # 3. 開発環境（devShell）の定義
      devShells = forAllSystems (system:
        let pkgs = pkgsFor system;
        in {
          default = pkgs.mkShell {
            # この環境で使えるようにするツールを、ここに列挙する
            buildInputs = with pkgs; [
              dotnet-sdk_8
              # ここに、必要なツール（例: go, rustc, etc...）を、どんどん追加していく
            ];
          };
        });
    };
}
