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
              nodejs
              # Avalonia (gbfs.Desktop) 実行時ネイティブ依存
              fontconfig
              libxkbcommon
              libGL
              wayland
              xorg.libX11
              xorg.libXext
              xorg.libXrandr
              xorg.libXi
              xorg.libXcursor
              xorg.libICE
              xorg.libSM
              xorg.libXrender
              xorg.libXfixes
              xorg.libXdamage
              xorg.libXcomposite
              xorg.libXtst
              openal
            ];

            # libSkiaSharp / Avalonia / OpenAL が探す共有ライブラリの探索パス
            shellHook = ''
              export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath (with pkgs; [ fontconfig libxkbcommon libGL wayland xorg.libX11 xorg.libXext xorg.libXrandr xorg.libXi xorg.libXcursor xorg.libICE xorg.libSM xorg.libXrender xorg.libXfixes xorg.libXdamage xorg.libXcomposite xorg.libXtst openal ])}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
              # nixストア内のdotnet-sdkのパスをDOTNET_ROOTに設定する
              export DOTNET_ROOT="${pkgs.dotnet-sdk_8}/share/dotnet";
              
              # 必要に応じて、PATHにもdotnetが含まれることを確実にする
              # (buildInputsに入っていれば通常は不要だが、念のため)
              export PATH="$DOTNET_ROOT/bin:$PATH"
              
              echo "Environment loaded: DOTNET_ROOT is set to $DOTNET_ROOT"
            '';
          };
        });
    };
}
