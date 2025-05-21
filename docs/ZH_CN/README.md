# ![Edit 图标](../../assets/edit.svg) Edit
简体中文 | [English](../../README.md)

简单的编辑器，给不简单的你。

这款编辑器致敬经典的 [MS-DOS Editor](https://en.wikipedia.org/wiki/MS-DOS_Editor), 但具有类似于 VS Code 的现代界面和输入控件。目标是提供一个可访问的编辑器，即使是不熟悉终端的用户也可以轻松使用。

![在前台显示 About 对话框时进行编辑的屏幕截图](../../assets/edit_hero_image.png)

## 安装说明

* 从我们的 [releases page](https://github.com/microsoft/edit/releases/latest) 中下载最新版本
* 解压压缩包
* 复制 `edit` 目录到一个存在于 `PATH` 中的文件夹（在`设置-系统-关于-高级系统设置-环境变量`中）
* 您可以删除存档中的任何其他不需要的文件

## 从源代码构建

* [安装Rust](https://www.rust-lang.org/tools/install)
* 安装nightly工具包: `rustup install nightly`
  * 或者，将环境变量设为 `RUSTC_BOOTSTRAP=1`
* 克隆此仓库
* 运行以下指令来构建 Release 包: `cargo build --config .cargo/release.toml --release`

## 其他文档
[Microsoft 开源行为准则](./CODE_OF_CONDUCT.md)
[贡献](./CONTRIBUTING.md)
[漏洞报告与安全性](./SECURITY.md)
