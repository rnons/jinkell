## Jing.fm API文档

<http://rnons.github.io/jinkell>

## JingLib: Jing.fm wrapper in Haskell

安装

    cd jinglib
    cabal install

## Jinkell: Jing.fm CLI 客户端

Jinkell使用mplayer的slave模式进行后台播放。请确保你已经安装有mplayer，注意: [mplayer]不等于[mplayer2]。

** 注 **: 以[mpd]为后端的jing.fm客户端已经并入[lord]项目。

### 安装

    cd jinkell
    cabal install
    
名为 **jinkell** 的可执行文件会被安装到 *~/.cabal/bin/* 路径下。


### 功能

- 登录Jing.fm
- “用大白话描述出你想听的音乐”
- 收听/暂停/换歌
- 标记喜欢/讨厌 

### 命令

所有控制命令均以`:`开头，其它字符均作为“你想听音乐的描述”发送到jing.fm

- `:p` 或 `:pause` 暂停/播放
- `:n` 或 `:next` 切歌
- `:love` 标记喜欢
- `:hate` 标记讨厌
- `:save` 保存"用户名/token"到 *~/.jinkell/jinkell.cfg* （！明文！）
- `:help` 帮助信息


[lord]: https://github.com/rnons/lord
[mplayer]: http://www.mplayerhq.hu/
[mplayer2]: http://www.mplayer2.org/
[MPD]: http://musicpd.org/