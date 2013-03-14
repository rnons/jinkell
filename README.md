## Jing.fm API文档

<http://rnons.github.com/jinkell>

## JingLib: Jing.fm wrapper in Haskell

### 安装

    cd jinglib
    cabal install

## Jinkell: Jing.fm CLI 客户端

**Note**: Jinkell使用mplayer的slave模式后台播放。

- jing.fm的音乐都是mp4(.m4a)格式的，mpg123播不了。
- 最初计划使用mpd，但mpd无法stream远程的mp4文件。(mp4ff: faad2 error: Bitstream value not allowed by specification)。

### 安装

    cd jinkell
    cabal install

### 功能

- 登录Jing.fm
- “用大白话描述出你想听的音乐”
- 收听/暂停/换歌
- 标记喜欢/讨厌 
