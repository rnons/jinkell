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

名为*jinkell*的可执行文件会被安装到*~/.cabal/bin/*下。

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
- `:save` 保存用户名/密码到*$HOME/.jinkell.cfg* （！明文！）
- `:help` 帮助信息
