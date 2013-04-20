## Jing.fm API文档

<http://rnons.github.com/jinkell>

## JingLib: Jing.fm wrapper in Haskell

### 安装

    cd jinglib
    cabal install

## Jing.fm CLI 客户端

目前有两个版本的CLI客户端，

- Jpd: 使用mpd后台播放 (**推荐**，资源占用小)
- Jinkell: 使用mplayer的slave模式后台播放。(Legacy)

Jpd (Jing.fm playing daemon) 有两种运行方式：

- `jpd keywords &`: 以类daemon方式运行
- `jpdi`: 以Interactive方式运行

Jpd的强大之处在于可以使用mpd的toolchain。比如用[mpc]控制，用[sonata]显示歌词，用[lastfmsubmitd]同步到last.fm。

### 安装

    # install jinkell
    cd jinkell
    cabal install
    # install jpd
    cd jpd
    cabal install

名为 *jinkell* 或 *jpd*/*jpdi* 的可执行文件会被安装到 *~/.cabal/bin/* 路径下。

### 配置

jinkell不需要进行配置即可使用。

jpd使用mpd作为播放器，首先需要配置好mpd, 可以参考[Arch Wiki: Mpd]。<br />
其次，jpd使用[http-conduit]将音乐文件stream到 *~/.jinkell/jinkell.m4a* ，然后mpd才能进行播放。而mpd只支持一个root music directory，因此需要将 *~/.jinkell/* 软链接到mpd的music directory里。

比如mpd的music directory位于 *~/music* ，则需要执行

    ln -s ~/.jinkell ~/music/jinkell

最后，在开启`jpd`/`jpdi`之前，需要先开启`mpd`，才能正常收听。

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


----

[Arch Wiki: Mpd]: https://wiki.archlinux.org/index.php/Mpd
[http-conduit]: http://hackage.haskell.org/package/http-conduit
[mpc]: http://www.musicpd.org/clients/mpc/
[sonata]: http://sonata.berlios.de/
[lastfmsubmitd]: https://www.archlinux.org/packages/community/any/lastfmsubmitd/
