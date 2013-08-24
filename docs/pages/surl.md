---
title: POST /media/song/surl
---

获取歌曲url

## 路径

    http://jing.fm/api/v1/media/song/surl

## 方法

    POST

## 参数

### mid

- 含义: items数组项的mid字段

### type

- 含义: NO表示高音质，MM表示普通音质
- 默认: NO

## 返回结果

### 成功

- HTTP Status Code

    `HTTP/1.1 200 OK`

- 返回值 (JSON格式)

        {
            "msg": "操作成功",
            "result": "http://jingfm.duomi.com/201302271858/d049ae635ac4ce9878d2c2547fdbd8e8/2012/0717/12/Pg/2012071712PgV.m4a",
            "success": true
        }
