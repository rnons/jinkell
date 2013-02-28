---
title: POST /music/post_hate_song
---

标记讨厌

## 路径

    http://jing.fm/api/v1/music/post_hate_song

## 方法
    
    POST

## 参数

### uid

### tid

### c

### cmbt

## 返回结果

### 成功

- HTTP Status Code

    `HTTP/1.1 200 OK`

- 返回值 (JSON格式)

        {
            "msg": "操作成功",
            "result": {
                total: 0,
                items: [],
                st: 0,
                ps: 0
            },
            "success":true
        }
