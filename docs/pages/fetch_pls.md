---
title: POST /search/jing/fetch_pls
---

获取播放列表

## 路径

    http://jing.fm/api/v1/search/jing/fetch_pls

## 方法

    POST

## 参数

### q

- 含义: 用大白话描述出你想听的音乐

### ps

- 含义: 请求曲目数

### u

- 含义: uid

### tid

- 默认: 0

### mt

- 默认: 空

### ss

- 默认: True

## 返回结果

### 成功

- HTTP Status Code

    `HTTP/1.1 200 OK`

- 返回值 (JSON格式)

    items数组就是曲目列表。

        {
            "msg": "操作成功", 
            "result": {
                "total": 959,
                "items": [{
                    "abid": 103365,
                    "aid": 203294,
                    "an": "Skyline",
                    "atn": "Yann Tiersen",
                    "b": "256",
                    "d": "356",
                    "fid": "2012010506BzI.jpg",
                    "mid": "2012122110MpC.m4a",
                    "n": "Forgive Me",
                    "tid": 1987589,
                    "y": null
                }, 
                ...
                ],
                "normalmode": true,
                "st": 0,
                "ps": 5
            },
            "success": true
        }
