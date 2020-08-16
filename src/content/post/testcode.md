---
title: "Testcode"
date: 2019-12-01T16:41:29+01:00
draft: false
---

```python
 #!/usr/bin/python

 def FUN_004005dc(param_1):

     buf1 = (param_1 + 0x55 & 0x1f) >> 1
     if (param_1 + 0x55 & 1) != 0:
         buf1 = buf1 | 0x10
     buf2 = buf1 >> 1
     if (buf1 & 1) != 0:
         buf2 = buf2 | 0x10
     buf1 = buf2 >> 1
     if (buf2 & 1) != 0:
         buf1 = buf1 | 0x10
     return buf1 * 3 & 0x1f

 if __name__=="__main__":
     ipos = {}
     for p in range(32):
       ipos[FUN_004005dc(p)] = p
     print(ipos)

     buf = bytearray("89349536319392163324855876422573")
     inp = bytearray("89349536319392163324855876422573")
     for p in range(0x20):
         inp[ipos[p]] = buf[p]
     print(inp)

```
