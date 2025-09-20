# SCREAMER-PLUS

#### Based on the original library by S. White

---

### Overview

**Screamer-Plus** is a modernized constraint logic programming library for **Common Lisp**.

It extends constraint propagation in Screamer, built upon a fundamental redesign of the core functions `funcallv` and `applyv` introduced in version 4.0.1 of Screamer.

This new foundation enables **automatic constraint propagation**, eliminating the need for manual noticers and simplifying function/macro definitions.

As a result, many of the macros and functions originally found in Screamer-Plus (by Simon White) — such as `CARV`, `CDRV`, `IFV`, and others — have been entirely rewritten or reimagined with cleaner semantics and greater efficiency.

> Some function names and general ideas are inspired by the original Screamer-Plus by Simon White, but all code in this package is original unless otherwise noted.

Contributions, feedback, and extensions are welcome.

---

### License

**Copyright (c) 2025 Paulo Henrique Raposo**

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to:

- use  
- copy  
- modify  
- merge  
- publish  
- distribute  
- sublicense  
- and/or sell copies of the Software  

and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

> The above copyright and authorship notice and this permission notice shall be included in all copies or substantial portions of the Software.

**Disclaimer:**  
The Software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of:

- merchantability  
- fitness for a particular purpose  
- and noninfringement  

In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the Software or the use or other dealings in the Software.