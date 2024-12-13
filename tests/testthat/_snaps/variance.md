# vcovHC works

    Code
      vcovHC(pc)
    Output
                     pbo          trt1          trt2
      pbo   1.106024e-03  4.523445e-07 -9.709004e-06
      trt1  4.523445e-07  1.164889e-03 -7.709031e-07
      trt2 -9.709004e-06 -7.709031e-07  1.170214e-03

---

    Code
      vcovHC(pc)
    Output
                     pbo          trt1          trt2
      pbo   1.106024e-03  4.523445e-07 -9.709004e-06
      trt1  4.523445e-07  1.164889e-03 -7.709031e-07
      trt2 -9.709004e-06 -7.709031e-07  1.170214e-03

# vcovG works

    Code
      vcovG(pc)
    Output
                    pbo         trt1         trt2
      pbo  1.128902e-03 1.856234e-05 1.333885e-05
      trt1 1.856234e-05 1.184599e-03 2.178112e-05
      trt2 1.333885e-05 2.178112e-05 1.157268e-03

---

    Code
      vcovG(pc)
    Output
                    pbo         trt1         trt2
      pbo  1.128902e-03 1.856234e-05 1.333885e-05
      trt1 1.856234e-05 1.184599e-03 2.178112e-05
      trt2 1.333885e-05 2.178112e-05 1.157268e-03

---

    Code
      vcovG(pc)
    Output
                    pbo         trt1         trt2
      pbo  1.128902e-03 1.856234e-05 1.333885e-05
      trt1 1.856234e-05 1.184599e-03 2.178112e-05
      trt2 1.333885e-05 2.178112e-05 1.157268e-03

---

    Code
      vcovG(pc, decompose = FALSE)
    Output
                    pbo         trt1         trt2
      pbo  1.127076e-03 1.856234e-05 1.333885e-05
      trt1 1.856234e-05 1.179430e-03 2.178112e-05
      trt2 1.333885e-05 2.178112e-05 1.164046e-03

---

    Code
      vcovG(pc)
    Output
                    pbo         trt1         trt2
      pbo  1.128902e-03 1.856234e-05 1.333885e-05
      trt1 1.856234e-05 1.184599e-03 2.178112e-05
      trt2 1.333885e-05 2.178112e-05 1.157268e-03

