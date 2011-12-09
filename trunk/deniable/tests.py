def testRandS():
    secret = trapdoor.loadtrapdoor()
    s = encrypt.makeS(secret)
    print(isS(s, secret))

    for i in range(1000 * 1000):
        r = encrypt.makeR()
        if isS(r, secret):
            print("oh snap collision!!!")

def testCountS():
    for i in range(1000):
        enc = encrypt.encode(1, secret)
        assert(countSs(enc, secret) % 2 == 1)

        enc = encrypt.encode(0, secret)
        assert(countSs(enc, secret) % 2 == 0)


