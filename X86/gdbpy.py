import gdb
def test():
    gdb.execute('tar sim -leon3')
    gdb.execute('load')
    gdb.execute('run')
    gdb.execute('quit')
if __name__ == '__main__':
    test()
    
