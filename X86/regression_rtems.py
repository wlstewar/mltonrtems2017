from subprocess import PIPE, run
import os
import sys
def test():
    os.environ["PATH"] = "/home/oem/pickle/rtems/4.11/bin:" + os.environ["PATH"]
    os.chdir('regression/sparc')
    for prog in os.listdir():
        prog = 'int'
        command = 'sparc-rtems4.11-gdb ' + prog + ' -x ../../gdbpy.py'
        x = run(command, stdout=PIPE, stderr=PIPE, env=os.environ, shell=True, universal_newlines=True)
        res = x.stderr.split('\n')
        print(x.stderr)
        f = open(prog+'.ok','w')
        for line in res:
            f.write(line + '\n')
            '''if line.startswith('Starting program:'):
                lines = x[(x.index(line)):(x.index("[Inferior 1 (process 42000) exited normally]"))]
                break'''
        '''for line in lines:
            print(line)'''
        f.close()
        break
if __name__ == '__main__':
    test()
else:
    print('ERROR?')
