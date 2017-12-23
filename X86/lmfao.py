f = open('test.sml','w')
for i in range(123412+1):
    f.write('val _ = print("Hello world! " ^ Int.toString(' + str(i) + ') ^ "\\n");\n')
f.close()
