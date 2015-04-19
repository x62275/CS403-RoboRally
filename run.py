import sys, os
'''
AUTHOR:
Tim Nosco 09FEB2015
scalatest must be installed to use
USAGE:
python run.py <ClassesToTest> <TestClass>
EXAMPLE:
python run.py GameObj.scala GameTest.scala
'''

if len(sys.argv)==1:
	raw0 = "GameObj.scala"
	raw = "GameTest.scala"
else:
	raw0 = sys.argv[1]
	raw = sys.argv[2]

name = raw0[:-6]
jar0 = name+".jar"
name = raw[:-6]
jar = name+".jar"

s0= "scalac -d bin/"+jar0+" "+raw0
s1= "scalac -classpath bin/"+jar0+" -d bin/"+jar+" "+raw
s2 = "scala -classpath bin/"+jar0+" org.scalatest.tools.Runner -R bin/"+jar
os.system("mkdir bin")
print s0
os.system(s0)
print s1
os.system(s1)
print s2
os.system(s2)