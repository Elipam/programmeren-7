
opg3 :: [(Integer, Integer, Integer)]
opg3 = [(x, y, z)|x<-[-100..100],y<-[-100..100],z<-[-100..100],x==2*(y-z),y==x*z,z*2==x+y]