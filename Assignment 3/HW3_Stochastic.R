c=c(-1,4)
A=matrix(c(-10,5,1,20,10,0),3,2)
b=c(22,49,5)
dir=c("<=")
sol=lp("max",c,A,dir,b)
sol$objval
sol$solution

c=c(-1,4)
A=matrix(c(-10,5,1,1,20,10,0,0),4,2)
b=c(22,49,5,3)
dir=c("<=")
sol=lp("max",c,A,dir,b)
sol$objval
sol$solution

c=c(-1,4)
A=matrix(c(-10,5,1,1,20,10,0,0),4,2)
b=c(22,49,5,4)
dir=c("<=","<=","<=",">=")
sol=lp("max",c,A,dir,b)
sol$objval
sol$solution

c=c(-1,4)
A=matrix(c(-10,5,1,1,0,20,10,0,0,1),5,2)
b=c(22,49,5,4,3)
dir=c("<=","<=","<=",">=",">=")
sol=lp("max",c,A,dir,b)
sol$objval
sol$solution
sol$status


c=c(-1,4)
A=matrix(c(-10,5,1,1,0,1,20,10,0,0,1,0),6,2)
b=c(22,49,5,3,2,2)
dir=c("<=","<=","<=","<=","<=",">=")
sol=lp("max",c,A,dir,b)
sol$objval
sol$solution


#Question 2
c=c(9,5,6,4)
A=matrix(c(6,1,0,3,1,0,5,0,1,2,0,1), 3, 4)
A=rbind(A, diag(4))
dir=c("<=", ">=", rep("<=", 5))
b=c(11, rep(1, 6))
s=lp("max", c, A, dir, b)
s$status
s$solution
s$objval

#Question 3
c=rep(1,12)
atl=c(1,0,1,0,1,0,1,1,1,0,0,0)
bos=c(0,1,0,0,0,0,0,1,1,0,0,0)
chi=c(1,0,1,0,0,0,1,1,1,0,0,0)
den=c(0,0,0,1,0,0,0,0,0,1,0,0)
hou=c(1,0,0,0,1,0,1,0,0,0,0,0)
lax=c(0,0,0,0,0,1,0,0,0,1,1,0)
no=c(1,0,1,0,1,0,1,0,0,0,0,0)
ny=c(1,1,1,0,0,0,0,1,1,0,0,0)
pit=c(1,1,1,0,0,0,0,1,1,0,0,0) 
slc=c(0,0,0,1,0,1,0,0,0,1,1,1)
sf=c(0,0,0,0,0,1,0,0,0,1,1,1)
sea=c(0,0,0,0,0,0,0,0,0,1,1,1)
A=matrix(c(atl,bos,chi,den,hou,lax,no,ny,pit,slc,sf,sea),12,12)
dir=rep(">=", 12)
b=rep(1, 12)
sol=lp("min", c, A, dir, b)
sol$status
sol$solution
sol$objval

#Question 4
c=c(95,83,70,66,58,46,45,41,33,29,21,20,16,12,9,8,4)
A=matrix(c(1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 1, 1, 1, 0, 0, 2, 0, 3, 0, 0, 1, 0, 1, 2, 1, 0, 0, 1, 1, 1, 2, 0, 4, 0, 0, 2, 0, 1, 0, 0, 2, 0, 3, 0, 3, 1, 0, 1, 1, 1), 3, 17)
dir=rep("=", 3)
b=c(233, 148, 106)
sol=lp("min", c, A, dir, b, all.int=TRUE)
sol$status
sol$solution
sol$objval

#Question 5
c=c(330,300,330,360,360,360,360)
A=matrix(c(rep(1,5),rep(0,3),rep(1,5),rep(0,3),rep(1,6),0,0,rep(1,6),0,0,rep(1,6),0,0,rep(1,6),0,0,1),7,7)
dir=rep(">=",7)
b=c(5,13,12,10,14,8,6)
s=lp("min", c, A, dir, b, all.int=TRUE)
s$status
s$solution
s$objval
