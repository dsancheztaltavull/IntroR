#if
k <- 0
if(3>2){
  k <- 1
}
k



k <- 0
if(3>5){
  k <- 1
}
k


#else

k <- 0
if(3>2){
  k <- 1
}else{
  k <- 2
}
k




k <- 0
if(3>5){
  k <- 1
}else{
  k <- 2
}
k

#Try to practice with other ifs




#Sum of first 500 numbers
sum(1:500)

#But how did R do that? How do we know that's correct?
#Let's answer first the second question

1+2+3+4+5+6+7+8+9+10+11+12+13+14+15+16+17+18+19+20+21+22+23+24+25+26+27+28+29+30+31+32+33+34+35+36+37+38+39+40+41+42+43+44+45+46+47+48+49+50+51+52+53+54+55+56+57+58+59+60+61+62+63+64+65+66+67+68+69+70+71+72+73+74+75+76+77+78+79+80+81+82+83+84+85+86+87+88+89+90+91+92+93+94+95+96+97+98+99+100+
  101+102+103+104+105+106+107+108+109+110+111+112+113+114+115+116+117+118+119+120+121+122+123+124+125+126+127+128+129+130+131+132+133+134+135+136+137+138+139+140+141+142+143+144+145+146+147+148+149+150+151+152+153+154+155+156+157+158+159+160+161+162+163+164+165+166+167+168+169+170+171+172+173+174+175+176+177+178+179+180+181+182+183+184+185+186+187+188+189+190+191+192+193+194+195+196+197+198+199+200+
  201+202+203+204+205+206+207+208+209+210+211+212+213+214+215+216+217+218+219+220+221+222+223+224+225+226+227+228+229+230+231+232+233+234+235+236+237+238+239+240+241+242+243+244+245+246+247+248+249+250+251+252+253+254+255+256+257+258+259+260+261+262+263+264+265+266+267+268+269+270+271+272+273+274+275+276+277+278+279+280+281+282+283+284+285+286+287+288+289+290+291+292+293+294+295+296+297+298+299+300+
  301+302+303+304+305+306+307+308+309+310+311+312+313+314+315+316+317+318+319+320+321+322+323+324+325+326+327+328+329+330+331+332+333+334+335+336+337+338+339+340+341+342+343+344+345+346+347+348+349+350+351+352+353+354+355+356+357+358+359+360+361+362+363+364+365+366+367+368+369+370+371+372+373+374+375+376+377+378+379+380+381+382+383+384+385+386+387+388+389+390+391+392+393+394+395+396+397+398+399+400+
  401+402+403+404+405+406+407+408+409+410+411+412+413+414+415+416+417+418+419+420+421+422+423+424+425+426+427+428+429+430+431+432+433+434+435+436+437+438+439+440+441+442+443+444+445+446+447+448+449+450+451+452+453+454+455+456+457+458+459+460+461+462+463+464+465+466+467+468+469+470+471+472+473+474+475+476+477+478+479+480+481+482+483+484+485+486+487+488+489+490+491+492+493+494+495+496+497+498+499+500

#Did Daniel lose 20 minutes of his life to write that?

#The answer to all those questions is using loops

#A loop is a task that is repeated while a certain condition is satisfied.
#Let's sum the first 100 numbers
total <- 0
i <- 1
while(i <= 100){
  total <- total + i
  i <- i+1
}

total


total <- 0
for(i in 1:100){
  total <- total+i
}
total

#Equivalently we could use the formula to sum the first 100 numbers
100*101/2


#Let's use some loops to solve other operations

#Sum of all even numbers below 100
i <- 2
total <- 0
while(i <=100){
  total <- total + i
  i <- i+2
}
total


#Sum of all odd numbers below 100
i <- 1
total <- 0
while(i <=100){
  total <- total + i
  i <- i+2
}
total

#Product of the odd numbers below 10


i <- 1
total <- 1
while(i <=10){
  total <- total * i
  i <- i+2
}
total

#In this case it would have been faster to just write them
1*3*5*7*9



#We could also use loops to write long words

c <- "1"
for(i in 2:500){
  c <- paste(c, paste("+",i, sep=""), sep="")
}
c




#Let's solve some problems with it


#Solve the system of equations from the other day

for(rook in 1:23){
  for(queen in 1:23){
    for(knight in 1:23){
      for(bishop in 1:23){
        if(2*queen+rook==23){
          if(queen+2*bishop==15){
            if(3*knight==9){
              if(knight+bishop+queen==15){
                solution <- knight+knight+rook
              }
            }
          }
        }
      }
    }
  }
}
solution



#Find all the prime numbers between 1 and 1000

isprime <- rep(TRUE, 1000)
isprime

vec <- 1:1000
vec[isprime]


7%%2

7%%2==0
8%%2==0



for(i in 3:1000){
  for(j in 2:(i-1)){
    if(i%%j==0){
      isprime[i] <- FALSE
    }
  }
}


vec[isprime]








#random numbers

#We will not got into the details on how R can generate random numbers
set.seed(1111)

#Pick 5 random numbers between 1 and 100
sample(1:100,5)

#Do it again
sample(1:100,5)

#Set the seed back to 1111 and do it again
set.seed(1111)
sample(1:100,5)

#Actually those are not random numbers, they are uniquely determined given a seed. 
#In any case, they look like random numbers, and they are the closest thing to random numbers that we can get. 

#Set another seed
set.seed(1234)
sample(1:100,5)


#

random1 <- runif(1)
random1

random2 <- runif(14)
random2

runif(106)

hist(runif(10000))


hist(runif(100000), breaks=100)


#Flip a coin

coin <- "head"
if(runif(1)<0.5){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.5){
  coin <- "tail"
}
coin


coin <- "head"
if(runif(1)<0.5){
  coin <- "tail"
}
coin


coin <- "head"
if(runif(1)<0.5){
  coin <- "tail"
}
coin


#Let's give it a better chance to get tail

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin


coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

#Do it 10 times

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin

coin <- "head"
if(runif(1)<0.9){
  coin <- "tail"
}
coin




#Montecarlo methods

#What's the minimum of the function x^2-4x+1 between 0 and 5?

#We don't have to, but let's visualize it

x <- seq(0,5,0.1)
plot(x, x^2-4*x+1)

#Now let's find the minimum, by trying random numbers until we find it. First we will guess that the minimum is at 5
x <- 5
minimum <- x^2-4*x+1

minimum

#Then we generate a random number between 0 and 5
x <- 5*runif(1)
x
#evaluate them
eval <- x^2-4*x+1
eval

if(eval<minimum){
  minimum=eval
  minat <- x
}

minimum
minat

for(i in 1:1000){
  x <- 5*runif(1)
  eval <- x^2-4*x+1
    if(eval<minimum){
      minimum=eval
      minat <- x
    }
}

minimum
minat



#Approximate PI

#Generate a random number between 0 and 1
x <- runif(1)
#Move it to between 0 and 2
x <- x*2
#Move it to between -1 and 1
x <- x-1

#Another one, in one line
y <- (2*runif(1))-1

#Check if it is inside the circle
x^2+y^2<=1


#With a loop

total <- 0
for(i in 1:100){
  x <- (2*runif(1))-1
  y <- (2*runif(1))-1
  
  if( (x^2+y^2) <= 1){
    total <- total+1
  }
  
}
4*total/i



#With a longer loop

total <- 0
for(i in 1:1000000){
  x <- (2*runif(1))-1
  y <- (2*runif(1))-1
  
  if( (x^2+y^2) <= 1){
    total <- total+1
  }
  
}
4*total/i


#Feel free to increase the number of simulations




#birthday paradox
#What's the probability of having (at least) two persons with the same birthday in a room with 30 persons?
#Generate 30 random numbers (with repetition) between 1 and 365
bdays <- sample(1:365, 30, replace = T)
bdays 
#Check if some of them appear there twice ()
duplicated(bdays)


sum(duplicated(sample(1:365, 30, replace = T)))>0

total <- 0
for(i in 1:10000){
    a <- sum(duplicated(sample(1:365, 30, replace = T)))
    if(a>0){
      total <- total+1
    }

    
}

total/i

#In a room of 1, 2, 3, 4, 5,..., 80?


vec <- rep(0, 80)

for(j in 1:80){
  total <- 0
  for(i in 1:10000){
    a <- sum(duplicated(sample(1:365, j, replace = T)))
    if(a>0){
      total <- total+1
    }
    
    
  }
  
  vec[j] <- 100*total/i
  
  
  
  
}


plot(1:80, vec, "l", xlab="Number of persons", ylab="Probability (%)")




