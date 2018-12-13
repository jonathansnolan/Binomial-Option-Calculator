#####Teams Members: #################
############## Jonathan Nolan #######


FI6081BinOptionTree&lt;-function(S,K,T,r,N,sigma,CP) {
#S = Starting Share price
#K = Strike price
#T = Time to maturity (Annualised)
#r = Risk free interest rate as per the USD MMkt rate in Bloomberg OVL
#N = Time steps
#sigma = Volatility - Bloomberg Terminal OVL IMPLIED VOLATILITY
#NOTE CP = 0 for Call, 1 for put
dt &lt;- T/N # delta t
v &lt;- r - .5*(sigma^2) # mu = (r/sigma^2)-.5 --&gt; v = mu*sigma^2
x &lt;- (sigma^2*dt)+(v^2*dt^2)
dx &lt;- sqrt(x) # jump nominal value
pu &lt;- .5 + .5*((v*dt)/dx) # probability up
pd &lt;- 1-pu # probability down
dxu &lt;- dx # This is the jump up
dxd &lt;- -dx # This is the jump down
disc &lt;- (exp(-r*dt))# discounting factor

SP &lt;- matrix(nrow=N+1, ncol=N+1) # stock price matrix
for(i in nrow(SP):1){
for(j in 1:ncol(SP)){
SP[i,j] &lt;- S*exp(dxu*(nrow(SP)-i)+dxd*(j-1))
}
SP[upper.tri(SP)]=0
}
SP
opt&lt;-matrix(nrow=N+1, ncol=N+1) # option price matrix
for(j in nrow(opt):1){
for(i in 1:ncol(opt)){
if (i == j){ # set values at maturity
if (CP == 0){ # for call
opt[i,j] &lt;- max(SP[i,j]-K,0)
}
else if (CP == 1){ # for put
opt[i,j] &lt;- max(K-SP[i,j],0)
}
}

else if(j &lt; (N+1) &amp; i &gt; 1){
if(CP == 0){
opt[i,j] &lt;- max(SP[i,j]-K,disc*(pu*opt[i-1,j]+pd*opt[i,j+1])) # American call
option prices
}
else if (CP == 1){
opt[i,j] &lt;- max(K-SP[i,j],disc*(pu*opt[i-1,j]+pd*opt[i,j+1])) # American put
option prices
}
}
}
opt[upper.tri(opt)]=0
}
opt[N+1,1] # displays options price
}
#NOTE CP = 0 for Call, 1 for put
OptionPrice&lt;-
FI6081BinOptionTree(S=6.085,K=12,T=819/365,r=.00741,N=100,sigma=0.6073
2,CP=0);
OptionPrice
