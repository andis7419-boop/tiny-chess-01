import json,os,urllib.request as R
U=os.getenv("MAKEFUN_ENV_URL")
C=(0,1,2,3,3,2,1,0)
T=[[0,100,320,330,500,900,0][q>>7]+5*(C[q&7]+C[q>>4&7])for q in range(896)]
NK=(-33,-31,-18,-14,14,18,31,33)
DG=(-16,16,-1,1,-15,-17,15,17)
NJ=NK+DG+DG[4:6]
DT=(0,0,NK,DG[4:],DG[:4],DG,DG+(-2,2))
b=[0]*128
side=1;castle=ep=0
u=[];J=[4,116]
N=0;B=4e4;AB=0;TT={};KL=[0]*999
def setup(f):
 global b,side,castle,ep
 b=[0]*128;q=f.split();s=112
 for ch in q[0]:
  if ch<'A':s+=int(ch)if ch>'/'else-24
  else:
   v=' PNBRQK'.index(ch.upper())
   if ch.islower():v=-v
   b[s]=v;s+=1
   if abs(v)>5:J[v<0]=s-1
 side=(q[1]>'v')*2-1
 castle=sum((c in q[2])<<i for i,c in enumerate('KQkq'))
 ep=-1 if q[3]=='-'else ord(q[3][0])-97|int(q[3][1])-1<<4
 u.clear()
def attacked(s,w):
 for i,d in enumerate(NJ):
  t=s+d*w
  if not t&0x88 and b[t]*w==(2,6,1)[i//8]:return 1
 for i,d in enumerate(DG):
  t=s+d
  while not t&0x88 and not b[t]:t+=d
  if not t&0x88 and b[t]*w in(5,4-(i>3)):return 1
 return 0
def ad(l,s,t):
 l+=[s|t<<7|r<<14 for r in(5,4,3,2)]if t>>4==7-7*(side<0)else[s|t<<7]
def gen(c=0):
 ms=[]
 for s,p in enumerate(b):
  if p*side<1:continue
  a=abs(p)
  if a==1:
   d=16*side;f=s+d
   if not b[f]:
    ad(ms,s,f)
    if s>>4==(1,6)[side<0] and not b[f+d]:ad(ms,s,f+d)
   for t in(s+d-1,s+d+1):
    if not t&0x88 and (b[t]*side<0 or t==ep):ad(ms,s,t)
  else:
   for d in DT[a]:
    t=s+d
    while not t&0x88 and b[t]*side<1:
     st=(d>0)*2-1
     if a-6 or abs(d)-2 or castle>>(d<0)+2*(side<0)&1 and not(b[t]|b[s+st]|(d<0)*b[s+3*st])and not any(attacked(x,-side)for x in(s,s+st,t)):ms.append(s|t<<7)
     if b[t]or a%4==2:break
     t+=d
 if c:ms=[m for m in ms if b[t:=m>>7&127] or m>>14 or t==ep]
 return[m for m in ms if(make(m),attacked(J[side>0],side),unmake())[1]==0]
legal=gen
def make(m):
 global side,castle,ep
 f=m&127;t=m>>7&127;r=m>>14
 p=b[f]
 u.append((b[:],side,castle,ep,J[:]))
 if p==side and t==ep and not b[t]:b[t-16*side]=0
 b[f]=0;b[t]=r*side or p
 if p==6*side:
  J[p<0]=t;castle&=~(3,12)[p<0]
  if abs(t-f)==2:
   g,k=(t+1,t-1)if t>f else(t-2,t+1);b[k],b[g]=b[g],0
 for x in(f,t):
  if x in(0,7,112,119):castle&=~(1<<(x>>5&2|~x&1))
 ep=f+16*side if p==side and t==f+32*side else-1
 side=-side
def unmake():
 global b,side,castle,ep,J
 b,side,castle,ep,J=u.pop()
def uci(m):return"".join(chr(x%8+97)+str(x//16+1)for x in(m&127,m>>7&127))+((r:=m>>14)and"rqnb"[r&3]or"")
def evaluate():
 return(sum(T[abs(p)<<7|q]*p//abs(p)for q,p in enumerate(b)if p)+30*((J[0]&~4==2)-(J[1]&~4==114)))*side
def search(d,al,be,pl):
 global N,AB
 N+=1
 if N>=B:AB=1;return 0
 if b in[w[0]for w in u]:return 0
 tm=TT.get(k:=tuple(b),0)
 ic=attacked(J[side<0],-side)
 if d<1 and not ic:
  s=evaluate()
  if s>=be:return s
  al=max(al,s)
 ms=gen(d<1 and not ic)
 if not ms:return-1e5+pl if ic else al*(d<1)
 ms.sort(key=lambda m:36*(m!=tm)-6*(m>>14)+b[m>>7&127]*side+(m!=KL[pl]));ca=-be;bm=0
 for i,m in enumerate(ms):
  q=b[m>>7&127]|m>>14
  make(m)
  r=d-1-(i>2<d and not(q or ic))
  s=-search(r,ca,-al,pl+1)
  if ca>-be and al<s<be or r<d-1 and s>al:s=-search(d-1,-be,-al,pl+1)
  unmake()
  if AB:return 0
  if s>al:al=s;bm=m
  if al>=be:
   if not q:KL[pl]=m
   break
  ca=-al-1
 TT[k]=bm
 return al
def root(fen,lu,hk):
 global N,AB,TT,B
 setup(fen)
 TT={};KL[:]=[0]*512;N=0
 ms=[m for m in gen() if uci(m) in lu]
 bm=tb=ms[0]
 for d in range(6):
  AB=0;B=min(N+4e4,12e4);bs=ca=W=-1e6
  for m in ms:
   make(m)
   s=-search(d,ca,-bs,1)
   if s>bs and ca>W:s=-search(d,W,-bs,1)
   unmake()
   if AB:break
   if s>bs:bs=s;tb=m;ca=-bs-1
  if AB or bs>9e4:bm=tb;break
  bm=tb;ms.sort(key=bm.__ne__)
 return [uci(bm)]
def post(p,y={}):
 try:return json.load(R.urlopen(U+p,json.dumps(y).encode()))
 except R.HTTPError as e:return e.code,
if __name__=="__main__":
 while True:
  g=post("/game")
  if type(g)is tuple:exit(g)
  if g["status"]=="done":break
  while g["status"]=="your_move":
   for w in root(g["fen"],L:=g["legal_moves"],0)+[L[0]]:
    r=post("/move",{"game":g["game"],"move":w})
    if type(r)is not tuple:g=r;break
    elif r[0]!=400:exit(r)
   else:exit("nomove")
