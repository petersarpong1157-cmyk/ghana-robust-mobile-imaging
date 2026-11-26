#include <Rcpp.h>
#include <vector>
#include <queue>
#include <unordered_map>
#include <limits>
#include <algorithm>
#include <cmath>
using namespace Rcpp;
struct State {std::unordered_map<int,int> next; int fail=0; bool bad=false; int vertex=-1;};
struct Router {
 int n,m; std::vector<int> head,edges,from,to; std::vector<State> ac;
 Router(int N,IntegerVector F,IntegerVector T,List patterns):n(N),m(F.size()),head(N+1,0),edges(m),from(m),to(m),ac(1){
  if(T.size()!=m||n<1) stop("invalid graph");
  for(int i=0;i<m;i++){from[i]=F[i]-1;to[i]=T[i]-1;if(from[i]<0||from[i]>=n||to[i]<0||to[i]>=n)stop("invalid endpoint");head[from[i]+1]++;}
  for(int i=1;i<=n;i++)head[i]+=head[i-1]; auto pos=head; for(int i=0;i<m;i++)edges[pos[from[i]]++]=i;
  for(int j=0;j<patterns.size();j++){IntegerVector p=patterns[j];if(p.size()<2)stop("short pattern");int s=0,last=-1;for(int x:p){int e=x-1;if(e<0||e>=m||(last>=0&&to[last]!=from[e]))stop("invalid pattern");last=e;auto it=ac[s].next.find(e);if(it==ac[s].next.end()){int k=ac.size();ac[s].next[e]=k;ac.emplace_back();ac[k].vertex=to[e];s=k;}else s=it->second;}ac[s].bad=true;}
  std::queue<int> q;for(auto kv:ac[0].next)q.push(kv.second);while(!q.empty()){int s=q.front();q.pop();for(auto kv:ac[s].next){int e=kv.first,k=kv.second,f=ac[s].fail;while(f&& !ac[f].next.count(e))f=ac[f].fail;auto it=ac[f].next.find(e);ac[k].fail=it==ac[f].next.end()?0:it->second;ac[k].bad=ac[k].bad||ac[ac[k].fail].bad;q.push(k);}}
 }
 int advance(int s,int e){while(s&&!ac[s].next.count(e))s=ac[s].fail;auto it=ac[s].next.find(e);return it==ac[s].next.end()?0:it->second;}
 NumericVector route(int origin,NumericVector cost,double bound,IntegerVector targets){
  if(origin<1||origin>n||cost.size()!=m||std::isnan(bound)||bound<0)stop("invalid query");
  const double inf=std::numeric_limits<double>::infinity();std::vector<double> d(n+ac.size(),inf),best(n,inf);std::vector<unsigned char> wanted(n,0);int remaining=0;
  for(int t:targets){if(t<1||t>n)stop("invalid target");if(!wanted[t-1]){wanted[t-1]=1;remaining++;}}
  bool limited=remaining>0;using P=std::pair<double,int>;std::priority_queue<P,std::vector<P>,std::greater<P>> heap;d[origin-1]=0;heap.push({0,origin-1});long long pops=0;
  while(!heap.empty()){auto p=heap.top();heap.pop();double x=p.first;int key=p.second;if(x!=d[key])continue;if(x>bound)break;if(++pops%100000==0)checkUserInterrupt();int s=key<n?0:key-n;int v=s?ac[s].vertex:key;
   if(!std::isfinite(best[v])){best[v]=x;if(wanted[v]&&--remaining==0&&limited)break;}
   for(int i=head[v];i<head[v+1];i++){int e=edges[i];int k=advance(s,e);if(ac[k].bad)continue;double w=cost[e];if(!std::isfinite(w)||w<=0)stop("invalid cost");double y=x+w;int dest=k?n+k:to[e];if(y<=bound&&y<d[dest]){d[dest]=y;heap.push({y,dest});}}
  }
  NumericVector ans(targets.size());for(int i=0;i<targets.size();i++)ans[i]=best[targets[i]-1];return ans;
 }
};
// [[Rcpp::export]]
SEXP router_create(int n,IntegerVector from,IntegerVector to,List patterns){XPtr<Router> p(new Router(n,from,to,patterns),true);return p;}
// [[Rcpp::export]]
NumericVector router_query(SEXP ptr,int origin,NumericVector cost,double bound,IntegerVector targets){XPtr<Router> p(ptr);return p->route(origin,cost,bound,targets);}

