# Phase 11A: deterministic greedy maximum expected coverage and equity benchmarks.
phase11A_run <- function() {
  suppressPackageStartupMessages({library(data.table);library(Matrix);library(digest);library(Rcpp)})
  start <- Sys.time()
  root <- normalizePath(file.path(Sys.getenv('USERPROFILE'),'Downloads','AI PREDICTION','Ghana_Diagnostic_Imaging_Research'),winslash='/',mustWork=TRUE)
  P <- function(...) file.path(root,...)
  hash <- function(x) vapply(x,function(f) digest(file=f,algo='sha256'),character(1),USE.NAMES=FALSE)
  old <- rbindlist(lapply(c('10A','10B','10C'),function(s){
    m <- fread(P('12_Reproducibility',paste0('phase',s,'_output_checksum_manifest.csv')))
    stopifnot(all(hash(P(m$relative_path))==m$SHA256));cat('Phase',s,':',nrow(m),'checksums PASS\n');m[,.(relative_path,SHA256)]
  }))
  out <- P('06_Optimization','Phase11A');dir.create(out,recursive=TRUE,showWarnings=FALSE)
  paths <- character();write <- function(x,p){fwrite(x,p);paths<<-c(paths,p)}
  base <- P('05_Travel_Time_Matrices','Phase10C_Mobile')
  idx <- readRDS(file.path(base,'mobile_matrix_index.rds'));d <- as.data.table(idx$demand);cands <- as.data.table(idx$candidates)
  loc <- as.data.table(readRDS(P('08_Results','Phase10A_Accessibility','primary_location_probabilities.rds')))
  stopifnot(!anyDuplicated(d$demand_id),!anyDuplicated(cands$candidate_id),!anyDuplicated(loc$demand_id),setequal(d$demand_id,loc$demand_id))
  loc <- loc[match(d$demand_id,loc$demand_id)];stopifnot(identical(d$demand_id,loc$demand_id),all(abs(d$population-loc$population)<1e-8),identical(d$match_1km,loc$match_1km))
  cov <- readRDS(file.path(base,'BASELINE','coverage_matrices.rds'))
  stopifnot(nrow(d)==51014L,nrow(cands)==3581L,sum(d$match_1km)==31002L,abs(sum(d$population)-34746370)<1)
  for(B in cov) stopifnot(identical(rownames(B),as.character(d$demand_id)),identical(colnames(B),as.character(cands$candidate_id)),all(Matrix::rowSums(B)[!d$match_1km]==0))
  regions <- sort(unique(d$region_group[d$region_group!='UNASSIGNED']));stopifnot(length(regions)==16L)
  rg <- match(d$region_group,regions);rg[is.na(rg)] <- 0L
  pop <- d$population;N <- sum(pop);rp <- vapply(seq_along(regions),function(r)sum(pop[rg==r]),numeric(1));stopifnot(all(rp>0))
  if(Sys.info()[['sysname']]=='Windows' && file.exists('C:/rtools45/usr/bin/make.exe')) Sys.setenv(PATH=paste('C:/rtools45/usr/bin','C:/rtools45/x86_64-w64-mingw32.static.posix/bin',Sys.getenv('PATH'),sep=';'),RTOOLS45_HOME='C:/rtools45')
  # Incremental sparse marginal gains: a newly covered row is removed from every incident candidate.
  Rcpp::cppFunction(code='
  Rcpp::List greedy_sparse(Rcpp::IntegerVector cp, Rcpp::IntegerVector ci,
    Rcpp::IntegerVector rp0, Rcpp::IntegerVector ri, Rcpp::NumericVector w,
    Rcpp::IntegerVector region, Rcpp::NumericVector denom, Rcpp::NumericVector base,
    double total, double baseNat, double lambda, int K) {
    int J=cp.size()-1, R=denom.size(), n=w.size();
    Rcpp::NumericMatrix gain(J,R); Rcpp::NumericVector g(J), cur=Rcpp::clone(base);
    Rcpp::LogicalVector covered(n), chosen(J); Rcpp::IntegerVector sel(K);
    Rcpp::NumericVector obj(K), ub(K); double nat=baseNat;
    for(int j=0;j<J;j++) for(int a=cp[j];a<cp[j+1];a++) {int i=ci[a];g[j]+=w[i]; if(region[i]>0)gain(j,region[i]-1)+=w[i];}
    for(int step=0;step<K;step++) {
      int best=-1; double bv=-1e100;
      for(int j=0;j<J;j++) if(!chosen[j] && cp[j+1]>cp[j]) {
        double mn=1.0; for(int r=0;r<R;r++) mn=std::min(mn,cur[r]+gain(j,r)/denom[r]);
        double v=(1-lambda)*(nat+g[j]/total)+lambda*mn;
        if(v>bv+1e-13){bv=v;best=j;}
      }
      if(best<0) Rcpp::stop("Insufficient productive candidates");
      chosen[best]=true;sel[step]=best+1;
      for(int a=cp[best];a<cp[best+1];a++){int i=ci[a];if(covered[i])continue;covered[i]=true;nat+=w[i]/total;int r=region[i]-1;if(r>=0)cur[r]+=w[i]/denom[r];
        for(int b=rp0[i];b<rp0[i+1];b++){int j=ri[b];g[j]-=w[i];if(r>=0)gain(j,r)-=w[i];}
      }
      double mn=1;for(int r=0;r<R;r++)mn=std::min(mn,cur[r]);obj[step]=(1-lambda)*nat+lambda*mn;
      // Global upper bound for a fleet of step+1, not conditional on greedy selections.
      // For any feasible O: f(O)<=f(S)+sum of its individual residual marginal gains at S.
      int k=step+1; double un=nat, ur=1;
      std::vector<double> v(J);for(int j=0;j<J;j++)v[j]=std::max(0.0,g[j]);std::sort(v.begin(),v.end(),std::greater<double>());for(int j=0;j<k;j++)un+=v[j]/total;un=std::min(1.0,un);
      for(int r=0;r<R;r++){for(int j=0;j<J;j++)v[j]=std::max(0.0,gain(j,r));std::sort(v.begin(),v.end(),std::greater<double>());double u=cur[r];for(int j=0;j<k;j++)u+=v[j]/denom[r];ur=std::min(ur,u);}
      ub[step]=std::min(1.0,(1-lambda)*un+lambda*ur);
    }
    return Rcpp::List::create(Rcpp::Named("selected")=sel,Rcpp::Named("objective")=obj,Rcpp::Named("upper")=ub);
  }',includes='#include <algorithm>\n#include <vector>\n#include <functional>',env=environment())
  solve <- function(B,w,region,denom,base,baseNat,lambda,K){
    B <- as(B,'CsparseMatrix');Bt <- as(t(B),'CsparseMatrix')
    greedy_sparse(B@p,B@i,Bt@p,Bt@i,as.numeric(w),as.integer(region),as.numeric(denom),as.numeric(base),sum(pop),baseNat,lambda,as.integer(K))
  }
  # Tiny exhaustive benchmark: independent of national data; greedy bound checked against all subsets.
  toy <- sparseMatrix(i=c(1,2,2,3,4),j=c(1,1,2,2,3),dims=c(4,3));tt <- as(toy,'CsparseMatrix');tr <- as(t(tt),'CsparseMatrix');tw <- c(4,3,2,1);tg <- c(1L,1L,2L,2L);td <- c(7,3)
  for(lam in c(0,.5,1)){
    z <- greedy_sparse(tt@p,tt@i,tr@p,tr@i,tw,tg,td,c(0,0),10,0,lam,2L)
    score <- function(j){hit<-as.numeric(Matrix::rowSums(toy[,j,drop=FALSE])>0);(1-lam)*sum(tw*hit)/10+lam*min(sum(tw[1:2]*hit[1:2])/7,sum(tw[3:4]*hit[3:4])/3)}
    opt <- max(combn(3,2,FUN=score));stopifnot(abs(score(z$selected)-z$objective[2])<1e-10,z$objective[2]<=opt+1e-10,z$upper[2]>=opt-1e-10)
  };cat('Synthetic exhaustive objective and bound tests: PASS\n')
  inequality <- function(x,w){w<-w/sum(w);mu<-sum(w*x);o<-order(x);xx<-x[o];ww<-w[o];g<-if(mu>0)1-sum(ww*(2*cumsum(ww*xx)-ww*xx))/mu else NA_real_;v<-x/mu;t<-if(mu>0)sum(w[v>0]*v[v>0]*log(v[v>0]))else NA_real_;c(gini=g,theil=t)}
  district <- as.character(d$district_code);district[rg==0L] <- 'UNASSIGNED';stopifnot(!anyNA(district[rg>0]),all(nzchar(district[rg>0])))
  dg <- match(paste(d$region_group,district,sep='|'),unique(paste(d$region_group,district,sep='|')))
  districts <- data.table(g=dg,region=d$region_group,district=district,population=pop)[,.(population=sum(population)),by=.(g,region,district)];setorder(districts,g)
  stopifnot(nrow(districts)==261L)
  fleets <- c(8L,16L,32L,64L,100L);weights <- c(0,.25,.5,.75,1);modalities <- c('XRAY','ULTRASOUND','COLOCATED');thresholds <- c(30L,60L,120L)
  results<-list();regs<-list();dists<-list();selections<-list();audits<-list();id<-0L
  for(h in thresholds){
    B <- cov[[as.character(h)]];stopifnot(!is.null(B));productive<-which(Matrix::colSums(B)>0);stopifnot(length(productive)>=max(fleets));B <- B[,productive,drop=FALSE]
    # Deterministic tie-breaking by lexicographic candidate identifier.
    o<-order(colnames(B));B<-B[,o,drop=FALSE];candix<-productive[o]
    for(m in modalities){
      p<-loc[[paste(m,h,sep='_')]];stopifnot(length(p)==nrow(d),all(is.finite(p)&p>=0&p<=1),all(p[!d$match_1km]==0))
      w<-pop*(1-p);baseNat<-sum(pop*p)/N;br<-vapply(seq_along(regions),function(r)sum(pop[rg==r]*p[rg==r])/rp[r],numeric(1))
      for(lam in weights){
        z<-solve(B,w,rg,rp,br,baseNat,lam,max(fleets));stopifnot(!anyDuplicated(z$selected),all(diff(z$objective)>=-1e-10),all(z$upper+1e-9>=z$objective))
        for(k in fleets){
          id<-id+1L;j<-z$selected[seq_len(k)];hit<-as.logical(Matrix::rowSums(B[,j,drop=FALSE])>0);post<-p+(1-p)*hit;gain<-sum(w[hit]);nat<-sum(pop*post)/N
          rr<-vapply(seq_along(regions),function(r)sum(pop[rg==r]*post[rg==r])/rp[r],numeric(1));dv<-as.numeric(rowsum(matrix(pop*post,ncol=1),dg,reorder=TRUE))/districts$population
          obj<-(1-lam)*nat+lam*min(rr);stopifnot(abs(obj-z$objective[k])<1e-8,abs(nat-baseNat-gain/N)<1e-9,all(post>=p),all(post<=1+1e-12),all(!hit[!d$match_1km]),abs(sum(districts$population*dv)/N-nat)<1e-9)
          ir<-inequality(rr,rp);keep<-districts$region!='UNASSIGNED';di<-inequality(dv[keep],districts$population[keep]);ub<-max(obj,z$upper[k]);
          results[[id]]<-data.table(run_id=id,model=if(lam==0)'A_CONVENTIONAL'else 'B_EQUITY',modality=m,threshold_minutes=h,lambda=lam,fleet=k,baseline_pct=100*baseNat,post_pct=100*nat,gain_pp=100*(nat-baseNat),additional_population=gain,min_region_pct=100*min(rr),baseline_min_region_pct=100*min(br),region_gap_pp=100*diff(range(rr)),region_gini=ir[['gini']],region_theil=ir[['theil']],district_gini=di[['gini']],district_theil=di[['theil']],objective= obj,global_upper_bound=ub,absolute_bound_gap=ub-obj,relative_bound_gap=if(ub>0)(ub-obj)/ub else 0,method='DETERMINISTIC_GREEDY',productive_candidates=ncol(B))
          regs[[id]]<-data.table(run_id=id,region=regions,population=rp,baseline_pct=100*br,post_pct=100*rr,gain_pp=100*(rr-br));dists[[id]]<-cbind(data.table(run_id=id),districts[,.(region,district,population)],data.table(post_pct=100*dv));
          selections[[id]]<-cbind(data.table(run_id=id,selection_order=seq_len(k)),cands[candix[j],.(candidate_id,candidate_type,region,district,longitude,latitude,vertex_id)])
          audits[[id]]<-data.table(run_id=id,objective_error=abs(obj-z$objective[k]),population_error=abs(sum(districts$population*dv)-N*nat),fleet_unique=length(unique(j))==k,unmatched_covered=sum(hit[!d$match_1km]))
        };cat('Completed',h,m,'lambda',lam,'\n');flush.console()
      }
    }
  }
  ans<-rbindlist(results);reg<-rbindlist(regs);dis<-rbindlist(dists);sel<-rbindlist(selections);audit<-rbindlist(audits)
  stopifnot(nrow(ans)==225L,nrow(reg)==3600L,nrow(dis)==225L*261L,all(audit$fleet_unique),all(audit$unmatched_covered==0),all(audit$objective_error<1e-8),all(audit$population_error<1e-4))
  # Pareto flag on achieved solutions only, not a certified global frontier.
  ans[,nondominated:=vapply(seq_len(.N),function(i)!any(post_pct>=post_pct[i]-1e-9 & min_region_pct>=min_region_pct[i]-1e-9 & (post_pct>post_pct[i]+1e-9 | min_region_pct>min_region_pct[i]+1e-9)),logical(1)),by=.(modality,threshold_minutes,fleet)]
  natref<-fread(P('10_Tables','phase10A_national_accessibility.csv'))[run=='PRIMARY'];ref<-merge(ans,natref[,.(modality,threshold_minutes,mean_pct)],by=c('modality','threshold_minutes'));stopifnot(nrow(ref)==225L,all(abs(ref$baseline_pct-ref$mean_pct)<1e-6))
  write(ans,P('10_Tables','phase11A_optimization_summary.csv'));write(reg,P('10_Tables','phase11A_regional_results.csv'));write(dis,P('10_Tables','phase11A_district_results.csv'));write(sel,file.path(out,'selected_candidates.csv'));write(audit,P('03_Data_Audit','phase11A_solution_audit.csv'))
  saveRDS(list(summary=ans,regional=reg,district=dis,selected=sel),file.path(out,'phase11A_results.rds'));paths<-c(paths,file.path(out,'phase11A_results.rds'))
  fig<-function(name,fun){for(ext in c('png','pdf')){p<-P('09_Figures',paste0(name,'.',ext));if(ext=='png')png(p,width=2200,height=1500,res=200)else pdf(p,width=11,height=7.5);tryCatch(fun(),finally=dev.off());paths<<-c(paths,p)}}
  fig('Figure_40_Fleet_Coverage',function(){par(mfrow=c(1,3),mar=c(5,4,3,1));for(m in modalities){a<-ans[modality==m & threshold_minutes==60 & lambda%in%c(0,.5)];plot(NA,xlim=range(fleets),ylim=c(0,100),xlab='Fleet size',ylab='Expected national coverage (%)',main=m);for(l in c(0,.5)){q<-a[lambda==l];setorder(q,fleet);lines(q$fleet,q$post_pct,type='b',col=if(l==0)'#2166AC'else '#1B9E77',pch=if(l==0)16 else 17)};abline(h=a$baseline_pct[1],lty=3);legend('bottomright',c('Conventional','Equity 0.5','No fleet'),col=c('#2166AC','#1B9E77','black'),lty=c(1,1,3),bty='n',cex=.8)}})
  fig('Figure_41_Efficiency_Equity',function(){par(mfrow=c(1,3),mar=c(5,4,3,1));for(m in modalities){a<-ans[modality==m & threshold_minutes==60 & fleet==32];plot(a$post_pct,a$min_region_pct,pch=19,xlim=range(a$post_pct)+c(-.5,.5),ylim=range(a$min_region_pct)+c(-.5,.5),xlab='National coverage (%)',ylab='Minimum regional coverage (%)',main=m);text(a$post_pct,a$min_region_pct,labels=a$lambda,pos=3,cex=.8)}})
  fig('Figure_42_Regional_Xray_Gains',function(){a<-ans[modality=='XRAY' & threshold_minutes==60 & fleet==32 & lambda%in%c(0,.5)];q<-merge(reg,a[,.(run_id,lambda)],by='run_id');z<-dcast(q,region~lambda,value.var='gain_pp');setorder(z,region);par(mar=c(5,10,3,2));barplot(t(as.matrix(z[,-1,with=FALSE])),beside=TRUE,horiz=TRUE,names.arg=z$region,las=1,col=c('#2166AC','#1B9E77'),xlab='Expected coverage gain (percentage points)',main='32 hypothetical X-ray units: 60 minutes');legend('bottomright',c('Conventional','Equity 0.5'),fill=c('#2166AC','#1B9E77'),bty='n')})
  gate<-data.table(check=c('Prior manifests unchanged','Population and candidate alignment','Synthetic exhaustive objective and bound tests','225 allocations','Regional and district inventory','Unique sites and fleet sizes','Independent objective reconciliation','Unmatched population preserved','Baseline reproduced','Global upper bounds valid for reported objective'),passed=c(all(hash(P(old$relative_path))==old$SHA256),TRUE,TRUE,nrow(ans)==225L,nrow(reg)==3600L&&nrow(dis)==58725L,all(audit$fleet_unique),all(audit$objective_error<1e-8),all(audit$unmatched_covered==0),all(abs(ref$baseline_pct-ref$mean_pct)<1e-6),all(ans$global_upper_bound>=ans$objective-1e-9)));stopifnot(all(gate$passed));write(gate,P('03_Data_Audit','phase11A_quality_gate.csv'))
  write(data.table(item=c('Solver','Coverage','Equity','Fleet','Bounds','Scope'),treatment=c('Deterministic greedy, not exact MIP; nested fleet paths','Expected residual coverage using full primary simulation means; no double counting','Weighted national coverage plus minimum of 16 regional expected coverages','One hypothetical unit per selected site, unlimited service capacity','Submodular residual marginal upper bounds combined for national and minimum region objective; may be loose','Baseline transportation only; no held-out or robust optimization yet')),P('12_Reproducibility','phase11A_modelling_assumptions.csv'))
  sp<-P('13_R_Scripts','phase11A_complete.R');writeLines(c('phase11A_run <-',deparse(phase11A_run,width.cutoff=500L),'phase11A_results <- phase11A_run()'),sp);paths<-c(paths,sp)
  paths<-sort(unique(normalizePath(paths,winslash='/',mustWork=TRUE)));man<-data.table(relative_path=substring(paths,nchar(root)+2),SHA256=hash(paths),size_bytes=file.info(paths)$size);fwrite(man,P('12_Reproducibility','phase11A_output_checksum_manifest.csv'));man[,verified:=hash(P(relative_path))==SHA256];stopifnot(all(man$verified));fwrite(man,P('12_Reproducibility','phase11A_checksum_recheck.csv'))
  print(ans[threshold_minutes==60 & lambda%in%c(0,.5),.(modality,lambda,fleet,post_pct,gain_pp,min_region_pct,region_gini,absolute_bound_gap)],nrows=30);print(gate);cat('PHASE 11A GREEDY BENCHMARKS PASS\nAllocations:',nrow(ans),'\nChecksums:',sum(man$verified),'/',nrow(man),'\nElapsed seconds:',as.numeric(difftime(Sys.time(),start,units='secs')),'\nNOT CERTIFIED GLOBAL OPTIMA; HYPOTHETICAL DEPLOYMENT ONLY\n');invisible(ans)
}
phase11A_results <- phase11A_run()
