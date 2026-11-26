# Phase 11C2: frozen allocations, paired held-out draw-level stress assessment.
phase11C2_run <- function(){
  suppressPackageStartupMessages({library(data.table);library(Matrix);library(Rcpp);library(digest)})
  started<-Sys.time();root<-normalizePath(file.path(Sys.getenv('USERPROFILE'),'Downloads','AI PREDICTION','Ghana_Diagnostic_Imaging_Research'),winslash='/',mustWork=TRUE)
  P<-function(...)file.path(root,...);hash<-function(p)vapply(p,function(x)digest(file=x,algo='sha256'),character(1),USE.NAMES=FALSE)
  prior<-rbindlist(lapply(c('6B','6B2','9B1','9B2','10A','10B','10C','11A','11B','11C1','11C1B'),function(s){m<-fread(P('12_Reproducibility',paste0('phase',s,'_output_checksum_manifest.csv')));stopifnot(all(hash(P(m$relative_path))==m$SHA256));cat('Phase',s,':',nrow(m),'checksums PASS\n');m[,.(relative_path,SHA256)]}))
  out<-P('06_Optimization','Phase11C2');ck<-file.path(out,'Checkpoints');dir.create(ck,recursive=TRUE,showWarnings=FALSE);paths<-character()
  write<-function(x,p){fwrite(x,p);paths<<-c(paths,p)}
  if(Sys.info()[['sysname']]=='Windows')Sys.setenv(PATH=paste('C:/rtools45/usr/bin','C:/rtools45/x86_64-w64-mingw32.static.posix/bin',Sys.getenv('PATH'),sep=';'),RTOOLS45_HOME='C:/rtools45')
  cpp <- '
#include <Rcpp.h>
#include <map>
#include <vector>
#include <algorithm>
using namespace Rcpp;
// [[Rcpp::export]]
List group_weights(IntegerVector ptr,IntegerVector fac,IntegerVector region,NumericVector pop,LogicalMatrix hit){
 int n=pop.size(),J=hit.ncol(); std::map<std::vector<int>,int> lookup; std::vector<std::vector<int> > keys; std::vector<double> weights;
 for(int i=0;i<n;i++){
  std::vector<int> key;key.push_back(region[i]);for(int e=ptr[i];e<ptr[i+1];e++)key.push_back(fac[e]);
  auto it=lookup.find(key);int g;if(it==lookup.end()){g=keys.size();lookup[key]=g;keys.push_back(key);weights.resize((size_t)(g+1)*(J+1),0.0);}else g=it->second;
  weights[(size_t)g*(J+1)+J]+=pop[i];for(int j=0;j<J;j++)if(!hit(i,j))weights[(size_t)g*(J+1)+j]+=pop[i];
 }
 int G=keys.size();NumericMatrix W(G,J+1);IntegerVector gp(G+1),gr(G);std::vector<int> ff;
 for(int g=0;g<G;g++){gr[g]=keys[g][0];for(size_t k=1;k<keys[g].size();k++)ff.push_back(keys[g][k]);gp[g+1]=ff.size();for(int j=0;j<=J;j++)W(g,j)=weights[(size_t)g*(J+1)+j];}
 return List::create(_["ptr"]=gp,_["fac"]=wrap(ff),_["region"]=gr,_["W"]=W);
}
// [[Rcpp::export]]
NumericVector draw_deficits(List groups,LogicalMatrix X,LogicalMatrix U,IntegerVector modality,int R){
 IntegerVector ptr=groups["ptr"],fac=groups["fac"],region=groups["region"];NumericMatrix W=groups["W"];
 int S=X.ncol(),J=modality.size(),G=W.nrow();NumericVector ans((size_t)S*R*(J+3));ans.attr("dim")=IntegerVector::create(S,R,J+3);
 std::vector<int> missing[3];for(int m=0;m<3;m++)missing[m].reserve(S);
 for(int g=0;g<G;g++){
  for(int m=0;m<3;m++)missing[m].clear();
  for(int s=0;s<S;s++){bool x=false,u=false,c=false;for(int e=ptr[g];e<ptr[g+1];e++){int j=fac[e]-1;bool xx=X(j,s),uu=U(j,s);x|=xx;u|=uu;c|=(xx&&uu);if(c)break;}if(!x)missing[0].push_back(s);if(!u)missing[1].push_back(s);if(!c)missing[2].push_back(s);}
  for(int j=0;j<J+3;j++){int m=j<J?modality[j]:j-J;double w=W(g,j<J?j:J);if(w==0)continue;size_t k=(size_t)S*(region[g]+R*j);for(int s:missing[m])ans[k+s]+=w;}
 }
 return ans;
}
'
  Rcpp::sourceCpp(code=cpp,env=environment())
  # Independent toy reference: both modalities at different sites do not imply colocated coverage.
  tp<-c(0L,2L,3L,3L);tf<-c(1L,2L,2L);tg<-c(1L,1L,0L);tw<-c(3,5,2);th<-matrix(c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE,FALSE),3,3)
  tx<-matrix(c(TRUE,FALSE,FALSE,TRUE),2,2);tu<-matrix(c(FALSE,TRUE,FALSE,TRUE),2,2);tm<-0:2
  tt<-draw_deficits(group_weights(tp,tf,tg,tw,th),tx,tu,tm,2L);ref<-array(0,c(2,2,6))
  for(i in 1:3)for(s in 1:2){f<-if(tp[i+1]>tp[i])tf[seq.int(tp[i]+1L,tp[i+1L])]else integer();a<-c(any(tx[f,s]),any(tu[f,s]),any(tx[f,s]&tu[f,s]));for(j in 1:6)if(!a[(j-1)%%3+1] && (j>3 || !th[i,j]))ref[s,tg[i]+1,j]<-ref[s,tg[i]+1,j]+tw[i]}
  stopifnot(max(abs(tt-ref))<1e-12);cat('Synthetic draw-level reference PASS\n')
  bdir<-P('06_Optimization','Phase11B');fit<-readRDS(file.path(bdir,'phase11B_results.rds'));ans<-as.data.table(fit$summary);sel<-as.data.table(fit$selected);orig<-as.data.table(fit$design);setorder(ans,run_id)
  testids<-as.integer(fit$testids);trainids<-as.integer(fit$trainids);stopifnot(length(testids)==1000L,length(trainids)==1000L,!anyDuplicated(testids),!anyDuplicated(trainids),length(intersect(testids,trainids))==0L,identical(sort(c(testids,trainids)),1:2000),nrow(ans)==60L,identical(ans$run_id,1:60))
  common<-sort(testids)[seq_len(250L)];mods<-c('XRAY','ULTRASOUND','COLOCATED');mi<-match(ans$modality,mods)-1L;stopifnot(!anyNA(mi),all(ans[, .N,by=.(modality,model,fleet)]$N==1L))
  base<-P('05_Travel_Time_Matrices','Phase10C_Mobile');idx<-readRDS(file.path(base,'mobile_matrix_index.rds'));d<-as.data.table(idx$demand);cands<-as.data.table(idx$candidates)
  pop<-as.numeric(d$population);N<-sum(pop);regions<-sort(unique(d$region_group[d$region_group!='UNASSIGNED']));rg<-match(d$region_group,regions);rg[is.na(rg)]<-0L;R<-length(regions);regnames<-c('UNASSIGNED',regions);rp<-vapply(0:R,function(r)sum(pop[rg==r]),numeric(1));stopifnot(R==16L,nrow(d)==51014L,nrow(cands)==3581L,abs(N-34746370)<1,all(rp>0))
  sites<-lapply(ans$run_id,function(k){ids<-sel[run_id==k,candidate_id];j<-match(ids,cands$candidate_id);stopifnot(length(j)==ans$fleet[k],!anyNA(j),!anyDuplicated(j));j})
  ens<-c(list.files(P('02_Processed_Data','Phase6B_Infrastructure_Ensemble'),'^ENS_.*[.]rds$',full.names=TRUE),list.files(P('02_Processed_Data','Phase6B_Part2_Ensemble'),'^(FAC_|STRUCT_).*[.]rds$',full.names=TRUE));names(ens)<-sub('[.]rds$','',basename(ens));stopifnot(length(ens)==115L,!anyDuplicated(names(ens)))
  speeds<-c('baseline','slow','favorable');mids<-c('M01','M02','M03');origens<-unique(orig$ensemble)
  # Deterministic design, not selected by accessibility outcomes: four evenly spaced indices per family plus remaining structural cases.
  stress<-c(sprintf('ENS_%03d',c(1,19,36,54)),sprintf('FAC_%03d',c(1,19,36,54)),sort(setdiff(names(ens)[startsWith(names(ens),'STRUCT_')],origens)))
  stopifnot(length(stress)==12L,!anyDuplicated(stress),all(stress%in%names(ens)),!any(stress%in%origens))
  design<-rbindlist(list(orig[,.(speed,matrix_id,ensemble,original_scenario_id=scenario_id)],data.table(speed='baseline',matrix_id='M01',ensemble=sort(setdiff(names(ens),origens)),original_scenario_id=NA_integer_),data.table(speed='slow',matrix_id='M02',ensemble=stress,original_scenario_id=NA_integer_),data.table(speed='favorable',matrix_id='M03',ensemble=stress,original_scenario_id=NA_integer_)))
  design[,scenario_id:=.I];design[,draws:=ifelse(is.na(original_scenario_id),250L,1000L)];stopifnot(nrow(design)==145L,!anyDuplicated(design,by=c('speed','ensemble')),sum(design$speed=='baseline')==115L)
  write(design,file.path(out,'scenario_design.csv'));write(data.table(draw=sort(testids),extended_sample=sort(testids)%in%common),file.path(out,'heldout_draw_indices.csv'))
  fixed<-function(mid){dir<-if(mid=='M01')P('05_Travel_Time_Matrices','Phase9B1_Core_Baseline')else P('05_Travel_Time_Matrices','Phase9B2_Sensitivity',mid);a<-readRDS(file.path(dir,'matrix_index.rds'));dd<-as.data.table(a$demand);f<-as.data.table(a$facilities);stopifnot(identical(dd$demand_id,d$demand_id),identical(dd$match_1km,d$match_1km),all(abs(dd$population-pop)<1e-8),a$bound_seconds==7200)
    ds<-dd[match_1km==TRUE,.(target_index,di=row_id,dc=connector_m)];fs<-f[match_1km==TRUE & supply_candidate==TRUE,.(source_index,fi=facility_row,fc=connector_m)]
    e<-rbindlist(lapply(a$batch_files,function(b){z<-readRDS(file.path(dir,'Checkpoints',b));stopifnot(identical(z$signature,a$signature));raw<-rbindlist(lapply(seq_along(z$source_indices),function(k){x<-z$columns[[k]];data.table(source_index=rep(z$source_indices[k],length(x$i)),target_index=x$i,net=x$seconds)}));q<-merge(raw,fs,by='source_index',allow.cartesian=TRUE);q<-merge(q,ds,by='target_index',allow.cartesian=TRUE);q[net+3.6*(dc+fc)/4<=3600,.(di,fi)]}));setorder(e,di,fi);stopifnot(!anyDuplicated(e,by=c('di','fi')),all(e$di>=1L&e$di<=nrow(d)),all(e$fi>=1L&e$fi<=nrow(f)),all(d$match_1km[e$di]));list(f=f,e=e,ptr=as.integer(c(0,cumsum(tabulate(e$di,nbins=nrow(d))))))}
  sig<-digest(list(prior=prior,design=design,sites=sites,testids=testids,common=common,cpp=cpp,version='11C2-v1'),algo='sha256')
  ns<-nrow(design);J<-nrow(ans);summarylist<-reglist<-pairlist<-auditlist<-vector('list',ns)
  # Shared draw columns allow paired stress summaries; equal design weights are not probabilities.
  accum<-matrix(0,length(common),J);worst<-worstr<-matrix(Inf,length(common),J)
  describe<-function(x){q<-as.numeric(quantile(x,c(.025,.05,.5,.975),names=FALSE,type=7));k<-max(1L,ceiling(.05*length(x)));c(mean=mean(x),sd=sd(x),mcse=sd(x)/sqrt(length(x)),p025=q[1],p05=q[2],median=q[3],p975=q[4],lower5_mean=mean(sort(x)[seq_len(k)]))}
  pairrows<-function(nat,mnr){rbindlist(lapply(mods,function(m)rbindlist(lapply(sort(unique(ans$fleet)),function(K){ii<-which(ans$modality==m & ans$fleet==K);a<-ii[match(c('A','B','C','D'),ans$model[ii])];rbindlist(lapply(1:3,function(h)rbindlist(lapply(c('NATIONAL','DRAW_MIN_REGION'),function(metric){v<-if(metric=='NATIONAL')nat else mnr;delta<-v[,a[4]]-v[,a[h]];data.table(modality=m,fleet=K,comparison=paste0('D-',c('A','B','C')[h]),metric=metric,as.list(describe(delta)),positive_fraction=mean(delta>1e-10),negative_fraction=mean(delta< -1e-10),draws=length(delta))}))))}))))}
  for(sp in speeds){
    B<-as(readRDS(file.path(base,toupper(sp),'coverage_matrices.rds'))[['60']],'CsparseMatrix');stopifnot(identical(rownames(B),as.character(d$demand_id)),identical(colnames(B),as.character(cands$candidate_id)))
    hit<-vapply(sites,function(j)Matrix::rowSums(B[,j,drop=FALSE])>0,logical(nrow(d)));stopifnot(all(!hit[!d$match_1km,]));a<-fixed(mids[match(sp,speeds)]);g<-group_weights(a$ptr,as.integer(a$e$fi),as.integer(rg),pop,hit);stopifnot(abs(sum(g$W[,J+1L])-N)<1e-5,all(abs(colSums(g$W)[seq_len(J)]-colSums((!hit)*pop))<1e-5));cat('Speed',sp,'coverage groups',nrow(g$W),'\n');rm(B,hit);gc()
    for(k in which(design$speed==sp)){
      dd<-design[k];draw<-if(dd$draws==1000L)sort(testids)else common;fpath<-file.path(ck,sprintf('scenario_%03d.rds',k))
      if(file.exists(fpath)){q<-readRDS(fpath);stopifnot(identical(q$signature,sig),identical(q$draw,draw),identical(q$scenario_id,k))}else{
        z<-readRDS(ens[[dd$ensemble]]);mf<-match(as.character(z$facility_ids),as.character(a$f$id));stopifnot(!anyNA(mf),!anyDuplicated(mf),ncol(z$xray)==2000L,identical(dim(z$xray),dim(z$ultrasound)),!anyNA(z$xray),!anyNA(z$ultrasound));X<-U<-matrix(FALSE,nrow(a$f),length(draw));X[mf,]<-z$xray[,draw,drop=FALSE];U[mf,]<-z$ultrasound[,draw,drop=FALSE]
        deficit<-draw_deficits(g,X,U,as.integer(mi),as.integer(R+1L));stopifnot(all(is.finite(deficit)),min(deficit)>=-1e-7)
        nat<-matrix(0,length(draw),J+3L);mnr<-nat;rr<-matrix(0,R,J+3L)
        for(j in seq_len(J+3L)){v<-matrix(deficit[,,j],nrow=length(draw));stopifnot(all(sweep(v,2,rp,'-')<1e-5));nat[,j]<-100*(1-rowSums(v)/N);rmat<-100*(1-sweep(v[,-1,drop=FALSE],2,rp[-1],'/'));mnr[,j]<-apply(rmat,1,min);rr[,j]<-colMeans(rmat)}
        stopifnot(all(nat>=-1e-7&nat<=100+1e-7),all(mnr>=-1e-7&mnr<=100+1e-7),all(nat[,seq_len(J)]>=nat[,J+mi+1L]-1e-7))
        q<-list(signature=sig,scenario_id=k,draw=draw,national=nat,min_region=mnr,regional_mean=rr);tmp<-paste0(fpath,'.tmp');saveRDS(q,tmp);stopifnot(file.rename(tmp,fpath));rm(deficit,X,U,z)
      }
      stopifnot(identical(dim(q$national),c(length(draw),J+3L)),identical(dim(q$regional_mean),c(R,J+3L)),all(is.finite(q$national)),all(is.finite(q$min_region)))
      paths<-c(paths,fpath);nat<-q$national[,seq_len(J),drop=FALSE];mnr<-q$min_region[,seq_len(J),drop=FALSE];rr<-q$regional_mean[,seq_len(J),drop=FALSE]
      summarylist[[k]]<-rbindlist(lapply(seq_len(J),function(j)data.table(scenario_id=k,run_id=ans$run_id[j],as.list(describe(nat[,j])),draw_min_region_mean=mean(mnr[,j]),draw_min_region_p05=as.numeric(quantile(mnr[,j],.05)),min_region_mean=min(rr[,j]),baseline_mean=mean(q$national[,J+mi[j]+1L]),mean_gain_pp=mean(nat[,j]-q$national[,J+mi[j]+1L]),draws=length(draw))))
      reglist[[k]]<-data.table(scenario_id=k,run_id=rep(ans$run_id,each=R),region=rep(regions,J),mean_pct=as.vector(rr));pairlist[[k]]<-cbind(data.table(scenario_id=k),pairrows(nat,mnr))
      erN<-erR<-NA_real_;if(!is.na(dd$original_scenario_id)){oo<-dd$original_scenario_id;v<-as.data.table(fit$scenario)[scenario_id==oo];v<-v[match(ans$run_id,v$run_id)];r0<-as.data.table(fit$regional)[scenario_id==oo];r1<-merge(reglist[[k]],r0,by=c('run_id','region'));stopifnot(nrow(r1)==J*R,identical(v$run_id,ans$run_id));erN<-max(abs(colMeans(nat)-v$test_pct));erR<-max(abs(r1$mean_pct-r1$test_pct));stopifnot(erN<1e-7,erR<1e-7)}
      auditlist[[k]]<-data.table(scenario_id=k,draws=length(draw),original_national_error=erN,original_regional_error=erR,minimum_gain=min(nat-q$national[,J+mi+1L]),groups=nrow(g$W))
      ii<-match(common,draw);stopifnot(!anyNA(ii));accum<-accum+nat[ii,,drop=FALSE]/ns;worst<-pmin(worst,nat[ii,,drop=FALSE]);worstr<-pmin(worstr,mnr[ii,,drop=FALSE]);cat('Completed',k,'/',ns,sp,dd$ensemble,'draws',length(draw),'\n');flush.console()
    };rm(a,g);gc()
  }
  tab<-merge(rbindlist(summarylist),ans[,.(run_id,modality,model,fleet)],by='run_id');reg<-rbindlist(reglist);pairs<-rbindlist(pairlist);aud<-rbindlist(auditlist)
  aggregate<-tab[,.(scenario_mean_pct=mean(mean),worst_scenario_mean_pct=min(mean),worst_scenario_region_mean_pct=min(min_region_mean),worst_scenario_p05_pct=min(p05),mean_gain_pp=mean(mean_gain_pp)),by=.(run_id,modality,model,fleet)]
  for(j in seq_len(J)){k<-ans$run_id[j];aggregate[run_id==k,`:=`(paired_draw_mean_scenario_pct=mean(accum[,j]),paired_draw_worst_scenario_mean_pct=mean(worst[,j]),paired_draw_worst_scenario_p05_pct=as.numeric(quantile(worst[,j],.05)),paired_draw_worst_region_mean_pct=mean(worstr[,j]),paired_draw_worst_region_p05_pct=as.numeric(quantile(worstr[,j],.05)))]}
  cross<-rbindlist(list(cbind(data.table(aggregate_metric='MEAN_SCENARIO_NATIONAL_AND_WORST_REGION'),pairrows(accum,worstr)),cbind(data.table(aggregate_metric='WORST_SCENARIO_NATIONAL_AND_WORST_REGION'),pairrows(worst,worstr))))
  ranks<-aggregate[,.(D_best_worst_region=worst_scenario_region_mean_pct[model=='D']>=max(worst_scenario_region_mean_pct)-1e-9,D_minus_A_worst_region=worst_scenario_region_mean_pct[model=='D']-worst_scenario_region_mean_pct[model=='A'],D_minus_A_worst_national=worst_scenario_mean_pct[model=='D']-worst_scenario_mean_pct[model=='A'],D_minus_A_mean= scenario_mean_pct[model=='D']-scenario_mean_pct[model=='A']),by=.(modality,fleet)]
  write(tab,P('10_Tables','phase11C2_scenario_draw_summary.csv'));write(reg,P('10_Tables','phase11C2_regional_means.csv'));write(pairs,P('10_Tables','phase11C2_paired_scenario_differences.csv'));write(aggregate,P('10_Tables','phase11C2_extended_model_summary.csv'));write(cross,P('10_Tables','phase11C2_paired_aggregate_differences.csv'));write(ranks,P('10_Tables','phase11C2_model_ranking.csv'));write(aud,P('03_Data_Audit','phase11C2_reconciliation.csv'))
  stopifnot(nrow(tab)==ns*J,nrow(reg)==ns*J*R,nrow(pairs)==ns*3L*5L*3L*2L,nrow(aggregate)==J,nrow(ranks)==15L,sum(!is.na(aud$original_national_error))==9L,all(aud$minimum_gain>=-1e-7))
  fig<-function(n,f){for(ext in c('png','pdf')){p<-P('09_Figures',paste0(n,'.',ext));if(ext=='png')png(p,width=2200,height=1500,res=200)else pdf(p,width=11,height=7.5);tryCatch(f(),finally=dev.off());paths<<-c(paths,p)}}
  colors<-c(A='#666666',B='#D95F02',C='#2166AC',D='#1B9E77')
  fig('Figure_46_Extended_Robust_Equity',function(){par(mfrow=c(1,3),mar=c(5,4.5,3,1));for(m in mods){q<-aggregate[modality==m];plot(NA,xlim=range(q$fleet),ylim=c(0,100),xlab='Fleet size',ylab='Worst scenario-region mean (%)',main=m);for(h in names(colors)){z<-q[model==h];setorder(z,fleet);lines(z$fleet,z$worst_scenario_region_mean_pct,type='b',col=colors[h],pch=match(h,names(colors))+14,lwd=2)};legend('bottomright',names(colors),col=colors,lty=1,pch=15:18,bty='n')}})
  fig('Figure_47_Paired_Xray_Differences',function(){q<-pairs[modality=='XRAY' & fleet==32 & comparison=='D-A' & metric=='NATIONAL'];q<-merge(q,design[,.(scenario_id,speed)],by='scenario_id');setorder(q,speed,scenario_id);par(mar=c(5,5,3,1));plot(seq_len(nrow(q)),q$mean,ylim=range(c(q$p025,q$p975,0)),pch=19,cex=.55,col=match(q$speed,speeds)+1,xlab='Stress scenario (grouped by speed)',ylab='Paired D minus A (percentage points)',main='32 X-ray units: paired simulation differences');segments(seq_len(nrow(q)),q$p025,seq_len(nrow(q)),q$p975,col=adjustcolor('grey45',.6));points(seq_len(nrow(q)),q$mean,pch=19,cex=.55,col=match(q$speed,speeds)+1);abline(h=0,lty=2);legend('topright',speeds,col=2:4,pch=19,bty='n')})
  gate<-data.table(check=c('Prior manifests unchanged','Original training and test indices disjoint','Synthetic draw-level reference','60 frozen allocations retained','115 baseline ensembles and 145 stress configurations','All scenario draw counts','Nine original national means reproduced','Nine original regional means reproduced','No negative coverage gains','Complete paired summaries','All checkpoints exist'),passed=c(all(hash(P(prior$relative_path))==prior$SHA256),length(intersect(trainids,testids))==0L,TRUE,J==60L,nrow(design)==145L&&sum(design$speed=='baseline')==115L,sum(aud$draws)==43000L,all(aud$original_national_error[!is.na(aud$original_national_error)]<1e-7),all(aud$original_regional_error[!is.na(aud$original_regional_error)]<1e-7),all(aud$minimum_gain>=-1e-7),nrow(pairs)==ns*90L,all(file.exists(file.path(ck,sprintf('scenario_%03d.rds',seq_len(ns)))))))
  print(gate);stopifnot(all(gate$passed));write(gate,P('03_Data_Audit','phase11C2_quality_gate.csv'))
  write(data.table(item=c('Frozen allocations','Design','Draw selection','Paired intervals','Tail metric','Scenario weighting','Scope'),treatment=c('No refitting; original Phase11B sites only','All 115 baseline ensembles plus original and systematically selected speed stress combinations','Original nine use 1000 test draws; other scenarios use first 250 sorted test indices; shared 250 used for cross-scenario draw aggregates','Empirical simulation percentiles and MCSE, not empirical population confidence intervals; dependence across scenario configurations retained','Lower 5 percent mean of simulated coverage; minimum region per draw differs from minimum region expected coverage','Equal weight per tested scenario; not estimated real-world probabilities; design intentionally not full Cartesian product','Core road network, 60 minutes, hypothetical unlimited mobile service; documentary-only additions and physical feasibility not modelled')),P('12_Reproducibility','phase11C2_modelling_assumptions.csv'))
  spath<-P('13_R_Scripts','phase11C2_complete.R');writeLines(c('phase11C2_run <-',deparse(phase11C2_run,width.cutoff=500L),'phase11C2_results <- phase11C2_run()'),spath);paths<-c(paths,spath);sess<-file.path(out,'session_info.txt');writeLines(capture.output(sessionInfo()),sess);paths<-c(paths,sess)
  paths<-sort(unique(normalizePath(paths,winslash='/',mustWork=TRUE)));man<-data.table(relative_path=substring(paths,nchar(root)+2),SHA256=hash(paths),size_bytes=file.info(paths)$size);fwrite(man,P('12_Reproducibility','phase11C2_output_checksum_manifest.csv'));man[,verified:=hash(P(relative_path))==SHA256];stopifnot(all(man$verified));fwrite(man,P('12_Reproducibility','phase11C2_checksum_recheck.csv'))
  print(aggregate[fleet==32],nrows=12);print(ranks,nrows=15);cat('PHASE 11C2 EXTENDED STRESS TEST PASS\nScenarios:',ns,'\nScenario draws:',sum(aud$draws),'\nFrozen allocation draw evaluations:',sum(aud$draws)*J,'\nChecksums:',sum(man$verified),'/',nrow(man),'\nElapsed seconds:',as.numeric(difftime(Sys.time(),started,units='secs')),'\nSIMULATED STRESS TEST ONLY; NO REFITTING\n');invisible(aggregate)
}
phase11C2_results<-phase11C2_run()
