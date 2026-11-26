# Phase 11C1: solver benchmarks, no refitting on held-out draws.
phase11C1B_run <- function(){
  suppressPackageStartupMessages({library(data.table);library(Matrix);library(digest)})
  if(!requireNamespace('highs',quietly=TRUE)) install.packages('highs',repos='https://cloud.r-project.org',type='binary')
  stopifnot(requireNamespace('highs',quietly=TRUE))
  started<-Sys.time();root<-normalizePath(file.path(Sys.getenv('USERPROFILE'),'Downloads','AI PREDICTION','Ghana_Diagnostic_Imaging_Research'),winslash='/',mustWork=TRUE)
  P<-function(...)file.path(root,...);hash<-function(p)vapply(p,function(x)digest(file=x,algo='sha256'),character(1),USE.NAMES=FALSE)
  old<-rbindlist(lapply(c('10A','10B','10C','11A','11B','11C1'),function(s){a<-fread(P('12_Reproducibility',paste0('phase',s,'_output_checksum_manifest.csv')));stopifnot(all(hash(P(a$relative_path))==a$SHA256));cat('Phase',s,':',nrow(a),'checksums PASS\n');a[,.(relative_path,SHA256)]}))
  out<-P('06_Optimization','Phase11C1B');dir.create(out,recursive=TRUE,showWarnings=FALSE);paths<-character()
  write<-function(x,p){fwrite(x,p);paths<<-c(paths,p)}
  bdir<-P('06_Optimization','Phase11B');a<-readRDS(file.path(bdir,'phase11B_results.rds'));probs<-readRDS(file.path(bdir,'scenario_probabilities.rds'))
  base<-P('05_Travel_Time_Matrices','Phase10C_Mobile');idx<-readRDS(file.path(base,'mobile_matrix_index.rds'));d<-as.data.table(idx$demand);cands<-as.data.table(idx$candidates)
  pop<-as.numeric(d$population);N<-sum(pop);regions<-sort(unique(d$region_group[d$region_group!='UNASSIGNED']));rg<-match(d$region_group,regions);rp<-vapply(seq_along(regions),function(r)sum(pop[which(rg==r)]),numeric(1));stopifnot(length(regions)==16L,length(pop)==51014L,abs(N-34746370)<1,nrow(cands)==3581L,length(intersect(a$trainids,a$testids))==0,length(probs)==9L)
  speeds<-c('baseline','slow','favorable');B<-lapply(speeds,function(s){b<-readRDS(file.path(base,toupper(s),'coverage_matrices.rds'))[['60']];stopifnot(identical(rownames(b),as.character(d$demand_id)),identical(colnames(b),as.character(cands$candidate_id)),all(Matrix::rowSums(b)[!d$match_1km]==0));b});names(B)<-speeds
  keep<-which(Matrix::colSums(B$baseline)>0);keep<-keep[order(cands$candidate_id[keep])];stopifnot(length(keep)==3571L);B<-lapply(B,function(b)b[,keep,drop=FALSE]);ids<-as.character(cands$candidate_id[keep]);des<-as.data.table(a$design);stopifnot(identical(as.integer(des$scenario_id),1:9),identical(as.character(des$speed),rep(speeds,each=3)))
  mods<-c('XRAY','ULTRASOUND','COLOCATED');ans<-as.data.table(a$summary);sel<-as.data.table(a$selected)
  evalset<-function(j,pp,ss,l,g){n<-numeric(length(ss));rr<-matrix(0,16,length(ss));for(t in seq_along(ss)){s<-ss[t];hit<-if(length(j)) Matrix::rowSums(B[[des$speed[s]]][,j,drop=FALSE])>0 else rep(FALSE,length(pop));post<-pp[,s]+(1-pp[,s])*hit;n[t]<-sum(pop*post)/N;rr[,t]<-vapply(1:16,function(r){ii<-which(rg==r);sum(pop[ii]*post[ii])/rp[r]},numeric(1))};list(obj=(1-l-g)*mean(n)+l*min(rr)+g*min(n),mean=mean(n),worst=min(n),equity=min(rr))}
  # Shared coverage variables for scenarios with identical speed. Continuous y is exact for binary x because all coverage coefficients are nonnegative.
  build<-function(pool,pp,ss,l,g,K){
    J<-length(pool);sp<-unique(as.character(des$speed[ss]));br<-lapply(sp,function(s){b<-B[[s]][,pool,drop=FALSE];ii<-which(Matrix::rowSums(b)>0);list(i=ii,b=b[ii,,drop=FALSE])});names(br)<-sp
    ny<-vapply(br,function(z)length(z$i),integer(1));off<-J+c(0L,cumsum(ny));V<-J+sum(ny)+2L;ze<-V-1L;zn<-V
    ii<-list();jj<-list();vv<-list();rhs<-numeric();nr<-0L
    add<-function(i,j,v,r){ii[[length(ii)+1L]]<<-as.integer(i+nr);jj[[length(jj)+1L]]<<-as.integer(j);vv[[length(vv)+1L]]<<-as.numeric(v);rhs<<-c(rhs,r);nr<<-nr+length(r)}
    add(rep(1L,J),seq_len(J),rep(1,J),K)
    for(k in seq_along(br)){z<-br[[k]];q<-summary(z$b);n<-ny[k];if(n){add(c(q$i,seq_len(n)),c(q$j,off[k]+seq_len(n)),c(rep(-1,nrow(q)),rep(1,n)),rep(0,n))}}
    L<-numeric(V);L[ze]<-l;L[zn]<-g;offset<-0
    for(s in ss){k<-match(des$speed[s],sp);ix<-br[[k]]$i;yi<-off[k]+seq_along(ix);w<-pop*(1-pp[,s]);bn<-sum(pop*pp[,s])/N;offset<-offset+(1-l-g)*bn/length(ss);L[yi]<-L[yi]+(1-l-g)*w[ix]/N/length(ss)
      if(g>0)add(rep(1L,length(yi)+1L),c(yi,zn),c(-w[ix]/N,1),bn)
      if(l>0)for(r in 1:16){ir<-which(rg[ix]==r);allr<-which(rg==r);baseline<-sum(pop[allr]*pp[allr,s])/rp[r];add(rep(1L,length(ir)+1L),c(yi[ir],ze),c(-w[ix[ir]]/rp[r],1),baseline)}
    }
    # Scale national/regional coverage constraints to avoid HiGHS dropping small population weights.
    A<-sparseMatrix(i=unlist(ii),j=unlist(jj),x=unlist(vv),dims=c(nr,V)); scale<-rep(1,nr); first_weighted<-1L+sum(ny); if(nr>first_weighted) scale[seq.int(first_weighted+1L,nr)]<-1e6
    A<-as(Diagonal(x=scale)%*%A,'CsparseMatrix'); rhs<-rhs*scale
    # Explicitly remove any remaining tiny terms; enlarge RHS by the maximum contribution of omitted negative terms (0 <= variables <= 1).
    qq<-summary(A); tiny<-which(abs(qq$x)<=1e-9 & qq$x!=0); omitted<-length(tiny); allowance<-0
    if(omitted){tt<-qq[tiny,,drop=FALSE]; adj<-rowsum(pmax(0,-tt$x),tt$i,reorder=FALSE); rr<-as.integer(rownames(adj));rhs[rr]<-rhs[rr]+as.numeric(adj);allowance<-sum(adj);qq$x[tiny]<-0; A<-drop0(sparseMatrix(i=qq$i,j=qq$j,x=qq$x,dims=c(nr,V)))}
    list(A=A,L=L,rhs=rhs,offset=offset,J=J,V=V,omitted=omitted,allowance=allowance)
  }
  solve<-function(z,mip=FALSE,limit=60){highs::highs_solve(L=z$L,lower=rep(0,z$V),upper=rep(1,z$V),A=z$A,lhs=rep(-Inf,nrow(z$A)),rhs=z$rhs,types=c(rep(if(mip)'I' else 'C',z$J),rep('C',z$V-z$J)),maximum=TRUE,offset=z$offset,control=highs::highs_control(threads=2L,time_limit=limit,log_to_console=FALSE,mip_rel_gap=0,mip_abs_gap=0,small_matrix_value=1e-12))}
  optimal<-function(s) identical(tolower(trimws(as.character(s$status_message))),'optimal')
  # Solver smoke test: exact 0-1 maximum with an independently known answer.
  toy<-highs::highs_solve(L=c(3,2),lower=c(0,0),upper=c(1,1),A=matrix(c(1,1),1),rhs=1,types=c('I','I'),maximum=TRUE);stopifnot(optimal(toy),abs(toy$objective_value-3)<1e-8)
  # Exhaustive evaluation via identical coverage-signature aggregation.
  F<-fread(P('10_Tables','phase11C1_full_lp_bounds.csv'))
  G<-fread(P('10_Tables','phase11C1_restricted_mip.csv'))
  saved<-fread(P('06_Optimization','Phase11C1','restricted_candidate_sets.csv'))
  stopifnot(nrow(F)==12L,nrow(G)==12L)
  if(!requireNamespace('Rcpp',quietly=TRUE)) stop('Rcpp required')
  rt<-'C:/rtools45'; if(dir.exists(rt)) Sys.setenv(PATH=paste(file.path(rt,'usr/bin'),file.path(rt,'x86_64-w64-mingw32.static.posix/bin'),Sys.getenv('PATH'),sep=';'))
  Rcpp::cppFunction(code='Rcpp::List enumerate4(Rcpp::IntegerVector masks, Rcpp::NumericMatrix W, Rcpp::NumericMatrix baseline, int J) {
    int S=baseline.ncol(), R=baseline.nrow(), M=masks.size();
    Rcpp::NumericVector best(4,-1.0); Rcpp::IntegerMatrix sites(4,4); int count=0;
    std::vector<double> z(R*S);
    for(int a=0;a<J-3;++a) for(int b=a+1;b<J-2;++b) for(int c=b+1;c<J-1;++c) for(int d=c+1;d<J;++d){
      unsigned int mask=(1u<<a)|(1u<<b)|(1u<<c)|(1u<<d); ++count;
      for(int s=0;s<S;++s) for(int r=0;r<R;++r) z[r+R*s]=baseline(r,s);
      for(int i=0;i<M;++i) if(mask & (unsigned int)masks[i]) for(int k=0;k<R*S;++k) z[k]+=W(i,k);
      double mean=0,worst=1,equity=1,primary=z[0],pr=1;
      for(int s=0;s<S;++s){mean+=z[R*s]/S; worst=std::min(worst,z[R*s]); for(int r=1;r<R;++r) equity=std::min(equity,z[r+R*s]);}
      for(int r=1;r<R;++r) pr=std::min(pr,z[r]);
      double obj[4]={primary,0.5*primary+0.5*pr,0.5*mean+0.5*worst,0.3*mean+0.4*equity+0.3*worst};
      for(int h=0;h<4;++h) if(obj[h]>best[h]){best[h]=obj[h];sites(h,0)=a+1;sites(h,1)=b+1;sites(h,2)=c+1;sites(h,3)=d+1;}
    }
    return Rcpp::List::create(Rcpp::Named("best")=best,Rcpp::Named("sites")=sites,Rcpp::Named("count")=count);
  }',env=environment())
  # Toy: one region, two scenarios, five sites, exactly four chosen.
  # Use a simple independently known case instead of relying on row ordering above.
  toyW<-matrix(rep(c(.01,.02,.03,.04,.05),4),5,4)
  toyE<-enumerate4(as.integer(2^(0:4)),toyW,matrix(0,2,2),5L)
  stopifnot(toyE$count==5L,all(abs(toyE$best-.14)<1e-12),all(apply(toyE$sites,1,function(x)identical(as.integer(x),2:5))))
  results<-list(); selections<-list(); retries<-list(); kk<-0L
  for(m in seq_along(mods)){
    mod<-mods[m]; tr<-do.call(cbind,lapply(probs,function(v)v$train[,m]));stopifnot(all(is.finite(tr)),all(tr>=0&tr<=1))
    pool<-sort(unique(match(saved[modality==mod & method=='POOL',candidate_id],ids)))
    expected<-c(24L,26L,27L)[m];stopifnot(!anyNA(pool),length(pool)==expected)
    for(h in c('A','B','C','D')) stopifnot(setequal(ids[pool],saved[modality==mod & model==h & method=='POOL',candidate_id]))
    # A mask represents the set of sites covering a demand row at one speed.
    # Aggregate gains by mask across scenario and region, retaining full population denominators.
    baseline<-matrix(0,17,9); agg<-list()
    for(sid in 1:9){
      b<-B[[des$speed[sid]]][,pool,drop=FALSE]; mask<-as.integer(b %*% 2^(seq_along(pool)-1L));stopifnot(!anyNA(mask),all(mask>=0))
      residual<-pop*(1-tr[,sid]); baseline[1,sid]<-sum(pop*tr[,sid])/N
      for(r in 1:16){ir<-which(rg==r);baseline[r+1,sid]<-sum(pop[ir]*tr[ir,sid])/rp[r]}
      ii<-which(mask!=0L); dt<-data.table(mask=mask[ii],region=rg[ii],gain=residual[ii]);
      n<-dt[,.(gain=sum(gain)/N),by=mask];n[,col:=17L*(sid-1L)+1L]
      rr<-dt[!is.na(region),.(gain=sum(gain)/rp[region[1L]]),by=.(mask,region)];rr[,col:=17L*(sid-1L)+region+1L];rr[,region:=NULL]
      agg[[sid]]<-rbind(n,rr,use.names=TRUE)
    }
    ag<-rbindlist(agg); um<-sort(unique(ag$mask)); W<-matrix(0,length(um),153); W[cbind(match(ag$mask,um),ag$col)]<-ag$gain
    cat('Enumerating',mod,'pool',length(pool),'combinations',choose(length(pool),4),'signatures',length(um),'\n');flush.console()
    e<-enumerate4(um,W,baseline,length(pool));stopifnot(e$count==choose(length(pool),4))
    for(hh in 1:4){h<-LETTERS[hh];kk<-kk+1L;q<-ans[modality==mod & model==h & fleet==32];ss<-if(hh<=2)1L else 1:9;l<-q$lambda;g<-q$gamma;stopifnot(abs(l-c(0,.5,0,.4)[hh])<1e-12,abs(g-c(0,0,.5,.3)[hh])<1e-12)
      j<-pool[e$sites[hh,]]; v<-evalset(j,tr,ss,l,g)$obj;stopifnot(abs(v-e$best[hh])<1e-9,length(unique(j))==4L)
      prev<-G[modality==mod & model==h];gj<-match(saved[modality==mod & model==h & method=='GREEDY',candidate_id],ids);mj<-match(saved[modality==mod & model==h & method=='MIP',candidate_id],ids)
      gv<-evalset(gj,tr,ss,l,g)$obj;mv<-if(length(mj))evalset(mj,tr,ss,l,g)$obj else NA_real_
      stopifnot(abs(gv-prev$greedy)<1e-8,v>=gv-1e-9,is.na(mv)||v>=mv-1e-9)
      results[[kk]]<-data.table(modality=mod,model=h,pool_size=length(pool),fleet=4L,combinations=e$count,exhaustive_optimum=v,greedy=gv,mip_recalculated=mv,greedy_gap=v-gv,mip_gap=v-mv,objective_recheck=abs(v-e$best[hh]))
      selections[[kk]]<-data.table(modality=mod,model=h,candidate_id=ids[j]);cat(h,' optimum:',v,' greedy gap:',v-gv,' MIP gap:',v-mv,'\n')
    }
    if(mod %in% c('XRAY','COLOCATED')){
      q<-ans[modality==mod & model=='D' & fleet==32]; prev<-F[modality==mod & model=='D'];j<-match(sel[run_id==q$run_id,candidate_id],ids);v<-evalset(j,tr,1:9,q$lambda,q$gamma)$obj;stopifnot(abs(v-prev$heuristic)<1e-8)
      cat('Targeted LP retry:',mod,'D\n');flush.console();z<-build(seq_along(ids),tr,1:9,q$lambda,q$gamma,32L)
      # Try IPM with crossover disabled. Retain previous ceiling on any failure.
      sol<-tryCatch(highs::highs_solve(L=z$L,lower=rep(0,z$V),upper=rep(1,z$V),A=z$A,lhs=rep(-Inf,nrow(z$A)),rhs=z$rhs,types=rep('C',z$V),maximum=TRUE,offset=z$offset,control=highs::highs_control(threads=2L,time_limit=300,solver='ipm',run_crossover='off',log_to_console=FALSE,small_matrix_value=1e-12)),error=function(e)list(status_message=conditionMessage(e)))
      raw<-if(optimal(sol)&&length(sol$objective_value)==1L)as.numeric(sol$objective_value) else NA_real_; accepted<-is.finite(raw)&&raw>=v-1e-8; upper<-if(accepted)min(prev$valid_upper,raw+1e-5) else prev$valid_upper
      retries[[mod]]<-data.table(modality=mod,model='D',status=as.character(sol$status_message),raw_lp=raw,accepted=accepted,previous_upper=prev$valid_upper,retained_upper=upper,heuristic=v,gap=upper-v)
      print(retries[[mod]]);rm(z,sol);gc()
    }
  }
  E<-rbindlist(results); T<-rbindlist(retries); S<-rbindlist(selections)
  stopifnot(nrow(E)==12L,nrow(T)==2L,nrow(S)==48L,all(E$objective_recheck<1e-9),all(T$retained_upper>=T$heuristic-1e-8),all(hash(P(old$relative_path))==old$SHA256))
  F[,`:=`(final_upper=valid_upper,final_gap=tightened_gap)];for(i in seq_len(nrow(T))) F[modality==T$modality[i]&model=='D',`:=`(final_upper=T$retained_upper[i],final_gap=T$gap[i])]
  write(E,P('10_Tables','phase11C1B_exhaustive_restricted.csv'));write(T,P('10_Tables','phase11C1B_lp_retries.csv'));write(F,P('10_Tables','phase11C1B_consolidated_bounds.csv'));write(S,file.path(out,'exhaustive_selected_sites.csv'))
  gate<-data.table(check=c('Prior manifests unchanged','Training split disjoint','Exhaustive toy reference','Original pools reproduced','All combinations evaluated','12 exhaustive optima reconciled','Greedy and MIP feasible objectives bounded','Two targeted LP attempts','Previous ten LP bounds preserved'),passed=c(TRUE,TRUE,TRUE,TRUE,all(E$combinations==choose(E$pool_size,4)),all(E$objective_recheck<1e-9),all(E$greedy_gap>=-1e-9)&all(is.na(E$mip_gap)|E$mip_gap>=-1e-9),nrow(T)==2L,all(F[!((modality %in% c('XRAY','COLOCATED'))&model=='D'),final_upper==valid_upper])))
  stopifnot(all(gate$passed));write(gate,P('03_Data_Audit','phase11C1B_quality_gate.csv'))
  write(data.table(item=c('Enumeration','Exactness','LP','Scope'),treatment=c('All four-site subsets; monotone objectives imply at-most-four optimum attained with four; identical coverage masks aggregated','Floating-point exhaustive restricted optimum, not full national optimum; independent original objective reconciliation','300-second IPM attempts; retain earlier ceiling if no accepted optimal result; numerical allowance 1e-5','Training only; original allocations unchanged')),P('12_Reproducibility','phase11C1B_modelling_assumptions.csv'))
  p<-file.path(out,'session_info.txt');writeLines(capture.output(sessionInfo()),p);paths<-c(paths,p)
  p<-P('13_R_Scripts','phase11C1B_complete.R');writeLines(c('phase11C1B_run <-',deparse(phase11C1B_run,width.cutoff=500L),'phase11C1B_results <- phase11C1B_run()'),p);paths<-c(paths,p)
  paths<-sort(unique(normalizePath(paths,winslash='/',mustWork=TRUE)));man<-data.table(relative_path=substring(paths,nchar(root)+2),SHA256=hash(paths),size_bytes=file.info(paths)$size);fwrite(man,P('12_Reproducibility','phase11C1B_output_checksum_manifest.csv'));man[,verified:=hash(P(relative_path))==SHA256];stopifnot(all(man$verified));fwrite(man,P('12_Reproducibility','phase11C1B_checksum_recheck.csv'))
  print(E);print(T);print(F);print(gate);cat('PHASE 11C1B COMPLETE\nRestricted optima:',nrow(E),' / 12\nUnique combinations:',sum(E[model=='A',combinations]),'\nAccepted LP retries:',sum(T$accepted),' / 2\nChecksums:',sum(man$verified),'/',nrow(man),'\nElapsed seconds:',as.numeric(difftime(Sys.time(),started,units='secs')),'\n');invisible(list(exhaustive=E,retries=T,bounds=F))
}
phase11C1B_results<-phase11C1B_run()
