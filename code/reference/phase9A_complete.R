# Ghana diagnostic imaging research: Phase 9A
# Standalone nearest-vertex matching; no modification of earlier files.
phase9A_run <- function() {
  t0 <- Sys.time()
  suppressPackageStartupMessages({library(data.table); library(sf); library(digest)})
  if (!requireNamespace('RANN', quietly=TRUE)) install.packages('RANN', repos='https://cloud.r-project.org', type='binary')
  stopifnot(requireNamespace('RANN', quietly=TRUE))
  root <- normalizePath(file.path(Sys.getenv('USERPROFILE'),'Downloads','AI PREDICTION','Ghana_Diagnostic_Imaging_Research'), winslash='/', mustWork=TRUE)
  P <- function(...) file.path(root,...)
  hash <- function(p) vapply(p,function(x) digest(file=x,algo='sha256'),character(1),USE.NAMES=FALSE)
  phases <- c('phase7B','phase6B2','phase6A','phase8D','phase8C2','phase8B2')
  manifests <- lapply(phases,function(z) fread(P('12_Reproducibility',paste0(z,'_output_checksum_manifest.csv'))))
  for (i in seq_along(phases)) {
    m <- manifests[[i]]; stopifnot(all(c('relative_path','SHA256') %in% names(m)),all(file.exists(P(m$relative_path))),identical(hash(P(m$relative_path)),as.character(m$SHA256)))
    cat(phases[i],':',nrow(m),'checksums PASS\n')
  }
  out <- P('04_Spatial_Analysis','National_Road_Network','Location_Snapping'); dir.create(out,recursive=TRUE,showWarnings=FALSE)
  gd <- P('04_Spatial_Analysis','National_Road_Network','Directed_Graphs')
  dd <- P('04_Spatial_Analysis','Validated_Demand_Locations')
  country <- st_read(P('04_Spatial_Analysis','Administrative_Boundaries','gha_admin0.geojson'),quiet=TRUE)
  country <- st_union(st_geometry(st_make_valid(st_transform(country,32630))))
  resolutions <- c('1km','2km','5km'); scenarios <- c('CORE_PRIMARY','EXPANDED','CORE_STRICT_BARRIERS'); thresholds <- c(250,500,1000,2000,5000)
  grids <- setNames(lapply(resolutions,function(r) {
    d <- as.data.table(readRDS(file.path(dd,paste0('ghana_validated_demand_',r,'.rds'))))
    stopifnot(all(c('demand_id','population','district_index','region','routing_longitude','routing_latitude','routing_geography_ready') %in% names(d)),!anyDuplicated(d$demand_id),all(is.finite(d$population)&d$population>0))
    d[, eligible := !is.na(district_index)&district_index>0 & routing_geography_ready %in% TRUE & is.finite(routing_longitude)&is.finite(routing_latitude)]
    d[, region_group := fifelse(is.na(region)|district_index==0,'UNASSIGNED',as.character(region))]
    d
  }),resolutions)
  stopifnot(identical(unname(vapply(grids,nrow,integer(1))),c(156615L,51014L,11546L)),sum(grids[['2km']]$eligible)==50592,all(vapply(grids,function(d) abs(sum(d$population)-34746370)<1,logical(1))))
  # Use union of all mapped facilities, including out-of-frame documentary leads.
  fac <- as.data.table(readRDS(P('02_Processed_Data','phase6A_integrated_network_scenarios.rds')))
  stopifnot(all(c('id','scenario','longitude','latitude','calibration_eligible') %in% names(fac)))
  fac[,id:=as.character(id)]
  f <- unique(fac[,.(id,longitude,latitude)])
  stopifnot(!anyDuplicated(f$id),nrow(f)==2672L)
  eligible_ids <- unique(fac[calibration_eligible %in% TRUE,id])
  f[, supply_candidate := id %chin% eligible_ids]
  leads <- fread(P('03_Data_Audit','phase6A_osm_documentary_imaging_leads.csv'),colClasses=list(character='id'))
  f[,documentary_lead := id %chin% leads$id]
  stopifnot(all(leads$id %chin% f$id))
  f[, eligible := is.finite(longitude)&is.finite(latitude)]
  project <- function(lon,lat) sf_project('EPSG:4326','EPSG:32630',cbind(lon,lat),authority_compliant=FALSE)
  fxy <- project(f$longitude[f$eligible],f$latitude[f$eligible]); fp <- which(f$eligible)
  f[fp,eligible:=lengths(st_intersects(st_sfc(lapply(seq_len(nrow(fxy)),function(i) st_point(fxy[i,])),crs=32630),country))>0]
  # Exact Euclidean nearest vertex in the specified projection. No largest-component restriction.
  snap <- function(d, v, xy, lon, lat) {
    z <- copy(d); z[,`:=`(vertex_id=NA_integer_,node_id=NA_character_,connector_m=NA_real_,snap_longitude=NA_real_,snap_latitude=NA_real_,weak_component=NA_integer_,strong_component=NA_integer_,largest_weak_component=NA,largest_strong_component=NA,connector_inside_country=NA)]
    ix <- which(z$eligible); if (!length(ix)) return(z)
    q <- project(lon[ix],lat[ix]); stopifnot(all(is.finite(q)))
    nn <- RANN::nn2(xy,q,k=1,eps=0); j <- as.integer(nn$nn.idx[,1]); dist <- as.numeric(nn$nn.dists[,1]); stopifnot(all(j>0),all(is.finite(dist)),max(abs(dist-sqrt(rowSums((q-xy[j,,drop=FALSE])^2))))<1e-5)
    z[ix,`:=`(vertex_id=as.integer(v$vertex_id[j]),node_id=as.character(v$node_id[j]),connector_m=dist,snap_longitude=v$longitude[j],snap_latitude=v$latitude[j],weak_component=as.integer(v$weak_component[j]),strong_component=as.integer(v$strong_component[j]),largest_weak_component=v$largest_weak_component[j],largest_strong_component=v$largest_strong_component[j])]
    # Audit straight connectors within 5km only. Longer matches are never accepted.
    near <- which(dist<=max(thresholds)); zero <- near[dist[near]==0]; if(length(zero)) z[ix[zero],connector_inside_country:=TRUE]
    nz <- near[dist[near]>0]
    for (b in split(nz, ceiling(seq_along(nz)/10000))) {
      if(!length(b)) next
      lines <- st_sfc(lapply(b,function(k) st_linestring(rbind(q[k,],xy[j[k],]))),crs=32630)
      z[ix[b],connector_inside_country:=lengths(st_covered_by(lines,country))>0]
    }
    z[, snap_status := fcase(!eligible,'GEOGRAPHY_NOT_READY',is.na(connector_m),'NO_MATCH',connector_m>max(thresholds),'BEYOND_5KM',!connector_inside_country %in% TRUE,'CONNECTOR_OUTSIDE_POLYGON',default='WITHIN_5KM')]
    z
  }
  files <- character(); coverage <- list(); regions <- list(); facility_summary <- list(); distance_summary <- list(); checks <- list()
  save <- function(x,n) {p <- file.path(out,n); saveRDS(x,p); files <<- c(files,p); p}
  for (s in scenarios) {
    cat('\nPROCESSING',s,'\n'); v <- as.data.table(readRDS(file.path(gd,paste0(s,'_vertices.rds')))); stopifnot(!anyDuplicated(v$vertex_id),all(v$inside_country),identical(as.integer(v$vertex_id),seq_len(nrow(v))))
    xy <- as.matrix(v[,.(easting,northing)]); stopifnot(all(is.finite(xy)))
    # Brute-force reference on small deterministic sample: validates nearest-neighbor search implementation.
    set.seed(20261125); qtest <- xy[sample.int(nrow(xy),5),,drop=FALSE]+matrix(runif(10,-100,100),5)
    test <- RANN::nn2(xy,qtest,k=1,eps=0)$nn.dists[,1]
    ref <- vapply(1:5,function(k) sqrt(min((xy[,1]-qtest[k,1])^2+(xy[,2]-qtest[k,2])^2)),numeric(1)); stopifnot(max(abs(test-ref))<1e-6)
    for (r in resolutions) {
      d <- grids[[r]]; z <- snap(d,v,xy,d$routing_longitude,d$routing_latitude); z[,network:=s]
      save(z,paste0(s,'_demand_',r,'_snapped.rds'))
      stopifnot(nrow(z)==nrow(d),identical(z$demand_id,d$demand_id),identical(z$population,d$population),all(is.na(z$vertex_id[!z$eligible])))
      for (h in thresholds) {
        z[, accepted := eligible & !is.na(connector_m) & connector_m<=h & connector_inside_country %in% TRUE]
        a <- z[,.(locations=.N,population=sum(population),accepted_locations=sum(accepted),accepted_population=sum(population[accepted]),geography_pending_population=sum(population[!eligible]),outside_largest_weak_population=sum(population[accepted & largest_weak_component %in% FALSE]))]
        a[,`:=`(network=s,resolution=r,threshold_m=h,coverage_pct=100*accepted_population/population)]
        coverage[[length(coverage)+1L]] <- a
        b <- z[,.(locations=.N,population=sum(population),accepted_locations=sum(accepted),accepted_population=sum(population[accepted]),geography_pending_population=sum(population[!eligible]),outside_largest_weak_population=sum(population[accepted & largest_weak_component %in% FALSE])),by=region_group]
        b[,`:=`(network=s,resolution=r,threshold_m=h,coverage_pct=100*accepted_population/population)]
        regions[[length(regions)+1L]] <- b
      }
      distance_summary[[length(distance_summary)+1L]] <- z[eligible==TRUE,.(network=s,resolution=r,median_m=median(connector_m),p90_m=as.numeric(quantile(connector_m,.90)),p95_m=as.numeric(quantile(connector_m,.95)),maximum_m=max(connector_m))]
      cat(r,':',nrow(z),'locations preserved\n'); rm(z); gc()
    }
    ff <- snap(f,v,xy,f$longitude,f$latitude); ff[,network:=s]; save(ff,paste0(s,'_facilities_snapped.rds'))
    stopifnot(nrow(ff)==2672L,identical(ff$id,f$id))
    for (h in thresholds) {
      ff[, accepted := eligible & !is.na(connector_m)&connector_m<=h&connector_inside_country %in% TRUE]
      facility_summary[[length(facility_summary)+1L]] <- ff[,.(network=s,threshold_m=h,all_mapped=.N,all_accepted=sum(accepted),supply_candidates=sum(supply_candidate),supply_accepted=sum(supply_candidate&accepted),documentary_leads=sum(documentary_lead),documentary_accepted=sum(documentary_lead&accepted))]
    }
    checks[[s]] <- data.table(network=s,nearest_reference_pass=TRUE,all_demand_rows_preserved=TRUE,all_facility_rows_preserved=TRUE)
    rm(v,xy,ff); gc()
  }
  cov <- rbindlist(coverage); reg <- rbindlist(regions); fs <- rbindlist(facility_summary); ds <- rbindlist(distance_summary)
  setorder(cov,network,resolution,threshold_m); setorder(reg,network,resolution,region_group,threshold_m)
  stopifnot(nrow(cov)==45,nrow(fs)==15,all(cov$accepted_population<=cov$population+1e-6),all(cov$accepted_population+cov$geography_pending_population<=cov$population+1e-6),all(cov[,all(diff(accepted_population)>=-1e-6),by=.(network,resolution)]$V1),all(reg[,all(diff(accepted_population)>=-1e-6),by=.(network,resolution,region_group)]$V1))
  rr <- reg[,.(regional_total=sum(population),regional_accepted=sum(accepted_population)),by=.(network,resolution,threshold_m)]; rr <- merge(rr,cov,by=c('network','resolution','threshold_m')); stopifnot(max(abs(rr$regional_total-rr$population))<1e-5,max(abs(rr$regional_accepted-rr$accepted_population))<1e-5)
  write <- function(x,dir,n) {p <- P(dir,n); fwrite(x,p); files <<- c(files,p)}
  write(cov,'10_Tables','phase9A_population_coverage.csv'); write(reg,'10_Tables','phase9A_regional_coverage.csv'); write(fs,'10_Tables','phase9A_facility_coverage.csv'); write(ds,'10_Tables','phase9A_connector_distance_summary.csv'); write(rbindlist(checks),'03_Data_Audit','phase9A_network_checks.csv')
  assumptions <- data.table(item=c('Matching','Connector','Population denominator','Facility candidates','Thresholds','Road component','Travel time'),treatment=c('Exact nearest vertex in EPSG32630; not nearest segment','Straight projected distance and national polygon coverage only; actual path and water crossing unverified','All grid population retained including administrative/geographic uncertainty','All mapped facilities retained; supply eligibility and documentary flags separate; no operational verification','250 500 1000 2000 5000 metres; no final threshold selected','All active graph components retained','Not calculated in this phase'))
  write(assumptions,'12_Reproducibility','phase9A_modelling_assumptions.csv')
  plot_save <- function(stem,draw,w=11,h=7) {for(ext in c('png','pdf')) {p <- P('09_Figures',paste0(stem,'.',ext)); if(ext=='png') png(p,width=w,height=h,units='in',res=300,bg='white') else pdf(p,width=w,height=h); tryCatch(draw(),finally=dev.off()); files <<- c(files,p)}}
  cols <- c('#2166AC','#16866B','#D95F02'); labs <- c('Core','Expanded','Strict barriers')
  plot_save('Figure_31_Population_Connector_Coverage',function(){par(mar=c(5,5,4,2)); plot(NA,xlim=range(thresholds),ylim=c(0,100),log='x',xaxt='n',xlab='Maximum connector distance (m)',ylab='National population matched (%)',main='Population matching sensitivity: approximately 2 km grid'); axis(1,at=thresholds,labels=thresholds); for(i in seq_along(scenarios)){z<-cov[network==scenarios[i]&resolution=='2km']; lines(z$threshold_m,z$coverage_pct,type='b',col=cols[i],pch=i,lwd=2)}; legend('bottomright',labs,col=cols,pch=1:3,lwd=2,bty='n')})
  plot_save('Figure_32_Regional_Connector_Coverage',function(){z<-reg[network=='CORE_PRIMARY'&resolution=='2km'&threshold_m==1000&region_group!='UNASSIGNED']; setorder(z,coverage_pct); par(mar=c(5,12,4,2),las=1); bp<-barplot(z$coverage_pct,names.arg=z$region_group,horiz=TRUE,xlim=c(0,110),col='#2166AC',border=NA,xlab='Regional population matched (%)',main='Core network: 1 km connector threshold'); text(z$coverage_pct,bp,sprintf('%.1f',z$coverage_pct),pos=4,cex=.75)})
  plot_save('Figure_33_Facility_Connector_Coverage',function(){par(mar=c(5,5,4,2)); plot(NA,xlim=range(thresholds),ylim=c(0,100),log='x',xaxt='n',xlab='Maximum connector distance (m)',ylab='Candidate facilities matched (%)',main='Imaging supply candidate matching sensitivity'); axis(1,at=thresholds,labels=thresholds); for(i in seq_along(scenarios)){z<-fs[network==scenarios[i]]; lines(z$threshold_m,100*z$supply_accepted/z$supply_candidates,type='b',col=cols[i],pch=i,lwd=2)}; legend('bottomright',labs,col=cols,pch=1:3,lwd=2,bty='n')})
  for(i in seq_along(phases)) stopifnot(identical(hash(P(manifests[[i]]$relative_path)),as.character(manifests[[i]]$SHA256)))
  gate <- data.table(check=c('Previous manifests verified unchanged','Population totals conserved','Three networks and resolutions','Nearest neighbor reference tests','All population and facility rows preserved','Threshold monotonicity','Regional reconciliation','Twelve RDS outputs','Six figures'),passed=c(TRUE,TRUE,nrow(cov)==45,all(rbindlist(checks)$nearest_reference_pass),TRUE,TRUE,TRUE,sum(grepl('\\.rds$',files))==12,sum(grepl('\\.(png|pdf)$',files))==6)); stopifnot(all(gate$passed)); write(gate,'03_Data_Audit','phase9A_quality_gate.csv')
  session <- P('12_Reproducibility','phase9A_R_session_info.txt'); writeLines(capture.output(sessionInfo()),session); files <- c(files,session)
  script <- P('13_R_Scripts','phase9A_complete.R'); code <- c('phase9A_run <-',deparse(phase9A_run,width.cutoff=500L),'phase9A_results <- phase9A_run()'); parse(text=code); writeLines(code,script); files <- c(files,script)
  files <- normalizePath(unique(files),winslash='/',mustWork=TRUE); m <- data.table(relative_path=substring(files,nchar(root)+2),SHA256=hash(files),size_bytes=file.info(files)$size); stopifnot(all(m$size_bytes>0)); fwrite(m,P('12_Reproducibility','phase9A_output_checksum_manifest.csv')); m[,verified:=hash(P(relative_path))==SHA256]; stopifnot(all(m$verified)); fwrite(m,P('12_Reproducibility','phase9A_checksum_recheck.csv'))
  print(cov[resolution=='2km']); print(fs); print(ds); print(gate); cat('\nPHASE 9A MATCHING AUDIT PASS\nChecksums:',sum(m$verified),'/',nrow(m),'\nElapsed seconds:',as.numeric(difftime(Sys.time(),t0,units='secs')),'\nTRAVEL TIMES NOT YET CALCULATED\n'); invisible(list(coverage=cov,regional=reg,facilities=fs,distances=ds,checks=gate,manifest=m))
}
phase9A_results <- phase9A_run()
