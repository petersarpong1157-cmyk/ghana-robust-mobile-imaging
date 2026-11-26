phase11B_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(Matrix)
        library(Rcpp)
        library(digest)
    })
    start <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    P <- function(...) file.path(root, ...)
    hash <- function(p) vapply(p, function(x) digest(file = x, algo = "sha256"), character(1), USE.NAMES = FALSE)
    old <- rbindlist(lapply(c("6B2", "9B1", "9B2", "10A", "10B", "10C", "11A"), function(s) {
        m <- fread(P("12_Reproducibility", paste0("phase", s, "_output_checksum_manifest.csv")))
        stopifnot(all(hash(P(m$relative_path)) == m$SHA256))
        cat("Phase", s, ":", nrow(m), "checksums PASS\n")
        m[, .(relative_path, SHA256)]
    }))
    out <- P("06_Optimization", "Phase11B")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    paths <- character()
    write <- function(x, p) {
        fwrite(x, p)
        paths <<- c(paths, p)
    }
    if (Sys.info()[["sysname"]] == "Windows") 
        Sys.setenv(PATH = paste("C:/rtools45/usr/bin", "C:/rtools45/x86_64-w64-mingw32.static.posix/bin", Sys.getenv("PATH"), sep = ";"), RTOOLS45_HOME = "C:/rtools45")
    cpp <- "\n#include <Rcpp.h>\n#include <algorithm>\n#include <vector>\n#include <cmath>\nusing namespace Rcpp;\n// [[Rcpp::export]]\nList prob_split(IntegerVector ptr,IntegerVector fac,LogicalMatrix X,LogicalMatrix U,IntegerVector train){\n int n=ptr.size()-1,S=X.ncol(); NumericMatrix a(n,3),b(n,3);\n for(int i=0;i<n;i++) for(int s=0;s<S;s++){\n  bool x=false,u=false,c=false;\n  for(int e=ptr[i];e<ptr[i+1];e++){int j=fac[e]-1;bool xx=X(j,s),uu=U(j,s);x|=xx;u|=uu;c|=(xx&&uu);if(c)break;}\n  if(train[s]){a(i,0)+=x;a(i,1)+=u;a(i,2)+=c;}else{b(i,0)+=x;b(i,1)+=u;b(i,2)+=c;}\n }\n return List::create(_[\"train\"]=a,_[\"test\"]=b);\n}\n// [[Rcpp::export]]\nList optimize_robust(List cpL,List ciL,List rpL,List riL,NumericMatrix W,IntegerVector reg,NumericMatrix base,double lambda,double gamma,int K,IntegerVector initial,int exchanges){\n int S=W.ncol(),n=W.nrow(),R=base.nrow()-1,J=as<IntegerVector>(cpL[0]).size()-1,D=S*(R+1);\n std::vector<IntegerVector> cp,ci,rp,ri;for(int s=0;s<S;s++){cp.push_back(as<IntegerVector>(cpL[s]));ci.push_back(as<IntegerVector>(ciL[s]));rp.push_back(as<IntegerVector>(rpL[s]));ri.push_back(as<IntegerVector>(riL[s]));}\n // W has raw population residuals; base rows contain raw covered population, denominators are separate below.\n // The last row of W is not special; region 0 is unassigned.\n // base is normalized in R; W is normalized to national population, regional gain normalized with region share.\n NumericVector share(R);for(int r=0;r<R;r++)share[r]=base(r,0); // column 0 contains regional shares\n std::vector<double> cur(D),g((size_t)J*D,0.0);std::vector<int> cnt((size_t)n*S,0),chosen(J,0),sel;\n for(int s=0;s<S;s++){for(int r=0;r<=R;r++)cur[s*(R+1)+r]=base(r,s+1);for(int j=0;j<J;j++)for(int e=cp[s][j];e<cp[s][j+1];e++){int i=ci[s][e],r=reg[i];double w=W(i,s);g[(size_t)j*D+s*(R+1)+R]+=w;if(r>0)g[(size_t)j*D+s*(R+1)+r-1]+=w/share[r-1];}}\n auto score=[&](int j){double mean=0,worst=1,minreg=1;for(int s=0;s<S;s++){int k=s*(R+1);double v=cur[k+R]+(j<0?0:g[(size_t)j*D+k+R]);mean+=v/S;worst=std::min(worst,v);for(int r=0;r<R;r++)minreg=std::min(minreg,cur[k+r]+(j<0?0:g[(size_t)j*D+k+r]));}return (1-lambda-gamma)*mean+lambda*minreg+gamma*worst;};\n auto change=[&](int j,int sign){chosen[j]=(sign==1);for(int s=0;s<S;s++)for(int e=cp[s][j];e<cp[s][j+1];e++){int i=ci[s][e],r=reg[i],k=s*(R+1);int &c=cnt[(size_t)s*n+i];int old=c;c+=sign;if(c<0)stop(\"negative coverage counter\");if((sign==1&&old!=0)||(sign==-1&&c!=0))continue;double w=sign*W(i,s);cur[k+R]+=w;if(r>0)cur[k+r-1]+=w/share[r-1];for(int b=rp[s][i];b<rp[s][i+1];b++){int h=ri[s][b];g[(size_t)h*D+k+R]-=w;if(r>0)g[(size_t)h*D+k+r-1]-=w/share[r-1];}}};\n auto bestadd=[&](){int best=-1;double val=-1e100;for(int j=0;j<J;j++)if(!chosen[j]){bool productive=false;for(int s=0;s<S;s++)productive|=(cp[s][j+1]>cp[s][j]);if(!productive)continue;double v=score(j);if(v>val+1e-13){best=j;val=v;}}if(best<0)stop(\"no candidate\");return best;};\n for(int j:initial){if(j<1||j>J||chosen[j-1])stop(\"invalid warm start\");change(j-1,1);sel.push_back(j-1);}\n while((int)sel.size()<K){int j=bestadd();change(j,1);sel.push_back(j);}\n if((int)sel.size()!=K)stop(\"wrong fleet size\");double before=score(-1);int accepted=0;\n // Bounded exchanges: remove a deterministic spread of selected sites, search all replacements.\n for(int t=0;t<exchanges;t++){int pos=(int)((long long)t*K/std::max(1,exchanges));int j=sel[pos];double prev=score(-1);change(j,-1);int h=bestadd();change(h,1);if(score(-1)>prev+1e-12){sel[pos]=h;accepted++;}else{change(h,-1);change(j,1);}}\n IntegerVector ans(K);for(int k=0;k<K;k++)ans[k]=sel[k]+1;\n return List::create(_[\"selected\"]=ans,_[\"objective\"]=score(-1),_[\"before\"]=before,_[\"accepted\"]=accepted);\n}\n"
    Rcpp::sourceCpp(code = cpp, env = environment())
    base <- P("05_Travel_Time_Matrices", "Phase10C_Mobile")
    idx <- readRDS(file.path(base, "mobile_matrix_index.rds"))
    d <- as.data.table(idx$demand)
    cands <- as.data.table(idx$candidates)
    pop <- as.numeric(d$population)
    N <- sum(pop)
    regions <- sort(unique(d$region_group[d$region_group != "UNASSIGNED"]))
    rg <- match(d$region_group, regions)
    rg[is.na(rg)] <- 0L
    R <- length(regions)
    rp <- vapply(seq_len(R), function(r) sum(pop[rg == r]), numeric(1))
    stopifnot(R == 16L, nrow(d) == 51014L, nrow(cands) == 3581L, !anyDuplicated(d$demand_id), !anyDuplicated(cands$candidate_id), abs(N - 34746370) < 1, all(rp > 0))
    speeds <- c("baseline", "slow", "favorable")
    mids <- c("M01", "M02", "M03")
    ensnames <- c("STRUCT_ORIGINAL", "STRUCT_CLASSIFICATION_LOWER_STRESS", "STRUCT_EXPANDED_DEDUP_RECALIBRATED")
    mods <- c("XRAY", "ULTRASOUND", "COLOCATED")
    fleets <- c(8L, 16L, 32L, 64L, 100L)
    set.seed(20261127)
    trainids <- sort(sample.int(2000, 1000))
    testids <- setdiff(1:2000, trainids)
    trainflag <- as.integer((1:2000) %in% trainids)
    stopifnot(length(intersect(trainids, testids)) == 0L, sum(trainflag) == 1000L)
    write(data.table(draw = 1:2000, partition = ifelse(trainflag == 1, "TRAIN", "TEST")), file.path(out, "draw_split.csv"))
    design <- rbindlist(lapply(seq_along(speeds), function(s) data.table(speed = speeds[s], matrix_id = mids[s], ensemble = ensnames)))
    design[, `:=`(scenario_id, .I)]
    write(design, file.path(out, "scenario_design.csv"))
    B <- lapply(speeds, function(s) {
        b <- readRDS(file.path(base, toupper(s), "coverage_matrices.rds"))[["60"]]
        stopifnot(identical(rownames(b), as.character(d$demand_id)), identical(colnames(b), as.character(cands$candidate_id)), all(Matrix::rowSums(b)[!d$match_1km] == 0))
        as(b, "CsparseMatrix")
    })
    names(B) <- speeds
    fixed <- function(mid) {
        dir <- if (mid == "M01") 
            P("05_Travel_Time_Matrices", "Phase9B1_Core_Baseline")
        else P("05_Travel_Time_Matrices", "Phase9B2_Sensitivity", mid)
        a <- readRDS(file.path(dir, "matrix_index.rds"))
        dd <- as.data.table(a$demand)
        f <- as.data.table(a$facilities)
        stopifnot(identical(dd$demand_id, d$demand_id), identical(dd$match_1km, d$match_1km), all(abs(dd$population - pop) < 1e-08), a$bound_seconds == 7200)
        ds <- dd[match_1km == TRUE, .(target_index, di = row_id, dc = connector_m)]
        fs <- f[match_1km == TRUE & supply_candidate == TRUE, .(source_index, fi = facility_row, fc = connector_m)]
        e <- rbindlist(lapply(a$batch_files, function(b) {
            z <- readRDS(file.path(dir, "Checkpoints", b))
            stopifnot(identical(z$signature, a$signature))
            raw <- rbindlist(lapply(seq_along(z$source_indices), function(k) {
                x <- z$columns[[k]]
                data.table(source_index = rep(z$source_indices[k], length(x$i)), target_index = x$i, net = x$seconds)
            }))
            q <- merge(raw, fs, by = "source_index", allow.cartesian = TRUE)
            q <- merge(q, ds, by = "target_index", allow.cartesian = TRUE)
            q[net + 3.6 * (dc + fc)/4 <= 3600, .(di, fi)]
        }))
        setorder(e, di, fi)
        stopifnot(!anyDuplicated(e, by = c("di", "fi")))
        list(f = f, e = e, ptr = as.integer(c(0, cumsum(tabulate(e$di, nbins = nrow(d))))))
    }
    probs <- vector("list", nrow(design))
    loc <- as.data.table(readRDS(P("08_Results", "Phase10A_Accessibility", "primary_location_probabilities.rds")))
    loc <- loc[match(d$demand_id, loc$demand_id)]
    stopifnot(identical(loc$demand_id, d$demand_id))
    for (s in seq_along(speeds)) {
        a <- fixed(mids[s])
        for (e in seq_along(ensnames)) {
            k <- (s - 1L) * length(ensnames) + e
            z <- readRDS(P("02_Processed_Data", "Phase6B_Part2_Ensemble", paste0(ensnames[e], ".rds")))
            mf <- match(as.character(z$facility_ids), as.character(a$f$id))
            stopifnot(!anyNA(mf), !anyDuplicated(mf), ncol(z$xray) == 2000L, identical(dim(z$xray), dim(z$ultrasound)), !anyNA(z$xray), !anyNA(z$ultrasound))
            X <- U <- matrix(FALSE, nrow(a$f), 2000)
            X[mf, ] <- z$xray
            U[mf, ] <- z$ultrasound
            v <- prob_split(a$ptr, as.integer(a$e$fi), X, U, trainflag)
            v <- lapply(v, function(x) x/1000)
            stopifnot(all(v$train >= 0 & v$train <= 1), all(v$test >= 0 & v$test <= 1), all(v$train[!d$match_1km, ] == 0), all(v$test[!d$match_1km, ] == 0))
            if (k == 1L) 
                for (m in seq_along(mods)) stopifnot(max(abs((v$train[, m] + v$test[, m])/2 - loc[[paste0(mods[m], "_60")]])) < 1e-10)
            probs[[k]] <- v
            cat("Scenario", k, speeds[s], ensnames[e], "probabilities PASS\n")
            flush.console()
        }
        rm(a)
        gc()
    }
    probpath <- file.path(out, "scenario_probabilities.rds")
    saveRDS(probs, probpath)
    paths <- c(paths, probpath)
    keep <- which(Matrix::colSums(B[["baseline"]]) > 0)
    keep <- keep[order(cands$candidate_id[keep])]
    stopifnot(length(keep) == 3571L)
    B <- lapply(B, function(b) b[, keep, drop = FALSE])
    BB <- B[match(design$speed, speeds)]
    matrices <- function(b) {
        bt <- lapply(b, function(x) as(t(x), "CsparseMatrix"))
        list(cp = lapply(b, function(x) x@p), ci = lapply(b, function(x) x@i), rp = lapply(bt, function(x) x@p), ri = lapply(bt, function(x) x@i))
    }
    mats <- matrices(BB)
    obj <- function(n, r, l, g) (1 - l - g) * mean(n) + l * min(r) + g * min(n)
    evalset <- function(j, pp, ss) {
        n <- numeric(length(ss))
        rr <- matrix(0, R, length(ss))
        for (t in seq_along(ss)) {
            s <- ss[t]
            hit <- if (length(j)) 
                Matrix::rowSums(BB[[s]][, j, drop = FALSE]) > 0
            else rep(FALSE, length(pop))
            post <- pp[, s] + (1 - pp[, s]) * hit
            n[t] <- sum(pop * post)/N
            rr[, t] <- vapply(seq_len(R), function(r) sum(pop[rg == r] * post[rg == r])/rp[r], numeric(1))
            stopifnot(all(!hit[!d$match_1km]), all(post >= pp[, s] - 1e-12))
        }
        list(n = n, r = rr)
    }
    toy <- as(sparseMatrix(i = c(1, 2, 2, 3, 4), j = c(1, 1, 2, 2, 3), dims = c(4, 3)), "CsparseMatrix")
    tm <- matrices(list(toy, toy))
    tw <- matrix(c(0.4, 0.3, 0.2, 0.1, 0.2, 0.3, 0.3, 0.2), 4, 2)
    tg <- c(1L, 1L, 2L, 2L)
    tb <- cbind(c(0.7, 0.3, 1), matrix(0, 3, 2))
    ts <- matrices(list(toy))
    tz <- optimize_robust(ts$cp, ts$ci, ts$rp, ts$ri, tw[, 1, drop = FALSE], tg, tb[, 1:2, drop = FALSE], 0.5, 0, 2L, integer(), 2L)
    scoretoy <- function(j) {
        h <- Matrix::rowSums(toy[, j, drop = FALSE]) > 0
        0.5 * sum(tw[h, 1]) + 0.5 * min(sum(tw[h & tg == 1, 1])/0.7, sum(tw[h & tg == 2, 1])/0.3)
    }
    opt <- max(combn(3, 2, FUN = scoretoy))
    stopifnot(abs(scoretoy(tz$selected) - tz$objective) < 1e-10, tz$objective <= opt + 1e-10, tz$objective >= tz$before - 1e-10)
    cat("Synthetic objective/exchange test PASS\n")
    models <- data.table(model = c("A", "B", "C", "D"), lambda = c(0, 0.5, 0, 0.4), gamma = c(0, 0, 0.5, 0.3))
    write(models, file.path(out, "objective_weights.csv"))
    result <- list()
    detail <- list()
    regional <- list()
    selected <- list()
    audit <- list()
    id <- 0L
    for (m in seq_along(mods)) {
        tr <- do.call(cbind, lapply(probs, function(v) v$train[, m]))
        te <- do.call(cbind, lapply(probs, function(v) v$test[, m]))
        p0 <- evalset(integer(), tr, 1:9)
        paths0 <- list()
        solve <- function(ss, l, g, K, initial = integer(), ex = 0L) {
            mm <- matrices(BB[ss])
            w <- sweep(1 - tr[, ss, drop = FALSE], 1, pop/N, "*")
            b <- cbind(c(rp/N, 1), rbind(p0$r[, ss, drop = FALSE], p0$n[ss]))
            optimize_robust(mm$cp, mm$ci, mm$rp, mm$ri, w, as.integer(rg), b, l, g, as.integer(K), as.integer(initial), as.integer(ex))
        }
        for (h in 1:4) {
            ss <- if (h <= 2) 
                1L
            else 1:9
            paths0[[h]] <- solve(ss, models$lambda[h], models$gamma[h], 100L)$selected
        }
        for (K in fleets) for (h in 1:4) {
            id <- id + 1L
            ss <- if (h <= 2) 
                1L
            else 1:9
            l <- models$lambda[h]
            g <- models$gamma[h]
            starts <- unique(c(h, 1L, 2L))
            fits <- lapply(starts, function(j) solve(ss, l, g, K, paths0[[j]][seq_len(K)], min(6L, K)))
            v <- vapply(fits, function(x) x$objective, numeric(1))
            best <- fits[[which.max(v)]]
            j <- best$selected
            stopifnot(length(unique(j)) == K, best$objective >= best$before - 1e-09)
            at <- evalset(j, tr, 1:9)
            ae <- evalset(j, te, 1:9)
            bte <- evalset(integer(), te, 1:9)
            chk <- obj(at$n[ss], at$r[, ss, drop = FALSE], l, g)
            stopifnot(abs(chk - best$objective) < 1e-08)
            ub <- evalset(seq_along(keep), tr, ss)
            upper <- obj(ub$n, ub$r, l, g)
            stopifnot(upper >= chk - 1e-09)
            result[[id]] <- data.table(run_id = id, modality = mods[m], model = models$model[h], fleet = K, lambda = l, gamma = g, training_objective = chk, upper_bound = upper, absolute_bound_gap = max(0, upper - chk), train_primary_pct = 100 * at$n[1], test_primary_pct = 100 * ae$n[1], test_mean_pct = 100 * mean(ae$n), test_worst_pct = 100 * min(ae$n), test_min_region_pct = 100 * min(ae$r), test_primary_min_region_pct = 100 * min(ae$r[, 1]), test_mean_gain_pp = 100 * mean(ae$n - bte$n), test_worst_gain_pp = 100 * 
                min(ae$n - bte$n), accepted_exchanges = best$accepted, starts = length(starts))
            detail[[id]] <- cbind(data.table(run_id = id), design, data.table(train_pct = 100 * at$n, test_pct = 100 * ae$n, test_baseline_pct = 100 * bte$n, test_gain_pp = 100 * (ae$n - bte$n), test_min_region_pct = 100 * apply(ae$r, 2, min)))
            regional[[id]] <- data.table(run_id = id, scenario_id = rep(1:9, each = R), region = rep(regions, 9), population = rep(rp, 9), train_pct = 100 * as.vector(at$r), test_pct = 100 * as.vector(ae$r), test_baseline_pct = 100 * as.vector(bte$r))
            selected[[id]] <- cbind(data.table(run_id = id, selection_order = seq_len(K)), cands[keep[j], .(candidate_id, candidate_type, region, district, longitude, latitude, vertex_id)])
            audit[[id]] <- data.table(run_id = id, objective_error = abs(chk - best$objective), unique_sites = length(unique(j)), fleet = K, refinement_non_decreasing = best$objective >= best$before - 1e-09, upper_valid = upper >= chk - 1e-09)
            cat("Completed", mods[m], models$model[h], K, "test mean", round(100 * mean(ae$n), 3), "worst", round(100 * min(ae$n), 3), "\n")
            flush.console()
        }
    }
    ans <- rbindlist(result)
    det <- rbindlist(detail)
    reg <- rbindlist(regional)
    sel <- rbindlist(selected)
    aud <- rbindlist(audit)
    stopifnot(nrow(ans) == 60L, nrow(det) == 540L, nrow(reg) == 8640L, all(aud$unique_sites == aud$fleet), all(aud$objective_error < 1e-08), all(aud$upper_valid), all(aud$refinement_non_decreasing), all(det$test_gain_pp >= -1e-08))
    write(ans, P("10_Tables", "phase11B_optimization_summary.csv"))
    write(det, P("10_Tables", "phase11B_scenario_results.csv"))
    write(reg, P("10_Tables", "phase11B_regional_results.csv"))
    write(sel, file.path(out, "selected_candidates.csv"))
    write(aud, P("03_Data_Audit", "phase11B_solution_audit.csv"))
    p <- file.path(out, "phase11B_results.rds")
    saveRDS(list(summary = ans, scenario = det, regional = reg, selected = sel, design = design, models = models, trainids = trainids, testids = testids), p)
    paths <- c(paths, p)
    fig <- function(n, f) {
        for (ext in c("png", "pdf")) {
            p <- P("09_Figures", paste0(n, ".", ext))
            if (ext == "png") 
                png(p, width = 2200, height = 1500, res = 200)
            else pdf(p, width = 11, height = 7.5)
            tryCatch(f(), finally = dev.off())
            paths <<- c(paths, p)
        }
    }
    fig("Figure_43_Robust_Fleet_Coverage", function() {
        par(mfrow = c(1, 3), mar = c(5, 4, 3, 1))
        for (m in mods) {
            q <- ans[modality == m]
            plot(NA, xlim = range(fleets), ylim = c(0, 100), xlab = "Fleet size", ylab = "Held-out worst scenario mean (%)", main = m)
            for (h in 1:4) {
                z <- q[model == models$model[h]]
                setorder(z, fleet)
                lines(z$fleet, z$test_worst_pct, type = "b", col = h, pch = 14 + h)
            }
            legend("bottomright", models$model, col = 1:4, lty = 1, pch = 15:18, bty = "n")
        }
    })
    fig("Figure_44_Robust_Equity_Tradeoff", function() {
        par(mfrow = c(1, 3), mar = c(5, 4, 3, 1))
        for (m in mods) {
            q <- ans[modality == m & fleet == 32]
            plot(q$test_mean_pct, q$test_min_region_pct, pch = 19, xlim = range(q$test_mean_pct) + c(-0.5, 0.5), ylim = range(q$test_min_region_pct) + c(-1, 1), xlab = "Mean scenario coverage (%)", ylab = "Worst scenario-region mean (%)", main = m)
            text(q$test_mean_pct, q$test_min_region_pct, q$model, pos = 3)
        }
    })
    fig("Figure_45_Robust_Xray_Scenarios", function() {
        q <- merge(det, ans[modality == "XRAY" & fleet == 32, .(run_id, model)], by = "run_id")
        z <- dcast(q, scenario_id ~ model, value.var = "test_pct")
        par(mar = c(5, 5, 3, 1))
        barplot(t(as.matrix(z[, -1, with = FALSE])), beside = TRUE, names.arg = z$scenario_id, ylim = c(0, 100), col = 1:4, xlab = "Scenario ID (see scenario design)", ylab = "Held-out expected coverage (%)", main = "32 X-ray units: nine uncertainty configurations")
        legend("bottomright", models$model, fill = 1:4, bty = "n")
    })
    gate <- data.table(check = c("Previous manifests unchanged", "Disjoint paired simulation indices", "Primary full-sample probability reconciliation", "Nine scenario probabilities", "60 allocations", "540 scenario rows and 8640 region rows", "Unique fleet sites", "Independent objective reconciliation", "Nondecreasing accepted refinements", "All-site upper bounds", "No negative test coverage gains"), passed = c(all(hash(P(old$relative_path)) == old$SHA256), length(intersect(trainids, testids)) == 
        0L, TRUE, length(probs) == 9L, nrow(ans) == 60L, nrow(det) == 540L && nrow(reg) == 8640L, all(aud$unique_sites == aud$fleet), all(aud$objective_error < 1e-08), all(aud$refinement_non_decreasing), all(aud$upper_valid), all(det$test_gain_pp >= -1e-08)))
    stopifnot(all(gate$passed))
    write(gate, P("03_Data_Audit", "phase11B_quality_gate.csv"))
    write(data.table(item = c("Split", "Scenarios", "Robustness", "Equity", "Optimization", "Scope"), treatment = c("Common random column split 1000 train and 1000 test; prior descriptive analyses used all draws", "Three structural configurations crossed with three core-network speeds; equal design weights, not estimated probabilities", "Minimum of scenario expected national coverage, not individual-draw tail risk", "Minimum of scenario-region expected coverage; excludes unassigned from equity but not national denominator", 
        "Greedy and bounded exchanges with up to three starts; valid all-site upper ceiling; not certified optimum", "60 minutes, hypothetical unlimited service at selected sites; no observed validation or operational feasibility")), P("12_Reproducibility", "phase11B_modelling_assumptions.csv"))
    sp <- P("13_R_Scripts", "phase11B_complete.R")
    writeLines(c("phase11B_run <-", deparse(phase11B_run, width.cutoff = 500L), "phase11B_results <- phase11B_run()"), sp)
    paths <- c(paths, sp)
    paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
    man <- data.table(relative_path = substring(paths, nchar(root) + 2), SHA256 = hash(paths), size_bytes = file.info(paths)$size)
    fwrite(man, P("12_Reproducibility", "phase11B_output_checksum_manifest.csv"))
    man[, `:=`(verified, hash(P(relative_path)) == SHA256)]
    stopifnot(all(man$verified))
    fwrite(man, P("12_Reproducibility", "phase11B_checksum_recheck.csv"))
    print(ans[, .(modality, model, fleet, test_primary_pct, test_mean_pct, test_worst_pct, test_min_region_pct, absolute_bound_gap, accepted_exchanges)], nrows = 60)
    print(gate)
    cat("PHASE 11B ROBUST SCENARIO BENCHMARKS PASS\nAllocations:", nrow(ans), "\nChecksums:", sum(man$verified), "/", nrow(man), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), start, units = "secs")), "\nSIMULATED SCENARIO MEANS ONLY; NOT CERTIFIED GLOBAL OPTIMA\n")
    invisible(ans)
}
phase11B_results <- phase11B_run()
