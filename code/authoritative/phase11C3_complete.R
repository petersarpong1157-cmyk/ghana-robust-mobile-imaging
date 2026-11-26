phase11C3_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(Matrix)
        library(Rcpp)
        library(digest)
    })
    started <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    P <- function(...) file.path(root, ...)
    hash <- function(p) vapply(p, function(x) digest(file = x, algo = "sha256"), character(1), USE.NAMES = FALSE)
    old <- rbindlist(lapply(c("6B", "6B2", "9B1", "9B2", "10A", "10B", "10C", "11A", "11B", "11C1", "11C1B", "11C2"), function(s) {
        m <- fread(P("12_Reproducibility", paste0("phase", s, "_output_checksum_manifest.csv")))
        stopifnot(all(hash(P(m$relative_path)) == m$SHA256))
        cat("Phase", s, ":", nrow(m), "checksums PASS\n")
        m[, .(relative_path, SHA256)]
    }))
    out <- P("06_Optimization", "Phase11C3")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    paths <- character()
    write <- function(x, p) {
        fwrite(x, p)
        paths <<- c(paths, p)
    }
    if (Sys.info()[["sysname"]] == "Windows") 
        Sys.setenv(PATH = paste("C:/rtools45/usr/bin", "C:/rtools45/x86_64-w64-mingw32.static.posix/bin", Sys.getenv("PATH"), sep = ";"), RTOOLS45_HOME = "C:/rtools45")
    cpp <- "\n#include <Rcpp.h>\n#include <algorithm>\n#include <vector>\n#include <cmath>\nusing namespace Rcpp;\n// [[Rcpp::export]]\nList optimize_robust(List cpL,List ciL,List rpL,List riL,NumericMatrix W,IntegerVector reg,NumericMatrix base,double lambda,double gamma,int K,IntegerVector initial,int exchanges){\n int S=W.ncol(),n=W.nrow(),R=base.nrow()-1,J=as<IntegerVector>(cpL[0]).size()-1,D=S*(R+1);\n std::vector<IntegerVector> cp,ci,rp,ri;for(int s=0;s<S;s++){cp.push_back(as<IntegerVector>(cpL[s]));ci.push_back(as<IntegerVector>(ciL[s]));rp.push_back(as<IntegerVector>(rpL[s]));ri.push_back(as<IntegerVector>(riL[s]));}\n // W has raw population residuals; base rows contain raw covered population, denominators are separate below.\n // The last row of W is not special; region 0 is unassigned.\n // base is normalized in R; W is normalized to national population, regional gain normalized with region share.\n NumericVector share(R);for(int r=0;r<R;r++)share[r]=base(r,0); // column 0 contains regional shares\n std::vector<double> cur(D),g((size_t)J*D,0.0);std::vector<int> cnt((size_t)n*S,0),chosen(J,0),sel;\n for(int s=0;s<S;s++){for(int r=0;r<=R;r++)cur[s*(R+1)+r]=base(r,s+1);for(int j=0;j<J;j++)for(int e=cp[s][j];e<cp[s][j+1];e++){int i=ci[s][e],r=reg[i];double w=W(i,s);g[(size_t)j*D+s*(R+1)+R]+=w;if(r>0)g[(size_t)j*D+s*(R+1)+r-1]+=w/share[r-1];}}\n auto score=[&](int j){double mean=0,worst=1,minreg=1;for(int s=0;s<S;s++){int k=s*(R+1);double v=cur[k+R]+(j<0?0:g[(size_t)j*D+k+R]);mean+=v/S;worst=std::min(worst,v);for(int r=0;r<R;r++)minreg=std::min(minreg,cur[k+r]+(j<0?0:g[(size_t)j*D+k+r]));}return (1-lambda-gamma)*mean+lambda*minreg+gamma*worst;};\n auto change=[&](int j,int sign){chosen[j]=(sign==1);for(int s=0;s<S;s++)for(int e=cp[s][j];e<cp[s][j+1];e++){int i=ci[s][e],r=reg[i],k=s*(R+1);int &c=cnt[(size_t)s*n+i];int old=c;c+=sign;if(c<0)stop(\"negative coverage counter\");if((sign==1&&old!=0)||(sign==-1&&c!=0))continue;double w=sign*W(i,s);cur[k+R]+=w;if(r>0)cur[k+r-1]+=w/share[r-1];for(int b=rp[s][i];b<rp[s][i+1];b++){int h=ri[s][b];g[(size_t)h*D+k+R]-=w;if(r>0)g[(size_t)h*D+k+r-1]-=w/share[r-1];}}};\n auto bestadd=[&](){int best=-1;double val=-1e100;for(int j=0;j<J;j++)if(!chosen[j]){bool productive=false;for(int s=0;s<S;s++)productive|=(cp[s][j+1]>cp[s][j]);if(!productive)continue;double v=score(j);if(v>val+1e-13){best=j;val=v;}}if(best<0)stop(\"no candidate\");return best;};\n for(int j:initial){if(j<1||j>J||chosen[j-1])stop(\"invalid warm start\");change(j-1,1);sel.push_back(j-1);}\n while((int)sel.size()<K){int j=bestadd();change(j,1);sel.push_back(j);}\n if((int)sel.size()!=K)stop(\"wrong fleet size\");double before=score(-1);int accepted=0;\n // Bounded exchanges: remove a deterministic spread of selected sites, search all replacements.\n for(int t=0;t<exchanges;t++){int pos=(int)((long long)t*K/std::max(1,exchanges));int j=sel[pos];double prev=score(-1);change(j,-1);int h=bestadd();change(h,1);if(score(-1)>prev+1e-12){sel[pos]=h;accepted++;}else{change(h,-1);change(j,1);}}\n IntegerVector ans(K);for(int k=0;k<K;k++)ans[k]=sel[k]+1;\n return List::create(_[\"selected\"]=ans,_[\"objective\"]=score(-1),_[\"before\"]=before,_[\"accepted\"]=accepted);\n}\n"
    Rcpp::sourceCpp(code = cpp, env = environment())
    base <- P("05_Travel_Time_Matrices", "Phase10C_Mobile")
    idx <- readRDS(file.path(base, "mobile_matrix_index.rds"))
    d <- as.data.table(idx$demand)
    cands <- as.data.table(idx$candidates)
    prev <- readRDS(P("06_Optimization", "Phase11B", "phase11B_results.rds"))
    probs <- readRDS(P("06_Optimization", "Phase11B", "scenario_probabilities.rds"))
    design <- as.data.table(prev$design)
    stopifnot(length(probs) == 9L, nrow(design) == 9L, identical(sort(c(prev$trainids, prev$testids)), 1:2000), length(intersect(prev$trainids, prev$testids)) == 0L, length(prev$trainids) == 1000L, nrow(prev$summary) == 60L)
    pop <- as.numeric(d$population)
    N <- sum(pop)
    regions <- sort(unique(d$region_group[d$region_group != "UNASSIGNED"]))
    rg <- match(d$region_group, regions)
    rg[is.na(rg)] <- 0L
    R <- length(regions)
    rp <- vapply(seq_len(R), function(r) sum(pop[rg == r]), numeric(1))
    stopifnot(R == 16L, nrow(d) == 51014L, nrow(cands) == 3581L, abs(N - 34746370) < 1, !anyDuplicated(cands$candidate_id))
    speeds <- c("baseline", "slow", "favorable")
    mods <- c("XRAY", "ULTRASOUND", "COLOCATED")
    fleets <- c(8L, 16L, 32L, 64L, 100L)
    B <- lapply(speeds, function(s) {
        b <- readRDS(file.path(base, toupper(s), "coverage_matrices.rds"))[["60"]]
        stopifnot(identical(rownames(b), as.character(d$demand_id)), identical(colnames(b), as.character(cands$candidate_id)), all(Matrix::rowSums(b)[!d$match_1km] == 0))
        as(b, "CsparseMatrix")
    })
    names(B) <- speeds
    keep <- which(Matrix::colSums(B[["baseline"]]) > 0)
    keep <- keep[order(cands$candidate_id[keep])]
    stopifnot(length(keep) == 3571L)
    cands <- cands[keep]
    B <- lapply(B, function(b) b[, keep, drop = FALSE])
    BB <- B[match(design$speed, speeds)]
    pools <- list(COMBINED = seq_len(nrow(cands)), HEALTHCARE_PROXY = which(cands$candidate_type == "HEALTHCARE_PROXY"), POPULATION_PROXY = which(cands$candidate_type == "POPULATION_PROXY"))
    stopifnot(length(pools[[2]]) + length(pools[[3]]) == length(pools[[1]]), min(lengths(pools)) >= 100)
    write(data.table(pool = names(pools), sites = lengths(pools)), file.path(out, "candidate_pool_counts.csv"))
    weights <- data.table(weight_id = c("W01", "W02", "W03", "W04", "W05", "W06", "W07", "W08"), lambda = c(0, 0.2, 0.4, 0.6, 0.8, 0.4, 0.4, 1), gamma = c(0.3, 0.3, 0.3, 0.3, 0.2, 0, 0.6, 0))
    stopifnot(all(weights$lambda + weights$gamma <= 1 + 1e-12))
    write(weights, file.path(out, "weight_design.csv"))
    experiments <- rbind(weights[, .(weight_id, lambda, gamma, pool = "COMBINED")], weights[weight_id == "W03"][rep(1L, 2L)][, `:=`(pool, names(pools)[2:3])])
    matrices <- function(b) {
        bt <- lapply(b, function(x) as(t(x), "CsparseMatrix"))
        list(cp = lapply(b, function(x) x@p), ci = lapply(b, function(x) x@i), rp = lapply(bt, function(x) x@p), ri = lapply(bt, function(x) x@i))
    }
    obj <- function(n, r, l, g) (1 - l - g) * mean(n) + l * min(r) + g * min(n)
    evalset <- function(j, pp) {
        n <- numeric(9)
        rr <- matrix(0, R, 9)
        for (s in 1:9) {
            hit <- if (length(j)) 
                Matrix::rowSums(BB[[s]][, j, drop = FALSE]) > 0
            else rep(FALSE, length(pop))
            post <- pp[, s] + (1 - pp[, s]) * hit
            n[s] <- sum(pop * post)/N
            rr[, s] <- vapply(seq_len(R), function(r) sum(pop[rg == r] * post[rg == r])/rp[r], numeric(1))
            stopifnot(all(!hit[!d$match_1km]), all(post >= pp[, s] - 1e-12))
        }
        list(n = n, r = rr)
    }
    rows <- list()
    sels <- list()
    regs <- list()
    dets <- list()
    audit <- list()
    id <- 0L
    recon <- numeric()
    for (m in seq_along(mods)) {
        tr <- do.call(cbind, lapply(probs, function(v) v$train[, m]))
        te <- do.call(cbind, lapply(probs, function(v) v$test[, m]))
        stopifnot(all(dim(tr) == c(nrow(d), 9)), all(tr >= 0 & tr <= 1), all(te >= 0 & te <= 1))
        p0 <- evalset(integer(), tr)
        t0 <- evalset(integer(), te)
        w <- sweep(1 - tr, 1, pop/N, "*")
        b <- cbind(c(rp/N, 1), rbind(p0$r, p0$n))
        frozen <- function(h, K) {
            a <- prev$summary[modality == mods[m] & model == h & fleet == K]
            stopifnot(nrow(a) == 1L)
            z <- prev$selected[run_id == a$run_id]
            setorder(z, selection_order)
            j <- match(z$candidate_id, cands$candidate_id)
            stopifnot(length(j) == K, !anyNA(j), !anyDuplicated(j))
            j
        }
        for (K in fleets) {
            for (h in c("A", "B", "C", "D")) {
                j <- frozen(h, K)
                a <- prev$summary[modality == mods[m] & model == h & fleet == K]
                t <- evalset(j, te)
                recon <- c(recon, abs(100 * mean(t$n) - a$test_mean_pct), abs(100 * min(t$n) - a$test_worst_pct), abs(100 * min(t$r) - a$test_min_region_pct))
            }
        }
        for (e in seq_len(nrow(experiments))) {
            ex <- experiments[e]
            pool <- pools[[ex$pool]]
            mm <- matrices(lapply(BB, function(x) x[, pool, drop = FALSE]))
            l <- ex$lambda
            g <- ex$gamma
            solve <- function(K, initial = integer(), exchange = 0L) optimize_robust(mm$cp, mm$ci, mm$rp, mm$ri, w, as.integer(rg), b, l, g, as.integer(K), as.integer(initial), as.integer(exchange))
            greedy <- solve(100L)$selected
            for (K in fleets) {
                id <- id + 1L
                starts <- list(greedy[seq_len(K)], na.omit(match(frozen("A", K), pool)), na.omit(match(frozen("D", K), pool)))
                fits <- lapply(starts, function(j) solve(K, as.integer(j), min(6L, K)))
                scores <- vapply(fits, function(x) x$objective, numeric(1))
                best <- fits[[which.max(scores)]]
                j <- pool[best$selected]
                at <- evalset(j, tr)
                ae <- evalset(j, te)
                chk <- obj(at$n, at$r, l, g)
                stopifnot(abs(chk - best$objective) < 1e-08, length(j) == K, !anyDuplicated(j), all(j %in% pool), best$objective >= best$before - 1e-09)
                reference <- evalset(frozen("D", K), te)
                reftrain <- evalset(frozen("D", K), tr)
                reference_obj <- obj(reftrain$n, reftrain$r, l, g)
                rows[[id]] <- data.table(run_id = id, modality = mods[m], fleet = K, pool = ex$pool, weight_id = ex$weight_id, lambda = l, gamma = g, training_objective = chk, reference_training_objective = reference_obj, test_mean_pct = 100 * mean(ae$n), test_worst_pct = 100 * min(ae$n), test_min_region_pct = 100 * min(ae$r), test_primary_pct = 100 * ae$n[1], test_gain_pp = 100 * mean(ae$n - t0$n), delta_D_mean_pp = 100 * (mean(ae$n) - mean(reference$n)), delta_D_worst_pp = 100 * (min(ae$n) - min(reference$n)), 
                  delta_D_region_pp = 100 * (min(ae$r) - min(reference$r)), accepted_exchanges = best$accepted)
                sels[[id]] <- cbind(data.table(run_id = id, selection_order = seq_len(K)), cands[j, .(candidate_id, candidate_type, region, district, longitude, latitude, vertex_id)])
                dets[[id]] <- cbind(data.table(run_id = id), design, data.table(train_pct = 100 * at$n, test_pct = 100 * ae$n, test_baseline_pct = 100 * t0$n))
                regs[[id]] <- data.table(run_id = id, scenario_id = rep(design$scenario_id, each = R), region = rep(regions, 9), population = rep(rp, 9), train_pct = 100 * as.vector(at$r), test_pct = 100 * as.vector(ae$r))
                audit[[id]] <- data.table(run_id = id, objective_error = abs(chk - best$objective), unique_sites = length(unique(j)), fleet = K, eligible = all(j %in% pool), refinement_ok = best$objective >= best$before - 1e-09, reference_start_ok = ex$pool != "COMBINED" || chk >= reference_obj - 1e-08)
                cat("Completed", mods[m], ex$pool, ex$weight_id, K, "test worst region", round(100 * min(ae$r), 3), "\n")
                flush.console()
            }
        }
    }
    ans <- rbindlist(rows)
    sel <- rbindlist(sels)
    det <- rbindlist(dets)
    reg <- rbindlist(regs)
    aud <- rbindlist(audit)
    ans[, `:=`(nondominated, vapply(seq_len(.N), function(i) !any(test_mean_pct >= test_mean_pct[i] & test_worst_pct >= test_worst_pct[i] & test_min_region_pct >= test_min_region_pct[i] & (test_mean_pct > test_mean_pct[i] | test_worst_pct > test_worst_pct[i] | test_min_region_pct > test_min_region_pct[i])), logical(1))), by = .(modality, fleet, pool)]
    pooltab <- ans[weight_id == "W03", .(modality, fleet, pool, training_objective, test_mean_pct, test_worst_pct, test_min_region_pct, delta_D_mean_pp, delta_D_worst_pp, delta_D_region_pp)]
    write(ans, P("10_Tables", "phase11C3_sensitivity_summary.csv"))
    write(pooltab, P("10_Tables", "phase11C3_candidate_sensitivity.csv"))
    write(det, P("10_Tables", "phase11C3_scenario_results.csv"))
    write(reg, P("10_Tables", "phase11C3_regional_results.csv"))
    write(sel, file.path(out, "selected_candidates.csv"))
    write(aud, P("03_Data_Audit", "phase11C3_solution_audit.csv"))
    fig <- function(n, f) {
        for (ext in c("png", "pdf")) {
            p <- P("09_Figures", paste0(n, ".", ext))
            if (ext == "png") 
                png(p, width = 2400, height = 1500, res = 200)
            else pdf(p, width = 12, height = 7.5)
            tryCatch(f(), finally = dev.off())
            paths <<- c(paths, p)
        }
    }
    labels <- c("X-ray", "Ultrasound", "Integrated")
    cols <- c("#2166AC", "#D95F02", "#1B9E77")
    fig("Figure_48_Objective_Weight_Sensitivity", function() {
        par(mfrow = c(1, 3), mar = c(5, 5, 4, 1))
        for (m in seq_along(mods)) {
            q <- ans[modality == mods[m] & fleet == 32 & pool == "COMBINED"]
            plot(q$test_mean_pct, q$test_min_region_pct, pch = ifelse(q$nondominated, 19, 1), col = "#2166AC", xlim = range(q$test_mean_pct) + c(-0.3, 0.3), ylim = range(q$test_min_region_pct) + c(-1, 2), xlab = "Mean national coverage (%)", ylab = "Worst scenario-region mean (%)", main = labels[m])
            text(q$test_mean_pct, q$test_min_region_pct, q$weight_id, pos = 3, cex = 0.8)
        }
    })
    fig("Figure_49_Candidate_Pool_Sensitivity", function() {
        par(mfrow = c(1, 3), mar = c(5, 5, 4, 1))
        for (m in seq_along(mods)) {
            plot(NA, xlim = range(fleets), ylim = c(0, 100), xlab = "Fleet size", ylab = "Worst scenario-region mean (%)", main = labels[m])
            for (k in seq_along(pools)) {
                q <- pooltab[modality == mods[m] & pool == names(pools)[k]]
                setorder(q, fleet)
                lines(q$fleet, q$test_min_region_pct, type = "b", col = cols[k], pch = 15 + k, lwd = 2)
            }
            legend("bottomright", c("Combined", "Healthcare proxy", "Population proxy"), col = cols, lty = 1, pch = 16:18, bty = "n", cex = 0.7)
        }
    })
    gate <- data.table(check = c("Prior files unchanged", "Training test separation", "Frozen 60 allocations reconciled", "Candidate partition", "150 allocations", "1350 scenario rows", "21600 regional rows", "Independent objective reconciliation", "Unique eligible selected sites", "Nondecreasing refinement", "Combined reference start retained or improved", "Nonnegative test gains"), passed = c(all(hash(P(old$relative_path)) == old$SHA256), !length(intersect(prev$trainids, prev$testids)), length(recon) == 
        180L && max(recon) < 1e-07, length(unique(unlist(pools[2:3]))) == 3571L, nrow(ans) == 150L, nrow(det) == 1350L, nrow(reg) == 21600L, all(aud$objective_error < 1e-08), all(aud$unique_sites == aud$fleet & aud$eligible), all(aud$refinement_ok), all(aud$reference_start_ok), all(ans$test_gain_pp >= -1e-08)))
    print(gate)
    stopifnot(all(gate$passed))
    write(gate, P("03_Data_Audit", "phase11C3_quality_gate.csv"))
    write(data.table(item = c("Status", "Weights", "Pools", "Evaluation", "Scope"), treatment = c("Exploratory sensitivity after prior held-out results; no untouched confirmation", "Eight weight pairs across combined representatives, reference pair across two restricted representative subsets", "Proxy labels of existing deduplicated routing representatives; not complete healthcare inventory or feasible sites", "Original nine scenarios and disjoint held-out indices; no test-based selection; equal scenario design weights", 
        "Hypothetical 60-minute unlimited service; heuristic, not certified optimum; original Phase11B and Phase11C2 unchanged")), P("12_Reproducibility", "phase11C3_modelling_assumptions.csv"))
    p <- file.path(out, "phase11C3_results.rds")
    saveRDS(list(summary = ans, selected = sel, scenario = det, regional = reg, weights = weights, pools = lapply(pools, function(j) cands$candidate_id[j])), p)
    paths <- c(paths, p)
    sp <- P("13_R_Scripts", "phase11C3_complete.R")
    writeLines(c("phase11C3_run <-", deparse(phase11C3_run, width.cutoff = 500L), "phase11C3_results <- phase11C3_run()"), sp)
    paths <- c(paths, sp)
    paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
    man <- data.table(relative_path = substring(paths, nchar(root) + 2), SHA256 = hash(paths), size_bytes = file.info(paths)$size)
    fwrite(man, P("12_Reproducibility", "phase11C3_output_checksum_manifest.csv"))
    man[, `:=`(verified, hash(P(relative_path)) == SHA256)]
    stopifnot(all(man$verified))
    fwrite(man, P("12_Reproducibility", "phase11C3_checksum_recheck.csv"))
    print(ans[fleet == 32, .(modality, pool, weight_id, training_objective, test_mean_pct, test_worst_pct, test_min_region_pct, delta_D_region_pp)], nrows = 30)
    print(pooltab, nrows = 45)
    print(gate)
    cat("PHASE 11C3 SENSITIVITY PASS\nAllocations:", nrow(ans), "\nChecksums:", sum(man$verified), "/", nrow(man), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\nEXPLORATORY SENSITIVITY; NOT UNTOUCHED CONFIRMATION\n")
    invisible(ans)
}
phase11C3_results <- phase11C3_run()
