phase10A_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(digest)
        library(Rcpp)
    })
    start <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    P <- function(...) file.path(root, ...)
    hash <- function(p) vapply(p, function(x) digest(file = x, algo = "sha256"), character(1), USE.NAMES = FALSE)
    phases <- c("6B", "6B2", "9B1", "9B2")
    old <- rbindlist(lapply(phases, function(s) {
        m <- fread(P("12_Reproducibility", paste0("phase", s, "_output_checksum_manifest.csv")))
        stopifnot(all(hash(P(m$relative_path)) == m$SHA256))
        cat("Phase", s, ":", nrow(m), "checksums PASS\n")
        m[, .(relative_path, SHA256)]
    }))
    out <- P("08_Results", "Phase10A_Accessibility")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    cp <- file.path(out, "Checkpoints_v2")
    dir.create(cp, showWarnings = FALSE)
    ens <- c(list.files(P("02_Processed_Data", "Phase6B_Infrastructure_Ensemble"), "^ENS_.*[.]rds$", full.names = TRUE), list.files(P("02_Processed_Data", "Phase6B_Part2_Ensemble"), "^(FAC_|STRUCT_).*[.]rds$", full.names = TRUE))
    names(ens) <- sub("[.]rds$", "", basename(ens))
    stopifnot(length(ens) == 115L, !anyDuplicated(names(ens)), sum(startsWith(names(ens), "ENS_")) == 54L, sum(startsWith(names(ens), "FAC_")) == 54L, sum(startsWith(names(ens), "STRUCT_")) == 7L, "STRUCT_ORIGINAL" %in% names(ens))
    stopifnot(all(substring(normalizePath(ens, winslash = "/"), nchar(root) + 2) %in% old$relative_path))
    if (Sys.info()[["sysname"]] == "Windows") 
        Sys.setenv(PATH = paste("C:/rtools45/usr/bin", "C:/rtools45/x86_64-w64-mingw32.static.posix/bin", Sys.getenv("PATH"), sep = ";"), RTOOLS45_HOME = "C:/rtools45")
    cpp <- "\n#include <Rcpp.h>\n#include <algorithm>\nusing namespace Rcpp;\n// [[Rcpp::export]]\nList access_eval(IntegerVector ptr,IntegerVector fac,NumericVector sec,LogicalMatrix X,LogicalMatrix U,NumericVector w,IntegerVector grp,int G){\n int N=w.size(), S=X.ncol(); NumericMatrix totals(G*12,S), counts(N,12);\n for(int i=0;i<N;++i){if(ptr[i]==ptr[i+1])continue;int g=grp[i]-1;\n  for(int s=0;s<S;++s){double a=1e100,b=1e100,c=1e100;\n   for(int e=ptr[i];e<ptr[i+1];++e){int j=fac[e]-1;bool x=X(j,s),u=U(j,s);double t=sec[e];\n    if(x && a>t)a=t;if(u && b>t)b=t;if(x&&u&&c>t)c=t;\n    if(c<1e99)break;\n   }\n   double z[4]={a,b,std::max(a,b),c};\n   for(int m=0;m<4;++m)for(int h=0;h<3;++h)if(z[m]<= (h==0?1800:(h==1?3600:7200))){int k=m*3+h;totals(g+G*k,s)+=w[i];counts(i,k)+=1;}\n  }\n }\n return List::create(_[\"totals\"]=totals,_[\"counts\"]=counts);\n}\n"
    Rcpp::sourceCpp(code = cpp, env = environment())
    tx <- matrix(c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE), 3, 2)
    tu <- matrix(c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE), 3, 2)
    tt <- c(1800, 2000, 3600, 7200)
    tf <- c(1L, 2L, 3L, 2L)
    tp <- c(0L, 3L, 4L, 4L)
    tw <- c(2, 3, 5)
    tg <- c(1L, 2L, 1L)
    test <- access_eval(tp, tf, tt, tx, tu, tw, tg, 2L)
    ref <- matrix(0, 24, 2)
    rc <- matrix(0, 3, 12)
    for (i in 1:3) for (s in 1:2) {
        e <- if (tp[i + 1] > tp[i]) 
            seq.int(tp[i] + 1L, tp[i + 1])
        else integer()
        a <- min(c(Inf, tt[e][tx[tf[e], s]]))
        b <- min(c(Inf, tt[e][tu[tf[e], s]]))
        c0 <- min(c(Inf, tt[e][tx[tf[e], s] & tu[tf[e], s]]))
        z <- c(a, b, max(a, b), c0)
        for (m in 1:4) for (h in 1:3) if (z[m] <= c(1800, 3600, 7200)[h]) {
            k <- (m - 1) * 3 + h
            ref[tg[i] + 2 * (k - 1), s] <- ref[tg[i] + 2 * (k - 1), s] + tw[i]
            rc[i, k] <- rc[i, k] + 1
        }
    }
    stopifnot(identical(test$totals, ref), identical(test$counts, rc))
    cat("Synthetic modality reference: PASS\n")
    metrics <- rep(c("XRAY", "ULTRASOUND", "BOTH_REACHABLE", "COLOCATED"), each = 3)
    mins <- rep(c(30, 60, 120), 4)
    dir_for <- function(id) if (id == "M01") 
        P("05_Travel_Time_Matrices", "Phase9B1_Core_Baseline")
    else P("05_Travel_Time_Matrices", "Phase9B2_Sensitivity", id)
    load_edges <- function(mid) {
        dir <- dir_for(mid)
        idx <- readRDS(file.path(dir, "matrix_index.rds"))
        d <- as.data.table(idx$demand)
        f <- as.data.table(idx$facilities)
        stopifnot(!anyDuplicated(d$demand_id), !anyDuplicated(f$id), abs(sum(d$population) - 34746370) < 1, idx$bound_seconds == 7200)
        ds <- d[match_1km == TRUE, .(target_index, di = row_id, dc = connector_m)]
        fs <- f[match_1km == TRUE & supply_candidate == TRUE, .(source_index, fi = facility_row, fc = connector_m)]
        stopifnot(identical(as.integer(d$row_id), seq_len(nrow(d))), identical(as.integer(f$facility_row), seq_len(nrow(f))), !anyNA(ds$target_index), !anyNA(fs$source_index))
        pieces <- lapply(idx$batch_files, function(b) {
            z <- readRDS(file.path(dir, "Checkpoints", b))
            stopifnot(identical(z$signature, idx$signature))
            raw <- rbindlist(lapply(seq_along(z$source_indices), function(k) {
                x <- z$columns[[k]]
                data.table(source_index = rep(z$source_indices[k], length(x$i)), target_index = x$i, net = x$seconds)
            }))
            q <- merge(raw, fs, by = "source_index", allow.cartesian = TRUE)
            q <- merge(q, ds, by = "target_index", allow.cartesian = TRUE)
            q[, `:=`(seconds, net + 3.6 * (dc + fc)/4)]
            q[seconds <= 7200, .(di, fi, seconds)]
        })
        e <- rbindlist(pieces)
        setorder(e, di, seconds, fi)
        stopifnot(!anyDuplicated(e, by = c("di", "fi")), all(is.finite(e$seconds) & e$seconds >= 0))
        ptr <- as.integer(c(0, cumsum(tabulate(e$di, nbins = nrow(d)))))
        stopifnot(tail(ptr, 1) == nrow(e))
        diag <- fread(if (mid == "M01") 
            P("10_Tables", "phase9B1_all_candidate_diagnostic.csv")
        else file.path(dir, "all_candidate_diagnostic.csv"))
        best <- rep(Inf, nrow(d))
        a <- e[, .(t = min(seconds)), by = di]
        best[a$di] <- a$t
        check <- vapply(c(30, 60, 120), function(h) sum(d$population[best <= 60 * h]), numeric(1))
        stopifnot(all(abs(check - diag$covered_population) < 1e-04))
        region <- as.character(d$region_group)
        stopifnot(!anyNA(region), length(unique(region)) == 17L)
        district <- as.character(d$district_code)
        district[is.na(district) | district == ""] <- "UNASSIGNED"
        district[region == "UNASSIGNED"] <- "UNASSIGNED"
        stopifnot(length(region) == nrow(d), length(district) == nrow(d), length(d$population) == nrow(d), all(is.finite(d$population)), all(d$population > 0))
        groups <- unique(data.table(region = region, district = district))
        groups[, `:=`(group_id, .I)]
        demand_group <- match(paste(region, district, sep = "|"), paste(groups$region, groups$district, sep = "|"))
        stopifnot(length(demand_group) == nrow(d), !anyNA(demand_group), all(demand_group >= 1L & demand_group <= nrow(groups)))
        grouped_population <- rowsum(matrix(as.numeric(d$population), ncol = 1L), group = demand_group, reorder = TRUE)
        positions <- match(as.character(groups$group_id), rownames(grouped_population))
        stopifnot(!anyNA(positions), nrow(grouped_population) == nrow(groups))
        group_pop <- as.numeric(grouped_population[positions, 1L])
        data.table::set(groups, j = "population", value = group_pop)
        independently_grouped <- vapply(seq_len(nrow(groups)), function(k) sum(d$population[demand_group == k]), numeric(1))
        stopifnot(all(groups$population > 0), all(abs(groups$population - independently_grouped) < 1e-05), abs(sum(groups$population) - sum(d$population)) < 1e-05)
        cat("Population grouping: PASS | demand rows:", nrow(d), "| grouping vector:", length(demand_group), "| district-region groups:", nrow(groups), "| population:", format(sum(groups$population), digits = 14), "\n")
        list(d = d, f = f, e = e, ptr = ptr, g = as.integer(demand_group), groups = groups, benchmark = check, signature = idx$signature)
    }
    local({
        pop <- c(2, 3, 5, 7, 11)
        demand_group <- c(1L, 2L, 1L, 3L, 2L)
        groups <- data.table(group_id = 1:3)
        sums <- rowsum(matrix(pop, ncol = 1L), group = demand_group, reorder = TRUE)
        values <- as.numeric(sums[match(as.character(groups$group_id), rownames(sums)), 1L])
        data.table::set(groups, j = "population", value = values)
        stopifnot(identical(groups$population, c(7, 14, 7)), sum(groups$population) == sum(pop))
        cat("Population grouping regression test: PASS\n")
    })
    set.seed(20261126)
    sens_ids <- sort(sample.int(2000, 250))
    trans_ids <- sort(sample.int(2000, 500))
    full_ids <- 1:2000
    plan <- rbindlist(list(data.table(run = "PRIMARY", matrix_id = "M01", ensemble = "STRUCT_ORIGINAL", sample = "FULL"), data.table(run = paste0("INFRA_", names(ens)), matrix_id = "M01", ensemble = names(ens), sample = "SENS"), data.table(run = paste0("TRANSPORT_M", sprintf("%02d", 1:11)), matrix_id = sprintf("M%02d", 1:11), ensemble = "STRUCT_ORIGINAL", sample = "TRANSPORT")))
    sig <- digest(list(old = old, plan = plan, sens_ids = sens_ids, trans_ids = trans_ids, cpp = cpp, version = "10A_v2_grouping_fix"), algo = "sha256")
    output_paths <- character()
    all_nat <- list()
    all_reg <- list()
    all_district <- list()
    primary_draws <- NULL
    qsummary <- function(M, den, labels) {
        stopifnot(nrow(M) == nrow(labels), length(den) == nrow(M), all(den > 0))
        v <- M/den * 100
        stopifnot(all(v >= -1e-08 & v <= 100 + 1e-08))
        qs <- t(apply(v, 1, quantile, probs = c(0.025, 0.5, 0.975), names = FALSE, type = 7))
        cbind(labels, data.table(population = den, mean_pct = rowMeans(v), sd_pct = apply(v, 1, sd), mcse_pct = apply(v, 1, sd)/sqrt(ncol(v)), p025_pct = qs[, 1], median_pct = qs[, 2], p975_pct = qs[, 3], draws = ncol(v)))
    }
    for (mid in unique(plan$matrix_id)) {
        cat("\nMATRIX", mid, "\n")
        a <- load_edges(mid)
        G <- nrow(a$groups)
        R <- sort(unique(a$groups$region))
        rg <- match(a$groups$region, R)
        rp <- as.numeric(rowsum(a$groups$population, rg, reorder = TRUE))
        for (ii in which(plan$matrix_id == mid)) {
            p <- plan[ii]
            ids <- switch(p$sample, FULL = full_ids, SENS = sens_ids, TRANSPORT = trans_ids)
            path <- file.path(cp, paste0(p$run, ".rds"))
            output_paths <- c(output_paths, path)
            if (file.exists(path)) {
                res <- readRDS(path)
                stopifnot(identical(res$signature, sig), identical(res$ids, ids), identical(res$plan, p))
                cat("Recovered", p$run, "\n")
            }
            else {
                z <- readRDS(ens[[p$ensemble]])
                fid <- as.character(z$facility_ids)
                stopifnot(length(fid) == nrow(z$xray), !anyDuplicated(fid), identical(dim(z$xray), dim(z$ultrasound)), ncol(z$xray) == 2000L, !anyNA(z$xray), !anyNA(z$ultrasound), all(z$xray %in% c(FALSE, TRUE)), all(z$ultrasound %in% c(FALSE, TRUE)))
                mf <- match(fid, as.character(a$f$id))
                stopifnot(!anyNA(mf), all(a$f$supply_candidate[mf] %in% TRUE))
                X <- U <- matrix(FALSE, nrow(a$f), length(ids))
                X[mf, ] <- z$xray[, ids, drop = FALSE]
                U[mf, ] <- z$ultrasound[, ids, drop = FALSE]
                zz <- access_eval(a$ptr, as.integer(a$e$fi), a$e$seconds, X, U, a$d$population, a$g, G)
                D <- zz$totals
                N <- matrix(0, 12, length(ids))
                RR <- matrix(0, length(R) * 12, length(ids))
                for (k in 1:12) {
                  v <- D[seq_len(G) + G * (k - 1), , drop = FALSE]
                  N[k, ] <- colSums(v)
                  RR[seq_along(R) + length(R) * (k - 1), ] <- rowsum(v, rg, reorder = TRUE)
                }
                for (m in 0:3) {
                  stopifnot(all(N[m * 3 + 1, ] <= N[m * 3 + 2, ] + 1e-06), all(N[m * 3 + 2, ] <= N[m * 3 + 3, ] + 1e-06))
                }
                stopifnot(all(N[7:9, ] <= pmin(N[1:3, ], N[4:6, ]) + 1e-06), all(N[10:12, ] <= N[7:9, ] + 1e-06), all(N <= rep(a$benchmark, 4) + 1e-05), all(zz$counts >= 0 & zz$counts <= length(ids)))
                nat <- qsummary(N, rep(sum(a$d$population), 12), data.table(modality = metrics, threshold_minutes = mins))
                reg <- qsummary(RR, rep(rp, 12), data.table(region = rep(R, 12), modality = rep(metrics, each = length(R)), threshold_minutes = rep(mins, each = length(R))))
                dis <- NULL
                loc <- NULL
                if (p$run == "PRIMARY") {
                  dis <- qsummary(D, rep(a$groups$population, 12), cbind(a$groups[rep(seq_len(G), 12), .(region, district)], data.table(modality = rep(metrics, each = G), threshold_minutes = rep(mins, each = G))))
                  loc <- a$d[, .(demand_id, region_group, district_code, population, match_1km)]
                  prob <- as.data.table(zz$counts/length(ids))
                  setnames(prob, paste(metrics, mins, sep = "_"))
                  loc <- cbind(loc, prob)
                  stopifnot(all(abs(colSums(zz$counts * a$d$population)/length(ids) - rowMeans(N)) < 1e-05))
                }
                res <- list(signature = sig, plan = p, ids = ids, national = nat, regional = reg, district = dis, location = loc, national_draws = N, regional_draws = RR, regions = R, active_counts = data.table(draw = ids, xray = colSums(X), ultrasound = colSums(U)), unmatched_population = sum(a$d$population[!a$d$match_1km]))
                tmp <- paste0(path, ".tmp")
                saveRDS(res, tmp)
                stopifnot(file.rename(tmp, path))
                cat("Completed", p$run, "draws", length(ids), "\n")
            }
            all_nat[[ii]] <- cbind(p[rep(1, nrow(res$national))], res$national)
            all_reg[[ii]] <- cbind(p[rep(1, nrow(res$regional))], res$regional)
            if (p$run == "PRIMARY") {
                primary_draws <- res
                all_district[[1]] <- res$district
            }
        }
        rm(a)
        gc()
    }
    nat <- rbindlist(all_nat)
    reg <- rbindlist(all_reg)
    dis <- rbindlist(all_district)
    stopifnot(nrow(nat) == nrow(plan) * 12, nrow(reg) == nrow(plan) * 204, nrow(plan) == 127L)
    pairs <- list(c("M02", "M01"), c("M01", "M03"), c("M08", "M04"), c("M04", "M09"), c("M10", "M05"), c("M05", "M11"))
    speed <- rbindlist(lapply(pairs, function(p) {
        x <- readRDS(file.path(cp, paste0("TRANSPORT_", p[1], ".rds")))
        y <- readRDS(file.path(cp, paste0("TRANSPORT_", p[2], ".rds")))
        stopifnot(identical(x$ids, y$ids))
        data.table(slower = p[1], faster = p[2], comparisons = length(x$national_draws), violations = sum(x$national_draws > y$national_draws + 1e-05))
    }))
    stopifnot(all(speed$violations == 0))
    gate <- data.table(check = c("Prior manifests unchanged", "115 ensembles retained", "127 analysis runs", "Primary 2000 draws", "All national rows", "All regional rows", "Synthetic reference", "Paired speed order", "All checkpoints exist"), passed = c(all(hash(P(old$relative_path)) == old$SHA256), length(ens) == 115, nrow(plan) == 127, length(primary_draws$ids) == 2000, nrow(nat) == 1524, nrow(reg) == 25908, TRUE, all(speed$violations == 0), all(file.exists(output_paths))))
    stopifnot(all(gate$passed))
    outputs <- list(`10_Tables/phase10A_national_accessibility.csv` = nat, `10_Tables/phase10A_regional_accessibility.csv` = reg, `10_Tables/phase10A_primary_district_accessibility.csv` = dis, `03_Data_Audit/phase10A_quality_gate.csv` = gate, `03_Data_Audit/phase10A_paired_speed_order.csv` = speed, `12_Reproducibility/phase10A_experiment_design.csv` = plan, `12_Reproducibility/phase10A_draw_indices.csv` = rbindlist(list(data.table(sample = "FULL", draw = full_ids), data.table(sample = "SENS", draw = sens_ids), 
        data.table(sample = "TRANSPORT", draw = trans_ids))))
    paths <- output_paths
    for (n in names(outputs)) {
        fwrite(outputs[[n]], P(n))
        paths <- c(paths, P(n))
    }
    locpath <- file.path(out, "primary_location_probabilities.rds")
    saveRDS(primary_draws$location, locpath)
    paths <- c(paths, locpath)
    fig <- function(name, fun) {
        for (ext in c("png", "pdf")) {
            p <- P("09_Figures", paste0(name, ".", ext))
            if (ext == "png") 
                png(p, width = 2200, height = 1500, res = 200)
            else pdf(p, width = 11, height = 7.5)
            tryCatch(fun(), finally = dev.off())
            paths <<- c(paths, p)
        }
    }
    fig("Figure_34_Stochastic_National_Accessibility", function() {
        par(mar = c(5, 5, 3, 2))
        q <- primary_draws$national
        plot(c(30, 60, 120), c(0, 100, 0), type = "n", ylim = c(0, 100), xlab = "Travel time threshold (minutes)", ylab = "Expected population coverage (%)", main = "Scenario-based national imaging accessibility", xaxt = "n")
        axis(1, at = c(30, 60, 120))
        cc <- c("#2166AC", "#1B9E77", "#D95F02", "#7570B3")
        for (i in 1:4) {
            z <- q[modality == unique(metrics)[i]]
            lines(z$threshold_minutes, z$mean_pct, type = "b", pch = 19, col = cc[i], lwd = 2)
            arrows(z$threshold_minutes, z$p025_pct, z$threshold_minutes, z$p975_pct, angle = 90, code = 3, length = 0.04, col = cc[i])
        }
        legend("bottomright", legend = c("X-ray", "Ultrasound", "Both reachable", "Co-located"), col = cc, lty = 1, pch = 19, bty = "n")
    })
    fig("Figure_35_Regional_Modality_Accessibility", function() {
        par(mar = c(5, 10, 3, 2))
        q <- primary_draws$regional[threshold_minutes == 60 & region != "UNASSIGNED" & modality %in% c("XRAY", "ULTRASOUND")]
        r <- dcast(q, region ~ modality, value.var = "mean_pct")
        setorder(r, XRAY)
        barplot(t(as.matrix(r[, .(XRAY, ULTRASOUND)])), beside = TRUE, horiz = TRUE, names.arg = r$region, las = 1, xlim = c(0, 100), col = c("#2166AC", "#1B9E77"), xlab = "Expected population coverage (%)", main = "Regional imaging accessibility within 60 minutes")
        legend("bottomright", c("X-ray", "Ultrasound"), fill = c("#2166AC", "#1B9E77"), bty = "n")
    })
    fig("Figure_36_Transport_Sensitivity_Modality", function() {
        par(mar = c(6, 5, 3, 2))
        q <- nat[sample == "TRANSPORT" & threshold_minutes == 60 & modality %in% c("XRAY", "ULTRASOUND")]
        r <- dcast(q, matrix_id ~ modality, value.var = "mean_pct")
        barplot(t(as.matrix(r[, .(XRAY, ULTRASOUND)])), beside = TRUE, names.arg = r$matrix_id, ylim = c(0, 100), col = c("#2166AC", "#1B9E77"), ylab = "Expected coverage (%)", main = "Transportation sensitivity: paired infrastructure draws")
        legend("topright", c("X-ray", "Ultrasound"), fill = c("#2166AC", "#1B9E77"), bty = "n")
    })
    assumptions <- data.table(item = c("Supply", "Primary", "Sensitivity", "Transport", "Connectors", "Missing routes", "Joint", "Intervals", "Supplementary leads", "Unmatched"), treatment = c("Historical hypothetical draws, not current observed service", "STRUCT_ORIGINAL, all 2000 draws", "250 common column indices per 115 configurations; not all draws evaluated", "500 common primary draws per matrix", "1km both ends, 4km/h, same as prior benchmark", "No coverage demonstrated within model threshold; no disconnection claim", 
        "Both reachable may be different sites; colocated requires same facility", "Empirical 2.5 and 97.5 percentiles; MCSE describes simulation mean only", "Documentary-only additions excluded; separate stress test needed", "All weights retained; unmatched treated as zero demonstrated coverage, not known absence"))
    ap <- P("12_Reproducibility", "phase10A_modelling_assumptions.csv")
    fwrite(assumptions, ap)
    paths <- c(paths, ap)
    sp <- P("13_R_Scripts", "phase10A_complete.R")
    writeLines(c("phase10A_run <-", deparse(phase10A_run, width.cutoff = 500L), "phase10A_results <- phase10A_run()"), sp)
    paths <- c(paths, sp)
    paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
    manifest <- data.table(relative_path = substring(paths, nchar(root) + 2), SHA256 = hash(paths), size_bytes = file.info(paths)$size)
    fwrite(manifest, P("12_Reproducibility", "phase10A_output_checksum_manifest.csv"))
    manifest[, `:=`(verified, hash(P(relative_path)) == SHA256)]
    stopifnot(all(manifest$verified), all(hash(P(old$relative_path)) == old$SHA256))
    fwrite(manifest, P("12_Reproducibility", "phase10A_checksum_recheck.csv"))
    print(primary_draws$national)
    print(primary_draws$regional[threshold_minutes == 60], nrows = 70)
    print(speed)
    print(gate)
    cat("\nPHASE 10A STOCHASTIC ACCESSIBILITY PASS\nRuns:", nrow(plan), "\nChecksums:", sum(manifest$verified), "/", nrow(manifest), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), start, units = "secs")), "\nHYPOTHETICAL SERVICE AVAILABILITY ONLY\n")
    invisible(list(national = nat, regional = reg, district = dis, quality = gate, manifest = manifest))
}
phase10A_results <- phase10A_run()
