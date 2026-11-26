phase11C1_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(Matrix)
        library(digest)
    })
    if (!requireNamespace("highs", quietly = TRUE)) 
        install.packages("highs", repos = "https://cloud.r-project.org", type = "binary")
    stopifnot(requireNamespace("highs", quietly = TRUE))
    started <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    P <- function(...) file.path(root, ...)
    hash <- function(p) vapply(p, function(x) digest(file = x, algo = "sha256"), character(1), USE.NAMES = FALSE)
    old <- rbindlist(lapply(c("10A", "10B", "10C", "11A", "11B"), function(s) {
        a <- fread(P("12_Reproducibility", paste0("phase", s, "_output_checksum_manifest.csv")))
        stopifnot(all(hash(P(a$relative_path)) == a$SHA256))
        cat("Phase", s, ":", nrow(a), "checksums PASS\n")
        a[, .(relative_path, SHA256)]
    }))
    out <- P("06_Optimization", "Phase11C1")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    paths <- character()
    write <- function(x, p) {
        fwrite(x, p)
        paths <<- c(paths, p)
    }
    bdir <- P("06_Optimization", "Phase11B")
    a <- readRDS(file.path(bdir, "phase11B_results.rds"))
    probs <- readRDS(file.path(bdir, "scenario_probabilities.rds"))
    base <- P("05_Travel_Time_Matrices", "Phase10C_Mobile")
    idx <- readRDS(file.path(base, "mobile_matrix_index.rds"))
    d <- as.data.table(idx$demand)
    cands <- as.data.table(idx$candidates)
    pop <- as.numeric(d$population)
    N <- sum(pop)
    regions <- sort(unique(d$region_group[d$region_group != "UNASSIGNED"]))
    rg <- match(d$region_group, regions)
    rp <- vapply(seq_along(regions), function(r) sum(pop[which(rg == r)]), numeric(1))
    stopifnot(length(regions) == 16L, length(pop) == 51014L, abs(N - 34746370) < 1, nrow(cands) == 3581L, length(intersect(a$trainids, a$testids)) == 0, length(probs) == 9L)
    speeds <- c("baseline", "slow", "favorable")
    B <- lapply(speeds, function(s) {
        b <- readRDS(file.path(base, toupper(s), "coverage_matrices.rds"))[["60"]]
        stopifnot(identical(rownames(b), as.character(d$demand_id)), identical(colnames(b), as.character(cands$candidate_id)), all(Matrix::rowSums(b)[!d$match_1km] == 0))
        b
    })
    names(B) <- speeds
    keep <- which(Matrix::colSums(B$baseline) > 0)
    keep <- keep[order(cands$candidate_id[keep])]
    stopifnot(length(keep) == 3571L)
    B <- lapply(B, function(b) b[, keep, drop = FALSE])
    ids <- as.character(cands$candidate_id[keep])
    des <- as.data.table(a$design)
    stopifnot(identical(as.integer(des$scenario_id), 1:9), identical(as.character(des$speed), rep(speeds, each = 3)))
    mods <- c("XRAY", "ULTRASOUND", "COLOCATED")
    ans <- as.data.table(a$summary)
    sel <- as.data.table(a$selected)
    evalset <- function(j, pp, ss, l, g) {
        n <- numeric(length(ss))
        rr <- matrix(0, 16, length(ss))
        for (t in seq_along(ss)) {
            s <- ss[t]
            hit <- if (length(j)) 
                Matrix::rowSums(B[[des$speed[s]]][, j, drop = FALSE]) > 0
            else rep(FALSE, length(pop))
            post <- pp[, s] + (1 - pp[, s]) * hit
            n[t] <- sum(pop * post)/N
            rr[, t] <- vapply(1:16, function(r) {
                ii <- which(rg == r)
                sum(pop[ii] * post[ii])/rp[r]
            }, numeric(1))
        }
        list(obj = (1 - l - g) * mean(n) + l * min(rr) + g * min(n), mean = mean(n), worst = min(n), equity = min(rr))
    }
    build <- function(pool, pp, ss, l, g, K) {
        J <- length(pool)
        sp <- unique(as.character(des$speed[ss]))
        br <- lapply(sp, function(s) {
            b <- B[[s]][, pool, drop = FALSE]
            ii <- which(Matrix::rowSums(b) > 0)
            list(i = ii, b = b[ii, , drop = FALSE])
        })
        names(br) <- sp
        ny <- vapply(br, function(z) length(z$i), integer(1))
        off <- J + c(0L, cumsum(ny))
        V <- J + sum(ny) + 2L
        ze <- V - 1L
        zn <- V
        ii <- list()
        jj <- list()
        vv <- list()
        rhs <- numeric()
        nr <- 0L
        add <- function(i, j, v, r) {
            ii[[length(ii) + 1L]] <<- as.integer(i + nr)
            jj[[length(jj) + 1L]] <<- as.integer(j)
            vv[[length(vv) + 1L]] <<- as.numeric(v)
            rhs <<- c(rhs, r)
            nr <<- nr + length(r)
        }
        add(rep(1L, J), seq_len(J), rep(1, J), K)
        for (k in seq_along(br)) {
            z <- br[[k]]
            q <- summary(z$b)
            n <- ny[k]
            if (n) {
                add(c(q$i, seq_len(n)), c(q$j, off[k] + seq_len(n)), c(rep(-1, nrow(q)), rep(1, n)), rep(0, n))
            }
        }
        L <- numeric(V)
        L[ze] <- l
        L[zn] <- g
        offset <- 0
        for (s in ss) {
            k <- match(des$speed[s], sp)
            ix <- br[[k]]$i
            yi <- off[k] + seq_along(ix)
            w <- pop * (1 - pp[, s])
            bn <- sum(pop * pp[, s])/N
            offset <- offset + (1 - l - g) * bn/length(ss)
            L[yi] <- L[yi] + (1 - l - g) * w[ix]/N/length(ss)
            if (g > 0) 
                add(rep(1L, length(yi) + 1L), c(yi, zn), c(-w[ix]/N, 1), bn)
            if (l > 0) 
                for (r in 1:16) {
                  ir <- which(rg[ix] == r)
                  allr <- which(rg == r)
                  baseline <- sum(pop[allr] * pp[allr, s])/rp[r]
                  add(rep(1L, length(ir) + 1L), c(yi[ir], ze), c(-w[ix[ir]]/rp[r], 1), baseline)
                }
        }
        A <- sparseMatrix(i = unlist(ii), j = unlist(jj), x = unlist(vv), dims = c(nr, V))
        scale <- rep(1, nr)
        first_weighted <- 1L + sum(ny)
        if (nr > first_weighted) 
            scale[seq.int(first_weighted + 1L, nr)] <- 1e+06
        A <- as(Diagonal(x = scale) %*% A, "CsparseMatrix")
        rhs <- rhs * scale
        qq <- summary(A)
        tiny <- which(abs(qq$x) <= 1e-09 & qq$x != 0)
        omitted <- length(tiny)
        allowance <- 0
        if (omitted) {
            tt <- qq[tiny, , drop = FALSE]
            adj <- rowsum(pmax(0, -tt$x), tt$i, reorder = FALSE)
            rr <- as.integer(rownames(adj))
            rhs[rr] <- rhs[rr] + as.numeric(adj)
            allowance <- sum(adj)
            qq$x[tiny] <- 0
            A <- drop0(sparseMatrix(i = qq$i, j = qq$j, x = qq$x, dims = c(nr, V)))
        }
        list(A = A, L = L, rhs = rhs, offset = offset, J = J, V = V, omitted = omitted, allowance = allowance)
    }
    solve <- function(z, mip = FALSE, limit = 60) {
        highs::highs_solve(L = z$L, lower = rep(0, z$V), upper = rep(1, z$V), A = z$A, lhs = rep(-Inf, nrow(z$A)), rhs = z$rhs, types = c(rep(if (mip) "I" else "C", z$J), rep("C", z$V - z$J)), maximum = TRUE, offset = z$offset, control = highs::highs_control(threads = 2L, time_limit = limit, log_to_console = FALSE, mip_rel_gap = 0, mip_abs_gap = 0, small_matrix_value = 1e-12))
    }
    optimal <- function(s) identical(tolower(trimws(as.character(s$status_message))), "optimal")
    toy <- highs::highs_solve(L = c(3, 2), lower = c(0, 0), upper = c(1, 1), A = matrix(c(1, 1), 1), rhs = 1, types = c("I", "I"), maximum = TRUE)
    stopifnot(optimal(toy), abs(toy$objective_value - 3) < 1e-08)
    full <- list()
    reduced <- list()
    chosen <- list()
    counter <- 0L
    for (m in seq_along(mods)) {
        tr <- do.call(cbind, lapply(probs, function(v) v$train[, m]))
        stopifnot(all(is.finite(tr)), all(tr >= 0 & tr <= 1))
        for (h in c("A", "B", "C", "D")) {
            counter <- counter + 1L
            q <- ans[modality == mods[m] & model == h & fleet == 32]
            stopifnot(nrow(q) == 1L)
            ss <- if (h %in% c("A", "B")) 
                1L
            else 1:9
            l <- q$lambda
            g <- q$gamma
            j <- match(sel[run_id == q$run_id, candidate_id], ids)
            stopifnot(!anyNA(j), length(unique(j)) == 32L)
            v <- evalset(j, tr, ss, l, g)$obj
            stopifnot(abs(v - q$training_objective) < 1e-08)
            cat("Full LP:", mods[m], h, "K=32\n")
            flush.console()
            z <- build(seq_along(ids), tr, ss, l, g, 32L)
            s <- solve(z, FALSE, 90)
            ok <- optimal(s)
            raw_ub <- if (ok) 
                as.numeric(s$objective_value)
            else NA_real_
            ok <- ok && is.finite(raw_ub) && raw_ub >= v - 1e-05
            ub <- if (ok) 
                raw_ub
            else NA_real_
            if (!ok) 
                cat("LP bound not accepted; retaining previous ceiling\n")
            full[[counter]] <- data.table(modality = mods[m], model = h, fleet = 32L, heuristic = v, old_upper = q$upper_bound, lp_status = as.character(s$status_message), lp_raw_objective = raw_ub, lp_upper = ub, valid_upper = min(c(q$upper_bound, ub + 1e-05), na.rm = TRUE), variables = z$V, constraints = nrow(z$A), omitted_terms = z$omitted, scaled_rhs_relaxation = z$allowance)
            rm(z, s)
            gc()
            runs <- ans[modality == mods[m] & fleet == 8, run_id]
            pool <- sort(unique(match(sel[run_id %in% runs, candidate_id], ids)))
            stopifnot(!anyNA(pool), length(pool) >= 8L)
            K <- 4L
            gj <- integer()
            for (step in seq_len(K)) {
                avail <- setdiff(pool, gj)
                scores <- vapply(avail, function(k) evalset(c(gj, k), tr, ss, l, g)$obj, numeric(1))
                gj <- c(gj, avail[which.max(scores)])
            }
            gv <- evalset(gj, tr, ss, l, g)$obj
            cat("Restricted MIP:", mods[m], h, "pool", length(pool), "K=4\n")
            flush.console()
            z <- build(pool, tr, ss, l, g, K)
            s <- solve(z, TRUE, 90)
            ok <- optimal(s)
            mj <- integer()
            mv <- NA_real_
            if (length(s$primal_solution) == z$V && all(is.finite(s$primal_solution[seq_along(pool)]))) {
                xx <- s$primal_solution[seq_along(pool)]
                if (all(abs(xx - round(xx)) < 1e-05) && sum(xx > 0.5) <= K) {
                  mj <- pool[xx > 0.5]
                  mv <- evalset(mj, tr, ss, l, g)$obj
                }
            }
            raw_mip <- if (length(s$objective_value) == 1L) 
                as.numeric(s$objective_value)
            else NA_real_
            delta <- mv - raw_mip
            certified <- ok && is.finite(mv) && is.finite(raw_mip) && abs(delta) < 1e-05 && mv >= gv - 1e-05
            cat("MIP status:", as.character(s$status_message), " solver objective:", raw_mip, " recalculated:", mv, " difference:", delta, " certified:", certified, "\n")
            flush.console()
            reduced[[counter]] <- data.table(modality = mods[m], model = h, fleet = K, pool_size = length(pool), greedy = gv, mip_status = as.character(s$status_message), mip_solver_objective = raw_mip, mip_feasible_objective = mv, objective_difference = delta, certification_status = if (certified) 
                "RECONCILED_OPTIMAL_WITHIN_TOLERANCE"
            else "NOT_CERTIFIED", omitted_terms = z$omitted, scaled_rhs_relaxation = z$allowance, certified_restricted_optimum = if (certified) 
                mv
            else NA_real_, greedy_gap = if (certified) 
                max(0, mv - gv)
            else NA_real_)
            chosen[[counter]] <- rbind(data.table(modality = mods[m], model = h, method = "POOL", candidate_id = ids[pool]), data.table(modality = mods[m], model = h, method = "GREEDY", candidate_id = ids[gj]), if (length(mj)) 
                data.table(modality = mods[m], model = h, method = "MIP", candidate_id = ids[mj])
            else NULL)
            rm(z, s)
            gc()
        }
    }
    F <- rbindlist(full)
    F[, `:=`(tightened_gap, pmax(0, valid_upper - heuristic))]
    G <- rbindlist(reduced)
    S <- rbindlist(chosen)
    stopifnot(nrow(F) == 12L, nrow(G) == 12L, all(F$valid_upper >= F$heuristic - 1e-05), all(F$valid_upper <= F$old_upper + 1e-08), all(G$greedy_gap[!is.na(G$greedy_gap)] >= 0), all(hash(P(old$relative_path)) == old$SHA256))
    write(F, P("10_Tables", "phase11C1_full_lp_bounds.csv"))
    write(G, P("10_Tables", "phase11C1_restricted_mip.csv"))
    write(S, file.path(out, "restricted_candidate_sets.csv"))
    gate <- data.table(check = c("Prior manifests unchanged", "Training split disjoint", "Solver smoke test", "12 full LP attempts", "12 restricted MIP attempts", "Training objectives reproduced", "Valid retained bounds", "Only reconciled optimal restricted solutions certified"), passed = TRUE)
    gate[check == "Only reconciled optimal restricted solutions certified", `:=`(passed, all(is.na(G$certified_restricted_optimum) | (G$certification_status == "RECONCILED_OPTIMAL_WITHIN_TOLERANCE" & abs(G$objective_difference) < 1e-05)))]
    stopifnot(all(gate$passed))
    write(gate, P("03_Data_Audit", "phase11C1_quality_gate.csv"))
    write(data.table(item = c("Full LP", "Restricted MIP", "Timeouts", "Scope"), treatment = c("All 3571 productive candidates; 32 units; continuous site variables; only optimal solver status accepted as LP bound with numerical allowance", "Training-derived candidate pool; 4 units; binary site variables, continuous coverage; optimum applies only to restricted pool", "90 second solver time limit per attempt; nonoptimal or inconsistent LP uses old ceiling; nonoptimal or unreconciled MIP not certified; coverage constraints scaled by 1e6, remaining tiny constraint terms explicitly relaxed; numerical objective tolerance 1e-5", 
        "No held-out tuning; original Phase11B allocations unchanged; no draw-level or extended scenario analysis in this subphase")), P("12_Reproducibility", "phase11C1_modelling_assumptions.csv"))
    p <- file.path(out, "session_info.txt")
    writeLines(capture.output(sessionInfo()), p)
    paths <- c(paths, p)
    p <- P("13_R_Scripts", "phase11C1_corrected.R")
    writeLines(c("phase11C1_run <-", deparse(phase11C1_run, width.cutoff = 500L), "phase11C1_results <- phase11C1_run()"), p)
    paths <- c(paths, p)
    paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
    man <- data.table(relative_path = substring(paths, nchar(root) + 2), SHA256 = hash(paths), size_bytes = file.info(paths)$size)
    fwrite(man, P("12_Reproducibility", "phase11C1_output_checksum_manifest.csv"))
    man[, `:=`(verified, hash(P(relative_path)) == SHA256)]
    stopifnot(all(man$verified))
    fwrite(man, P("12_Reproducibility", "phase11C1_checksum_recheck.csv"))
    print(F)
    print(G)
    print(gate)
    cat("PHASE 11C1 BENCHMARKS COMPLETE\nOptimal LP:", sum(!is.na(F$lp_upper)), "/12\nCertified restricted MIP:", sum(!is.na(G$certified_restricted_optimum)), "/12\nChecksums:", sum(man$verified), "/", nrow(man), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), started, units = "secs")), "\n")
    invisible(list(full = F, reduced = G))
}
phase11C1_results <- phase11C1_run()
