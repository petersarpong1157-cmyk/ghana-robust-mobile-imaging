phase8C2_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(digest)
        library(jsonlite)
    })
    started <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    p <- function(...) file.path(root, ...)
    graph_dir <- p("04_Spatial_Analysis", "National_Road_Network", "Directed_Graphs")
    turn_dir <- p("04_Spatial_Analysis", "National_Road_Network", "Turn_Restrictions")
    out <- p("04_Spatial_Analysis", "National_Road_Network", "Routing_Costs")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    hash <- function(x) {
        stopifnot(all(file.exists(x)))
        vapply(x, function(f) digest(file = f, algo = "sha256"), character(1), USE.NAMES = FALSE)
    }
    old <- fread(p("12_Reproducibility", "phase8C1_output_checksum_manifest.csv"))
    old2 <- fread(p("12_Reproducibility", "phase8B2_output_checksum_manifest.csv"))
    inputs <- unique(rbindlist(list(old[, .(relative_path, SHA256)], old2[, .(relative_path, SHA256)])))
    stopifnot(nrow(old) == 21L, nrow(old2) == 52L, all(hash(p(inputs$relative_path)) == inputs$SHA256))
    q <- fread(p("03_Data_Audit", "phase8C1_quality_gate.csv"))
    q2 <- fread(p("03_Data_Audit", "phase8B2_quality_gate.csv"))
    stopifnot(nrow(q) == 20L, all(q$passed), nrow(q2) == 36L, all(q2$passed))
    cat("Previous manifests: 21/21 and 52/52 PASS\n")
    turns <- readRDS(file.path(graph_dir, "ghana_turn_restrictions_corrected.rds"))
    members <- as.data.table(turns$members)
    members[, `:=`(member_ref = as.character(member_ref), member_type = tolower(member_type), member_role = tolower(member_role))]
    setorder(members, relation_id, member_order)
    ways <- as.data.table(readRDS(file.path(graph_dir, "ghana_way_access_direction_audit.rds")))
    stopifnot(nrow(ways) == 364113L, identical(as.integer(ways$way_index), seq_len(nrow(ways))))
    tag <- function(nm) {
        x <- if (nm %in% names(ways)) 
            tolower(trimws(as.character(ways[[nm]])))
        else rep("", nrow(ways))
        x[is.na(x)] <- ""
        x
    }
    parse_speed <- function(x) {
        ok <- grepl("^[0-9]+([.][0-9]+)?[ ]*(km/h|kmh|kph|mph)?$", x)
        v <- rep(NA_real_, length(x))
        v[ok] <- as.numeric(sub("[ ]*(km/h|kmh|kph|mph)$", "", x[ok]))
        mph <- ok & grepl("mph$", x)
        v[mph] <- v[mph] * 1.609344
        v[!is.finite(v) | v <= 0 | v > 160] <- NA_real_
        v
    }
    speed_design <- data.table(road_class = c("motorway", "trunk", "primary", "secondary", "tertiary", "residential", "unclassified", "living_street", "service", "track", "road"), slow = c(65, 45, 35, 30, 25, 15, 15, 5, 10, 5, 10), baseline = c(85, 65, 55, 45, 35, 25, 30, 10, 15, 10, 20), favorable = c(100, 80, 70, 60, 45, 35, 40, 15, 25, 20, 30))
    cls <- tag("highway")
    link <- grepl("_link$", cls)
    base_class <- sub("_link$", "", cls)
    j <- match(base_class, speed_design$road_class)
    stopifnot(!anyNA(j))
    surface <- tag("surface")
    smooth <- tag("smoothness")
    track <- tag("tracktype")
    rough <- surface %in% c("unpaved", "gravel", "fine_gravel", "ground", "dirt", "earth", "compacted", "laterite", "pebblestone", "grass", "woodchips") | track %in% c("grade2", "grade3", "grade4")
    poor <- smooth %in% c("bad", "very_bad", "horrible", "very_horrible", "impassable") | track %in% c("grade4", "grade5") | surface %in% c("mud", "sand")
    paved <- surface %in% c("asphalt", "concrete", "paved", "paving_stones", "concrete:plates", "concrete:lanes")
    condition <- ifelse(poor, "POOR", ifelse(rough, "ROUGH", ifelse(paved, "PAVED", ifelse(surface == "", "UNSPECIFIED", "OTHER"))))
    factors <- data.table(condition = c("POOR", "ROUGH", "PAVED", "UNSPECIFIED", "OTHER"), slow = c(0.4, 0.6, 1, 0.75, 0.7), baseline = c(0.6, 0.75, 1, 0.9, 0.85), favorable = c(0.75, 0.9, 1, 1, 1))
    k <- match(condition, factors$condition)
    raw_general <- tag("maxspeed")
    raw_forward <- tag("maxspeed:forward")
    raw_reverse <- tag("maxspeed:backward")
    general <- parse_speed(raw_general)
    forward <- parse_speed(raw_forward)
    reverse <- parse_speed(raw_reverse)
    cap_forward <- ifelse(is.na(forward), general, forward)
    cap_reverse <- ifelse(is.na(reverse), general, reverse)
    speed_profile <- data.table(way_index = ways$way_index, way_id = ways$way_id, road_class = cls, condition = condition, raw_general = raw_general, raw_forward = raw_forward, raw_reverse = raw_reverse, cap_forward = cap_forward, cap_reverse = cap_reverse)
    for (s in c("slow", "baseline", "favorable")) {
        v <- speed_design[[s]][j] * factors[[s]][k] * ifelse(link, 0.75, 1)
        speed_profile[[paste0(s, "_forward")]] <- pmin(v, ifelse(is.na(cap_forward), Inf, cap_forward))
        speed_profile[[paste0(s, "_reverse")]] <- pmin(v, ifelse(is.na(cap_reverse), Inf, cap_reverse))
    }
    speed_audit <- data.table(tag = c("maxspeed", "maxspeed:forward", "maxspeed:backward"), specified = c(sum(raw_general != ""), sum(raw_forward != ""), sum(raw_reverse != "")), parsed = c(sum(!is.na(general)), sum(!is.na(forward)), sum(!is.na(reverse))))
    speed_audit[, `:=`(unparsed, specified - parsed)]
    print(speed_audit)
    find_sequence <- function(a, order_ids, max_states = 100000L, max_arcs = 1000L) {
        if (length(order_ids) < 3L || anyNA(order_ids) || anyDuplicated(order_ids)) 
            return(list(status = "UNSUPPORTED_MEMBER_PATTERN", path = integer(), states = 0L))
        parts <- lapply(order_ids, function(w) a[way_index == w])
        if (any(vapply(parts, nrow, integer(1)) == 0L)) 
            return(list(status = "MEMBER_ABSENT", path = integer(), states = 0L))
        lookup <- lapply(parts, function(z) split(seq_len(nrow(z)), as.character(z$from)))
        first <- parts[[1L]]
        eligible <- which(first$to %in% parts[[2L]]$from)
        stack <- lapply(eligible, function(i) list(stage = 2L, node = first$to[i], path = as.integer(first$arc_id[i]), visited = first$to[i], used = FALSE))
        solutions <- list()
        states <- 0L
        truncated <- FALSE
        while (length(stack) > 0L) {
            states <- states + 1L
            if (states > max_states) {
                truncated <- TRUE
                break
            }
            st <- stack[[length(stack)]]
            stack[[length(stack)]] <- NULL
            if (length(st$path) >= max_arcs) {
                truncated <- TRUE
                next
            }
            h <- st$stage
            z <- parts[[h]]
            rows <- lookup[[h]][[as.character(st$node)]]
            if (h == length(parts)) {
                for (i in rows) {
                  path <- c(st$path, as.integer(z$arc_id[i]))
                  if (!any(vapply(solutions, function(x) identical(x, path), logical(1)))) 
                    solutions[[length(solutions) + 1L]] <- path
                  if (length(solutions) > 1L) 
                    return(list(status = "AMBIGUOUS_SEQUENCE", path = integer(), states = states))
                }
                next
            }
            if (st$used && st$node %in% parts[[h + 1L]]$from) 
                stack[[length(stack) + 1L]] <- list(stage = h + 1L, node = st$node, path = st$path, visited = st$node, used = FALSE)
            for (i in rows) {
                nxt <- z$to[i]
                if (nxt %in% st$visited) 
                  next
                stack[[length(stack) + 1L]] <- list(stage = h, node = nxt, path = c(st$path, as.integer(z$arc_id[i])), visited = c(st$visited, nxt), used = TRUE)
            }
        }
        if (truncated) 
            return(list(status = "SEARCH_LIMIT_REVIEW", path = integer(), states = states))
        if (length(solutions) == 0L) 
            return(list(status = "NO_CONTINUOUS_SEQUENCE", path = integer(), states = states))
        list(status = "UNIQUE_SEQUENCE", path = solutions[[1L]], states = states)
    }
    toy <- data.table(arc_id = 1:5, way_index = c(1L, 2L, 2L, 3L, 2L), from = c(1L, 2L, 3L, 4L, 3L), to = c(2L, 3L, 4L, 5L, 2L))
    tests <- data.table(test = c("mph conversion", "reject symbolic speed", "unique continuous via path", "missing directed link", "absent member", "ambiguous path"), passed = c(abs(parse_speed("30 mph") - 48.28032) < 1e-08, is.na(parse_speed("signals")), identical(find_sequence(toy, 1:3)$path, 1:4), find_sequence(toy[arc_id != 3L], 1:3)$status == "NO_CONTINUOUS_SEQUENCE", find_sequence(toy, c(1L, 8L, 3L))$status == "MEMBER_ABSENT", find_sequence(rbind(toy, data.table(arc_id = 6:7, way_index = 2L, 
        from = c(2L, 6L), to = c(6L, 4L))), 1:3)$status == "AMBIGUOUS_SEQUENCE"))
    stopifnot(all(tests$passed))
    print(tests)
    scenarios <- c("CORE_PRIMARY", "EXPANDED", "CORE_STRICT_BARRIERS")
    expected <- c(7065687L, 8321285L, 7065335L)
    files <- character()
    summaries <- list()
    audits <- list()
    speed_summaries <- list()
    for (ii in seq_along(scenarios)) {
        nm <- scenarios[ii]
        cat("\nPROCESSING", nm, "\n")
        a <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_arcs.rds"))))
        tr <- readRDS(file.path(turn_dir, paste0(nm, "_turn_resolution.rds")))
        stopifnot(nrow(a) == expected[ii], identical(as.integer(a$arc_id), seq_len(nrow(a))), all(a$length_m > 0), all(a$arc_direction %in% c("FORWARD", "REVERSE")), all(a$way_index %in% ways$way_index))
        rr <- as.data.table(tr$relation_resolution)
        complex <- rr[relation_structure == "COMPLEX_VIA_WAY"]
        stopifnot(nrow(rr) == 154L, nrow(complex) == 33L)
        ref <- unique(members[relation_id %chin% complex$relation_id & member_type %in% c("w", "way"), member_ref])
        wi <- match(ref, ways$way_id)
        wi <- wi[!is.na(wi)]
        small <- a[way_index %in% wi, .(arc_id, way_index, from, to)]
        seqs <- list()
        ar <- list()
        for (i in seq_len(nrow(complex))) {
            rid <- complex$relation_id[i]
            m <- members[relation_id == rid]
            fw <- m[member_role == "from" & member_type %in% c("w", "way"), member_ref]
            vw <- m[member_role == "via" & member_type %in% c("w", "way"), member_ref]
            tw <- m[member_role == "to" & member_type %in% c("w", "way"), member_ref]
            typ <- complex$effective_restriction[i]
            if (complex$applicability[i] != "APPLICABLE") 
                ans <- list(status = complex$applicability[i], path = integer(), states = 0L)
            else if (length(fw) != 1L || length(tw) != 1L || length(vw) < 1L || nrow(m) != length(c(fw, vw, tw))) 
                ans <- list(status = "UNSUPPORTED_MEMBER_PATTERN", path = integer(), states = 0L)
            else if (!all(c(fw, vw, tw) %in% ways$way_id)) 
                ans <- list(status = "MEMBER_ABSENT", path = integer(), states = 0L)
            else ans <- find_sequence(small, match(c(fw, vw, tw), ways$way_id))
            if (ans$status == "UNIQUE_SEQUENCE") {
                z <- ans$path
                stopifnot(length(z) >= 3L, all(a$to[z[-length(z)]] == a$from[z[-1L]]))
                observed <- rle(a$way_index[z])$values
                stopifnot(identical(as.integer(observed), as.integer(match(c(fw, vw, tw), ways$way_id))))
                seqs[[rid]] <- list(relation_id = rid, restriction = typ, rule_type = if (startsWith(typ, "only_")) "REQUIRE_CONTINUATION" else "FORBID_COMPLETE_SEQUENCE", arc_ids = z)
            }
            ar[[i]] <- data.table(scenario = nm, relation_id = rid, restriction = typ, previous_status = complex$resolution_status[i], status = ans$status, arc_count = length(ans$path), states = ans$states)
        }
        audit <- rbindlist(ar)
        audits[[nm]] <- audit
        simple <- as.data.table(tr$unique_forbidden_transitions)
        stopifnot(nrow(simple) == c(115L, 120L, 115L)[ii], all(a$to[simple$incoming_arc] == a$from[simple$outgoing_arc]))
        rule_file <- file.path(out, paste0(nm, "_turn_rules.rds"))
        saveRDS(list(scenario = nm, simple_forbidden_pairs = simple, complex_rules = seqs, complex_audit = audit, simple_resolution = rr[relation_structure == "SIMPLE_VIA_NODE"], enforcement_status = "COMPILED_NOT_ROUTER_ENFORCED"), rule_file)
        files <- c(files, rule_file)
        costs <- data.table(arc_id = a$arc_id)
        rev <- a$arc_direction == "REVERSE"
        idx <- a$way_index
        cap <- ifelse(rev, cap_reverse[idx], cap_forward[idx])
        for (s in c("slow", "baseline", "favorable")) {
            v <- ifelse(rev, speed_profile[[paste0(s, "_reverse")]][idx], speed_profile[[paste0(s, "_forward")]][idx])
            t <- 3.6 * a$length_m/v
            stopifnot(all(is.finite(v) & v > 0), all(is.finite(t) & t > 0), all(is.na(cap) | v <= cap + 1e-10))
            costs[[paste0(s, "_seconds")]] <- t
            speed_summaries[[length(speed_summaries) + 1L]] <- data.table(scenario = nm, speed_scenario = s, arcs = nrow(a), capped_arcs = sum(!is.na(cap)), mean_arc_speed_kmh = mean(v), length_weighted_speed_kmh = sum(a$length_m)/sum(a$length_m/v), minimum_speed_kmh = min(v), maximum_speed_kmh = max(v), total_arc_hours = sum(t)/3600)
        }
        stopifnot(all(costs$slow_seconds >= costs$baseline_seconds - 1e-10), all(costs$baseline_seconds >= costs$favorable_seconds - 1e-10))
        cost_file <- file.path(out, paste0(nm, "_arc_travel_costs.rds"))
        saveRDS(costs, cost_file)
        files <- c(files, cost_file)
        summaries[[nm]] <- data.table(scenario = nm, arcs = nrow(a), simple_pairs = nrow(simple), complex_relations = nrow(audit), unique_complex_sequences = length(seqs), unresolved_complex = sum(audit$status != "UNIQUE_SEQUENCE"), unresolved_mandatory = sum(audit$status != "UNIQUE_SEQUENCE" & startsWith(audit$restriction, "only_")), cost_order_verified = TRUE)
        print(summaries[[nm]])
        print(audit[, .N, by = status])
        rm(a, small, costs, cap, idx, rev, v, t, tr)
        gc()
    }
    summary <- rbindlist(summaries)
    audit <- rbindlist(audits)
    ss <- rbindlist(speed_summaries)
    stopifnot(nrow(summary) == 3L, nrow(audit) == 99L, nrow(ss) == 9L, all(summary$unique_complex_sequences + summary$unresolved_complex == 33L), all(hash(p(inputs$relative_path)) == inputs$SHA256))
    profile_file <- file.path(out, "ghana_way_speed_profiles.rds")
    saveRDS(speed_profile, profile_file)
    files <- c(files, profile_file)
    assumptions <- data.table(item = c("Speed assumptions", "Link roads", "Surface adjustment", "Speed caps", "Unknown directional caps", "Turn sequences", "Mandatory missing members", "Routing enforcement", "Vehicle dimensions", "Travel time scope"), treatment = c("Hypothetical operating speeds, not observations or statutory limits", "75% of corresponding main class", "Worst applicable condition factor; missing condition explicitly modelled", "Valid directional tag overrides general; numeric km/h or mph only", 
        "Fallback to valid general cap; unparsed values audited", "Unique directed path through ordered via ways; ambiguous or capped searches unresolved", "Explicitly unresolved; not assumed permitted", "Rules compiled only; shortest-path engine still required", "Light motorcar proxy; heavy vehicle clearance not certified", "Arc movement seconds only; no junction delay, congestion or connector travel"))
    checks <- data.table(check = c("Previous manifests verified and unchanged", "Previous quality gates passed", "Synthetic compiler and parsing tests", "Three scenarios retained", "99 complex scenario relations retained", "All complex outcomes reconciled", "350 original simple pairs retained", "Nine speed summaries", "All cost orders verified", "All output RDS files exist"), passed = c(all(hash(p(inputs$relative_path)) == inputs$SHA256), all(q$passed) && all(q2$passed), all(tests$passed), nrow(summary) == 
        3L, nrow(audit) == 99L, all(summary$unique_complex_sequences + summary$unresolved_complex == 33L), sum(summary$simple_pairs) == 350L, nrow(ss) == 9L, all(summary$cost_order_verified), all(file.exists(files))))
    stopifnot(all(checks$passed))
    tables <- list(`10_Tables/phase8C2_scenario_summary.csv` = summary, `10_Tables/phase8C2_speed_summary.csv` = ss, `10_Tables/phase8C2_speed_design.csv` = speed_design, `10_Tables/phase8C2_surface_factors.csv` = factors, `03_Data_Audit/phase8C2_complex_sequence_audit.csv` = audit, `03_Data_Audit/phase8C2_unresolved_complex.csv` = audit[status != "UNIQUE_SEQUENCE"], `03_Data_Audit/phase8C2_speed_tag_audit.csv` = speed_audit, `03_Data_Audit/phase8C2_synthetic_tests.csv` = tests, `03_Data_Audit/phase8C2_quality_gate.csv` = checks, 
        `12_Reproducibility/phase8C2_modelling_assumptions.csv` = assumptions)
    for (nm in names(tables)) fwrite(tables[[nm]], p(nm))
    files <- c(files, p(names(tables)))
    draw <- function(which) {
        if (which == 1L) {
            par(mar = c(8, 5, 4, 1))
            barplot(t(as.matrix(speed_design[, .(slow, baseline, favorable)])), beside = TRUE, names.arg = speed_design$road_class, las = 2, col = c("#D95F02", "#2166AC", "#1B9E77"), border = NA, ylim = c(0, 120), ylab = "Assumed speed (km/h)", main = "Road-class operating speed scenarios")
            legend("topright", c("Slow", "Baseline", "Favorable"), fill = c("#D95F02", "#2166AC", "#1B9E77"), bty = "n")
        }
        else {
            par(mar = c(6, 5, 4, 1))
            b <- barplot(rbind(summary$unique_complex_sequences, summary$unresolved_complex), names.arg = c("Core", "Expanded", "Strict barriers"), col = c("#2166AC", "#D95F02"), border = NA, ylim = c(0, 43), ylab = "Complex turn relations", main = "Complex turn-sequence compilation")
            legend("topright", c("Unique sequence", "Unresolved"), fill = c("#2166AC", "#D95F02"), bty = "n")
            text(b, summary$unique_complex_sequences/2, summary$unique_complex_sequences, col = "white")
            text(b, 33 - summary$unresolved_complex/2, ifelse(summary$unresolved_complex > 0, summary$unresolved_complex, ""), col = "white")
        }
    }
    stems <- c("Figure_29_Road_Speed_Scenarios", "Figure_30_Complex_Turn_Sequence_Resolution")
    for (i in 1:2) for (ext in c("png", "pdf")) {
        f <- p("09_Figures", paste0(stems[i], ".", ext))
        if (ext == "png") 
            png(f, width = 12, height = 8, units = "in", res = 400, bg = "white")
        else pdf(f, width = 12, height = 8)
        tryCatch(draw(i), finally = dev.off())
        files <- c(files, f)
    }
    session <- p("12_Reproducibility", "phase8C2_R_session_info.txt")
    writeLines(capture.output(sessionInfo()), session)
    files <- c(files, session)
    script <- p("13_R_Scripts", "phase8C2_complete.R")
    code <- c("phase8C2_run <-", deparse(phase8C2_run, width.cutoff = 500L), "phase8C2_results <- phase8C2_run()")
    parse(text = code)
    writeLines(code, script)
    files <- c(files, script)
    files <- normalizePath(unique(files), winslash = "/", mustWork = TRUE)
    stopifnot(all(file.info(files)$size > 0))
    mf <- data.table(relative_path = substring(files, nchar(root) + 2L), size_bytes = file.info(files)$size, SHA256 = hash(files))
    fwrite(mf, p("12_Reproducibility", "phase8C2_output_checksum_manifest.csv"))
    mf[, `:=`(current_SHA256, hash(p(relative_path)))]
    mf[, `:=`(verified, SHA256 == current_SHA256)]
    stopifnot(all(mf$verified), all(hash(p(inputs$relative_path)) == inputs$SHA256))
    fwrite(mf, p("12_Reproducibility", "phase8C2_checksum_recheck.csv"))
    log <- c("PHASE 8C2 COST AND RULE COMPILATION PASS", paste("Quality checks:", sum(checks$passed), "/", nrow(checks)), paste("Checksums:", sum(mf$verified), "/", nrow(mf)), paste("Execution seconds:", round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)), "TURN-AWARE ROUTER ENFORCEMENT PENDING")
    writeLines(log, p("16_Research_Logs", "phase8C2_execution_log.txt"))
    print(summary)
    print(audit[, .N, by = .(scenario, status)])
    print(ss)
    print(checks)
    cat(paste(log, collapse = "\n"), "\n")
    invisible(list(summary = summary, complex_audit = audit, speed_summary = ss, speed_tag_audit = speed_audit, tests = tests, checks = checks, manifest = mf))
}
phase8C2_results <- phase8C2_run()
