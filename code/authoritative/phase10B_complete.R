phase10B_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(digest)
        library(sf)
    })
    start <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    P <- function(...) file.path(root, ...)
    hash <- function(x) vapply(x, function(z) digest(file = z, algo = "sha256"), character(1), USE.NAMES = FALSE)
    old <- rbindlist(lapply(c("9A", "9B1", "9B2", "10A"), function(s) {
        m <- fread(P("12_Reproducibility", paste0("phase", s, "_output_checksum_manifest.csv")))
        stopifnot(all(hash(P(m$relative_path)) == m$SHA256))
        cat("Phase", s, ":", nrow(m), "checksums PASS\n")
        m[, .(relative_path, SHA256)]
    }))
    out <- P("06_Optimization", "Phase10B_Candidates")
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    paths <- character()
    write <- function(x, p) {
        fwrite(x, p)
        paths <<- c(paths, p)
    }
    save <- function(x, p) {
        saveRDS(x, p)
        paths <<- c(paths, p)
    }
    nat <- fread(P("10_Tables", "phase10A_national_accessibility.csv"))[run == "PRIMARY"]
    reg <- fread(P("10_Tables", "phase10A_regional_accessibility.csv"))[run == "PRIMARY"]
    dis <- fread(P("10_Tables", "phase10A_primary_district_accessibility.csv"))
    loc <- as.data.table(readRDS(P("08_Results", "Phase10A_Accessibility", "primary_location_probabilities.rds")))
    idx <- readRDS(P("05_Travel_Time_Matrices", "Phase9B1_Core_Baseline", "matrix_index.rds"))
    d <- as.data.table(idx$demand)
    f <- as.data.table(idx$facilities)
    stopifnot(nrow(nat) == 12L, nrow(reg) == 204L, nrow(dis) == 3132L, nrow(loc) == 51014L, !anyDuplicated(loc$demand_id), !anyDuplicated(d$demand_id), setequal(loc$demand_id, d$demand_id), sum(d$match_1km) == 31002L)
    loc <- loc[match(d$demand_id, loc$demand_id)]
    stopifnot(identical(loc$demand_id, d$demand_id), all(abs(loc$population - d$population) < 1e-08), identical(loc$match_1km, d$match_1km))
    inequality <- function(x, w) {
        stopifnot(length(x) == length(w), all(is.finite(x) & x >= 0 & x <= 1 + 1e-10), all(w > 0))
        w <- w/sum(w)
        mu <- sum(w * x)
        o <- order(x)
        xx <- x[o]
        ww <- w[o]
        L <- cumsum(ww * xx)
        g <- if (mu > 0) 
            sum(ww * (2 * L - ww * xx))/mu - 1
        else NA_real_
        g <- if (mu > 0) 
            1 - sum(ww * (2 * L - ww * xx))/mu
        else NA_real_
        t <- if (mu > 0) {
            z <- x/mu
            sum(w[z > 0] * z[z > 0] * log(z[z > 0]))
        }
        else NA_real_
        c(mean_pct = 100 * mu, min_pct = 100 * min(x), max_pct = 100 * max(x), gap_pp = 100 * diff(range(x)), weighted_sd_pp = 100 * sqrt(sum(w * (x - mu)^2)), gini = g, theil = t)
    }
    stopifnot(abs(inequality(c(1, 1), c(1, 1))["gini"]) < 1e-12, abs(inequality(c(0, 1), c(1, 1))["gini"] - 0.5) < 1e-12, abs(inequality(c(0, 1), c(1, 1))["theil"] - log(2)) < 1e-12)
    metrics <- unique(nat$modality)
    mins <- c(30, 60, 120)
    recon <- list()
    ineq <- list()
    def <- list()
    for (m in metrics) for (h in mins) {
        col <- paste(m, h, sep = "_")
        x <- loc[[col]]
        stopifnot(length(x) == nrow(d), all(is.finite(x) & x >= 0 & x <= 1), all(x[!d$match_1km] == 0))
        z <- data.table(region = d$region_group, district = as.character(d$district_code), population = d$population, covered = d$population * x, matched = d$population * d$match_1km)
        z[region == "UNASSIGNED" | is.na(district) | district == "", `:=`(district, "UNASSIGNED")]
        rr <- z[, .(population = sum(population), covered = sum(covered), matched = sum(matched)), by = region]
        dd <- z[, .(population = sum(population), covered = sum(covered), matched = sum(matched)), by = .(region, district)]
        for (level in c("REGION", "DISTRICT")) {
            a <- if (level == "REGION") 
                rr
            else dd
            b <- copy(a)
            b[, `:=`(mean_pct = 100 * covered/population, expected_deficit = population - covered, unmatched_population = population - matched, matched_deficit = matched - covered, modality = m, threshold_minutes = h, level = level)]
            if (level == "REGION") 
                b[, `:=`(district, NA_character_)]
            def[[length(def) + 1L]] <- b
            u <- b[region != "UNASSIGNED"]
            for (weighting in c("POPULATION", "EQUAL_AREA")) {
                v <- inequality(u$mean_pct/100, if (weighting == "POPULATION") 
                  u$population
                else rep(1, nrow(u)))
                ineq[[length(ineq) + 1L]] <- cbind(data.table(level = level, weighting = weighting, modality = m, threshold_minutes = h, areas = nrow(u)), as.data.table(as.list(v)))
            }
            ref <- if (level == "REGION") 
                reg[modality == m & threshold_minutes == h]
            else dis[modality == m & threshold_minutes == h]
            key <- if (level == "REGION") 
                "region"
            else c("region", "district")
            q <- merge(b, ref[, c(key, "mean_pct", "population"), with = FALSE], by = key, suffixes = c("_new", "_old"))
            stopifnot(nrow(q) == nrow(b), nrow(q) == nrow(ref))
            recon[[length(recon) + 1L]] <- data.table(level = level, modality = m, threshold_minutes = h, max_pct_error = max(abs(q$mean_pct_new - q$mean_pct_old)), max_pop_error = max(abs(q$population_new - q$population_old)))
        }
        stopifnot(abs(100 * sum(z$covered)/sum(z$population) - nat[modality == m & threshold_minutes == h, mean_pct]) < 1e-06)
    }
    rec <- rbindlist(recon)
    stopifnot(all(rec$max_pct_error < 1e-06), all(rec$max_pop_error < 1e-05))
    ineq <- rbindlist(ineq)
    def <- rbindlist(def, use.names = TRUE, fill = TRUE)
    stopifnot(all(def$matched_deficit >= -1e-05), all(abs(def$expected_deficit - def$matched_deficit - def$unmatched_population) < 1e-05))
    h <- f[match_1km == TRUE & supply_candidate == TRUE, .(candidate_id = paste0("H_", id), candidate_type = "HEALTHCARE_PROXY", origin_id = as.character(id), longitude, latitude, vertex_id, node_id, connector_m, weak_component)]
    z <- copy(d[match_1km == TRUE])
    xy <- sf_project("EPSG:4326", "EPSG:32630", cbind(z$routing_longitude, z$routing_latitude), authority_compliant = FALSE)
    z[, `:=`(cell_x = floor(xy[, 1]/10000), cell_y = floor(xy[, 2]/10000))]
    setorder(z, -population, demand_id)
    sel <- unique(rbindlist(list(z[!duplicated(z, by = c("cell_x", "cell_y"))], z[!duplicated(z, by = c("region_group", "district_code"))])), by = "demand_id")
    s <- sel[, .(candidate_id = paste0("P_", demand_id), candidate_type = "POPULATION_PROXY", origin_id = as.character(demand_id), longitude = routing_longitude, latitude = routing_latitude, vertex_id, node_id, connector_m, weak_component)]
    cands <- rbindlist(list(h, s))
    stopifnot(!anyDuplicated(cands$candidate_id), all(cands$connector_m <= 1000), all(cands$connector_m >= 0), !anyNA(cands$vertex_id))
    cands[, `:=`(feasibility = "UNVERIFIED", source_in_primary_matrix = vertex_id %in% idx$sources)]
    adm <- st_read(P("04_Spatial_Analysis", "Administrative_Boundaries", "gha_admin2.geojson"), quiet = TRUE)
    stopifnot(all(c("adm1_name", "adm2_pcode") %in% names(adm)))
    adm <- st_make_valid(st_transform(adm, 32630))
    pts <- st_transform(st_as_sf(cands, coords = c("longitude", "latitude"), crs = 4326), 32630)
    hits <- st_intersects(pts, adm)
    j <- vapply(hits, function(x) if (length(x) == 1L) 
        x
    else NA_integer_, integer(1))
    cands[, `:=`(region = toupper(as.character(adm$adm1_name[j])), district = as.character(adm$adm2_pcode[j]), admin_match_count = lengths(hits))]
    cands[, `:=`(candidate_eligible, admin_match_count == 1L)]
    setorder(cands, vertex_id, connector_m, candidate_id)
    cands[, `:=`(routing_representative, FALSE)]
    ix <- which(cands$candidate_eligible)
    repix <- ix[!duplicated(cands$vertex_id[ix])]
    cands[repix, `:=`(routing_representative, TRUE)]
    cands[, `:=`(representative_id, cands$candidate_id[repix][match(vertex_id, cands$vertex_id[repix])])]
    active <- cands[routing_representative == TRUE]
    stopifnot(!anyDuplicated(active$vertex_id), all(active$candidate_eligible), all(active$feasibility == "UNVERIFIED"))
    cs <- cands[, .(candidates = .N, eligible = sum(candidate_eligible), representatives = sum(routing_representative), new_routing_vertices = sum(routing_representative & !source_in_primary_matrix)), by = .(region, candidate_type)]
    write(ineq, P("10_Tables", "phase10B_inequality_indices.csv"))
    write(def, P("10_Tables", "phase10B_population_deficits.csv"))
    write(rec, P("03_Data_Audit", "phase10B_accessibility_reconciliation.csv"))
    write(cands, file.path(out, "mobile_candidate_inventory.csv"))
    save(cands, file.path(out, "mobile_candidate_inventory.rds"))
    save(active, file.path(out, "mobile_routing_candidates.rds"))
    write(cs, P("10_Tables", "phase10B_candidate_summary.csv"))
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
    fig("Figure_37_Regional_Expected_Deficits", function() {
        a <- def[level == "REGION" & region != "UNASSIGNED" & threshold_minutes == 60 & modality %in% c("XRAY", "ULTRASOUND")]
        q <- dcast(a, region ~ modality, value.var = "expected_deficit")
        setorder(q, XRAY)
        par(mar = c(5, 10, 3, 2))
        barplot(t(as.matrix(q[, .(XRAY, ULTRASOUND)]))/1e+06, beside = TRUE, horiz = TRUE, names.arg = q$region, las = 1, col = c("#2166AC", "#1B9E77"), xlab = "Expected population not covered (millions)", main = "Regional expected imaging coverage deficits: 60 minutes")
        legend("bottomright", c("X-ray", "Ultrasound"), fill = c("#2166AC", "#1B9E77"), bty = "n")
    })
    fig("Figure_38_Between_Area_Inequality", function() {
        a <- ineq[weighting == "POPULATION" & modality %in% c("XRAY", "ULTRASOUND")]
        par(mar = c(5, 5, 3, 2))
        plot(NA, xlim = c(30, 120), ylim = c(0, max(a$gini) * 1.12), xlab = "Travel-time threshold (minutes)", ylab = "Population-weighted between-area Gini", main = "Geographical inequality in expected coverage", xaxt = "n")
        axis(1, at = mins)
        for (i in 1:2) for (k in 1:2) {
            q <- a[level == c("REGION", "DISTRICT")[i] & modality == c("XRAY", "ULTRASOUND")[k]]
            setorder(q, threshold_minutes)
            lines(q$threshold_minutes, q$gini, type = "b", pch = k, lty = i, col = c("#2166AC", "#1B9E77")[k], lwd = 2)
        }
        legend("topright", c("Region X-ray", "Region ultrasound", "District X-ray", "District ultrasound"), col = rep(c("#2166AC", "#1B9E77"), 2), lty = rep(1:2, each = 2), pch = rep(1:2, 2), bty = "n")
    })
    fig("Figure_39_Hypothetical_Mobile_Candidates", function() {
        par(mar = c(2, 2, 3, 2))
        plot(st_geometry(st_transform(adm, 4326)), col = "grey95", border = "grey75", main = "Hypothetical mobile imaging host locations")
        for (k in 1:2) {
            q <- active[candidate_type == c("HEALTHCARE_PROXY", "POPULATION_PROXY")[k]]
            points(q$longitude, q$latitude, pch = 16, cex = 0.45, col = c("#2166AC", "#D95F02")[k])
        }
        legend("bottomleft", c("Healthcare proxy", "Population proxy"), pch = 16, col = c("#2166AC", "#D95F02"), bty = "n")
    })
    assumptions <- data.table(item = c("Inequality", "Unassigned", "Candidate hosts", "Grid", "Deduplication", "Feasibility", "Routing"), treatment = c("Between-area inequality of mean scenario coverage; not individual realized inequality", "Excluded from named-area indices, retained in national deficit", "Matched supply-eligible healthcare proxies plus population representatives; not verified host sites", "10 km projected cells plus one representative per eligible district; selected by population not supply draws", 
        "Minimum connector at same vertex dominates only for equal-service coverage model; all candidates retained", "No physical clearance, power, licensing, staffing, demand or capacity verification", "New vertices require targeted reverse routing before optimization"))
    write(assumptions, P("12_Reproducibility", "phase10B_modelling_assumptions.csv"))
    gate <- data.table(check = c("Prior checksums unchanged", "Primary location alignment", "All 24 area reconciliations", "48 inequality records", "Deficit decomposition", "Unique candidate IDs", "Unique representative vertices", "All representatives administratively assigned", "Candidate threshold", "Six figures"), passed = c(all(hash(P(old$relative_path)) == old$SHA256), TRUE, nrow(rec) == 24L && all(rec$max_pct_error < 1e-06), nrow(ineq) == 48L, TRUE, !anyDuplicated(cands$candidate_id), !anyDuplicated(active$vertex_id), 
        all(active$admin_match_count == 1L), all(active$connector_m <= 1000), sum(grepl("\\.(png|pdf)$", paths)) == 6))
    stopifnot(all(gate$passed))
    write(gate, P("03_Data_Audit", "phase10B_quality_gate.csv"))
    sp <- P("13_R_Scripts", "phase10B_complete.R")
    writeLines(c("phase10B_run <-", deparse(phase10B_run, width.cutoff = 500L), "phase10B_results <- phase10B_run()"), sp)
    paths <- c(paths, sp)
    paths <- sort(unique(normalizePath(paths, winslash = "/", mustWork = TRUE)))
    man <- data.table(relative_path = substring(paths, nchar(root) + 2), SHA256 = hash(paths), size_bytes = file.info(paths)$size)
    fwrite(man, P("12_Reproducibility", "phase10B_output_checksum_manifest.csv"))
    man[, `:=`(verified, hash(P(relative_path)) == SHA256)]
    stopifnot(all(man$verified))
    fwrite(man, P("12_Reproducibility", "phase10B_checksum_recheck.csv"))
    print(ineq[threshold_minutes == 60 & weighting == "POPULATION"])
    print(def[level == "REGION" & threshold_minutes == 60 & modality == "XRAY"][order(-expected_deficit), .(region, mean_pct, expected_deficit, unmatched_population)])
    print(cands[, .(candidates = .N, eligible = sum(candidate_eligible), representatives = sum(routing_representative), new_vertices = sum(routing_representative & !source_in_primary_matrix)), by = candidate_type])
    print(gate)
    cat("PHASE 10B PASS\nChecksums:", sum(man$verified), "/", nrow(man), "\nElapsed seconds:", as.numeric(difftime(Sys.time(), start, units = "secs")), "\nNEW CANDIDATE ROUTING AND OPTIMIZATION NOT YET PERFORMED\n")
    invisible(list(inequality = ineq, deficits = def, candidates = cands, quality = gate))
}
phase10B_results <- phase10B_run()
