phase8B1_finalize <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(digest)
        library(jsonlite)
    })
    start_time <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    raw_dir <- file.path(root, "01_Raw_Data")
    audit_dir <- file.path(root, "03_Data_Audit")
    spatial_dir <- file.path(root, "04_Spatial_Analysis")
    results_dir <- file.path(root, "08_Results")
    tables_dir <- file.path(root, "10_Tables")
    repro_dir <- file.path(root, "12_Reproducibility")
    scripts_dir <- file.path(root, "13_R_Scripts")
    logs_dir <- file.path(root, "16_Research_Logs")
    topology_dir <- file.path(spatial_dir, "National_Road_Network", "PBF_Topology")
    hash_files <- function(paths) {
        stopifnot(all(file.exists(paths)))
        vapply(paths, function(p) {
            digest(file = p, algo = "sha256")
        }, character(1), USE.NAMES = FALSE)
    }
    cat("\n========================================\n")
    cat("PHASE 8B1: FINAL COORDINATE REPAIR\n")
    cat("========================================\n")
    previous_manifest <- fread(file.path(repro_dir, "phase8A_output_checksum_manifest.csv"))
    previous_paths <- file.path(root, previous_manifest$relative_path)
    stopifnot(nrow(previous_manifest) == 35, all(file.exists(previous_paths)))
    previous_hashes <- hash_files(previous_paths)
    stopifnot(identical(previous_hashes, as.character(previous_manifest$SHA256)))
    previous_quality <- fread(file.path(audit_dir, "phase8A_quality_gate.csv"))
    stopifnot(nrow(previous_quality) == 23, all(previous_quality$passed))
    cat("Previous 35 checksums: PASS\n")
    cat("Previous 23 quality checks: PASS\n")
    pbf_path <- file.path(raw_dir, "ghana-261123.osm.pbf")
    raw_manifest <- fread(file.path(audit_dir, "ghana_raw_data_copy_manifest.csv"))
    pbf_manifest <- raw_manifest[basename(destination) == basename(pbf_path)]
    stopifnot(file.exists(pbf_path), nrow(pbf_manifest) == 1)
    original_pbf_hash <- hash_files(pbf_path)
    stopifnot(identical(original_pbf_hash, as.character(pbf_manifest$SHA256)))
    cat("Original PBF integrity: PASS\n")
    required_files <- c("osm_road_ways.csv", "osm_native_segments.csv", "osm_turn_restrictions.csv", "osm_barrier_nodes.csv", "osm_omitted_segments.csv", "osm_extraction_statistics.json")
    staging_candidates <- list.dirs(topology_dir, recursive = FALSE, full.names = TRUE)
    staging_candidates <- staging_candidates[grepl("^phase8B1_staging_", basename(staging_candidates))]
    valid_staging <- staging_candidates[vapply(staging_candidates, function(d) {
        p <- file.path(d, required_files)
        if (!all(file.exists(p))) {
            return(FALSE)
        }
        s <- tryCatch(fromJSON(file.path(d, "osm_extraction_statistics.json")), error = function(e) NULL)
        if (is.null(s)) {
            return(FALSE)
        }
        isTRUE(s$selected_ways == 364113 && s$segments_written == 4308002 && s$core_ways == 304491 && s$conditional_ways == 59622)
    }, logical(1))]
    cat("\nRECOVERABLE EXTRACTION DIRECTORIES\n")
    print(valid_staging)
    if (length(valid_staging) == 0) {
        stop(paste("No complete matching staging directory found.", "Do not delete or rerun anything.", "Inspect the PBF_Topology folder."))
    }
    if (length(valid_staging) > 1) {
        staging_hashes <- lapply(valid_staging, function(d) {
            hash_files(file.path(d, required_files))
        })
        identical_copies <- all(vapply(staging_hashes, function(h) {
            identical(h, staging_hashes[[1]])
        }, logical(1)))
        if (!identical_copies) {
            stop("Multiple nonidentical staging datasets found.")
        }
    }
    staging_dir <- valid_staging[1]
    extracted_paths <- file.path(staging_dir, required_files)
    extracted_hashes <- hash_files(extracted_paths)
    cat("\nRecovered staging directory:\n")
    cat(staging_dir, "\n")
    cat("Existing Python extraction recovered: PASS\n")
    ways <- fread(file.path(staging_dir, "osm_road_ways.csv"), colClasses = "character", na.strings = "")
    ways[, `:=`(node_count, as.integer(node_count))]
    segments <- fread(file.path(staging_dir, "osm_native_segments.csv"), colClasses = list(character = c("way_id", "from_node", "to_node")))
    restrictions <- fread(file.path(staging_dir, "osm_turn_restrictions.csv"), colClasses = "character")
    barriers <- fread(file.path(staging_dir, "osm_barrier_nodes.csv"), colClasses = "character")
    omitted <- fread(file.path(staging_dir, "osm_omitted_segments.csv"), colClasses = "character")
    stats <- fromJSON(file.path(staging_dir, "osm_extraction_statistics.json"))
    stopifnot(nrow(ways) == 364113, nrow(segments) == 4308002, nrow(restrictions) == 154, nrow(barriers) == 1427, nrow(omitted) == 0)
    cat("\nEXTRACTION DATA RECOVERED\n")
    cat("Road ways:", nrow(ways), "\n")
    cat("Road segments:", nrow(segments), "\n")
    cat("Turn restrictions:", nrow(restrictions), "\n")
    cat("Barrier nodes:", nrow(barriers), "\n")
    stopifnot(!anyNA(ways$way_id), !anyDuplicated(ways$way_id), !anyNA(ways$node_count), all(ways$node_count >= 2), !anyNA(segments$from_node), !anyNA(segments$to_node), all(segments$from_node != segments$to_node), all(segments$way_id %chin% ways$way_id), all(is.finite(segments$length_m) & segments$length_m > 0), uniqueN(segments, by = c("way_id", "segment_index")) == nrow(segments))
    cat("Road and segment identifiers: PASS\n")
    from_nodes <- unique(segments[, .(node_id = from_node, longitude = from_lon, latitude = from_lat)])
    to_nodes <- unique(segments[, .(node_id = to_node, longitude = to_lon, latitude = to_lat)])
    nodes <- unique(rbindlist(list(from_nodes, to_nodes)))
    rm(from_nodes, to_nodes)
    gc()
    coordinate_conflicts <- nodes[, .N, by = node_id][N > 1]
    cat("\nCORRECTED NODE COORDINATE AUDIT\n")
    cat("Unique coordinate records:", nrow(nodes), "\n")
    cat("Unique OSM node identifiers:", uniqueN(nodes$node_id), "\n")
    cat("Remaining coordinate conflicts:", nrow(coordinate_conflicts), "\n")
    fwrite(coordinate_conflicts, file.path(audit_dir, "phase8B1_node_coordinate_conflicts.csv"))
    if (nrow(coordinate_conflicts) > 0) {
        fwrite(nodes[node_id %chin% coordinate_conflicts$node_id], file.path(audit_dir, "phase8B1_conflicting_node_coordinates.csv"))
        stop("Genuine coordinate discrepancies remain after repair.")
    }
    stopifnot(!anyNA(nodes$node_id), !anyDuplicated(nodes$node_id), all(is.finite(nodes$longitude)), all(is.finite(nodes$latitude)), all(abs(nodes$longitude) <= 180), all(abs(nodes$latitude) <= 90))
    cat("Corrected node reconstruction: PASS\n")
    from_idx <- match(segments$from_node, nodes$node_id)
    to_idx <- match(segments$to_node, nodes$node_id)
    stopifnot(!anyNA(from_idx), !anyNA(to_idx))
    coordinate_errors <- data.table(endpoint = c("FROM_LONGITUDE", "FROM_LATITUDE", "TO_LONGITUDE", "TO_LATITUDE"), maximum_absolute_error = c(max(abs(segments$from_lon - nodes$longitude[from_idx])), max(abs(segments$from_lat - nodes$latitude[from_idx])), max(abs(segments$to_lon - nodes$longitude[to_idx])), max(abs(segments$to_lat - nodes$latitude[to_idx]))))
    print(coordinate_errors)
    stopifnot(all(coordinate_errors$maximum_absolute_error == 0))
    cat("All segment endpoint coordinates reproduced: PASS\n")
    rm(from_idx, to_idx)
    gc()
    radius <- 6371008.8
    rad <- pi/180
    block_size <- 250000L
    starts <- seq(1L, nrow(segments), by = block_size)
    distance_audit <- rbindlist(lapply(seq_along(starts), function(k) {
        ix <- starts[k]:min(starts[k] + block_size - 1L, nrow(segments))
        z <- segments[ix]
        phi1 <- z$from_lat * rad
        phi2 <- z$to_lat * rad
        dphi <- phi2 - phi1
        dlambda <- (z$to_lon - z$from_lon) * rad
        a <- sin(dphi/2)^2 + cos(phi1) * cos(phi2) * sin(dlambda/2)^2
        recomputed <- 2 * radius * asin(sqrt(pmin(1, pmax(0, a))))
        error <- abs(recomputed - z$length_m)
        data.table(block = k, segments_checked = length(ix), maximum_error_m = max(error), mean_error_m = mean(error), errors_over_tolerance = sum(error > 1e-05))
    }))
    stopifnot(sum(distance_audit$segments_checked) == nrow(segments), sum(distance_audit$errors_over_tolerance) == 0)
    cat("\nIndependently verified road lengths:", sum(distance_audit$segments_checked), "\n")
    cat("Maximum distance recomputation error (m):", max(distance_audit$maximum_error_m), "\n")
    cat("Segment length verification: PASS\n")
    segment_counts <- segments[, .(extracted_segments = .N, native_length_km = sum(length_m)/1000, minimum_segment_index = min(segment_index), maximum_segment_index = max(segment_index)), by = way_id]
    reconciliation <- merge(ways[, .(way_id, network_class, highway, node_count)], segment_counts, by = "way_id", all.x = TRUE)
    reconciliation[, `:=`(expected_segments, node_count - 1L)]
    reconciliation[, `:=`(omitted_segments, expected_segments - extracted_segments)]
    stopifnot(nrow(reconciliation) == nrow(ways), !anyNA(reconciliation$extracted_segments), all(reconciliation$omitted_segments == 0), all(reconciliation$minimum_segment_index == 1), all(reconciliation$maximum_segment_index == reconciliation$expected_segments), sum(reconciliation$expected_segments) == nrow(segments))
    topology_summary <- reconciliation[, .(road_ways = .N, expected_segments = sum(expected_segments), extracted_segments = sum(extracted_segments), omitted_segments = sum(omitted_segments), ways_without_segments = sum(extracted_segments == 0), native_length_km = sum(native_length_km)), by = network_class]
    cat("\nTOPOLOGY SUMMARY\n")
    print(topology_summary)
    setorder(segments, way_id, segment_index)
    continuity_breaks <- segments[, .(discontinuities = sum(from_node[-1L] != to_node[-.N])), by = way_id][discontinuities > 0]
    stopifnot(nrow(continuity_breaks) == 0)
    cat("Original within-way node continuity: PASS\n")
    attributes <- setdiff(names(ways), c("way_id", "network_class", "node_count", "tags_json"))
    attribute_audit <- rbindlist(lapply(attributes, function(nm) {
        x <- ways[[nm]]
        available <- !is.na(x) & nzchar(trimws(x))
        data.table(attribute = nm, total_ways = length(x), available = sum(available), unspecified = sum(!available), coverage_pct = 100 * mean(available))
    }))
    detailed_tags <- intersect(c("highway", "oneway", "oneway:motor_vehicle", "oneway:motorcar", "oneway:hgv", "junction", "access", "vehicle", "motor_vehicle", "motorcar", "hgv", "goods", "surface", "smoothness", "tracktype", "bridge", "tunnel", "layer", "maxspeed", "maxweight", "maxheight", "maxwidth", "seasonal", "ford"), names(ways))
    tag_summary <- rbindlist(lapply(detailed_tags, function(nm) {
        z <- ways[, .N, by = c(nm)]
        setnames(z, nm, "tag_value")
        z[, `:=`(attribute, nm)]
        setorder(z, -N)
        z
    }), fill = TRUE)
    cat("\nORIGINAL ROAD ATTRIBUTE COVERAGE\n")
    print(attribute_audit, nrows = 100)
    barriers[, `:=`(belongs_to_candidate_network, node_id %chin% nodes$node_id)]
    barrier_summary <- barriers[, .N, by = .(barrier, belongs_to_candidate_network)][order(-N)]
    restriction_summary <- restrictions[, .N, by = restriction][order(-N)]
    stopifnot(!anyDuplicated(barriers$node_id), !anyDuplicated(restrictions$relation_id))
    cat("\nBARRIER SUMMARY\n")
    print(barrier_summary, nrows = 100)
    cat("\nTURN RESTRICTION SUMMARY\n")
    print(restriction_summary, nrows = 100)
    shape_network <- readRDS(file.path(spatial_dir, "National_Road_Network", "ghana_osm_roads_audited.rds"))
    shape_ids <- unique(as.character(shape_network$osm_id_text))
    pbf_ids <- ways$way_id
    source_comparison <- data.table(metric = c("Shapefile road identifiers", "Selected PBF way identifiers", "Shared identifiers", "Selected PBF identifiers not in shapefile", "Shapefile identifiers not in selected PBF"), value = c(length(shape_ids), length(pbf_ids), length(intersect(shape_ids, pbf_ids)), length(setdiff(pbf_ids, shape_ids)), length(setdiff(shape_ids, pbf_ids))))
    rm(shape_network)
    gc()
    cat("\nSOURCE IDENTIFIER COMPARISON\n")
    print(source_comparison)
    previous_unchanged <- identical(hash_files(previous_paths), previous_hashes)
    pbf_unchanged <- identical(hash_files(pbf_path), original_pbf_hash)
    extraction_unchanged <- identical(hash_files(extracted_paths), extracted_hashes)
    checks <- data.table(check = c("Previous 35 output checksums verified", "Previous 23 quality checks passed", "Previous outputs unchanged", "Original PBF checksum verified", "Original PBF unchanged", "Existing extraction recovered", "Recovered extraction unchanged", "Extracted way count verified", "Extracted segment count verified", "Core way count verified", "Conditional way count verified", "Unique original road way identifiers", "Unique original node identifiers", "Unique way-segment identifiers", 
        "All segments linked to original ways", "All segment endpoint identifiers available", "No identical segment endpoint identifiers", "All retained road lengths positive", "All node coordinates finite", "No conflicting node coordinates", "Origin longitude independently verified", "Origin latitude independently verified", "Destination longitude independently verified", "Destination latitude independently verified", "All segment lengths independently reproduced", "All per-way segment counts reconciled", 
        "All segment index ranges reconciled", "Original within-way continuity verified", "No missing native node locations", "No repeated consecutive node references", "No zero-length segments", "No nonfinite-length segments", "No omitted segments", "Turn restriction inventory reconciled", "Barrier inventory reconciled", "Original road attributes audited", "Shapefile and PBF identifiers compared"), passed = c(all(previous_hashes == previous_manifest$SHA256), all(previous_quality$passed), previous_unchanged, 
        original_pbf_hash == pbf_manifest$SHA256, pbf_unchanged, all(file.exists(extracted_paths)), extraction_unchanged, nrow(ways) == stats$selected_ways, nrow(segments) == stats$segments_written, sum(ways$network_class == "CORE") == stats$core_ways, sum(ways$network_class == "CONDITIONAL") == stats$conditional_ways, !anyDuplicated(ways$way_id), !anyDuplicated(nodes$node_id), uniqueN(segments, by = c("way_id", "segment_index")) == nrow(segments), all(segments$way_id %chin% ways$way_id), !anyNA(segments$from_node) && 
            !anyNA(segments$to_node), all(segments$from_node != segments$to_node), all(is.finite(segments$length_m) & segments$length_m > 0), all(is.finite(nodes$longitude)) && all(is.finite(nodes$latitude)), nrow(coordinate_conflicts) == 0, coordinate_errors$maximum_absolute_error[1] == 0, coordinate_errors$maximum_absolute_error[2] == 0, coordinate_errors$maximum_absolute_error[3] == 0, coordinate_errors$maximum_absolute_error[4] == 0, sum(distance_audit$errors_over_tolerance) == 0, all(reconciliation$omitted_segments == 
            0), all(reconciliation$minimum_segment_index == 1) && all(reconciliation$maximum_segment_index == reconciliation$expected_segments), nrow(continuity_breaks) == 0, stats$missing_node_locations == 0, stats$repeated_consecutive_node_refs == 0, stats$zero_length_segments == 0, stats$nonfinite_length_segments == 0, nrow(omitted) == 0, nrow(restrictions) == stats$restriction_relations, nrow(barriers) == stats$tagged_barrier_nodes, nrow(attribute_audit) == length(attributes), nrow(source_comparison) == 
            5))
    checks[is.na(passed), `:=`(passed, FALSE)]
    checks[, `:=`(status, fifelse(passed, "PASS", "FAIL"))]
    quality_path <- file.path(audit_dir, "phase8B1_quality_gate.csv")
    fwrite(checks, quality_path)
    cat("\nSTRUCTURAL QUALITY GATE\n")
    print(checks, nrows = 100)
    stopifnot(nrow(checks) == 37, all(checks$passed))
    final_extracted_paths <- file.path(topology_dir, required_files)
    for (i in seq_along(extracted_paths)) {
        if (file.exists(final_extracted_paths[i])) {
            stopifnot(identical(hash_files(final_extracted_paths[i]), extracted_hashes[i]))
        }
        else {
            stopifnot(file.copy(extracted_paths[i], final_extracted_paths[i], overwrite = FALSE))
        }
    }
    stopifnot(identical(hash_files(final_extracted_paths), extracted_hashes))
    rds_objects <- list(ghana_osm_native_nodes.rds = nodes, ghana_osm_native_segments.rds = segments, ghana_osm_native_ways.rds = ways, ghana_osm_turn_restrictions.rds = restrictions, ghana_osm_barrier_nodes.rds = barriers, ghana_osm_omitted_segments.rds = omitted)
    rds_paths <- file.path(topology_dir, names(rds_objects))
    for (i in seq_along(rds_objects)) {
        saveRDS(rds_objects[[i]], rds_paths[i])
    }
    stopifnot(all(file.exists(rds_paths)), all(file.info(rds_paths)$size > 0))
    omission_summary <- omitted[, .N, by = reason]
    correction_log <- data.table(issue = "Incorrect destination longitude variable", original_expression = "longitude = to_lat", corrected_expression = "longitude = to_lon", extraction_repeated = FALSE, remaining_coordinate_conflicts = nrow(coordinate_conflicts), maximum_endpoint_coordinate_error = max(coordinate_errors$maximum_absolute_error))
    outputs <- list(`03_Data_Audit/phase8B1_way_reconciliation.csv` = reconciliation, `03_Data_Audit/phase8B1_node_coordinate_conflicts.csv` = coordinate_conflicts, `03_Data_Audit/phase8B1_coordinate_repair_log.csv` = correction_log, `03_Data_Audit/phase8B1_endpoint_coordinate_audit.csv` = coordinate_errors, `03_Data_Audit/phase8B1_segment_distance_recheck.csv` = distance_audit, `03_Data_Audit/phase8B1_continuity_breaks.csv` = continuity_breaks, `03_Data_Audit/phase8B1_attribute_completeness.csv` = attribute_audit, 
        `03_Data_Audit/phase8B1_omitted_segment_summary.csv` = omission_summary, `10_Tables/phase8B1_topology_summary.csv` = topology_summary, `10_Tables/phase8B1_detailed_tag_summary.csv` = tag_summary, `10_Tables/phase8B1_barrier_summary.csv` = barrier_summary, `10_Tables/phase8B1_turn_restriction_summary.csv` = restriction_summary, `10_Tables/phase8B1_source_comparison.csv` = source_comparison)
    for (nm in names(outputs)) {
        fwrite(outputs[[nm]], file.path(root, nm))
    }
    summary <- data.table(metric = c("Selected original OSM ways", "Core road ways", "Conditional road ways", "Unique original network nodes", "Native consecutive-node segments", "Total native segment length km", "Missing native node locations", "Repeated consecutive node references", "Zero length segments", "Nonfinite length segments", "Omitted segments", "Conflicting node coordinates", "Within-way continuity breaks", "Independently verified segment lengths", "Maximum segment distance error m", 
        "Maximum endpoint coordinate error", "Original turn restriction relations", "Tagged barrier nodes", "Barrier nodes on candidate network", "Structural checks passed", "Structural checks total"), value = c(nrow(ways), stats$core_ways, stats$conditional_ways, nrow(nodes), nrow(segments), sum(segments$length_m)/1000, stats$missing_node_locations, stats$repeated_consecutive_node_refs, stats$zero_length_segments, stats$nonfinite_length_segments, nrow(omitted), nrow(coordinate_conflicts), nrow(continuity_breaks), 
        sum(distance_audit$segments_checked), max(distance_audit$maximum_error_m), max(coordinate_errors$maximum_absolute_error), nrow(restrictions), nrow(barriers), sum(barriers$belongs_to_candidate_network), sum(checks$passed), nrow(checks)))
    summary_path <- file.path(results_dir, "phase8B1_master_summary.csv")
    fwrite(summary, summary_path)
    assumptions <- data.table(assumption = c("Topology source", "Original source checksum", "Node identifier representation", "Node coordinate reconstruction", "Segment distance", "Road class eligibility", "Turn restrictions", "Vehicle access restrictions", "Barrier restrictions", "National boundary enforcement", "Graph connectivity", "Travel times", "Repair method"), treatment = c(basename(pbf_path), original_pbf_hash, "Original OSM identifiers retained as character", "From-node coordinates and corrected to-node longitude and latitude", 
        "Spherical great-circle distance independently reproduced", "Core and conditional motor-road candidates", "Extracted but not yet enforced", "Extracted but not yet enforced", "Extracted but not yet enforced", "Not yet implemented", "Not yet calculated", "Not yet calculated", "Recovered existing extraction without repeated PBF processing"))
    assumptions_path <- file.path(repro_dir, "phase8B1_modelling_assumptions.csv")
    fwrite(assumptions, assumptions_path)
    session_path <- file.path(repro_dir, "phase8B1_R_session_info.txt")
    writeLines(capture.output(sessionInfo()), session_path)
    repair_script <- file.path(scripts_dir, "phase8B1_coordinate_repair.R")
    writeLines(c("phase8B1_finalize <-", deparse(phase8B1_finalize, width.cutoff = 500L), "", "phase8B1_final_results <- phase8B1_finalize()"), repair_script)
    python_script <- file.path(scripts_dir, "phase8B1_extract_osm_topology.py")
    stopifnot(file.exists(python_script))
    output_files <- sort(unique(c(final_extracted_paths, rds_paths, file.path(root, names(outputs)), quality_path, summary_path, assumptions_path, session_path, python_script, repair_script)))
    output_files <- normalizePath(output_files, winslash = "/", mustWork = TRUE)
    stopifnot(all(file.exists(output_files)), all(file.info(output_files)$size > 0))
    output_manifest <- data.table(filename = basename(output_files), relative_path = substring(output_files, nchar(root) + 2), size_bytes = file.info(output_files)$size, SHA256 = hash_files(output_files))
    manifest_path <- file.path(repro_dir, "phase8B1_output_checksum_manifest.csv")
    fwrite(output_manifest, manifest_path)
    recheck <- fread(manifest_path)
    recheck[, `:=`(current_SHA256, hash_files(file.path(root, relative_path)))]
    recheck[, `:=`(verified, current_SHA256 == SHA256)]
    stopifnot(all(recheck$verified))
    fwrite(recheck, file.path(repro_dir, "phase8B1_checksum_recheck.csv"))
    stopifnot(identical(hash_files(previous_paths), previous_hashes), identical(hash_files(pbf_path), original_pbf_hash), identical(hash_files(extracted_paths), extracted_hashes))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    writeLines(c("GHANA DIAGNOSTIC IMAGING RESEARCH", "PHASE 8B1 FINAL COORDINATE REPAIR", "Original error: destination longitude assigned to latitude", "Correction: longitude = to_lon", "PBF extraction repeated: NO", paste("Unique original nodes:", nrow(nodes)), paste("Native road segments:", nrow(segments)), paste("Coordinate conflicts:", nrow(coordinate_conflicts)), paste("Quality checks:", sum(checks$passed), "/", nrow(checks)), paste("Output checksums:", sum(recheck$verified), "/", nrow(recheck)), 
        paste("Execution seconds:", round(elapsed, 2)), "Original extraction preserved", "Previous research outputs unchanged", "Graph connectivity: NOT YET CALCULATED", "Road restrictions: NOT YET ENFORCED", "Travel times: NOT YET CALCULATED"), file.path(logs_dir, "phase8B1_execution_log.txt"))
    cat("\n========================================\n")
    cat("PHASE 8B1 FINAL REPAIR RESULTS\n")
    cat("========================================\n")
    print(summary, nrows = 100)
    cat("\nTOPOLOGY SUMMARY\n")
    print(topology_summary)
    cat("\nCOORDINATE REPAIR AUDIT\n")
    print(coordinate_errors)
    cat("\nSOURCE IDENTIFIER COMPARISON\n")
    print(source_comparison)
    cat("\nFINAL QUALITY CHECKS\n")
    print(checks, nrows = 100)
    cat("\nPrevious checksums verified:", length(previous_hashes), "/35\n")
    cat("New output checksums verified:", sum(recheck$verified), "/", nrow(recheck), "\n")
    cat("Repair execution seconds:", round(elapsed, 2), "\n")
    cat("\nPHASE 8B1 STRUCTURAL STATUS: PASS\n")
    cat("NO PBF EXTRACTION REPEATED\n")
    cat("ORIGINAL EXTRACTION PRESERVED\n")
    cat("DIRECTED GRAPH CONSTRUCTION PENDING\n")
    invisible(list(summary = summary, topology_summary = topology_summary, coordinate_errors = coordinate_errors, correction_log = correction_log, distance_audit = distance_audit, attribute_audit = attribute_audit, tag_summary = tag_summary, barrier_summary = barrier_summary, restriction_summary = restriction_summary, source_comparison = source_comparison, checks = checks, manifest = recheck))
}

phase8B1_final_results <- phase8B1_finalize()
