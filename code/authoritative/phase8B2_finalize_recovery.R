phase8B2_finalize <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(sf)
        library(igraph)
        library(digest)
        library(jsonlite)
    })
    start_time <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    audit_dir <- file.path(root, "03_Data_Audit")
    spatial_dir <- file.path(root, "04_Spatial_Analysis")
    results_dir <- file.path(root, "08_Results")
    figures_dir <- file.path(root, "09_Figures")
    tables_dir <- file.path(root, "10_Tables")
    repro_dir <- file.path(root, "12_Reproducibility")
    scripts_dir <- file.path(root, "13_R_Scripts")
    logs_dir <- file.path(root, "16_Research_Logs")
    topology_dir <- file.path(spatial_dir, "National_Road_Network", "PBF_Topology")
    graph_dir <- file.path(spatial_dir, "National_Road_Network", "Directed_Graphs")
    hash_files <- function(paths) {
        stopifnot(all(file.exists(paths)))
        vapply(paths, function(p) {
            digest(file = p, algo = "sha256")
        }, character(1), USE.NAMES = FALSE)
    }
    cat("\n========================================\n")
    cat("PHASE 8B2 AUTOMATIC FINALIZATION\n")
    cat("========================================\n")
    previous_manifest <- fread(file.path(repro_dir, "phase8B1_output_checksum_manifest.csv"))
    previous_paths <- file.path(root, previous_manifest$relative_path)
    previous_hashes <- hash_files(previous_paths)
    stopifnot(nrow(previous_manifest) == 31, identical(previous_hashes, as.character(previous_manifest$SHA256)))
    previous_quality <- fread(file.path(audit_dir, "phase8B1_quality_gate.csv"))
    stopifnot(nrow(previous_quality) == 37, all(previous_quality$passed))
    cat("Previous checksums: 31/31 PASS\n")
    cat("Previous quality checks: 37/37 PASS\n")
    scenarios <- c("CORE_PRIMARY", "EXPANDED", "CORE_STRICT_BARRIERS")
    graph_suffixes <- c("_graph.rds", "_vertices.rds", "_arcs.rds", "_native_segments.rds", "_weak_components.csv")
    graph_files <- unlist(lapply(scenarios, function(nm) {
        file.path(graph_dir, paste0(nm, graph_suffixes))
    }), use.names = FALSE)
    stopifnot(length(graph_files) == 15, all(file.exists(graph_files)), all(file.info(graph_files)$size > 0))
    graph_hashes_before <- hash_files(graph_files)
    cat("Existing graph files: 15/15 RECOVERED\n")
    inventory_names <- c("ghana_network_node_inventory.rds", "ghana_way_access_direction_audit.rds", "ghana_native_segment_eligibility.rds", "ghana_barrier_access_audit.rds")
    inventory_paths <- file.path(graph_dir, inventory_names)
    stopifnot(all(file.exists(inventory_paths)))
    inventory_hashes_before <- hash_files(inventory_paths)
    nodes <- as.data.table(readRDS(inventory_paths[1]))
    ways <- as.data.table(readRDS(inventory_paths[2]))
    segments <- as.data.table(readRDS(inventory_paths[3]))
    barriers <- as.data.table(readRDS(inventory_paths[4]))
    stopifnot(nrow(nodes) == 4150434, nrow(ways) == 364113, nrow(segments) == 4308002, nrow(barriers) == 1427, !anyDuplicated(nodes$node_id), !anyDuplicated(ways$way_id), !anyDuplicated(segments$segment_id))
    cat("Existing network inventories: PASS\n")
    access_summary <- ways[, .N, by = .(network_class, effective_access, access_status, access_allowed)]
    direction_summary <- ways[, .N, by = .(network_class, direction, direction_source, effective_oneway)]
    suitability_summary <- ways[, .(road_ways = .N, access_excluded = sum(!access_allowed), unresolved_direction = sum(direction == "UNRESOLVED"), severe_surface = sum(severe_surface_flag), ford_flagged = sum(ford_flag), restricted_service = sum(restricted_service_flag), provisionally_eligible = sum(provisional_vehicle_eligible)), by = network_class]
    barrier_summary <- barriers[, .N, by = .(barrier_type, barrier_status, on_network)]
    boundary_summary <- segments[, .(native_segments = .N, length_km = sum(length_m)/1000), by = .(both_endpoints_inside, fully_inside_country)]
    eligibility_summary <- segments[, .(native_segments = .N, length_km = sum(length_m)/1000), by = .(network_class, exclusion_reason)]
    scenario_design <- data.table(scenario = scenarios, include_conditional_roads = c(FALSE, TRUE, FALSE), exclude_uncertain_barriers = c(FALSE, FALSE, TRUE))
    stopifnot(sum(boundary_summary$native_segments) == 4308002, sum(eligibility_summary$native_segments) == 4308002, sum(segments$fully_inside_country) == 4204918, sum(nodes$inside_country) == 4048368, sum(segments$base_eligible) == 4187798)
    cat("Recovered network inventory reconciliation: PASS\n")
    restriction_csv <- file.path(topology_dir, "osm_turn_restrictions.csv")
    stopifnot(file.exists(restriction_csv))
    restrictions <- as.data.table(read.csv(restriction_csv, colClasses = "character", check.names = FALSE, stringsAsFactors = FALSE, quote = "\"", comment.char = "", fileEncoding = "UTF-8"))
    stopifnot(nrow(restrictions) == 154, !anyDuplicated(restrictions$relation_id), all(c("relation_id", "restriction", "members_json", "tags_json") %in% names(restrictions)))
    cat("\nTURN RESTRICTION CSV RECOVERY\n")
    cat("Relations recovered:", nrow(restrictions), "\n")
    cat("\nFirst original member JSON record:\n")
    print(restrictions$members_json[1])
    decode_json <- function(x) {
        if (length(x) != 1L || is.na(x) || !nzchar(x)) {
            return(NULL)
        }
        candidates <- unique(c(x, gsub("\"\"", "\"", x, fixed = TRUE)))
        for (candidate in candidates) {
            parsed <- tryCatch(jsonlite::fromJSON(candidate, simplifyVector = FALSE, bigint_as_char = TRUE), error = function(e) NULL)
            if (!is.null(parsed)) {
                return(parsed)
            }
        }
        NULL
    }
    id_string <- function(x) {
        if (is.null(x)) {
            return("")
        }
        if (is.numeric(x)) {
            return(format(x, scientific = FALSE, trim = TRUE))
        }
        as.character(x)
    }
    member_list <- lapply(restrictions$members_json, decode_json)
    tag_list <- lapply(restrictions$tags_json, decode_json)
    json_audit <- data.table(relation_id = restrictions$relation_id, members_parsed = vapply(member_list, function(x) {
        is.list(x) && length(x) > 0
    }, logical(1)), tags_parsed = vapply(tag_list, function(x) {
        is.list(x) && length(x) > 0
    }, logical(1)))
    cat("\nSuccessfully parsed member records:", sum(json_audit$members_parsed), "/154\n")
    cat("Successfully parsed tag records:", sum(json_audit$tags_parsed), "/154\n")
    fwrite(json_audit, file.path(audit_dir, "phase8B2_turn_json_parse_audit.csv"))
    stopifnot(all(json_audit$members_parsed), all(json_audit$tags_parsed))
    restriction_audit <- rbindlist(lapply(seq_len(nrow(restrictions)), function(i) {
        m <- member_list[[i]]
        t <- tag_list[[i]]
        members <- rbindlist(lapply(seq_along(m), function(j) {
            z <- m[[j]]
            data.table(member_order = j, type = tolower(id_string(z$type)), ref = id_string(z$ref), role = id_string(z$role))
        }), fill = TRUE)
        from_ids <- members[role == "from" & type %in% c("w", "way"), ref]
        to_ids <- members[role == "to" & type %in% c("w", "way"), ref]
        via_nodes <- members[role == "via" & type %in% c("n", "node"), ref]
        via_ways <- members[role == "via" & type %in% c("w", "way"), ref]
        present <- length(from_ids) > 0 && length(to_ids) > 0 && (length(via_nodes) + length(via_ways)) > 0 && all(c(from_ids, to_ids, via_ways) %in% ways$way_id) && all(via_nodes %in% nodes$node_id)
        relation_status <- if (length(from_ids) == 1 && length(to_ids) == 1 && length(via_nodes) == 1 && length(via_ways) == 0) {
            "SIMPLE_VIA_NODE"
        }
        else if (length(via_ways) > 0) {
            "COMPLEX_VIA_WAY"
        }
        else {
            "OTHER_REQUIRES_REVIEW"
        }
        get_relation_tag <- function(nm) {
            x <- t[[nm]]
            if (is.null(x)) {
                return("")
            }
            as.character(x)
        }
        data.table(relation_id = restrictions$relation_id[i], restriction = restrictions$restriction[i], from_ways = paste(from_ids, collapse = ";"), to_ways = paste(to_ids, collapse = ";"), via_nodes = paste(via_nodes, collapse = ";"), via_ways = paste(via_ways, collapse = ";"), from_count = length(from_ids), to_count = length(to_ids), via_node_count = length(via_nodes), via_way_count = length(via_ways), candidate_members_present = present, relation_status = relation_status, except = get_relation_tag("except"), 
            motorcar_restriction = get_relation_tag("restriction:motorcar"), motor_vehicle_restriction = get_relation_tag("restriction:motor_vehicle"), hgv_restriction = get_relation_tag("restriction:hgv"), conditional_restriction = get_relation_tag("restriction:conditional"), enforcement_status = "NOT_YET_ENFORCED")
    }), fill = TRUE)
    restriction_members <- rbindlist(lapply(seq_along(member_list), function(i) {
        m <- member_list[[i]]
        rbindlist(lapply(seq_along(m), function(j) {
            z <- m[[j]]
            data.table(relation_id = restrictions$relation_id[i], member_order = j, member_type = id_string(z$type), member_ref = id_string(z$ref), member_role = id_string(z$role))
        }))
    }))
    restriction_summary <- restriction_audit[, .N, by = .(relation_status, candidate_members_present)]
    stopifnot(nrow(restriction_audit) == 154, !anyDuplicated(restriction_audit$relation_id), all(restriction_members$relation_id %in% restriction_audit$relation_id))
    cat("\nCORRECTED TURN RESTRICTION SUMMARY\n")
    print(restriction_summary, nrows = 100)
    fwrite(restriction_members, file.path(audit_dir, "phase8B2_turn_relation_members.csv"))
    restriction_rds <- file.path(graph_dir, "ghana_turn_restrictions_corrected.rds")
    saveRDS(list(restrictions = restrictions, members = restriction_members, audit = restriction_audit), restriction_rds)
    safe_sample <- function(x, maximum) {
        if (length(x) == 0L) {
            return(integer(0))
        }
        if (length(x) <= maximum) {
            return(x)
        }
        x[sample.int(length(x), maximum, replace = FALSE)]
    }
    verify_graph <- function(nm) {
        cat("\n========================================\n")
        cat("VERIFYING SAVED GRAPH:", nm, "\n")
        cat("========================================\n")
        g <- readRDS(file.path(graph_dir, paste0(nm, "_graph.rds")))
        v <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_vertices.rds"))))
        a <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_arcs.rds"))))
        n <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_native_segments.rds"))))
        ci <- fread(file.path(graph_dir, paste0(nm, "_weak_components.csv")))
        stopifnot(is_directed(g), vcount(g) == nrow(v), ecount(g) == nrow(a), !anyDuplicated(v$vertex_id), !anyDuplicated(n$segment_id), !anyDuplicated(a$arc_id), identical(as.integer(v$vertex_id), seq_len(nrow(v))), all(a$from > 0), all(a$to > 0), all(a$from <= nrow(v)), all(a$to <= nrow(v)), all(a$from != a$to), all(a$length_m > 0), all(a$segment_id %in% n$segment_id))
        edge_matrix <- as_edgelist(g, names = FALSE)
        edge_order_valid <- all(edge_matrix[, 1] == a$from) && all(edge_matrix[, 2] == a$to)
        stopifnot(edge_order_valid)
        rm(edge_matrix)
        gc()
        segment_positions <- match(n$segment_id, segments$segment_id)
        stopifnot(!anyNA(segment_positions), all(segments$base_eligible[segment_positions]), all(segments$fully_inside_country[segment_positions]), !any(segments$touches_blocked_barrier[segment_positions]), all(n$from_index == segments$from_index[segment_positions]), all(n$to_index == segments$to_index[segment_positions]), all(n$way_index == segments$way_index[segment_positions]), all(n$length_m == segments$length_m[segment_positions]))
        if (nm != "EXPANDED") {
            stopifnot(all(segments$network_class[segment_positions] == "CORE"))
        }
        if (nm == "CORE_STRICT_BARRIERS") {
            stopifnot(!any(segments$touches_uncertain_barrier[segment_positions]))
        }
        arc_segment_positions <- match(a$segment_id, n$segment_id)
        stopifnot(!anyNA(arc_segment_positions))
        is_forward <- a$arc_direction == "FORWARD"
        is_reverse <- a$arc_direction == "REVERSE"
        stopifnot(all(is_forward | is_reverse), all(a$way_index == n$way_index[arc_segment_positions]), all(a$length_m == n$length_m[arc_segment_positions]), all(v$node_index[a$from] == a$from_global), all(v$node_index[a$to] == a$to_global), all(a$from_global[is_forward] == n$from_index[arc_segment_positions[is_forward]]), all(a$to_global[is_forward] == n$to_index[arc_segment_positions[is_forward]]), all(a$from_global[is_reverse] == n$to_index[arc_segment_positions[is_reverse]]), all(a$to_global[is_reverse] == 
            n$from_index[arc_segment_positions[is_reverse]]), all(n$direction[arc_segment_positions[is_forward]] %in% c("FORWARD", "BOTH")), all(n$direction[arc_segment_positions[is_reverse]] %in% c("REVERSE", "BOTH")))
        expected_arcs <- sum(ifelse(n$direction == "BOTH", 2L, 1L))
        stopifnot(expected_arcs == nrow(a), uniqueN(a, by = c("segment_id", "arc_direction")) == nrow(a))
        cat("Original segment and arc mapping: PASS\n")
        rm(arc_segment_positions, is_forward, is_reverse)
        gc()
        vp <- match(v$node_id, nodes$node_id)
        stopifnot(!anyNA(vp), all(v$node_index == nodes$node_index[vp]), all(v$longitude == nodes$longitude[vp]), all(v$latitude == nodes$latitude[vp]), all(v$inside_country))
        cat("Original vertex coordinates: PASS\n")
        weak <- components(g, mode = "weak")
        strong <- components(g, mode = "strong")
        weak_partition <- unique(data.table(stored = v$weak_component, recomputed = weak$membership))
        strong_partition <- unique(data.table(stored = v$strong_component, recomputed = strong$membership))
        weak_partition_valid <- uniqueN(weak_partition$stored) == nrow(weak_partition) && uniqueN(weak_partition$recomputed) == nrow(weak_partition)
        strong_partition_valid <- uniqueN(strong_partition$stored) == nrow(strong_partition) && uniqueN(strong_partition$recomputed) == nrow(strong_partition)
        stopifnot(weak_partition_valid, strong_partition_valid, sum(weak$csize) == nrow(v), sum(strong$csize) == nrow(v), all(v$weak_component_size == weak$csize[weak$membership]), all(v$strong_component_size == strong$csize[strong$membership]), sum(v$largest_weak_component) == max(weak$csize), sum(v$largest_strong_component) == max(strong$csize))
        cat("Weak component partition: PASS\n")
        cat("Strong component partition: PASS\n")
        indeg <- degree(g, mode = "in")
        outdeg <- degree(g, mode = "out")
        stopifnot(all(indeg == v$in_degree), all(outdeg == v$out_degree), sum(indeg) == nrow(a), sum(outdeg) == nrow(a))
        cat("Vertex degrees: PASS\n")
        stopifnot(sum(ci$vertices) == nrow(v), sum(ci$native_segments) == nrow(n), abs(sum(ci$road_length_km) - sum(n$length_m)/1000) < 1e-05)
        graph_summary <- data.table(scenario = nm, retained_native_segments = nrow(n), active_vertices = nrow(v), directed_arcs = nrow(a), road_length_km = sum(n$length_m)/1000, forward_only_segments = sum(n$direction == "FORWARD"), reverse_only_segments = sum(n$direction == "REVERSE"), bidirectional_segments = sum(n$direction == "BOTH"), weak_components = weak$no, strong_components = strong$no, largest_weak_vertices = max(weak$csize), largest_strong_vertices = max(strong$csize), largest_weak_share_pct = 100 * 
            max(weak$csize)/nrow(v), largest_strong_share_pct = 100 * max(strong$csize)/nrow(v), singleton_weak_components = sum(weak$csize == 1), singleton_strong_components = sum(strong$csize == 1), zero_indegree_vertices = sum(indeg == 0), zero_outdegree_vertices = sum(outdeg == 0), largest_weak_road_length_km = sum(n[weak_component %in% v[largest_weak_component == TRUE, unique(weak_component)], length_m])/1000, uncertain_barrier_segments = sum(segments$touches_uncertain_barrier[segment_positions]))
        print(graph_summary)
        set.seed(20261125)
        selected <- c(safe_sample(which(v$largest_weak_component), 100000L), safe_sample(which(!v$largest_weak_component), 50000L))
        map_sample <- v[selected, .(longitude, latitude, easting, northing, largest_weak_component)]
        map_sample[, `:=`(scenario, nm)]
        setorder(ci, -vertices)
        verification <- data.table(scenario = nm, edge_order_verified = TRUE, native_segment_mapping_verified = TRUE, vertex_coordinates_verified = TRUE, weak_partition_verified = weak_partition_valid, strong_partition_verified = strong_partition_valid, vertex_degree_verified = TRUE, component_lengths_reconciled = TRUE)
        result <- list(summary = graph_summary, map_sample = map_sample, top_components = head(ci, 20), retained_segment_ids = n$segment_id, verification = verification)
        rm(g, v, a, n, ci, weak, strong, indeg, outdeg, weak_partition, strong_partition)
        gc()
        cat(nm, "INDEPENDENT VERIFICATION: PASS\n")
        result
    }
    verified_graphs <- setNames(lapply(scenarios, verify_graph), scenarios)
    graph_summary <- rbindlist(lapply(verified_graphs, function(z) z$summary))
    graph_verification <- rbindlist(lapply(verified_graphs, function(z) z$verification))
    map_samples <- rbindlist(lapply(verified_graphs, function(z) z$map_sample))
    top_components <- rbindlist(lapply(verified_graphs, function(z) z$top_components))
    expected <- data.table(scenario = scenarios, expected_native_segments = c(3556948, 4187798, 3556770), expected_vertices = c(3421445, 4033081, 3421335), expected_arcs = c(7065687, 8321285, 7065335), expected_weak_components = c(621, 879, 643), expected_strong_components = c(801, 1103, 851), expected_largest_weak = c(3392359, 3987470, 3390112), expected_largest_strong = c(3392197, 3987089, 3389922))
    reconciliation <- merge(graph_summary, expected, by = "scenario")
    reconciliation[, `:=`(counts_match, retained_native_segments == expected_native_segments & active_vertices == expected_vertices & directed_arcs == expected_arcs & weak_components == expected_weak_components & strong_components == expected_strong_components & largest_weak_vertices == expected_largest_weak & largest_strong_vertices == expected_largest_strong)]
    stopifnot(nrow(reconciliation) == 3, all(reconciliation$counts_match))
    cat("\nORIGINAL GRAPH RESULT RECONCILIATION: PASS\n")
    core_ids <- verified_graphs[["CORE_PRIMARY"]]$retained_segment_ids
    expanded_ids <- verified_graphs[["EXPANDED"]]$retained_segment_ids
    strict_ids <- verified_graphs[["CORE_STRICT_BARRIERS"]]$retained_segment_ids
    stopifnot(all(core_ids %in% expanded_ids), all(strict_ids %in% core_ids))
    network_nesting <- data.table(comparison = c("Expanded versus core", "Strict barriers versus core"), added_segments = c(length(setdiff(expanded_ids, core_ids)), length(setdiff(strict_ids, core_ids))), removed_segments = c(length(setdiff(core_ids, expanded_ids)), length(setdiff(core_ids, strict_ids))))
    stopifnot(network_nesting$added_segments[1] == 630850, network_nesting$removed_segments[1] == 0, network_nesting$added_segments[2] == 0, network_nesting$removed_segments[2] == 178)
    cat("\nNETWORK NESTING: PASS\n")
    print(network_nesting)
    relation_presence <- rbindlist(lapply(scenarios, function(nm) {
        native_path <- file.path(graph_dir, paste0(nm, "_native_segments.rds"))
        n <- readRDS(native_path)
        active_way_ids <- unique(ways$way_id[n$way_index])
        active_node_indices <- unique(c(n$from_index, n$to_index))
        active_node_ids <- nodes$node_id[active_node_indices]
        mm <- copy(restriction_members)
        mm[, `:=`(member_present, FALSE)]
        mm[member_type %in% c("w", "way"), `:=`(member_present, member_ref %in% active_way_ids)]
        mm[member_type %in% c("n", "node"), `:=`(member_present, member_ref %in% active_node_ids)]
        out <- mm[, .(member_count = .N, present_member_count = sum(member_present), all_members_present = all(member_present)), by = relation_id]
        out[, `:=`(scenario, nm)]
        rm(n, mm, active_way_ids, active_node_indices, active_node_ids)
        gc()
        out
    }))
    stopifnot(nrow(relation_presence) == 154 * 3)
    relation_presence_summary <- relation_presence[, .(relations = .N, all_members_present = sum(all_members_present), some_or_all_members_absent = sum(!all_members_present)), by = scenario]
    cat("\nTURN RELATIONS BY NETWORK SCENARIO\n")
    print(relation_presence_summary)
    regions <- st_read(file.path(spatial_dir, "Administrative_Boundaries", "gha_admin1.geojson"), quiet = TRUE)
    regions_m <- st_make_valid(st_transform(regions, 32630))
    stopifnot(nrow(regions_m) == 16)
    save_figure <- function(stem, draw, width = 12, height = 8) {
        png_path <- file.path(figures_dir, paste0(stem, ".png"))
        pdf_path <- file.path(figures_dir, paste0(stem, ".pdf"))
        png(png_path, width = width, height = height, units = "in", res = 400, bg = "white")
        tryCatch(draw(), finally = dev.off())
        pdf(pdf_path, width = width, height = height, bg = "white")
        tryCatch(draw(), finally = dev.off())
        stopifnot(all(file.info(c(png_path, pdf_path))$size > 0))
        c(png_path, pdf_path)
    }
    labels <- c(CORE_PRIMARY = "Core", EXPANDED = "Expanded", CORE_STRICT_BARRIERS = "Strict barriers")
    draw_figure25 <- function() {
        z <- graph_summary
        par(mfrow = c(1, 2), mar = c(6, 5, 4, 2), oma = c(3, 0, 4, 0))
        values <- rbind(z$largest_weak_share_pct, z$largest_strong_share_pct)
        bp <- barplot(values, beside = TRUE, names.arg = labels[z$scenario], col = c("#2166AC", "#1B9E77"), border = NA, ylim = c(0, 125), ylab = "Share of active vertices (%)", main = "A. Largest component coverage", cex.names = 0.85)
        text(bp, values, labels = sprintf("%.3f", values), pos = 3, cex = 0.6)
        legend("topright", legend = c("Weak connectivity", "Strong connectivity"), fill = c("#2166AC", "#1B9E77"), border = NA, bty = "n", cex = 0.75)
        bp2 <- barplot(z$road_length_km, names.arg = labels[z$scenario], col = "#2166AC", border = NA, ylim = c(0, max(z$road_length_km) * 1.2), ylab = "Retained road length (km)", main = "B. Retained road inventory", cex.names = 0.85)
        text(bp2, z$road_length_km, labels = format(round(z$road_length_km), big.mark = ",", scientific = FALSE), pos = 3, cex = 0.8)
        mtext("Directed Road Network Connectivity Across Ghana", side = 3, outer = TRUE, line = 2, font = 2, cex = 1.25)
        mtext("Provisional motor-vehicle graphs; turn restrictions not yet enforced.", side = 1, outer = TRUE, line = 1, cex = 0.8)
    }
    figure25 <- save_figure("Figure_25_Directed_Network_Connectivity", draw_figure25)
    cat("Figure 25 regenerated: PASS\n")
    draw_figure26 <- function() {
        par(mfrow = c(1, 3), mar = c(2, 1, 4, 1), oma = c(3, 0, 4, 0))
        for (nm in scenarios) {
            z <- map_samples[scenario == nm]
            major <- z[largest_weak_component == TRUE]
            minor <- z[largest_weak_component == FALSE]
            plot(st_geometry(regions_m), col = "grey97", border = "grey65", axes = FALSE, lwd = 0.7)
            if (nrow(major) > 0) {
                points(major$easting, major$northing, pch = 16, cex = 0.13, col = "#2166AC")
            }
            if (nrow(minor) > 0) {
                points(minor$easting, minor$northing, pch = 16, cex = 0.25, col = "#D95F02")
            }
            title(main = labels[[nm]])
            legend("bottomleft", legend = c("Largest weak component", "Other components"), col = c("#2166AC", "#D95F02"), pch = 16, bty = "n", cex = 0.65)
        }
        mtext("Spatial Distribution of Road Network Components", side = 3, outer = TRUE, line = 2, font = 2, cex = 1.25)
        mtext("Stratified vertex samples; connectivity calculated on complete graphs.", side = 1, outer = TRUE, line = 1, cex = 0.8)
    }
    figure26 <- save_figure("Figure_26_Road_Component_Geography", draw_figure26, width = 15, height = 8)
    cat("Figure 26 regenerated: PASS\n")
    draw_figure27 <- function() {
        z <- eligibility_summary[, .(road_length_km = sum(length_km)), by = exclusion_reason]
        setorder(z, road_length_km)
        par(mar = c(6, 15, 5, 3), las = 1)
        bp <- barplot(z$road_length_km, horiz = TRUE, names.arg = gsub("_", " ", z$exclusion_reason), col = ifelse(z$exclusion_reason == "ELIGIBLE", "#1B9E77", "#D95F02"), border = NA, xlim = c(0, max(z$road_length_km) * 1.25), xlab = "Native road-feature length (km)")
        text(z$road_length_km, bp, labels = format(round(z$road_length_km), big.mark = ",", scientific = FALSE), pos = 4, cex = 0.8)
        title(main = "National Road Network Eligibility Screening", line = 3)
        mtext("Mutually exclusive primary exclusion reasons", side = 3, line = 1)
        mtext("Eligibility is modelled, not independently verified vehicle accessibility.", side = 1, line = 4.2, cex = 0.8)
    }
    figure27 <- save_figure("Figure_27_Road_Eligibility_Screen", draw_figure27)
    cat("Figure 27 regenerated: PASS\n")
    figure_files <- c(figure25, figure26, figure27)
    draw_figure28 <- function() {
        z <- relation_presence_summary
        z <- z[match(scenarios, scenario)]
        values <- rbind(z$all_members_present, z$some_or_all_members_absent)
        par(mar = c(6, 5, 5, 2), oma = c(2, 0, 0, 0))
        bp <- barplot(values, names.arg = labels[z$scenario], col = c("#2166AC", "#D95F02"), border = NA, ylim = c(0, 195), ylab = "Original turn restriction relations", main = "Turn Restriction Member Availability")
        for (i in seq_len(ncol(values))) {
            if (values[1, i] > 0) {
                text(bp[i], values[1, i]/2, labels = values[1, i], col = "white", font = 2)
            }
            if (values[2, i] > 0) {
                text(bp[i], values[1, i] + values[2, i]/2, labels = values[2, i], col = "white", font = 2)
            }
        }
        legend("topright", legend = c("All members retained", "At least one member absent"), fill = c("#2166AC", "#D95F02"), border = NA, bty = "n")
        mtext("Member availability does not establish valid or enforced turning movements.", side = 1, line = 4, cex = 0.8)
    }
    figure28 <- save_figure("Figure_28_Turn_Restriction_Availability", draw_figure28)
    figure_files <- c(figure_files, figure28)
    cat("Figure 28 generated: PASS\n")
    correction_log <- data.table(issue = c("Figure logical column filtering", "Turn restriction JSON parsing", "Incomplete final execution"), original_status = c("Figure 26 failed", "154 relations classified unresolved", "Quality manifest not finalized"), correction = c("Explicit logical column comparisons", "Standards-compliant CSV reading and member parsing", "Recovered and independently verified saved graph files"), graph_reconstructed = c(FALSE, FALSE, FALSE), pbf_extraction_repeated = c(FALSE, 
        FALSE, FALSE))
    outputs <- list(`10_Tables/phase8B2_access_summary.csv` = access_summary, `10_Tables/phase8B2_direction_summary.csv` = direction_summary, `10_Tables/phase8B2_vehicle_suitability.csv` = suitability_summary, `10_Tables/phase8B2_barrier_summary.csv` = barrier_summary, `10_Tables/phase8B2_boundary_summary.csv` = boundary_summary, `10_Tables/phase8B2_eligibility_summary.csv` = eligibility_summary, `10_Tables/phase8B2_graph_summary.csv` = graph_summary, `10_Tables/phase8B2_largest_components.csv` = top_components, 
        `10_Tables/phase8B2_network_nesting.csv` = network_nesting, `03_Data_Audit/phase8B2_turn_relation_audit.csv` = restriction_audit, `10_Tables/phase8B2_turn_relation_summary.csv` = restriction_summary, `02_Processed_Data/phase8B2_scenario_design.csv` = scenario_design, `03_Data_Audit/phase8B2_graph_reverification.csv` = graph_verification, `03_Data_Audit/phase8B2_original_result_reconciliation.csv` = reconciliation, `03_Data_Audit/phase8B2_recovery_correction_log.csv` = correction_log, `03_Data_Audit/phase8B2_turn_relation_scenario_presence.csv` = relation_presence, 
        `10_Tables/phase8B2_turn_relation_scenario_summary.csv` = relation_presence_summary)
    for (nm in names(outputs)) {
        fwrite(outputs[[nm]], file.path(root, nm))
    }
    previous_unchanged <- identical(hash_files(previous_paths), previous_hashes)
    graphs_unchanged <- identical(hash_files(graph_files), graph_hashes_before)
    inventories_unchanged <- identical(hash_files(inventory_paths), inventory_hashes_before)
    checks <- data.table(check = c("Previous 31 checksums verified", "Previous 37 quality checks passed", "Previous phase outputs unchanged", "All 15 saved graph files recovered", "Original graph files unchanged", "All four saved inventories recovered", "Original network inventories unchanged", "All original nodes retained", "All original ways retained", "All original segments retained", "Barrier inventory reconciled", "National boundary counts reconciled", "Road eligibility counts reconciled", "All three graph scenarios recovered", 
        "Graph edge ordering independently verified", "Original native segment mappings verified", "Original vertex coordinates verified", "Weak components independently reproduced", "Strong components independently reproduced", "Vertex degrees independently reproduced", "Component road lengths reconciled", "Original execution results reproduced", "Core nested inside expanded", "Strict barrier network nested inside core", "Network nesting counts reproduced", "Hard barriers excluded", "Strict barrier scenario excludes uncertain barriers", 
        "All turn restriction records recovered", "All turn member JSON records parsed", "All turn tag JSON records parsed", "Turn relation identifiers unique", "Turn restriction scenario inventory complete", "Turn relation member identifiers retained", "All four figures generated", "All eight figure files available", "All graph directions reconcile with arc counts"), passed = c(all(previous_hashes == previous_manifest$SHA256), all(previous_quality$passed), previous_unchanged, length(graph_files) == 
        15 && all(file.exists(graph_files)), graphs_unchanged, length(inventory_paths) == 4 && all(file.exists(inventory_paths)), inventories_unchanged, nrow(nodes) == 4150434, nrow(ways) == 364113, nrow(segments) == 4308002, nrow(barriers) == 1427, sum(boundary_summary$native_segments) == nrow(segments), sum(eligibility_summary$native_segments) == nrow(segments), nrow(graph_summary) == 3, all(graph_verification$edge_order_verified), all(graph_verification$native_segment_mapping_verified), all(graph_verification$vertex_coordinates_verified), 
        all(graph_verification$weak_partition_verified), all(graph_verification$strong_partition_verified), all(graph_verification$vertex_degree_verified), all(graph_verification$component_lengths_reconciled), all(reconciliation$counts_match), all(core_ids %in% expanded_ids), all(strict_ids %in% core_ids), network_nesting$added_segments[1] == 630850 && network_nesting$removed_segments[2] == 178, !any(segments$touches_blocked_barrier[match(expanded_ids, segments$segment_id)]), !any(segments$touches_uncertain_barrier[match(strict_ids, 
            segments$segment_id)]), nrow(restriction_audit) == 154, all(json_audit$members_parsed), all(json_audit$tags_parsed), !anyDuplicated(restriction_audit$relation_id), nrow(relation_presence) == 462, all(nzchar(restriction_members$member_ref)), length(figure_files) == 8, all(file.exists(figure_files)) && all(file.info(figure_files)$size > 0), all(graph_summary$directed_arcs == graph_summary$forward_only_segments + graph_summary$reverse_only_segments + 2 * graph_summary$bidirectional_segments)))
    checks[is.na(passed), `:=`(passed, FALSE)]
    checks[, `:=`(status, fifelse(passed, "PASS", "FAIL"))]
    quality_path <- file.path(audit_dir, "phase8B2_quality_gate.csv")
    fwrite(checks, quality_path)
    cat("\nFINAL STRUCTURAL QUALITY GATE\n")
    print(checks, nrows = 100)
    stopifnot(nrow(checks) == 36, all(checks$passed))
    primary <- graph_summary[scenario == "CORE_PRIMARY"]
    summary <- data.table(metric = c("Original network nodes", "Original OSM ways", "Original native segments", "Nodes inside national polygon", "Segments fully inside national polygon", "Segments excluded by boundary", "Blocked network barrier nodes", "Uncertain network barrier nodes", "Eligible expanded native segments", "Primary native segments", "Primary active vertices", "Primary directed arcs", "Primary road length km", "Primary weak components", "Primary strong components", "Primary largest weak share pct", 
        "Primary largest strong share pct", "Original turn restriction relations", "Turn member JSON records parsed", "Simple via-node restrictions", "Complex via-way restrictions", "Other turn relation structures", "Directed graph scenarios verified", "Figures generated", "Structural checks passed", "Structural checks total"), value = c(nrow(nodes), nrow(ways), nrow(segments), sum(nodes$inside_country), sum(segments$fully_inside_country), sum(!segments$fully_inside_country), barriers[on_network == 
        TRUE & startsWith(barrier_status, "BLOCKED"), .N], barriers[on_network == TRUE & barrier_status == "UNCERTAIN", .N], length(expanded_ids), primary$retained_native_segments, primary$active_vertices, primary$directed_arcs, primary$road_length_km, primary$weak_components, primary$strong_components, primary$largest_weak_share_pct, primary$largest_strong_share_pct, nrow(restriction_audit), sum(json_audit$members_parsed), sum(restriction_audit$relation_status == "SIMPLE_VIA_NODE"), sum(restriction_audit$relation_status == 
        "COMPLEX_VIA_WAY"), sum(restriction_audit$relation_status == "OTHER_REQUIRES_REVIEW"), nrow(graph_summary), 4, sum(checks$passed), nrow(checks)))
    summary_path <- file.path(results_dir, "phase8B2_master_summary.csv")
    fwrite(summary, summary_path)
    assumptions <- data.table(assumption = c("Graph topology", "Vehicle profile", "Motor access precedence", "Missing access information", "Destination-restricted roads", "Road direction", "Conditional access", "Surface restrictions", "Ford crossings", "Hard barriers", "Uncertain barriers", "Vehicle physical clearance", "National boundary", "Disconnected components", "Turn relation JSON parsing", "Turn restriction member availability", "Turn restriction enforcement", "Road travel speeds", "Travel-time estimation", 
        "Recovery strategy"), treatment = c("Original OSM node and way identifiers", "Provisional light motor-vehicle proxy", "motorcar then motor_vehicle then vehicle then access", "Modelled road-class defaults; not verified permission", "Excluded from through-routing network", "Explicit tags plus selected implied direction rules", "Unresolved conditional restrictions excluded", "Selected severe surface and smoothness classes excluded", "Tagged ford segments excluded", "Incident segments removed under provisional access rules", 
        "Retained in primary and excluded in strict sensitivity", "Not yet assessed against defined imaging vehicle dimensions", "Complete projected segment covered by supplied Ghana polygon", "All eligible components retained", "Original CSV reread using standard quote handling", "Member identifiers checked separately for each network", "Not yet enforced; routing graph remains provisional", "Not yet assigned", "Not yet calculated", "Existing saved graphs independently revalidated without reconstruction"))
    assumptions_path <- file.path(repro_dir, "phase8B2_modelling_assumptions.csv")
    fwrite(assumptions, assumptions_path)
    session_path <- file.path(repro_dir, "phase8B2_R_session_info.txt")
    writeLines(capture.output(sessionInfo()), session_path)
    script_path <- file.path(scripts_dir, "phase8B2_finalize_recovery.R")
    script_code <- c("phase8B2_finalize <-", deparse(phase8B2_finalize, width.cutoff = 500L), "", "phase8B2_final_results <- phase8B2_finalize()")
    stopifnot(length(parse(text = script_code)) > 0)
    writeLines(script_code, script_path)
    stopifnot(length(parse(file = script_path)) > 0)
    output_files <- sort(unique(c(graph_files, inventory_paths, restriction_rds, figure_files, file.path(root, names(outputs)), file.path(audit_dir, "phase8B2_turn_json_parse_audit.csv"), file.path(audit_dir, "phase8B2_turn_relation_members.csv"), quality_path, summary_path, assumptions_path, session_path, script_path)))
    output_files <- normalizePath(output_files, winslash = "/", mustWork = TRUE)
    stopifnot(all(file.exists(output_files)), all(file.info(output_files)$size > 0))
    manifest <- data.table(filename = basename(output_files), relative_path = substring(output_files, nchar(root) + 2), size_bytes = file.info(output_files)$size, SHA256 = hash_files(output_files))
    manifest_path <- file.path(repro_dir, "phase8B2_output_checksum_manifest.csv")
    fwrite(manifest, manifest_path)
    recheck <- fread(manifest_path)
    recheck[, `:=`(current_SHA256, hash_files(file.path(root, relative_path)))]
    recheck[, `:=`(verified, current_SHA256 == SHA256)]
    stopifnot(all(recheck$verified), identical(hash_files(previous_paths), previous_hashes), identical(hash_files(graph_files), graph_hashes_before), identical(hash_files(inventory_paths), inventory_hashes_before))
    fwrite(recheck, file.path(repro_dir, "phase8B2_checksum_recheck.csv"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    writeLines(c("GHANA DIAGNOSTIC IMAGING RESEARCH", "PHASE 8B2 FINAL RECOVERY", "Original graph reconstruction repeated: NO", "Original PBF extraction repeated: NO", "Original graph files unchanged: YES", "Previous Phase 8B1 outputs unchanged: YES", paste("Graph scenarios independently verified:", nrow(graph_summary)), paste("Turn restriction JSON records parsed:", sum(json_audit$members_parsed), "/154"), paste("Quality checks:", sum(checks$passed), "/", nrow(checks)), paste("Output checksums:", 
        sum(recheck$verified), "/", nrow(recheck)), paste("Execution seconds:", round(elapsed, 2)), "Structural quality status: PASS", "Turn restriction enforcement: PENDING", "Vehicle-specific clearance: PENDING", "Travel-speed modelling: PENDING", "Travel-time calculation: PENDING"), file.path(logs_dir, "phase8B2_execution_log.txt"))
    cat("\n========================================\n")
    cat("PHASE 8B2 FINAL RECOVERY RESULTS\n")
    cat("========================================\n")
    print(summary, nrows = 100)
    cat("\nGRAPH SCENARIO SUMMARY\n")
    print(graph_summary)
    cat("\nINDEPENDENT GRAPH VERIFICATION\n")
    print(graph_verification)
    cat("\nCORRECTED TURN RESTRICTION SUMMARY\n")
    print(restriction_summary)
    cat("\nTURN RESTRICTION SCENARIO PRESENCE\n")
    print(relation_presence_summary)
    cat("\nNETWORK NESTING\n")
    print(network_nesting)
    cat("\nFINAL QUALITY CHECKS\n")
    print(checks, nrows = 100)
    cat("\nPrevious checksums verified:", length(previous_hashes), "/31\n")
    cat("Final output checksums verified:", sum(recheck$verified), "/", nrow(recheck), "\n")
    cat("Recovery execution seconds:", round(elapsed, 2), "\n")
    cat("\nPHASE 8B2 STRUCTURAL STATUS: PASS\n")
    cat("ALL THREE SAVED GRAPHS VERIFIED\n")
    cat("NO GRAPH RECONSTRUCTION REPEATED\n")
    cat("TURN RELATION PARSING CORRECTED\n")
    cat("ALL FOUR FIGURES COMPLETED\n")
    cat("TURN RESTRICTION ENFORCEMENT PENDING\n")
    invisible(list(summary = summary, graph_summary = graph_summary, graph_verification = graph_verification, reconciliation = reconciliation, access_summary = access_summary, direction_summary = direction_summary, suitability_summary = suitability_summary, barrier_summary = barrier_summary, boundary_summary = boundary_summary, eligibility_summary = eligibility_summary, restriction_audit = restriction_audit, restriction_summary = restriction_summary, relation_presence_summary = relation_presence_summary, 
        network_nesting = network_nesting, checks = checks, manifest = recheck))
}

phase8B2_final_results <- phase8B2_finalize()
