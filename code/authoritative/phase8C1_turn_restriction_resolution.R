phase8C1_run <-
function () 
{
    suppressPackageStartupMessages({
        library(data.table)
        library(digest)
        library(jsonlite)
    })
    start_time <- Sys.time()
    root <- normalizePath(file.path(Sys.getenv("USERPROFILE"), "Downloads", "AI PREDICTION", "Ghana_Diagnostic_Imaging_Research"), winslash = "/", mustWork = TRUE)
    audit_dir <- file.path(root, "03_Data_Audit")
    results_dir <- file.path(root, "08_Results")
    tables_dir <- file.path(root, "10_Tables")
    repro_dir <- file.path(root, "12_Reproducibility")
    scripts_dir <- file.path(root, "13_R_Scripts")
    logs_dir <- file.path(root, "16_Research_Logs")
    graph_dir <- file.path(root, "04_Spatial_Analysis", "National_Road_Network", "Directed_Graphs")
    turn_dir <- file.path(root, "04_Spatial_Analysis", "National_Road_Network", "Turn_Restrictions")
    dir.create(turn_dir, recursive = TRUE, showWarnings = FALSE)
    hash_files <- function(paths) {
        stopifnot(all(file.exists(paths)))
        vapply(paths, function(p) {
            digest(file = p, algo = "sha256")
        }, character(1), USE.NAMES = FALSE)
    }
    cat("\n========================================\n")
    cat("PHASE 8C1: TURN RESTRICTION RESOLUTION\n")
    cat("========================================\n")
    manifest <- fread(file.path(repro_dir, "phase8B2_output_checksum_manifest.csv"))
    previous_paths <- file.path(root, manifest$relative_path)
    previous_hashes <- hash_files(previous_paths)
    stopifnot(nrow(manifest) == 52, identical(previous_hashes, as.character(manifest$SHA256)))
    quality <- fread(file.path(audit_dir, "phase8B2_quality_gate.csv"))
    stopifnot(nrow(quality) == 36, all(quality$passed))
    cat("Previous checksums: 52/52 PASS\n")
    cat("Previous quality checks: 36/36 PASS\n")
    turn_data <- readRDS(file.path(graph_dir, "ghana_turn_restrictions_corrected.rds"))
    relations <- as.data.table(turn_data$restrictions)
    members <- as.data.table(turn_data$members)
    relation_audit <- as.data.table(turn_data$audit)
    ways <- as.data.table(readRDS(file.path(graph_dir, "ghana_way_access_direction_audit.rds")))
    stopifnot(nrow(relations) == 154, nrow(relation_audit) == 154, !anyDuplicated(relations$relation_id), !anyDuplicated(ways$way_id), sum(relation_audit$relation_status == "SIMPLE_VIA_NODE") == 121, sum(relation_audit$relation_status == "COMPLEX_VIA_WAY") == 33)
    members[, `:=`(member_ref, as.character(member_ref))]
    members[, `:=`(member_type, tolower(member_type))]
    members[, `:=`(member_role, tolower(member_role))]
    setorder(members, relation_id, member_order)
    cat("Original restriction inventory: PASS\n")
    get_tag <- function(tags, nm) {
        x <- tags[[nm]]
        if (is.null(x) || length(x) == 0L) {
            return("")
        }
        x <- trimws(tolower(as.character(x[1])))
        if (is.na(x)) {
            return("")
        }
        x
    }
    profile_rows <- lapply(seq_len(nrow(relations)), function(i) {
        tags <- fromJSON(relations$tags_json[i], simplifyVector = FALSE)
        specific <- get_tag(tags, "restriction:motorcar")
        motor <- get_tag(tags, "restriction:motor_vehicle")
        vehicle <- get_tag(tags, "restriction:vehicle")
        general <- get_tag(tags, "restriction")
        restriction_type <- c(specific, motor, vehicle, general)
        restriction_type <- restriction_type[nzchar(restriction_type)]
        effective <- if (length(restriction_type) > 0L) {
            restriction_type[1]
        }
        else {
            ""
        }
        exceptions <- get_tag(tags, "except")
        except_values <- trimws(unlist(strsplit(exceptions, "[;,]")))
        exempt <- any(except_values %in% c("motorcar", "motor_vehicle", "vehicle"))
        tag_names <- names(tags)
        conditional <- any(grepl("^restriction.*:conditional$", tag_names))
        supported <- effective %in% c("no_left_turn", "no_right_turn", "no_u_turn", "no_straight_on", "no_entry", "no_exit", "only_left_turn", "only_right_turn", "only_straight_on", "only_u_turn")
        applicability <- if (exempt) {
            "EXEMPT_PROFILE"
        }
        else if (conditional) {
            "CONDITIONAL_REVIEW"
        }
        else if (effective == "") {
            "NO_APPLICABLE_RESTRICTION"
        }
        else if (!supported) {
            "UNSUPPORTED_TYPE"
        }
        else {
            "APPLICABLE"
        }
        data.table(relation_id = relations$relation_id[i], effective_restriction = effective, exception_tags = exceptions, profile_exempt = exempt, conditional_restriction = conditional, applicability = applicability)
    })
    profile_audit <- rbindlist(profile_rows)
    cat("\nVEHICLE PROFILE APPLICABILITY\n")
    print(profile_audit[, .N, by = applicability])
    scenarios <- c("CORE_PRIMARY", "EXPANDED", "CORE_STRICT_BARRIERS")
    presence_path <- file.path(audit_dir, "phase8B2_turn_relation_scenario_presence.csv")
    previous_presence <- fread(presence_path, colClasses = list(character = "relation_id"))
    stopifnot(nrow(previous_presence) == 462)
    empty_transitions <- function() {
        data.table(relation_id = character(), restriction = character(), via_vertex = integer(), incoming_arc = integer(), outgoing_arc = integer(), rule_type = character())
    }
    empty_candidates <- function() {
        data.table(relation_id = character(), member_order = integer(), member_type = character(), member_ref = character(), member_role = character(), candidate_arcs = integer(), member_present = logical())
    }
    resolve_scenario <- function(nm) {
        cat("\n========================================\n")
        cat("RESOLVING:", nm, "\n")
        cat("========================================\n")
        vertices <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_vertices.rds"))))
        arcs <- as.data.table(readRDS(file.path(graph_dir, paste0(nm, "_arcs.rds"))))
        stopifnot(!anyDuplicated(vertices$vertex_id), !anyDuplicated(arcs$arc_id), identical(as.integer(vertices$vertex_id), seq_len(nrow(vertices))), identical(as.integer(arcs$arc_id), seq_len(nrow(arcs))))
        referenced_way_ids <- unique(members[member_type %in% c("w", "way"), member_ref])
        referenced_node_ids <- unique(members[member_type %in% c("n", "node"), member_ref])
        referenced_way_indices <- match(referenced_way_ids, ways$way_id)
        referenced_way_indices <- referenced_way_indices[!is.na(referenced_way_indices)]
        relevant_vertices <- vertices[node_id %chin% referenced_node_ids, .(node_id, vertex_id)]
        relevant_vertex_ids <- relevant_vertices$vertex_id
        relevant_arcs <- arcs[way_index %in% referenced_way_indices | from %in% relevant_vertex_ids | to %in% relevant_vertex_ids, .(arc_id, segment_id, way_index, from, to, length_m)]
        relevant_arcs[, `:=`(way_id, ways$way_id[way_index])]
        stopifnot(!anyNA(relevant_arcs$way_id))
        cat("Restriction-related candidate arcs:", nrow(relevant_arcs), "\n")
        status_rows <- vector("list", nrow(relations))
        transition_rows <- vector("list", nrow(relations))
        candidate_rows <- vector("list", nrow(relations))
        complex_rows <- vector("list", nrow(relations))
        for (i in seq_len(nrow(relations))) {
            rid <- as.character(relations$relation_id[i])
            m <- members[relation_id == rid]
            p <- profile_audit[relation_id == rid]
            structure_type <- relation_audit[relation_id == rid, relation_status]
            restriction <- p$effective_restriction[1]
            applicability <- p$applicability[1]
            candidates <- copy(m)
            candidates[, `:=`(candidate_arcs = 0L, member_present = FALSE)]
            for (j in seq_len(nrow(candidates))) {
                ref <- candidates$member_ref[j]
                typ <- candidates$member_type[j]
                if (typ %in% c("w", "way")) {
                  count <- relevant_arcs[way_id == ref, .N]
                  candidates[j, `:=`(candidate_arcs = count, member_present = count > 0L)]
                }
                else if (typ %in% c("n", "node")) {
                  found <- ref %in% relevant_vertices$node_id
                  candidates[j, `:=`(member_present, found)]
                }
            }
            candidate_rows[[i]] <- candidates
            all_present <- all(candidates$member_present)
            status <- "NOT_RESOLVED"
            incoming_count <- 0L
            outgoing_count <- 0L
            forbidden_count <- 0L
            transitions <- empty_transitions()
            if (structure_type == "COMPLEX_VIA_WAY") {
                ordered_way_ids <- c(m[member_role == "from" & member_type %in% c("w", "way"), member_ref], m[member_role == "via" & member_type %in% c("w", "way"), member_ref], m[member_role == "to" & member_type %in% c("w", "way"), member_ref])
                pair_rows <- list()
                if (length(ordered_way_ids) >= 2L) {
                  for (j in seq_len(length(ordered_way_ids) - 1L)) {
                    left_way <- ordered_way_ids[j]
                    right_way <- ordered_way_ids[j + 1L]
                    left_arcs <- relevant_arcs[way_id == left_way]
                    right_arcs <- relevant_arcs[way_id == right_way]
                    shared_vertices <- intersect(unique(left_arcs$to), unique(right_arcs$from))
                    pair_rows[[j]] <- data.table(relation_id = rid, pair_order = j, from_way = left_way, to_way = right_way, directed_shared_vertices = length(shared_vertices), junction_vertex_ids = paste(shared_vertices, collapse = ";"))
                  }
                }
                if (length(pair_rows) > 0L) {
                  pair_table <- rbindlist(pair_rows)
                  complex_rows[[i]] <- pair_table
                }
                else {
                  pair_table <- data.table(directed_shared_vertices = integer())
                }
                if (applicability != "APPLICABLE") {
                  status <- applicability
                }
                else if (!all_present) {
                  status <- "COMPLEX_MEMBER_ABSENT"
                }
                else if (nrow(pair_table) == 0L || any(pair_table$directed_shared_vertices == 0)) {
                  status <- "COMPLEX_DIRECTED_LINK_MISSING"
                }
                else {
                  status <- "COMPLEX_SEQUENCE_PENDING"
                }
            }
            else if (applicability != "APPLICABLE") {
                status <- applicability
            }
            else if (structure_type != "SIMPLE_VIA_NODE") {
                status <- "OTHER_STRUCTURE_REVIEW"
            }
            else {
                from_way <- m[member_role == "from" & member_type %in% c("w", "way"), member_ref]
                to_way <- m[member_role == "to" & member_type %in% c("w", "way"), member_ref]
                via_node <- m[member_role == "via" & member_type %in% c("n", "node"), member_ref]
                stopifnot(length(from_way) == 1L, length(to_way) == 1L, length(via_node) == 1L)
                via_vertex <- relevant_vertices[node_id == via_node, vertex_id]
                if (length(via_vertex) != 1L) {
                  status <- "VIA_NODE_ABSENT"
                }
                else {
                  incoming <- relevant_arcs[way_id == from_way & to == via_vertex]
                  outgoing_all <- relevant_arcs[from == via_vertex]
                  designated <- outgoing_all[way_id == to_way]
                  incoming_count <- nrow(incoming)
                  outgoing_count <- nrow(designated)
                  if (incoming_count == 0L) {
                    status <- "INCOMING_MOVEMENT_ABSENT"
                  }
                  else if (incoming_count > 1L) {
                    status <- "AMBIGUOUS_INCOMING_MOVEMENT"
                  }
                  else if (outgoing_count == 0L) {
                    if (startsWith(restriction, "no_")) {
                      status <- "PROHIBITED_MOVEMENT_ABSENT"
                    }
                    else {
                      status <- "MANDATORY_MOVEMENT_ABSENT_REVIEW"
                    }
                  }
                  else {
                    if (restriction %in% c("no_u_turn", "only_u_turn") && from_way == to_way) {
                      designated <- designated[segment_id == incoming$segment_id[1] & to == incoming$from[1]]
                    }
                    if (nrow(designated) == 0L) {
                      status <- if (startsWith(restriction, "no_")) {
                        "PROHIBITED_MOVEMENT_ABSENT"
                      }
                      else {
                        "MANDATORY_MOVEMENT_ABSENT_REVIEW"
                      }
                    }
                    else if (nrow(designated) > 1L) {
                      status <- "AMBIGUOUS_OUTGOING_MOVEMENT"
                    }
                    else {
                      if (startsWith(restriction, "no_")) {
                        forbidden <- designated
                        rule_type <- "PROHIBITED_TURN"
                      }
                      else {
                        forbidden <- outgoing_all[!arc_id %in% designated$arc_id]
                        rule_type <- "MANDATORY_TURN"
                      }
                      if (nrow(forbidden) > 0L) {
                        transitions <- data.table(relation_id = rid, restriction = restriction, via_vertex = as.integer(via_vertex), incoming_arc = as.integer(incoming$arc_id[1]), outgoing_arc = as.integer(forbidden$arc_id), rule_type = rule_type)
                        forbidden_count <- nrow(transitions)
                        status <- "SIMPLE_RESOLVED"
                      }
                      else {
                        status <- "NO_ALTERNATIVE_MOVEMENT"
                      }
                    }
                  }
                }
            }
            transition_rows[[i]] <- transitions
            status_rows[[i]] <- data.table(relation_id = rid, relation_structure = structure_type, effective_restriction = restriction, applicability = applicability, all_members_present = all_present, incoming_candidates = incoming_count, outgoing_candidates = outgoing_count, forbidden_transitions = forbidden_count, resolution_status = status)
        }
        resolution <- rbindlist(status_rows, fill = TRUE)
        transitions <- rbindlist(c(list(empty_transitions()), transition_rows), fill = TRUE)
        candidate_table <- rbindlist(c(list(empty_candidates()), candidate_rows), fill = TRUE)
        complex_pairs <- rbindlist(complex_rows, fill = TRUE)
        resolution[, `:=`(scenario, nm)]
        transitions[, `:=`(scenario, nm)]
        candidate_table[, `:=`(scenario, nm)]
        if (nrow(complex_pairs) > 0L) {
            complex_pairs[, `:=`(scenario, nm)]
        }
        stopifnot(nrow(resolution) == 154, !anyDuplicated(resolution$relation_id))
        if (nrow(transitions) > 0L) {
            incoming_positions <- match(transitions$incoming_arc, arcs$arc_id)
            outgoing_positions <- match(transitions$outgoing_arc, arcs$arc_id)
            stopifnot(!anyNA(incoming_positions), !anyNA(outgoing_positions), all(arcs$to[incoming_positions] == transitions$via_vertex), all(arcs$from[outgoing_positions] == transitions$via_vertex), all(transitions$incoming_arc != transitions$outgoing_arc))
            rm(incoming_positions, outgoing_positions)
        }
        unique_forbidden <- unique(transitions[, .(incoming_arc, outgoing_arc)])
        setorder(unique_forbidden, incoming_arc, outgoing_arc)
        prev <- previous_presence[scenario == nm, .(relation_id, previous_all_members_present = all_members_present)]
        presence_recheck <- merge(resolution[, .(relation_id, all_members_present)], prev, by = "relation_id", all = TRUE)
        presence_recheck[, `:=`(presence_matches, !is.na(all_members_present) & !is.na(previous_all_members_present) & all_members_present == previous_all_members_present)]
        presence_recheck[, `:=`(scenario, nm)]
        stopifnot(nrow(presence_recheck) == 154, all(presence_recheck$presence_matches))
        scenario_summary <- data.table(scenario = nm, relations = nrow(resolution), simple_relations = sum(resolution$relation_structure == "SIMPLE_VIA_NODE"), complex_relations = sum(resolution$relation_structure == "COMPLEX_VIA_WAY"), all_members_present = sum(resolution$all_members_present), simple_resolved = sum(resolution$resolution_status == "SIMPLE_RESOLVED"), complex_sequence_pending = sum(resolution$resolution_status == "COMPLEX_SEQUENCE_PENDING"), relation_level_forbidden_pairs = nrow(transitions), 
            unique_forbidden_pairs = nrow(unique_forbidden), member_presence_checks_passed = sum(presence_recheck$presence_matches), graph_vertices = nrow(vertices), graph_arcs = nrow(arcs))
        status_summary <- resolution[, .N, by = .(scenario, relation_structure, resolution_status)]
        print(scenario_summary)
        print(status_summary, nrows = 100)
        output_path <- file.path(turn_dir, paste0(nm, "_turn_resolution.rds"))
        saveRDS(list(scenario = nm, relation_resolution = resolution, relation_transitions = transitions, unique_forbidden_transitions = unique_forbidden, complex_way_pairs = complex_pairs, member_candidates = candidate_table, profile = "LIGHT_MOTORCAR_PROXY", enforcement_status = "PREPARED_NOT_APPLIED_TO_ROUTER"), output_path)
        stopifnot(file.exists(output_path), file.info(output_path)$size > 0)
        result <- list(summary = scenario_summary, resolution = resolution, transitions = transitions, unique_forbidden = unique_forbidden, candidates = candidate_table, complex_pairs = complex_pairs, presence_recheck = presence_recheck, status_summary = status_summary, output_file = output_path)
        rm(arcs, vertices, relevant_arcs, relevant_vertices)
        gc()
        cat(nm, "TURN RESOLUTION AUDIT: PASS\n")
        result
    }
    scenario_results <- setNames(lapply(scenarios, resolve_scenario), scenarios)
    collect <- function(nm) {
        rbindlist(lapply(scenario_results, function(z) {
            z[[nm]]
        }), fill = TRUE)
    }
    scenario_summary <- collect("summary")
    all_resolution <- collect("resolution")
    all_transitions <- collect("transitions")
    all_candidates <- collect("candidates")
    all_complex_pairs <- collect("complex_pairs")
    all_presence <- collect("presence_recheck")
    status_summary <- collect("status_summary")
    stopifnot(nrow(all_resolution) == 462, nrow(all_presence) == 462, all(all_presence$presence_matches))
    cat("\nALL NETWORK SCENARIOS RESOLVED OR AUDITED\n")
    unresolved <- all_resolution[grepl("PENDING|REVIEW|AMBIGUOUS|UNSUPPORTED|MISSING", resolution_status)]
    unresolved_summary <- unresolved[, .N, by = .(scenario, resolution_status)]
    cat("\nRESTRICTIONS REQUIRING FURTHER ROUTING WORK\n")
    print(unresolved_summary, nrows = 100)
    toy_arcs <- data.table(arc_id = 1:6, from = c(1L, 2L, 2L, 2L, 3L, 4L), to = c(2L, 3L, 4L, 1L, 2L, 2L))
    toy_prohibited <- data.table(incoming_arc = 1L, outgoing_arc = 3L)
    toy_mandatory <- data.table(incoming_arc = c(1L, 1L), outgoing_arc = c(3L, 4L))
    transition_allowed <- function(incoming, outgoing, forbidden) {
        a <- toy_arcs[arc_id == incoming]
        b <- toy_arcs[arc_id == outgoing]
        if (nrow(a) != 1L || nrow(b) != 1L || a$to != b$from) {
            return(FALSE)
        }
        nrow(forbidden[incoming_arc == incoming & outgoing_arc == outgoing]) == 0L
    }
    unit_tests <- data.table(test = c("Prohibited turn rejected", "Alternative turn allowed", "Different incoming arc remains allowed", "Mandatory designated movement allowed", "Mandatory alternative movement rejected", "Mandatory U-turn alternative rejected", "Nonconnecting movement rejected", "Same-way reverse segment identified"), passed = c(!transition_allowed(1L, 3L, toy_prohibited), transition_allowed(1L, 2L, toy_prohibited), transition_allowed(5L, 3L, toy_prohibited), transition_allowed(1L, 
        2L, toy_mandatory), !transition_allowed(1L, 3L, toy_mandatory), !transition_allowed(1L, 4L, toy_mandatory), !transition_allowed(1L, 5L, toy_prohibited), toy_arcs$from[1] == toy_arcs$to[4] && toy_arcs$to[1] == toy_arcs$from[4]))
    stopifnot(all(unit_tests$passed))
    cat("\nSYNTHETIC TRANSITION TESTS\n")
    print(unit_tests)
    expected_presence <- data.table(scenario = scenarios, expected_members_present = c(147L, 154L, 147L))
    scenario_recheck <- merge(scenario_summary, expected_presence, by = "scenario")
    scenario_recheck[, `:=`(matches, all_members_present == expected_members_present)]
    stopifnot(all(scenario_recheck$matches))
    actual_counts <- all_transitions[, .(actual_forbidden_pairs = .N), by = .(scenario, relation_id)]
    count_recheck <- merge(all_resolution[, .(scenario, relation_id, forbidden_transitions)], actual_counts, by = c("scenario", "relation_id"), all.x = TRUE)
    count_recheck[is.na(actual_forbidden_pairs), `:=`(actual_forbidden_pairs, 0L)]
    count_recheck[, `:=`(matches, forbidden_transitions == actual_forbidden_pairs)]
    stopifnot(nrow(count_recheck) == 462, all(count_recheck$matches))
    cat("Cross-scenario reconciliation: PASS\n")
    scenario_files <- vapply(scenario_results, function(z) {
        z$output_file
    }, character(1), USE.NAMES = FALSE)
    checks <- data.table(check = c("Previous 52 checksums verified", "Previous 36 quality checks passed", "Previous network outputs unchanged", "All 154 original restrictions retained", "121 simple relations reconciled", "33 complex relations reconciled", "All vehicle applicability records retained", "Three network scenarios audited", "462 scenario-relation records retained", "462 presence checks reproduced", "Original scenario presence counts reproduced", "Resolved transition counts reconciled", 
        "All resolved transitions have valid arc references", "Resolved transitions belong to matching via nodes", "No duplicate scenario-relation records", "Unresolved restrictions explicitly inventoried", "Complex restrictions not falsely marked enforced", "Simple restrictions not falsely marked routed", "Synthetic transition tests passed", "All scenario output files available"), passed = c(all(previous_hashes == manifest$SHA256), all(quality$passed), identical(hash_files(previous_paths), previous_hashes), 
        nrow(relations) == 154, sum(relation_audit$relation_status == "SIMPLE_VIA_NODE") == 121, sum(relation_audit$relation_status == "COMPLEX_VIA_WAY") == 33, nrow(profile_audit) == 154, nrow(scenario_summary) == 3, nrow(all_resolution) == 462, all(all_presence$presence_matches), all(scenario_recheck$matches), all(count_recheck$matches), all(all_transitions$incoming_arc > 0 & all_transitions$outgoing_arc > 0), all(all_transitions$via_vertex > 0), uniqueN(all_resolution, by = c("scenario", "relation_id")) == 
            462, all(unresolved$relation_id %in% relations$relation_id), !any(all_resolution[relation_structure == "COMPLEX_VIA_WAY", resolution_status] == "SIMPLE_RESOLVED"), !any(grepl("ROUTED|ENFORCED", all_resolution$resolution_status)), all(unit_tests$passed), length(scenario_files) == 3 && all(file.exists(scenario_files)) && all(file.info(scenario_files)$size > 0)))
    checks[is.na(passed), `:=`(passed, FALSE)]
    checks[, `:=`(status, fifelse(passed, "PASS", "FAIL"))]
    quality_path <- file.path(audit_dir, "phase8C1_quality_gate.csv")
    fwrite(checks, quality_path)
    print(checks, nrows = 100)
    stopifnot(nrow(checks) == 20, all(checks$passed))
    outputs <- list(`03_Data_Audit/phase8C1_vehicle_profile_audit.csv` = profile_audit, `03_Data_Audit/phase8C1_relation_resolution.csv` = all_resolution, `03_Data_Audit/phase8C1_member_candidates.csv` = all_candidates, `03_Data_Audit/phase8C1_complex_way_pairs.csv` = all_complex_pairs, `03_Data_Audit/phase8C1_presence_recheck.csv` = all_presence, `03_Data_Audit/phase8C1_transition_count_recheck.csv` = count_recheck, `03_Data_Audit/phase8C1_unresolved_relations.csv` = unresolved, `03_Data_Audit/phase8C1_synthetic_unit_tests.csv` = unit_tests, 
        `10_Tables/phase8C1_scenario_summary.csv` = scenario_summary, `10_Tables/phase8C1_resolution_status_summary.csv` = status_summary, `10_Tables/phase8C1_unresolved_summary.csv` = unresolved_summary, `10_Tables/phase8C1_scenario_reconciliation.csv` = scenario_recheck, `02_Processed_Data/phase8C1_forbidden_transitions.csv` = all_transitions)
    for (nm in names(outputs)) {
        fwrite(outputs[[nm]], file.path(root, nm))
    }
    summary <- data.table(metric = c("Original turn restriction relations", "Original simple via-node relations", "Original complex via-way relations", "Network scenarios", "Scenario-relation records", "Simple resolved scenario-relations", "Complex sequence pending scenario-relations", "Relation-level forbidden transition records", "Unique scenario-specific forbidden pairs", "Member presence checks passed", "Synthetic unit tests passed", "Synthetic unit tests total", "Structural checks passed", "Structural checks total"), 
        value = c(nrow(relations), 121, 33, nrow(scenario_summary), nrow(all_resolution), sum(scenario_summary$simple_resolved), sum(scenario_summary$complex_sequence_pending), nrow(all_transitions), sum(scenario_summary$unique_forbidden_pairs), sum(all_presence$presence_matches), sum(unit_tests$passed), nrow(unit_tests), sum(checks$passed), nrow(checks)))
    summary_path <- file.path(results_dir, "phase8C1_master_summary.csv")
    fwrite(summary, summary_path)
    assumptions <- data.table(assumption = c("Vehicle profile", "Restriction applicability", "Simple prohibited turns", "Simple mandatory turns", "Same-way U-turn restrictions", "Ambiguous incoming or outgoing movements", "Absent prohibited movements", "Absent mandatory movements", "Complex via-way restrictions", "Directed connectivity checks", "Turn enforcement", "Travel times", "Graph modification"), treatment = c("Provisional light motorcar proxy", "Motorcar then motor_vehicle then vehicle then general restriction; exceptions retained", 
        "Resolved into incoming and outgoing arc pairs", "All other outgoing arcs identified as prohibited alternatives", "Reverse movement on the same native road segment", "Retained as unresolved rather than guessed", "Recorded when designated prohibited arc is unavailable", "Recorded for further review; not interpreted as unrestricted", "Ordered member ways and adjacent directed junction candidates retained", "Pairwise connectivity is necessary but not sufficient for full sequence validity", "Transition inventory prepared but not yet applied to shortest-path search", 
        "Not yet calculated", "Original verified graphs preserved unchanged"))
    assumptions_path <- file.path(repro_dir, "phase8C1_modelling_assumptions.csv")
    fwrite(assumptions, assumptions_path)
    session_path <- file.path(repro_dir, "phase8C1_R_session_info.txt")
    writeLines(capture.output(sessionInfo()), session_path)
    script_path <- file.path(scripts_dir, "phase8C1_turn_restriction_resolution.R")
    script_code <- c("phase8C1_run <-", deparse(phase8C1_run, width.cutoff = 500L), "", "phase8C1_results <- phase8C1_run()")
    stopifnot(length(parse(text = script_code)) > 0)
    writeLines(script_code, script_path)
    output_files <- sort(unique(c(scenario_files, file.path(root, names(outputs)), quality_path, summary_path, assumptions_path, session_path, script_path)))
    output_files <- normalizePath(output_files, winslash = "/", mustWork = TRUE)
    stopifnot(all(file.info(output_files)$size > 0))
    output_manifest <- data.table(filename = basename(output_files), relative_path = substring(output_files, nchar(root) + 2), size_bytes = file.info(output_files)$size, SHA256 = hash_files(output_files))
    manifest_path <- file.path(repro_dir, "phase8C1_output_checksum_manifest.csv")
    fwrite(output_manifest, manifest_path)
    recheck <- fread(manifest_path)
    recheck[, `:=`(current_SHA256, hash_files(file.path(root, relative_path)))]
    recheck[, `:=`(verified, current_SHA256 == SHA256)]
    stopifnot(all(recheck$verified), identical(hash_files(previous_paths), previous_hashes))
    fwrite(recheck, file.path(repro_dir, "phase8C1_checksum_recheck.csv"))
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    writeLines(c("GHANA DIAGNOSTIC IMAGING RESEARCH", "PHASE 8C1 TURN RESTRICTION RESOLUTION", paste("Scenario-relation records:", nrow(all_resolution)), paste("Simple resolved scenario-relations:", sum(scenario_summary$simple_resolved)), paste("Unique scenario-specific forbidden pairs:", sum(scenario_summary$unique_forbidden_pairs)), paste("Quality checks:", sum(checks$passed), "/", nrow(checks)), paste("Output checksums:", sum(recheck$verified), "/", nrow(recheck)), paste("Execution seconds:", 
        round(elapsed, 2)), "Structural resolution status: PASS", "Previous graphs preserved", "Complex sequence enforcement pending", "Turn-aware shortest-path integration pending", "Travel-speed modelling pending"), file.path(logs_dir, "phase8C1_execution_log.txt"))
    cat("\n========================================\n")
    cat("PHASE 8C1 FINAL RESULTS\n")
    cat("========================================\n")
    print(summary, nrows = 100)
    cat("\nSCENARIO SUMMARY\n")
    print(scenario_summary)
    cat("\nRESOLUTION STATUS SUMMARY\n")
    print(status_summary, nrows = 100)
    cat("\nUNRESOLVED RESTRICTIONS\n")
    print(unresolved_summary, nrows = 100)
    cat("\nVEHICLE APPLICABILITY SUMMARY\n")
    print(profile_audit[, .N, by = applicability])
    cat("\nSYNTHETIC TRANSITION TESTS\n")
    print(unit_tests)
    cat("\nQUALITY CHECKS\n")
    print(checks, nrows = 100)
    cat("\nPrevious checksums verified:", length(previous_hashes), "/52\n")
    cat("New output checksums verified:", sum(recheck$verified), "/", nrow(recheck), "\n")
    cat("Execution seconds:", round(elapsed, 2), "\n")
    cat("\nPHASE 8C1 STRUCTURAL STATUS: PASS\n")
    cat("ORIGINAL GRAPHS PRESERVED\n")
    cat("SIMPLE TURN TRANSITION INVENTORY PREPARED\n")
    cat("COMPLEX WAY SEQUENCES AUDITED\n")
    cat("FULL ROUTING ENFORCEMENT PENDING\n")
    cat("READY FOR NEXT ROUTING MODELLING STEP\n")
    invisible(list(summary = summary, scenario_summary = scenario_summary, resolution = all_resolution, transitions = all_transitions, status_summary = status_summary, complex_pairs = all_complex_pairs, unresolved = unresolved, unresolved_summary = unresolved_summary, profile_audit = profile_audit, unit_tests = unit_tests, checks = checks, manifest = recheck))
}

phase8C1_results <- phase8C1_run()
