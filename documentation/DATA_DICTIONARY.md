# Analytical output dictionary

Column inventory generated from the uploaded CSVs. Unmapped fields retain their exact source names; consult the generating R phase for detailed definitions. Percentages and percentage-point differences are not interchangeable.

## Table_01_National_baseline.csv (12 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run` | object | Source field; definition to confirm from generating phase. |
| `matrix_id` | object | Source field; definition to confirm from generating phase. |
| `ensemble` | object | Source field; definition to confirm from generating phase. |
| `sample` | object | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `threshold_minutes` | int64 | Source field; definition to confirm from generating phase. |
| `population` | float64 | Modelled population denominator. |
| `mean_pct` | float64 | Mean simulated population coverage, percent. |
| `sd_pct` | float64 | Source field; definition to confirm from generating phase. |
| `mcse_pct` | float64 | Source field; definition to confirm from generating phase. |
| `p025_pct` | float64 | 2.5th empirical simulation percentile, percent. |
| `median_pct` | float64 | Source field; definition to confirm from generating phase. |
| `p975_pct` | float64 | 97.5th empirical simulation percentile, percent. |
| `draws` | int64 | Number of simulation draws used. |

## Table_02_Regional_baseline_60min.csv (68 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run` | object | Source field; definition to confirm from generating phase. |
| `matrix_id` | object | Source field; definition to confirm from generating phase. |
| `ensemble` | object | Source field; definition to confirm from generating phase. |
| `sample` | object | Source field; definition to confirm from generating phase. |
| `region` | object | Administrative region or unassigned accounting group. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `threshold_minutes` | int64 | Source field; definition to confirm from generating phase. |
| `population` | float64 | Modelled population denominator. |
| `mean_pct` | float64 | Mean simulated population coverage, percent. |
| `sd_pct` | float64 | Source field; definition to confirm from generating phase. |
| `mcse_pct` | float64 | Source field; definition to confirm from generating phase. |
| `p025_pct` | float64 | 2.5th empirical simulation percentile, percent. |
| `median_pct` | float64 | Source field; definition to confirm from generating phase. |
| `p975_pct` | float64 | 97.5th empirical simulation percentile, percent. |
| `draws` | int64 | Number of simulation draws used. |

## Table_03_Model_specifications.csv (4 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `model` | object | Optimization specification A–D. |
| `training_scenarios` | int64 | Source field; definition to confirm from generating phase. |
| `mean_weight` | float64 | Source field; definition to confirm from generating phase. |
| `equity_weight` | float64 | Source field; definition to confirm from generating phase. |
| `robustness_weight` | float64 | Source field; definition to confirm from generating phase. |

## Table_04_Primary_optimization_32.csv (12 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `lambda` | float64 | Source field; definition to confirm from generating phase. |
| `gamma` | float64 | Source field; definition to confirm from generating phase. |
| `training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `upper_bound` | float64 | Source field; definition to confirm from generating phase. |
| `absolute_bound_gap` | float64 | Source field; definition to confirm from generating phase. |
| `train_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_min_region_pct` | float64 | Minimum scenario-region expected coverage in the evaluation design, percent. |
| `test_primary_min_region_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `accepted_exchanges` | int64 | Source field; definition to confirm from generating phase. |
| `starts` | int64 | Source field; definition to confirm from generating phase. |

## Table_05a_LP_bounds.csv (12 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `heuristic` | float64 | Source field; definition to confirm from generating phase. |
| `old_upper` | float64 | Source field; definition to confirm from generating phase. |
| `lp_status` | object | Source field; definition to confirm from generating phase. |
| `lp_raw_objective` | float64 | Source field; definition to confirm from generating phase. |
| `lp_upper` | float64 | Source field; definition to confirm from generating phase. |
| `valid_upper` | float64 | Source field; definition to confirm from generating phase. |
| `variables` | int64 | Source field; definition to confirm from generating phase. |
| `constraints` | int64 | Source field; definition to confirm from generating phase. |
| `omitted_terms` | int64 | Source field; definition to confirm from generating phase. |
| `scaled_rhs_relaxation` | int64 | Source field; definition to confirm from generating phase. |
| `tightened_gap` | float64 | Source field; definition to confirm from generating phase. |
| `final_upper` | float64 | Source field; definition to confirm from generating phase. |
| `final_gap` | float64 | Source field; definition to confirm from generating phase. |

## Table_05b_Restricted_exact.csv (12 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `pool_size` | int64 | Source field; definition to confirm from generating phase. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `combinations` | int64 | Source field; definition to confirm from generating phase. |
| `exhaustive_optimum` | float64 | Source field; definition to confirm from generating phase. |
| `greedy` | float64 | Source field; definition to confirm from generating phase. |
| `mip_recalculated` | float64 | Source field; definition to confirm from generating phase. |
| `greedy_gap` | float64 | Source field; definition to confirm from generating phase. |
| `mip_gap` | int64 | Source field; definition to confirm from generating phase. |
| `objective_recheck` | float64 | Source field; definition to confirm from generating phase. |

## Table_06a_Extended_robustness_32.csv (12 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `scenario_mean_pct` | float64 | Equal-design-weight mean national expected coverage across configurations, percent. |
| `worst_scenario_mean_pct` | float64 | Minimum national expected coverage across configurations, percent. |
| `worst_scenario_region_mean_pct` | float64 | Minimum region-specific expected coverage across configurations and regions, percent. |
| `worst_scenario_p05_pct` | float64 | Source field; definition to confirm from generating phase. |
| `mean_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_mean_scenario_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_scenario_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_scenario_p05_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_region_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_region_p05_pct` | float64 | Source field; definition to confirm from generating phase. |

## Table_06b_Extended_D_minus_A.csv (15 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `delta_mean_pp` | float64 | Model D minus A mean national expected coverage in percentage points. |
| `delta_worst_pp` | float64 | Model D minus A worst national expected coverage in percentage points. |
| `delta_region_pp` | float64 | Model D minus A minimum scenario-region expected coverage in percentage points. |

## Table_07a_Weight_sensitivity_32.csv (24 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `pool` | object | Source field; definition to confirm from generating phase. |
| `weight_id` | object | Source field; definition to confirm from generating phase. |
| `lambda` | float64 | Source field; definition to confirm from generating phase. |
| `gamma` | float64 | Source field; definition to confirm from generating phase. |
| `training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `reference_training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_min_region_pct` | float64 | Minimum scenario-region expected coverage in the evaluation design, percent. |
| `test_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_mean_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_worst_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_region_pp` | float64 | Source field; definition to confirm from generating phase. |
| `accepted_exchanges` | int64 | Source field; definition to confirm from generating phase. |
| `nondominated` | bool | Source field; definition to confirm from generating phase. |

## Table_07b_Candidate_sensitivity.csv (45 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `pool` | object | Source field; definition to confirm from generating phase. |
| `training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_min_region_pct` | float64 | Minimum scenario-region expected coverage in the evaluation design, percent. |
| `delta_D_mean_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_worst_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_region_pp` | float64 | Source field; definition to confirm from generating phase. |

## Table_08_Reproduction.csv (20 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `table` | object | Source field; definition to confirm from generating phase. |
| `metric` | object | Metric name; NATIONAL or DRAW_MIN_REGION in paired table. |
| `original_rows` | int64 | Source field; definition to confirm from generating phase. |
| `rerun_rows` | int64 | Source field; definition to confirm from generating phase. |
| `matched_rows` | int64 | Source field; definition to confirm from generating phase. |
| `max_abs_error` | int64 | Maximum absolute difference between original and rerun exported metric. |
| `tolerance` | float64 | Source field; definition to confirm from generating phase. |
| `passed` | bool | Recorded quality check result. |

## Extended_all_allocations.csv (60 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `scenario_mean_pct` | float64 | Equal-design-weight mean national expected coverage across configurations, percent. |
| `worst_scenario_mean_pct` | float64 | Minimum national expected coverage across configurations, percent. |
| `worst_scenario_region_mean_pct` | float64 | Minimum region-specific expected coverage across configurations and regions, percent. |
| `worst_scenario_p05_pct` | float64 | Source field; definition to confirm from generating phase. |
| `mean_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_mean_scenario_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_scenario_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_scenario_p05_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_region_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `paired_draw_worst_region_p05_pct` | float64 | Source field; definition to confirm from generating phase. |

## Primary_all_allocations.csv (60 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `model` | object | Optimization specification A–D. |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `lambda` | float64 | Source field; definition to confirm from generating phase. |
| `gamma` | float64 | Source field; definition to confirm from generating phase. |
| `training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `upper_bound` | float64 | Source field; definition to confirm from generating phase. |
| `absolute_bound_gap` | float64 | Source field; definition to confirm from generating phase. |
| `train_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_min_region_pct` | float64 | Minimum scenario-region expected coverage in the evaluation design, percent. |
| `test_primary_min_region_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `accepted_exchanges` | int64 | Source field; definition to confirm from generating phase. |
| `starts` | int64 | Source field; definition to confirm from generating phase. |

## Paired_draw_comparisons.csv (180 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `aggregate_metric` | object | Aggregation across scenarios before paired comparison; MEAN or WORST as encoded. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `comparison` | object | Source field; definition to confirm from generating phase. |
| `metric` | object | Metric name; NATIONAL or DRAW_MIN_REGION in paired table. |
| `positive_fraction` | float64 | Source field; definition to confirm from generating phase. |
| `negative_fraction` | float64 | Source field; definition to confirm from generating phase. |
| `draws` | int64 | Number of simulation draws used. |
| `mean` | float64 | Mean paired difference, percentage points for coverage metrics. |
| `sd` | float64 | Source field; definition to confirm from generating phase. |
| `mcse` | float64 | Source field; definition to confirm from generating phase. |
| `p025` | float64 | 2.5th empirical percentile of paired difference. |
| `p05` | float64 | Source field; definition to confirm from generating phase. |
| `median` | float64 | Source field; definition to confirm from generating phase. |
| `p975` | float64 | 97.5th empirical percentile of paired difference. |
| `lower5_mean` | float64 | Source field; definition to confirm from generating phase. |

## Selected_D_32_proxy_locations.csv (96 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `selection_order` | int64 | Source field; definition to confirm from generating phase. |
| `candidate_id` | object | Identifier of hypothetical routing proxy, not a verified deployment site. |
| `candidate_type` | object | Source field; definition to confirm from generating phase. |
| `region` | object | Administrative region or unassigned accounting group. |
| `district` | object | Source field; definition to confirm from generating phase. |
| `longitude` | float64 | Proxy longitude (WGS84 degrees). |
| `latitude` | float64 | Proxy latitude (WGS84 degrees). |
| `vertex_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |

## All_sensitivity_allocations.csv (150 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `run_id` | int64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `fleet` | int64 | Number of hypothetical mobile units. |
| `pool` | object | Source field; definition to confirm from generating phase. |
| `weight_id` | object | Source field; definition to confirm from generating phase. |
| `lambda` | float64 | Source field; definition to confirm from generating phase. |
| `gamma` | float64 | Source field; definition to confirm from generating phase. |
| `training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `reference_training_objective` | float64 | Source field; definition to confirm from generating phase. |
| `test_mean_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_worst_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_min_region_pct` | float64 | Minimum scenario-region expected coverage in the evaluation design, percent. |
| `test_primary_pct` | float64 | Source field; definition to confirm from generating phase. |
| `test_gain_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_mean_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_worst_pp` | float64 | Source field; definition to confirm from generating phase. |
| `delta_D_region_pp` | float64 | Source field; definition to confirm from generating phase. |
| `accepted_exchanges` | int64 | Source field; definition to confirm from generating phase. |
| `nondominated` | bool | Source field; definition to confirm from generating phase. |

## Baseline_deficits.csv (3336 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `region` | object | Administrative region or unassigned accounting group. |
| `population` | float64 | Modelled population denominator. |
| `covered` | float64 | Source field; definition to confirm from generating phase. |
| `matched` | float64 | Source field; definition to confirm from generating phase. |
| `mean_pct` | float64 | Mean simulated population coverage, percent. |
| `expected_deficit` | float64 | Source field; definition to confirm from generating phase. |
| `unmatched_population` | float64 | Source field; definition to confirm from generating phase. |
| `matched_deficit` | float64 | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `threshold_minutes` | int64 | Source field; definition to confirm from generating phase. |
| `level` | object | Source field; definition to confirm from generating phase. |
| `district` | object | Source field; definition to confirm from generating phase. |

## Baseline_inequality.csv (48 rows)

| Field | Type | Definition / observed encoding |
|---|---|---|
| `level` | object | Source field; definition to confirm from generating phase. |
| `weighting` | object | Source field; definition to confirm from generating phase. |
| `modality` | object | Imaging configuration (XRAY, ULTRASOUND, COLOCATED; other baseline definitions as recorded). |
| `threshold_minutes` | int64 | Source field; definition to confirm from generating phase. |
| `areas` | int64 | Source field; definition to confirm from generating phase. |
| `mean_pct` | float64 | Mean simulated population coverage, percent. |
| `min_pct` | float64 | Source field; definition to confirm from generating phase. |
| `max_pct` | float64 | Source field; definition to confirm from generating phase. |
| `gap_pp` | float64 | Source field; definition to confirm from generating phase. |
| `weighted_sd_pp` | float64 | Source field; definition to confirm from generating phase. |
| `gini` | float64 | Source field; definition to confirm from generating phase. |
| `theil` | float64 | Source field; definition to confirm from generating phase. |
