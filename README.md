# Ghana robust mobile imaging

**Robust Equity-Aware Spatial Optimization of Mobile Diagnostic Imaging Services Under Infrastructure Uncertainty: A Nationwide Study of Ghana**

Prepared research archive version: **4.1.8** (release and new DOI pending).

## Authors

1. Peter Sarpong — Independent Researcher, USA; [ORCID](https://orcid.org/0009-0000-0871-8285)
2. Charity Antwiwaa Boateng — Gujarat University, Som-Lalit College, Ahmedabad, Gujarat, India; ORCID: 0009-0008-3943-2464
3. Pokuaah Doris Kwarteng — University of Cape Coast, Ghana; [ORCID](https://orcid.org/0009-0006-2095-6984)

## Research scope

Scenario-based strategic allocation of hypothetical X-ray, ultrasound and co-located mobile imaging services in Ghana. The analysis compares national coverage, geographic equity and robustness under uncertain existing infrastructure and transportation conditions. It includes 435 principal allocation experiments and evaluates 60 frozen principal allocations across 145 designed configurations. These are conditional simulated planning outcomes, not verified current service availability or operational deployment recommendations.

## Archive contents

- `code/authoritative/`: 15 recovered R research scripts and one reconstructed C++ routing engine.
- `code/reference/`: earlier reference copies retained for historical transparency; prefer the recovered sources.
- `Phase15D_source_SHA256.csv`: SHA256 checksums for the recovered and reconstructed source files.
- `tables/`: 11 main analytical tables.
- `supplementary/`: 5 supplementary aggregate datasets.
- `documentation/`: data sources, dictionary, environment, reuse and reproducibility notes.
- `code/verify_aggregate_results.R`: separate aggregate consistency utility.

## Reproducibility

The 15 recovered R scripts match historical Phase 14D source checksums. The reconstructed C++ engine matches the Phase 15C candidate checksum; the original Phase 8D engine manifest has not been independently reconciled. The principal 60 allocations were reproduced from saved upstream inputs. This archive is not a complete standalone raw-data-to-results workflow. Earlier stages, upstream simulation ensembles, routing matrices, external datasets, historical manifests and environment dependencies remain necessary. No claim of cross-platform execution or external empirical validation is made. See [source provenance](code/authoritative/README.md) and [reproducibility](documentation/REPRODUCIBILITY.md).

## Citation and release

See [CITATION.cff](CITATION.cff). The previous archive v4.1.7 has reported DOI https://doi.org/10.5281/zenodo.24568759; this DOI refers to the previous version, not the prepared 4.1.8 update. Verify the final version-specific Zenodo record after release.

## Reuse

The authors have confirmed public redistribution of the recovered research code. This is not a blanket software or data license. Third-party source-specific permissions remain applicable. Manuscripts, rendered figures, private audit reports, raw third-party inputs and detailed geographic datasets are intentionally excluded.
