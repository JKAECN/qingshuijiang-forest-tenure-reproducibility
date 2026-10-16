# Qingshuijiang Forest Tenure: Data and Reproducibility Materials

This repository contains the documentary corpus, derived datasets, analysis code, and reproducibility materials for the study:

**Land, trees, labour, and shares: Contractual allocation of forest-tenure relations in Qingshuijiang, southwest China, 1748–1950**

The study examines historical forestry contracts from the Qingshuijiang region of southwest China and investigates how land, standing trees, shares, planting/cultivation contributions, management clauses, transfer arrangements, and harvest-conditioned claims were contractually organised.

## 1. Repository overview

The repository documents the complete analytical workflow from historical source material to the statistical and institutional analyses reported in the manuscript.

The workflow is:

```text
Published documentary volumes
        ↓
Manual reading and transcription
        ↓
Word transcription files
        ↓
Structured Excel corpus
        ↓
Institutional coding and clause-level adjudication
        ↓
R analysis
        ↓
Tables, figures, sensitivity analyses, and audit outputs
```

The documentary texts were **manually read and entered by the author from the published volumes**. They were not generated through automated OCR and then treated as unverified machine-readable text.

## 2. Documentary sources

The working corpus was prepared from two published documentary collections.

### Qingshuijiang Documents

Zhang, Y., & Wang, Z. (Eds.). (2011).  
**清水江文书 第3辑 [Qingshuijiang Documents, Series 3]**, 10 vols.  
Guangxi Normal University Press, Guilin.

The present study uses material from **Series 3, Volume 1**.

ISBN: **978-7-5495-0444-2**

### Tianzhu Documents

Zhang, X. (Ed.). (2014).  
**天柱文书 第1辑 [Tianzhu Documents, Series 1]**, 22 vols.  
Jiangsu People's Publishing House, Nanjing.

The present study uses material from **Series 1, Volume 21**.

ISBN: **978-7-214-10645-2**

The Word files in this repository are research transcriptions prepared by the author through manual reading of the published documentary materials. The Excel corpus was subsequently constructed from these transcriptions for systematic coding and analysis.

## 3. Corpus construction

The project workbook contains **192 forestry-related records** identified from the two source volumes.

The sample-selection procedure was:

- 192 forestry-related records screened;
- 23 index/list-like records excluded;
- 14 records without a usable historical year excluded;
- 6 post-1950 copied-inscription records excluded;
- 149 records retained under the main-study rules;
- one exact continuation/duplicate removed after programmatic text comparison;
- **148 unique documentary texts** retained for quantitative analysis.

The duplicate check confirmed that Tianzhu item 76 reproduces the normalised body text of item 74.

The analytical corpus covers the period **1748–1950**.

## 4. Data files

The repository contains several levels of data.

### 4.1 Manual Word transcriptions

These files contain the author's manually entered versions of the relevant published documentary materials.

They preserve the documentary text used to construct the structured corpus.

### 4.2 Excel corpus

The Excel workbook converts the documentary material into structured research data.

It includes information such as:

- source collection;
- document/item number;
- document identifier;
- year;
- source location;
- document heading;
- transaction type;
- documentary text;
- inclusion/exclusion information;
- coding variables used for institutional analysis.

The Excel corpus is the main machine-readable input for the R analysis.

### 4.3 Institutional coding

The final analysis distinguishes two levels of evidence.

**Level 1: broad dictionary screening**

This stage retrieves documents containing potentially relevant combinations of textual markers.

The final screening identifies:

- **45 broad layered-tenure candidates**;
- **17 broad planting/cultivator candidates**;
- **10 broad temporal candidates**.

These screening counts are retrieval indicators and are **not interpreted as estimates of the historical prevalence of particular tenure institutions**.

**Level 2: clause-level relational adjudication**

Candidate documents are then read in context to determine whether the surrounding contractual clause establishes an identifiable institutional relationship.

The strict clause-supported sets contain:

- **7 strict layered-relation cases**;
- **8 labour-linked entitlement cases**;
- **2 harvest-triggered rights-change cases**.

The sets overlap:

- 6 texts belong to both the layered and labour-linked sets;
- both rights-change cases also belong to the layered set;
- the union of the three strict sets contains **9 unique documentary texts**.

These are reported as a conservative set of clause-supported cases. Their retrieval completeness is not assumed, and the counts should not be interpreted as population prevalence estimates.

## 5. Analytical approach

The coding framework separates categories that may have different institutional meanings.

### Resource objects

- land / mountain tracts;
- standing trees.

### Allocation forms

- fractional shares;
- proportional division.

### Participant and claim markers

- landholder;
- cultivator / planter;
- purchaser;
- inheritance-related claimant;
- collective holding.

### Activities and contractual clauses

- planting;
- management;
- maintenance;
- protection;
- harvesting / timber removal;
- property-management formulae.

### Transaction and institutional relations

- transfer / disposal;
- lease / tenancy;
- inheritance / partition;
- strict exclusion;
- reversion.

### Temporal conditions

The analysis distinguishes ordinary production time from a genuine rights-changing condition.

Expressions concerning growth, maturation, or harvesting are not automatically interpreted as changes in tenure.

A strict temporal case requires evidence that an ecological event changes a subsequent claim or residual interest.

Only two documents in the present corpus clearly satisfy this criterion through post-felling land reversion.

## 6. Main quantitative results

The final reproducible analysis uses **148 unique texts**.

```text
Unique analytical texts                         148

Broad layered candidates                         45
Broad labour candidates                          17
Broad temporal candidates                        10

Strict layered cases                              7
Strict labour-linked cases                        8
Strict harvest-triggered rights-change cases      2

Layered ∩ labour-linked                           6
Union of strict case sets                         9
```

Period comparisons of documentary relation-marker counts were conducted using Kruskal–Wallis tests.

### Full analytical corpus

```text
H = 4.41
df = 3
p = .221
```

### Sales only

```text
H = 4.31
df = 3
p = .230
```

These results do not demonstrate institutional stability or the absence of historical change. They indicate only that the surviving corpus does not show statistically detectable differences in the refined relation-marker count across the four periods under the present operationalisation.

## 7. Repository structure

```text
.
├── README.md
├── source_transcriptions/
├── data/
├── code/
├── supplement/
├── results/
├── figures/
└── sessionInfo.txt
```

Suggested contents:

```text
source_transcriptions/
    Qingshuijiang_Documents_Word/
    Tianzhu_Documents_Word/

data/
    Qingshuijiang_Forestry_Semantic_Corpus.xlsx

code/
    Qingshuijiang_FPE_Strict_Relational_Sensitivity_v2_3_submission.R

supplement/
    Table_S0_duplicate_verification.csv
    Table_S1_selection_flow.csv
    Table_S2_strict_adjudication.csv
    Table_S2a_nine_core_cases.csv
    Table_S3_broad_vs_strict_sensitivity.csv
    Table_S4_negative_control_audit.csv
    Table_S5_document_index.csv
    Table_S6_strict_set_overlap.csv

results/
    Table_01_transaction_types_unique_texts.csv
    Table_02_period_sensitivity.csv
    Table_03_sales_only_period_sensitivity.csv
    Test_01_period_KruskalWallis.txt

figures/
    Figure_01_resource_object_relation_heatmap.png
    Figure_02_entitlement_markers.png
    Figure_03_refined_relation_network.png
    Figure_04_transaction_relation_heatmap.png
    Figure_05_relation_marker_count_by_period.png
```

Actual filenames may differ slightly from this schematic structure.

## 8. Reproducing the analysis

The analysis was conducted in R.

To reproduce the main results:

1. Download or clone this repository.
2. Open the final R script in RStudio.
3. Run the script.
4. When prompted, select the Excel corpus file.
5. The script will create the analytical outputs, tables, figures, audit files, and session information.

The final script used for the submitted analysis is:

```text
Qingshuijiang_FPE_Strict_Relational_Sensitivity_v2_3_submission.R
```

The script includes the coding dictionary and the procedures used for sample selection, duplicate verification, transaction classification, documentary screening, strict relational coding, negative-screen audit, period comparison, sales-only sensitivity analysis, and figure generation.

## 9. Evidence and interpretation

The repository distinguishes carefully between **textual detection** and **institutional interpretation**.

The presence of two terms in the same document is not sufficient to establish a divided property right.

For example:

- land and standing trees appearing in the same deed indicate documentary co-mention;
- a cultivator term does not automatically establish a labour-generated entitlement;
- protection language does not automatically imply exclusion rights;
- management formulae are not automatically treated as management rights;
- a maturation clause does not constitute a rights transition unless the document identifies a change in claims.

Institutional interpretation is therefore based on the surrounding contractual clause.

## 10. Core adjudicated cases

The main institutional argument is supported by nine unique documentary texts.

The supplementary adjudication tables identify source volume, document/item number, year, source page, documentary excerpt, institutional interpretation, and strict classification.

The complete classification logic is documented in:

```text
Table_S2_strict_adjudication.csv
Table_S2a_nine_core_cases.csv
Table_S6_strict_set_overlap.csv
```

## 11. Data provenance and copyright

The historical documentary sources are published materials.

The Word transcriptions and structured corpus in this repository were prepared by the author for research purposes through manual reading and data entry from the published volumes.

Copyright in the original published documentary materials remains with the relevant authors, editors, publishers, and other rights holders.

The presence of research transcriptions in this repository should not be interpreted as transferring copyright in the underlying published sources.

Users of the repository should cite the original documentary collections when referring to historical source material.

If reuse of the full documentary transcriptions is intended beyond scholarly verification and reproducibility, users should ensure that such reuse is consistent with applicable copyright and publisher requirements.

## 12. Citation

If you use the analysis code, derived data, or coding framework, please cite the associated article and this repository.

Citation details for the article will be added after publication.

Suggested repository citation:

> Zhang, J. *Qingshuijiang Forest Tenure: Data and Reproducibility Materials*. GitHub repository.

## 13. Contact

For questions about the corpus, coding, or reproducibility materials, please contact the author through the institutional contact information provided in the associated publication.
