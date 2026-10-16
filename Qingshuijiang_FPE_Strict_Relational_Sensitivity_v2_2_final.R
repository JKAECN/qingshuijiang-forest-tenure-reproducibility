# ==============================================================================
# Qingshuijiang Forest Tenure Analysis v2
# Target journal: Forest Policy and Economics
#
# Purpose of v2:
#   1) Separate documentary screening markers from strict relational evidence.
#   2) Separate resource objects, allocation forms, activities, clauses and events.
#   3) Remove the exact Tianzhu duplicate/continuation from quantitative analysis.
#   4) Treat post-harvest reversion as a rare, clause-level existence result.
#   5) Add a negative-control audit and sales-only diachronic sensitivity.
#
# IMPORTANT:
# - Strict adjudication below is based on supplied collection-level transcriptions.
# - The analytical corpus was manually transcribed from the published document collections.
# - Screening rules are retrieval devices; manual clause-level adjudication remains the basis for strict cases.
# ==============================================================================

# ------------------------------ 0. SETTINGS ------------------------------------
DATA_FILE <- file.choose()
OUT_DIR <- file.path(dirname(DATA_FILE), "Qingshuijiang_FPE_strict_v2")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

MASTER_SEED <- 20261010
set.seed(MASTER_SEED)

required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "ggplot2", "igraph", "ggraph", "openxlsx", "scales"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_pkgs) > 0) {
  stop("Install required packages first: ", paste(missing_pkgs, collapse = ", "))
}

# Do not attach MASS. All select/mutate/filter calls use explicit namespaces.
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(ggplot2)
  library(igraph)
  library(ggraph)
  library(openxlsx)
  library(scales)
})

write_csv_utf8 <- function(x, name) {
  write.csv(
    x,
    file.path(OUT_DIR, name),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

save_plot <- function(p, name, width = 9, height = 6) {
  ggplot2::ggsave(
    filename = file.path(OUT_DIR, name),
    plot = p,
    width = width,
    height = height,
    dpi = 400,
    bg = "white"
  )
}

clean_text <- function(x) {
  x <- tidyr::replace_na(as.character(x), "")
  stringr::str_replace_all(x, "[\\r\\n\\t ]+", "")
}

has_pat <- function(text, pattern) {
  stringr::str_detect(
    clean_text(text),
    stringr::regex(pattern, ignore_case = TRUE)
  )
}

read_sheet_at_header <- function(path, sheet, key_cols, probe_n = 12) {
  probe <- suppressMessages(
    readxl::read_excel(
      path, sheet = sheet, col_names = FALSE, n_max = probe_n
    )
  )

  header_hit <- vapply(
    seq_len(nrow(probe)),
    function(i) {
      vals <- trimws(as.character(unlist(probe[i, ], use.names = FALSE)))
      any(vals %in% key_cols, na.rm = TRUE)
    },
    logical(1)
  )

  hit <- which(header_hit)
  if (length(hit) == 0) {
    stop("Could not locate header in sheet: ", sheet)
  }

  dat <- suppressMessages(
    readxl::read_excel(path, sheet = sheet, skip = hit[1] - 1)
  )

  names(dat) <- trimws(names(dat))
  dat %>%
    dplyr::filter(!dplyr::if_all(dplyr::everything(), ~ is.na(.x)))
}

# ------------------------------ 1. IMPORT / SAMPLE FLOW ------------------------
docs <- read_sheet_at_header(
  DATA_FILE,
  "Forest_Documents",
  c("Source", "Item_No", "Include_MainStudy")
)

screened_n <- nrow(docs)

# Reconstruct the documented exclusion flow.
index_excluded_n <- sum(
  docs$Include_MainStudy != "Yes" & docs$Index_Like == "Yes",
  na.rm = TRUE
)

nonindex_excluded <- docs %>%
  dplyr::filter(
    Include_MainStudy != "Yes",
    Index_Like != "Yes" | is.na(Index_Like)
  )

post1950_excluded_n <- sum(
  !is.na(suppressWarnings(as.numeric(nonindex_excluded$Year))) &
    suppressWarnings(as.numeric(nonindex_excluded$Year)) > 1950,
  na.rm = TRUE
)

undated_excluded_n <- nrow(nonindex_excluded) - post1950_excluded_n


# Transaction classification is created before duplicate handling so that the
# full 149-record document index and the 148-text analytical corpus use exactly
# the same classification logic.
classify_transaction <- function(heading, text) {
  h <- clean_text(heading)
  t <- clean_text(text)

  dplyr::case_when(
    stringr::str_detect(h, "分关|分關|分合同|分清单|分清單|分拨|分撥") ~
      "PARTITION_INHERITANCE",
    stringr::str_detect(h, "换|換") ~ "EXCHANGE",
    stringr::str_detect(h, "租|佃") ~ "LEASE_TENANCY",
    stringr::str_detect(h, "典") &
      !stringr::str_detect(h, "卖|賣") ~ "DIAN_MORTGAGE",
    stringr::str_detect(h, "断卖|斷賣|卖|賣") ~ "SALE",
    stringr::str_detect(h, "禁|戒约|戒約|蓄") ~ "PROTECTION_REGULATION",
    stringr::str_detect(h, "合同|合約|合约|约|約") ~ "AGREEMENT",
    stringr::str_detect(t, "分关|分關|均分|分业|分業") ~
      "PARTITION_INHERITANCE",
    TRUE ~ "OTHER"
  )
}

docs_main_149 <- docs %>%
  dplyr::filter(Include_MainStudy == "Yes") %>%
  dplyr::mutate(
    Year = suppressWarnings(as.numeric(Year)),
    Item_No = suppressWarnings(as.numeric(Item_No)),
    Doc_Key = paste(Source, Item_No, sep = "__"),
    Full_Text = paste0(clean_text(Heading), "；", clean_text(Body_Text)),
    Transaction_Type = classify_transaction(Heading, Full_Text),
    Period_FPE = cut(
      Year,
      breaks = c(1747, 1799, 1849, 1899, 1950),
      labels = c("1748–1799", "1800–1849", "1850–1899", "1900–1950"),
      include.lowest = TRUE
    )
  )

# Exact duplicate/continuation check.
# Do not delete item 76 merely because of its identifier: verify the body text.
duplicate_source_key <- "Tianzhu_I__74"
duplicate_key <- "Tianzhu_I__76"

dup_74 <- docs_main_149 %>%
  dplyr::filter(Doc_Key == duplicate_source_key) %>%
  dplyr::pull(Body_Text) %>%
  clean_text()

dup_76 <- docs_main_149 %>%
  dplyr::filter(Doc_Key == duplicate_key) %>%
  dplyr::pull(Body_Text) %>%
  clean_text()

if (length(dup_74) != 1 || length(dup_76) != 1) {
  stop("Could not uniquely locate Tianzhu items 74 and 76 for duplicate verification.")
}

duplicate_verified <- identical(dup_74, dup_76)

if (!duplicate_verified) {
  stop("Tianzhu item 76 is not text-identical to item 74; duplicate removal aborted.")
}

duplicate_verification <- tibble::tibble(
  Retained_Doc_Key = duplicate_source_key,
  Removed_Doc_Key = duplicate_key,
  Normalized_Body_Text_Identical = duplicate_verified,
  Decision = "Item 76 removed from quantitative counts as an exact continuation/duplicate."
)

write_csv_utf8(
  duplicate_verification,
  "Table_S0_duplicate_verification.csv"
)

docs_main <- docs_main_149 %>%
  dplyr::filter(Doc_Key != duplicate_key)

stopifnot(nrow(docs_main_149) == 149)
stopifnot(nrow(docs_main) == 148)

selection_flow <- tibble::tribble(
  ~Stage, ~N, ~Rule_or_note,
  "Records screened from two supplied collections", screened_n,
  "All forestry-related records in project workbook",
  "Excluded: index/list-like records", index_excluded_n,
  "Excluded first; Index_Like=Yes",
  "Excluded: undated/unresolved year", undated_excluded_n,
  "Among remaining non-index records",
  "Excluded: post-1950 transcription records", post1950_excluded_n,
  "1984 copied inscription records",
  "Main-study records before duplicate handling", nrow(docs_main_149),
  "Include_MainStudy=Yes",
  "Exact duplicate/continuation removed", 1,
  "Tianzhu item 76 duplicates item 74 body text",
  "Unique documentary texts in quantitative analysis", nrow(docs_main),
  "136 Qingshuijiang + 12 Tianzhu"
)

write_csv_utf8(selection_flow, "Table_S1_selection_flow.csv")

# ------------------------------ 2. TRANSACTION CLASSIFICATION ------------------
# Transaction_Type was classified before duplicate handling so the document
# index and analytical corpus share the same classification source.

# ------------------------------ 3. REFINED CODING DICTIONARY -------------------
# Families are intentionally separated:
#   OBJECT   = resource object
#   ALLOC    = allocation form
#   ACTIVITY = productive action
#   CLAUSE   = contractual clause/formula
#   EVENT    = transaction event
#   ROLE     = participant-role marker
#   TIME     = ecological/time cue
#
# A positive lexical marker is not automatically interpreted as a legal right.

P <- list(
  # Resource objects
  OBJ_LAND =
    "山场|山場|山土|地土|山地|土地|杉山|柴山|公山|共山|祖山|禁山|山林",
  OBJ_TREES =
    "杉木|嫩杉|子杉|老杉|杉苗|杉秧|成林|成材|栗木|油树|油樹",

  # Allocation form
  ALLOC_SHARE = "股|股份|分股|占股",
  REL_SHARE_ALLOCATION =
    "分为.{0,6}股|分為.{0,6}股|分作.{0,6}股|占股|均分|分股|股分|股份|[一二三四五六七八九十百千壹貳叁肆伍陸柒捌玖拾佰仟0-9]+股",

  # Productive activities
  ACT_HARVEST =
    "砍伐|砍尽|砍盡|砍木|伐木|裁杉|采伐|採伐|下河|出山",
  REL_ACCESS_USE =
    "耕管|耕作|耕当|耕當|使用|开山|開山",

  # Contractual clauses / formulae
  CLAUSE_MANAGEMENT_ACTION =
    "修理|耕管|管理|照管|蓄养|蓄養|看守|培育",
  FORMULA_GUANYE = "管业|管業",
  CLAUSE_PROTECTION = "蓄禁|禁山",
  REL_EXCLUSION_STRICT =
    "不许外人|不許外人|不得外人|不得越分|外人争|外人爭|不得侵|不许侵|不許侵",

  # Transaction events
  EVENT_TRANSFER_DISPOSAL =
    "断卖|斷賣|出卖|出賣|卖|賣|承买|承買|买主|買主|典|换|換",
  EVENT_LEASE_TENANCY = "租|佃",
  REL_INHERIT_PARTITION =
    "分关|分關|分合同|分清单|分清單|先人遗下|先人遺下|祖业|祖業|遗业|遺業|祖山|继承|繼承",
  REL_REVERSION =
    "仍归|仍歸|归还|歸還|归原主|歸原主|仍付原主|砍.{0,12}(尽|盡).{0,16}(归|歸)|地任.{0,8}原主|山土仍归|山土仍歸",

  # Participant / claim-basis markers
  ROLE_LANDHOLDER = "地主|土主",
  ROLE_CULTIVATOR = "栽手|栽主|裁手|裁主",
  SHARE_LAND_BASED = "地股|土股|山股",
  SHARE_PLANTING_BASED = "栽手股|栽股|裁手股|裁股",
  MARK_PURCHASE = "承买|承買|买主|買主|买得|買得|承置",
  MARK_INHERITANCE =
    "先人遗下|先人遺下|祖业|祖業|遗业|遺業|祖山|分关|分關",
  MARK_COLLECTIVE =
    "公山|共山|公共|全寨|三姓|众|眾|合众|合眾",
  MARK_PRIOR_SHARE =
    "占股|分股|大股|小股|一股|壹股|二股|贰股|貳股|三股|叁股|伍股|五股|十股",

  # Ecological time
  TIME_GROWTH = "长大|長大|成林|成材|嫩杉|子杉|老杉",
  TIME_HARVEST = "砍伐|砍尽|砍盡|砍木|伐木|裁杉|采伐|採伐"
)

coded <- docs_main
for (nm in names(P)) {
  coded[[nm]] <- as.integer(has_pat(coded$Full_Text, P[[nm]]))
}

relation_cols <- c(
  "REL_ACCESS_USE",
  "ACT_HARVEST",
  "CLAUSE_MANAGEMENT_ACTION",
  "REL_EXCLUSION_STRICT",
  "EVENT_TRANSFER_DISPOSAL",
  "EVENT_LEASE_TENANCY",
  "REL_SHARE_ALLOCATION",
  "REL_INHERIT_PARTITION",
  "REL_REVERSION"
)

coded <- coded %>%
  dplyr::mutate(
    Relation_Marker_N = rowSums(
      dplyr::across(tidyselect::all_of(relation_cols))
    )
  )

# ------------------------------ 4. BROAD SCREENING SENSITIVITY -----------------
# These flags are retained ONLY as candidate-retrieval diagnostics.

P_broad <- list(
  RES_LAND =
    "山场|山場|山土|地土|山地|土地|杉山|柴山|公山|共山|祖山|禁山|山林|地股|土股|山股",
  RES_TREES =
    "杉木|嫩杉|子杉|老杉|杉苗|杉秧|成林|成材|栗木|油树|油樹",
  RES_SHARE = "股|股份|分股|占股",
  B_LANDHOLDING = "地主|土主|地股|土股|山股",
  B_PLANTING_LABOUR = "栽手|栽主|栽股|栽手股|裁手|裁主|裁股|裁手股",
  B_PURCHASE = "承买|承買|买主|買主|买得|買得|承置",
  B_INHERITANCE =
    "先人遗下|先人遺下|祖业|祖業|遗业|遺業|祖山|分关|分關",
  B_COLLECTIVE = "公山|共山|公共|全寨|三姓|众|眾|合众|合眾",
  B_PRIOR_SHARE =
    "占股|分股|大股|小股|一股|壹股|二股|贰股|貳股|三股|叁股|伍股|五股|十股",
  R_ACCESS_USE = "耕管|耕作|耕当|耕當|使用|出山|开山|開山",
  R_WITHDRAWAL = "砍伐|砍尽|砍盡|砍木|伐木|裁杉|采伐|採伐|下河",
  R_MANAGEMENT = "修理|管业|管業|耕管|管理|照管|蓄养|蓄養|看守|培育",
  R_EXCLUSION =
    "蓄禁|禁山|不许外人|不許外人|不得外人|不得越分|外人争|外人爭|不得侵|不许侵|不許侵",
  R_ALIENATION =
    "断卖|斷賣|出卖|出賣|卖|賣|承买|承買|典|租|佃|换|換",
  R_SHARE_BENEFIT = P$REL_SHARE_ALLOCATION,
  R_INHERIT_PARTITION = P$REL_INHERIT_PARTITION,
  R_REVERSION = P$REL_REVERSION,
  T_GROWTH = P$TIME_GROWTH,
  T_POST_HARVEST = "砍.{0,8}(尽|盡)|伐.{0,8}(尽|盡)|砍完|伐完",
  A_LANDHOLDER = "地主|土主",
  A_CULTIVATOR = "栽手|栽主|裁手|裁主",
  A_BUYER = "买主|買主|承买|承買|承置",
  A_SELLER = "立.{0,6}(卖|賣)|卖.{0,4}人|賣.{0,4}人|出卖|出賣",
  A_KIN_GROUP = "兄弟|母子|父子|房族|亲房|親房|族戚|族兄|先人",
  A_COLLECTIVE = "三姓|全寨|众|眾|公山|共山|公共"
)

for (nm in names(P_broad)) {
  coded[[paste0("BROAD_", nm)]] <-
    as.integer(has_pat(coded$Full_Text, P_broad[[nm]]))
}

resource_broad <- paste0("BROAD_", c("RES_LAND","RES_TREES","RES_SHARE"))
basis_broad <- paste0(
  "BROAD_",
  c(
    "B_LANDHOLDING","B_PLANTING_LABOUR","B_PURCHASE",
    "B_INHERITANCE","B_COLLECTIVE","B_PRIOR_SHARE"
  )
)
rights_broad <- paste0(
  "BROAD_",
  c(
    "R_ACCESS_USE","R_WITHDRAWAL","R_MANAGEMENT","R_EXCLUSION",
    "R_ALIENATION","R_SHARE_BENEFIT","R_INHERIT_PARTITION","R_REVERSION"
  )
)
actor_broad <- paste0(
  "BROAD_",
  c("A_LANDHOLDER","A_CULTIVATOR","A_BUYER","A_SELLER","A_KIN_GROUP","A_COLLECTIVE")
)

coded <- coded %>%
  dplyr::mutate(
    Broad_Resource_N = rowSums(
      dplyr::across(tidyselect::all_of(resource_broad))
    ),
    Broad_Basis_N = rowSums(
      dplyr::across(tidyselect::all_of(basis_broad))
    ),
    Broad_Rights_N = rowSums(
      dplyr::across(tidyselect::all_of(rights_broad))
    ),
    Broad_Actor_N = rowSums(
      dplyr::across(tidyselect::all_of(actor_broad))
    ),

    Broad_Land_Tree_Candidate =
      as.integer(BROAD_RES_LAND == 1 & BROAD_RES_TREES == 1),

    Broad_Labour_Candidate =
      BROAD_B_PLANTING_LABOUR,

    Broad_Temporal_Candidate = as.integer(
      (BROAD_T_GROWTH == 1 | BROAD_T_POST_HARVEST == 1) &
        (
          BROAD_R_MANAGEMENT == 1 |
            BROAD_R_WITHDRAWAL == 1 |
            BROAD_R_REVERSION == 1
        )
    ),

    Broad_Layered_Candidate = as.integer(
      (
        Broad_Resource_N >= 2 &
          (Broad_Basis_N >= 2 | Broad_Actor_N >= 2) &
          Broad_Rights_N >= 2
      ) |
        (
          Broad_Basis_N >= 2 &
            Broad_Rights_N >= 3
        ) |
        (
          BROAD_R_REVERSION == 1 &
            BROAD_R_WITHDRAWAL == 1
        )
    )
  )

# ------------------------------ 5. STRICT ADJUDICATION --------------------------
# Transcription-supported lower-bound cases.
# Do not interpret these as exhaustive historical prevalence.

strict_layered_keys <- c(
  "Qingshuijiang_III__36",
  "Qingshuijiang_III__50",
  "Qingshuijiang_III__112",
  "Qingshuijiang_III__221",
  "Qingshuijiang_III__229",
  "Qingshuijiang_III__253",
  "Qingshuijiang_III__412"
)

strict_labour_keys <- c(
  "Qingshuijiang_III__36",
  "Qingshuijiang_III__112",
  "Qingshuijiang_III__158",
  "Qingshuijiang_III__204",
  "Qingshuijiang_III__221",
  "Qingshuijiang_III__229",
  "Qingshuijiang_III__253",
  "Qingshuijiang_III__412"
)

strict_temporal_keys <- c(
  "Qingshuijiang_III__50",
  "Qingshuijiang_III__412"
)

coded <- coded %>%
  dplyr::mutate(
    Strict_Layered_Relation =
      as.integer(Doc_Key %in% strict_layered_keys),
    Strict_Labour_Linked_Entitlement =
      as.integer(Doc_Key %in% strict_labour_keys),
    Strict_Temporal_Rights_Change =
      as.integer(Doc_Key %in% strict_temporal_keys),
    Terminological_Evidence_Only =
      as.integer(Doc_Key == "Qingshuijiang_III__194"),
    Reversion_Candidate_Unconfirmed =
      as.integer(Doc_Key == "Tianzhu_I__40")
  )

strict_adjudication <- coded %>%
  dplyr::filter(
    Strict_Layered_Relation == 1 |
      Strict_Labour_Linked_Entitlement == 1 |
      Strict_Temporal_Rights_Change == 1 |
      Terminological_Evidence_Only == 1 |
      Reversion_Candidate_Unconfirmed == 1
  ) %>%
  dplyr::select(
    Source, Item_No, Doc_ID, Year, Heading, Page, Text_Reliability,
    Strict_Layered_Relation,
    Strict_Labour_Linked_Entitlement,
    Strict_Temporal_Rights_Change,
    Terminological_Evidence_Only,
    Reversion_Candidate_Unconfirmed,
    Body_Text
  ) %>%
  dplyr::mutate(
    Verification_Level =
      "Manually re-read against the relevant published source text; strict status requires an explicit clause-level relation."
  )

write_csv_utf8(
  strict_adjudication,
  "Table_S2_strict_adjudication.csv"
)

sensitivity_table <- tibble::tribble(
  ~Indicator, ~Positive_N, ~Denominator, ~Interpretation,
  "Land-tree co-mention / broad layering cue",
    sum(coded$Broad_Land_Tree_Candidate), nrow(coded),
    "Object co-mention; not a tenure relation by itself",
  "Broad layered-tenure candidate",
    sum(coded$Broad_Layered_Candidate), nrow(coded),
    "Dictionary screening definition",
  "Strict relational layered cases",
    sum(coded$Strict_Layered_Relation), nrow(coded),
    "Clause-supported relation; retrieval completeness not established",
  "Broad planting/cultivator marker",
    sum(coded$Broad_Labour_Candidate), nrow(coded),
    "Role/term marker only",
  "Strict labour-linked entitlement cases",
    sum(coded$Strict_Labour_Linked_Entitlement), nrow(coded),
    "Clause-supported relation; retrieval completeness not established",
  "Broad temporal candidate",
    sum(coded$Broad_Temporal_Candidate), nrow(coded),
    "Production/time screening cue",
  "Strict harvest-triggered rights transition",
    sum(coded$Strict_Temporal_Rights_Change), nrow(coded),
    "1821 and 1828 clause-supported cases only",
  "Automatic reversion candidate",
    sum(coded$BROAD_R_REVERSION), nrow(coded),
    "Includes an uncertain 1917 documentary candidate",
  "Strict post-harvest reversion",
    sum(coded$Strict_Temporal_Rights_Change), nrow(coded),
    "No inferential significance test because n=2"
) %>%
  dplyr::mutate(
    Percent = 100 * Positive_N / Denominator
  )

write_csv_utf8(
  sensitivity_table,
  "Table_S3_broad_vs_strict_sensitivity.csv"
)

# ------------------------------ 6. NEGATIVE-CONTROL AUDIT -----------------------
negative_pool <- coded %>%
  dplyr::filter(
    Broad_Layered_Candidate == 0,
    Broad_Labour_Candidate == 0,
    Broad_Temporal_Candidate == 0
  )

set.seed(MASTER_SEED)
negative_audit <- negative_pool %>%
  dplyr::slice_sample(n = min(20, nrow(negative_pool))) %>%
  dplyr::mutate(
    Potential_Dictionary_Missed_Cue = dplyr::case_when(
      Doc_Key == "Qingshuijiang_III__212" ~
        "Possible maturity/share cue: growth-to-stand + three-share wording; not promoted because the relation is not explicit enough in context.",
      Doc_Key == "Qingshuijiang_III__228" ~
        "Possible landholder/cultivator layering: 地主 + difficult cultivator-related wording + 6/4 shares + four-year maturation condition; not promoted because the relation is not explicit enough in context.",
      TRUE ~ "No obvious strict relation identified in the surrounding published text."
    )
  ) %>%
  dplyr::select(
    Source, Item_No, Year, Heading, Page, Text_Reliability,
    Potential_Dictionary_Missed_Cue, Body_Text
  )

write_csv_utf8(
  negative_audit,
  "Table_S4_negative_control_audit.csv"
)

# ------------------------------ 7. TRANSACTION / PERIOD SUMMARIES ---------------
transaction_summary <- coded %>%
  dplyr::count(Transaction_Type, name = "Document_N") %>%
  dplyr::mutate(
    Percent = 100 * Document_N / sum(Document_N)
  ) %>%
  dplyr::arrange(dplyr::desc(Document_N))

write_csv_utf8(
  transaction_summary,
  "Table_01_transaction_types_unique_texts.csv"
)

period_summary <- coded %>%
  dplyr::group_by(Period_FPE) %>%
  dplyr::summarise(
    N = dplyr::n(),
    Mean_Relation_Marker_N = mean(Relation_Marker_N),
    Median_Relation_Marker_N = median(Relation_Marker_N),
    Broad_Layered_N = sum(Broad_Layered_Candidate),
    Broad_Layered_Pct = 100 * mean(Broad_Layered_Candidate),
    Broad_Labour_N = sum(Broad_Labour_Candidate),
    Broad_Temporal_N = sum(Broad_Temporal_Candidate),
    .groups = "drop"
  )

write_csv_utf8(
  period_summary,
  "Table_02_period_sensitivity.csv"
)

kw_all <- stats::kruskal.test(
  Relation_Marker_N ~ Period_FPE,
  data = coded
)

sales_only <- coded %>%
  dplyr::filter(Transaction_Type == "SALE")

sales_period_summary <- sales_only %>%
  dplyr::group_by(Period_FPE) %>%
  dplyr::summarise(
    Sale_N = dplyr::n(),
    Mean_Relation_Marker_N = mean(Relation_Marker_N),
    Broad_Layered_N = sum(Broad_Layered_Candidate),
    Broad_Layered_Pct = 100 * mean(Broad_Layered_Candidate),
    .groups = "drop"
  )

write_csv_utf8(
  sales_period_summary,
  "Table_03_sales_only_period_sensitivity.csv"
)

kw_sales <- stats::kruskal.test(
  Relation_Marker_N ~ Period_FPE,
  data = sales_only
)

capture.output(
  kw_all,
  kw_sales,
  file = file.path(OUT_DIR, "Test_01_period_KruskalWallis.txt")
)

# ------------------------------ 8. FIGURE 1: OBJECT × RELATION ------------------
object_cols <- c("OBJ_LAND", "OBJ_TREES")
object_labels <- c(
  OBJ_LAND = "Land / mountain",
  OBJ_TREES = "Standing trees"
)

fig1_rel_cols <- c(
  "REL_ACCESS_USE",
  "ACT_HARVEST",
  "CLAUSE_MANAGEMENT_ACTION",
  "REL_EXCLUSION_STRICT",
  "EVENT_TRANSFER_DISPOSAL",
  "REL_SHARE_ALLOCATION",
  "REL_INHERIT_PARTITION"
)

fig1_rel_labels <- c(
  REL_ACCESS_USE = "Access/use",
  ACT_HARVEST = "Harvest/removal",
  CLAUSE_MANAGEMENT_ACTION = "Management action",
  REL_EXCLUSION_STRICT = "Strict exclusion",
  EVENT_TRANSFER_DISPOSAL = "Transfer/disposal",
  REL_SHARE_ALLOCATION = "Share allocation",
  REL_INHERIT_PARTITION = "Inheritance/partition"
)

figure1_data <- tidyr::expand_grid(
  Object = object_cols,
  Relation = fig1_rel_cols
) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    Denominator = sum(coded[[Object]] == 1),
    Cooccurrence_N = sum(
      coded[[Object]] == 1 &
        coded[[Relation]] == 1
    ),
    Percent_within_object =
      100 * Cooccurrence_N / Denominator,
    Cell_Label = sprintf(
      "%d/%d\n(%.0f%%)",
      Cooccurrence_N,
      Denominator,
      Percent_within_object
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Object_Label = object_labels[Object],
    Relation_Label = fig1_rel_labels[Relation]
  )

p1 <- ggplot2::ggplot(
  figure1_data,
  ggplot2::aes(
    x = Relation_Label,
    y = Object_Label,
    fill = Percent_within_object
  )
) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(label = Cell_Label),
    size = 3
  ) +
  ggplot2::labs(
    title = "Resource objects × documentary relation markers",
    x = NULL,
    y = NULL,
    fill = "Within-object\n%"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 35,
      hjust = 1
    )
  )

save_plot(
  p1,
  "Figure_01_resource_object_relation_heatmap.png",
  10.5,
  4.6
)

# ------------------------------ 9. FIGURE 2: CLAIM MARKERS ----------------------
claim_marker_cols <- c(
  "MARK_PURCHASE",
  "MARK_PRIOR_SHARE",
  "MARK_COLLECTIVE",
  "ROLE_LANDHOLDER",
  "ROLE_CULTIVATOR",
  "MARK_INHERITANCE"
)

claim_marker_labels <- c(
  MARK_PURCHASE = "Purchase/acquisition",
  MARK_PRIOR_SHARE = "Prior share",
  MARK_COLLECTIVE = "Collective holding",
  ROLE_LANDHOLDER = "Landholder role",
  ROLE_CULTIVATOR = "Cultivator/planting role",
  MARK_INHERITANCE = "Inheritance/ancestral"
)

figure2_data <- purrr::map_dfr(
  claim_marker_cols,
  function(v) {
    tibble::tibble(
      Variable = v,
      Label = claim_marker_labels[[v]],
      Document_N = sum(coded[[v]] == 1),
      Percent = 100 * mean(coded[[v]] == 1)
    )
  }
)

p2 <- figure2_data %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(Label, Document_N),
      y = Document_N
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%d (%.1f%%)", Document_N, Percent)
    ),
    hjust = -0.05,
    size = 3.2
  ) +
  ggplot2::labs(
    title = "Documentary markers relevant to bases of forest claims",
    x = NULL,
    y = "Documents with explicit marker"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::expand_limits(
    y = max(figure2_data$Document_N) * 1.15
  )

save_plot(
  p2,
  "Figure_02_entitlement_markers.png",
  8.8,
  5.6
)

# ------------------------------ 10. FIGURE 3: NETWORK ---------------------------
network_cols <- c(
  "REL_ACCESS_USE",
  "ACT_HARVEST",
  "CLAUSE_MANAGEMENT_ACTION",
  "FORMULA_GUANYE",
  "CLAUSE_PROTECTION",
  "REL_EXCLUSION_STRICT",
  "EVENT_TRANSFER_DISPOSAL",
  "EVENT_LEASE_TENANCY",
  "REL_SHARE_ALLOCATION",
  "REL_INHERIT_PARTITION",
  "REL_REVERSION"
)

network_labels <- c(
  REL_ACCESS_USE = "Access/use",
  ACT_HARVEST = "Harvest/removal",
  CLAUSE_MANAGEMENT_ACTION = "Management action",
  FORMULA_GUANYE = "Property-management formula",
  CLAUSE_PROTECTION = "Protection clause",
  REL_EXCLUSION_STRICT = "Strict exclusion",
  EVENT_TRANSFER_DISPOSAL = "Transfer/disposal",
  EVENT_LEASE_TENANCY = "Lease/tenancy",
  REL_SHARE_ALLOCATION = "Share allocation",
  REL_INHERIT_PARTITION = "Inheritance/partition",
  REL_REVERSION = "Reversion"
)

pairs <- utils::combn(
  network_cols,
  2,
  simplify = FALSE
)

network_edges <- purrr::map_dfr(
  pairs,
  function(z) {
    tibble::tibble(
      from = z[1],
      to = z[2],
      weight = sum(
        coded[[z[1]]] == 1 &
          coded[[z[2]]] == 1
      )
    )
  }
) %>%
  dplyr::filter(weight >= 4)

retained_network_nodes <- sort(unique(c(network_edges$from, network_edges$to)))

network_vertices <- tibble::tibble(
  name = retained_network_nodes,
  label = unname(network_labels[retained_network_nodes]),
  Document_N = vapply(
    retained_network_nodes,
    function(v) sum(coded[[v]] == 1),
    numeric(1)
  )
)

g <- igraph::graph_from_data_frame(
  network_edges,
  directed = FALSE,
  vertices = network_vertices
)

set.seed(MASTER_SEED)
p3 <- ggraph::ggraph(g, layout = "fr") +
  ggraph::geom_edge_link(
    ggplot2::aes(width = weight),
    alpha = 0.35
  ) +
  ggraph::geom_node_point(
    ggplot2::aes(size = Document_N)
  ) +
  ggraph::geom_node_text(
    ggplot2::aes(label = label),
    repel = TRUE,
    size = 3.2
  ) +
  ggraph::scale_edge_width(
    range = c(0.4, 2.3)
  ) +
  ggplot2::labs(
    title = "Co-occurring documentary relation markers",
    subtitle = "Edges require co-occurrence in at least four unique texts",
    size = "Documents with marker",
    edge_width = "Co-occurring texts"
  ) +
  ggplot2::guides(
    size = ggplot2::guide_legend(order = 1),
    edge_width = ggplot2::guide_legend(order = 2)
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "right"
  )

save_plot(
  p3,
  "Figure_03_refined_relation_network.png",
  10,
  7
)

# ------------------------------ 11. FIGURE 4: TRANSACTION × RELATION ------------
fig4_cols <- c(
  "EVENT_TRANSFER_DISPOSAL",
  "EVENT_LEASE_TENANCY",
  "CLAUSE_MANAGEMENT_ACTION",
  "FORMULA_GUANYE",
  "CLAUSE_PROTECTION",
  "REL_EXCLUSION_STRICT",
  "ACT_HARVEST",
  "REL_SHARE_ALLOCATION",
  "REL_INHERIT_PARTITION"
)

fig4_labels <- network_labels[fig4_cols]

figure4_data <- coded %>%
  dplyr::select(
    Transaction_Type,
    tidyselect::all_of(fig4_cols)
  ) %>%
  tidyr::pivot_longer(
    cols = -Transaction_Type,
    names_to = "Marker",
    values_to = "Present"
  ) %>%
  dplyr::group_by(
    Transaction_Type,
    Marker
  ) %>%
  dplyr::summarise(
    N_transaction = dplyr::n(),
    Present_N = sum(Present),
    Present_Pct = 100 * mean(Present),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Marker_Label = fig4_labels[Marker],
    Transaction_Label = paste0(
      stringr::str_to_title(
        stringr::str_replace_all(
          Transaction_Type,
          "_",
          " "
        )
      ),
      " (n=",
      N_transaction,
      ")"
    )
  )

p4 <- ggplot2::ggplot(
  figure4_data,
  ggplot2::aes(
    x = Marker_Label,
    y = Transaction_Label,
    fill = Present_Pct
  )
) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.0f%%", Present_Pct)
    ),
    size = 3
  ) +
  ggplot2::labs(
    title = "Documentary relation markers by transaction type",
    x = NULL,
    y = NULL,
    fill = "%"
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 35,
      hjust = 1
    )
  )

save_plot(
  p4,
  "Figure_04_transaction_relation_heatmap.png",
  11,
  6.2
)

# ------------------------------ 12. FIGURE 5: PERIOD DOT PLOT -------------------
profile_cols <- c(
  "Broad_Layered_Candidate",
  "Broad_Labour_Candidate",
  "REL_SHARE_ALLOCATION",
  "Broad_Temporal_Candidate"
)

profile_labels <- c(
  Broad_Layered_Candidate = "Broad layered candidate",
  Broad_Labour_Candidate = "Planting/cultivator marker",
  REL_SHARE_ALLOCATION = "Share-allocation marker",
  Broad_Temporal_Candidate = "Broad temporal candidate"
)

period_n <- coded %>%
  dplyr::count(Period_FPE, name = "N_period")

figure5_data <- coded %>%
  dplyr::select(
    Period_FPE,
    tidyselect::all_of(profile_cols)
  ) %>%
  tidyr::pivot_longer(
    cols = -Period_FPE,
    names_to = "Profile",
    values_to = "Present"
  ) %>%
  dplyr::group_by(
    Period_FPE,
    Profile
  ) %>%
  dplyr::summarise(
    Percent = 100 * mean(Present),
    .groups = "drop"
  ) %>%
  dplyr::left_join(
    period_n,
    by = "Period_FPE"
  ) %>%
  dplyr::mutate(
    Profile_Label = profile_labels[Profile],
    Period_Label = paste0(
      Period_FPE,
      "\n(n=",
      N_period,
      ")"
    )
  )

p5 <- ggplot2::ggplot(
  figure5_data,
  ggplot2::aes(
    x = Period_Label,
    y = Percent,
    shape = Profile_Label
  )
) +
  ggplot2::geom_point(
    size = 3,
    position = ggplot2::position_dodge(width = 0.35)
  ) +
  ggplot2::labs(
    title = "Screening indicators by historical period",
    x = NULL,
    y = "Documents with screening marker (%)",
    shape = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 15,
      hjust = 1
    )
  )

save_plot(
  p5,
  "Figure_05_period_dotplot.png",
  9.6,
  5.8
)

# ------------------------------ 13. FIGURE 6: X-ONLY JITTER ---------------------
p6 <- ggplot2::ggplot(
  coded,
  ggplot2::aes(
    x = Period_FPE,
    y = Relation_Marker_N
  )
) +
  ggplot2::geom_boxplot(
    outlier.shape = NA,
    width = 0.45
  ) +
  ggplot2::geom_jitter(
    width = 0.12,
    height = 0,
    alpha = 0.35,
    size = 1.6
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(
      0,
      max(coded$Relation_Marker_N),
      by = 1
    )
  ) +
  ggplot2::labs(
    title = "Documentary relation-marker counts by historical period",
    x = NULL,
    y = "Documentary relation-marker count"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 15,
      hjust = 1
    )
  )

save_plot(
  p6,
  "Figure_06_relation_marker_count_by_period.png",
  8.8,
  5.8
)


# ------------------------------ 13B. STRICT-SET OVERLAP -------------------------
strict_overlap <- tibble::tibble(
  Metric = c(
    "Strict layered cases",
    "Strict labour-linked cases",
    "Strict harvest-triggered rights-change cases",
    "Layered ∩ labour-linked",
    "Unique texts across the three strict sets"
  ),
  N = c(
    sum(coded$Strict_Layered_Relation),
    sum(coded$Strict_Labour_Linked_Entitlement),
    sum(coded$Strict_Temporal_Rights_Change),
    sum(
      coded$Strict_Layered_Relation == 1 &
        coded$Strict_Labour_Linked_Entitlement == 1
    ),
    sum(
      coded$Strict_Layered_Relation == 1 |
        coded$Strict_Labour_Linked_Entitlement == 1 |
        coded$Strict_Temporal_Rights_Change == 1
    )
  )
)

write_csv_utf8(
  strict_overlap,
  "Table_S6_strict_set_overlap.csv"
)

# ------------------------------ 14. DOCUMENT INDEX / MASTER OUTPUT --------------
document_index <- docs_main_149 %>%
  dplyr::mutate(
    Duplicate_Continuation_Flag =
      as.integer(Doc_Key == duplicate_key),
    Source_Volume = dplyr::case_when(
      Source == "Qingshuijiang_III" ~ "Qingshuijiang Documents, Series 3, vol. 1",
      Source == "Tianzhu_I" ~ "Tianzhu Documents, Series 1, vol. 21",
      TRUE ~ Source
    ),
    Set_ISBN = dplyr::case_when(
      Source == "Qingshuijiang_III" ~ "978-7-5495-0444-2",
      Source == "Tianzhu_I" ~ "978-7-214-10645-2",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(
    Source, Source_Volume, Set_ISBN,
    Item_No, Doc_ID, Year, Period_FPE, Origin,
    Heading, Page, Text_Reliability, Transaction_Type,
    Duplicate_Continuation_Flag
  )

write_csv_utf8(
  document_index,
  "Table_S5_document_index.csv"
)

master_list <- list(
  Duplicate_Verification = duplicate_verification,
  Selection_Flow = selection_flow,
  Transaction_Types = transaction_summary,
  Refined_Coded_148 = coded,
  Strict_Adjudication = strict_adjudication,
  Broad_vs_Strict = sensitivity_table,
  Negative_Control_Audit = negative_audit,
  Strict_Set_Overlap = strict_overlap,
  Period_Sensitivity = period_summary,
  Sales_Only_Sensitivity = sales_period_summary,
  Document_Index = document_index
)

openxlsx::write.xlsx(
  master_list,
  file = file.path(
    OUT_DIR,
    "Qingshuijiang_FPE_Strict_v2_All_Results.xlsx"
  ),
  overwrite = TRUE
)

capture.output(
  sessionInfo(),
  file = file.path(OUT_DIR, "sessionInfo.txt")
)

cat("\n====================================================\n")
cat("Qingshuijiang FPE strict-relational sensitivity v2.2 complete\n")
cat("====================================================\n")
cat("Screened records:", screened_n, "\n")
cat("Main-study records:", nrow(docs_main_149), "\n")
cat("Unique analytical texts:", nrow(coded), "\n")
cat("Broad layered candidates:", sum(coded$Broad_Layered_Candidate), "\n")
cat("Strict layered cases:", sum(coded$Strict_Layered_Relation), "\n")
cat("Broad labour candidates:", sum(coded$Broad_Labour_Candidate), "\n")
cat("Strict labour-linked cases:", sum(coded$Strict_Labour_Linked_Entitlement), "\n")
cat("Broad temporal candidates:", sum(coded$Broad_Temporal_Candidate), "\n")
cat("Strict harvest-triggered transitions:", sum(coded$Strict_Temporal_Rights_Change), "\n")
cat("Kruskal-Wallis all texts p =", kw_all$p.value, "\n")
cat("Kruskal-Wallis sales only p =", kw_sales$p.value, "\n")
cat("Outputs:", normalizePath(OUT_DIR, winslash = "/", mustWork = FALSE), "\n")
