## code to prepare `journal_field` dataset goes here

psychology <- tibble::tribble(
  ~ journal, ~ journal_abbr, ~ openalex_id,
  "Developmental Psychology", "DP", "https://openalex.org/S126635229",
  "Journal of Personality and Social Psychology", "JPSP", "https://openalex.org/S29984966",
  "Journal of Abnormal Psychology", "JAP", "https://openalex.org/S121947241",
  "Journal of Family Psychology", "JFP", "https://openalex.org/S50147421",
  "Health Psychology", "HP", "https://openalex.org/S34137071",
  "Journal of Educational Psychology", "JEP", "https://openalex.org/S61703936",
  # Above journals are the original
  "Journal of Experimental Social Psychology", "JESP", "https://openalex.org/S12410666",
  "Collabra. Psychology", "Collabra", "https://openalex.org/S4210175756",
  "Collabra", "Collabra", "https://openalex.org/S2737007392",
  "Journal of Experimental Psychology. General", "JEPG", "https://openalex.org/S62013203",
  "Journal of Applied Psychology", "JApP", "https://openalex.org/S166002381",
  "Psychological Methods", "PM", "https://openalex.org/S45419345",
  "Advances in Methods and Practices in Psychological Science", "AMPPS", "https://openalex.org/S4210173062",
  "Psychological Science", "PS", "https://openalex.org/S58854535",
  "Child Development", "CD", "https://openalex.org/S109723506",
  "Developmental Science", "DS", "https://openalex.org/S154906575",
  "Personality & Social Psychology bulletin", "PSPB", "https://openalex.org/S187348256",
  "Nature Human Behaviour", "NHB", "https://openalex.org/S2764866340"
)

economics <- tibble::tribble(
  ~ journal, ~ journal_abbr, ~ openalex_id,
  "Journal of Economic Psychology", "JEcP", "https://openalex.org/S71968408",
  "Journal of Experimental and Behavioral Economics", "JEBE", "https://openalex.org/S4210233753",
  "Experimental Economics", "EE", "https://openalex.org/S181493553",
  "Journal of Development Economics", "JDE", "https://openalex.org/S101209419",
  "World Development", "WD", "https://openalex.org/S85457386",
  "Quarterly Journal of Economics", "QJE", "https://openalex.org/S203860005",
  "Econometrica", "Econometrica", "https://openalex.org/S95464858",
  "Behavioural Public Policy", "BPP", "https://openalex.org/S4210205184",
  "African Development Review", "ADR", "https://openalex.org/S33443600",
  "African Journal of Agricultural and Resource Economics", "AJARE", "https://openalex.org/S2764872897",
  "African Journal of Economic and Management Studies", "AJEMS", "https://openalex.org/S42260610",
  "African Journal of Economic Policy", "AJEP", "https://openalex.org/W4251276908", # no records found
  "American Economic Journal. Applied Economics", "AEJAE", "https://openalex.org/S42893225",
  "American Economic Journal. Economic Policy", "AEJEP", "https://openalex.org/S158011328",
  "American Economic Journal. Macroeconomics", "AEJM", "https://openalex.org/S170166683",
  "American Economic Review", "AER", "https://openalex.org/S23254222",
  "American Economic Review. Insights", "AERI", "https://openalex.org/S4210174288",
  "Economic Development and Cultural Change", "EDCC", "https://openalex.org/S71670289",
  "Economic Journal", "EC", "https://openalex.org/S45992627",
  "Journal of African Economies", "JAE", "https://openalex.org/S36588791",
  "Journal of African Development", "JAD", "https://openalex.org/S4210182335",
  "Journal of Human Development and Capabilities", "JHDC", "https://openalex.org/S119060959",
  "Journal of Development Effectiveness", "JDE", "https://openalex.org/S136516072",
  "Journal of Development Studies", "JDS", "https://openalex.org/S61808140",
  "Journal of Economic Growth", "JEG", "https://openalex.org/S181171746",
  "Journal of Labor Economics", "JLE", "https://openalex.org/S8557221",
  "Journal of Political Economy", "JPE", "https://openalex.org/S95323914",
  "Journal of Public Economics", "JPuE", "https://openalex.org/S199447588",
  "Review of African Political Economy", "RAfPE", "https://openalex.org/S36169284",
  "Review of Development Economics", "RDE", "https://openalex.org/S95496512",
  "Review of Economic Studies", "RES", "https://openalex.org/S88935262",
  "Review of International Political Economy", "RIPE", "https://openalex.org/S2875300",
  "South African Journal of Economics", "SAJE", "https://openalex.org/S90392387",
  "World Bank Economic Review", "WBER", "https://openalex.org/S2735890421",
  "World Bank Research Observer", "WBRO", "https://openalex.org/S117685085",
  "World Development Perspectives", "WDP", "https://openalex.org/S2898425895"
)

general <- tibble::tribble(
  ~ journal, ~ journal_abbr, ~ openalex_id,
  "Proceedings of the National Academy of Sciences of the United States of America", "PNAS", "https://openalex.org/S125754415",
  "Science", "Science", "https://openalex.org/S3880285",
  "Nature", "Nature", "https://openalex.org/S137773608",
  "Plos One", "Plos One", "https://openalex.org/S202381698"
)

journal_field <- dplyr::bind_rows(psychology, economics, general)

# journal_field$journal_short <- clean_journal_names(journal_field$journal)

journal_field$field <- ifelse(
  journal_field$journal %in% psychology$journal, "psychology", ifelse(
    journal_field$journal %in% economics$journal, "economics", ifelse(
      journal_field$journal %in% general$journal, "general", NA
    )
  )
)
journal_field$original_journal <- journal_field$journal %in% psychology$journal[1:6]

usethis::use_data(journal_field, overwrite = TRUE)
