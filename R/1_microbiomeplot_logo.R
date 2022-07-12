#' @title microbiomeplot_logo
#' @description Get the detailed information of microbiomeplot package.
#' @docType methods
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @importFrom stringr str_replace str_split str_replace_all str_trim
#' @importFrom ComplexHeatmap Heatmap columnAnnotation anno_barplot
#' @importFrom grid gpar
#' @importFrom ggplotify as.ggplot
#' @importFrom dplyr filter mutate select everything left_join case_when
#' @importFrom plyr dlply .
#' @importFrom rstudioapi isAvailable hasFun getThemeInfo
#' @importFrom utils packageDescription write.csv
#' @importFrom cli rule symbol
#' @importFrom crayon green blue col_align red black white style make_style num_colors
#' @importFrom plotly ggplotly
#' @importFrom pbapply pblapply
#' @importFrom openxlsx write.xlsx
#' @importFrom purrr map map2
#' @importFrom readr write_csv read_csv
#' @importFrom methods slot slot<-
#' @import ggplot2
#' @importFrom methods .hasSlot new is
#' @importFrom stats p.adjust rgamma sd median time setNames
#' @importFrom utils data str head tail packageVersion write.table
#' @importFrom magrittr %>%
#' @importFrom ggsci pal_lancet
#' @importFrom rlang warn quo_is_null abort seq2
#' @importFrom tibble add_column
#' @importFrom microbiomedataset activate_microbiome_dataset convert2tbl_graph extract_intensity
#' @importFrom tidygraph activate
#' @importFrom ggraph ggraph geom_node_circle geom_node_label theme_graph
#' @importFrom viridis viridis
#' @importFrom tidyr pivot_longer
#' @importFrom massdataset extract_sample_info extract_variable_info extract_expression_data
#' @export
#' @return logo
#' @examples
#' microbiomeplot_logo()

microbiomeplot_logo <-
  function() {
    message(crayon::green("Thank you for using microbiomeplot!\n"))
    message(crayon::green("Version",
                          microbiomeplot_version,
                          "(",
                          update_date,
                          ')\n'))
    message(crayon::green(
      "More information: search 'tidymicrobiome microbiomeplot'.\n"
    ))
    cat(crayon::green(
      c(
        "            _                _     _                      _____  _       _   ",
        "           (_)              | |   (_)                    |  __ \\| |     | |  ",
        "  _ __ ___  _  ___ _ __ ___ | |__  _  ___  _ __ ___   ___| |__) | | ___ | |_ ",
        " | '_ ` _ \\| |/ __| '__/ _ \\| '_ \\| |/ _ \\| '_ ` _ \\ / _ \\  ___/| |/ _ \\| __|",
        " | | | | | | | (__| | | (_) | |_) | | (_) | | | | | |  __/ |    | | (_) | |_ ",
        " |_| |_| |_|_|\\___|_|  \\___/|_.__/|_|\\___/|_| |_| |_|\\___|_|    |_|\\___/ \\__|",
        "                                                                             ",
        "                                                                             "
      )
      
    ), sep = "\n")
  }

microbiomeplot_version <-
  as.character(utils::packageVersion(pkg = "microbiomeplot"))
update_date <- as.character(Sys.time())

# library(cowsay)
# # https://onlineasciitools.com/convert-text-to-ascii-art
# # writeLines(capture.output(say("Hello"), type = "message"), con = "ascii_art.txt")
# art <- readLines("logo.txt")
# dput(art)
# microbiomeplot_logo <-
#   c("                          _____        _                 _   ",
#     "                         |  __ \\      | |               | |  ",
#     "  _ __ ___   __ _ ___ ___| |  | | __ _| |_ __ _ ___  ___| |_ ",
#     " | '_ ` _ \\ / _` / __/ __| |  | |/ _` | __/ _` / __|/ _ \\ __|",
#     " | | | | | | (_| \\__ \\__ \\ |__| | (_| | || (_| \\__ \\  __/ |_ ",
#     " |_| |_| |_|\\__,_|___/___/_____/ \\__,_|\\__\\__,_|___/\\___|\\__|",
#     "                                                             ",
#     "                                                             "
#   )
# cat(microbiomeplot_logo, sep = "\n")
