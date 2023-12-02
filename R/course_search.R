#' Search the online Smith College Course Catalog
#'
#' @param year Scalar character vector identifying the academic year you would like to search for classes during. AY 2022-23 is identified as 2023, AY 2021-22 is identified as 2022, etc.
#' @param semester  Scalar character vector identifying you would like to search for classes during. Fall is identified as 01, J-Term is identified as 02, Spring is identified as 03.
#' @param dept Scalar character vector identifying a department or program by it's three letter abbreviation (SDS, ENG, MTH, etc.). Note that searching for a specific department can return courses that are cross-listed in that department.
#' @param instructor Scalar character vector describing the name of the instructor whose courses you want to search for.
#' @param instr_method Scalar character vector, either "Online" or "In-Person".
#' @param credits Scalar vector describing the number of credits you want the classes in the results to have. Can be numeric, but is coerced to character.
#' @param course_number Scalar vector describing the course number you are searching for (e.g., 192, 220). Can be numeric, but is coerced to character.
#' @param course_keywords Character vector identifying keyword to look for in course names and descriptions.
#' @param course_levels A numeric vector with a most 4 elements. Elements should be restricted to integer values 1, 2, 3 or 4 (indicating a search for courses at the 100, 200, 300, or 400 level).
#' @param include_details Logical indicating whether to include the expanded course details (e.g., prerequisites, curriculum distribution, etc.). This can be relatively slow, so for large searches it may be desirable to suppress this portion of the results.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr bind_cols
#' @importFrom rvest read_html
#' @importFrom rvest html_element
#' @importFrom rvest html_elements
#' @importFrom utils URLencode
#'
#' @return
#' A data frame with up 19 variables:
#' * Dept
#' * Course.Num
#' * Course.Sec
#' * Title
#' * Instructor
#' * Waitlist
#' * Credits
#' * Course.Type
#' * Grade.Mode
#' * Coreq
#' * Curriculum.Distribution
#' * Time.Location
#' * Max.Enrollment
#' * Section.Enrollment
#' * Waitlist.Count
#' * Reserved.Seats
#' * Enforced.Requirements
#' * Instructional.Method
#' * Description
#'
#' @examples
#' \dontrun{
#' course_search(year = 2024, semester = "03", dept="SDS", course_levels = c(1, 2))
#' }
#'
#' @export
course_search <- function(year,
                          semester,
                          dept = "",
                          instructor = "",
                          instr_method = "",
                          credits = "",
                          course_number = "",
                          course_keywords = "",
                          course_levels = "",
                          include_details = TRUE

  ) {

  term <- paste0(year, semester)

  instructor <- paste0(instructor, collapse = "+")

  course_keywords <- paste(course_keywords, sep = "+", collapse = "+")

  course_levels <- sort(course_levels)
  names(course_levels) <- paste0("&course_level_", course_levels)

  URL <- paste0("https://www.smith.edu/apps/course_search/?",
                "term=", term,
                "&dept=", dept,
                "&instructor=", instructor,
                "&instr_method=", instr_method,
                "&credits=", credits,
                "&course_number=", course_number,
                "&course_keyword=", course_keywords,
                paste(names(course_levels), course_levels, sep = "=", collapse = ""),
                "&op=Submit",
                "&form_id=campus_course_search_basic_search_form"
                )

  search_results <- URL |>
    utils::URLencode() |>
    rvest::read_html() |>
    rvest::html_elements("article.campus-course-search-result")

  dept <- rvest::html_element(search_results, "span.course-dept") |>
    rvest::html_text()
  course_num <- rvest::html_element(search_results, "span.course-course-num") |>
    rvest::html_text()
  course_sec <- rvest::html_element(search_results, "span.course-section-num") |>
    rvest::html_text()
  title <- rvest::html_elements(search_results, "span.course-section-title") |>
    rvest::html_text2()
  instructor <- rvest::html_element(search_results, "span.course-section-instructor") |>
    rvest::html_text2()
  waitlist <- rvest::html_element(search_results, "span.course-section-status") |>
    rvest::html_text()

  main <- data.frame(
    Dept = dept,
    Course.Num = course_num,
    Course.Sec = course_sec,
    Title = title,
    Instructor = instructor,
    Waitlist = waitlist
  )

  if (include_details) {

    template <- data.frame(
      Credits = NA_character_,
      Course.Type = NA_character_,
      Grade.Mode = NA_character_,
      Coreq = NA_character_,
      Curriculum.Distribution = NA_character_,
      Time.Location = NA_character_,
      Max.Enrollment = NA_character_,
      Section.Enrollment = NA_character_,
      Waitlist.Count = NA_character_,
      Reserved.Seats = NA_character_,
      Enforced.Requirements = NA_character_,
      Instructional.Method = NA_character_,
      Description = NA_character_
    )

    details <- rvest::html_element(search_results, "div.collapse table") |>
      rvest::html_table() |>
      lapply(\(x) {

        y <- unlist(x[-nrow(x), ])
        y <- y[y != ""]

        y2 <- strsplit(y, split = ": ", fixed = TRUE)

        results <- lapply(y2, `[`, 2)
        names(results) <- lapply(y2, `[`, 1)

        tmp <- cbind(
          as.data.frame(results),
          data.frame("Description" = x[[1]][nrow(x)])
          )

        template_copy <- template
        template_copy[, names(template) %in% names(tmp)] <- tmp[1,]
        return(template_copy)
      }) |>
      dplyr::bind_rows()

    main <- dplyr::bind_cols(main, details)
  }

  return(main)
}
