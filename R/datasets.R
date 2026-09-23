#' National Park Service Monthly Visitor Use Statistics
#'
#' A dataset containing monthly visitation metrics, overnight stays, and geographic
#' metadata for National Park Service (NPS) units.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{UnitCode}{A character string. Official 4-character alphanumeric park unit code (e.g., "YELL", "GRCA").}
#'   \item{UnitName}{A character string. Full name of the National Park Service unit.}
#'   \item{State}{A character string. Two-letter postal abbreviation for the primary state or territory in which the unit is located.}
#'   \item{Latitude}{A numeric value. Latitude coordinate of the park centroid in decimal degrees.}
#'   \item{Longitude}{A numeric value. Longitude coordinate of the park centroid in decimal degrees.}
#'   \item{Year}{An integer. Four-digit calendar year.}
#'   \item{Month}{An integer. Calendar month (1-12).}
#'   \item{StateID}{An integer or character vector. FIPS state code or state identifier.}
#'   \item{RecreationVisits}{An integer. Count of recreational entries into the park unit.}
#'   \item{NonRecreationVisits}{An integer. Count of non-recreational entries (e.g., commuters, commercial traffic).}
#'   \item{TotalVisits}{An integer. Total count of visits (sum of recreation and non-recreation visits).}
#'   \item{RecreationVisitorHours}{An integer. Total hours spent in the park by recreation visitors.}
#'   \item{TotalVisitorHours}{An integer. Total hours spent in the park by all visitors.}
#'   \item{ConcessionerLodgingOvernights}{An integer. Overnight stays in lodging facilities operated by commercial concessioners.}
#'   \item{ConcessionerCampingOvernights}{An integer. Overnight stays in campgrounds operated by commercial concessioners.}
#'   \item{TentOvernights}{An integer. Overnight stays in tent sites in NPS-managed campgrounds.}
#'   \item{RVCampingOvernights}{An integer. Overnight stays in RV sites in NPS-managed campgrounds.}
#'   \item{TotalRecreationOvernights}{An integer. Total recreation overnight stays across all categories.}
#'   \item{BackcountryOvernights}{An integer. Overnight stays in backcountry or wilderness areas.}
#'   \item{MiscOvernights}{An integer. Miscellaneous overnight stays (e.g., group sites, boat stays).}
#'   \item{NonRecreationOvernights}{An integer. Overnight stays associated with non-recreation visits.}
#'   \item{TotalOvernights}{An integer. Total overnight stays recorded across all categories.}
#'   \item{NonRecreationVisitorHours}{An integer. Total hours spent in the park by non-recreation visitors.}
#' }
#'
#' @details
#' This dataset is in the Public Domain (U.S. Government Work under 17 U.S.C. § 105)
#' and is free of copyright restrictions for public reuse.
#'
#' @docType data
#' @keywords datasets
#' @name NPSvisits
#' @usage data(NPSvisits)
#'
#' @source National Park Service Visitor Use Statistics Portal (\url{https://irma.nps.gov/Stats/})
#'   and IRMA DataStore Reference 2317666 (\url{https://irma.nps.gov/DataStore/Reference/Profile/2317666}).
#'   Geographic coordinate and unit metadata retrieved via the official NPS API (\url{https://developer.nps.gov/}).
"NPSvisits"
