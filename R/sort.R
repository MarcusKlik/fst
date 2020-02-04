#  fst - R package for ultra fast storage and retrieval of datasets
#
#  Copyright (C) 2017-present, Mark AJ Klik
#
#  This file is part of the fst R package.
#
#  The fst R package is free software: you can redistribute it and/or modify it
#  under the terms of the GNU Affero General Public License version 3 as
#  published by the Free Software Foundation.
#
#  The fst R package is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
#  for more details.
#
#  You should have received a copy of the GNU Affero General Public License along
#  with the fst R package. If not, see <http://www.gnu.org/licenses/>.
#
#  You can contact the author at:
#  - fst R package source repository : https://github.com/fstpackage/fst



#' Sort integer vector
#'
#' Fast in-place sorting of an integer vector. Uses multithreading to increase sorting
#' speed.
#'
#' @param int_vec integer vector to be sorted
#' @param method character string specifying the algorithm used. Valid methods are
#' 'quick' for quicksort and 'radix' for radix sort.
#'
#' @return sorted vector
#' @export
sort_fst <- function(int_vec, method = "radix") {

  if (method == "radix") {
    fstsort_radix(int_vec)
  } else if (method == "quick") {
    fstsort(int_vec)
  } else {
    stop("please select a valid sorting method, allowed values are 'radix' and 'quick'")
  }
}
