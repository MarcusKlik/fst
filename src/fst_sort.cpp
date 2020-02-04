/*
 fst - R package for ultra fast storage and retrieval of datasets

 Copyright (C) 2017-present, Mark AJ Klik

 This file is part of the fst R package.

 The fst R package is free software: you can redistribute it and/or modify it
 under the terms of the GNU Affero General Public License version 3 as
 published by the Free Software Foundation.

 The fst R package is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 for more details.

 You should have received a copy of the GNU Affero General Public License along
 with the fst R package. If not, see <http://www.gnu.org/licenses/>.

 You can contact the author at:
 - fst R package source repository : https://github.com/fstpackage/fst
*/


#include <Rcpp.h>
#include <Rcpp.h>

#include <sort/sort.h>


SEXP fstmergesort(SEXP int_vec_left, SEXP int_vec_right) {

  int length_left = LENGTH(int_vec_left);
  int length_right = LENGTH(int_vec_right);

  int tot_size = length_left + length_right;

  // create result vector
  SEXP res_vec = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) tot_size));

  // pointers
  int* left_p = INTEGER(int_vec_left);
  int* right_p = INTEGER(int_vec_right);
  int* res_p = INTEGER(res_vec);

  // do something here
  fst_merge_sort(left_p, right_p, length_left, length_right, res_p);

  UNPROTECT(1);

  return res_vec;
}


SEXP fstsort_combined(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  if (length == 0) return int_vec;

  int pos = length / 2;

  // create result vector
  SEXP res_vec = PROTECT(Rf_allocVector(INTSXP, (R_xlen_t) length));
  int* res_p = INTEGER(res_vec);

  // split in two and sort
  int pivot = (vec[0] + vec[pos - 1]) / 2;
  fst_quick_sort(vec, pos, pivot);

  pivot = (vec[pos] + vec[length - 1]) / 2;
  fst_quick_sort(&vec[pos], length - pos, pivot);

  fst_merge_sort(vec, &vec[pos], pos, length - pos, res_p);

  UNPROTECT(1);

  return res_vec;
}


SEXP fstsort_radix(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  SEXP buffer_vec = PROTECT(Rf_allocVector(INTSXP, length));
  int* buffer = INTEGER(buffer_vec);

  fst_radix_sort(vec, length, buffer);

  UNPROTECT(1);

  return int_vec;
}


SEXP fstsort(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  if (length < 3) {
    if (length == 2) {
      if (vec[0] > vec[1]) {
        int tmp = vec[0];
        vec[0] = vec[1];
        vec[1] = tmp;
      }
    }
    return int_vec;
  }

  // take center value as median estimate
  int pivot = (vec[0] + vec[(length - 1) / 2] + vec[length - 1]) / 3;
  fst_quick_sort(vec, length, pivot);

  return int_vec;
}
