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


inline void fst_quicksort(int* vec, int length, int pivot) {

  // std::cout << "\npivot: " << pivot << "\n" << std::flush;

  int pos_left = 0;
  int pos_right = length - 1;

  int elem_left = vec[0];
  int elem_right = vec[pos_right];

  int64_t sum_left = 0;
  int64_t sum_right = 0;

  while (true) {

    // iterate left until value > pivot
    while (elem_left <= pivot && pos_left != pos_right) {
      sum_left += elem_left;
      // std::cout << "add sum left: " << elem_left << " (" << sum_left << ")\n" << std::flush;
      elem_left = vec[++pos_left];
    }

    // left swap value found, iterate right until value < pivot
    while (elem_right > pivot && pos_right != pos_left) {
      sum_right += elem_right;
      // std::cout << "add sum right: " << elem_right << " (" << sum_right << ")\n" << std::flush;
      elem_right = vec[--pos_right];
    }

    if (pos_left == pos_right) break;

    // swap values

    int tmp = elem_right;
    elem_right = elem_left;  // swapped
    elem_left = tmp;

    vec[pos_left] = elem_left;
    vec[pos_right] = elem_right;
  }

  // pos_left == pos_right as this point

  if (vec[pos_left] < pivot) {
    sum_left += vec[pos_left];
    // std::cout << "add sum left: " << vec[pos_left] << " (" << sum_left << ")\n" << std::flush;
    pos_left++;
  } else
  {
    sum_right += vec[pos_left];
    // std::cout << "add sum right: " << vec[pos_left] << " (" << sum_right << ")\n" << std::flush;
  }

  // do not use elem_left after this point (as pos_left is possibly updated)

  if (pos_left > 2) {
    int piv = (int) (sum_left / (int64_t) (pos_left));
    // int piv = (vec[0] + vec[pos_left - 1]) / 2;
    // std::cout << "piv: " << piv << " piv new: " << sum_left / pos_left << "\n" << std::flush;
    fst_quicksort(vec, pos_left, piv);
  }
  else if (pos_left == 2 && vec[0] > vec[1]) {
  // swap first 2 elements
    int tmp = vec[1];
    vec[1] = vec[0];
    vec[0] = tmp;
  }

  if (pos_left < (length - 2)) {
    int piv = (int) (sum_right / (int64_t)(length - pos_left));
    // int piv = (vec[pos_left] + vec[length - 1]) / 2;
    // std::cout << "piv: " << piv << " piv new: " << sum_right / (length - pos_left) << "\n" << std::flush;
    fst_quicksort(&vec[pos_left], length - pos_left, piv);
  } else if (pos_left == (length - 2) && vec[pos_left] > vec[pos_left + 1]) {
    // swap last 2 elements if in reverse order
    int tmp = vec[pos_left];
    vec[pos_left] = vec[pos_left + 1];
    vec[pos_left + 1] = tmp;
  }
}


void fst_mergesort(int* left_p, int* right_p, int length_left, int length_right, int* res_p) {

  int pos_left = 0;
  int pos_right = 0;

  // populate result vector
  int pos = 0;
  while (pos_left < length_left && pos_right < length_right) {

    int val_left = left_p[pos_left];
    int val_right = right_p[pos_right];

    if (val_left <= val_right) {
      res_p[pos] = val_left;
      pos_left++;
      pos++;
      continue;
    }

    res_p[pos] = val_right;
    pos_right++;
    pos++;
  }

  // populate remainder
  if (pos_left == length_left) {
    while (pos_right < length_right) {
      res_p[pos] = right_p[pos_right];
      pos++;
      pos_right++;
    }
  } else
  {
    while (pos_left < length_left) {
      res_p[pos] = left_p[pos_left];
      pos++;
      pos_left++;
    }
  }
}


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
  fst_mergesort(left_p, right_p, length_left, length_right, res_p);

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
  fst_quicksort(vec, pos, pivot);

  pivot = (vec[pos] + vec[length - 1]) / 2;
  fst_quicksort(&vec[pos], length - pos, pivot);

  fst_mergesort(vec, &vec[pos], pos, length - pos, res_p);

  UNPROTECT(1);

  return res_vec;
}


SEXP fstsort(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  if (length == 0) return int_vec;

  // take center value as median estimate
  int pivot = (vec[0] + vec[length - 1]) / 2;
  fst_quicksort(vec, length, pivot);

  return int_vec;
}
