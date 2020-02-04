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

  while (true) {

    // iterate left until value > pivot
    while (vec[pos_left] <= pivot && pos_left != pos_right) ++pos_left;

    // left swap value found, iterate right until value < pivot
    while (vec[pos_right] > pivot && pos_right != pos_left) --pos_right;

    if (pos_left == pos_right) break;

    // swap values
    int tmp = vec[pos_right];
    vec[pos_right] = vec[pos_left];
    vec[pos_left] = tmp;
  }

  // pos_left == pos_right as this point

  if (vec[pos_left] < pivot) {
    pos_left++;
  }

  // do not use elem_left after this point (as pos_left is possibly updated)

  if (pos_left > 2) {
    int piv = (vec[0] + vec[pos_left / 2] + vec[pos_left - 1]) / 3;
    fst_quicksort(vec, pos_left, piv);
  }
  else if (pos_left == 2 && vec[0] > vec[1]) {
  // swap first 2 elements
    int tmp = vec[1];
    vec[1] = vec[0];
    vec[0] = tmp;
  }

  if (pos_left < (length - 2)) {
    int piv = (vec[pos_left] + vec[(length + pos_left) / 2] + vec[length - 1]) / 3;
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


void fst_radixsort(int* vec, int length, int* buffer)
{
  int index[256];

  // phase 1: sort on lower byte

  // initialize
  for (int ind = 0; ind < 256; ++ind) index[ind] = 0;

  // count each occurence
  int batch_length = length / 8;

  for (int pos = 0; pos < batch_length; ++pos) {
    int ind = 8 * pos;
    ++index[vec[ind] & 255];
    ++index[vec[ind + 1] & 255];
    ++index[vec[ind + 2] & 255];
    ++index[vec[ind + 3] & 255];
    ++index[vec[ind + 4] & 255];
    ++index[vec[ind + 5] & 255];
    ++index[vec[ind + 6] & 255];
    ++index[vec[ind + 7] & 255];
  }

  for (int pos = 8 * batch_length; pos < length; ++pos) {
    ++index[vec[pos] & 255];
  }


  // cumulative positions
  int cum_pos = index[0];
  index[0] = 0;
  for (int ind = 1; ind < 256; ++ind) {
    int old_val = index[ind];
    index[ind] = cum_pos;
    cum_pos += old_val;
  }

  // fill buffer
  // count each occurence
  for (int pos = 0; pos < length; ++pos) {
    int value = vec[pos];
    int target_pos = index[value & 255]++;
    buffer[target_pos] = value;
  }

  // phase 2: sort on byte 2

  // initialize
  for (int ind = 0; ind < 256; ++ind) index[ind] = 0;

  // count each occurence
  for (int pos = 0; pos < batch_length; ++pos) {
    int ind = 8 * pos;
    ++index[(buffer[ind] >> 8) & 255];
    ++index[(buffer[ind + 1] >> 8) & 255];
    ++index[(buffer[ind + 2] >> 8) & 255];
    ++index[(buffer[ind + 3] >> 8) & 255];
    ++index[(buffer[ind + 4] >> 8) & 255];
    ++index[(buffer[ind + 5] >> 8) & 255];
    ++index[(buffer[ind + 6] >> 8) & 255];
    ++index[(buffer[ind + 7] >> 8) & 255];
  }

  for (int pos = 8 * batch_length; pos < length; ++pos) {
    ++index[(buffer[pos] >> 8) & 255];
  }

  // cumulative positions
  cum_pos = index[0];
  index[0] = 0;
  for (int ind = 1; ind < 256; ++ind) {
    int old_val = index[ind];
    index[ind] = cum_pos;
    cum_pos += old_val;
  }

  // fill buffer
  // count each occurence
  for (int pos = 0; pos < length; ++pos) {
    int value = buffer[pos];
    int target_pos = index[(value >> 8) & 255]++;
    vec[target_pos] = value;
  }

  // phase 3: sort on byte 3

  // initialize
  for (int ind = 0; ind < 256; ++ind) index[ind] = 0;

  // count each occurence
  for (int pos = 0; pos < batch_length; ++pos) {
    int ind = 8 * pos;
    ++index[(vec[ind] >> 16) & 255];
    ++index[(vec[ind + 1] >> 16) & 255];
    ++index[(vec[ind + 2] >> 16) & 255];
    ++index[(vec[ind + 3] >> 16) & 255];
    ++index[(vec[ind + 4] >> 16) & 255];
    ++index[(vec[ind + 5] >> 16) & 255];
    ++index[(vec[ind + 6] >> 16) & 255];
    ++index[(vec[ind + 7] >> 16) & 255];
  }

  for (int pos = 8 * batch_length; pos < length; ++pos) {
    ++index[(vec[pos] >> 16) & 255];
  }

  // cumulative positions
  cum_pos = index[0];
  index[0] = 0;
  for (int ind = 1; ind < 256; ++ind) {
    int old_val = index[ind];
    index[ind] = cum_pos;
    cum_pos += old_val;
  }

  // fill vec
  // count each occurence
  for (int pos = 0; pos < length; ++pos) {
    int value = vec[pos];
    int target_pos = index[(value >> 16) & 255]++;
    buffer[target_pos] = value;
  }

  // phase 4: sort on byte 3

  // initialize
  for (int ind = 0; ind < 256; ++ind) index[ind] = 0;

  // count each occurence
  for (int pos = 0; pos < batch_length; ++pos) {
    int ind = 8 * pos;
    ++index[((buffer[ind] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 1] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 2] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 3] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 4] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 5] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 6] >> 24) & 255) ^ 128];
    ++index[((buffer[ind + 7] >> 24) & 255) ^ 128];
  }

  for (int pos = 8 * batch_length; pos < length; ++pos) {
    ++index[((buffer[pos] >> 24) & 255) ^ 128];
  }


  // cumulative positions
  cum_pos = index[0];
  index[0] = 0;
  for (int ind = 1; ind < 256; ++ind) {
    int old_val = index[ind];
    index[ind] = cum_pos;
    cum_pos += old_val;
  }

  // fill buffer
  // count each occurence
  for (int pos = 0; pos < length; ++pos) {
    int value = buffer[pos];
    int target_pos = index[((value >> 24) & 255) ^ 128]++;
    vec[target_pos] = value;
  }

}

SEXP fstsort_radix(SEXP int_vec) {

  int* vec = INTEGER(int_vec);
  int length = LENGTH(int_vec);

  SEXP buffer_vec = PROTECT(Rf_allocVector(INTSXP, length));
  int* buffer = INTEGER(buffer_vec);

  fst_radixsort(vec, length, buffer);

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
  int piv = (vec[0] + vec[(length - 1) / 2] + vec[length - 1]) / 3;
  int pivot = (vec[0] + vec[length - 1]) / 2;
  fst_quicksort(vec, length, pivot);

  return int_vec;
}
