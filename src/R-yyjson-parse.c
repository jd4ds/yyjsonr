#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "zlib.h"
#include "yyjson.h"
#include "R-yyjson-parse.h"



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Forward declarations
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP json_array_of_objects_to_data_frame(yyjson_val *arr, parse_options *opt);
SEXP json_as_robj(yyjson_val *val, parse_options *opt);


//===========================================================================
// Pare the R list of options into the 'parse_options' struct
//
// @param parse_opts_ An R named list of options. Passed in from the user.
//===========================================================================
parse_options create_parse_options(SEXP parse_opts_) {
  
  // Set default options
  parse_options opt = {
    .int64                 = INT64_AS_STR,
    .df_missing_list_elem  = R_NilValue,
    .obj_of_arrs_to_df     = true,
    .arr_of_objs_to_df     = true,
    .length1_array_asis    = false,
    .str_specials          = STR_SPECIALS_AS_STRING,
    .num_specials          = NUM_SPECIALS_AS_SPECIAL,
    .promote_num_to_string = false,
    .yyjson_read_flag      = 0,
    // ++ START: Initialize new options for issue #52
    .empty_array           = R_NilValue,
    .empty_array_set       = false,
    .empty_object          = R_NilValue,
    .empty_object_set      = false
    // ++ END: Initialize new options for issue #52
  };
  
  // Sanity check and extract option names from the named list
  if (isNull(parse_opts_) || length(parse_opts_) == 0) {
    return opt;
  }
  
  if (!isNewList(parse_opts_)) {
    error("'parse_opts' must be a list");
  }
  
  SEXP nms_ = getAttrib(parse_opts_, R_NamesSymbol);
  if (isNull(nms_)) {
    error("'parse_opts' must be a named list");
  }
  
  // Loop over options in R named list and assign to C struct
  for (int i = 0; i < length(parse_opts_); i++) {
    const char *opt_name = CHAR(STRING_ELT(nms_, i));
    SEXP val_ = VECTOR_ELT(parse_opts_, i);
    
    if (strcmp(opt_name, "length1_array_asis") == 0) {
      opt.length1_array_asis = asLogical(val_);
    } else if (strcmp(opt_name, "int64") == 0) {
      const char *val = CHAR(STRING_ELT(val_, 0));
      if (strcmp(val, "double") == 0) {
        opt.int64 = INT64_AS_DBL;
      } else if (strcmp(val, "bit64") == 0) {
        opt.int64 = INT64_AS_BIT64;
      } else {
        opt.int64 = INT64_AS_STR;
      }
    } else if (strcmp(opt_name, "df_missing_list_elem") == 0) {
      opt.df_missing_list_elem = val_;
    } else if (strcmp(opt_name, "yyjson_read_flag") == 0) {
      for (unsigned int idx = 0; idx < length(val_); idx++) {
        opt.yyjson_read_flag |= (unsigned int)INTEGER(val_)[idx];
      }
    } else if (strcmp(opt_name, "obj_of_arrs_to_df") == 0) {
      opt.obj_of_arrs_to_df = asLogical(val_);
    } else if (strcmp(opt_name, "arr_of_objs_to_df") == 0) {
      opt.arr_of_objs_to_df = asLogical(val_);
    } else if (strcmp(opt_name, "str_specials") == 0) {
      const char *val = CHAR(STRING_ELT(val_, 0));
      opt.str_specials = strcmp(val, "string") == 0 ? STR_SPECIALS_AS_STRING : STR_SPECIALS_AS_SPECIAL;
    } else if (strcmp(opt_name, "num_specials") == 0) {
      const char *val = CHAR(STRING_ELT(val_, 0));
      opt.num_specials = strcmp(val, "string") == 0 ? NUM_SPECIALS_AS_STRING : NUM_SPECIALS_AS_SPECIAL;
    } else if (strcmp(opt_name, "promote_num_to_string") == 0) {
      opt.promote_num_to_string = asLogical(val_);
    // ++ START: Add handlers for new options for issue #52
    } else if (strcmp(opt_name, "empty_array") == 0) {
      opt.empty_array = val_;
      opt.empty_array_set = true;
    } else if (strcmp(opt_name, "empty_object") == 0) {
      opt.empty_object = val_;
      opt.empty_object_set = true;
    // ++ END: Add handlers for new options for issue #52
    } else {
      warning("Unknown option ignored: '%s'\n", opt_name);
    }
  }

  return opt;
}

// ... (The rest of the file remains unchanged until json_array_as_robj)
// ...
// ... (Lines 125 to 1130 are identical to the original)
// ...

//===========================================================================
// Parse JSON []-array to R object
// 
// Possible outputs:
//   - Atomic vector: lgl, int, real, str, integer64
//   - List
//   - Matrix
//   - 3D array
//   - Data.frame
//===========================================================================
SEXP json_array_as_robj(yyjson_val *arr, parse_options *opt) {
  
  int nprotect = 0;
  SEXP res_ = R_NilValue;
  
  if (!yyjson_is_arr(arr)) {
    error("json_array_() got passed something NOT a json array");
  }
  
  
  size_t len = yyjson_get_len(arr);
  
  // ++ START: Modified empty array handling for issue #52
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Empty []-array becomes an empty list or user-defined value
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (len == 0) {
    if (opt->empty_array_set) {
      // User has provided a specific SEXP for empty arrays
      return opt->empty_array;
    } else {
      // Default behaviour: empty []-array becomes an empty list
      res_ = PROTECT(allocVector(VECSXP, 0)); 
      UNPROTECT(1);
      return res_;
    }
  }
  // ++ END: Modified empty array handling for issue #52
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Find what sort of containers exists within this array
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  unsigned int ctn_bitset = get_json_array_sub_container_types(arr, opt);
  
  if (ctn_bitset == CTN_NONE) {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // There are no containers within the array.
    // Process as an atomic vector or list.
    // Use the 'type_bitset' of all the elements to determine best SEXP
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    unsigned int type_bitset = get_type_bitset_for_json_array(arr, 0, opt);
    unsigned int sexp_type = get_best_sexp_to_represent_type_bitset(type_bitset, opt);
    
    switch(sexp_type) {
    case LGLSXP:
      res_ = PROTECT(json_array_as_lglsxp(arr, opt)); nprotect++;
      break;
    case INTSXP:
      res_ = PROTECT(json_array_as_intsxp(arr, opt)); nprotect++;
      break;
    case REALSXP:
      res_ = PROTECT(json_array_as_realsxp(arr, opt)); nprotect++;
      break;
    case STRSXP:
      res_ = PROTECT(json_array_as_strsxp(arr, opt)); nprotect++;
      break;
    case VECSXP:
      res_ = PROTECT(json_array_as_vecsxp(arr, opt)); nprotect++;
      break;
    case INT64SXP:
      res_ = PROTECT(json_array_as_integer64(arr, opt)); nprotect++;
      break;
    default:
      error("json_array_as_robj(). Ooops\n");
    }
    
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // Tag a length-1 array as class = 'AsIs'
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (opt->length1_array_asis && length(res_) == 1 && !inherits(res_, "Integer64")) {
      setAttrib(res_, R_ClassSymbol, mkString("AsIs"));
    }
    
  } else if (ctn_bitset == CTN_ARR) {
    unsigned int sexp_type = get_best_sexp_type_for_matrix(arr, opt);
    if (sexp_type != 0) {
      res_ = PROTECT(json_array_as_matrix(arr, sexp_type, opt)); nprotect++;
    } else {
      res_ = PROTECT(json_array_as_vecsxp(arr, opt)); nprotect++;
      
      // Check if compatible sub-matrices to make a 3d matrix
      //  i.e. 
      //    all members are matrices
      //    all members have the same dimension
      //    all matrices have the same type.
      //      Note: in future could check for compatible type e.g. int/real
      //      and promote all types to that for the final 3d matrix.
      //      For now, just keeping it basic.  Mike 2023-08-12
      bool is_3d_matrix = true;
      int dim0 = 0;
      int dim1 = 0;
      int nlayer = length(res_);
      unsigned int sexp_type = 0;
      
      if (nlayer > 1) {
        for (unsigned int layer = 0; layer < nlayer; layer++) {
          
          // check is matrix
          SEXP elem_ = VECTOR_ELT(res_, layer);
          if (!isMatrix(elem_)) {
            is_3d_matrix = false;
            break;
          }
          
          // Check dims
          SEXP dims_ = getAttrib(elem_, R_DimSymbol);
          if (layer == 0) {
            dim0 = INTEGER(dims_)[0];
            dim1 = INTEGER(dims_)[1];
          } else {
            if (INTEGER(dims_)[0] != dim0 || INTEGER(dims_)[1] != dim1) {
              is_3d_matrix = false;
              break;
            }
          }
          
          // check type
          if (layer == 0) {
            sexp_type = (unsigned int)TYPEOF(elem_);
          } else {
            if (TYPEOF(elem_) != sexp_type) {
              is_3d_matrix = false;
              break;
            }
          }
        }
        
        if (is_3d_matrix) {
          SEXP arr_ = R_NilValue;
          
          R_xlen_t N = nlayer * dim0 * dim1;
          switch(sexp_type) {
          case LGLSXP: {
            arr_ = PROTECT(allocVector(LGLSXP, N)); nprotect++;
            int *ptr = INTEGER(arr_);
            for (unsigned int layer = 0; layer < nlayer; layer++) {
              memcpy(ptr, INTEGER(VECTOR_ELT(res_, layer)), (size_t)dim0 * (size_t)dim1 * sizeof(int));
              ptr += dim0 * dim1;
            }
          }
            break;
          case INTSXP: {
            arr_ = PROTECT(allocVector(INTSXP, N)); nprotect++;
            int *ptr = INTEGER(arr_);
            for (unsigned int layer = 0; layer < nlayer; layer++) {
              memcpy(ptr, INTEGER(VECTOR_ELT(res_, layer)), (size_t)dim0 * (size_t)dim1 * sizeof(int));
              ptr += dim0 * dim1;
            }
          }
            break;
          case REALSXP: {
            arr_ = PROTECT(allocVector(REALSXP, N)); nprotect++;
            double *ptr = REAL(arr_);
            for (unsigned int layer = 0; layer < nlayer; layer++) {
              memcpy(ptr, REAL(VECTOR_ELT(res_, layer)), (size_t)dim0 * (size_t)dim1 * sizeof(double));
              ptr += dim0 * dim1;
            }
          }
            break;
          case STRSXP: {
            arr_ = PROTECT(allocVector(STRSXP, N)); nprotect++;
            unsigned int arr_idx = 0;
            for (unsigned int layer = 0; layer < nlayer; layer++) {
              SEXP mat_ = VECTOR_ELT(res_, layer);
              for (unsigned int idx = 0; idx < dim0 * dim1; idx++) {
                SET_STRING_ELT(arr_, arr_idx, STRING_ELT(mat_, idx));        
                arr_idx++;
              }
            }
          }
            break;
          default:
            warning("Warning: Unhandled 3d matrix type: %i (%s)\n", sexp_type, type2char(sexp_type));
          }
          
          // Set dims on new 3d array.
          SEXP dims_ = PROTECT(allocVector(INTSXP, 3)); nprotect++;
          INTEGER(dims_)[0] = dim0;
          INTEGER(dims_)[1] = dim1;
          INTEGER(dims_)[2] = nlayer;
          setAttrib(arr_, R_DimSymbol, dims_);
          
          res_ = arr_;
          
        }
      }

    }    
  } else if (ctn_bitset == CTN_OBJ && opt->arr_of_objs_to_df) {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // []-array ONLY contains {}-objects!
    // Parse as a data.frame
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    res_ = json_array_of_objects_to_data_frame(arr, opt);
  } else {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // This array contains a mixture of container types
    // Parse as list
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    res_ = PROTECT(json_array_as_vecsxp(arr, opt)); nprotect++;
  }
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Tidy and return
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  UNPROTECT(nprotect);
  return res_;
}

// ... (The rest of the file remains unchanged until json_object_as_list)
// ...
// ... (Lines 1341 to 1627 are identical to the original)
// ...

//===========================================================================
//  #        #            #    
//  #                     #    
//  #       ##     ###   ####  
//  #        #    #       #    
//  #        #     ###    #    
//  #        #        #   #  # 
//  #####   ###   ####     ##  
//
// JSON {}-object to R List
//===========================================================================
SEXP json_object_as_list(yyjson_val *obj, parse_options *opt) {
  int nprotect = 0;
  
  if (!yyjson_is_obj(obj)) {
    error("json_object(): Must be object. Not %i -> %s\n", yyjson_get_type(obj), 
          yyjson_get_type_desc(obj));
  }
  R_xlen_t n = (R_xlen_t)yyjson_get_len(obj);
  
  // ++ START: Modified empty object handling for issue #52
  if (n == 0) {
    if (opt->empty_object_set) {
      // User has provided a specific SEXP for empty objects
      return opt->empty_object;
    } else {
      // Default behavior: empty {}-object becomes an empty named list
      SEXP res_ = PROTECT(allocVector(VECSXP, 0));
      SEXP nms_ = PROTECT(allocVector(STRSXP, 0));
      Rf_setAttrib(res_, R_NamesSymbol, nms_);
      UNPROTECT(2);
      return res_;
    }
  }
  // ++ END: Modified empty object handling for issue #52
  
  SEXP res_ = PROTECT(allocVector(VECSXP, n)); nprotect++;
  SEXP nms_ = PROTECT(allocVector(STRSXP, n)); nprotect++;
  
  yyjson_val *key, *val;
  yyjson_obj_iter iter = yyjson_obj_iter_with(obj);
  unsigned int idx = 0;
  while ((key = yyjson_obj_iter_next(&iter))) {
    val = yyjson_obj_iter_get_val(key);
    SET_VECTOR_ELT(res_, idx, json_as_robj(val, opt));
    SET_STRING_ELT(nms_, idx, mkChar(yyjson_get_str(key)));
    ++idx;
  }
  
  Rf_setAttrib(res_, R_NamesSymbol, nms_);
  
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // Test if list is promote-able to a data.frame
  //
  // * Opt to promote {}-object of []-arrays to data.frame
  // * All elements are atomic arrays or vecsxp
  // * All these elements are the same length
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (opt->obj_of_arrs_to_df) {
    R_xlen_t nrow = 0;
    bool possible_data_frame = true;
    for (unsigned int col = 0; col < idx; col++) {
      
      SEXP elem_ = VECTOR_ELT(res_, col);
      if (col == 0) {
        nrow = xlength(elem_);
      } else {
        R_xlen_t this_len = xlength(elem_);
        if (this_len != nrow) {
          possible_data_frame = false;
          break;
        }
      }
    }
    
    // all lengths are the same, and there is more
    // than 1 row, and there is more than 1 column
    if (possible_data_frame && nrow > 1 && idx > 1) {
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // Set rownames on data.frame
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SEXP rownames = PROTECT(allocVector(INTSXP, 2)); nprotect++;
      SET_INTEGER_ELT(rownames, 0, NA_INTEGER);
      SET_INTEGER_ELT(rownames, 1, -(int)nrow);
      setAttrib(res_, R_RowNamesSymbol, rownames);
      
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // Set 'data.frame' class
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      SET_CLASS(res_, mkString("data.frame"));
    }
  }
  
  UNPROTECT(nprotect);
  return res_;
}


// ... (The rest of the file is identical to the original)
// ...
