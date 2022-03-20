#ifndef XAPIAN_LITE_INTERNAL_H
#define XAPIAN_LITE_INTERNAL_H

#include "emacs-module.h"

typedef emacs_value (*emacs_subr) (emacs_env *env,
                                   ptrdiff_t nargs, emacs_value *args,
                                   void *data);
#ifdef __cplusplus
extern "C" {
#endif

void
define_error
(emacs_env *env, const char *name,
 const char *description, const char *parent);

emacs_value
Fxapian_lite_reindex_file
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
  EMACS_NOEXCEPT;

emacs_value
Fxapian_lite_query_term
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
  EMACS_NOEXCEPT;

void
define_function
(emacs_env *env, const char *name, ptrdiff_t min_arity,
 ptrdiff_t max_arity, emacs_subr function, const char *documentation);

void
provide (emacs_env *env, const char *feature);

#ifdef __cplusplus
}
#endif

#endif /* XAPIAN_LITE_INTERNAL_H */
