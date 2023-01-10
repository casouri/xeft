#include "emacs-module.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#ifndef EMACS_MODULE_PRELUDE_H
#define EMACS_MODULE_PRELUDE_H

#define EMP_MAJOR_VERSION 1
#define EMP_MINOR_VERSION 0
#define EMP_PATCH_VERSION 0


/*
  Copy a Lisp string VALUE into BUFFER, and store the string size in
  SIZE.  A user doesn’t need to allocate BUFFER, but it is the user’s
  responsibility to free it.
 */
bool
emp_copy_string_contents
(emacs_env *env, emacs_value value, char **buffer, size_t *size)
/* Copied from Pillipp’s document.  I commented out assertions. */
{
  ptrdiff_t buffer_size;
  if (!env->copy_string_contents (env, value, NULL, &buffer_size))
    return false;
  /* assert (env->non_local_exit_check (env) == emacs_funcall_exit_return); */
  /* assert (buffer_size > 0); */
  *buffer = (char*) malloc ((size_t) buffer_size);
  if (*buffer == NULL)
    {
      env->non_local_exit_signal (env, env->intern (env, "memory-full"),
                                  env->intern (env, "nil"));
      return false;
    }
  ptrdiff_t old_buffer_size = buffer_size;
  if (!env->copy_string_contents (env, value, *buffer, &buffer_size))
    {
      free (*buffer);
      *buffer = NULL;
      return false;
    }
  /* assert (env->non_local_exit_check (env) == emacs_funcall_exit_return); */
  /* assert (buffer_size == old_buffer_size); */
  *size = (size_t) (buffer_size - 1);
  return true;
}

/*
  Return a Lisp string. This is basically env->make_string except that
  it calls strlen for you.
 */
emacs_value
emp_build_string (emacs_env *env, const char *string)
{
  return env->make_string (env, string, strlen (string));
}

/*
  Intern NAME to a symbol. NAME has to be all-ASCII.
 */
emacs_value
emp_intern (emacs_env *env, const char *name)
{
  return env->intern (env, name);
}

/*
  Call a function named FN which takes NARGS number of arguments.
  Example: funcall (env, "cons", 2, car, cdr);
 */
emacs_value
emp_funcall (emacs_env *env, const char* fn, ptrdiff_t nargs, ...)
{
  va_list argv;
  va_start (argv, nargs);
  emacs_value *args = (emacs_value *) malloc(nargs * sizeof(emacs_value));
  for (int idx = 0; idx < nargs; idx++)
    {
      args[idx] = va_arg (argv, emacs_value);
    }
  va_end (argv);
  emacs_value val = env->funcall (env, emp_intern (env, fn), nargs, args);
  free (args);
  return val;
}

/*
  Provide FEATURE like ‘provide’ in Lisp.
*/
void
emp_provide (emacs_env *env, const char *feature)
{
  emp_funcall (env, "provide", 1, emp_intern (env, feature));
}
  
/*
  Raise a signal where NAME is the signal name and MESSAGE is the
  error message.
 */
void
emp_signal_message1
(emacs_env *env, const char *name, const char *message)
{
  env->non_local_exit_signal
    (env, env->intern (env, name),
     emp_funcall (env, "cons", 2,
                  env->make_string (env, message, strlen (message)),
                  emp_intern (env, "nil")));
}

/*
  Define an error like ‘define-error’.
 */
void
emp_define_error
(emacs_env *env, const char *name,
 const char *description, const char *parent)
{
  emp_funcall (env, "define-error", 3,
               emp_intern (env, name),
               env->make_string (env, description, strlen (description)),
               emp_intern (env, parent));
}

/*
  Return true if VAL is symbol nil.
 */
bool
emp_nilp (emacs_env *env, emacs_value val)
{
  return !env->is_not_nil (env, val);
}

/*
  Define a function NAME. The number of arguments that the function
  takes is between MIN_ARITY and MAX_ARITY.  FUNCTION is a function
  with signature

  static emacs_value
  function
  (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
  EMACS_NOEXCEPT

  DOCUMENTATION is the docstring for FUNCTION.
 */
void
emp_define_function
(emacs_env *env, const char *name, ptrdiff_t min_arity,
 ptrdiff_t max_arity,
 emacs_value (*function) (emacs_env *env,
                          ptrdiff_t nargs,
                          emacs_value* args,
                          void *data) EMACS_NOEXCEPT,
 const char *documentation)
{
  emacs_value fn = env->make_function
    (env, min_arity, max_arity, function, documentation, NULL);
  emp_funcall (env, "fset", 2, emp_intern (env, name), fn);
}

#endif /* EMACS_MODULE_PRELUDE_H */
