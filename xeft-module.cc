#include <string>
#include <cstring>
#include <iostream>
#include <fstream>
#include <vector>
#include <exception>
#include <iterator>

#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <xapian.h>

#include "emacs-module.h"

using namespace std;

int plugin_is_GPL_compatible;

#if defined __cplusplus && __cplusplus >= 201103L
# define EMACS_NOEXCEPT noexcept
#else
# define EMACS_NOEXCEPT
#endif

/*** Xapian stuff */

static const Xapian::valueno DOC_MTIME = 0;
static const Xapian::valueno DOC_FILEPATH = 1;

static Xapian::WritableDatabase database;
static string cached_dbpath = "";

class xeft_cannot_open_file: public exception {};

// Reindex the file at PATH, using database at DBPATH. Throws
// cannot_open_file. Both path must be absolute. Normally only reindex
// if file has change since last index, if FORCE is true, always
// reindex. Return true if re-indexed, return false if didn’t.
// LANG is the language used by the stemmer.
// Possible langauges:
// https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html
static bool
reindex_file
(string path, string dbpath, string lang = "en", bool force = false)
{
  // Check for mtime.
  struct stat st;
  time_t file_mtime;
  off_t file_size;
  if (stat (path.c_str(), &st) == 0)
    {
      file_mtime = st.st_mtime;
      file_size = st.st_size;
    }
  else
    {
      throw xeft_cannot_open_file();
    }

  // Even though the document says that database object only carries a
  // pointer to the actual object, it is still not cheap enough. By
  // using this cache, we get much better performance when reindexing
  // hundreds of files, which most are no-op because they hasn’t been
  // modified.
  if (dbpath != cached_dbpath)
    {
      database = Xapian::WritableDatabase
        (dbpath, Xapian::DB_CREATE_OR_OPEN);
      cached_dbpath = dbpath;
    }
  // Track doc with file path as "id". See
  // https://getting-started-with-xapian.readthedocs.io/en/latest/practical_example/indexing/updating_the_database.html
  string termID = 'Q' + path;
  Xapian::PostingIterator it_begin = database.postlist_begin (termID);
  Xapian::PostingIterator it_end = database.postlist_end (termID);
  bool has_doc = it_begin != it_end;
  time_t db_mtime;
  if (has_doc)
    {
      // sortable_serialise is for double and we can’t really use it.
      Xapian::Document db_doc = database.get_document(*it_begin);
      db_mtime = (time_t) stoi (db_doc.get_value (DOC_MTIME));
    }

  // Need re-index.
  if (!has_doc || (has_doc && db_mtime < file_mtime) || force)
    {
      // Get the file content.
      // REF: https://stackoverflow.com/questions/2912520/read-file-contents-into-a-string-in-c
      ifstream infile (path);
      string content ((istreambuf_iterator<char>(infile)),
                      (istreambuf_iterator<char>()));
      // Create the indexer.
      Xapian::TermGenerator indexer;
      Xapian::Stem stemmer (lang);
      indexer.set_stemmer (stemmer);
      indexer.set_stemming_strategy
        (Xapian::TermGenerator::STEM_SOME);
      // Support CJK.
      indexer.set_flags (Xapian::TermGenerator::FLAG_CJK_NGRAM);
      // Index file content.
      Xapian::Document new_doc;
      indexer.set_document (new_doc);
      indexer.index_text (content);
      // Set doc info.
      new_doc.add_boolean_term (termID);
      // We store the path in value, no need to use set_data.
      new_doc.add_value (DOC_FILEPATH, path);
      new_doc.add_value (DOC_MTIME, (string) to_string (file_mtime));
      database.replace_document (termID, new_doc);
      return true;
    }
  else
    {
      return false;
    }
}

// Query TERM in the databse at DBPATH. OFFSET and PAGE_SIZE is for
// paging, see the docstring for the lisp function. If a file in the
// result doesn’t exist anymore, it is removed from the database.
// LANG is the language used by the stemmer.
// Possible langauges:
// https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html
static vector<string>
query_term
(string term, string dbpath, int offset, int page_size, string lang = "en")
{
  // See reindex_file for the reason for caching the database object.
  if (dbpath != cached_dbpath)
    {
      database = Xapian::WritableDatabase
        (dbpath, Xapian::DB_CREATE_OR_OPEN);
      cached_dbpath = dbpath;
    }

  Xapian::QueryParser parser;
  Xapian::Stem stemmer (lang);
  parser.set_stemmer (stemmer);
  parser.set_stemming_strategy (Xapian::QueryParser::STEM_SOME);

  Xapian::Query query;
  try
    {
      query = parser.parse_query
        // CJK_NGRAM is the flag for CJK support. PARTIAL makes
        // interactive search more stable. DEFAULT enables AND OR and
        // +/-.
        (term, Xapian::QueryParser::FLAG_CJK_NGRAM
         | Xapian::QueryParser::FLAG_PARTIAL
         | Xapian::QueryParser::FLAG_DEFAULT);
    }
  // If the syntax is wrong (xxx AND xxx), Xapian throws this error.
  // Try again without enabling any special syntax.
  catch (Xapian::QueryParserError &e)
    {
      query = parser.parse_query
        (term, Xapian::QueryParser::FLAG_CJK_NGRAM
         | Xapian::QueryParser::FLAG_PARTIAL);
    }
  
  Xapian::Enquire enquire (database);
  enquire.set_query (query);

  Xapian::MSet mset = enquire.get_mset (offset, page_size);
  vector<string> result (0);
  for (Xapian::MSetIterator it = mset.begin(); it != mset.end(); it++)
    {
      Xapian::Document doc = it.get_document();
      string path = doc.get_value(DOC_FILEPATH);
      // If the file doesn’t exists anymore, remove it.
      struct stat st;
      if (stat (path.c_str(), &st) == 0)
        {
          result.push_back (doc.get_value (DOC_FILEPATH));
        }
      else
        {
          database.delete_document (doc.get_docid());
        }
    }
  return result;
}

/*** Module definition */

/**** Convenient functions */

static bool
copy_string_contents
(emacs_env *env, emacs_value value, char **buffer, size_t *size)
{
  ptrdiff_t buffer_size;
  if (!env->copy_string_contents (env, value, NULL, &buffer_size))
    return false;
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size > 0);
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
  assert (env->non_local_exit_check (env) == emacs_funcall_exit_return);
  assert (buffer_size == old_buffer_size);
  *size = (size_t) (buffer_size - 1);
  return true;
}

static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);

  emacs_value args[] = {Qsym, Sfun};
  env->funcall (env, Qfset, 2, args);
}

static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

static emacs_value
nil (emacs_env *env) {
  return env->intern (env, "nil");
}

static emacs_value
cons (emacs_env *env, emacs_value car, emacs_value cdr) {
  emacs_value args[] = {car, cdr};
  return env->funcall (env, env->intern(env, "cons"), 2, args);
}

static void
signal (emacs_env *env, const char *name, const char *message)
{
  env->non_local_exit_signal
    (env, env->intern (env, name),
     cons (env, env->make_string (env, message, strlen (message)),
           nil (env)));
}

static string
copy_string (emacs_env *env, emacs_value value)
{
  char* char_buffer;
  size_t size;
  if (copy_string_contents (env, value, &char_buffer, &size))
    {
      string str = (string) char_buffer;
      return str;
    }
  else
    {
      signal (env, "xeft-error",
              "Error turning lisp string to C++ string");
      return "";
    }
}

void
define_error
(emacs_env *env, const char *name,
 const char *description, const char *parent)
{
  emacs_value args[] = {
    env->intern (env, name),
    env->make_string (env, description, strlen (description)),
    env->intern (env, parent)
  };
  env->funcall (env, env->intern (env, "define-error"), 3, args);
}

emacs_value
file_name_absolute_p (emacs_env *env, emacs_value path)
{
  emacs_value args[] = {path};
  return env->funcall
    (env, env->intern (env, "file-name-absolute-p"), 1, args);
}

bool
nilp (emacs_env *env, emacs_value val)
{
  return !env->is_not_nil (env, val);
}

static const char* xeft_reindex_file_doc =
  "Refindex file at PATH with database at DBPATH"
  "Both paths has to be absolute.  Normally, this function only"
  "reindex a file if it has been modified since last indexed,"
  "but if FORCE is non-nil, this function will always reindex."
  "Return non-nil if actually reindexed the file, return nil if not."
  ""
  "LANG is the language used by the indexer, it tells Xapian how to"
  "reduce words to stems and vice versa, e.g., apples <-> apple."
  "A full list of possible languages can be found at"
  "https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html."
  "By default, LANG is \"en\"."
  ""
  "\(PATH DBPATH &optional LANG FORCE)";

static emacs_value
Fxeft_reindex_file
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  
  emacs_value lisp_path = args[0];
  emacs_value lisp_dbpath = args[1];

  if (nilp (env, file_name_absolute_p (env, lisp_path)))
    {
      signal (env, "xeft-file-error", "PATH is not a absolute path");
    }
  if (nilp (env, file_name_absolute_p (env, lisp_dbpath)))
    {
      signal (env, "xeft-file-error", "DBPATH is not a absolute path");
    }

  // Expand "~" in the filename.
  emacs_value lisp_args[] = {lisp_path};
  lisp_path = env->funcall
    (env, env->intern (env, "expand-file-name"), 1, lisp_args);
  lisp_args[0] = lisp_dbpath;
  lisp_dbpath = env->funcall
    (env, env->intern (env, "expand-file-name"), 1, lisp_args);

  emacs_value lisp_lang = nargs < 3 ? nil (env) : args[2];
  emacs_value lisp_force = nargs < 4 ? nil (env) : args[3];
  
  string path = copy_string (env, lisp_path);
  string dbpath = copy_string (env, lisp_dbpath);
  bool force = !nilp (env, lisp_force);
  string lang = nilp (env, lisp_lang) ?
    "en" : copy_string (env, lisp_lang);

  bool indexed;
  try
    {
      indexed = reindex_file (path, dbpath, lang, force);
    }
  catch (xeft_cannot_open_file &e)
    {
      signal (env, "xeft-file-error", "Cannot open the file");
    }
  catch (Xapian::Error &e)
    {
      signal (env, "xeft-xapian-error", e.get_description().c_str());
    }
  catch (exception &e)
    {
      signal (env, "xeft-error", "Something went wrong");
    }
  
  return indexed ? env->intern (env, "t") : nil (env);
}

static const char *xeft_query_term_doc =
  "Query for TERM in database at DBPATH."
  "Paging is supported by OFFSET and PAGE-SIZE. OFFSET specifies page"
  "start, and PAGE-SIZE the size. For example, if a page is 10 entries,"
  "OFFSET and PAGE-SIZE would be first 0 and 10, then 10 and 10, and"
  "so on."
  ""
  "If a file in the result doesn't exist anymore, it is removed from"
  "the database, and not included in the return value."
  ""
  "LANG is the language used by the indexer, it tells Xapian how to"
  "reduce words to stems and vice versa, e.g., apples <-> apple."
  "A full list of possible languages can be found at"
  "https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html."
  "By default, LANG is \"en\"."
  ""
  "\(TERM DBPATH OFFSET PAGE-SIZE &optional LANG)";

static emacs_value
Fxeft_query_term
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
  emacs_value lisp_term = args[0];
  emacs_value lisp_dbpath = args[1];
  emacs_value lisp_offset = args[2];
  emacs_value lisp_page_size = args[3];

  if (nilp (env, file_name_absolute_p (env, lisp_dbpath)))
    {
      signal (env, "xeft-file-error", "DBPATH is not a absolute path");
    }

  emacs_value lisp_args[] = {lisp_dbpath};
  lisp_dbpath = env->funcall
    (env, env->intern (env, "expand-file-name"), 1, lisp_args);

  string term = copy_string (env, lisp_term);
  string dbpath = copy_string (env, lisp_dbpath);
  int offset = env->extract_integer (env, lisp_offset);
  int page_size = env->extract_integer (env, lisp_page_size);

  vector<string> result;
  try
    {
      result = query_term (term, dbpath, offset, page_size);
    }
  catch (Xapian::Error &e)
    {
      signal (env, "xeft-xapian-error", e.get_description().c_str());
    }
  catch (exception &e)
    {
      signal (env, "xeft-error", "Something went wrong");
    }

  vector<string>::iterator it;
  emacs_value ret = nil (env);
  for (it = result.begin(); it != result.end(); it++) {
    ret = cons (env, env->make_string(env, it->c_str(),
                                     strlen(it->c_str())),
                ret);
  }

  return env->funcall (env, env->intern (env, "reverse"), 1, &ret);
}

int
emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  emacs_env *env = ert->get_environment (ert);

  define_error (env, "xeft-error", "Generic xeft error", "error");
  define_error (env, "xeft-xapian-error", "Xapian error", "xeft-error");
  define_error (env, "xeft-file-error", "Cannot open file", "xeft-error");

  bind_function (env, "xeft-reindex-file",
                 env->make_function
                 (env, 2, 3, &Fxeft_reindex_file,
                  xeft_reindex_file_doc, NULL));

  bind_function (env, "xeft-query-term",
                 env->make_function
                 (env, 4, 4, &Fxeft_query_term,
                  xeft_query_term_doc, NULL));

  provide (env, "xeft-module");

  /* Return 0 to indicate module loaded successfully.  */
  return 0;
}
