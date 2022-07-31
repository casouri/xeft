#include <string>
#include <cstring>
#include <iostream>
#include <fstream>
#include <vector>
#include <exception>
#include <iterator>
#include <cstdarg>

#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <xapian.h>

#include "emacs-module.h"
#include "emacs-module-prelude.h"

using namespace std;

int plugin_is_GPL_compatible;

#if defined __cplusplus && __cplusplus >= 201103L
# define EMACS_NOEXCEPT noexcept
#else
# define EMACS_NOEXCEPT
#endif

#define CHECK_EXIT(env) \
  if (env->non_local_exit_check (env) \
      != emacs_funcall_exit_return)   \
    { return NULL; }

/* A few notes: The database we use, WritableDatabase, will not throw
   DatabaseModifiedError, so we don’t need to handle that. For query,
   we first try to parse it with special syntax enabled, i.e., with
   AND, OR, +/-, etc. If that doesn’t parse, we’ll just parse it as
   plain text.

   REF: https://lists.xapian.org/pipermail/xapian-discuss/2021-August/009906.html
 */ 

/*** Xapian stuff */

static const Xapian::valueno DOC_MTIME = 0;
static const Xapian::valueno DOC_FILEPATH = 1;

static Xapian::WritableDatabase database;
static string cached_dbpath = "";

class xapian_lite_cannot_open_file: public exception {};

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
      throw xapian_lite_cannot_open_file();
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
(string term, string dbpath, int offset, int page_size,
 string lang = "en")
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
  // Partial match (FLAG_PARTIAL) needs the database to expand
  // wildcards.
  parser.set_database(database);

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
  // If the syntax is syntactically wrong, Xapian throws this error.
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

static string
copy_string (emacs_env *env, emacs_value value)
{
  char* char_buffer;
  size_t size;
  if (emp_copy_string_contents (env, value, &char_buffer, &size))
    {
      string str = (string) char_buffer;
      free (char_buffer);
      return str;
    }
  else
    {
      emp_signal_message1 (env, "xapian-lite-error",
              "Error turning lisp string to C++ string");
      return "";
    }
}

static bool
NILP (emacs_env *env, emacs_value val)
{
  return !env->is_not_nil (env, val);
}

static const char* xapian_lite_reindex_file_doc =
  "Refindex file at PATH with database at DBPATH\n"
  "Both paths has to be absolute.  Normally, this function only\n"
  "reindex a file if it has been modified since last indexed,\n"
  "but if FORCE is non-nil, this function will always reindex.\n"
  "Return non-nil if actually reindexed the file, return nil if not.\n"
  "\n"
  "LANG is the language used by the indexer, it tells Xapian how to\n"
  "reduce words to word stems, e.g., apples <-> apple.\n"
  "A full list of possible languages can be found at\n"
  "https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html.\n"
  "By default, LANG is \"en\".\n"
  "\n"
  "(fn PATH DBPATH &optional LANG FORCE)";

static emacs_value
Fxapian_lite_reindex_file
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
  EMACS_NOEXCEPT
{

  // Decode arguments.
  emacs_value lisp_path = args[0];
  emacs_value lisp_dbpath = args[1];

  if (NILP (env, emp_funcall (env, "file-name-absolute-p", 1, lisp_path)))
    {
      emp_signal_message1 (env, "xapian-lite-file-error",
                           "PATH is not a absolute path");
      return NULL;
    }
  if (NILP (env,
            emp_funcall (env, "file-name-absolute-p", 1, lisp_dbpath)))
    {
      emp_signal_message1 (env, "xapian-lite-file-error",
                           "DBPATH is not a absolute path");
      return NULL;
    }

  // Expand "~" in the filename.
  emacs_value lisp_args[] = {lisp_path};
  lisp_path = emp_funcall (env, "expand-file-name", 1, lisp_path);
  lisp_dbpath = emp_funcall (env, "expand-file-name", 1, lisp_dbpath);

  emacs_value lisp_lang = nargs < 3 ? emp_intern (env, "nil") : args[2];
  emacs_value lisp_force = nargs < 4 ? emp_intern (env, "nil") : args[3];
  
  string path = copy_string (env, lisp_path);
  string dbpath = copy_string (env, lisp_dbpath);
  bool force = !NILP (env, lisp_force);
  CHECK_EXIT (env);
  string lang = NILP (env, lisp_lang) ?
    "en" : copy_string (env, lisp_lang);
  CHECK_EXIT (env);
  
  // Do the work.
  bool indexed;
  try
    {
      indexed = reindex_file (path, dbpath, lang, force);
      return indexed ? emp_intern (env, "t") : emp_intern (env, "nil");
    }
  catch (xapian_lite_cannot_open_file &e)
    {
      emp_signal_message1 (env, "xapian-lite-file-error",
                           "Cannot open the file");
      return NULL;
    }
  catch (Xapian::DatabaseCorruptError &e)
    {
      emp_signal_message1 (env, "xapian-lite-database-corrupt-error",
                           e.get_description().c_str());
      return NULL;
    }
  catch (Xapian::DatabaseLockError &e)
    {
      emp_signal_message1 (env, "xapian-lite-database-lock-error",
                           e.get_description().c_str());
      return NULL;
    }
  catch (Xapian::Error &e)
    {
      emp_signal_message1 (env, "xapian-lite-lib-error",
                           e.get_description().c_str());
      return NULL;
    }
  catch (exception &e)
    {
      emp_signal_message1 (env, "xapian-lite-error",
                           "Something went wrong");
      return NULL;
    }
}

static const char *xapian_lite_query_term_doc =
  "Query for TERM in database at DBPATH.\n"
  "Paging is supported by OFFSET and PAGE-SIZE. OFFSET specifies page\n"
  "start, and PAGE-SIZE the size. For example, if a page is 10 entries,\n"
  "OFFSET and PAGE-SIZE would be first 0 and 10, then 10 and 10, and\n"
  "so on.\n"
  "\n"
  "If a file in the result doesn't exist anymore, it is removed from\n"
  "the database, and is not included in the return value.\n"
  "\n"
  "LANG is the language used by the indexer, it tells Xapian how to\n"
  "reduce words to word stems, e.g., apples <-> apple.\n"
  "A full list of possible languages can be found at\n"
  "https://xapian.org/docs/apidoc/html/classXapian_1_1Stem.html.\n"
  "By default, LANG is \"en\".\n"
  "\n"
  "TERM can use common Xapian syntax like AND, OR, and +/-.\n"
  "Specifically, this function supports:\n"
  "\n"
  "    Boolean operators: AND, OR, XOR, NOT\n"
  "    Parenthesized expression: ()\n"
  "    Love/hate terms: +/-\n"
  "    Exact match: \"\"\n"
  "\n"
  "If TERM contains syntactic errors, like \"a AND AND b\",\n"
  "it is treated as a plain term.\n"
  "\n"
  "(fn TERM DBPATH OFFSET PAGE-SIZE &optional LANG)";

static emacs_value
Fxapian_lite_query_term
(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
  EMACS_NOEXCEPT
{
  // Decode arguments.
  emacs_value lisp_term = args[0];
  emacs_value lisp_dbpath = args[1];
  emacs_value lisp_offset = args[2];
  emacs_value lisp_page_size = args[3];

  if (NILP (env,
            emp_funcall (env, "file-name-absolute-p", 1, lisp_dbpath)))
    {
      emp_signal_message1 (env, "xapian-lite-file-error",
                           "DBPATH is not a absolute path");
      return NULL;
    }

  lisp_dbpath = emp_funcall (env, "expand-file-name", 1, lisp_dbpath);

  string term = copy_string (env, lisp_term);
  string dbpath = copy_string (env, lisp_dbpath);
  int offset = env->extract_integer (env, lisp_offset);
  int page_size = env->extract_integer (env, lisp_page_size);
  CHECK_EXIT (env);

  vector<string> result;
  try
    {
      result = query_term (term, dbpath, offset, page_size);
    }
  catch (Xapian::Error &e)
    {
      emp_signal_message1 (env, "xapian-lite-lib-error",
                           e.get_description().c_str());
      return NULL;
    }
  catch (exception &e)
    {
      emp_signal_message1 (env, "xapian-lite-error",
                           "Something went wrong");
      return NULL;
    }

  vector<string>::iterator it;
  emacs_value ret = emp_intern (env, "nil");
  for (it = result.begin(); it != result.end(); it++) {
    ret = emp_funcall (env, "cons", 2,
                   env->make_string
                   (env, it->c_str(), strlen(it->c_str())),
                   ret);
    CHECK_EXIT (env);
  }
  return emp_funcall (env, "reverse", 1, ret);
}

int
emacs_module_init (struct emacs_runtime *ert) EMACS_NOEXCEPT
{
  emacs_env *env = ert->get_environment (ert);

  emp_define_error (env, "xapian-lite-error",
                "Generic xapian-lite error", "error");
  emp_define_error (env, "xapian-lite-lib-error",
                "Xapian library error", "xapian-lite-error");
  emp_define_error (env, "xapian-lite-database-corrupt-error",
                "Xapian library error", "xapian-lite-lib-error");
  emp_define_error (env, "xapian-lite-database-lock-error",
                "Xapian library error", "xapian-lite-lib-error");
  emp_define_error (env, "xapian-lite-file-error",
                "Cannot open file", "xapian-lite-error");

  emp_define_function(env, "xapian-lite-reindex-file", 2, 3,
                  &Fxapian_lite_reindex_file,
                  xapian_lite_reindex_file_doc);
  emp_define_function(env, "xapian-lite-query-term", 4, 4,
                  &Fxapian_lite_query_term,
                  xapian_lite_query_term_doc);

  emp_provide (env, "xapian-lite");

  /* Return 0 to indicate module loaded successfully.  */
  return 0;
}
