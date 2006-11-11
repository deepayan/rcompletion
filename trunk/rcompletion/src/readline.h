
#if !defined (_READLINE_H_)
#define _READLINE_H_

/* declare essential things if readline headers not found.  Not sure
   if this is a good approach */

#ifdef __cplusplus
extern "C" {
#endif


#if !defined (PARAMS)
#  if defined (__STDC__) || defined (__GNUC__) || defined (__cplusplus)
#    define PARAMS(protos) protos
#  else
#    define PARAMS(protos) ()
#  endif
#endif

typedef char *rl_compentry_func_t PARAMS((const char *, int));
typedef char **rl_completion_func_t PARAMS((const char *, int, int));


extern const char *rl_readline_name;
extern rl_completion_func_t *rl_attempted_completion_function;
extern int rl_attempted_completion_over;
extern int rl_completion_append_character;
extern const char *rl_basic_word_break_characters;
extern /*const*/ char *rl_completer_word_break_characters;
extern char *rl_line_buffer;
extern char **rl_completion_matches PARAMS((const char *, rl_compentry_func_t *));

#ifdef __cplusplus
}
#endif

#endif /* _READLINE_H_ */



