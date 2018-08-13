# Punctuation dictionary
dict_puncts = rbind(
  c("-", " mark_minus ", "between_words"),
  c("_", " mark_underscore ", "between_words"),
  c("<", " mark_lessthan ", "special"),
  c(">", " mark_morethan ", "special"),
  c("?", " mark_question ", "regexp"),
  c("*", " mark_star ", "regexp"),
  c("+", " mark_plus ", "regexp"),
  c(".", " mark_period ", "regexp"),
  c("^", " mark_caret ", "regexp"),
  c("$", " mark_dollar " ,"regexp"),
  c("(", " mark_leftparenthesis ", "regexp"),
  c("[", " mark_leftbracket ", "regexp"),
  c("{", " mark_leftcurlybracket ", "regexp"),
  c(")", " mark_rightparenthesis ", "no"),
  c("]", " mark_rightbracket ", "no"),
  c("}", " mark_rightcurlybracket ", "no"),
  c("!", " mark_exclamation ", "no"),
  c("&", " mark_and ", "no"),
  c("|", " mark_pipe ", "no"),
  c(",", " mark_comma ", "no"),
  c(":", " mark_colon ", "no"),
  c(";", " mark_semicolon ", "no"),
  c("@", " mark_at ", "no"),
  c("#", " mark_pound ", "no"),
  c("%", " mark_percentage ", "no"),
  c("'", " mark_singlequote ", "no"),
  c('"', " mark_doublequote ", "no"),
  c("`", " mark_backquote ", "no"),
  c("~", " mark_tilde ", "no"),
  c("/", " mark-fwslash ", "no"),
  c("=", " mark_equal ", "no")
)
dict_puncts = as.data.frame(dict_puncts, stringsAsFactors = FALSE)
colnames(dict_puncts) = c("punct","replacement", "type")
most = paste("\\", dict_puncts$punct, "+", sep = '')
special = paste(dict_puncts$punct, "+", sep = '')
dict_puncts$pattern_multiple = ifelse(dict_puncts$type == "special", special, most)
most = paste("\\", dict_puncts$punct, sep = '')
special = paste(dict_puncts$punct, sep = '')
between = paste("\\W", dict_puncts$punct, "|", dict_puncts$punct,"\\W", sep = '')
dict_puncts$pattern = ifelse(dict_puncts$type == "special", special, ifelse(dict_puncts$type == "between_words", between, most))

# Parameters for DocumentTermMatrix()
space_tokenizer = function(doc) {  # Default tokenizer
  unlist(strsplit(as.character(doc), "[[:space:]]+"))
}

ngram_tokenizer = function(x) {  # ngram tokenizer
  ngramTokenizer(x, n = 1, split = " ")
}
