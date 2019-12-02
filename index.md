--- 
title: "{testdown} report"
author: "Colin Fay"
date: "2018-12-06"
site: bookdown::bookdown_site
documentclass: book
biblio-style: apalike
link-citations: yes
description: "{testdown} report for package attempt"
---


# Coverage results for package attempt {-} 

Done on: 2018-12-06 21:35:56 



|file            |context             |test                                     | nb| failed|skipped |error | warning|  user| system|  real|
|:---------------|:-------------------|:----------------------------------------|--:|------:|:-------|:-----|-------:|-----:|------:|-----:|
|test-if.R       |if                  |any, all and none works                  | 18|      0|FALSE   |FALSE |       0| 0.031|  0.004| 0.036|
|test-if.R       |if                  |if_then work                             | 26|      0|FALSE   |FALSE |       0| 0.033|  0.007| 0.040|
|test-if.R       |if                  |if_else work                             |  7|      0|FALSE   |FALSE |       0| 0.008|  0.002| 0.009|
|test-if.R       |if                  |scoped if works                          | 27|      0|FALSE   |FALSE |       0| 0.027|  0.005| 0.032|
|test-trycatch.R |trycatch            |errors catching                          |  4|      0|FALSE   |FALSE |       0| 0.007|  0.001| 0.008|
|test-trycatch.R |trycatch            |warning catching                         |  5|      0|FALSE   |FALSE |       0| 0.014|  0.003| 0.018|
|test-trycatch.R |trycatch            |finally works                            |  2|      0|FALSE   |FALSE |       0| 0.005|  0.001| 0.006|
|test-trycatch.R |trycatch            |trycatch works with an external variabel |  2|      0|FALSE   |FALSE |       0| 0.003|  0.000| 0.003|
|test-trycatch.R |try_catch_df        |try_catch_df works                       | 14|      0|FALSE   |FALSE |       0| 0.024|  0.003| 0.027|
|test-trycatch.R |map_try_catch       |map_try_catch works                      | 20|      0|FALSE   |FALSE |       0| 0.027|  0.004| 0.031|
|test-trycatch.R |attempt             |attempt works                            |  8|      0|FALSE   |FALSE |       0| 0.014|  0.003| 0.016|
|test-trycatch.R |attempt             |attempt and try work the same way        |  3|      0|FALSE   |FALSE |       0| 0.010|  0.002| 0.011|
|test-trycatch.R |adverbs             |silently works                           |  1|      0|FALSE   |FALSE |       0| 0.002|  0.000| 0.002|
|test-trycatch.R |adverbs             |surely works                             |  2|      0|FALSE   |FALSE |       0| 0.003|  0.000| 0.004|
|test-trycatch.R |adverbs             |silent_attempt works                     |  3|      0|FALSE   |FALSE |       0| 0.005|  0.001| 0.005|
|test-trycatch.R |adverbs             |with_* works                             |  6|      0|FALSE   |FALSE |       0| 0.011|  0.001| 0.012|
|test-utils.R    |test-utils.R        |try_catch_builder works                  |  7|      0|FALSE   |FALSE |       0| 0.009|  0.001| 0.011|
|test-warn.R     |test-warn.R         |stop, warn and message works             | 22|      0|FALSE   |FALSE |       0| 0.054|  0.007| 0.062|
|test-warn.R     |test-any-all-none.R |any, all and none works                  | 18|      0|FALSE   |FALSE |       0| 0.033|  0.004| 0.038|


# if


|Test                                                                                                      |Location        |Test.time           |Result  |
|:---------------------------------------------------------------------------------------------------------|:---------------|:-------------------|:-------|
|`expect_message(message_if_all(.l = LETTERS, .p = is.character, msg = This is character only))`           |test-if.R#5:1   |2018-12-06 21:35:55 |success |
|`expect_message(message_if_any(.l = 1:10, .p = function(vec) { is.numeric(vec)}, msg = No numeric here))` |test-if.R#9:1   |2018-12-06 21:35:55 |success |
|`expect_message(message_if_none(.l = letters, .p = function(vec) { is.numeric(vec)}))`                    |test-if.R#14:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_all(.l = 12:14, .p = is.numeric))`                                                  |test-if.R#19:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_all(.l = 12:14, .p = is.numeric, msg = nop))`                                       |test-if.R#21:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_any(.l = 12:14, .p = is.numeric))`                                                  |test-if.R#25:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_any(.l = 12:14, .p = is.numeric, msg = y should be numeric))`                       |test-if.R#27:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_none(.l = 12:14, .p = is.character))`                                               |test-if.R#31:1  |2018-12-06 21:35:55 |success |
|`expect_error(stop_if_none(.l = 12:14, .p = is.character, msg = y should be numeric))`                    |test-if.R#33:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_all(.l = 1:14, .p = is.numeric))`                                                 |test-if.R#37:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_all(.l = 1:14, .p = is.numeric, msg = hey!))`                                     |test-if.R#39:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_any(.l = 1:13, .p = ~.x == 10))`                                                  |test-if.R#43:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_any(.l = 1:13, .p = ~.x == 10, msg = b should be 10))`                            |test-if.R#45:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_none(.l = 20:30, .p = ~.x == 10))`                                                |test-if.R#49:1  |2018-12-06 21:35:55 |success |
|`expect_warning(warn_if_none(.l = 20:30, .p = ~.x == 10, msg = b should be 10))`                          |test-if.R#51:1  |2018-12-06 21:35:55 |success |
|`expect_message(message_if_any(.l = letters, .p = is.character))`                                         |test-if.R#55:1  |2018-12-06 21:35:55 |success |
|`expect_message(message_if_any(.l = letters, .p = is.character, msg = You entered a character vector))`   |test-if.R#57:1  |2018-12-06 21:35:55 |success |
|`expect_message(message_if_all(.l = LETTERS, .p = is.character))`                                         |test-if.R#61:1  |2018-12-06 21:35:55 |success |
|`expect_is(a, character)`                                                                                 |test-if.R#70:1  |2018-12-06 21:35:55 |success |
|`expect_length(a, 1)`                                                                                     |test-if.R#72:1  |2018-12-06 21:35:55 |success |
|`expect_equal(a, lol)`                                                                                    |test-if.R#73:1  |2018-12-06 21:35:55 |success |
|`expect_is(ab, character)`                                                                                |test-if.R#75:1  |2018-12-06 21:35:55 |success |
|`expect_length(ab, 1)`                                                                                    |test-if.R#76:1  |2018-12-06 21:35:55 |success |
|`expect_equal(ab, lol)`                                                                                   |test-if.R#77:1  |2018-12-06 21:35:55 |success |
|`expect_null(ac)`                                                                                         |test-if.R#79:1  |2018-12-06 21:35:55 |success |
|`expect_is(ad, character)`                                                                                |test-if.R#81:1  |2018-12-06 21:35:55 |success |
|`expect_length(ad, 1)`                                                                                    |test-if.R#82:1  |2018-12-06 21:35:55 |success |
|`expect_equal(ad, lol)`                                                                                   |test-if.R#83:1  |2018-12-06 21:35:55 |success |
|`expect_is(a, character)`                                                                                 |test-if.R#85:1  |2018-12-06 21:35:55 |success |
|`expect_length(a, 1)`                                                                                     |test-if.R#86:1  |2018-12-06 21:35:55 |success |
|`expect_equal(a, lol)`                                                                                    |test-if.R#87:1  |2018-12-06 21:35:55 |success |
|`expect_is(af, character)`                                                                                |test-if.R#89:1  |2018-12-06 21:35:55 |success |
|`expect_length(af, 1)`                                                                                    |test-if.R#90:1  |2018-12-06 21:35:55 |success |
|`expect_equal(af, Yay)`                                                                                   |test-if.R#91:1  |2018-12-06 21:35:55 |success |
|`expect_is(g, character)`                                                                                 |test-if.R#93:1  |2018-12-06 21:35:55 |success |
|`expect_length(g, 1)`                                                                                     |test-if.R#94:1  |2018-12-06 21:35:55 |success |
|`expect_equal(g, lol)`                                                                                    |test-if.R#95:1  |2018-12-06 21:35:55 |success |
|`expect_is(h, character)`                                                                                 |test-if.R#97:1  |2018-12-06 21:35:55 |success |
|`expect_length(h, 1)`                                                                                     |test-if.R#98:1  |2018-12-06 21:35:55 |success |
|`expect_equal(h, lol)`                                                                                    |test-if.R#99:1  |2018-12-06 21:35:55 |success |
|`expect_null(i)`                                                                                          |test-if.R#101:1 |2018-12-06 21:35:55 |success |
|`expect_null(j)`                                                                                          |test-if.R#103:1 |2018-12-06 21:35:55 |success |
|`expect_is(k, character)`                                                                                 |test-if.R#105:1 |2018-12-06 21:35:55 |success |
|`expect_null(l)`                                                                                          |test-if.R#107:1 |2018-12-06 21:35:55 |success |
|`expect_equal(a, Yay)`                                                                                    |test-if.R#112:1 |2018-12-06 21:35:55 |success |
|`expect_is(a, character)`                                                                                 |test-if.R#113:1 |2018-12-06 21:35:55 |success |
|`expect_equal(b, Nay)`                                                                                    |test-if.R#115:1 |2018-12-06 21:35:55 |success |
|`expect_is(b, character)`                                                                                 |test-if.R#116:1 |2018-12-06 21:35:55 |success |
|`expect_is(c, character)`                                                                                 |test-if.R#118:1 |2018-12-06 21:35:55 |success |
|`expect_length(c, 1)`                                                                                     |test-if.R#119:1 |2018-12-06 21:35:55 |success |
|`expect_equal(c, lol)`                                                                                    |test-if.R#120:1 |2018-12-06 21:35:55 |success |
|`expect_is(b, character)`                                                                                 |test-if.R#126:1 |2018-12-06 21:35:55 |success |
|`expect_length(b, 10)`                                                                                    |test-if.R#127:1 |2018-12-06 21:35:55 |success |
|`expect_equal(b, letters[1:10])`                                                                          |test-if.R#128:1 |2018-12-06 21:35:55 |success |
|`expect_is(ba, character)`                                                                                |test-if.R#130:1 |2018-12-06 21:35:55 |success |
|`expect_length(ba, 10)`                                                                                   |test-if.R#131:1 |2018-12-06 21:35:55 |success |
|`expect_equal(ba, letters[1:10])`                                                                         |test-if.R#132:1 |2018-12-06 21:35:55 |success |
|`expect_is(bb, character)`                                                                                |test-if.R#134:1 |2018-12-06 21:35:55 |success |
|`expect_length(bb, 10)`                                                                                   |test-if.R#135:1 |2018-12-06 21:35:55 |success |
|`expect_equal(bb, letters[1:10])`                                                                         |test-if.R#136:1 |2018-12-06 21:35:55 |success |
|`expect_is(c, character)`                                                                                 |test-if.R#138:1 |2018-12-06 21:35:55 |success |
|`expect_length(c, 10)`                                                                                    |test-if.R#139:1 |2018-12-06 21:35:55 |success |
|`expect_equal(c, letters[1:10])`                                                                          |test-if.R#140:1 |2018-12-06 21:35:55 |success |
|`expect_is(ca, character)`                                                                                |test-if.R#142:1 |2018-12-06 21:35:55 |success |
|`expect_length(ca, 10)`                                                                                   |test-if.R#143:1 |2018-12-06 21:35:55 |success |
|`expect_equal(ca, letters[1:10])`                                                                         |test-if.R#144:1 |2018-12-06 21:35:55 |success |
|`expect_is(cb, character)`                                                                                |test-if.R#146:1 |2018-12-06 21:35:55 |success |
|`expect_length(cb, 10)`                                                                                   |test-if.R#147:1 |2018-12-06 21:35:55 |success |
|`expect_equal(cb, letters[1:10])`                                                                         |test-if.R#148:1 |2018-12-06 21:35:55 |success |
|`expect_is(d, character)`                                                                                 |test-if.R#150:1 |2018-12-06 21:35:55 |success |
|`expect_length(d, 10)`                                                                                    |test-if.R#151:1 |2018-12-06 21:35:55 |success |
|`expect_equal(d, letters[1:10])`                                                                          |test-if.R#152:1 |2018-12-06 21:35:55 |success |
|`expect_is(e, character)`                                                                                 |test-if.R#154:1 |2018-12-06 21:35:55 |success |
|`expect_length(e, 10)`                                                                                    |test-if.R#155:1 |2018-12-06 21:35:55 |success |
|`expect_equal(e, letters[1:10])`                                                                          |test-if.R#156:1 |2018-12-06 21:35:55 |success |
|`expect_is(f, character)`                                                                                 |test-if.R#158:1 |2018-12-06 21:35:55 |success |
|`expect_length(f, 10)`                                                                                    |test-if.R#159:1 |2018-12-06 21:35:55 |success |
|`expect_equal(f, letters[1:10])`                                                                          |test-if.R#160:1 |2018-12-06 21:35:55 |success |




# trycatch


|Test                                                  |Location             |Test.time           |Result  |
|:-----------------------------------------------------|:--------------------|:-------------------|:-------|
|`expect_is(a, character)`                             |test-trycatch.R#5:1  |2018-12-06 21:35:55 |success |
|`expect_match(a, There was an error:)`                |test-trycatch.R#6:1  |2018-12-06 21:35:55 |success |
|`expect_equal(a, 0)`                                  |test-trycatch.R#8:1  |2018-12-06 21:35:55 |success |
|`expect_equal(a, 12)`                                 |test-trycatch.R#15:1 |2018-12-06 21:35:55 |success |
|`expect_is(a, character)`                             |test-trycatch.R#20:1 |2018-12-06 21:35:55 |success |
|`expect_match(a, There was a warning:)`               |test-trycatch.R#21:1 |2018-12-06 21:35:55 |success |
|`expect_is(a, simpleError)`                           |test-trycatch.R#26:1 |2018-12-06 21:35:55 |success |
|`expect_is(a, error)`                                 |test-trycatch.R#27:1 |2018-12-06 21:35:55 |success |
|`expect_is(a, condition)`                             |test-trycatch.R#28:1 |2018-12-06 21:35:55 |success |
|`expect_null(a)`                                      |test-trycatch.R#33:1 |2018-12-06 21:35:55 |success |
|`expect_output(try_catch(log(1), .f = ~print(a)), a)` |test-trycatch.R#34:1 |2018-12-06 21:35:55 |success |
|`expect_equal(try_catch(log(a), .e = ~.x), 0)`        |test-trycatch.R#39:1 |2018-12-06 21:35:55 |success |
|`expect_is(try_catch(log(a), .e = ~.x), simpleError)` |test-trycatch.R#40:1 |2018-12-06 21:35:55 |success |




# try_catch_df


|Test                                      |Location             |Test.time           |Result  |
|:-----------------------------------------|:--------------------|:-------------------|:-------|
|`expect_is(res_log, tbl_df)`              |test-trycatch.R#47:1 |2018-12-06 21:35:55 |success |
|`expect_is(res_log, tbl)`                 |test-trycatch.R#48:1 |2018-12-06 21:35:55 |success |
|`expect_is(res_log, data.frame)`          |test-trycatch.R#49:1 |2018-12-06 21:35:55 |success |
|`expect_match(res_log$call, log)`         |test-trycatch.R#50:1 |2018-12-06 21:35:55 |success |
|`expect_is(res_log$error, character)`     |test-trycatch.R#51:1 |2018-12-06 21:35:55 |success |
|`expect_is(res_log$value, list)`          |test-trycatch.R#52:1 |2018-12-06 21:35:56 |success |
|`expect_equal(res_log$value[[1]], error)` |test-trycatch.R#53:1 |2018-12-06 21:35:56 |success |
|`expect_is(res_log, tbl_df)`              |test-trycatch.R#55:1 |2018-12-06 21:35:56 |success |
|`expect_is(res_log, tbl)`                 |test-trycatch.R#56:1 |2018-12-06 21:35:56 |success |
|`expect_is(res_log, data.frame)`          |test-trycatch.R#57:1 |2018-12-06 21:35:56 |success |
|`expect_match(res_log$call, log)`         |test-trycatch.R#58:1 |2018-12-06 21:35:56 |success |
|`expect_is(res_log$error, logical)`       |test-trycatch.R#59:1 |2018-12-06 21:35:56 |success |
|`expect_is(res_log$value, list)`          |test-trycatch.R#60:1 |2018-12-06 21:35:56 |success |
|`expect_equal(res_log$value[[1]], 0)`     |test-trycatch.R#61:1 |2018-12-06 21:35:56 |success |




# map_try_catch


|Test                                  |Location             |Test.time           |Result  |
|:-------------------------------------|:--------------------|:-------------------|:-------|
|`expect_is(a, list)`                  |test-trycatch.R#68:1 |2018-12-06 21:35:56 |success |
|`expect_equal(a[[1]], 0)`             |test-trycatch.R#69:1 |2018-12-06 21:35:56 |success |
|`expect_equal(round(a[[2]], 2), 1.1)` |test-trycatch.R#70:1 |2018-12-06 21:35:56 |success |
|`expect_is(a[[3]], error)`            |test-trycatch.R#71:1 |2018-12-06 21:35:56 |success |
|`expect_is(b, tbl_df)`                |test-trycatch.R#73:1 |2018-12-06 21:35:56 |success |
|`expect_is(b, tbl)`                   |test-trycatch.R#74:1 |2018-12-06 21:35:56 |success |
|`expect_is(b, data.frame)`            |test-trycatch.R#75:1 |2018-12-06 21:35:56 |success |
|`expect_equal(nrow(b), 3)`            |test-trycatch.R#76:1 |2018-12-06 21:35:56 |success |
|`expect_equal(ncol(b), 4)`            |test-trycatch.R#77:1 |2018-12-06 21:35:56 |success |
|`expect_match(b$call[1], log)`        |test-trycatch.R#78:1 |2018-12-06 21:35:56 |success |
|`expect_is(b$value, list)`            |test-trycatch.R#80:1 |2018-12-06 21:35:56 |success |
|`expect_equal(b$value[[1]], 0)`       |test-trycatch.R#81:1 |2018-12-06 21:35:56 |success |
|`expect_is(c, tbl_df)`                |test-trycatch.R#83:1 |2018-12-06 21:35:56 |success |
|`expect_is(c, tbl)`                   |test-trycatch.R#84:1 |2018-12-06 21:35:56 |success |
|`expect_is(c, data.frame)`            |test-trycatch.R#85:1 |2018-12-06 21:35:56 |success |
|`expect_equal(nrow(c), 1)`            |test-trycatch.R#86:1 |2018-12-06 21:35:56 |success |
|`expect_equal(ncol(c), 4)`            |test-trycatch.R#87:1 |2018-12-06 21:35:56 |success |
|`expect_true(is.na(c$error[1]))`      |test-trycatch.R#88:1 |2018-12-06 21:35:56 |success |
|`expect_is(c$value, list)`            |test-trycatch.R#89:1 |2018-12-06 21:35:56 |success |
|`expect_equal(c$value[[1]], plop)`    |test-trycatch.R#90:1 |2018-12-06 21:35:56 |success |




# attempt


|Test                                                   |Location              |Test.time           |Result  |
|:------------------------------------------------------|:---------------------|:-------------------|:-------|
|`expect_equal(a, 0)`                                   |test-trycatch.R#97:1  |2018-12-06 21:35:56 |success |
|`expect_is(b, try-error)`                              |test-trycatch.R#99:1  |2018-12-06 21:35:56 |success |
|`expect_match(b, lol)`                                 |test-trycatch.R#100:1 |2018-12-06 21:35:56 |success |
|`expect_is(c, try-error)`                              |test-trycatch.R#102:1 |2018-12-06 21:35:56 |success |
|`expect_match(c, lol)`                                 |test-trycatch.R#103:1 |2018-12-06 21:35:56 |success |
|`expect_is(d, try-error)`                              |test-trycatch.R#105:1 |2018-12-06 21:35:56 |success |
|`expect_is(e, try-error)`                              |test-trycatch.R#107:1 |2018-12-06 21:35:56 |success |
|`expect_is(f, try-error)`                              |test-trycatch.R#109:1 |2018-12-06 21:35:56 |success |
|`expect_equal(class(a), class(b))`                     |test-trycatch.R#116:1 |2018-12-06 21:35:56 |success |
|`expect_equal(length(a), length(b))`                   |test-trycatch.R#117:1 |2018-12-06 21:35:56 |success |
|`expect_equal(attr(a, condition), attr(c, condition))` |test-trycatch.R#118:1 |2018-12-06 21:35:56 |success |




# adverbs


|Test                                                   |Location              |Test.time           |Result  |
|:------------------------------------------------------|:---------------------|:-------------------|:-------|
|`expect_is(a, try-error)`                              |test-trycatch.R#127:1 |2018-12-06 21:35:56 |success |
|`expect_is(b, try-error)`                              |test-trycatch.R#134:1 |2018-12-06 21:35:56 |success |
|`expect_length(b, 1)`                                  |test-trycatch.R#135:1 |2018-12-06 21:35:56 |success |
|`expect_is(silent_attempt(log(1)), NULL)`              |test-trycatch.R#139:1 |2018-12-06 21:35:56 |success |
|`expect_is(a, try-error)`                              |test-trycatch.R#141:1 |2018-12-06 21:35:56 |success |
|`expect_is(b, try-error)`                              |test-trycatch.R#143:1 |2018-12-06 21:35:56 |success |
|`expect_message(as_num_msg(1))`                        |test-trycatch.R#151:1 |2018-12-06 21:35:56 |success |
|`expect_warning(as_num_warn(1))`                       |test-trycatch.R#152:1 |2018-12-06 21:35:56 |success |
|`expect_is(suppressMessages(as_num_msg(1)), numeric)`  |test-trycatch.R#153:1 |2018-12-06 21:35:56 |success |
|`expect_is(suppressWarnings(as_num_warn(1)), numeric)` |test-trycatch.R#154:1 |2018-12-06 21:35:56 |success |
|`expect_message(nowar())`                              |test-trycatch.R#161:1 |2018-12-06 21:35:56 |success |
|`expect_warning(nomess())`                             |test-trycatch.R#162:1 |2018-12-06 21:35:56 |success |




# test-utils.R


|Test                                          |Location          |Test.time           |Result  |
|:---------------------------------------------|:-----------------|:-------------------|:-------|
|`expect_is(a, call)`                          |test-utils.R#5:1  |2018-12-06 21:35:56 |success |
|`expect_match(as.character(a)[1], try_catch)` |test-utils.R#6:1  |2018-12-06 21:35:56 |success |
|`expect_match(as.character(a)[2], log)`       |test-utils.R#7:1  |2018-12-06 21:35:56 |success |
|`expect_is(a, error)`                         |test-utils.R#9:1  |2018-12-06 21:35:56 |success |
|`expect_is(a, call)`                          |test-utils.R#11:1 |2018-12-06 21:35:56 |success |
|`expect_match(as.character(a)[1], try_catch)` |test-utils.R#12:1 |2018-12-06 21:35:56 |success |
|`expect_match(as.character(a)[2], log)`       |test-utils.R#13:1 |2018-12-06 21:35:56 |success |




# test-warn.R


|Test                                                                                                                         |Location         |Test.time           |Result  |
|:----------------------------------------------------------------------------------------------------------------------------|:----------------|:-------------------|:-------|
|`expect_error(stop_if(.x = 12, .p = is.numeric))`                                                                            |test-warn.R#5:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if(.x = 12, .p = is.numeric, msg = plop))`                                                                |test-warn.R#7:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if(., .p = function(x) TRUE, msg = plop))`                                                                |test-warn.R#10:1 |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_not(.x = 20, .p = is.numeric))`                                                                        |test-warn.R#14:1 |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_not(.x = 20, .p = is.numeric, msg = y should be numeric))`                                             |test-warn.R#16:1 |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_not(.p = function() FALSE, msg = plop))`                                                               |test-warn.R#19:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if(.x = this is not numeric, .p = is.character))`                                                       |test-warn.R#23:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if(.x = this is not numeric, .p = is.character, msg = lol))`                                            |test-warn.R#25:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if(.x = TRUE, msg = plop))`                                                                             |test-warn.R#28:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_not(.x = 20, .p = ~.x == 10))`                                                                       |test-warn.R#31:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_not(.x = 20, .p = ~.x == 10, msg = b should be 10))`                                                 |test-warn.R#33:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_not(.x = FALSE, msg = plop))`                                                                        |test-warn.R#36:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if(.x = a, .p = is.character))`                                                                      |test-warn.R#40:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if(.x = a, .p = is.character, msg = You entered a character element))`                               |test-warn.R#42:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if(.p = function(x) TRUE, msg = plop))`                                                              |test-warn.R#45:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if(.x = 100, .p = ~sqrt(.x) < 42, msg = The square root of your element must be more than 42))`      |test-warn.R#49:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if(.x = 100, .p = ~sqrt(.x) < 42))`                                                                  |test-warn.R#52:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_not(.x = 100, .p = ~sqrt(.x) > 42))`                                                              |test-warn.R#54:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_not(.x = 100, .p = ~sqrt(.x) > 42, msg = Your sqrt should be less than 30))`                      |test-warn.R#56:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_not(.x = function() FALSE, msg = plop))`                                                          |test-warn.R#59:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_not(.x = 30, .p = function(vec) { return(vec > 30)}, msg = Your element should be less that 30))` |test-warn.R#62:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_not(.x = 30, .p = function(vec) { return(vec > 30)}))`                                            |test-warn.R#67:1 |2018-12-06 21:35:56 |success |




# test-any-all-none.R


|Test                                                                                                          |Location          |Test.time           |Result  |
|:-------------------------------------------------------------------------------------------------------------|:-----------------|:-------------------|:-------|
|`expect_error(stop_if_all(.l = 12:14, .p = is.numeric))`                                                      |test-warn.R#77:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_all(.l = 12:14, .p = is.numeric, msg = nop))`                                           |test-warn.R#79:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_any(.l = 12:14, .p = is.numeric))`                                                      |test-warn.R#83:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_any(.l = 12:14, .p = is.numeric, msg = y should be numeric))`                           |test-warn.R#85:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_none(.l = 12:14, .p = is.character))`                                                   |test-warn.R#89:1  |2018-12-06 21:35:56 |success |
|`expect_error(stop_if_none(.l = 12:14, .p = is.character, msg = y should be numeric))`                        |test-warn.R#91:1  |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_all(.l = 1:14, .p = is.numeric))`                                                     |test-warn.R#95:1  |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_all(.l = 1:14, .p = is.numeric, msg = hey!))`                                         |test-warn.R#97:1  |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_any(.l = 1:13, .p = ~.x == 10))`                                                      |test-warn.R#101:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_any(.l = 1:13, .p = ~.x == 10, msg = b should be 10))`                                |test-warn.R#103:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_none(.l = 20:30, .p = ~.x == 10))`                                                    |test-warn.R#107:1 |2018-12-06 21:35:56 |success |
|`expect_warning(warn_if_none(.l = 20:30, .p = ~.x == 10, msg = b should be 10))`                              |test-warn.R#109:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_any(.l = letters, .p = is.character))`                                             |test-warn.R#113:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_any(.l = letters, .p = is.character, msg = You entered a character vector))`       |test-warn.R#115:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_all(.l = LETTERS, .p = is.character))`                                             |test-warn.R#119:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_all(.l = LETTERS, .p = is.character, msg = This is character only))`               |test-warn.R#121:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_none(.l = letters, .p = function(vec) { is.numeric(vec)}, msg = No numeric here))` |test-warn.R#125:1 |2018-12-06 21:35:56 |success |
|`expect_message(message_if_none(.l = letters, .p = function(vec) { is.numeric(vec)}))`                        |test-warn.R#130:1 |2018-12-06 21:35:56 |success |

