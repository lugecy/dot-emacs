(defun yas/define-andystewart-bundle ()
  "Initialize YASnippet and load snippets in the bundle."
;;; snippets for text-mode
(yas/define-snippets 'text-mode
'(
  ("time" "`(current-time-string)`" "(current time)" nil nil)
  ("email" "`user-mail-address`" "(user's email)" nil nil)
  )
nil)

;;; snippets for cc-mode
(yas/define-snippets 'cc-mode
'(
  ("swi" "switch(${1:condition}) {
case ${2:condition}:
     $3
     break;
default:
     $0
     break;
}
" "switch (...) { ...}" nil nil)
  ("struct" "struct ${1:name}
{
    $0
};" "struct ... { ... }" nil nil)
  ("pri" "println(\"${1:var}\" + $1);
" "println (...)" nil nil)
  ("once" "#ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
#define $1

$0

#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil)
  ("main" "int main(int argc, char *argv[]) {
     $0
     return 0;
}
" "int main(argc, argv) { ... }" nil nil)
  ("inc," "#include <$1>
" "#include <...>" nil nil)
  ("inc" "#include \"$1\"
" "#include \"...\"" nil nil)
  ("ifeie" "if (${1:condition}) {
     $2
} else if (${3:condition}) {
     $4
} else {
     $0
}         
" "if (...) { ... } else if (...) { ... } else { ... }" nil nil)
  ("ifei" "if (${1:condition}) {
     $2
} else if (${3:condition}) {
     $0
}         
" "if (...) { ... } else if (...) { ... }" nil nil)
  ("ife" "if (${1:condition}) {
     $2
} else {
     $0
}         
" "if (...) { ... } else { ... }" nil nil)
  ("if" "if (${1:condition})
{
    $0
}" "if (...) { ... }" nil nil)
  ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:++i})
{
    $0
}" "for (...; ...; ...) { ... }" nil nil)
  ("elseif" "} else if (${1:condition}) {
     $0
" "else if (...) { ... }" nil nil)
  ("do" "do
{
    $0
} while (${1:condition});" "do { ... } while (...)" nil nil)
  )
'text-mode)

;;; snippets for c++-mode
(yas/define-snippets 'c++-mode
'(
  ("using" "using namespace ${std};
$0" "using namespace ... " nil nil)
  ("ns" "namespace " "namespace ..." nil nil)
  ("class" "class ${1:Name}
{
public:
    $1($2);
    virtual ~$1();
};" "class ... { ... }" nil nil)
  ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil)
  )
'cc-mode)

;;; snippets for c-mode
(yas/define-snippets 'c-mode
'(
  ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");
" "FILE *fp = fopen(..., ...);" nil nil)
  )
'cc-mode)

;;; snippets for css-mode
(yas/define-snippets 'css-mode
'(
  ("pad.top" "padding-top: $1;
" "padding-top: ..." nil nil)
  ("pad.right" "padding-right: $1;
" "padding-right: ..." nil nil)
  ("pad.padding" "padding: ${top} ${right} ${bottom} ${left};
" "padding: top right bottom left" nil nil)
  ("pad.pad" "padding: $1;
" "padding: ..." nil nil)
  ("pad.left" "padding-left: $1;
" "padding-left: ..." nil nil)
  ("pad.bottom" "padding-bottom: $1;
" "padding-bottom: ..." nil nil)
  ("mar.top" "margin-top: $1;
" "margin-top: ..." nil nil)
  ("mar.right" "margin-right: $1;
" "margin-right: ..." nil nil)
  ("mar.margin" "margin: ${top} ${right} ${bottom} ${left};
" "margin top right bottom left" nil nil)
  ("mar.mar" "margin: $1;
" "margin: ..." nil nil)
  ("mar.left" "margin-left: $1;
" "margin-left: ..." nil nil)
  ("mar.bottom" "margin-bottom: $1;
" "margin-bottom: ..." nil nil)
  ("fs" "font-size: ${12px};
" "font-size: ..." nil nil)
  ("ff" "font-family: $1;
" "font-family: ..." nil nil)
  ("disp.none" "dislpay: none;
" "display: none" nil nil)
  ("disp.inline" "dislpay: inline;
" "display: inline" nil nil)
  ("disp.block" "dislpay: block;
" "display: block" nil nil)
  ("cl" "clear: $1;
" "clear: ..." nil nil)
  ("bor" "border: ${1:1px} ${2:solid} #${3:999};" "border size style color" nil nil)
  ("bg.1" "background-image: url($1);" "background-image: ..." nil nil)
  ("bg" "background-color: #${1:DDD};" "background-color: ..." nil nil)
  )
'text-mode)

;;; snippets for erlang-mode
(yas/define-snippets 'erlang-mode
'(
  ("undef" "-undef($1).
$0
" "-undef(...)." nil nil)
  ("try" "try $1 of
    $0
catch
after
end
" "try ... of ... catch after end" nil nil)
  ("rec" "-record($1,{$2}).
$0
" "-record(...,{...})." nil nil)
  ("rcv.after" "receive
after
    $1 -> $0
end
" "receive after ... -> ... end" nil nil)
  ("rcv" "receive
    $1 -> $0
end
" "receive ... -> ... end" nil nil)
  ("mod" "-module(${1:$(file-name-nondirectory 
               (file-name-sans-extension (buffer-file-name)))}).
$0

" "-module()." nil nil)
  ("loop" "${1:loop}($2) ->
    receive
	${3:_} ->
	    $1($2)
    end.
$0
" "loop(...) -> receive _ -> loop(...) end." nil nil)
  ("inc.lib" "-include_lib(\"$1\").
$0
" "-include_lib(\"...\")." nil nil)
  ("inc" "-include(\"$1\").
$0
" "-include(\"...\")." nil nil)
  ("imp" "-import(${1:lists}, [${2:map/2, sum/1}]).
$0
" "-import([])." nil nil)
  ("ifndef" "-ifndef($1).
$0
-endif.
" "-ifndef(...). ... -endif." nil nil)
  ("ifdef" "-ifdef($1).
$0
-endif.
" "-ifdef(...). ... -endif." nil nil)
  ("if" "if
    $1 -> $2;
    true -> $0
end
" "if ... -> ... ; true -> ... end" nil nil)
  ("fun" "fun ($1) -> $0 end
" "fun (...) -> ... end" nil nil)
  ("exp" "-export([${1:start/0}]).
$0
" "-export([])." nil nil)
  ("def" "-define($1,$2).
$0
" "-define(...,...)." nil nil)
  ("compile" "-compile([${1:export_all}]).
$0
" "-compile(...)." nil nil)
  ("case" "case $1 of
    $0
end
" "case ... of ... end" nil nil)
  ("beh" "-behaviour(${1:gen_server}).
$0
" "-behaviour(...)." nil nil)
  ("begin" "begin
    $0
end
" "begin ... end" nil nil)
  ("after" "after
    $1 -> $0
" "after ... ->" nil nil)
  )
'text-mode)

;;; snippets for f90-mode
(yas/define-snippets 'f90-mode
'(
  ("wr" "write (${1:*},${2:*}) $0
" "write (*,*)" nil nil)
  ("su" "subroutine $0
" "subroutine" nil nil)
  ("st" "structure $0
" "structure" nil nil)
  ("re" "read (${1:*},${2:*}) $0
" "read (*,*)" nil nil)
  ("pr" "program ${1:name}
  $0
end program ${1:name}
" "program ... end program ..." nil nil)
  ("pa" "parameter $0
" "parameter" nil nil)
  ("l" "logical $0
" "logical" nil nil)
  ("ir" "implicit real $0
" "implicit real" nil nil)
  ("intr" "intrinsic $0
" "intrinsic" nil nil)
  ("inc" "include $0
" "include" nil nil)
  ("in" "implicit none
" "implicit none" nil nil)
  ("il" "implicit logical $0
" "implicit logical" nil nil)
  ("ii" "implicit integer $0
" "implicit integer " nil nil)
  ("if" "if ( ${1:condition} ) then
   $0
end if
" "if then end if" nil nil)
  ("ich" "implicit character $0
" "implicit character" nil nil)
  ("ic" "implicit complex $0
" "implicit complex" nil nil)
  ("ib" "implicit byte $0
" "implicit byte" nil nil)
  ("eq" "equivalence $0
" "equivalence" nil nil)
  ("dp" "double precision $0
" "double precision" nil nil)
  ("do" "do while (${1:condition})
   $0
end do
" "do while (...) end do" nil nil)
  ("dc" "double complex $0
" "double complex" nil nil)
  ("cx" "complex $0
" "complex" nil nil)
  ("ch" "character $0
" "character" nil nil)
  ("c" "continue $0
" "continue" nil nil)
  ("bd" "block data $0
" "block data" nil nil)
  ("au" "automatic $0 
" "automatic" nil nil)
  )
'text-mode)

;;; snippets for haskell-mode
(yas/define-snippets 'haskell-mode
'(
  ("modwh" "module ${1:module-name} ($0) where" "module ... where" nil nil)
  ("header" "-----------------------------------------------------------------------------
-- |
-- Module      :  ${1:Module Name}
-- Copyright   :  (c) ${2:Andy Stewart} 2008
-- License     :  GPL
-- 
-- Maintainer  :  ${3:lazycat.manatee@gmail.com}
-- Stability   :  experimental
-- Portability :  non-portable (not tested)
--
-- ${4:Simple Describe}
--
-- * Changlelog:
--
--      ${5:Today}
--              First released.
-- 
-- * TODO
--      
--      $6
-- 
-----------------------------------------------------------------------------
" nil nil nil)
  ("addw" "where $0" "where ..." nil nil)
  ("addq" "\\`${1:content}\\` $0
" "`...`" nil nil)
  ("addo" "| otherwise = $0
" "| otherwise = ..." nil nil)
  ("addi" "import $0" "import ..." nil nil)
  ("addg" "| $0" "| ..." nil nil)
  ("adde" "= $0" "= ..." nil nil)
  ("addd" "${1:name} :: $2
$1 $0" "... :: ... -> ... " nil nil)
  ("addc" "{- | $0 -}" "{- | ... -}" nil nil)
  )
'text-mode)

;;; snippets for html-mode
(yas/define-snippets 'html-mode
'(
  ("ul.id" "<ul id=\"$1\">
  $0
</ul>" "<ul id=\"...\">...</ul>" nil nil)
  ("ul.class" "<ul class=\"$1\">
  $0
</ul>" "<ul class=\"...\">...</ul>" nil nil)
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil nil)
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil nil)
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil nil)
  ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil nil)
  ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">
  $0
</table>" "<table ...>...</table>" nil nil)
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil)
  ("span.class" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("script.javascript-src" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script> " nil nil)
  ("script.javascript" "<script type=\"text/javascript\">
  $0
</script>" "<script type=\"text/javascript\">...</script> " nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol.id" "<ol id=\"$1\">
  $0
</ol>" "<ol id=\"...\">...</ol>" nil nil)
  ("ol.class" "<ol class=\"$1\">
  $0
</ol>" "<ol class=\"...\">...</ol>" nil nil)
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil nil)
  ("meta.http-equiv" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil nil)
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil nil)
  ("mailto" "<a href=\"mailto:$1@$2\">$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil)
  ("link.stylesheet-ie" "<!--[if IE]>
<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />
<![endif]-->" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil)
  ("link.stylesheet" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li.class" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil nil)
  ("li" "<li>$1</li>" "<li>...</li>" nil nil)
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil)
  ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("html" "<html>
  $0
</html>
" "<html>...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil nil)
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil nil)
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil nil)
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil nil)
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil nil)
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil nil)
  ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">
  $0
</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil nil)
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil nil)
  ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil nil)
  ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil nil)
  ("doctype" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" "Doctype HTML 4.01 Strict" nil nil)
  ("div.id-class" "<div id=\"$1\" class=\"$2\">
  $0
</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil)
  ("div.id" "<div id=\"$1\">
  $0
</div>" "<div id=\"...\">...</div>" nil nil)
  ("div.class" "<div class=\"$1\">
  $0
</div>" "<div class=\"...\">...</div>" nil nil)
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil)
  ("code.class" "<code class=\"$1\">
  $0
</code>" "<code class=\"...\">...</code>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for latex-mode
(yas/define-snippets 'latex-mode
'(
  ("begin" "
\\begin{${1:environment}}
$0
\\end{$1}
" "\\begin{environment} ... \\end{environment}" nil nil)
  )
'text-mode)

;;; snippets for lisp-mode
(yas/define-snippets 'lisp-mode
'(
  ("rsc" "(run-shell-command $0)" "(run-shell-command ...)" nil nil)
  ("lt" "(let (${1:varlist})
  $0)" "(let ... ...)" nil nil)
  ("evalal" "(eval-after-load ${1:file-name}
  $0)" "(eval-after-load ... ...)" nil nil)
  ("dlhkd" "(dolist (hooked (list
               ${1:mode-list}
               ))
  (add-hook '$0 'hooked))
" "(dolist (hooked (list ...)) (add-hook '... 'hooked))" nil nil)
  ("dlhk" "(dolist (hook (list
               ${1:mode-list}
               ))
  (add-hook hook '$0))" "(dolist (hook (list ... )) (add-hook hook '...))" nil nil)
  ("defvr" "(defvar ${1:variable-name} ${2:variable-varlue}
  \"$0\")" "(defvar ... ... \"...\")" nil nil)
;;   ("deft" "(defun test ()
;;   \"Just for test\"        
;;   (interactive)
;;   $0
;; )
;; " "(defun test () \"Just for test\" (interactive) ...)" nil nil)
  ("defsc" "(define-stumpwm-command ${1:name} ()
  $0)" "(define-stumpwm-command ... ...)" nil nil)
  ("defpr" "(defparameter ${1:name} $0)
" "(defparameter ... ...)" nil nil)
  ("deffu" "(defun ${1:Function Name} ($2)
  \"${3:Function document}\"
  $0)" "(defun ... (...) \"...\" ...)" nil nil)
  ("deffc" "(defface ${1:face}
  ${2:spec}
  \"${3:doc}\"
  $0)
" "(defface ... ... ... ...)" nil nil)
  ("defcm" "(defcustom ${1:symbol} ${2:value}
  \"${3:doc}\"
  $0)
" "(defcustom ... ... ... ...)" nil nil)
  ("defav" "(defadvice ${1:function-name} (${2:args})
  \"${3:advice-document}\"
  ($0)
)" "(defadvice ... ... ...)" nil nil)
  ("defas" "(defalias ${1:symbol} $0)" "(defalias ... ...)" nil nil)
  ("addul" "(unless ${1:conditional}
  $0)" "(unless ... ...)" nil nil)
  ("addtl" "(add-to-list ${1:list-var} $0)" "(add-to-list ... ...)" nil nil)
  ("addt" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$0
" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" nil nil)
  ("addse" "(setf ${1:variable-name} $0)" "(setq ... ...)" nil nil)
  ("addrq" "(require '$0)" "(require '...)" nil nil)
  ("addpr" "(provide '$0)" "(provide '...)" nil nil)
  ("addlt" ";;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;
$0
" ";;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;" nil nil)
  ("addlk" "(define-key ${1:some-mode-map} (kbd \"${2:some-key}\") $0)" "(define-key ... (kbd \"...\") ...)" nil nil)
  ("addip" "(in-package :$0)
" "(in-package :...)" nil nil)
  ("addif" "(if ${1:conditional}
    ${2:then})" "(if ... ... ...)" nil nil)
  ("addic" "((= c ?${1:char}) ($0))" "((= c ...) (...))" nil nil)
  ("addhk" "(add-hook ${1:hook} $0)" "(add-hook ... ...)" nil nil)
  ("addguk" "(global-unset-key (kbd $0))" "(global-unset-key ...)" nil nil)
  ("addgk" "(global-set-key (kbd \"${1:some-key}\") $0)" "(global-set-key (kbd \"...\") ...)" nil nil)
  ("addc" ";;; ### $1 ###
;;; --- $2
$0" ";;; ### ... ### ..." nil nil)
  ("addbku" "(basic-unset-key-list ${1:keymap} '(\"$0\"))" "(basic-unset-key-list ... ...)" nil nil)
  ("addbk" "(basic-set-key-alist $0)" "(basic-set-key-alist ...)" nil nil)
  ("addal" "(autoload '${1:function-name} \"${2:file-name}\" \"${3:document}\" $0)" "(autoload '... \"...\" \"...\" ...)" nil nil)
  )
'text-mode)

;;; snippets for elisp-mode
(yas/define-snippets 'elisp-mode
'(
  ("rmhk" "(remove-hook '${1:Hook} '${2:Function})" "(remove-hook ... ...)" nil nil)
  ("lt" "(let (${1:varlist})
  $0)" "(let ... ...)" nil nil)
  ("lai" "(lambda () (interactive) ($0))" "(lambda () (interactive) (...))" nil nil)
  ("evalal" "(eval-after-load ${1:file-name}
  $0)" "(eval-after-load ... ...)" nil nil)
;;   ("dlhkd" "(dolist (hooked (list
;;                ${1:mode-list}
;;                ))
;;   (add-hook '$0 hooked))
;; " "(dolist (hooked (list ...)) (add-hook '... hooked))" nil nil)
;;   ("dlhk" "(dolist (hook (list
;;                ${1:mode-list}
;;                ))
;;   (add-hook hook '$0))" "(dolist (hook (list ... )) (add-hook hook '...))" nil nil)
  ("dlaa" "(dolist (${1:elt-cons} '(${2:elt-cons-list}))
  (add-to-alist '$0 $1))
 
" "(dolist (... (...)) (add-to-alist '... ...))" nil nil)
 ;;  ("defvras" "(defvar anything-c-source-${1:name}
 ;; '((name . \"${2:describe}\")
 ;;   $0))" "(defvar anything-c-source-* '((name . \"...\") ...))" nil nil)
  ("defvr" "(defvar ${1:variable-name} ${2:variable-varlue}
  \"$0\")" "(defvar ... ... \"...\")" nil nil)
;;   ("deft" "(defun test ()
;;   \"Just for test\"        
;;   (interactive)
;;   $0
;; )
;; " "(defun test () \"Just for test\" (interactive) ...)" nil nil)
  ;; ("defmm" "(define-minor-mode ${1:mode-name}
  ;; \"${2:Document}\"
  ;; :init-value ${3:init-value}
  ;; :lighter \"${4:highlight-name}\"
  ;; :keymap ${5:keymap}
  ;; :group '${6:group})" "(define-minor-mode ... ... ... ... ... ...)" nil nil)
  ("defgp" "(defgroup ${1:Group-Name} ${2:Group-value}
  \"${3:Group-doc}\"
  $0)" "(defgroup ... ... ... ...)" nil nil)
  ("deffui" "(defun ${1:function-name} ($2)
 \"${3:Function documantion}.\"
 (interactive)
 $0)
" "(defun function-name () ... (interactive) ...) " nil nil)
;;   ("deffua" "(defun anything-${1:name} ()
;;  \"${2:document}\"
;;  (interactive)
;;  (anything 'anything-$0))
;; " "(defun anything-* () ... (interactive) (anything 'anything-*))" nil nil)
  ("deffu" "(defun ${1:Function Name} ($2)
  \"${3:Function document}\"
  $0)" "(defun ... (...) \"...\" ...)" nil nil)
  ("deffc" "(defface ${1:face}
  ${2:spec}
  \"${3:doc}\"
  $0)
" "(defface ... ... ... ...)" nil nil)
  ("defcn" "(defconst ${1:symbol} ${2:initvalue}
  \"$0\")" "(defconst ... ... \"...\")" nil nil)
  ("defcm" "(defcustom ${1:symbol} ${2:value}
  \"${3:doc}\"
  $0)" "(defcustom ... ... ... ...)" nil nil)
  ("defav" "(defadvice ${1:function-name} (${2:args})
  \"${3:advice-document}\"
  ($0)
)" "(defadvice ... ... ...)" nil nil)
  ("defas" "(defalias ${1:symbol} $0)" "(defalias ... ...)" nil nil)
;;   ("addusk" "(lazy-unset-key '(\"${1:key-list}\")$0)
;; " "(lazy-unset-key ... ...)" nil nil)
  ("addul" "(unless ${1:conditional}
  $0)" "(unless ... ...)" nil nil)
  ("addtl" "(add-to-list ${1:list-var} $0)" "(add-to-list ... ...)" nil nil)
  ("addt" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
$0
" ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;" nil nil)
;;   ("addsk" "(lazy-set-key $0)
;; " "(lazy-set-key ...)" nil nil)
  ("addse" "(setq ${1:variable-name} $0)" "(setq ... ...)" nil nil)
  ("addrq" "(require '$0)" "(require '...)" nil nil)
  ("addpr" "(provide '$0)" "(provide '...)" nil nil)
  ("addlt" ";;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;
$0
" ";;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;" nil nil)
  ("addlk" "(define-key ${1:some-mode-map} (kbd \"${2:some-key}\") $0)" "(define-key ... (kbd \"...\") ...)" nil nil)
  ("addif" "(if ${1:conditional}
    ${2:then})" "(if ... ... ...)" nil nil)
  ("addic" "((= c ?${1:char}) ($0))" "((= c ...) (...))" nil nil)
  ("addhk" "(add-hook '${1:hook} $0)" "(add-hook ... ...)" nil nil)
  ("addguk" "(global-unset-key (kbd $0))" "(global-unset-key ...)" nil nil)
  ("addgk" "(global-set-key (kbd \"${1:some-key}\") $0)" "(global-set-key (kbd \"...\") ...)" nil nil)
  ("addcll" ";; ${1:Date}
;;  * ${2:Name}:
;;      * $0
;;
" "Add change logs with details" nil nil)
  ("addcl" ";; `(format-time-string \"%Y/%m/%d\")`
;;  * `user-full-name`:
;;      * $0
;;
" "Add change logs" nil nil)
  ("addc" ";;; ### $1 ###
;;; --- $2
$0" ";;; ### ... ### ..." nil nil)
  ("addal" "(autoload '${1:function-name} \"${2:file-name}\" \"${3:document}\" $0)" "(autoload '... \"...\" \"...\" ...)" nil nil)
  ("adda" "(ad-disable-advice '${1:Function} '${2:Class} '${3:Ad-name})" "(ad-disable-advice ...)" nil nil)
  ("ada" "(ad-enable-advice '${1:Function} '${2:Class} '${3:Ad-name})" "(ad-enable-advice  ...)" nil nil)
  )
'text-mode)

;;; snippets for lisp-interaction-mode
(yas/define-snippets 'lisp-interaction-mode nil '(elisp-mode))

;;; snippets for emacs-lisp-mode
(yas/define-snippets 'emacs-lisp-mode nil '(elisp-mode))

;;; snippets for inferior-emacs-lisp-mode
(yas/define-snippets 'inferior-emacs-lisp-mode nil '(elisp-mode))

;;; snippets for markdown-mode
(yas/define-snippets 'markdown-mode
'(
  ("rlink" "[${1:Link Text}][$2] $0
" "Reference Link" nil nil)
  ("rlb" "[${1:Reference}]: ${2:URL} $3
$0
" "Reference Label" nil nil)
  ("rimg" "![${1:Alt Text}][$2] $0
" "Referenced Image" nil nil)
  ("ol" "${1:1}. ${2:Text}
${1:$(number-to-string (1+ (string-to-number text)))}. $0
" "Ordered List" nil nil)
  ("link" "[${1:Link Text}](${2:URL} $3) $0
" "Link" nil nil)
  ("img" "![${1:Alt Text}](${2:URL} $3) $0
" "Image" nil nil)
  ("hr.2" "
*******

$0
" "Horizontal Rule (*)" nil nil)
  ("hr.1" "
----------

$0
" "Horizontal Rule (-)" nil nil)
  ("h6" "###### ${1:Header 6} ######

$0
" "Header 6" nil nil)
  ("h5" "##### ${1:Header 5} #####

$0
" "Header 5" nil nil)
  ("h4" "#### ${1:Header 4} ####

$0
" "Header 4" nil nil)
  ("h3" "### ${1:Header 3} ###

$0
" "Header 3" nil nil)
  ("h2.2" "${1:Header 2}
${1:$(make-string (string-width text) ?\\-)}

$0
" "Header 2 (-)" nil nil)
  ("h2.1" "## ${1:Header 1} ##

$0
" "Header 2 (##)" nil nil)
  ("h1.2" "${1:Header 1}
${1:$(make-string (string-width text) ?\\=)}

$0
" "Header 1 (=)" nil nil)
  ("h1.1" "# ${1:Header 1} #

$0
" "Header 1 (#)" nil nil)
  ("`" "\\`${1:Code}\\` $0
" "Inline Code" nil nil)
  ("__" "**${1:Text}** $0
" "Strong" nil nil)
  ("_" "_${1:Text}_ $0
" "Emphasis" nil nil)
  ("-" "- ${1:Text}
-$0
" "Unordered List" nil nil)
  ("+" "+ ${1:Text}
+$0
" "Unordered List" nil nil)
  )
'text-mode)

;;; snippets for message-mode
(yas/define-snippets 'message-mode
'(
  ("addc" "------------------------------> ${1:Title} start <------------------------------
$0
------------------------------> ${1:Title} end   <------------------------------
" "--------> ... start <----------- /n -----------> ... end <--------------" nil nil)
  ("add-" "  -- Andy
" "-- Andy" nil nil)
  )
'text-mode)

;;; snippets for nxml-mode
(yas/define-snippets 'nxml-mode
'(
  ("ul" "<ul>
  $0
</ul>" "<ul>...</ul>" nil nil)
  ("tr" "<tr>
  $0
</tr>" "<tr>...</tr>" nil nil)
  ("title" "<title>$1</title>" "<title>...</title>" nil nil)
  ("th" "<th$1>$2</th>" "<th>...</th>" nil nil)
  ("td" "<td$1>$2</td>" "<td>...</td>" nil nil)
  ("tag.2l" "<${1:tag}>
  $2
</$1>$0" "<tag> \\n...\\n</tag>" nil nil)
  ("tag.1l" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil)
  ("table" "<table>
  $0
</table>" "<table>...</table>" nil nil)
  ("style" "<style type=\"text/css\" media=\"${1:screen}\">
  $0
</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil)
  ("span" "<span>$1</span>" "<span>...</span>" nil nil)
  ("quote" "<blockquote>
  $1
</blockquote>" "<blockquote>...</blockquote>" nil nil)
  ("pre" "<pre>
  $0
</pre>" "<pre>...</pre>" nil nil)
  ("p" "<p>$1</p>" "<p>...</p>" nil nil)
  ("ol" "<ol>
  $0
</ol>" "<ol>...</ol>" nil nil)
  ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil)
  ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil nil)
  ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link stylesheet=\"...\" />" nil nil)
  ("li" "<li>$1</li>" "<li>...</li>" nil nil)
  ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil)
  ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil)
  ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">
  $0
</html>
" "<html xmlns=\"...\">...</html>" nil nil)
  ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil)
  ("hr" "<hr />
" "<hr />" nil nil)
  ("head" "<head>
  $0
</head>" "<head>...</head>" nil nil)
  ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil nil)
  ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil nil)
  ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil nil)
  ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil nil)
  ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil nil)
  ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil nil)
  ("form" "<form method=\"$1\" action=\"$2\">
  $0
</form>" "<form method=\"...\" action=\"...\"></form>" nil nil)
  ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil nil)
  ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil nil)
  ("doctype" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil nil)
  ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil)
  ("code" "<code>
  $0
</code>" "<code>...</code>" nil nil)
  ("br" "<br />" "<br />" nil nil)
  ("body" "<body$1>
  $0
</body>" "<body>...</body>" nil nil)
  )
'text-mode)

;;; snippets for objc-mode
(yas/define-snippets 'objc-mode
'(
  ("prop" "- (${1:id})${2:foo}
{
    return $2;
}

- (void)set${2:$(capitalize text)}:($1)aValue
{
    [$2 autorelease];
    $2 = [aValue retain];
}
$0" "foo { ... } ; setFoo { ... }" nil nil)
  )
'text-mode)

;;; snippets for org-mode
(yas/define-snippets 'org-mode
'(
  ("sai" "sudo aptitude install $0 -y" "sudo aptitude install ... -y" nil nil)
  )
'text-mode)

;;; snippets for perl-mode
(yas/define-snippets 'perl-mode
'(
  ("xwhile" "${1:expression} while ${2:condition};" "... while ..." nil nil)
  ("xunless" "${1:expression} unless ${2:condition}" "... unless ..." nil nil)
  ("xif" "${1:expression} if ${2:condition}" "... if ..." nil nil)
  ("xfore" "${1:expression} foreach @${2:array};" "... foreach ..." nil nil)
  ("while" "while ($1) {
    $0
}" "while (...) { ... }" nil nil)
  ("unless" "unless ($1) {
    $0
}" "unless (...) { ... }" nil nil)
  ("sub" "sub ${1:function_name} {
    $0
}" "sub ... { ... }" nil nil)
  ("ifee" "if ($1) {
	${2:# body...}
} elsif ($3) {
	${4:# elsif...}
} else {
	${5:# else...}
}" "if, elsif, else ..." nil nil)
  ("ife" "if ($1) {
    $2
} else {
    $3
}" "if (...) { ... } else { ... }" nil nil)
  ("if" "if ($1) {
    $0
}" "if (...) { ... }" nil nil)
  ("fore" "foreach my \\$${1:x} (@${2:array}) {
    ${3:# body...}
}" "foreach ... { ... }" nil nil)
  ("for" "for (my \\$${1:var} = 0; \\$$1 < ${2:expression}; \\$$1++) {
    ${3:# body...}
}" "for (...) { ... }" nil nil)
  ("eval" "eval {
    ${1:# do something risky...}
};
if (\\$@) {
    ${2:# handle failure...}
}" "eval { ... } if ($@) { ... }" nil nil)
  )
'text-mode)

;;; snippets for cperl-mode
(yas/define-snippets 'cperl-mode
'(
  )
'perl-mode)

;;; snippets for python-mode
(yas/define-snippets 'python-mode
'(
  ("while" "while ${condition}:
    $0" "while ... : ..." nil nil)
  ("propsg" "def _set_${1:foo}(self, value):
    self._$1 = value

def _get_$1(self):
    return self._$1

$1 = property(_get_$1, _set_$1)

$0
" "_get_foo ... _set_foo ... foo=property(...)" nil nil)
  ("propg" "def _get_${1:foo}(self):
    return self._$1

$1 = property(_get_$1)

$0
" "_get_foo ... foo=property(...)" nil nil)
  ("ifmain" "if __name__ == '__main__':
    $0" "if __name__ == '__main__': ..." nil nil)
  ("for" "for ${var} in ${collection}:
    $0" "for ... in ... : ..." nil nil)
  ("defm" "def ${1:name}(self, $2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" nil nil nil)
  ("def" "def ${1:name}($2):
    \"\"\"$3
    ${2:$
    (let* ((indent
            (concat \"\\n\" (make-string (current-column) 32)))
           (args
            (mapconcat
             '(lambda (x)
                (if (not (string= (nth 0 x) \"\"))
                    (concat \"- \" (char-to-string 96) (nth 0 x)
                            (char-to-string 96) \":\")))
             (mapcar
              '(lambda (x)
                 (mapcar
                  '(lambda (x)
                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
                  x))
              (mapcar '(lambda (x) (split-string x \"=\"))
                      (split-string text \",\")))
             indent)))
      (if (string= args \"\")
          (make-string 3 34)
        (mapconcat
         'identity
         (list \"\" \"Arguments:\" args (make-string 3 34))
         indent)))
    }
    $0
" nil nil nil)
  ("class" "class ${1:ClassName}(${2:object}):
    \"\"\"$3
    \"\"\"

    def __init__(self, $4):
        \"\"\"$5
        ${4:$
        (let* ((indent
                (concat \"\\n\" (make-string (current-column) 32)))
               (args
                (mapconcat
                 '(lambda (x)
                    (if (not (string= (nth 0 x) \"\"))
                        (concat \"- \" (char-to-string 96) (nth 0 x)
                                (char-to-string 96) \":\")))
                 (mapcar
                  '(lambda (x)
                     (mapcar
                      (lambda (x)
                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))
                  (mapcar '(lambda (x) (split-string x \"=\"))
                          (split-string text \",\")))
                 indent)))
          (if (string= args \"\")
              (make-string 3 34)
            (mapconcat
             'identity
             (list \"\" \"Arguments:\" args (make-string 3 34))
             indent)))
        }
        ${4:$
        (mapconcat
         '(lambda (x)
            (if (not (string= (nth 0 x) \"\"))
                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))
         (mapcar
          '(lambda (x)
             (mapcar
              '(lambda (x)
                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"
                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))
              x))
          (mapcar '(lambda (x) (split-string x \"=\"))
                  (split-string text \",\")))
         (concat \"\\n\" (make-string (current-column) 32)))
        }
        $0
" nil nil nil)
  ("__" "__${init}__" "__...__" nil nil)
  )
'text-mode)

;;; snippets for rst-mode
(yas/define-snippets 'rst-mode
'(
  ("tit" "${1:$(make-string (string-width text) ?\\=)}
${1:Title}
${1:$(make-string (string-width text) ?\\=)}

$0" "Document title" nil nil)
  ("sec" "${1:Section}
${1:$(make-string (string-width text) ?\\-)}

$0" "Section title" nil nil)
  ("chap" "${1:Chapter}
${1:$(make-string (string-width text) ?\\=)}

$0" "Chapter title" nil nil)
  )
'text-mode)

;;; snippets for ruby-mode
(yas/define-snippets 'ruby-mode
'(
  ("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil nil)
  ("y" ":yields: $0" ":yields: arguments (rdoc)" nil nil)
  ("w" "attr_writer :" "attr_writer ..." nil nil)
  ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil nil)
  ("rw" "attr_accessor :" "attr_accessor ..." nil nil)
  ("rreq" "require File.join(File.dirname(__FILE__), $0)" "require File.join(File.dirname(__FILE__), ...)" nil nil)
  ("req" "require \"$0\"" "require \"...\"" nil nil)
  ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil nil)
  ("rb" "#!/usr/bin/ruby -wKU
" "/usr/bin/ruby -wKU" nil nil)
  ("r" "attr_reader :" "attr_reader ..." nil nil)
  ("mm" "def method_missing(method, *args)
  $0
end" "def method_missing ... end" nil nil)
  ("it" "it ${1:should} do
  $0
end
" "it \"...\" do ... end" nil nil)
  ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil nil)
  ("ife" "if ${1:condition}
  $2
else
  $3
end" "if ... else ... end" nil nil)
  ("if" "if ${1:condition}
  $0
end" "if ... end" nil nil)
  ("forin" "for ${1:element} in ${2:collection}
  $0
end" "for ... in ...; ... end" nil nil)
  ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil nil)
  ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil nil)
  ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil nil)
  ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil nil)
  ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil nil)
  ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil nil)
  ("desc" "describe ${1} do
  $0
end
" "describe ... do ... end" nil nil)
  ("deli" "delete_if { |${e} $0 }" "delete_if { |...| ... }" nil nil)
  ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil nil)
  ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil nil)
  ("cls" "class ${Name}
  $0
end" "class ... end" nil nil)
  ("classify" "classify { |${e}| $0 }" "classify { |...| ... }" nil nil)
  ("cla" "class << ${self}
  $0
end" "class << self ... end" nil nil)
  ("case" "case ${1:object}
when ${2:condition}
  $0
end" "case ... end" nil nil)
  ("bm" "Benchmark.bmbm(${1:10}) do |x|
  $0
end" "Benchmark.bmbm(...) do ... end" nil nil)
  ("app" "if __FILE__ == $PROGRAM_NAME
  $0
end" "if __FILE__ == $PROGRAM_NAME ... end" nil nil)
  ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil nil)
  ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil nil)
  ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil nil)
  ("Comp" "include Comparable

def <=> other
  $0
end" "include Comparable; def <=> ... end" nil nil)
  ("=b" "=begin rdoc
  $0
=end" nil nil nil)
  )
'text-mode)

;;; snippets for scala-mode
(yas/define-snippets 'scala-mode
'(
  ("with" "with $0" "with T" nil nil)
  ("whi" "while (${1:condition}) {
  $0
}" "while(cond) { .. }" nil nil)
  ("var.ret" "var ${1:name}: ${2:T} = ${3:obj} $0
" "var name: T = .." nil nil)
  ("var.new" "var ${1:name} = new ${2:obj} $0
" "var name = new .." nil nil)
  ("var" "var ${1:name} = ${2:obj} $0
" "var name = .." nil nil)
  ("val.ret" "val ${1:name}: ${2:T} = ${3:obj} $0
" "val name: T = .." nil nil)
  ("val.new" "val ${1:name} = new ${2:obj} $0" "val name = new .." nil nil)
  ("val" "val ${1:name} = ${2:obj} $0" "val name = .." nil nil)
  ("tup.paren" "(${1:element1}, ${2:element2}) $0" "(element1, element2)" nil nil)
  ("tup.arrow" "${1:element1} -> ${2:element2} $0" "element1 -> element2" nil nil)
  ("try.finally" "try {

} finally {
  $0
}" "try { .. } finally { .. }" nil nil)
  ("try.catch-finally" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
} finally {

}" "try { .. } catch { case e => ..} finally { ..}" nil nil)
  ("try" "try {
  $0
} catch {
  case ${1:e}: ${2:Exception} => 
    ${1:println(\\\"ERROR: \\\" + e) // TODO: handle exception}\\n}
}" "try { .. } catch { case e => ..}" nil nil)
  ("tr.with" "trait ${1:name} with ${2:trait} {
  $0
}" "trait T1 with T2 { .. }" nil nil)
  ("tr.ext-with" "trait ${1:name} extends ${2:class} with ${3:trait} {
  $0
}" "trait T1 extends C with T2 { .. }" nil nil)
  ("tr.ext" "trait ${1:name} extends ${2:class} {
  $0
}" "trait T extends C { .. }" nil nil)
  ("tr" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("throw" "throw new ${1:Exception}(${2:msg}) $0" "throw new Exception" nil nil)
  ("test" "//@Test
def test${1:name} = {
  $0
}" "@Test def testX = ..." nil nil)
  ("suite" "import org.scalatest._

class ${1:name} extends Suite {
  $0
}" "class T extends Suite { .. }" nil nil)
  ("pro.param" "protected[${1:this}] $0" "protected[this]" nil nil)
  ("pro" "protected $0" "protected" nil nil)
  ("pri.param" "private[${1:this}] $0" "private[this]" nil nil)
  ("pri" "private $0" "private" nil nil)
  ("pr.trace" "println(\"${1:obj}: \" + ${1:obj}) $0" "println(\"obj: \" + obj)" nil nil)
  ("pr.string" "println(\"${1:msg}\") $0" "println(\"..\")" nil nil)
  ("pr.simple" "print(${1:obj}) $0" "print(..)" nil nil)
  ("pr.newline" "println(${1:obj}) $0" "println(..)" nil nil)
  ("pac" "package $0" "package .." nil nil)
  ("ob" "object ${1:name} extends ${2:type} $0" "object name extends T" nil nil)
  ("mix" "trait ${1:name} {
  $0
}" "trait T { .. }" nil nil)
  ("match.option" "${1:option} match {
  case None => $0
  case Some(res) => 

}" "option match { case None => .. }" nil nil)
  ("match.can" "${1:option} match {
  case Full(res) => $0

  case Empty => 

  case Failure(msg, _, _) => 

}" "can match { case Full(res) => .. }" nil nil)
  ("match" "${1:cc} match {
  case ${2:pattern} => $0
}" "cc match { .. }" nil nil)
  ("map.new" "Map(${1:key} -> ${2:value}) $0" "Map(key -> value)" nil nil)
  ("map" "map(${1:x} => ${2:body}) $0" "map(x => ..)" nil nil)
  ("main" "def main(args: Array[String]) = {
  $0
}" "def main(args: Array[String]) = { ... }" nil nil)
  ("ls.val-new" "val ${1:l} = List(${2:args}, ${3:args}) $0" "val l = List(..)" nil nil)
  ("ls.new" "List(${1:args}, ${2:args}) $0" "List(..)" nil nil)
  ("isof" "isInstanceOf[${1:type}] $0" "isInstanceOf[T] " nil nil)
  ("intercept" "intercept(classOf[${1:Exception]}) {
  $0
}" "intercept(classOf[T]) { ..}" nil nil)
  ("imp" "import $0" "import .." nil nil)
  ("if.else" "if (${1:condition}) {
  $2
} else {
  $0
}" "if (cond) { .. } else { .. }" nil nil)
  ("if" "if (${1:condition}) {
  $0
}" "if (cond) { .. }" nil nil)
  ("hset.val-new" "val ${1:m} = new HashSet[${2:key}] $0" "val m = new HashSet[K]" nil nil)
  ("hset.new" "new HashSet[${1:key}] $0
" "new HashSet[K]" nil nil)
  ("hmap.val-new" "val ${1:m} = new HashMap[${2:key}, ${3:value}] $0" "val m = new HashMap[K, V]" nil nil)
  ("hmap.new" "new HashMap[${1:key}, ${2:value}] $0" "new HashMap[K, V]" nil nil)
  ("foreach" "foreach(${1:x} => ${2:body}) $0" "foreach(x => ..)" nil nil)
  ("for.multi" "for {
  ${1:x} <- ${2:xs}
  ${3:x} <- ${4:xs}
} {
  yield $0
}" "for {x <- xs \\ y <- ys} { yield }" nil nil)
  ("for.loop" "for (${1:x} <- ${2:xs}) {
  $0
}" "for (x <- xs) { ... }" nil nil)
  ("for.if" "for (${1:x} <- ${2:xs} if ${3:guard}) {
  $0
}" "for (x <- xs if guard) { ... }" nil nil)
  ("for.extract" "${1:x} <- ${2:xs}" "x <- xs" nil nil)
  ("ext" "extends $0" "extends T" nil nil)
  ("expect" "expect(${1:reply}) {
  $0
}" "expect(value) { ..}" nil nil)
  ("doc.scaladoc" "/**
 * ${1:description}
 * $0
 */" "/** ... */" nil nil)
  ("doc.file-scala-api" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")`, LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala api file */" nil nil)
  ("doc.file-scala" "/*                     __                                               *\\
**     ________ ___   / /  ___     Scala $3                               **
**    / __/ __// _ | / /  / _ |    (c) 2005-`(format-time-string \"%Y\")` , LAMP/EPFL             **
**  __\\ \\/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\\*                                                                      */
/** 
 * $0
 * @author ${1:name} 
 * @version ${2:0.1}
 * $Id$
 */" "/** scala file */" nil nil)
  ("doc.file" "/**
 * `(scala-mode-file-doc)`
 * $0
 * @author ${1:name}
 * @version ${2:0.1} 
 */" "/** file name */" nil nil)
  ("doc.def" "/** 
 * `(scala-mode-def-and-args-doc)`
 */ " "/** method name */" nil nil)
  ("doc.class" "/** 
 * `(scala-mode-find-clstrtobj-name-doc)`
 * ${1:description}
 * $0  
 */" "/** cls/trt/obj name */" nil nil)
  ("def.simple" "def ${1:name} = $0" "def f = ..." nil nil)
  ("def.ret-body" "def ${1:name}: ${3:Unit} = {
  $0
}" "def f: R = {...}" nil nil)
  ("def.ret" "def ${1:name}: ${2:Unit} = $0" "def f: R = ..." nil nil)
  ("def.body" "def ${1:name} = {
  $0
}" "def f = {...}" nil nil)
  ("def.arg-ret-body" "def ${1:name}(${2:args}): ${3:Unit} = {
  $0
}" "def f(arg: T): R = {...}" nil nil)
  ("def.arg-ret" "def ${1:name}(${2:args}): ${3:Unit} = $0" "def f(arg: T): R = ..." nil nil)
  ("def.arg-body" "def ${1:name}(${2:args}) = {
  $0
}" "def f(arg: T) = {...}" nil nil)
  ("def.arg" "def ${1:name}(${2:args}) = $0" "def f(arg: T) = ..." nil nil)
  ("cons.nil" "${1:element1} :: Nil $0
" "element1 :: Nil" nil nil)
  ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil)
  ("co" "case object ${1:name} $0" "case object T" nil nil)
  ("clof" "classOf[${1:type}] $0" "classOf[T] " nil nil)
  ("cl.arg" "class ${1:name}(${2:args}) {
  $0
}" "class T(args) { .. }" nil nil)
  ("cl.abs-arg" "abstract class ${1:name}(${2:args}) {
  $0
}" "abstract class T(args) { .. }" nil nil)
  ("cl.abs" "abstract class ${1:name} {
  $0
}" "abstract class T { .. }" nil nil)
  ("cl" "class ${1:name} {
  $0
}" "class T { .. }" nil nil)
  ("cc" "case class ${1:name}(${2:arg}: ${3:type}) $0" "case class T(arg: A)" nil nil)
  ("cast" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("case.match-all" "case _ => $0" "case _ => " nil nil)
  ("case" "case ${1:pattern} => $0" "case pattern => " nil nil)
  ("bang" "${1:actor} ! ${2:message} $0" "actor ! message" nil nil)
  ("at.version" "@version ${1:0.1} $0" "@version number" nil nil)
  ("at.return" "@return ${1:description} $0" "@return description" nil nil)
  ("at.param" "@param ${1:name} ${2:description} $0" "@param name description" nil nil)
  ("at.author" "@author ${1:name} $0" "@author name" nil nil)
  ("ass.true" "assert(true) $0" "assert(true)" nil nil)
  ("ass" "assert(${1:x} === ${2:y}) $0" "assert(x === y)" nil nil)
  ("asof" "asInstanceOf[${1:type}] $0" "asInstanceOf[T] " nil nil)
  ("arr.val-new" "val ${1:arr} = Array[${2:value}](${3:args}) $0" "val a = Array[T](..)" nil nil)
  ("arr.new" "Array[${1:value}](${2:args}) $0" "Array[T](..)" nil nil)
  ("app" "object ${1:name} extends Application {
  $0
}" "object name extends Application" nil nil)
  ("ano" "($1) => ${2:body} $0" "(args) => ..." nil nil)
  ("actor" "val a = actor {
  loop {
    react {
      $0
    }
  }
}" "val a = actor { ..}" nil nil)
  ("act.arg" "def act(${1:arg}: ${2:type}) = {
  loop {
    react {
      $0
    }
  }
}" "def act(arg: T) = { ..}" nil nil)
  ("act" "def act = {
  loop {
    react {
      $0
    }
  }
}" "def act = { ..}" nil nil)
  )
'text-mode)

;;; snippets for sh-mode
(yas/define-snippets 'sh-mode
'(
  ("addal" "alias ${1:alias-name}='$0'
" "alias ...=\"...\"" nil nil)
  )
'text-mode)

;;; snippets for sql-mode
(yas/define-snippets 'sql-mode
'(
  ("references" "REFERENCES ${1:TableName}([${2:ColumnName}])
" "REFERENCES ..." nil nil)
  ("create.1" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] 
(
		$3		$4		= ${5:NULL}		${6:OUTPUT}
)
AS
BEGIN
$0
END
GO
" "create procedure ..." nil nil)
  ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] 
(
		${3:Id}		${4:INT IDENTITY(1,1)}		${5:NOT NULL}
$0
	CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) 
)
GO
" "create table ..." nil nil)
  ("constraint.1" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] FOREIGN KEY ..." nil nil)
  ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) 
" "CONSTRAINT [..] PRIMARY KEY ..." nil nil)
  ("column" "	,	${1:Name}		${2:Type}			${3:NOT NULL}
" ", ColumnName ColumnType NOT NULL..." nil nil)
  )
'text-mode)

;;; snippets for yaoddmuse-mode
(yas/define-snippets 'yaoddmuse-mode
'(
  ("addw" "`yaoddmuse-wikiname`
" "Insert yaoddmuse wiki name" nil nil)
  ("addt" "<${1:tag}>${1:name}</$1>$0
" "<tag>content</tag>" nil nil)
  ("adds" "{{{
        $0
}}}" "{{{source code}}}" nil nil)
  ("addpr" "<pre>
$0
</pre>" "<pre> ... </pre>" nil nil)
  ("addp" "`yaoddmuse-pagename`
" "Insert yaoddmuse page name" nil nil)
  ("addn" "[new:`yaoddmuse-username`:`(format-time-string \"%Y-%m-%d %H:%M\")` UTC]
$0 -- `yaoddmuse-username`
" "[new:UTC time] -- UserName" nil nil)
  ("addm" "-- `yaoddmuse-username`
" "-- yaoddmuse-username" nil nil)
  ("addl" "Lisp:${1:FileName}.el$0
" "Lisp:file.el" nil nil)
  ("addii" "Here is a screenshot: $0" "Here is a screenshot: ImageName" nil nil)
  ("addi" "[[image:${1:PageName}]]$0
" "[[image:ImagePage]]" nil nil)
  ("addh" "=${1:=}${2:name}=$1$0
" "==Heading Name==" nil nil)
  ("addee" "==What is `yaoddmuse-pagename` ?==
`yaoddmuse-pagename` is a mode that ${1:Describe}.

==Why the need for `yaoddmuse-pagename` ?==
${2:Why}.

==Install==
Put Lisp:${3:package-name}.el in your load-path, add
{{{
    (require '$3)
}}}    
in ~/.emacs

==Usage==
${4:Usage}

==Customize==
All below option can customize by: M-x customize-group RET $3 RET

${5:Customize}
" "Add entire yaoddmuse template code." nil nil)
  ("adde" "==What is `yaoddmuse-pagename` ?==
`yaoddmuse-pagename` is a mode that ${1:Describe}.

==Install==
Put Lisp:${2:package-name}.el in your load-path, add
{{{
    (require '$2)
}}}    
in ~/.emacs

==Usage==
${3:Usage}

==Customize==
All below option can customize by: M-x customize-group RET $2 RET

${4:Customize}
" "Add entire yaoddmuse template code." nil nil)
  ("addd" "[new]
$0        
[new]
" "[new] dialog [new]" nil nil)
  ("addc" "/${1:name}/$0
" "/italic/" nil nil)
  ("addbb" "''${1:name}''$0
" "''Bold''" nil nil)
  ("addb" "*${1:*}${2:name}*$1$0
" "*Bold*" nil nil)
  ("add_" "_${1:name}_$0
" "_underline-" nil nil)
  ("[[" "[[${1:name}]]$0
" "[[name]]" nil nil)
  ("[" "[${1:url} ${2:name}]$0
" "[url name]" nil nil)
  )
'text-mode)

)

;; (yas/initialize-bundle)
(yas/define-andystewart-bundle)
(provide 'yas-andy-bundle)
