
%--------------------------------------------------------%
% Definición de la clase fisabior, a usar en los         %
% informes estadísticos realizados con el paquete de R   %
% fisabior. Basado en el paquete memoir. Puede           %
% customizarse para otras salidas                        %
%--------------------------------------------------------%

\NeedsTeXFormat{LaTeX2e}[2016/03/31]
\newcommand{\classname}{fisabior}
\ProvidesClass{\classname}[2017/01/26: v0.0.1]
\LoadClassWithOptions{memoir}

%--------------------------------------------------------%
% Carga de paquetes necesarios                           %
%--------------------------------------------------------%
\RequirePackage{lipsum}
\RequirePackage[pangram]{blindtext}
\RequirePackage[T1]{fontenc}
\RequirePackage{fontspec}
\RequirePackage{polyglossia}
\setdefaultlanguage{spanish}
\setotherlanguage{english}
\PolyglossiaSetup{spanish}{indentfirst = false}
\RequirePackage{xltxtra,xunicode}
\RequirePackage[multiple]{footmisc}
\RequirePackage{graphicx}
\RequirePackage{multirow}
\RequirePackage{rotating}
\RequirePackage{array}
\RequirePackage{float}
\RequirePackage{tikz,tkz-graph}
\RequirePackage[export]{adjustbox}
\RequirePackage{amsmath, mathtools}
\RequirePackage{booktabs}
\RequirePackage[blocks, auth-sc]{authblk}
\RequirePackage{longtable}
\RequirePackage{lscape}
\RequirePackage{hyperref}
\hypersetup{
    pdfpagemode         = {UseOutlines},
    bookmarks           = true,
    bookmarksopen       = true,
    bookmarksopenlevel  = 0,
    bookmarksnumbered   = true,
    hypertexnames       = false,
    colorlinks,
    xetex,
    linkcolor           = {blue},
    citecolor           = {blue},
    urlcolor            = {blue},
    pdfstartview        = {FitV},
    unicode,
    breaklinks          = true
}
\RequirePackage{caption}
\captionsetup{
    tablename       = Tabla, 
    justification   = centerlast, 
    font            = small, 
    labelfont       = {bf,sc}, 
    labelsep        = period, 
    margin          = 50pt,
    singlelinecheck = false, 
    skip            = 6pt, 
    parskip         = 0pt, 
    textformat      = period
}
\RequirePackage[
backend  = biber, 
style    = apa, 
hyperref = true]{biblatex}
\DeclareLanguageMapping{spanish}{spanish-apa}
\DefineBibliographyStrings{spanish}{
    bibliography = {Referencias Bibliográficas}, 
    andothers    = {et\addabbrvspace al\adddot}
}
\RequirePackage[autostyle = true]{csquotes}

%--------------------------------------------------------%
% Definición de página e interlineado                    %
%--------------------------------------------------------%
\settypeblocksize{245mm}{170mm}{*}
\setulmargins{2cm}{*}{*}
\setlrmargins{2cm}{*}{*}
\setheadfoot{6pt}{30pt}
\setheaderspaces{*}{\onelineskip}{*}
\checkandfixthelayout
\OnehalfSpacing

% Control de líneas viudas o huérfanas
\clubpenalty = 10000
\widowpenalty = 10000
\raggedbottom

%--------------------------------------------------------%
% Página de título                                       %
%--------------------------------------------------------%

\newcommand{\corremail}[2][Autor para correspondencia ]{%
    \gdef\corresponding{{#1}(\href{{mailto:#2}}{{#2}}).}}
\newcommand{\corresponding}{}
\renewcommand\Authands{ \& }

\setlength{\droptitle}{-7em}

\pretitle{\begin{center} \scshape \LARGE}
    \posttitle{\par \end{center} \vskip.5em}

\newcommand{\version}[2][Versión del manuscrito: ]{%
    \gdef\vnumber{{#1}{#2}}}
\newcommand{\vnumber}{}

\predate{\vspace{6pt}\begin{center}\small 
        \vnumber\space (\today)}
    \postdate{\par\end{center}\vspace{-.5cm}}
\date{}

\renewcommand{\abstractnamefont}{\normalfont \scshape \large \bfseries}

\newcommand{\logotitulo}[1]{
    \begin{figure}
        \begin{minipage}[c]{1cm}
            \includegraphics[width = 1.5cm]{#1}
        \end{minipage}
    \end{figure}
}


%--------------------------------------------------------%
% Numeración de secciones                                %
%--------------------------------------------------------%

\counterwithout{section}{chapter}




\makepagestyle{title}
\makeevenfoot{title}{\includegraphics[width = 2cm]{logo-ccby.png}}{\thepage}{ }
\makeoddfoot{title}{\includegraphics[width = 2cm]{logo-ccby.png}}{\thepage}{ }





%--------------------------------------------------------%
% Fin                                                    %
%--------------------------------------------------------%
\endinput