
%--------------------------------------------------------%
% Definición de la clase fisabior, a usar en los         %
% informes estadísticos realizados con el paquete de R   %
% fisabior. Basado en el paquete memoir. Puede           %
% customizarse para otras salidas                        %
%--------------------------------------------------------%

\NeedsTeXFormat{LaTeX2e}[2016/03/31]
\newcommand{\classname}{fisabior}
\ProvidesClass{\classname}[2017/01/31: v0.0.1]
\LoadClassWithOptions{article}

%--------------------------------------------------------%
% Carga de paquetes necesarios                           %
%--------------------------------------------------------%
\RequirePackage[margin = 2cm, bottom = 2.5cm]{geometry}
\RequirePackage[T1]{fontenc}
\RequirePackage{
    fontspec,
    polyglossia,
    xltxtra,
    xunicode,
    lmodern,
    graphicx,
    multirow,
    rotating,
    array,
    float,
    tikz,
    tkz-graph,
    amsmath,
    mathtools,
    amssymb,
    nag,
    booktabs,
    longtable,
    parskip,
    caption,
    fancyhdr,
    lastpage,
    stackengine,
    titling,
    microtype,
    color,    % R-Markdown
    fancyvrb, % R-Markdown
    framed,   % R-Markdown
    hyperref,
    letltxmacro
}
\RequirePackage[export]{adjustbox}
\RequirePackage[capitalize]{cleveref}
\RequirePackage[autostyle = true]{csquotes}
\RequirePackage[backend = biber, style = apa, hyperref = true]{biblatex}
\RequirePackage[noblocks]{authblk}


%--------------------------------------------------------%
% Definición de página de título                         %
%--------------------------------------------------------%
\renewcommand{\Affilfont}{\small}
\renewcommand{\Authands}{ \& }
\newcommand{\logotitulo}[1]{
    \begin{figure}
        \includegraphics[width = 1.5cm]{#1}
    \end{figure}
}
\newcommand{\corresmail}[2][Autor para correspondencia (]{%
    \gdef\corresponding{{#1}{\href{mailto:#2}{#2}) } } }
\newcommand{\version}[2][Versión del manuscrito: ]{%
    \gdef\vnumber{{#1}{#2}}}
\newcommand{\corresponding}{}
\newcommand{\vnumber}{}
\pretitle{
    \begin{center}
        \scshape \LARGE
    }
\posttitle{\par \end{center} }
\setlength{\droptitle}{-6.8em}
\predate{\vskip -.5 cm}
\date{}
\postdate{}


%--------------------------------------------------------%
% Encabezados y pies de página                           %
%--------------------------------------------------------%
\setstackEOL{\\}
\pagestyle{fancy}
\renewcommand{\headrule}{}
\renewcommand{\footrule}{}
\fancyhead{}
\fancyfoot[L]{}
\fancyfoot[R]{}
\fancyfoot[C]{\scriptsize Página \thepage \space de \pageref*{LastPage}}
\fancypagestyle{plain}{
    \fancyhead{}
    \fancyfoot[L]{
        \scriptsize\vnumber \\ Compilado el \today
    }
    \fancyfoot[C]{
        \raisebox{-.6\height}{
            \scriptsize Página \thepage \space de \pageref*{LastPage}
        }
    }
    \fancyfoot[R]{
        \raisebox{-.6\height}{
            \href{https://creativecommons.org/licenses/by/4.0/}{\includegraphics[width = 2cm]{../../figuras/logo-ccby.png}}
        }
    }
}


%--------------------------------------------------------%
% Referencias a imágenes                                 %
%--------------------------------------------------------%
\setlength{\XeTeXLinkMargin}{1pt}
\LetLtxMacro\SavedIncludeGraphics\includegraphics
\def\includegraphics#1#{% #1 catches optional stuff (star/opt. arg.)
    \IncludeGraphicsAux{#1}%
}%
\newcommand*{\IncludeGraphicsAux}[2]{%
    \XeTeXLinkBox{%
        \SavedIncludeGraphics#1{#2}%
    }%
}
\providecommand{\tightlist}{\setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}


%--------------------------------------------------------%
% Código incrustado para R-Markdown                      %
%--------------------------------------------------------%
\newcommand{\VerbBar}{|}
\newcommand{\VERB}{\Verb[commandchars=\\\{\}]}
\DefineVerbatimEnvironment{Highlighting}{Verbatim}{commandchars=\\\{\}}
% Add ',fontsize=\small' for more characters per line
\definecolor{shadecolor}{RGB}{248,248,248}
\newenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
\newcommand{\KeywordTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{\textbf{{#1}}}}
\newcommand{\DataTypeTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{{#1}}}
\newcommand{\DecValTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{{#1}}}
\newcommand{\BaseNTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{{#1}}}
\newcommand{\FloatTok}[1]{\textcolor[rgb]{0.00,0.00,0.81}{{#1}}}
\newcommand{\ConstantTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{{#1}}}
\newcommand{\CharTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{{#1}}}
\newcommand{\SpecialCharTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{{#1}}}
\newcommand{\StringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{{#1}}}
\newcommand{\VerbatimStringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{{#1}}}
\newcommand{\SpecialStringTok}[1]{\textcolor[rgb]{0.31,0.60,0.02}{{#1}}}
\newcommand{\ImportTok}[1]{{#1}}
\newcommand{\CommentTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textit{{#1}}}}
\newcommand{\DocumentationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{{#1}}}}}
\newcommand{\AnnotationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{{#1}}}}}
\newcommand{\CommentVarTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{{#1}}}}}
\newcommand{\OtherTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{{#1}}}
\newcommand{\FunctionTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{{#1}}}
\newcommand{\VariableTok}[1]{\textcolor[rgb]{0.00,0.00,0.00}{{#1}}}
\newcommand{\ControlFlowTok}[1]{\textcolor[rgb]{0.13,0.29,0.53}{\textbf{{#1}}}}
\newcommand{\OperatorTok}[1]{\textcolor[rgb]{0.81,0.36,0.00}{\textbf{{#1}}}}
\newcommand{\BuiltInTok}[1]{{#1}}
\newcommand{\ExtensionTok}[1]{{#1}}
\newcommand{\PreprocessorTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textit{{#1}}}}
\newcommand{\AttributeTok}[1]{\textcolor[rgb]{0.77,0.63,0.00}{{#1}}}
\newcommand{\RegionMarkerTok}[1]{{#1}}
\newcommand{\InformationTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{{#1}}}}}
\newcommand{\WarningTok}[1]{\textcolor[rgb]{0.56,0.35,0.01}{\textbf{\textit{{#1}}}}}
\newcommand{\AlertTok}[1]{\textcolor[rgb]{0.94,0.16,0.16}{{#1}}}
\newcommand{\ErrorTok}[1]{\textcolor[rgb]{0.64,0.00,0.00}{\textbf{{#1}}}}
\newcommand{\NormalTok}[1]{{#1}}


%--------------------------------------------------------%
% Fin                                                    %
%--------------------------------------------------------%
\endinput
