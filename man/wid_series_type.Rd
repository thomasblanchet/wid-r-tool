\name{wid_series_type}
\alias{wid_series_type}

\title{List of series type in WID.world}

\description{
Series types are one-letter codes that you use to specify the nature of the
series that you want (share, average, total, index, etc.) This letter is the
first character of the six-letter string that forms an \emph{indicator code}.
See \code{\link{wid_concepts}} for the five other letters.
}

\section{List of series type}{
\tabular{rcl}{
\bold{one-letter code} \tab      \tab \bold{description} \cr
\code{a} \tab      \tab average \cr
\code{s} \tab      \tab share \cr
\code{t} \tab      \tab threshold \cr
\code{m} \tab      \tab macroeconomic total \cr
\code{w} \tab      \tab wealth/income ratio \cr
\code{c} \tab      \tab decomposition of income or wealth \cr
\code{n} \tab      \tab population (both genders) \cr
\code{f} \tab      \tab female population \cr
\code{h} \tab      \tab male population \cr
\code{i} \tab      \tab indices (eg. price index) \cr
\code{x} \tab      \tab exchange rate (market or PPP) \cr
\code{b} \tab      \tab inverted Pareto coefficient \cr
}
}

\seealso{
\code{\link{download_wid}}
}

\author{
Thomas Blanchet
}
