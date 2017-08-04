\name{wid_concepts}
\alias{wid_concepts}

\title{List of concepts in WID.world}

\description{
Concepts are five-letter codes that generally refer to a definition or component of income or wealth, but also to a population, an index, etc. They form the last five letters of an \emph{indicator code}. See \code{\link{wid_series_type}} for the first letter.
}

\section{List of concepts}{

\subsection{Macro income}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{nninc} \tab      \tab national income\cr
\code{ndpro} \tab      \tab net domestic product\cr
\code{gdpro} \tab      \tab gross domestic product\cr
\code{confc} \tab      \tab consumption of fixed capital of national economy\cr
\code{nnfin} \tab      \tab net foreign income\cr
\code{pinrx} \tab      \tab property income received from abroad\cr
\code{pinpx} \tab      \tab property income paid abroad\cr
\code{comrx} \tab      \tab compensation of employees received from abroad\cr
\code{compx} \tab      \tab compensation of employees paid to rest of the world\cr
\code{nvanf} \tab      \tab net value added of non-financial corporations sector\cr
\code{nvafc} \tab      \tab net value added of financial corporations sector\cr
\code{nvago} \tab      \tab net value added of general government\cr
\code{nvaho} \tab      \tab net value added of personal sector\cr
\code{nvanp} \tab      \tab net value added of non-profit sector\cr
\code{nvatp} \tab      \tab taxes on product (net) of national economy\cr
\code{comnf} \tab      \tab compensation of employees paid by non financial corporations\cr
\code{nsunf} \tab      \tab net operating surplus of non-financial corporations \cr
\code{taxnf} \tab      \tab other taxes on production (net) paid by non-financial corporations sector\cr
\code{comfc} \tab      \tab compensation of employees paid by financial corporations sector\cr
\code{nsufc} \tab      \tab net operating surplus of financial corporations\cr
\code{taxfc} \tab      \tab other taxes on production (net) paid by financial corporations\cr
\code{comgo} \tab      \tab compensation of employees paid by general government\cr
\code{taxgo} \tab      \tab other taxes on production (net) paid by general government\cr
\code{comho} \tab      \tab compensation of employees paid by personal sector\cr
\code{nsuho} \tab      \tab net operating surplus of personal sector\cr
\code{nmiho} \tab      \tab net mixed income of personal sector\cr
\code{taxho} \tab      \tab other taxes on production (net) paid by personal sector\cr
\code{comnp} \tab      \tab compensation of employees paid by non-profit sector\cr
\code{taxnp} \tab      \tab other taxes on production (net) paid by non-profit sector\cr
\code{priho} \tab      \tab primary income of personal sector\cr
\code{prigo} \tab      \tab primary income of general government\cr
\code{prinf} \tab      \tab primary income of non-financial corporations sector\cr
\code{prifc} \tab      \tab primary income of financial corporations sector\cr
\code{prinp} \tab      \tab primary income of non-profit sector\cr
\code{corho} \tab      \tab compensation of employees received by personal sector\cr
\code{nmrho} \tab      \tab net mixed income of personal sector\cr
\code{nsrho} \tab      \tab net operating surplus (housing rents) of personal sector\cr
\code{prhho} \tab      \tab property income received by personal sector\cr
\code{prdho} \tab      \tab property income paid by personal sector\cr
\code{prngo} \tab      \tab primary income of general government \cr
\code{targo} \tab      \tab taxes on production (net) received by general government\cr
\code{phigo} \tab      \tab property income received by general government\cr
\code{phdgo} \tab      \tab property income paid by general government\cr
\code{phinf} \tab      \tab property income received by non-financial corporations\cr
\code{phdnf} \tab      \tab property income paid by non-financial corporations\cr
\code{phifc} \tab      \tab property income received by financial corporations\cr
\code{phdfc} \tab      \tab property income paid by financial corporations\cr
\code{prico} \tab      \tab primary income of corporate sector\cr
\code{nsuco} \tab      \tab net operating surplus of corporate sector\cr
\code{phico} \tab      \tab property income received by corporate sector\cr
\code{phdco} \tab      \tab property income paid by corporate sector\cr
\code{nsunp} \tab      \tab net operating surplus of non-profit sector\cr
\code{phinp} \tab      \tab property income received by non-profit sector\cr
\code{phdnp} \tab      \tab property income paid by non-profit sector\cr
\code{fainc} \tab      \tab personal factor income\cr
\code{fkigo} \tab      \tab property income received by general government sector\cr
\code{fkdgo} \tab      \tab property income paid by general government sector\cr
\code{nopnp} \tab      \tab net operating surplus of npish sector\cr
\code{fkinp} \tab      \tab property income received by npish sector\cr
\code{fkdnp} \tab      \tab property income paid by npish sector\cr
\code{fkcor} \tab      \tab primary income of corporate sector\cr
\code{fkprg} \tab      \tab taxes on production\cr
\code{flinc} \tab      \tab factor labor income\cr
\code{fkinc} \tab      \tab factor capital income\cr
\code{flemp} \tab      \tab compensation of employees\cr
\code{fllbu} \tab      \tab labor component of net mixed income\cr
\code{flprl} \tab      \tab imputed taxes on production\cr
\code{fkkbu} \tab      \tab capital component of net mixed income \cr
\code{fkhou} \tab      \tab housing asset income \cr
\code{fkfin} \tab      \tab property income received \cr
\code{fkdeb} \tab      \tab property income paid \cr
\code{fkret} \tab      \tab undistributed corporate profits\cr
\code{fkprk} \tab      \tab imputed taxed on production\cr
\code{ppinc} \tab      \tab pre-tax income (pension definition)\cr
\code{pnpco} \tab      \tab pension contributions\cr
\code{pmpen} \tab      \tab investment income payable to pension entitlements (pension definition)\cr
\code{ptpen} \tab      \tab pension benefits (pension definition)\cr
\code{ptinc} \tab      \tab pre-tax income (social insurance definition)\cr
\code{plcon} \tab      \tab net social contributions (employers and households)\cr
\code{pkpen} \tab      \tab investment income payable to pension entitlements (broad definition)\cr
\code{ptben} \tab      \tab pension benefits (social insurance-based definition)\cr
\code{plinc} \tab      \tab pre-tax labor income\cr
\code{pkinc} \tab      \tab pre-tax capital income\cr
\code{plbel} \tab      \tab labor share of social insurance income\cr
\code{kminc} \tab      \tab pre-tax capital income \cr
\code{pkbek} \tab      \tab capital share of social insurance income \cr
\code{soben} \tab      \tab social insurance benefits\cr
\code{sobec} \tab      \tab social insurance benefits in cash\cr
\code{sobeo} \tab      \tab other social insurance benefits \cr
\code{dincc} \tab      \tab disposable income (narrow definition)\cr
\code{fprod} \tab      \tab taxes on production\cr
\code{dicot} \tab      \tab taxes on income and wealth\cr
\code{dicab} \tab      \tab social assistance benefits\cr
\code{dikin} \tab      \tab social transfers in kind\cr
\code{diinc} \tab      \tab disposable income (broad definition)\cr
\code{dicol} \tab      \tab collective consumption expenditure\cr
}
}

\subsection{Distributed income}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{fiinc} \tab      \tab fiscal income \cr
\code{filin} \tab      \tab fiscal labour income [total fiscal income ranking]\cr
\code{fiwag} \tab      \tab wages and pensions\cr
\code{fimil} \tab      \tab labour component of mixed income\cr
\code{ficap} \tab      \tab fiscal capital income [total fiscal income ranking]\cr
\code{fimik} \tab      \tab capital component of mixed income\cr
\code{firen} \tab      \tab rents\cr
\code{fiint} \tab      \tab interest\cr
\code{fidiv} \tab      \tab dividends\cr
\code{fikgi} \tab      \tab capital gains\cr
\code{fimix} \tab      \tab mixed income\cr
\code{itinc} \tab      \tab intuitive income\cr
\code{itlab} \tab      \tab intuitive labor income\cr
\code{itcap} \tab      \tab intuitive capital income\cr
\code{fainc} \tab      \tab pre-tax factor income\cr
\code{flinc} \tab      \tab factor labor income [total pre-tax factor income ranking]\cr
\code{flemp} \tab      \tab compensation of employees\cr
\code{flwag} \tab      \tab wages and salaries\cr
\code{flsup} \tab      \tab supplements to wages and salaries\cr
\code{flcon} \tab      \tab social contributions\cr
\code{flmil} \tab      \tab labor component of net mixed income\cr
\code{fkinc} \tab      \tab factor capital income [total pre-tax factor income ranking]\cr
\code{fkhou} \tab      \tab housing asset income\cr
\code{fkequ} \tab      \tab equity asset income\cr
\code{fkfix} \tab      \tab interest income\cr
\code{fkpen} \tab      \tab pensions and life insurance asset income\cr
\code{fkmik} \tab      \tab capital component of mixed income\cr
\code{fkdeb} \tab      \tab interest payments\cr
\code{fkmor} \tab      \tab mortgage interest paid\cr
\code{fknmo} \tab      \tab non-mortgage interest paid\cr
\code{ptinc} \tab      \tab pre-tax national income \cr
\code{ptlin} \tab      \tab pre-tax labor income [total pre-tax income ranking]\cr
\code{ptemp} \tab      \tab compensation of employees \cr
\code{ptlbu} \tab      \tab labor component of net mixed income \cr
\code{ptsoc} \tab      \tab pensions and social insurance income (labor share)\cr
\code{ptkin} \tab      \tab pre-tax capital income [total pre-tax income ranking]\cr
\code{pthou} \tab      \tab housing asset income\cr
\code{ptfin} \tab      \tab equity asset income\cr
\code{ptint} \tab      \tab interest income\cr
\code{ptkpe} \tab      \tab pensions and social insurance income (capital share)\cr
\code{ptmik} \tab      \tab capital component of mixed income\cr
\code{ptinp} \tab      \tab interest payments\cr
\code{ptmor} \tab      \tab mortgage interest paid\cr
\code{ptnmo} \tab      \tab non-mortgage interest paid\cr
\code{govin} \tab      \tab net property income paid by government\cr
\code{npinc} \tab      \tab net primary income of non-profit institutions\cr
\code{prspn} \tab      \tab primary surplus of pension system\cr
\code{invpn} \tab      \tab investment income payable to pension funds\cr
\code{pllin} \tab      \tab pre-tax labor income [pre-tax labor income ranking]\cr
\code{plemp} \tab      \tab compensation of employees [pre-tax labor income ranking]\cr
\code{pllbu} \tab      \tab labor component of net mixed income [pre-tax labor income ranking]\cr
\code{plsoc} \tab      \tab pensions and social insurance income (labor share) [pre-tax labor income ranking]\cr
\code{pkkin} \tab      \tab pre-tax capital income [pre-tax capital income ranking]\cr
\code{pkhou} \tab      \tab housing asset income [pre-tax capital income ranking]\cr
\code{pkfin} \tab      \tab equity asset income [pre-tax capital income ranking]\cr
\code{pkint} \tab      \tab interest income [pre-tax capital income ranking]\cr
\code{pkkpe} \tab      \tab pensions and social insurance income (capital share) [pre-tax capital income ranking]\cr
\code{pkmik} \tab      \tab capital component of mixed income\cr
\code{pkinp} \tab      \tab interest payments [pre-tax capital income ranking]\cr
\code{pkmor} \tab      \tab mortgage interest paid [pre-tax capital income ranking]\cr
\code{pknmo} \tab      \tab non-mortgage interest paid [pre-tax capital income ranking]\cr
\code{diinc} \tab      \tab post-tax national income\cr
\code{cainc} \tab      \tab post-tax disposable income\cr
\code{fprod} \tab      \tab taxes on production (net)\cr
\code{fkprk} \tab      \tab capital component of taxes on production\cr
\code{flprl} \tab      \tab labor component of taxes on production\cr
\code{dicot} \tab      \tab current taxes on income and wealth\cr
\code{ditaf} \tab      \tab federal taxes on income and wealth\cr
\code{ditas} \tab      \tab state taxes on income and wealth\cr
\code{dicab} \tab      \tab social assistance benefits in cash\cr
\code{prspp} \tab      \tab primary surplus of private pension system\cr
\code{ppinc} \tab      \tab public spending\cr
\code{dikin} \tab      \tab social transfers in kind\cr
\code{dicol} \tab      \tab collective consumption expenditure\cr
\code{defic} \tab      \tab government deficit\cr
\code{dpinc} \tab      \tab post-tax national income (proportional attribution)\cr
\code{pppnc} \tab      \tab public spending (proportional attribution)\cr
\code{dpkin} \tab      \tab social transfers in kind (proportional attribution)\cr
\code{dpcol} \tab      \tab collective consumption expenditure (proportional attribution)\cr
\code{dpfic} \tab      \tab government deficit (proportional attribution)\cr
}
}


\subsection{Macro wealth}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{pweal} \tab      \tab net private wealth\cr
\code{pwnfa} \tab      \tab private non-financial assets\cr
\code{pwhou} \tab      \tab private housing assets\cr
\code{pwdwe} \tab      \tab private dwellings\cr
\code{pwlan} \tab      \tab private land underlying dwellings\cr
\code{pwbus} \tab      \tab private business assets\cr
\code{pwagr} \tab      \tab private agricultural land\cr
\code{pwodk} \tab      \tab other domestic private capital\cr
\code{pwnat} \tab      \tab private natural capital\cr
\code{pwfin} \tab      \tab private financial assets\cr
\code{pwfix} \tab      \tab private currency, deposits, bonds & loans\cr
\code{pwcud} \tab      \tab private currency & deposits\cr
\code{pwbol} \tab      \tab private bonds & loans\cr
\code{pwequ} \tab      \tab private equity, fund shares & offshore wealth\cr
\code{pweqi} \tab      \tab private equities\cr
\code{pwoff} \tab      \tab private offshore wealth\cr
\code{pwpen} \tab      \tab private pension funds & life insurance\cr
\code{pwdeb} \tab      \tab private debt\cr
\code{pwfie} \tab      \tab private financial assets excluding currency & deposits\cr
\code{hweal} \tab      \tab net personal wealth\cr
\code{hwnfa} \tab      \tab personal non-financial assets\cr
\code{hwhou} \tab      \tab personal housing assets\cr
\code{hwdwe} \tab      \tab personal dwellings\cr
\code{hwlan} \tab      \tab personal land underlying dwellings\cr
\code{hwbus} \tab      \tab personal business and other non-financial assets\cr
\code{hwagr} \tab      \tab personal agricultural land\cr
\code{hwodk} \tab      \tab other domestic personal capital\cr
\code{hwnat} \tab      \tab personal natural capital\cr
\code{hwfin} \tab      \tab personal financial assets\cr
\code{hwfix} \tab      \tab personal currency, deposits, bonds & loans\cr
\code{hwcud} \tab      \tab personal currency & deposits\cr
\code{hwbol} \tab      \tab personal bonds & loans\cr
\code{hwequ} \tab      \tab personal equity, fund shares & offshore wealth\cr
\code{hweqi} \tab      \tab personal equities\cr
\code{hwoff} \tab      \tab personal offshore wealth\cr
\code{hwpen} \tab      \tab personal pension funds & life insurance\cr
\code{hwdeb} \tab      \tab personal debt\cr
\code{hwfie} \tab      \tab personal financial assets excluding cash\cr
\code{iweal} \tab      \tab net non-profit wealth\cr
\code{iwnfa} \tab      \tab non-profit non-financial assets\cr
\code{iwhou} \tab      \tab non-profit housing assets\cr
\code{iwdwe} \tab      \tab non-profit dwellings\cr
\code{iwlan} \tab      \tab non-profit land underlying dwellings\cr
\code{iwbus} \tab      \tab non-profit business and other non-financial assets\cr
\code{iwagr} \tab      \tab non-profit agricultural land\cr
\code{iwodk} \tab      \tab non-profit other domestic capital\cr
\code{iwnat} \tab      \tab non-profit natural capital\cr
\code{iwfin} \tab      \tab non-profit financial assets\cr
\code{iwfix} \tab      \tab non-profit currency, deposits, bonds & loans\cr
\code{iwcud} \tab      \tab non-profit currency & deposits\cr
\code{iwbol} \tab      \tab non-profit bonds & loans\cr
\code{iwequ} \tab      \tab non-profit equity, fund shares & offshore wealth\cr
\code{iweqi} \tab      \tab non-profit equities\cr
\code{iwoff} \tab      \tab non-profit offshore wealth\cr
\code{iwpen} \tab      \tab non-profit pension funds & life insurance\cr
\code{iwdeb} \tab      \tab non-profit debt\cr
\code{iwfie} \tab      \tab non-profit financial assets excluding cash\cr
\code{cwboo} \tab      \tab book value of corporations\cr
\code{cwnfa} \tab      \tab corporate non-financial assets \cr
\code{cwhou} \tab      \tab corporate housing assets \cr
\code{cwdwe} \tab      \tab corporate dwellings \cr
\code{cwlan} \tab      \tab corporate land underlying dwellings \cr
\code{cwbus} \tab      \tab corporate business and other non-financial assets \cr
\code{cwagr} \tab      \tab corporate agricultural land\cr
\code{cwodk} \tab      \tab corporate other domestic capital\cr
\code{cwnat} \tab      \tab corporate natural capital\cr
\code{cwfin} \tab      \tab corporate financial assets\cr
\code{cwfix} \tab      \tab corporate currency, deposits, bonds & loans\cr
\code{cwcud} \tab      \tab corporate currency & deposits\cr
\code{cwbol} \tab      \tab corporate bonds & loans\cr
\code{cwequ} \tab      \tab corporate equity, fund shares & offshore wealth\cr
\code{cweqi} \tab      \tab corporate equities\cr
\code{cwoff} \tab      \tab corporate offshore wealth\cr
\code{cwpen} \tab      \tab corporate pension funds & life insurance\cr
\code{cwdeb} \tab      \tab corporate debt (non-equity liability)\cr
\code{cwdeq} \tab      \tab market value of corporations (equity liability)\cr
\code{cwres} \tab      \tab residual corporate wealth\cr
\code{cwtoq} \tab      \tab Tobin's Q\cr
\code{cwfie} \tab      \tab corporate financial assets excluding cash\cr
\code{gweal} \tab      \tab net public wealth\cr
\code{gwnfa} \tab      \tab government non-financial assets\cr
\code{gwhou} \tab      \tab government housing assets\cr
\code{gwdwe} \tab      \tab government dwellings\cr
\code{gwlan} \tab      \tab government land underlying dwellings\cr
\code{gwbus} \tab      \tab government business and other non-financial assets\cr
\code{gwagr} \tab      \tab government agricultural land\cr
\code{gwodk} \tab      \tab government other domestic capital\cr
\code{gwnat} \tab      \tab government natural capital\cr
\code{gwfin} \tab      \tab government financial assets\cr
\code{gwfix} \tab      \tab government currency, deposits, bonds & loans\cr
\code{gwcud} \tab      \tab government currency & deposits\cr
\code{gwbol} \tab      \tab government bonds & loans\cr
\code{gwequ} \tab      \tab government equity, fund shares & offshore wealth\cr
\code{gweqi} \tab      \tab government equities\cr
\code{gwoff} \tab      \tab government offshore wealth\cr
\code{gwpen} \tab      \tab government pension funds & life insurance\cr
\code{gwdeb} \tab      \tab government debt\cr
\code{gwfie} \tab      \tab government financial assets excluding cash\cr
\code{nweal} \tab      \tab market-value national wealth \cr
\code{nwnfm} \tab      \tab national non-financial assets (market value)\cr
\code{nwhom} \tab      \tab national housing assets (market value)\cr
\code{nwbum} \tab      \tab national business and other non-financial assets (market value)\cr
\code{nwodm} \tab      \tab national other domestic capital (market value)\cr
\code{nwagr} \tab      \tab national agricultural land (market value)\cr
\code{nwnat} \tab      \tab national natural capital (market value)\cr
\code{nwnxa} \tab      \tab net foreign assets\cr
\code{nwgxa} \tab      \tab gross foreign assets\cr
\code{nwgxd} \tab      \tab gross foreign liabilities\cr
\code{nwfim} \tab      \tab domestic financial assets\cr
\code{nwdem} \tab      \tab domestic financial liabilities\cr
\code{nwdwe} \tab      \tab national dwellings (book value)\cr
\code{nwlan} \tab      \tab national land underlying dwellings (book value)\cr
\code{nwbus} \tab      \tab national business and other non-financial assets (book value)\cr
\code{nwodk} \tab      \tab national other domestic capital (book value)\cr
\code{nwfin} \tab      \tab domestic financial assets (book value)\cr
\code{nwdeb} \tab      \tab domestic financial liabilities (book value)\cr
\code{nwfto} \tab      \tab gross domestic financial assets\cr
\code{nwboo} \tab      \tab book-value national wealth\cr
\code{psavi} \tab      \tab net private saving\cr
\code{psgro} \tab      \tab gross private saving\cr
\code{psdep} \tab      \tab private capital depreciation\cr
\code{hsavi} \tab      \tab net personal saving\cr
\code{hsgro} \tab      \tab gross personal saving\cr
\code{hsdep} \tab      \tab personal capital depreciation\cr
\code{isavi} \tab      \tab net non-profit saving\cr
\code{isgro} \tab      \tab gross non-profit saving\cr
\code{isdep} \tab      \tab non-profit capital depreciation\cr
\code{csavi} \tab      \tab net corporate saving\cr
\code{csgro} \tab      \tab gross corporate saving\cr
\code{csdep} \tab      \tab corporate capital depreciation\cr
\code{gsavi} \tab      \tab net government saving\cr
\code{gsgro} \tab      \tab gross government saving\cr
\code{gsdep} \tab      \tab government capital depreciation\cr
\code{nsavi} \tab      \tab net national saving\cr
\code{nsgro} \tab      \tab gross national saving\cr
\code{nsdep} \tab      \tab national capital depreciation\cr
\code{dsavi} \tab      \tab domestic investment\cr
\code{fsavi} \tab      \tab net foreign saving\cr
}
}

\subsection{Distributed wealth}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{hweal} \tab      \tab net personal wealth\cr
\code{hwnfa} \tab      \tab personal non-financial assets\cr
\code{hwhou} \tab      \tab personal housing assets\cr
\code{hwdwe} \tab      \tab personal dwellings\cr
\code{hwlan} \tab      \tab personal land underlying dwellings\cr
\code{hwbus} \tab      \tab personal business and other non-financial assets\cr
\code{hwagr} \tab      \tab personal agricultural land\cr
\code{hwodk} \tab      \tab personal other domestic capital\cr
\code{hwnat} \tab      \tab personal natural capital\cr
\code{hwfin} \tab      \tab personal financial assets\cr
\code{hwfix} \tab      \tab personal currency, bonds & deposits\cr
\code{hwcud} \tab      \tab personal currency & deposits\cr
\code{hwbol} \tab      \tab personal bonds & loans\cr
\code{hwequ} \tab      \tab personal equity, fund shares & offshore wealth\cr
\code{hweqi} \tab      \tab personal equities\cr
\code{hwoff} \tab      \tab personal offshore wealth\cr
\code{hwpen} \tab      \tab personal pension funds & life insurance\cr
\code{hwfie} \tab      \tab personal financial assets excluding currency & deposits\cr
\code{hwdeb} \tab      \tab personal debt\cr
}
}

\subsection{Wealth/income ratios}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{wealn} \tab      \tab net national wealth to net national income ratio \cr
\code{wealp} \tab      \tab net private wealth to net national income ratio \cr
\code{wealh} \tab      \tab net personal wealth to net national income ratio \cr
\code{weali} \tab      \tab net non-profit wealth to net national income ratio \cr
\code{wealc} \tab      \tab net corporate wealth to net national income ratio \cr
\code{wealg} \tab      \tab net public wealth to net national income ratio \cr
\code{incta} \tab      \tab income reduction as a result of income tax \cr
}
}

\subsection{Population}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{popul} \tab      \tab population \cr
\code{taxto} \tab      \tab total tax population \cr
\code{taxma} \tab      \tab number of tax units — married couples & single adults \cr
\code{taxad} \tab      \tab number of tax units — adults \cr
\code{taxre} \tab      \tab number of tax returns \cr
\code{popem} \tab      \tab employed population \cr
}
}

\subsection{Price and exchange rates}{
\tabular{rcl}{
\bold{five-letter code} \tab      \tab \bold{description} \cr
\code{nyixx} \tab      \tab national income price index \cr
\code{lcusp} \tab      \tab PPP conversion factor, LCU per USD \cr
\code{lceup} \tab      \tab PPP conversion factor, LCU per EUR \cr
\code{lcyup} \tab      \tab PPP conversion factor, LCU per CNY \cr
\code{lcusx} \tab      \tab market exchange rate, LCU per USD \cr
\code{lceux} \tab      \tab market exchange rate, LCU per EUR \cr
\code{lcyux} \tab      \tab market exchange rate, LCU per CNY \cr
}
}

}

\seealso{
\code{\link{download_wid}}
}

\author{
Thomas Blanchet
}
