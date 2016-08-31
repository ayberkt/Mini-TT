structure NormalForms  =
struct

  structure Syn = Syntax

  datatype nexp =
      NLAM of int * nexp
    | NPAIR of nexp * nexp
    | NCON of Syn.name * nexp
    | NUNIT
    | NSET
    | NPI of nexp * int * nexp
    | NSIGMA of nexp * int * nexp
    | NONE
    | NFUN of nsclos
    | NSUM of nsclos
    | NNT of nneut
  and nneut =
      NGEN of int
    | NFST of nneut
    | NSND of nneut
    | NNTFUN of nsclos * nneut
  and nrho =
      NRNIL
    | NUPVAR of nrho * Syn.patt * nexp
    | NUPDEC of nrho * Syn.decl
  and nsclos = NSCL of Syn.branch * nrho

end
