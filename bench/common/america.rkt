#lang racket
(provide (all-defined-out))

(define nodes
  '(alabama
    alaska
    arizona
    arkansas
    california
    colorado
    connecticut
    delaware
    district-of-columbia
    florida
    georgia
    hawaii
    idaho
    illinois
    indiana
    iowa
    kansas
    kentucky
    louisiana
    maine
    montana
    nebraska
    nevada
    new-hampshire
    new-jersey
    new-mexico
    new-york
    north-carolina
    north-dakota
    ohio
    oklahoma
    oregon
    maryland
    massachusetts
    michigan
    minnesota
    mississippi
    missouri
    pennsylvania
    rhode-island
    south-carolina
    south-dakota
    tennessee
    texas
    utah
    vermont
    virginia
    washington
    west-virginia
    wisconsin
    wyoming))

(define edges
  '((alabama florida)
    (alabama georgia)
    (alabama mississippi)
    (alabama tennessee)
    (arkansas louisiana)
    (arkansas missouri)
    (arkansas mississippi)
    (arkansas oklahoma)
    (arkansas tennessee)
    (arkansas texas)
    (arizona california)
    (arizona new-mexico)
    (arizona nevada)
    (arizona utah)
    (california nevada)
    (california oregon)
    (colorado kansas)
    (colorado nebraska)
    (colorado new-mexico)
    (colorado oklahoma)
    (colorado utah)
    (colorado wyoming)
    (connecticut massachusetts)
    (connecticut new-york)
    (connecticut rhode-island)
    (district-of-columbia maryland)
    (district-of-columbia virginia)
    (delaware maryland)
    (delaware new-jersey)
    (delaware pennsylvania)
    (florida georgia)
    (georgia north-carolina)
    (georgia south-carolina)
    (georgia tennessee)
    (iowa illinois)
    (iowa minnesota)
    (iowa missouri)
    (iowa nebraska)
    (iowa south-dakota)
    (iowa wisconsin)
    (idaho montana)
    (idaho nevada)
    (idaho oregon)
    (idaho utah)
    (idaho washington)
    (idaho wyoming)
    (illinois indiana)
    (illinois kentucky)
    (illinois missouri)
    (illinois wisconsin)
    (indiana kentucky)
    (indiana michigan)
    (indiana ohio)
    (kansas missouri)
    (kansas nebraska)
    (kansas oklahoma)
    (kentucky missouri)
    (kentucky ohio)
    (kentucky tennessee)
    (kentucky virginia)
    (kentucky west-virginia)
    (louisiana mississippi)
    (louisiana texas)
    (massachusetts new-hampshire)
    (massachusetts new-york)
    (massachusetts rhode-island)
    (massachusetts vermont)
    (maryland pennsylvania)
    (maryland virginia)
    (maryland west-virginia)
    (maine new-hampshire)
    (michigan ohio)
    (michigan wisconsin)
    (minnesota north-dakota)
    (minnesota south-dakota)
    (minnesota wisconsin)
    (missouri nebraska)
    (missouri oklahoma)
    (missouri tennessee)
    (mississippi tennessee)
    (montana north-dakota)
    (montana south-dakota)
    (montana wyoming)
    (north-carolina south-carolina)
    (north-carolina tennessee)
    (north-carolina virginia)
    (north-dakota south-dakota)
    (nebraska south-dakota)
    (nebraska wyoming)
    (new-hampshire vermont)
    (new-jersey new-york)
    (new-jersey pennsylvania)
    (new-mexico oklahoma)
    (new-mexico texas)
    (nevada oregon)
    (nevada utah)
    (new-york pennsylvania)
    (new-york vermont)
    (ohio pennsylvania)
    (ohio west-virginia)
    (oklahoma texas)
    (oregon washington)
    (pennsylvania west-virginia)
    (south-dakota wyoming)
    (tennessee virginia)
    (utah wyoming)
    (virginia west-virginia)))
