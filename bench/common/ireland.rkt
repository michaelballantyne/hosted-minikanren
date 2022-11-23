#lang racket

(provide (all-defined-out))

(define nodes
  '(carlow
    cavan
    clare
    cork
    donegal
    dublin
    galway
    kerry
    kildare
    kilkenny
    laois
    leitrim
    limerick
    longford
    louth
    mayo
    meath
    monaghan
    offaly
    roscommon
    sligo
    tipperary
    waterford
    westmeath
    wexford
    wicklow))

(define edges
  '((carlow kildare)
    (carlow kilkenny)
    (carlow laois)
    (carlow wexford)
    (carlow wicklow)
    (cavan leitrim)
    (cavan longford)
    (cavan meath)
    (cavan monaghan)
    (cavan westmeath)
    (clare galway)
    (clare limerick)
    (clare kerry)
    (clare tipperary)
    (cork kerry)
    (cork limerick)
    (cork tipperary)
    (cork waterford)
    (donegal leitrim)
    (dublin kildare)
    (dublin meath)
    (dublin wicklow)
    (galway mayo)
    (galway offaly)
    (galway roscommon)
    (galway tipperary)
    (kerry limerick)
    (kildare laois)
    (kildare meath)
    (kildare offaly)
    (kildare wicklow)
    (kilkenny laois)
    (kilkenny tipperary)
    (kilkenny waterford)
    (kilkenny wexford)
    (laois offaly)
    (laois tipperary)
    (leitrim longford)
    (leitrim mayo)
    (leitrim sligo)
    (limerick tipperary)
    (longford roscommon)
    (longford westmeath)
    (louth meath)
    (louth monaghan)
    (mayo roscommon)
    (mayo sligo)
    (meath offaly)
    (meath westmeath)
    (offaly roscommon)
    (offaly tipperary)
    (offaly westmeath)
    (roscommon sligo)
    (tipperary waterford)
    (wexford wicklow)))
